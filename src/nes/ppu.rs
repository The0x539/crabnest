#![allow(dead_code)]

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;
use ouroboros::self_referencing;
use sdl2::{
    pixels::PixelFormatEnum,
    render::{Texture, TextureCreator, WindowCanvas},
    video::WindowContext,
    EventPump, Sdl,
};

use crate::membus::{MemBus, MemRead, MemWrite};
use crate::mos6502::{Mos6502, NmiLine};
use crate::reset_manager::{Reset, ResetManager};
use crate::timekeeper::Timed;
use crate::{r, R};

use super::{a12_watcher::Edge, mapper::mmc3::Mmc3Irq};

const OUTPUT_WIDTH: u32 = 256;
const OUTPUT_HEIGHT: u32 = 240;
const CLK_DIVISOR: u64 = 4;

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
#[bits = 1]
enum VramAddrInc {
    Inc1 = 0,
    Inc32 = 1,
}

impl VramAddrInc {
    fn amount(&self) -> u16 {
        match self {
            Self::Inc1 => 1,
            Self::Inc32 => 32,
        }
    }
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
#[bits = 1]
enum ChrBaseAddr {
    Addr0000 = 0,
    Addr1000 = 1,
}

impl ChrBaseAddr {
    fn addr(&self) -> u16 {
        match self {
            Self::Addr0000 => 0x0000,
            Self::Addr1000 => 0x1000,
        }
    }
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
#[bits = 2]
enum NtBaseAddrEnum {
    Addr2000 = 0,
    Addr2400 = 1,
    Addr2800 = 2,
    Addr2C00 = 3,
}

#[bitfield(bits = 2)]
#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
struct NtBaseAddr {
    h: bool,
    v: bool,
}

impl NtBaseAddr {
    fn switch_h(&self) -> Self {
        self.with_h(!self.h())
    }

    fn switch_v(&self) -> Self {
        self.with_v(!self.v())
    }
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone, PartialEq)]
#[bits = 1]
enum SpriteSize {
    EightByEight = 0,
    EightBySixteen = 1,
}

impl SpriteSize {
    fn height(&self) -> u8 {
        match self {
            Self::EightByEight => 8,
            Self::EightBySixteen => 16,
        }
    }
}

#[bitfield(bits = 8)]
#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct SpriteAttrs {
    palette: B2,
    #[skip]
    __: B3,
    behind_bg: bool,
    horiz_flipped: bool,
    verti_flipped: bool,
}

#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Sprite {
    ypos: u8,
    tile: u8,
    attr: SpriteAttrs,
    xpos: u8,
}

#[bitfield(bits = 8)]
#[derive(Debug, Default, Copy, Clone, Zeroable)]
struct Mask {
    greyscale_en: bool,
    left_bg_en: bool,
    left_sprite_en: bool,
    bg_en: bool,
    sprite_en: bool,
    emph_red: bool,
    emph_green: bool,
    emph_blue: bool,
}

#[bitfield(bits = 12)]
#[derive(Debug, Default, Copy, Clone, Zeroable)]
struct PpuFlags {
    vram_addr_inc: VramAddrInc,
    bg_chr_baseaddr: ChrBaseAddr,
    sprite_chr_baseaddr: ChrBaseAddr,
    sprite_size: SpriteSize,

    nmi_en: bool,
    vblank: bool,
    sprite0_hit: bool,
    sprite_overflow: bool,
    write_toggle: bool,

    sprite0_hit_shouldset: bool,

    next_scanline_has_sprite0: bool,
    scanline_has_sprite0: bool,
}

#[bitfield(bits = 15)]
#[derive(Debug, Default, Copy, Clone, Zeroable)]
struct PpuVramAddr {
    coarse_xscroll: B5,
    coarse_yscroll: B5,
    nt_baseaddr: NtBaseAddr,
    fine_yscroll: B3,
}

impl PpuVramAddr {
    fn inc_coarse_x(&mut self) {
        if self.coarse_xscroll() == 31 {
            self.set_coarse_xscroll(0);
            self.set_nt_baseaddr(self.nt_baseaddr().switch_h());
        } else {
            // self.bytes[0] += 0x01;
            self.set_coarse_xscroll(self.coarse_xscroll() + 1);
        }
    }

    fn inc_y(&mut self) {
        if self.fine_yscroll() != 7 {
            // self.bytes[1] += 0x10;
            self.set_fine_yscroll(self.fine_yscroll() + 1);
            return;
        }

        self.set_fine_yscroll(0);
        let mut y = self.coarse_yscroll();
        if y == 29 {
            y = 0;
            self.set_nt_baseaddr(self.nt_baseaddr().switch_v())
        } else if y == 31 {
            y = 0;
        } else {
            y += 1;
        }
        self.set_coarse_yscroll(y);
    }

    fn copy_h(&mut self, tmp: Self) {
        self.set_coarse_xscroll(tmp.coarse_xscroll());
        self.set_nt_baseaddr(self.nt_baseaddr().with_h(tmp.nt_baseaddr().h()))
    }

    fn copy_v(&mut self, tmp: Self) {
        self.set_coarse_yscroll(tmp.coarse_yscroll());
        self.set_nt_baseaddr(self.nt_baseaddr().with_v(tmp.nt_baseaddr().v()));
        self.set_fine_yscroll(tmp.fine_yscroll());
    }

    fn to_u16(&self) -> u16 {
        u16::from_le_bytes(self.bytes)
    }

    fn set(&mut self, val: u16) {
        self.bytes = val.to_le_bytes();
    }
}

#[self_referencing]
struct PpuSdl {
    canvas: WindowCanvas,
    tex_cre: TextureCreator<WindowContext>,
    #[borrows(tex_cre)]
    #[covariant]
    tex: Texture<'this>,
    buf: Box<[[[u8; 4]; OUTPUT_WIDTH as usize]; OUTPUT_HEIGHT as usize]>,
}

pub struct Ppu {
    pub bus: R<MemBus>,
    nmi_line: NmiLine,
    pub(super) mmc3_irq: Option<R<Mmc3Irq>>,

    sdl: PpuSdl,
    sdl_events: R<EventPump>,

    pub state: PpuState,
}

#[derive(Debug, Clone, Zeroable)]
pub struct PpuState {
    framenum: usize,
    clk_countdown: u64,

    slnum: usize,
    dotnum: usize,
    overflow_dotnum: usize,

    mask: Mask,

    flags: PpuFlags,

    vram_addr: PpuVramAddr,
    vram_read_buf: u8,
    tmp_vram_addr: PpuVramAddr,
    vram_addr_bus: u16,

    fine_xscroll: u8,

    bmp_latch: [u8; 2],
    nt_latch: u8,
    attr_latch: u8,

    bg_bmp_shiftregs: [u16; 2],
    bg_attr_shiftregs: [u8; 2],
    bg_palette: u8,

    sprite_bmp_shiftregs: [[u8; 2]; 8],
    sprite_attrs: [SpriteAttrs; 8],
    sprite_xs: [u8; 8],

    eval_nsprites: u8,
    eval_sprites: [Sprite; 8],

    oam_addr: u8,
    sprites: [Sprite; 64],

    palette_mem: [u8; 32],
    pub palette_srgb: [[u8; 3]; 512],

    decay_latch: u8,
}

impl PpuState {
    fn render_disabled(&self) -> bool {
        !self.mask.bg_en() && !self.mask.sprite_en()
    }

    fn set_delayed_regs(&mut self) {
        let fl = &mut self.flags;
        if fl.sprite0_hit_shouldset() {
            fl.set_sprite0_hit(true);
            fl.set_sprite0_hit_shouldset(false);
        }
        if self.dotnum == self.overflow_dotnum && self.overflow_dotnum != 0 {
            self.flags.set_sprite_overflow(true);
            self.overflow_dotnum = 0;
        }
    }

    fn clear_regs(&mut self) {
        if self.dotnum == 1 && self.slnum == 261 {
            let fl = &mut self.flags;
            fl.set_sprite0_hit(false);
            fl.set_sprite0_hit_shouldset(false);
            fl.set_sprite_overflow(false);
            fl.set_write_toggle(false);
            fl.set_vblank(false);
        }
    }

    fn shift_shiftregs(&mut self) {
        match self.dotnum {
            2..=257 | 322..=337 => (),
            _ => return,
        }

        self.bg_bmp_shiftregs[0] <<= 1;
        self.bg_bmp_shiftregs[1] <<= 1;

        self.bg_attr_shiftregs[0] <<= 1;
        self.bg_attr_shiftregs[0] |= self.bg_palette & 1;

        self.bg_attr_shiftregs[1] <<= 1;
        self.bg_attr_shiftregs[1] |= self.bg_palette >> 1;
    }

    fn load_shiftregs(&mut self) {
        match self.dotnum {
            n @ (8..=256 | 328..=336) if n % 8 == 0 => (),
            _ => return,
        }

        self.bg_bmp_shiftregs[0] &= 0xFF00;
        self.bg_bmp_shiftregs[1] &= 0xFF00;

        self.bg_bmp_shiftregs[0] |= self.bmp_latch[0] as u16;
        self.bg_bmp_shiftregs[1] |= self.bmp_latch[1] as u16;

        let (xscroll, yscroll) = (
            self.vram_addr.coarse_xscroll(),
            self.vram_addr.coarse_yscroll(),
        );
        let attr_shift = match (xscroll >> 1 & 1, yscroll >> 1 & 1) {
            (0, 0) => 0,
            (1, 0) => 2,
            (0, 1) => 4,
            (1, 1) => 6,
            _ => unreachable!(),
        };

        self.bg_palette = (self.attr_latch >> attr_shift) & 0x3;
    }

    fn update_vram_addr(&mut self) {
        if self.render_disabled() {
            return;
        }

        if self.slnum < 240 {
            match self.dotnum {
                n @ (1..=256 | 321..=336) if n % 8 == 0 => {
                    self.vram_addr.inc_coarse_x();
                    if n == 256 {
                        self.vram_addr.inc_y();
                    }
                }
                257 => self.vram_addr.copy_h(self.tmp_vram_addr),
                _ => (),
            }
        } else if self.slnum == 261 && (280..=304).contains(&self.dotnum) {
            self.vram_addr.copy_v(self.tmp_vram_addr);
        }
    }

    fn sprite_in_range(&self, ypos: u8) -> bool {
        let top = ypos as usize;
        let bottom = top + self.flags.sprite_size().height() as usize;
        (top..bottom).contains(&self.slnum)
    }

    fn spriteeval(&mut self) {
        if self.render_disabled() || self.dotnum != 0 || matches!(self.slnum, 240..=260) {
            return;
        }

        let mut cycles = 0;

        bytemuck::bytes_of_mut(&mut self.eval_sprites).fill(0xFF);
        self.eval_nsprites = 0;

        cycles += 64;

        self.flags
            .set_scanline_has_sprite0(self.flags.next_scanline_has_sprite0());
        self.flags.set_next_scanline_has_sprite0(false);

        let mut n = 0;
        while n < 64 && self.eval_nsprites < 8 {
            cycles += 2;
            let sprite = &self.sprites[n];
            if self.sprite_in_range(sprite.ypos) {
                cycles += 6;
                if n == 0 {
                    self.flags.set_next_scanline_has_sprite0(true);
                }

                self.eval_sprites[self.eval_nsprites as usize] = *sprite;
                self.eval_nsprites += 1;
            }
            n += 1;
        }

        let mut m = 0;
        while n < 64 {
            cycles += 2;
            let ypos = self.oam()[4 * n + m];
            if self.sprite_in_range(ypos) {
                self.overflow_dotnum = cycles;
                break;
            } else {
                m = (m + 1) % 4;
            }
            n += 1;
        }
    }

    fn draw_pixel(&mut self) -> Option<u16> {
        let m = &self.mask;

        if self.render_disabled() || !(1..=256).contains(&self.dotnum) || self.slnum >= 240 {
            return None;
        }

        let mut bg_color = 0;
        bg_color |= ((self.bg_bmp_shiftregs[0] << self.fine_xscroll) >> 15) & 1;
        bg_color |= ((self.bg_bmp_shiftregs[1] << self.fine_xscroll) >> 14) & 2;

        let mut bg_palette = 0;
        bg_palette |= ((self.bg_attr_shiftregs[0] << self.fine_xscroll) >> 7) & 1;
        bg_palette |= ((self.bg_attr_shiftregs[1] << self.fine_xscroll) >> 6) & 2;

        let mut sprite_color = 0;
        let mut sprite_palette = 0;
        let mut sprite_behind_bg = false;
        let mut is_sprite0 = false;

        for i in 0..8 {
            if self.sprite_xs[i] > 0 {
                self.sprite_xs[i] -= 1;
                continue;
            }

            if sprite_color == 0 {
                sprite_color |= (self.sprite_bmp_shiftregs[i][0] >> 7) & 1;
                sprite_color |= (self.sprite_bmp_shiftregs[i][1] >> 6) & 2;
                sprite_palette = self.sprite_attrs[i].palette();
                sprite_behind_bg = self.sprite_attrs[i].behind_bg();
                is_sprite0 = i == 0 && self.flags.scanline_has_sprite0();
            }

            self.sprite_bmp_shiftregs[i][0] <<= 1;
            self.sprite_bmp_shiftregs[i][1] <<= 1;
        }

        if !m.sprite_en() || (!m.left_sprite_en() && self.dotnum <= 8) {
            sprite_color = 0;
        }

        if !m.bg_en() || (!m.left_bg_en() && self.dotnum <= 8) {
            bg_color = 0;
        }

        let fg_addr = 0x3F11 + 4 * sprite_palette as u16 + sprite_color as u16 - 1;
        let bg_addr = 0x3F01 + 4 * bg_palette as u16 + bg_color as u16 - 1;

        let paladdr = match (bg_color, sprite_color) {
            (0, 0) => 0x3F00,
            (0, 1..) => fg_addr,
            (1.., 0) => bg_addr,
            (1.., 1..) => {
                if is_sprite0 && self.dotnum != 256 {
                    self.flags.set_sprite0_hit_shouldset(true);
                }
                if sprite_behind_bg {
                    bg_addr
                } else {
                    fg_addr
                }
            }
        };

        let mut pixel_color = *self.palette_loc(paladdr).unwrap() as u16;
        pixel_color |= (self.mask.emph_red() as u16) << 6;
        pixel_color |= (self.mask.emph_green() as u16) << 7;
        pixel_color |= (self.mask.emph_blue() as u16) << 8;

        Some(pixel_color)
    }

    fn move_cursor(&mut self) {
        if self.dotnum == 0 && self.slnum == 0 && self.framenum % 2 == 1 && self.mask.bg_en() {
            // If the background is enabled, skip the first tick of the first scanline of odd frames
            self.dotnum += 1;
        }

        self.dotnum += 1;
        if self.dotnum <= 340 {
            return;
        }
        self.dotnum = 0;

        self.slnum += 1;
        if self.slnum <= 261 {
            return;
        }
        self.slnum = 0;

        self.framenum += 1;
    }

    fn palette_loc(&self, addr: u16) -> Option<&u8> {
        if !(0x3F00..=0x3FFF).contains(&addr) {
            return None;
        }

        let reladdr = (addr - 0x3F00) % 0x20;
        let offset = match reladdr {
            0x10 | 0x14 | 0x18 | 0x1C => reladdr - 0x10,
            _ => reladdr,
        };

        self.palette_mem.get(offset as usize)
    }

    fn palette_loc_mut(&mut self, addr: u16) -> Option<&mut u8> {
        if !(0x3F00..=0x3FFF).contains(&addr) {
            return None;
        }

        let reladdr = (addr - 0x3F00) % 0x20;
        let offset = match reladdr {
            0x10 | 0x14 | 0x18 | 0x1C => reladdr - 0x10,
            _ => reladdr,
        };

        self.palette_mem.get_mut(offset as usize)
    }

    fn oam(&self) -> &[u8] {
        bytemuck::bytes_of(&self.sprites)
    }

    fn oam_mut(&mut self) -> &mut [u8] {
        bytemuck::bytes_of_mut(&mut self.sprites)
    }

    fn inc_vram_addr_rw(&mut self) {
        if self.render_disabled() || matches!(self.slnum, 240..=260) {
            let mut vra = self.vram_addr.to_u16();
            vra += self.flags.vram_addr_inc().amount();
            vra %= 0x4000;
            self.vram_addr.set(vra);
        } else {
            self.vram_addr.inc_coarse_x();
            self.vram_addr.inc_y();
        }
        //self.vram_addr_bus = self.vram_addr.to_u16();
    }

    fn nt_addr(&self) -> u16 {
        0x2000 | (self.vram_addr.to_u16() & 0x0FFF)
    }

    fn attr_addr(&self) -> u16 {
        let va = self.vram_addr.to_u16();
        0x23C0 | (va & 0x0C00) | ((va >> 4) & 0x38) | ((va >> 2) & 0x07)
    }

    fn bg_bmp_addr(&self) -> u16 {
        let mut addr = self.flags.bg_chr_baseaddr().addr();
        addr += self.nt_latch as u16 * 16;
        addr += self.vram_addr.fine_yscroll() as u16;
        addr
    }

    fn check_vblank(&mut self) -> u8 {
        let v = self.flags.vblank();
        self.flags.set_vblank(false);
        if self.slnum == 241 && self.dotnum == 2 {
            0
        } else {
            v as u8
        }
    }
}

impl Ppu {
    pub fn new(
        sdl: &Sdl,
        rm: &R<ResetManager>,
        cpu: &mut Mos6502,
        event_pump: R<EventPump>,
        scale: u32,
    ) -> R<Self> {
        let bus = MemBus::new(rm);

        let video = sdl.video().expect("Could not init SDL video");
        let window = video
            .window("Crabnest", OUTPUT_WIDTH * scale, OUTPUT_HEIGHT * scale)
            .position_centered()
            .allow_highdpi()
            .build()
            .expect("Could not create window");
        let mut canvas = window
            .into_canvas()
            .build()
            .expect("Could not create canvas");

        canvas
            .set_integer_scale(true)
            .expect("Could not enable integer scaling");

        let tex_cre = canvas.texture_creator();
        // TODO: ndarray? image?
        let buf = vec![[[0_u8; 4]; OUTPUT_WIDTH as usize]; OUTPUT_HEIGHT as usize]
            .into_boxed_slice()
            .try_into()
            .unwrap();
        let ppu_sdl = PpuSdl::new(
            canvas,
            tex_cre,
            |tc| {
                tc.create_texture_streaming(PixelFormatEnum::ABGR8888, OUTPUT_WIDTH, OUTPUT_HEIGHT)
                    .expect("Could not create texture")
            },
            buf,
        );

        let ppu = r(Ppu {
            bus,
            nmi_line: cpu.get_nmi_line(),
            mmc3_irq: None,
            sdl: ppu_sdl,
            sdl_events: event_pump,
            state: Zeroable::zeroed(),
        });

        rm.borrow_mut().add_device(&ppu);
        // TODO: ask for a ref in this func
        cpu.tk.borrow_mut().add_timer(ppu.clone());

        // originally part of a `map` method; refactored
        let mut bus = cpu.bus.borrow_mut();
        for i in 0x20..0x40 {
            bus.set_read_handler(i, &ppu, 0);
            bus.set_write_handler(i, &ppu, 0);
        }

        ppu
    }

    fn present_frame(&mut self) {
        // We don't check for the quit event here (that's handled by something over in main)
        // but we do still need to process all the events, and it seems like this is where that happens
        self.sdl_events.borrow_mut().pump_events();

        self.sdl.with_mut(|sdl| {
            sdl.tex
                .update(
                    None,
                    bytemuck::bytes_of(&**sdl.buf),
                    OUTPUT_WIDTH as usize * 4,
                )
                .expect("Failed to copy bitmap to texture");

            sdl.canvas
                .copy(&sdl.tex, None, None)
                .expect("Failed to copy frame");

            sdl.canvas.present();
        })
    }

    fn draw_pixel(&mut self) {
        let pixel_color = match self.state.draw_pixel() {
            Some(c) => c as usize,
            None => return,
        };

        let pixdata = [
            self.state.palette_srgb[pixel_color][0],
            self.state.palette_srgb[pixel_color][1],
            self.state.palette_srgb[pixel_color][2],
            255,
        ];
        let x = self.state.dotnum - 1;
        let y = self.state.slnum;
        self.sdl.with_buf_mut(|buf| buf[y][x] = pixdata);
    }

    fn memfetch(&mut self) {
        if self.state.render_disabled() || matches!(self.state.slnum, 240..=260) {
            return;
        }

        match self.state.dotnum {
            0 => self.dummy_memfetch(),
            1..=256 => self.bg_memfetch(),
            257..=320 => self.sprite_memfetch(),
            321..=336 => self.bg_memfetch(),
            337..=340 => self.mysterious_memfetch(),
            341.. | _ => panic!(),
        }
    }

    fn dummy_memfetch(&mut self) {
        //self.state.vram_addr_bus = self.state.vram_addr.to_u16();
    }

    fn bg_memfetch(&mut self) {
        let st = &mut self.state;
        let bus = self.bus.borrow();
        match (st.dotnum - 1) % 8 {
            0 => st.vram_addr_bus = st.nt_addr(),
            1 => st.nt_latch = bus.read(st.vram_addr_bus),
            2 => st.vram_addr_bus = st.attr_addr(),
            3 => st.attr_latch = bus.read(st.vram_addr_bus),
            4 => st.vram_addr_bus = st.bg_bmp_addr(),
            5 => st.bmp_latch[0] = bus.read(st.vram_addr_bus),
            6 => st.vram_addr_bus = st.bg_bmp_addr() + 8,
            7 => st.bmp_latch[1] = bus.read(st.vram_addr_bus),
            _ => unreachable!(),
        }
    }

    fn sprite_memfetch(&mut self) {
        let st = &mut self.state;

        let spritenum = (st.dotnum - 257) / 8;
        let sprite = st.eval_sprites[spritenum];

        let mut bmp_addr: u16;
        let mut tile = sprite.tile;
        match st.flags.sprite_size() {
            SpriteSize::EightByEight => {
                bmp_addr = st.flags.sprite_chr_baseaddr().addr();
            }
            SpriteSize::EightBySixteen => {
                bmp_addr = 0x1000 * (tile as u16 & 0x1);
                tile &= !0x1;
            }
        }

        let mut y_offset = st.vram_addr.coarse_yscroll() * 8 + st.vram_addr.fine_yscroll();
        y_offset = y_offset.wrapping_sub(1).wrapping_sub(sprite.ypos);
        if st.flags.sprite_size() == SpriteSize::EightBySixteen {
            tile += (y_offset / 8) ^ sprite.attr.verti_flipped() as u8;
            y_offset %= 8;
        }

        if sprite.attr.verti_flipped() {
            y_offset = 7_u8.wrapping_sub(y_offset);
        }

        bmp_addr += tile as u16 * 16 + y_offset as u16;

        let bus = self.bus.borrow();
        match (st.dotnum - 1) % 8 {
            0 | 2 => st.vram_addr_bus = st.nt_addr(),
            1 | 3 => drop(bus.read(st.vram_addr_bus)),
            4 => st.vram_addr_bus = bmp_addr,
            5 => {
                let shiftreg = &mut st.sprite_bmp_shiftregs[spritenum][0];
                *shiftreg = bus.read(st.vram_addr_bus);
                if sprite.attr.horiz_flipped() {
                    *shiftreg = shiftreg.reverse_bits();
                }
                if spritenum >= st.eval_nsprites as usize {
                    *shiftreg = 0;
                }
            }
            6 => st.vram_addr_bus = bmp_addr + 8,
            7 => {
                let shiftreg = &mut st.sprite_bmp_shiftregs[spritenum][1];
                *shiftreg = bus.read(st.vram_addr_bus);
                if sprite.attr.horiz_flipped() {
                    *shiftreg = shiftreg.reverse_bits();
                }
                if spritenum >= st.eval_nsprites as usize {
                    *shiftreg = 0;
                }

                st.sprite_xs[spritenum] = sprite.xpos;
                st.sprite_attrs[spritenum] = sprite.attr;
            }
            _ => unreachable!(),
        }
    }

    fn mysterious_memfetch(&mut self) {
        let st = &mut self.state;
        match (st.dotnum - 1) % 4 {
            0 | 2 => st.vram_addr_bus = st.nt_addr(),
            1 | 3 => drop(self.bus.borrow().read(st.vram_addr_bus)),
            _ => unreachable!(),
        }
    }

    fn update_a12(&mut self) {
        let mut mmc3 = match &self.mmc3_irq {
            Some(x) => x.borrow_mut(),
            None => return,
        };

        let state = &self.state;

        let frame_cycle = (state.slnum * 341) + state.dotnum;
        let vram_addr = state.vram_addr_bus;
        if mmc3.a12_watcher.update_vram_addr(vram_addr, frame_cycle) != Edge::Rising {
            return;
        }

        if mmc3.counter == 0 || mmc3.reload {
            mmc3.counter = mmc3.latch;
        } else {
            mmc3.counter -= 1;
        }

        mmc3.reload = false;

        if mmc3.counter == 0 && mmc3.enabled {
            mmc3.line.raise();
        }
    }
}

impl Reset for Ppu {
    fn reset(&mut self) {
        self.state = PpuState {
            framenum: 0,
            clk_countdown: CLK_DIVISOR,

            slnum: 261,
            dotnum: 0,
            overflow_dotnum: 0,

            mask: Zeroable::zeroed(),

            flags: Zeroable::zeroed(),

            vram_addr: Zeroable::zeroed(),
            tmp_vram_addr: Zeroable::zeroed(),
            vram_read_buf: 0,

            fine_xscroll: 0,

            sprites: Zeroable::zeroed(),
            palette_mem: Zeroable::zeroed(),

            ..self.state
        }
    }
}

impl Timed for Ppu {
    fn fire(&mut self) {
        self.state.clk_countdown = CLK_DIVISOR;

        if self.state.slnum == 241 && self.state.dotnum == 1 {
            self.state.flags.set_vblank(true);
            if self.state.flags.nmi_en() {
                self.nmi_line.raise();
            }
            self.present_frame();
            self.state.decay_latch = 0;
        }

        self.state.set_delayed_regs();
        self.state.clear_regs();

        self.memfetch();
        self.update_a12();
        self.state.load_shiftregs();
        self.state.shift_shiftregs();
        self.state.update_vram_addr();
        self.state.spriteeval();

        self.draw_pixel();

        // TODO: should this be moved up and stuff shifted by 1 cycle?
        self.state.move_cursor();
    }

    fn countdown(&self) -> &u64 {
        &self.state.clk_countdown
    }

    fn countdown_mut(&mut self) -> &mut u64 {
        &mut self.state.clk_countdown
    }
}

impl MemRead for Ppu {
    fn read(&mut self, addr: u16, _lane_mask: &mut u8) -> u8 {
        let state = &mut self.state;
        match addr % 8 {
            0 | 1 | 3 | 5 | 6 | 0x14 => state.decay_latch,

            2 => {
                // PPUSTATUS
                let mut val = 0;
                val |= state.check_vblank() << 7;
                val |= (state.flags.sprite0_hit() as u8) << 6;
                val |= (state.flags.sprite_overflow() as u8) << 5;
                val |= state.decay_latch & 0b00011111;

                state.flags.set_write_toggle(false);

                val
            }

            4 => {
                // OAMDATA
                let mut v = state.oam()[state.oam_addr as usize];
                if state.oam_addr % 4 == 2 {
                    v &= 0b11100011;
                    state.decay_latch = v;
                }
                v
            }

            7 => {
                // PPUDATA
                let vram_addr = state.vram_addr.to_u16();
                let unmirrored_addr = match vram_addr {
                    0x3000..=0x3FFF => vram_addr - 0x1000,
                    _ => vram_addr,
                };
                let data = self.bus.borrow().read(unmirrored_addr);
                let buf_contents = std::mem::replace(&mut state.vram_read_buf, data);

                let val = match state.palette_loc(vram_addr) {
                    Some(x) => *x | (state.decay_latch & 0b11000000),
                    None => buf_contents,
                };

                state.inc_vram_addr_rw();
                state.decay_latch = val;
                val
            }

            _ => 0xFF,
        }
    }
}

impl MemWrite for Ppu {
    fn write(&mut self, addr: u16, val: u8) {
        let state = &mut self.state;
        state.decay_latch = val;
        match addr % 8 {
            0 => {
                // PPUCTRL
                let baseaddr = NtBaseAddr::new()
                    .with_h(val & 0x1 != 0)
                    .with_v(val & 0x2 != 0);
                state.tmp_vram_addr.set_nt_baseaddr(baseaddr);

                let inc = if val & 0x4 != 0 {
                    VramAddrInc::Inc32
                } else {
                    VramAddrInc::Inc1
                };
                state.flags.set_vram_addr_inc(inc);

                let sprite_base = if val & 0x8 != 0 {
                    ChrBaseAddr::Addr1000
                } else {
                    ChrBaseAddr::Addr0000
                };
                state.flags.set_sprite_chr_baseaddr(sprite_base);

                let bg_base = if val & 0x10 != 0 {
                    ChrBaseAddr::Addr1000
                } else {
                    ChrBaseAddr::Addr0000
                };
                state.flags.set_bg_chr_baseaddr(bg_base);

                let size = if val & 0x20 != 0 {
                    SpriteSize::EightBySixteen
                } else {
                    SpriteSize::EightByEight
                };
                state.flags.set_sprite_size(size);

                let old_nmi_en = state.flags.nmi_en();
                state.flags.set_nmi_en(val & 0x80 != 0);
                if state.flags.nmi_en() && !old_nmi_en && state.flags.vblank() {
                    self.nmi_line.raise();
                }
            }

            1 => state.mask.bytes[0] = val, // PPUMASK
            3 => state.oam_addr = val,      // OAMADDR
            4 => {
                // OAMDATA
                let addr = state.oam_addr as usize;
                state.oam_mut()[addr] = val;
                state.oam_addr += 1;
            }

            5 => {
                // PPUSCROLL
                state.flags.set_write_toggle(!state.flags.write_toggle());
                if state.flags.write_toggle() {
                    state.tmp_vram_addr.set_coarse_xscroll(val >> 3);
                    state.fine_xscroll = val & 0x7;
                } else {
                    state.tmp_vram_addr.set_fine_yscroll(val & 0x7);
                    state.tmp_vram_addr.set_coarse_yscroll(val >> 3);
                }
            }

            6 => {
                // PPUADDR
                state.flags.set_write_toggle(!state.flags.write_toggle());
                if state.flags.write_toggle() {
                    state.tmp_vram_addr.bytes[1] = val & 0x3F;
                } else {
                    state.tmp_vram_addr.bytes[0] = val;
                    state.vram_addr = state.tmp_vram_addr;
                    state.vram_addr_bus = state.vram_addr.to_u16();
                    self.update_a12();
                }
            }

            7 => {
                // PPUDATA
                let vram_addr = u16::from_le_bytes(state.vram_addr.bytes);
                if let Some(palloc) = state.palette_loc_mut(vram_addr) {
                    *palloc = val;
                } else {
                    let addr = if (0x3000..0x3F00).contains(&vram_addr) {
                        vram_addr - 0x1000
                    } else {
                        vram_addr
                    };
                    self.bus.borrow().write(addr, val);
                }
                self.state.inc_vram_addr_rw();
            }

            _ => (),
        }
    }
}
