#![allow(dead_code)]

use std::{cell::Cell, rc::Rc};

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;
use ouroboros::self_referencing;
use sdl2::{
    pixels::PixelFormatEnum,
    rect::Rect,
    render::{Texture, TextureCreator, WindowCanvas},
    video::WindowContext,
    EventPump, Sdl,
};

use crate::membus::{MemBus, MemRead, MemWrite};
use crate::mos6502::{Intr, Mos6502};
use crate::reset_manager::{Reset, ResetManager};
use crate::timekeeper::Timed;
use crate::{r, R};

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
        if self.fine_yscroll() != 3 {
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
}

pub struct Ppu {
    pub bus: R<MemBus>,
    intr: Rc<Cell<Intr>>,

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
}

impl PpuState {
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
        let fl = &mut self.flags;
        if self.dotnum == 1 && self.slnum == 261 {
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
            1 | 321 | 257..=320 => return,
            x if x.wrapping_sub(1) % 8 != 0 => return,
            _ => (),
        }

        self.bg_bmp_shiftregs[0] &= 0xFF00;
        self.bg_bmp_shiftregs[1] &= 0xFF00;

        self.bg_bmp_shiftregs[0] |= self.bmp_latch[0] as u16;
        self.bg_bmp_shiftregs[1] |= self.bmp_latch[1] as u16;

        let attr_x = (self.vram_addr.coarse_xscroll() / 2) % 2;
        let attr_y = (self.vram_addr.coarse_yscroll() / 2) % 2;
        let attr_idx = attr_y * 2 + attr_x;
        let attr_shift = attr_idx * 2;

        self.bg_palette = (self.attr_latch >> attr_shift) & 0x3;
    }

    fn update_vram_addr(&mut self) {
        if !(self.mask.bg_en() || self.mask.sprite_en()) {
            return;
        }
        if self.slnum >= 240 && self.slnum != 261 {
            return;
        }

        if self.slnum < 240 {
            if (237..=257).contains(&self.dotnum) && self.dotnum != 1 && (self.dotnum - 1) % 8 == 0
            {
                self.vram_addr.inc_coarse_x();
                if self.dotnum == 257 {
                    self.vram_addr.inc_y();
                }
            } else if self.dotnum == 258 {
                self.vram_addr.copy_h(self.tmp_vram_addr);
            }
        } else if self.slnum == 261 && (280..=304).contains(&self.dotnum) {
            self.vram_addr.copy_v(self.tmp_vram_addr);
        }
    }

    fn spriteeval(&mut self) {
        if !(self.mask.bg_en() || self.mask.sprite_en()) || self.dotnum != 0 || self.slnum >= 240 {
            return;
        }

        bytemuck::bytes_of_mut(&mut self.eval_sprites).fill(0xFF);
        self.eval_nsprites = 0;

        self.flags
            .set_scanline_has_sprite0(self.flags.next_scanline_has_sprite0());
        self.flags.set_next_scanline_has_sprite0(false);

        let sprite_height = self.flags.sprite_size().height();

        let mut sprites = self.sprites.iter().enumerate();

        for (i, sprite) in sprites.by_ref() {
            if self.eval_nsprites == 8 {
                break;
            }
            if self.slnum < sprite.ypos as usize
                || self.slnum >= (sprite.ypos + sprite_height) as usize
            {
                continue;
            }

            if i == 0 {
                self.flags.set_next_scanline_has_sprite0(true);
            }

            self.eval_sprites[self.eval_nsprites as usize] = *sprite;
            self.eval_nsprites += 1;
        }

        for (i, sprite) in sprites {
            if self.slnum >= sprite.ypos as usize
                || self.slnum < (sprite.ypos + sprite_height) as usize
            {
                self.overflow_dotnum = 8 * 8 + (i + 1 - 8) * 2;
                break;
            }
        }
    }

    fn draw_pixel(&mut self) -> Option<u16> {
        let m = &self.mask;

        if !(m.bg_en() || m.sprite_en()) || !(1..=256).contains(&self.dotnum) || self.slnum >= 240 {
            return None;
        }

        let mut bg_color = 0;
        bg_color |= ((self.bg_bmp_shiftregs[0] << self.fine_xscroll) >> 15) & 1;
        bg_color |= ((self.bg_bmp_shiftregs[1] << self.fine_xscroll) >> 14) & 2;

        let mut bg_palette = 0;
        bg_palette |= ((self.bg_attr_shiftregs[0] << self.fine_xscroll) >> 7) & 1;
        bg_palette |= ((self.bg_attr_shiftregs[1] << self.fine_xscroll) >> 6) & 2;

        for x in &mut self.sprite_xs {
            *x = x.saturating_sub(1);
        }

        let mut sprite_color = 0;
        let mut sprite_palette = 0;
        let mut sprite_behind_bg = false;
        let mut is_sprite0 = false;

        for i in 0..8 {
            if self.sprite_xs[i] > 0 {
                continue;
            }

            if sprite_color == 0 {
                sprite_color |= ((self.sprite_bmp_shiftregs[i][0] << self.fine_xscroll) >> 7) & 1;
                sprite_color |= ((self.sprite_bmp_shiftregs[i][1] << self.fine_xscroll) >> 6) & 2;
                sprite_palette = self.sprite_attrs[i].palette();
                sprite_behind_bg = self.sprite_attrs[i].behind_bg();
                is_sprite0 = i == 0 && self.flags.scanline_has_sprite0();
            }

            self.sprite_bmp_shiftregs[i][0] <<= 1;
            self.sprite_bmp_shiftregs[i][1] <<= 1;
        }

        if (bg_color, sprite_color) != (0, 0) {
            println!("{bg_color} {sprite_color}");
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

        if paladdr != 16128 {
            println!("{paladdr}");
        }
        let mut pixel_color = *self.palette_loc(paladdr).unwrap() as u16;
        pixel_color |= (self.mask.emph_red() as u16) << 6;
        pixel_color |= (self.mask.emph_green() as u16) << 7;
        pixel_color |= (self.mask.emph_blue() as u16) << 8;

        Some(pixel_color)
    }

    fn move_cursor(&mut self) {
        let sl_length = if self.slnum == 261 && self.framenum % 2 != 0 {
            340
        } else {
            341
        };

        self.dotnum += 1;
        if self.dotnum != sl_length {
            return;
        }
        self.dotnum = 0;

        self.slnum += 1;
        if self.slnum != 262 {
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
        if !(self.mask.bg_en() || self.mask.sprite_en()) || (self.slnum >= 240 && self.slnum != 261)
        {
            let mut vra = self.vram_addr.to_u16();
            vra += self.flags.vram_addr_inc().amount();
            vra %= 0x4000;
            self.vram_addr.set(vra);
            return;
        }
        self.vram_addr.inc_coarse_x();
        self.vram_addr.inc_y();
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
}

impl Ppu {
    pub fn new(
        sdl: &Sdl,
        rm: &R<ResetManager>,
        cpu: &R<Mos6502>,
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
        let ppu_sdl = PpuSdl::new(canvas, tex_cre, |tc| {
            tc.create_texture_streaming(PixelFormatEnum::RGBA4444, OUTPUT_WIDTH, OUTPUT_HEIGHT)
                .expect("Could not create texture")
        });

        let cpu = cpu.borrow_mut();

        let ppu = r(Ppu {
            bus,
            intr: cpu.intr_status.clone(),
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
        if pixel_color != 0 {
            println!("{}", pixel_color);
        }
        let pixdata = [
            self.state.palette_srgb[pixel_color][0],
            self.state.palette_srgb[pixel_color][1],
            self.state.palette_srgb[pixel_color][2],
            255,
        ];
        if pixdata != [0, 0, 0, 255] {
            println!("{:?}", pixdata);
        }
        let rect = Rect::new(self.state.slnum as i32, self.state.dotnum as i32, 1, 1);
        self.sdl
            .with_tex_mut(|tex| tex.update(rect, &pixdata, 4))
            .expect("Couldn't blit pixel");
    }

    fn memfetch(&mut self) {
        if !(self.state.mask.bg_en() || self.state.mask.sprite_en())
            || (self.state.slnum >= 240 && self.state.slnum != 261)
        {
            return;
        }
        self.bg_memfetch();
        self.sprite_memfetch();
        self.dummy_memfetch();
    }

    fn bg_memfetch(&mut self) {
        if !matches!(self.state.dotnum, 1..=256 | 231..=336) {
            return;
        }

        let (src, dst) = match (self.state.dotnum - 1) % 8 {
            1 => (self.state.nt_addr(), &mut self.state.nt_latch),
            3 => (self.state.attr_addr(), &mut self.state.attr_latch),
            5 => (self.state.bg_bmp_addr(), &mut self.state.bmp_latch[0]),
            7 => (self.state.bg_bmp_addr() + 8, &mut self.state.bmp_latch[1]),
            _ => return,
        };

        *dst = self.bus.borrow_mut().read(src);
    }

    fn sprite_memfetch(&mut self) {
        let st = &mut self.state;

        if !matches!(st.dotnum, 257..=320) {
            return;
        }

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

        let mut y_offset =
            st.vram_addr.coarse_yscroll() * 8 + st.vram_addr.fine_yscroll() - 1 - sprite.ypos;
        if st.flags.sprite_size() == SpriteSize::EightBySixteen {
            tile += (y_offset / 8) ^ sprite.attr.verti_flipped() as u8;
            y_offset %= 8;
        }

        if sprite.attr.verti_flipped() {
            y_offset = 7 - y_offset;
        }

        bmp_addr += tile as u16 * 16 + y_offset as u16;

        let mut bus = self.bus.borrow_mut();
        match (st.dotnum - 1) % 8 {
            1 => {
                bus.read(st.nt_addr());
            }
            3 => {
                bus.read(st.attr_addr());
            }
            5 => {
                let shiftreg = &mut st.sprite_bmp_shiftregs[spritenum][0];
                *shiftreg = bus.read(bmp_addr);
                if sprite.attr.horiz_flipped() {
                    *shiftreg = shiftreg.reverse_bits();
                }
                if spritenum >= st.eval_nsprites as usize {
                    *shiftreg = 0;
                }
            }
            7 => {
                let shiftreg = &mut st.sprite_bmp_shiftregs[spritenum][1];
                *shiftreg = bus.read(bmp_addr + 8);
                if sprite.attr.horiz_flipped() {
                    *shiftreg = shiftreg.reverse_bits();
                }
                if spritenum >= st.eval_nsprites as usize {
                    *shiftreg = 0;
                }

                st.sprite_xs[spritenum] = sprite.xpos;
                st.sprite_attrs[spritenum] = sprite.attr;
            }
            _ => (),
        }
    }

    fn dummy_memfetch(&mut self) {
        // "todo"
    }
}

impl Reset for Ppu {
    fn reset(&mut self) {
        self.state = PpuState {
            clk_countdown: CLK_DIVISOR,
            slnum: 261,
            ..Zeroable::zeroed()
        }
    }
}

impl Timed for Ppu {
    fn fire(&mut self) {
        self.state.clk_countdown = CLK_DIVISOR;

        if self.state.slnum == 241 && self.state.dotnum == 1 {
            self.state.flags.set_vblank(true);
            if self.state.flags.nmi_en() {
                self.intr.set(Intr::Nmi);
            }
            self.present_frame();
        }

        self.state.set_delayed_regs();
        self.state.clear_regs();

        self.state.shift_shiftregs();
        self.state.load_shiftregs();
        self.state.update_vram_addr();
        self.state.spriteeval();

        self.draw_pixel();

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
            2 => {
                // PPUSTATUS
                let mut val = 0;
                val |= (state.flags.vblank() as u8) << 7;
                val |= (state.flags.sprite0_hit() as u8) << 6;
                val |= (state.flags.sprite_overflow() as u8) << 5;

                state.flags.set_write_toggle(false);
                state.flags.set_vblank(false);

                val
            }

            4 => state.oam()[state.oam_addr as usize], // OAMDATA

            7 => {
                // PPUDATA
                let vram_addr = state.vram_addr.to_u16();
                let val = if let Some(palloc) = state.palette_loc(vram_addr) {
                    *palloc
                } else {
                    let addr = if (0x3000..0x3F00).contains(&vram_addr) {
                        vram_addr - 0x1000
                    } else {
                        vram_addr
                    };
                    std::mem::replace(&mut state.vram_read_buf, self.bus.borrow_mut().read(addr))
                };
                state.inc_vram_addr_rw();
                val
            }

            _ => 0xFF,
        }
    }
}

impl MemWrite for Ppu {
    fn write(&mut self, addr: u16, val: u8) {
        let state = &mut self.state;
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
                    ChrBaseAddr::Addr0000
                } else {
                    ChrBaseAddr::Addr1000
                };
                state.flags.set_sprite_chr_baseaddr(sprite_base);

                let bg_base = if val & 0x10 != 0 {
                    ChrBaseAddr::Addr0000
                } else {
                    ChrBaseAddr::Addr1000
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
                if state.flags.nmi_en() != old_nmi_en && state.flags.vblank() {
                    self.intr.set(Intr::Nmi);
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
                if !state.flags.write_toggle() {
                    state.tmp_vram_addr.set_coarse_xscroll(val >> 3);
                    state.fine_xscroll = val & 0x7;
                } else {
                    state.tmp_vram_addr.set_fine_yscroll(val & 0x7);
                    state.tmp_vram_addr.set_coarse_yscroll(val >> 3);
                }
                state.flags.set_write_toggle(!state.flags.write_toggle());
            }

            6 => {
                // PPUADDR
                if !state.flags.write_toggle() {
                    state.tmp_vram_addr.bytes[1] = val & 0x3F;
                } else {
                    state.tmp_vram_addr.bytes[0] = val;
                }
                state.flags.set_write_toggle(!state.flags.write_toggle());
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
                    self.bus.borrow_mut().write(addr, val);
                }
                self.state.inc_vram_addr_rw();
            }

            _ => (),
        }
    }
}
