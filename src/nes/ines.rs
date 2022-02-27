use std::fs::File;
use std::io::{self, Read, Seek};
use std::path::Path;

use bytemuck::Zeroable;
use sdl2::Sdl;

use crate::membus::BankSel;
use crate::memory::Memory;
use crate::mos6502::Mos6502;
use crate::reset_manager::ResetManager;
use crate::util::{r, R};

use super::{apu::Apu, io_reg::IoReg, mapper::Mapper, ppu::Ppu};

mod header;
pub use header::Mirroring;

pub struct RomInfo<'a> {
    pub rm: R<ResetManager>,
    pub cpu: &'a Mos6502,
    pub ppu: R<Ppu>,

    pub prg_rom: Memory,
    pub prg_ram: Option<Memory>,
    pub prg_nvram: Option<Memory>,
    pub chr: Memory,
    pub vram_sels: [BankSel; 4],
}

pub fn rom_load(
    mut f: File,
    sdl: &Sdl,
    rm: &R<ResetManager>,
    cpu: &mut Mos6502,
    palette_path: Option<&Path>,
    cscheme_path: Option<&Path>,
    scale: u32,
) -> io::Result<()> {
    let mut header = header::GenericHeader::zeroed();

    f.rewind()?;
    f.read_exact(bytemuck::bytes_of_mut(&mut header))?;
    let header = header.resolve();

    let e = |msg: &str| io::Error::new(io::ErrorKind::InvalidData, msg);

    header.validate().map_err(|m| e(&m))?;

    let mem = |size, w| {
        if size == 0 {
            None
        } else {
            Some(Memory::new(rm, size, w))
        }
    };

    let mut prg_rom = Memory::new(rm, header.prg_rom_size(), false);
    let prg_ram = mem(header.prg_ram_size(), true);
    let prg_nvram = mem(header.prg_nvram_size(), true);

    prg_rom.populate(&mut f)?;

    let chr = if header.chr_rom_size() != 0 {
        let mut rom = Memory::new(rm, header.chr_rom_size(), false);
        rom.populate(&mut f)?;
        rom
    } else {
        Memory::new(rm, header.chr_ram_size(), true)
    };

    let ppu = setup_common(sdl, rm, cpu, palette_path, cscheme_path, scale)?;
    let vram = Memory::new(rm, 0x0800, true);
    let vram_sels: [BankSel; 4] = Default::default();
    {
        match header.nt_mirroring() {
            Mirroring::Horizontal => vram_sels[2].set(0x0400),
            Mirroring::Vertical => vram_sels[1].set(0x0400),
        }
        vram_sels[3].set(0x0400);
        let ppu = ppu.borrow();
        let p_bus = &mut ppu.bus.borrow_mut();
        for i in 0..4 {
            let start = 0x2000 + 0x0400 * i as u16;
            vram.map_switchable(p_bus, start..=start + 0x3FF, &vram_sels[i]);
            let start = start + 0x1000;
            vram.map_switchable(p_bus, start..=start + 0x3FF, &vram_sels[i]);
        }
    }

    let info = RomInfo {
        rm: rm.clone(),
        cpu,
        ppu,
        prg_rom,
        prg_ram,
        prg_nvram,
        chr,
        vram_sels,
    };

    let mapper_id = header.mapper();
    Mapper::get(mapper_id)
        .ok_or_else(|| e(&format!("Unsupported mapper: {mapper_id}")))?
        .setup(info)
        .map_err(e)?;

    Ok(())
}

fn setup_common(
    sdl: &Sdl,
    rm: &R<ResetManager>,
    cpu: &mut Mos6502,
    palette_path: Option<&Path>,
    cscheme_path: Option<&Path>,
    scale: u32,
) -> io::Result<R<Ppu>> {
    let ram = Memory::new(rm, 0x0800, true);
    ram.map_mirroring(&mut *cpu.bus.borrow_mut(), 0x0000..=0x1FFF);

    let event_pump = r(sdl.event_pump().expect("Could not set up event pump"));

    let io_reg = IoReg::new(rm, cpu, sdl, event_pump.clone(), cscheme_path)?;

    let apu = Apu::new(sdl, rm, cpu);

    crate::nes::pageforty::setup(&mut cpu.bus.borrow_mut(), io_reg, apu);

    let ppu = Ppu::new(sdl, rm, cpu, event_pump, scale);

    {
        let mut ppu_ref = ppu.borrow_mut();
        let palette_data = bytemuck::bytes_of_mut(&mut ppu_ref.state.palette_srgb);
        if let Some(p) = palette_path {
            File::open(p)?.read_exact(palette_data)?;
        } else {
            palette_data.copy_from_slice(include_bytes!("../../res/palette"));
        }
    }

    Ok(ppu)
}
