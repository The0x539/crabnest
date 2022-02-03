use std::fs::File;
use std::io::{self, Read, Seek};
use std::path::Path;

use bytemuck::Zeroable;
use sdl2::Sdl;

use crate::memory::Memory;
use crate::mos6502::Mos6502;
use crate::nes::apu::Apu;
use crate::nes::io_reg::IoReg;
use crate::nes::ppu::Ppu;
use crate::reset_manager::ResetManager;
use crate::{r, R};

mod header;
pub use header::Mirroring;
use header::Nes20Header;

pub struct RomInfo {
    pub rm: R<ResetManager>,
    pub cpu: R<Mos6502>,
    pub ppu: R<Ppu>,
    pub mirroring: Mirroring,

    pub prg_rom: R<Memory>,
    pub prg_ram: Option<R<Memory>>,
    pub prg_nvram: Option<R<Memory>>,
    pub chr: R<Memory>,
    pub vram: R<Memory>,
}

pub fn rom_load(
    mut f: File,
    sdl: &Sdl,
    rm: &R<ResetManager>,
    cpu: &R<Mos6502>,
    palette_path: &Path,
    cscheme_path: &Path,
    scale: u32,
) -> io::Result<()> {
    let mut header = Nes20Header::zeroed();

    f.rewind()?;
    f.read_exact(bytemuck::bytes_of_mut(&mut header))?;
    let header = header;

    let e = |msg: &str| io::Error::new(io::ErrorKind::InvalidData, msg);

    header.validate().map_err(|m| e(&m))?;

    let mem = |size, w| {
        if size == 0 {
            None
        } else {
            Some(Memory::new(rm, size, w))
        }
    };

    let prg_rom = mem(header.prg_rom_size(), false).unwrap();
    let prg_ram = mem(header.prg_ram_size(), true);
    let prg_nvram = mem(header.prg_nvram_size(), true);

    let chr = if header.chr_rom_size() != 0 {
        Memory::new(rm, header.chr_rom_size(), false)
    } else {
        Memory::new(rm, header.chr_ram_size(), true)
    };

    let vram = Memory::new(rm, 0x0800, true);

    let ppu = setup_common(sdl, rm, cpu, palette_path, cscheme_path, scale)?;

    f.read_exact(&mut prg_rom.borrow_mut().bytes)?;

    let info = RomInfo {
        rm: rm.clone(),
        cpu: cpu.clone(),
        mirroring: header.nt_mirroring(),
        ppu,
        prg_rom,
        prg_ram,
        prg_nvram,
        chr,
        vram,
    };

    use crate::nes::{nrom, sxrom};

    match header.mapper() {
        0 => nrom::setup(info).map_err(e),
        1 => sxrom::setup(info).map_err(e),
        _ => unreachable!(),
    }
}

fn setup_common(
    sdl: &Sdl,
    rm: &R<ResetManager>,
    cpu: &R<Mos6502>,
    palette_path: &Path,
    cscheme_path: &Path,
    scale: u32,
) -> io::Result<R<Ppu>> {
    {
        let cpu_ref = cpu.borrow();
        let bus = &*cpu_ref.bus.borrow();

        let ram = Memory::new(rm, 0x0800, true);
        Memory::map(&ram, bus, 0x0000, ram.borrow().size() as u16, 0x0000);

        Memory::map_mirroring(&ram, bus, 0x0800, 0x0800, 0x0000, 3);
    }

    let event_pump = r(sdl.event_pump().expect("Could not set up event pump"));

    let io_reg = IoReg::new(rm, cpu, event_pump.clone(), cscheme_path)?;

    let apu = Apu::new(sdl, rm, cpu);

    crate::nes::pageforty::setup(&*cpu.borrow().bus.borrow(), io_reg, apu);

    let ppu = Ppu::new(sdl, rm, cpu, event_pump, scale);

    let mut f = File::open(palette_path)?;
    f.read_exact(bytemuck::bytes_of_mut(
        &mut ppu.borrow_mut().state.palette_srgb,
    ))?;

    Ok(ppu)
}
