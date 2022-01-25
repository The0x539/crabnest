#![allow(dead_code)]

use std::fs::File;
use std::io::{self, Read};
use std::mem::size_of;
use std::path::Path;

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;
use sdl2::Sdl;

use crate::memory::Memory;
use crate::mos6502::Mos6502;
use crate::nes::io_reg::IoReg;
use crate::nes::ppu::Ppu;
use crate::reset_manager::ResetManager;
use crate::R;

#[derive(BitfieldSpecifier, PartialEq)]
#[bits = 1]
pub enum TvType {
    Ntsc = 0,
    Pal = 1,
}

#[derive(BitfieldSpecifier, PartialEq)]
#[bits = 1]
pub enum Mirroring {
    Horizontal = 0,
    Vertical = 1,
}

pub struct RomInfo {
    rm: R<ResetManager>,
    pub cpu: R<Mos6502>,
    pub ppu: R<Ppu>,
    pub mirroring: Mirroring,

    pub wram: Option<R<Memory>>,
    pub prgrom: Option<R<Memory>>,
    pub chrom: Option<R<Memory>>,
    pub chram: Option<R<Memory>>,
    pub vram: R<Memory>,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Flags6 {
    mirroring: Mirroring,
    wram_present: B1,
    trainer_present: B1,
    four_screen_vram: B1,
    mapper_nib_low: B4,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Flags7 {
    vs_unisystem: B1,
    playchoice_10: B1,
    version: B2,
    mapper_nib_high: B4,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines1Flags9 {
    tv_type: TvType,
    reserved: B7,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines2Flags8 {
    submapper: B4,
    mapper_nib_higher: B4,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines2Flags9 {
    chrom_size_nib: B4,
    prgrom_size_nib: B4,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines2Flags10 {
    nbb_wram_size: B4,
    bb_wram_size: B4,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines2Flags11 {
    nbb_chram_size: B4,
    bb_chram_size: B4,
}

#[bitfield(bits = 8)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines2Flags12 {
    tv_type: TvType,
    universal: B1,
    reserved: B6,
}

#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct CommonHeader {
    prgrom_size: u8,
    chrom_size: u8,
    flags6: Flags6,
    flags7: Flags7,
}

#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines1Header {
    wram_size: u8,
    flags9: Ines1Flags9,
    ignored: [u8; 6],
}

#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Ines2Header {
    flags8: Ines2Flags8,
    flags9: Ines2Flags9,
    flags10: Ines2Flags10,
    flags11: Ines2Flags11,
    flags12: Ines2Flags12,
    ignored: [u8; 3],
}

pub fn rom_load(
    mut f: File,
    path: &Path,
    sdl: &Sdl,
    rm: &R<ResetManager>,
    cpu: &R<Mos6502>,
    palette_path: &Path,
    cscheme_path: &Path,
    scale: u32,
) -> io::Result<()> {
    const HEADER_SIZE: usize = size_of::<CommonHeader>() + size_of::<Ines1Header>();

    let mut header = [0; HEADER_SIZE];
    f.read_exact(&mut header)?;

    let (common_bytes, versioned_bytes) = header.split_at(size_of::<CommonHeader>());
    let common: &CommonHeader = bytemuck::from_bytes(common_bytes);
    let ines1: &Ines1Header = bytemuck::from_bytes(versioned_bytes);
    let ines2: &Ines2Header = bytemuck::from_bytes(versioned_bytes);

    let version = common.flags7.version();

    let mut mapper = common.flags6.mapper_nib_low() as usize;
    mapper += (common.flags7.mapper_nib_high() as usize) << 4;
    if version == 2 {
        mapper += (ines2.flags8.mapper_nib_higher() as usize) << 8;
    }

    let mut prgrom_size = 16384 * common.prgrom_size as usize;
    let mut chrom_size = 8192 * common.chrom_size as usize;
    if version == 2 {
        prgrom_size += ((ines2.flags9.prgrom_size_nib() as usize) << 8) * 16384;
        chrom_size += ((ines2.flags9.chrom_size_nib() as usize) << 8) * 8192;
    }

    let wram_size: usize;
    let chram_size: usize;
    if version == 2 {
        wram_size = decode_ram_size(ines2.flags10.nbb_wram_size() as usize)
            + decode_ram_size(ines2.flags10.bb_wram_size() as usize);
        chram_size = decode_ram_size(ines2.flags11.nbb_chram_size() as usize)
            + decode_ram_size(ines2.flags11.bb_chram_size() as usize);
    } else {
        if common.flags6.wram_present() != 0 {
            if ines1.wram_size != 0 {
                wram_size = 8192 * ines1.wram_size as usize;
            } else {
                wram_size = 8192;
            }
        } else {
            wram_size = 0;
        }

        if chrom_size == 0 {
            chram_size = 8192;
        } else {
            chram_size = 0;
        }
    }

    if common.flags6.trainer_present() != 0 {
        panic!("{path:?} requires trainer support");
    } else if common.flags7.vs_unisystem() != 0 {
        panic!("{path:?} requires Vs. Unisystem support");
    } else if common.flags7.playchoice_10() != 0 {
        panic!("{path:?} requires Playchoice 10 support");
    }

    if if version == 2 {
        ines2.flags12.universal() == 0 && ines2.flags12.tv_type() != TvType::Ntsc
    } else {
        ines1.flags9.tv_type() != TvType::Ntsc
    } {
        eprintln!("WARNING: {path:?} expects a PAL system");
    }

    let ppu = setup_common(sdl, rm, cpu, palette_path, cscheme_path, scale)?;

    let mut info = RomInfo {
        rm: rm.clone(),
        cpu: cpu.clone(),
        mirroring: common.flags6.mirroring(),
        ppu,
        wram: None,
        prgrom: None,
        chrom: None,
        chram: None,
        vram: Memory::new(rm, 0x0800, true),
    };

    if wram_size > 0 {
        info.wram = Some(Memory::new(rm, wram_size, true));
    }

    if prgrom_size > 0 {
        let prgrom = Memory::new(rm, prgrom_size, false);
        f.read_exact(&mut prgrom.borrow_mut().bytes)?;
        info.prgrom = Some(prgrom);
    }

    if chrom_size > 0 {
        let chrom = Memory::new(rm, chrom_size, false);
        f.read_exact(&mut chrom.borrow_mut().bytes)?;
        info.chrom = Some(chrom);
    }

    if chram_size > 0 {
        info.chram = Some(Memory::new(rm, chram_size, true));
    }

    use crate::nes::{nrom, sxrom};

    let e = |msg: &str| io::Error::new(io::ErrorKind::InvalidData, msg);

    match mapper {
        0 => nrom::setup(&mut info).map_err(e),
        1 => sxrom::setup(&mut info).map_err(e),
        _ => Err(io::Error::new(
            io::ErrorKind::Unsupported,
            format!("{path:?} requires mapper #{mapper}; only mappers #0 and #1 are supported"),
        )),
    }
}

fn decode_ram_size(encoded: usize) -> usize {
    if encoded == 0 {
        0
    } else {
        64 << encoded
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
        let bus = &mut *cpu_ref.bus.borrow_mut();

        let ram = Memory::new(rm, 0x0800, true);
        Memory::map(&ram, bus, 0x0000, ram.borrow().size() as u16, 0x0000);

        Memory::map_mirroring(&ram, bus, 0x0800, 0x0800, 0x0000, 3);
    }

    IoReg::setup(sdl, rm, cpu, cscheme_path)?;

    let ppu = Ppu::new(sdl, rm, cpu, scale);
    Ppu::map(&ppu);

    let mut f = File::open(palette_path)?;
    f.read_exact(bytemuck::bytes_of_mut(
        &mut ppu.borrow_mut().state.palette_srgb,
    ))?;

    Ok(ppu)
}
