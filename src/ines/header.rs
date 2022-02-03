#![allow(dead_code, redundant_semicolons)]

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;

#[derive(BitfieldSpecifier, PartialEq)]
pub enum Mirroring {
    Horizontal = 0,
    Vertical = 1,
}

#[derive(BitfieldSpecifier)]
pub enum ConsoleType {
    Nes = 0,
    VsSystem = 1,
    Playchoice10 = 2,
    Extended = 3,
}

#[derive(BitfieldSpecifier)]
pub enum TimingMode {
    Ntsc = 0,
    Pal = 1,
    Universal = 2,
    Dendy = 3,
}

#[bitfield(bytes = 16)]
#[derive(Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Nes20Header {
    // bytes 0-3
    cookie: u32,

    // byte 4
    prg_rom_size_lo: u8,

    // byte 5
    chr_rom_size_lo: u8,

    // byte 6
    pub nt_mirroring: Mirroring,
    pub battery_present: bool,
    pub trainer_present: bool,
    pub four_screen_vram: bool,
    mapper_lo: B4,

    // byte 7
    pub console_type: ConsoleType,
    pub header_version: B2,
    mapper_mid: B4,

    // byte 8
    mapper_hi: B4,
    pub submapper: B4,

    // byte 9
    prg_rom_size_hi: B4,
    chr_rom_size_hi: B4,

    // byte 10
    prg_ram_shift_count: B4,
    prg_nvram_shift_count: B4,

    // byte 11
    chr_ram_shift_count: B4,
    chr_nvram_shift_count: B4,

    // byte 12
    pub timing_mode: TimingMode,
    #[skip]
    __: B6,

    // byte 13
    pub vs_ppu_type: B4,
    pub vs_hw_type: B4,

    // byte 14
    pub misc_rom_count: B2,
    #[skip]
    __: B6,

    // byte 15
    pub default_expansion_device: B6,
    #[skip]
    __: B2,
}

fn shift_mem(n: u8) -> usize {
    if n == 0 {
        0
    } else {
        0x40 << n
    }
}

impl Nes20Header {
    const IDENTIFICATION_STRING: [u8; 4] = *b"NES\x1a";
    const VERSION: u8 = 2;

    pub fn validate(&self) -> Result<(), String> {
        macro_rules! e {
            ($($x:tt)*) => {
                return Err(format!($($x)*))
            }
        }

        let cookie = self.cookie().to_ne_bytes();
        if cookie != Self::IDENTIFICATION_STRING {
            e!(
                "Invalid identification string: {cookie:?} (expected {:?})",
                Self::IDENTIFICATION_STRING,
            );
        }

        let version = self.header_version();
        if version != Self::VERSION {
            e!(
                "Invalid header version: {version:?} (expected {:?})",
                Self::VERSION,
            );
        }

        if self.trainer_present() {
            e!("Unsupported feature: trainer section");
        }

        match self.console_type() {
            ConsoleType::Nes => (),
            ConsoleType::VsSystem => e!("Unsupported console type: Vs. System"),
            ConsoleType::Playchoice10 => e!("Unsupported console type: Playchoice 10"),
            ConsoleType::Extended => e!("Unsupported console type: extended"),
        }

        match self.timing_mode() {
            TimingMode::Ntsc | TimingMode::Universal => (),
            TimingMode::Pal => e!("Unsupported timing mode: PAL"),
            TimingMode::Dendy => e!("Unsupported timing mode: Dendy"),
        }

        match self.mapper() {
            0 | 1 => (),
            n => e!("Unsupported mapper: {n} (supported: 0, 1)"),
        }

        if self.prg_nvram_size() != 0 && !self.battery_present() {
            e!("ROM specifies nonzero PRG-NVRAM, but does not set Battery bit");
        }

        if self.chr_nvram_size() != 0 {
            e!("Unsupported feature: CHR-NVRAM");
        }

        match (self.chr_rom_size(), self.chr_ram_size()) {
            (0, 0) => e!("ROM has no CHR"),
            (0, _) | (_, 0) => (),
            (_, _) => e!("Unsupported feature: CHR-ROM + CHR-RAM"),
        }

        Ok(())
    }

    pub fn mapper(&self) -> u16 {
        let hi = self.mapper_hi() as u16;
        let mid = self.mapper_mid() as u16;
        let lo = self.mapper_lo() as u16;
        hi << 8 | mid << 4 | lo
    }

    pub fn prg_rom_size(&self) -> usize {
        let hi = self.prg_rom_size_hi() as usize;
        let lo = self.prg_rom_size_lo() as usize;
        (hi << 8 | lo) * 0x4000
    }

    pub fn chr_rom_size(&self) -> usize {
        let lo = self.chr_rom_size_lo() as usize;
        let hi = self.chr_rom_size_hi() as usize;

        let units = if hi == 0xF {
            let multiplier = lo & 0b11;
            let exponent = lo >> 2;
            (1 << exponent) * (multiplier * 2 + 1)
        } else {
            hi << 8 | lo
        };

        0x2000 * units
    }

    pub fn prg_ram_size(&self) -> usize {
        shift_mem(self.prg_ram_shift_count())
    }

    pub fn chr_ram_size(&self) -> usize {
        shift_mem(self.chr_ram_shift_count())
    }

    pub fn prg_nvram_size(&self) -> usize {
        shift_mem(self.prg_nvram_shift_count())
    }

    pub fn chr_nvram_size(&self) -> usize {
        shift_mem(self.chr_nvram_shift_count())
    }
}