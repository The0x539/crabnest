use crate::{
    membus::{BankSel, MemWrite},
    mos6502,
    reset_manager::Reset,
    timekeeper::Timekeeper,
    util::{r, R},
};

use super::mmc1::{Mirroring, Mmc1};
use crate::nes::ines::RomInfo;

mapper!(001, setup);

struct SxRom {
    mmc1: Mmc1,
    tk: R<Timekeeper>,

    prg_sels: [BankSel; 2],
    vram_sels: [BankSel; 4],
    chr_sels: [BankSel; 2],

    prg_rom_size: usize,
    chr_size: usize,
}

impl SxRom {
    fn remap(&mut self) {
        fn set<const N: usize>(sels: &[BankSel; N], vals: [u8; N], unit: usize, len: usize) {
            for (s, v) in sels.iter().zip(vals) {
                s.set((v as usize * unit) % len);
            }
        }

        let regs = &self.mmc1.reg;

        // VRAM nametable mirroring
        {
            let vals = match regs.0.mirroring() {
                Mirroring::OneScreenNt0 => [0, 0, 0, 0],
                Mirroring::OneScreenNt1 => [1, 1, 1, 1],
                Mirroring::Vertical => [0, 1, 0, 1],
                Mirroring::Horizontal => [0, 0, 1, 1],
            };
            set(&self.vram_sels, vals, 0x0400, 0x0800);
        }

        // Switchable PRG ROM banks
        {
            use super::mmc1::{PrgromFixation::*, PrgromSwitching::*};
            let (bs16k, bs32k) = (regs.3.banksel16k(), regs.3.banksel32k());
            let last = self.prg_rom_size / 0x4000 - 1;
            let vals = match (regs.0.prgrom_switching(), regs.0.prgrom_fixation()) {
                (ThirtyTwoK, _) => [bs32k * 2, bs32k * 2 + 1],
                (SixteenK, Low) => [0, bs16k],
                (SixteenK, High) => [bs16k, last as u8],
            };
            set(&self.prg_sels, vals, 0x4000, self.prg_rom_size);
        }

        // Switchable CHR banks
        {
            use super::mmc1::ChrSwitching::*;
            let bs8k = regs.1.banksel8k();
            let vals = match regs.0.chr_switching() {
                EightK => [bs8k * 2, bs8k * 2 + 1],
                FourK => [regs.1.banksel4k(), regs.2.banksel4k()],
            };
            set(&self.chr_sels, vals, 0x1000, self.chr_size);
        }
    }
}

fn setup(info: RomInfo<'_>) -> Result<(), &'static str> {
    let RomInfo {
        rm,
        cpu,
        ppu,
        prg_rom,
        prg_ram,
        chr,
        vram_sels,

        prg_nvram: _,
    } = info;

    if prg_rom.size() % 0x4000 != 0 {
        return Err("ROM's PRGROM size is not a multiple of 16384");
    }

    if chr.size() % 0x2000 != 0 {
        return Err("ROM's CHR size is not a multiple of 8192");
    }

    let cpu_bus = &mut *cpu.bus.borrow_mut();

    if let Some(prg_ram) = &prg_ram {
        prg_ram.map(cpu_bus, 0x6000..=0x7FFF, 0);
    }

    // TODO: actual default bank selections

    let prg_sels: [BankSel; 2] = Default::default();
    prg_rom.map_switchable(cpu_bus, 0x8000..=0xBFFF, &prg_sels[0]);
    prg_rom.map_switchable(cpu_bus, 0xC000..=0xFFFF, &prg_sels[1]);

    let ppu = ppu.borrow();
    let ppu_bus = &mut *ppu.bus.borrow_mut();

    let chr_sels: [BankSel; 2] = Default::default();
    chr.map_switchable(ppu_bus, 0x0000..=0x0FFF, &chr_sels[0]);
    chr.map_switchable(ppu_bus, 0x1000..=0x1FFF, &chr_sels[1]);

    let cart = r(SxRom {
        mmc1: Mmc1::default(),

        prg_sels,
        vram_sels,
        chr_sels,

        prg_rom_size: prg_rom.size(),
        chr_size: chr.size(),

        tk: cpu.tk.clone(),
    });

    rm.borrow_mut().add_device(&cart);

    for i in 0..0x80 {
        cpu_bus.set_write_handler(0x80 + i, &cart, i << 8);
    }

    Ok(())
}

impl MemWrite for SxRom {
    fn write(&mut self, addr: u16, val: u8) {
        self.mmc1.reg_write(
            addr as usize / 0x2000,
            val,
            self.tk.borrow().clk_cyclenum / mos6502::CLK_DIVISOR,
        );
        self.remap();
    }
}

impl Reset for SxRom {
    fn reset(&mut self) {
        self.mmc1.reset();
        self.remap();
    }
}
