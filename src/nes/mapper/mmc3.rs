#![allow(dead_code)]

use modular_bitfield::prelude::*;

use crate::{
    membus::{BankSel, MemWrite},
    mos6502::IrqLine,
    nes::{a12_watcher::A12Watcher, ines::RomInfo},
    reset_manager::Reset,
    {r, R},
};

mapper!(004, setup);

#[derive(BitfieldSpecifier)]
enum A12Inversion {
    TwoFour = 0,
    FourTwo = 1,
}

#[derive(BitfieldSpecifier)]
enum PrgRomFixation {
    FixC0 = 0,
    Fix80 = 1,
}

#[bitfield]
struct BankSelect {
    reg: B3,
    #[skip]
    __: B3,
    prg: PrgRomFixation,
    chr: A12Inversion,
}

#[bitfield]
struct PrgRamProtection {
    #[skip]
    __: B6,
    deny_writes: bool,
    enable_chip: bool,
}

struct Mmc3 {
    prg_sels: [BankSel; 4],
    chr_sels: [BankSel; 8],
    vram_sels: [BankSel; 4],

    prg_size: usize,
    chr_size: usize,

    banksel: BankSelect,
    banksel_regs: [u8; 8],
    prg_ram_protection: PrgRamProtection,

    irq: R<Mmc3Irq>,
}

pub(in crate::nes) struct Mmc3Irq {
    pub line: IrqLine,
    pub counter: u8,
    pub latch: u8,
    pub reload: bool,
    pub enabled: bool,
    pub a12_watcher: A12Watcher,
}

impl Mmc3 {
    fn remap(&self) {
        fn set<const N: usize>(sels: &[BankSel; N], vals: [u8; N], unit: usize, len: usize) {
            for (s, v) in sels.iter().zip(vals) {
                s.set((v as usize * unit) % len);
            }
        }

        let mut r = self.banksel_regs;
        r[0] &= !1;
        r[1] &= !1;
        r[6] &= 0x3F;
        r[7] &= 0x3F;

        let n = (self.prg_size / 0x2000) as u8;
        let prg_vals = match self.banksel.prg() {
            PrgRomFixation::FixC0 => [r[6], r[7], n - 2, n - 1],
            PrgRomFixation::Fix80 => [n - 2, r[7], r[6], n - 1],
        };
        set(&self.prg_sels, prg_vals, 0x2000, self.prg_size);

        let chr_vals = match self.banksel.chr() {
            A12Inversion::TwoFour => [r[0], r[0] + 1, r[1], r[1] + 1, r[2], r[3], r[4], r[5]],
            A12Inversion::FourTwo => [r[2], r[3], r[4], r[5], r[0], r[0] + 1, r[1], r[1] + 1],
        };
        set(&self.chr_sels, chr_vals, 0x0400, self.chr_size);
    }
}

impl MemWrite for Mmc3 {
    fn write(&mut self, addr: u16, data: u8) {
        let regnum = (addr >> 12 & !1) | (addr & 1);
        match regnum {
            0 => {
                self.banksel.bytes[0] = data;
                self.remap();
            }
            1 => {
                self.banksel_regs[self.banksel.reg() as usize] = data;
                self.remap();
            }
            2 => {
                if data & 1 == 0 {
                    // vertical
                    self.vram_sels[1].set(0x0400);
                    self.vram_sels[2].set(0x0000);
                } else {
                    // horizontal
                    self.vram_sels[1].set(0x0000);
                    self.vram_sels[2].set(0x0400);
                }
            }
            3 => {
                println!("PRG-RAM protection NYI");
                self.prg_ram_protection.bytes[0] = data;
            }
            4 => self.irq.borrow_mut().latch = data,
            5 => self.irq.borrow_mut().reload = true,
            6 => {
                self.irq.borrow_mut().line.clear();
                self.irq.borrow_mut().enabled = false;
            }
            7 => self.irq.borrow_mut().enabled = true,
            _ => (),
        }
    }
}

impl Reset for Mmc3 {
    fn reset(&mut self) {
        // TODO
    }
}

pub fn setup(info: RomInfo<'_>) -> Result<(), &'static str> {
    let RomInfo {
        cpu,
        ppu,

        prg_ram,
        prg_rom,
        chr,
        vram_sels,
        rm,
        ..
    } = info;

    let cpu_bus = &mut cpu.bus.borrow_mut();

    if let Some(prg_ram) = &prg_ram {
        prg_ram.map(cpu_bus, 0x6000..=0x7FFF, 0);
    }

    let prg_sels: [BankSel; 4] = Default::default();
    prg_rom.map_switchable(cpu_bus, 0x8000..=0x9FFF, &prg_sels[0]);
    prg_rom.map_switchable(cpu_bus, 0xA000..=0xBFFF, &prg_sels[1]);
    prg_rom.map_switchable(cpu_bus, 0xC000..=0xDFFF, &prg_sels[2]);
    prg_rom.map_switchable(cpu_bus, 0xE000..=0xFFFF, &prg_sels[3]);

    let mut ppu = ppu.borrow_mut();
    let mut ppu_bus = ppu.bus.borrow_mut();

    let chr_sels: [BankSel; 8] = Default::default();
    for i in 0..8 {
        let start = i * 0x0400;
        let end = start + 0x03FF;
        chr.map_switchable(&mut ppu_bus, start..=end, &chr_sels[i as usize]);
    }

    let mmc3 = r(Mmc3 {
        prg_sels,
        chr_sels,
        vram_sels,
        prg_size: prg_rom.size(),
        chr_size: chr.size(),
        banksel: BankSelect::new(),
        banksel_regs: [0; 8],
        prg_ram_protection: PrgRamProtection::new(),
        irq: r(Mmc3Irq {
            line: cpu.get_irq_line(),
            counter: 0,
            latch: 0,
            reload: false,
            enabled: false,
            a12_watcher: A12Watcher::default(),
        }),
    });

    drop(ppu_bus);
    ppu.mmc3_irq = Some(mmc3.borrow().irq.clone());

    rm.borrow_mut().add_device(&mmc3);

    for i in 0..0x80 {
        cpu_bus.set_write_handler(0x80 + i, &mmc3, i << 8);
    }

    mmc3.borrow().remap();

    Ok(())
}
