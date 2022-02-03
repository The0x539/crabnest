use crate::{
    ines::RomInfo,
    membus::{MemBus, MemWrite},
    memory::Memory,
    mos6502, r,
    reset_manager::Reset,
    timekeeper::Timekeeper,
    R,
};

use super::{
    mmc1::{ChrSwitching, Mmc1, PrgromFixation, PrgromSwitching},
    ppu::Ppu,
};

struct SxRom {
    mmc1: Mmc1,

    bus: R<MemBus>,
    tk: R<Timekeeper>,
    ppu: R<Ppu>,

    prg_rom: R<Memory>,
    chr: R<Memory>,
    prg_ram: Option<R<Memory>>,
    //prg_nvram: Option<R<Memory>>,
    vram: R<Memory>,
}

impl SxRom {
    fn remap(&mut self) {
        let bus = &self.bus.borrow();

        let prg_rom_size = self.prg_rom.borrow().size();
        let banksel = self.mmc1.reg.3.banksel() as usize;

        self.mmc1
            .map_vram(&*self.ppu.borrow().bus.borrow(), &self.vram);

        let map = |bus_start, size, start| Memory::map(&self.prg_rom, bus, bus_start, size, start);

        let r0 = &self.mmc1.reg.0;
        match (r0.prgrom_switching(), r0.prgrom_fixation()) {
            (PrgromSwitching::ThirtyTwoK, _) => {
                map(0x8000, 0x8000, (0x4000 * (banksel & !1)) % prg_rom_size);
            }

            (PrgromSwitching::SixteenK, PrgromFixation::Low) => {
                map(0x8000, 0x4000, 0x0000);
                map(0xC000, 0x4000, (banksel * 0x4000) % prg_rom_size);
            }

            (PrgromSwitching::SixteenK, PrgromFixation::High) => {
                map(0x8000, 0x4000, (banksel * 0x4000) % prg_rom_size);
                map(0xC000, 0x4000, prg_rom_size - 0x4000);
            }
        }

        let ppu = self.ppu.borrow();
        let pbus = &ppu.bus.borrow();

        let chr_size = self.chr.borrow().size() as usize;

        let pmap = |bus_start, size, start| Memory::map(&self.chr, pbus, bus_start, size, start);

        match r0.chr_switching() {
            ChrSwitching::EightK => {
                pmap(
                    0x0000,
                    0x2000,
                    (self.mmc1.reg.1.banksel8k() as usize * 0x2000) % chr_size,
                );
            }
            ChrSwitching::FourK => {
                pmap(
                    0x0000,
                    0x1000,
                    (self.mmc1.reg.1.banksel4k() as usize * 0x1000) % chr_size,
                );
                pmap(
                    0x1000,
                    0x1000,
                    (self.mmc1.reg.2.banksel4k() as usize * 0x1000) % chr_size,
                );
            }
        }

        if let Some(prg_ram) = &self.prg_ram {
            Memory::map(prg_ram, bus, 0x6000, 0x2000, 0x0000);
        }
    }
}

pub fn setup(info: RomInfo) -> Result<(), &'static str> {
    let RomInfo {
        rm,
        cpu,
        ppu,
        prg_rom,
        prg_ram,
        chr,
        vram,

        mirroring: _,
        prg_nvram: _,
    } = info;

    if prg_rom.borrow().size() % 0x4000 != 0 {
        return Err("ROM's PRGROM size is not a multiple of 16384");
    }

    if chr.borrow().size() % 0x2000 != 0 {
        return Err("ROM's CHR size is not a multiple of 8192");
    }

    if vram.borrow().size() != 0x0800 {
        return Err("ROM's VRAM size is not 2048");
    }

    let cart = r(SxRom {
        mmc1: Mmc1::default(),
        bus: cpu.borrow().bus.clone(),
        tk: cpu.borrow().tk.clone(),
        ppu,
        prg_rom,
        chr,
        prg_ram,
        vram,
    });

    rm.borrow_mut().add_device(&cart);

    for i in 0..0x80 {
        cart.borrow()
            .bus
            .borrow()
            .set_write_handler(0x80 + i, &cart, i << 8);
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
