use crate::{
    ines::RomInfo,
    membus::MemWrite,
    memory::Memory,
    mos6502::{self, Mos6502},
    r,
    reset_manager::Reset,
    R,
};

use super::{
    mmc1::{Mmc1, PrgromFixation, PrgromSwitching},
    ppu::Ppu,
};

struct SxRom {
    mmc1: Mmc1,

    cpu: R<Mos6502>,
    ppu: R<Ppu>,

    prgrom: R<Memory>,
    #[allow(dead_code)]
    chrom: R<Memory>,
    #[allow(dead_code)]
    wram: Option<R<Memory>>,
    vram: R<Memory>,
}

impl SxRom {
    fn remap(&mut self) {
        let cpu = self.cpu.borrow();
        let bus = &mut cpu.bus.borrow_mut();

        let prgrom_size = self.prgrom.borrow().size();
        let banksel = self.mmc1.reg.3.banksel() as usize;

        self.mmc1
            .map_vram(&mut *self.ppu.borrow().bus.borrow_mut(), &self.vram);

        let mut map =
            |bus_start, size, start| Memory::map(&self.prgrom, bus, bus_start, size, start);

        let r0 = &self.mmc1.reg.0;
        match (r0.prgrom_switching(), r0.prgrom_fixation()) {
            (PrgromSwitching::ThirtyTwoK, _) => {
                map(0x8000, 0x8000, (0x4000 * (banksel & !1)) % prgrom_size);
            }

            (PrgromSwitching::SixteenK, PrgromFixation::High) => {
                map(0x8000, 0x4000, 0x0000);
                map(0xC000, 0x4000, (banksel * 0x4000) % prgrom_size);
            }

            (PrgromSwitching::SixteenK, PrgromFixation::Low) => {
                map(0x8000, 0x4000, (banksel * 0x4000) % prgrom_size);
                map(0xC000, 0x4000, prgrom_size - 0x4000);
            }
        }
    }
}

fn mem_size(m: &Option<R<Memory>>) -> Option<usize> {
    m.as_ref().map(|r| r.borrow().size())
}

pub fn setup(info: &mut RomInfo) -> Result<(), &'static str> {
    if !matches!(mem_size(&info.wram), None | Some(0x2000)) {
        return Err("ROM has an invalid WRAM configuration");
    }

    if let Some(prgrom_size) = mem_size(&info.prgrom) {
        if prgrom_size % 0x8000 != 0 {
            return Err("ROM's PRGROM size is not a multiple of 32768");
        }
    } else {
        return Err("ROM is missing PRGROM");
    }

    if let Some(chrom_size) = mem_size(&info.chrom) {
        if chrom_size % 0x2000 != 0 {
            return Err("ROM's CHROM size is not a multiple of 8192");
        }
    } else {
        return Err("ROM is missing CHROM");
    }

    assert_eq!(mem_size(&Some(info.vram.clone())), Some(0x0800));

    if info.chram.is_some() {
        return Err("CHRAM is not supported");
    }

    let cart = r(SxRom {
        mmc1: Mmc1::default(),
        cpu: info.cpu.clone(),
        ppu: info.ppu.clone(),
        prgrom: info.prgrom.clone().unwrap(),
        chrom: info.chrom.clone().unwrap(),
        wram: info.wram.clone(),
        vram: info.vram.clone(),
    });

    info.rm.borrow_mut().add_device(&cart);

    for i in 0..0x80 {
        info.cpu
            .borrow()
            .bus
            .borrow_mut()
            .set_write_handler(0x80 + i, &cart, i << 8);
    }

    Ok(())
}

impl MemWrite for SxRom {
    fn write(&mut self, addr: u16, val: u8) {
        self.mmc1.reg_write(
            addr as usize / 0x2000,
            val,
            self.cpu.borrow().tk.borrow().clk_cyclenum / mos6502::CLKDIVISOR,
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
