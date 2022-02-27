use crate::{
    membus::{BankSel, MemWrite},
    nes::ines::RomInfo,
    reset_manager::Reset,
    util::r,
};

mapper!(007, setup);

struct AxRom {
    prg_sel: BankSel,
    vram_sels: [BankSel; 4],
}

impl MemWrite for AxRom {
    fn write(&mut self, _addr: u16, data: u8) {
        let prg_sel = data & 0b0000_0111;
        self.prg_sel.set(prg_sel as usize * 0x8000);

        let vram_sel = (data & 0b0001_0000) >> 4;
        for sel in &mut self.vram_sels {
            sel.set(vram_sel as usize * 0x0400);
        }
    }
}

impl Reset for AxRom {
    fn reset(&mut self) {
        self.write(0, 0);
    }
}

fn setup(info: RomInfo<'_>) -> Result<(), &'static str> {
    let RomInfo {
        cpu,
        ppu,

        prg_rom,
        chr,
        vram_sels,
        rm,
        ..
    } = info;

    let cpu_bus = &mut cpu.bus.borrow_mut();

    let prg_sel = BankSel::default();
    prg_rom.map_switchable(cpu_bus, 0x8000..=0xFFFF, &prg_sel);

    let ppu = ppu.borrow_mut();
    let mut ppu_bus = ppu.bus.borrow_mut();

    chr.map(&mut ppu_bus, 0..=chr.size() as u16 - 1, 0);

    let cart = r(AxRom { prg_sel, vram_sels });
    cart.borrow_mut().write(0, 0);

    rm.borrow_mut().add_device(&cart);

    for i in 0..0x80 {
        cpu_bus.set_write_handler(0x80 + i, &cart, i << 8);
    }

    Ok(())
}
