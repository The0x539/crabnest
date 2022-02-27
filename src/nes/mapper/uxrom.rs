use crate::{
    membus::{BankSel, MemWrite},
    nes::ines::RomInfo,
    reset_manager::Reset,
    util::r,
};

mapper!(002, setup);

struct UxRom {
    prg_sel: BankSel,
}

impl MemWrite for UxRom {
    fn write(&mut self, _addr: u16, data: u8) {
        self.prg_sel.set(data as usize * 0x4000);
    }
}

impl Reset for UxRom {
    fn reset(&mut self) {
        self.write(0, 0);
    }
}

fn setup(info: RomInfo<'_>) -> Result<(), &'static str> {
    let RomInfo {
        rm,
        cpu,
        ppu,
        prg_rom,
        chr,
        ..
    } = info;

    let cpu_bus = &mut *cpu.bus.borrow_mut();

    let prg_sel = BankSel::default();
    prg_rom.map_switchable(cpu_bus, 0x8000..=0xBFFF, &prg_sel);
    prg_rom.map(cpu_bus, 0xC000..=0xFFFF, prg_rom.size() - 0x4000);

    let ppu = ppu.borrow_mut();
    let ppu_bus = &mut ppu.bus.borrow_mut();

    chr.map(ppu_bus, 0..=chr.size() as u16 - 1, 0);

    let cart = r(UxRom { prg_sel });

    rm.borrow_mut().add_device(&cart);

    for i in 0..0x80 {
        cpu_bus.set_write_handler(0x80 + i, &cart, i << 8);
    }

    Ok(())
}
