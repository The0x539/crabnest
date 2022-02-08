use crate::nes::ines::{Mirroring, RomInfo};

pub fn setup(info: RomInfo<'_>) -> Result<(), &'static str> {
    if !matches!(
        info.prg_ram.as_ref().map(|r| r.size()),
        None | Some(0x0800 | 0x1000 | 0x2000),
    ) {
        return Err("ROM has invalid PRG-RAM configuration");
    }

    if !matches!(info.prg_rom.size(), 0x4000 | 0x8000) {
        return Err("ROM has invalid PRG-ROM configuration");
    }

    if !matches!(info.chr.size(), 0x2000) {
        return Err("ROM has invalid CHR-ROM configuration");
    }

    if !matches!(info.vram.size(), 0x0800) {
        return Err("ROM has invalid VRAM configuration");
    }

    let c_bus = &mut *info.cpu.bus.borrow_mut();

    if let Some(prg_ram) = &info.prg_ram {
        prg_ram.map_mirroring(c_bus, 0x6000..=0x7FFF);
    }

    info.prg_rom.map_mirroring(c_bus, 0x8000..=0xFFFF);

    let ppu = info.ppu.borrow();
    let p_bus = &mut *ppu.bus.borrow_mut();

    info.chr.map(p_bus, 0..=(info.chr.size() - 1) as u16, 0);

    let (a, b, c, d) = match info.mirroring {
        Mirroring::Horizontal => (0, 0, 0x0400, 0x0400),
        Mirroring::Vertical => (0, 0x0400, 0, 0x0400),
    };

    info.vram.map(p_bus, 0x2000..=0x23FF, a);
    info.vram.map(p_bus, 0x2400..=0x27FF, b);
    info.vram.map(p_bus, 0x2800..=0x2BFF, c);
    info.vram.map(p_bus, 0x2C00..=0x2FFF, d);

    Ok(())
}
