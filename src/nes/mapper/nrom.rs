use crate::nes::ines::RomInfo;

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

    let c_bus = &mut *info.cpu.bus.borrow_mut();

    if let Some(prg_ram) = &info.prg_ram {
        prg_ram.map_mirroring(c_bus, 0x6000..=0x7FFF);
    }

    info.prg_rom.map_mirroring(c_bus, 0x8000..=0xFFFF);

    let ppu = info.ppu.borrow();
    let p_bus = &mut *ppu.bus.borrow_mut();

    info.chr.map(p_bus, 0..=(info.chr.size() - 1) as u16, 0);

    Ok(())
}
