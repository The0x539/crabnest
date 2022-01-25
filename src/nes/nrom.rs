use crate::{
    ines::{Mirroring, RomInfo},
    memory::Memory,
    R,
};

fn mem_size(m: &Option<R<Memory>>) -> Option<usize> {
    m.as_ref().map(|r| r.borrow().size())
}

pub fn setup(info: &mut RomInfo) -> Result<(), &'static str> {
    if !matches!(mem_size(&info.wram), None | Some(0..=0x2000)) {
        return Err("ROM has invalid WRAM configuration");
    }

    if !matches!(mem_size(&info.prgrom), Some(0..=0x8000)) {
        return Err("ROM has invalid PRGROM configuration");
    }

    if mem_size(&info.chrom) != Some(0x2000) {
        return Err("ROM has invalid CHROM configuration");
    }

    if info.vram.borrow().size() > 0x0800 {
        return Err("ROM has invalid VRAM configuration");
    }

    if info.chram.is_some() {
        return Err("ROM has invalid CHRAM configuration");
    }

    let cpu = info.cpu.borrow();
    let bus = &mut *cpu.bus.borrow_mut();

    if let Some(wram) = &info.wram {
        let size = wram.borrow().size();
        Memory::map_mirroring(wram, bus, 0x6000, size as u16, 0x0000, 0x2000 / size);
    }

    // guaranteed
    if let Some(prgrom) = &info.prgrom {
        let size = prgrom.borrow().size();
        Memory::map_mirroring(prgrom, bus, 0x8000, size as u16, 0x0000, 0x8000 / size);
    }

    // guaranteed
    if let Some(chrom) = &info.chrom {
        let size = chrom.borrow().size();
        Memory::map_mirroring(chrom, bus, 0x0000, size as u16, 0x0000, 0x0000);
    }

    let vram_size = info.vram.borrow().size();
    if info.mirroring == Mirroring::Horizontal {
        Memory::map_mirroring(&info.vram, bus, 0x2000, vram_size as u16, 0, 2);
        Memory::map_mirroring(&info.vram, bus, 0x2800, vram_size as u16, vram_size, 2);
    } else {
        Memory::map(&info.vram, bus, 0x2000, vram_size as u16, 0);
        Memory::map(&info.vram, bus, 0x2400, vram_size as u16, vram_size);
        Memory::map(&info.vram, bus, 0x2800, vram_size as u16, 0);
        Memory::map(&info.vram, bus, 0x2C00, vram_size as u16, vram_size);
    }

    Ok(())
}