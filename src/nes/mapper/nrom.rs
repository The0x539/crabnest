use crate::memory::Memory;
use crate::nes::ines::{Mirroring, RomInfo};

pub fn setup(info: RomInfo) -> Result<(), &'static str> {
    if info.prg_ram.as_ref().map(|r| r.borrow().size()) > Some(0x2000) {
        return Err("ROM has invalid PRG-RAM configuration");
    }

    if info.prg_rom.borrow().size() > 0x8000 {
        return Err("ROM has invalid PRG-ROM configuration");
    }

    if info.chr.borrow().size() != 0x2000 {
        return Err("ROM has invalid CHR-ROM configuration");
    }

    if info.vram.borrow().size() > 0x0800 {
        return Err("ROM has invalid VRAM configuration");
    }

    let cpu = info.cpu.borrow();
    let c_bus = &*cpu.bus.borrow();

    if let Some(prg_ram) = &info.prg_ram {
        let size = prg_ram.borrow().size();
        Memory::map_mirroring(prg_ram, c_bus, 0x6000, size as u16, 0x0000, 0x2000 / size);
    }

    {
        let prg_rom = &info.prg_rom;
        let size = prg_rom.borrow().size();
        Memory::map_mirroring(prg_rom, c_bus, 0x8000, size as u16, 0x0000, 0x8000 / size);
    }

    let ppu = info.ppu.borrow();
    let p_bus = &*ppu.bus.borrow();

    {
        let size = info.chr.borrow().size();
        Memory::map(&info.chr, p_bus, 0x0000, size as u16, 0x0000);
    }

    let half_vram = info.vram.borrow().size() as u16 / 2;
    if info.mirroring == Mirroring::Horizontal {
        Memory::map_mirroring(&info.vram, p_bus, 0x2000, half_vram, 0, 2);
        Memory::map_mirroring(&info.vram, p_bus, 0x2800, half_vram, half_vram as usize, 2);
    } else {
        Memory::map(&info.vram, p_bus, 0x2000, half_vram, 0);
        Memory::map(&info.vram, p_bus, 0x2400, half_vram, half_vram as usize);
        Memory::map(&info.vram, p_bus, 0x2800, half_vram, 0);
        Memory::map(&info.vram, p_bus, 0x2C00, half_vram, half_vram as usize);
    }

    Ok(())
}
