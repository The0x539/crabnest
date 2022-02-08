use super::{apu::Apu, io_reg::IoReg};
use crate::membus::{MemBus, MemRead, MemWrite};
use crate::{r, R};

pub struct PageForty {
    pub io_reg: R<IoReg>,
    pub apu: R<Apu>,
}

pub fn setup(bus: &mut MemBus, io_reg: R<IoReg>, apu: R<Apu>) {
    let pf = r(PageForty { io_reg, apu });
    bus.set_read_handler(0x40, &pf, 0);
    bus.set_write_handler(0x40, &pf, 0);
}

impl MemRead for PageForty {
    fn read(&mut self, addr: u16, lane_mask: &mut u8) -> u8 {
        match addr {
            0x16 | 0x17 => self.io_reg.borrow_mut().read(addr, lane_mask),
            _ => self.apu.borrow_mut().read(addr, lane_mask),
        }
    }
}

impl MemWrite for PageForty {
    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x14 | 0x16 => self.io_reg.borrow_mut().write(addr, val),
            _ => self.apu.borrow_mut().write(addr, val),
        }
    }
}
