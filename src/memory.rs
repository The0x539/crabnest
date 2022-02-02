use crate::membus::{self, MemBus};
use crate::reset_manager::{Reset, ResetManager};
use crate::{r, R};

pub struct Memory {
    writeable: bool,
    pub(super) bytes: Vec<u8>,
}

impl Memory {
    pub fn size(&self) -> usize {
        self.bytes.len()
    }

    pub fn new(rm: &R<ResetManager>, size: usize, writeable: bool) -> R<Self> {
        let mem = r(Self {
            bytes: vec![0; size],
            writeable,
        });
        if writeable {
            rm.borrow_mut().add_device(&mem);
        }
        mem
    }

    pub fn map(this: &R<Self>, bus: &MemBus, bus_start: u16, size: u16, start: usize) {
        assert!(bus_start as usize % membus::PAGESIZE == 0);
        assert!(size as usize % membus::PAGESIZE == 0);
        assert!(start + size as usize <= this.borrow().bytes.len());

        let start_page = bus_start as usize / membus::PAGESIZE;
        let npages = size as usize / membus::PAGESIZE;

        for i in 0..npages {
            let start_addr = start + i * membus::PAGESIZE;
            bus.set_read_memory(start_page + i, this, start_addr);
            if this.borrow().writeable {
                bus.set_write_memory(start_page + i, this, start_addr);
            }
        }
    }

    pub fn map_mirroring(
        this: &R<Self>,
        bus: &MemBus,
        bus_start: u16,
        size: u16,
        start: usize,
        nmirrors: usize,
    ) {
        for i in 0..nmirrors {
            Self::map(this, bus, bus_start + i as u16 * size, size, start);
        }
    }

    pub fn get(&self, addr: usize) -> u8 {
        self.bytes[addr]
    }

    pub fn set(&mut self, addr: usize, val: u8) {
        assert!(self.writeable);
        self.bytes[addr] = val;
    }
}

impl Reset for Memory {
    fn reset(&mut self) {
        self.bytes.fill(0xFF);
    }
}
