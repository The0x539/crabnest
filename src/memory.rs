use std::iter::ExactSizeIterator;
use std::ops::RangeInclusive;

use crate::membus::{self, BankSel, MemBus};
use crate::reset_manager::{Reset, ResetManager};
use crate::util::{r, R};

pub struct MemInner {
    bytes: Vec<u8>,
    writeable: bool,
}

impl MemInner {
    fn new(size: usize, writeable: bool) -> Self {
        Self {
            bytes: vec![0; size],
            writeable,
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

impl Reset for MemInner {
    fn reset(&mut self) {
        self.bytes.fill(0xFF);
    }
}

pub struct Memory {
    inner: R<MemInner>,
    size: usize,
    writeable: bool,
}

impl Memory {
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn new(rm: &R<ResetManager>, size: usize, writeable: bool) -> Self {
        let mem = Self {
            inner: r(MemInner::new(size, writeable)),
            size,
            writeable,
        };
        if writeable {
            rm.borrow_mut().add_device(&mem.inner);
        }
        mem
    }

    pub fn map(&self, bus: &mut MemBus, range: RangeInclusive<u16>, start: usize) {
        let bus_start = *range.start() as usize;
        let size = range.len();

        assert!(bus_start as usize % membus::PAGESIZE == 0);
        assert!(size as usize % membus::PAGESIZE == 0);
        assert!(start + size as usize <= self.size);

        let start_page = bus_start as usize / membus::PAGESIZE;
        let npages = size as usize / membus::PAGESIZE;

        for i in 0..npages {
            let start_addr = start + i * membus::PAGESIZE;
            bus.set_read_memory(start_page + i, self.inner.clone(), start_addr);
            if self.writeable {
                bus.set_write_memory(start_page + i, self.inner.clone(), start_addr);
            }
        }
    }

    pub fn map_mirroring(&self, bus: &mut MemBus, bus_range: RangeInclusive<u16>) {
        for start in bus_range.step_by(self.size) {
            let end = start + (self.size - 1) as u16;
            self.map(bus, start..=end, 0)
        }
    }

    pub fn map_switchable(&self, bus: &mut MemBus, range: RangeInclusive<u16>, sel: &BankSel) {
        let bus_start = *range.start() as usize;
        let size = range.len();

        assert!(bus_start as usize % membus::PAGESIZE == 0);
        assert!(size as usize % membus::PAGESIZE == 0);
        assert!(size as usize <= self.size);

        let start_page = bus_start as usize / membus::PAGESIZE;
        let npages = size as usize / membus::PAGESIZE;

        for i in 0..npages {
            let start_addr = i * membus::PAGESIZE;
            bus.set_read_bank(start_page + i, self.inner.clone(), start_addr, sel.clone());
            if self.writeable {
                bus.set_write_bank(start_page + i, self.inner.clone(), start_addr, sel.clone());
            }
        }
    }

    pub fn populate(&mut self, mut f: impl std::io::Read) -> std::io::Result<()> {
        f.read_exact(&mut self.inner.borrow_mut().bytes)
    }
}
