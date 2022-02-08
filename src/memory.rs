use std::iter::ExactSizeIterator;
use std::ops::RangeInclusive;

use crate::membus::{self, BankSel, MemBus};
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

    pub fn map(this: &R<Self>, bus: &mut MemBus, range: RangeInclusive<u16>, start: usize) {
        let bus_start = *range.start() as usize;
        let size = range.len();

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

    pub fn map_mirroring(this: &R<Self>, bus: &mut MemBus, bus_range: RangeInclusive<u16>) {
        let mapped_len = this.borrow().size();
        for start in bus_range.step_by(mapped_len) {
            let end = start + (mapped_len - 1) as u16;
            Self::map(this, bus, start..=end, 0)
        }
    }

    pub fn map_switchable(
        this: &R<Self>,
        bus: &mut MemBus,
        range: RangeInclusive<u16>,
        sel: &BankSel,
    ) {
        let bus_start = *range.start() as usize;
        let size = range.len();

        assert!(bus_start as usize % membus::PAGESIZE == 0);
        assert!(size as usize % membus::PAGESIZE == 0);
        assert!(size as usize <= this.borrow().bytes.len());

        let start_page = bus_start as usize / membus::PAGESIZE;
        let npages = size as usize / membus::PAGESIZE;

        for i in 0..npages {
            let start_addr = i * membus::PAGESIZE;
            bus.set_read_bank(start_page + i, this, start_addr, &sel);
            if this.borrow().writeable {
                bus.set_write_bank(start_page + i, this, start_addr, &sel);
            }
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
