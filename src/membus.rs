use std::cell::RefCell;

use crate::memory::Memory;
use crate::reset_manager::{Reset, ResetManager};
use crate::{r, R};

pub const PAGESIZE: usize = 256;
pub const NPAGES: usize = 256;

pub trait MemRead: 'static {
    fn read(&mut self, addr: u16, lane_mask: &mut u8) -> u8;
}

pub trait MemWrite: 'static {
    fn write(&mut self, addr: u16, data: u8);
}

enum Mapping<H: ?Sized> {
    Unmapped,
    Handler { handler: R<H>, offset: usize },
    Data { mem: R<Memory>, start: usize },
}

type ReadMapping = Mapping<dyn MemRead>;
type WriteMapping = Mapping<dyn MemWrite>;

pub struct MemBus {
    read_mappings: RefCell<[ReadMapping; NPAGES]>,
    write_mappings: RefCell<[WriteMapping; NPAGES]>,
}

impl MemBus {
    pub fn new(rm: &R<ResetManager>) -> R<Self> {
        const RM_U: ReadMapping = Mapping::Unmapped;
        const WM_U: WriteMapping = Mapping::Unmapped;
        let bus = r(Self {
            read_mappings: RefCell::new([RM_U; 256]),
            write_mappings: RefCell::new([WM_U; 256]),
        });
        rm.borrow_mut().add_device(&bus);
        bus
    }

    pub fn read(&self, addr: u16) -> u8 {
        let pagenum = addr as usize / PAGESIZE;
        assert!(pagenum < NPAGES);

        let unmixed_val: u8;
        let mut lane_mask: u8 = 0xFF;

        match &self.read_mappings.borrow()[pagenum] {
            Mapping::Unmapped => unmixed_val = 0x00,
            Mapping::Handler { handler, offset } => {
                let final_addr = addr as usize % PAGESIZE + *offset;
                unmixed_val = handler.borrow_mut().read(final_addr as u16, &mut lane_mask);
            }
            Mapping::Data { mem, start } => {
                let final_addr = addr as usize % PAGESIZE + *start;
                unmixed_val = mem.borrow().get(final_addr);
            }
        }

        // TODO: figure out the meaning of OPEN_BUS_TO_VCC
        unmixed_val | !lane_mask
    }

    pub fn write(&self, addr: u16, val: u8) {
        let pagenum = addr as usize / PAGESIZE;
        assert!(pagenum < NPAGES);

        let mappings = self.write_mappings.borrow();
        match &mappings[pagenum] {
            Mapping::Unmapped => (),
            Mapping::Handler { handler, offset } => {
                let final_addr = addr as usize % PAGESIZE + *offset;

                // this little dance allows a write handler (namely sxrom's) to remap memory
                let handler = handler.clone();
                drop(mappings);

                handler.borrow_mut().write(final_addr as u16, val);
            }
            Mapping::Data { mem, start } => {
                let final_addr = addr as usize % PAGESIZE + *start;
                mem.borrow_mut().set(final_addr, val);
            }
        }
    }

    #[allow(dead_code)]
    pub fn clear_page(&self, pagenum: usize) {
        self.read_mappings.borrow_mut()[pagenum] = Mapping::Unmapped;
        self.write_mappings.borrow_mut()[pagenum] = Mapping::Unmapped;
    }

    pub fn set_read_memory(&self, pagenum: usize, mem: &R<Memory>, start: usize) {
        let mem = mem.clone();
        self.read_mappings.borrow_mut()[pagenum] = Mapping::Data { mem, start };
    }

    pub fn set_write_memory(&self, pagenum: usize, mem: &R<Memory>, start: usize) {
        let mem = mem.clone();
        self.write_mappings.borrow_mut()[pagenum] = Mapping::Data { mem, start };
    }

    pub fn set_read_handler(&self, pagenum: usize, handler: &R<impl MemRead>, offset: usize) {
        let handler = handler.clone();
        self.read_mappings.borrow_mut()[pagenum] = Mapping::Handler { handler, offset };
    }

    pub fn set_write_handler(&self, pagenum: usize, handler: &R<impl MemWrite>, offset: usize) {
        let handler = handler.clone();
        self.write_mappings.borrow_mut()[pagenum] = Mapping::Handler { handler, offset };
    }
}

impl Reset for MemBus {
    fn reset(&mut self) {
        // self.data_lanes = 0xFF;
    }
}
