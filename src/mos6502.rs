use std::cell::Cell;
use std::rc::Rc;

use bitflags::bitflags;

use crate::membus::MemBus;
use crate::reset_manager::ResetManager;
use crate::timekeeper::Timekeeper;
use crate::{r, R};

mod isa;
mod vmcall;

pub const CLK_DIVISOR: u64 = 12;

bitflags! {
    pub struct StatReg: u8 {
        const C = 0b00000001;
        const Z = 0b00000010;
        const I = 0b00000100;
        const D = 0b00001000;
        const B = 0b00010000;
        const U = 0b00100000;
        const V = 0b01000000;
        const N = 0b10000000;
    }
}

struct IrqPin {
    count: Rc<Cell<u8>>,
}

impl IrqPin {
    fn new() -> Self {
        Self {
            count: Default::default(),
        }
    }

    fn get_line(&self) -> IrqLine {
        assert!(Rc::strong_count(&self.count) < u8::MAX as usize);
        IrqLine {
            count: self.count.clone(),
            active: false,
        }
    }

    fn active(&self) -> bool {
        self.count.get() > 0
    }
}

pub struct IrqLine {
    count: Rc<Cell<u8>>,
    active: bool,
}

impl IrqLine {
    pub fn raise(&mut self) {
        if !self.active {
            self.active = true;
            self.count.set(self.count.get() + 1);
        }
    }

    pub fn clear(&mut self) -> bool {
        let val = self.active;
        if self.active {
            self.active = false;
            self.count.set(self.count.get() - 1);
        }
        val
    }
}

impl Drop for IrqLine {
    fn drop(&mut self) {
        self.clear();
    }
}

struct NmiPin {
    state: Rc<Cell<bool>>,
}

impl NmiPin {
    fn new() -> Self {
        Self {
            state: Default::default(),
        }
    }

    fn get_line(&self) -> NmiLine {
        NmiLine {
            state: self.state.clone(),
        }
    }

    fn poll(&mut self) -> bool {
        self.state.take()
    }
}

pub struct NmiLine {
    state: Rc<Cell<bool>>,
}

impl NmiLine {
    pub fn raise(&mut self) {
        self.state.set(true);
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StepResult {
    Success,
    UnhandledVmcall,
    IllegalInstruction,
    Vmbreak,
}

pub struct Mos6502 {
    pub(super) bus: R<MemBus>,
    pub(super) tk: R<Timekeeper>,

    pub pc: u16,
    pub sp: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: StatReg,

    pub paravirt: bool,

    irq_pin: IrqPin,
    nmi_pin: NmiPin,

    irq_pending: bool,
    nmi_pending: bool,

    #[cfg(feature = "cyclecheck")]
    last_branch_delay: u64,
    #[cfg(feature = "cyclecheck")]
    pub(super) last_takeover_delay: Rc<Cell<u64>>,

    paravirt_args: Vec<String>,

    #[cfg(windows)]
    open_files: slab::Slab<std::fs::File>,
}

impl Mos6502 {
    pub fn new(rm: &R<ResetManager>, tk: &R<Timekeeper>, paravirt_args: &[&str]) -> R<Self> {
        let bus = MemBus::new(rm);
        let tk = tk.clone();
        let cpu = Self {
            bus,
            tk,
            paravirt: false,
            paravirt_args: paravirt_args.iter().map(|&s| s.to_owned()).collect(),
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            p: StatReg::empty(),
            irq_pin: IrqPin::new(),
            nmi_pin: NmiPin::new(),
            irq_pending: false,
            nmi_pending: false,
            #[cfg(feature = "cyclecheck")]
            last_branch_delay: 0,
            #[cfg(feature = "cyclecheck")]
            last_takeover_delay: Rc::new(Cell::new(0)),
            #[cfg(windows)]
            open_files: Default::default(),
        };
        r(cpu)
    }

    pub fn advance_clk(&mut self, ncycles: usize) {
        self.tk
            .borrow_mut()
            .advance_clk(ncycles as u64 * CLK_DIVISOR);
    }

    pub fn reset(&mut self) {
        let bus = self.bus.borrow();
        let pc_lo = bus.read(0xfffc);
        let pc_hi = bus.read(0xfffd);
        self.pc = u16::from_le_bytes([pc_lo, pc_hi]);

        self.a = 0;
        self.x = 0;
        self.y = 0;

        self.sp = 0xfd;
        self.p.bits = 0x34;

        drop(bus);
        self.advance_clk(8);
    }

    pub fn get_irq_line(&self) -> IrqLine {
        self.irq_pin.get_line()
    }

    pub fn get_nmi_line(&self) -> NmiLine {
        self.nmi_pin.get_line()
    }

    pub fn read8(&self, addr: u16) -> u8 {
        self.bus.borrow().read(addr)
    }

    fn read16(&self, addr: u16) -> u16 {
        let lo = self.read8(addr);
        let hi = self.read8(addr + 1);
        u16::from_le_bytes([lo, hi])
    }

    fn read8pc(&mut self) -> u8 {
        let v = self.read8(self.pc);
        self.pc += 1;
        v
    }

    fn read16pc(&mut self) -> u16 {
        let v = self.read16(self.pc);
        self.pc += 2;
        v
    }

    fn buggy_read16(&self, addr: u16) -> u16 {
        let first = addr;
        let msb = addr & 0xff00;
        let lsb = if (addr & 0xff) == 0xff {
            0
        } else {
            (addr & 0xff) + 1
        };
        let secnd = msb | lsb;
        let lo = self.read8(first) as u16;
        let hi = self.read8(secnd) as u16;
        let val = (hi << 8) as u16 | lo;
        val
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        self.bus.borrow().write(addr, val);
    }

    pub fn write16(&mut self, addr: u16, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.write8(addr, lo);
        self.write8(addr + 1, hi);
    }

    pub fn push8(&mut self, val: u8) {
        self.write8(0x100 + self.sp as u16, val);
        self.sp -= 1;
    }

    pub fn push16(&mut self, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.push8(hi);
        self.push8(lo);
    }

    pub fn pop8(&mut self) -> u8 {
        self.sp += 1;
        self.read8(0x100 + self.sp as u16)
    }

    pub fn pop16(&mut self) -> u16 {
        let lo = self.pop8();
        let hi = self.pop8();
        u16::from_le_bytes([lo, hi])
    }

    fn handle_irq(&mut self) -> usize {
        self.push16(self.pc);
        self.push8((self.p | StatReg::B | StatReg::U).bits);
        self.p.insert(StatReg::I);

        self.pc = self.read16(0xFFFE);

        7
    }

    fn handle_nmi(&mut self) -> usize {
        self.push16(self.pc);
        self.push8((self.p | StatReg::B).bits);

        self.pc = self.read16(0xFFFA);
        8
    }
}
