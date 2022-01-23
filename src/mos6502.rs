use bitflags::bitflags;

use crate::membus::MemBus;
use crate::reset_manager::ResetManager;
use crate::timekeeper::Timekeeper;
use crate::{r, R};

bitflags! {
    struct StatReg: u8 {
        const C = 0b00000001;
        const Z = 0b00000010;
        const I = 0b00000100;
        const D = 0b00001000;
        const B = 0b00010000;
        // unused 0b00100000;
        const V = 0b01000000;
        const N = 0b10000000;
    }
}

enum Intr {
    None,
    Irq,
    Nmi,
}

enum AddrMode {
    None,
    Abs,
    AbsX,
    AbsY,
    Acc,
    Imm,
    Impl,
    IdxInd,
    Ind,
    IndIdx,
    Rel,
    ZeroP,
    ZeroPX,
    ZeroPY,
}

pub struct Mos6502<'a> {
    bus: R<MemBus>,
    tk: R<Timekeeper>,

    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    p: StatReg,

    intr_status: Intr,

    last_branch_delay: u64,
    last_takeover_delay: u64,

    paravirt_args: &'a [&'a str],
}

impl<'a> Mos6502<'a> {
    pub fn new(rm: &R<ResetManager>, tk: &R<Timekeeper>, paravirt_args: &'a [&'a str]) -> R<Self> {
        let bus = MemBus::new(rm);
        let tk = tk.clone();
        let cpu = Self {
            bus,
            tk,
            paravirt_args,
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            p: StatReg::empty(),
            intr_status: Intr::None,
            last_branch_delay: 0,
            last_takeover_delay: 0,
        };
        r(cpu)
    }
}
