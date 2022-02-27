#![allow(dead_code)]

use modular_bitfield::prelude::*;

use crate::{mos6502::IrqLine, nes::a12_watcher::A12Watcher};

#[derive(BitfieldSpecifier)]
pub(super) enum A12Inversion {
    TwoFour = 0,
    FourTwo = 1,
}

#[derive(BitfieldSpecifier)]
pub(super) enum PrgRomFixation {
    FixC0 = 0,
    Fix80 = 1,
}

#[bitfield]
#[repr(u8)]
pub(super) struct BankSelect {
    pub reg: B3,
    #[skip]
    __: B3,
    pub prg: PrgRomFixation,
    pub chr: A12Inversion,
}

#[bitfield]
#[repr(u8)]
pub(super) struct PrgRamProtection {
    #[skip]
    __: B6,
    pub deny_writes: bool,
    pub enable_chip: bool,
}

pub(in crate::nes) struct Mmc3Irq {
    pub line: IrqLine,
    pub counter: u8,
    pub latch: u8,
    pub reload: bool,
    pub enabled: bool,
    pub a12_watcher: A12Watcher,
}
