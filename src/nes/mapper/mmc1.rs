#![allow(dead_code)]

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;

use crate::reset_manager::Reset;

#[derive(Debug, BitfieldSpecifier)]
pub enum Mirroring {
    OneScreenNt0 = 0,
    OneScreenNt1 = 1,
    Vertical = 2,
    Horizontal = 3,
}

#[derive(Debug, BitfieldSpecifier)]
pub enum PrgromFixation {
    Low = 0,
    High = 1,
}

#[derive(Debug, BitfieldSpecifier)]
pub enum PrgromSwitching {
    ThirtyTwoK = 0,
    SixteenK = 1,
}

#[derive(Debug, BitfieldSpecifier)]
pub enum ChrSwitching {
    EightK = 0,
    FourK = 1,
}

#[bitfield(bits = 8)]
#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Reg0 {
    pub mirroring: Mirroring,
    pub prgrom_fixation: PrgromFixation,
    pub prgrom_switching: PrgromSwitching,
    pub chr_switching: ChrSwitching,

    #[skip]
    __: B3,
}

#[bitfield(bits = 8)]
#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Reg1 {
    pub banksel4k: B5,

    #[skip]
    ___: B3,
}

impl Reg1 {
    pub fn banksel8k(&self) -> u8 {
        self.banksel4k() >> 1
    }
    pub fn set_banksel8k(&mut self, val: u8) {
        self.set_banksel4k(val << 1);
    }
}

#[bitfield(bits = 8)]
#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Reg2 {
    pub banksel4k: B5,

    #[skip]
    __: B3,
}

#[bitfield(bits = 8)]
#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Reg3 {
    pub banksel16k: B4,
    pub wram_disabled: bool,

    #[skip]
    __: B3,
}

impl Reg3 {
    pub fn banksel32k(&self) -> u8 {
        self.banksel16k() >> 1
    }
    pub fn set_banksel32k(&mut self, val: u8) {
        self.set_banksel16k(val << 1);
    }
}

#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct MmcRegs(pub Reg0, pub Reg1, pub Reg2, pub Reg3);

#[derive(Debug, Default, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Mmc1 {
    pub last_cpu_cyclenum: u64,
    pub shiftreg: u8,

    pub reg: MmcRegs,

    padding: [u8; 3],
}

impl Mmc1 {
    fn reset_shiftreg(&mut self) {
        self.shiftreg = 0x20;
    }

    pub fn reg_write(&mut self, regnum: usize, val: u8, cpu_cyclenum: u64) {
        assert!(regnum < 4);

        if cpu_cyclenum.wrapping_sub(self.last_cpu_cyclenum) == 1 {
            self.last_cpu_cyclenum = cpu_cyclenum;
            return;
        }

        if val & 0x80 != 0 {
            self.reset_shiftreg();
            return;
        }

        self.shiftreg >>= 1;
        self.shiftreg |= (val & 0x1) << 5;

        if self.shiftreg & 0x1 != 0 {
            // Shhh not-so-evil memory reinterpretation.
            let regs = bytemuck::bytes_of_mut(&mut self.reg);
            regs[regnum] = self.shiftreg >> 1;

            self.reset_shiftreg();
        }

        self.last_cpu_cyclenum = cpu_cyclenum;
    }
}

impl Reset for Mmc1 {
    fn reset(&mut self) {
        self.reset_shiftreg();

        self.reg.0.set_prgrom_fixation(PrgromFixation::Low);
        self.reg.0.set_prgrom_switching(PrgromSwitching::SixteenK);

        self.reg.3.set_banksel16k(0xF);
        self.reg.3.set_wram_disabled(false);

        self.last_cpu_cyclenum = u64::MAX;
    }
}
