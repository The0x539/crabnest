use super::{Mos6502, StepResult};

impl Mos6502 {
    pub fn instr_repr(&self, addr: u16) -> impl std::fmt::Display {
        ""
    }

    pub fn step(&mut self) -> StepResult {
        todo!()
    }
}
