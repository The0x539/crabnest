use super::{Mos6502, StepResult};

const VMCALL_ARGS: u8 = 0;
const VMCALL_EXIT: u8 = 1;
const VMCALL_OPEN: u8 = 2;
const VMCALL_CLOSE: u8 = 3;
const VMCALL_READ: u8 = 4;
const VMCALL_WRITE: u8 = 5;
const VMCALL_BREAK: u8 = 6;
const VMCALL_DUMP: u8 = 7;

impl Mos6502 {
    fn handle_args(&mut self) -> StepResult {
        todo!()
    }
    fn handle_exit(&mut self) -> StepResult {
        todo!()
    }
    fn handle_open(&mut self) -> StepResult {
        todo!()
    }
    fn handle_close(&mut self) -> StepResult {
        todo!()
    }
    fn handle_read(&mut self) -> StepResult {
        todo!()
    }
    fn handle_write(&mut self) -> StepResult {
        todo!()
    }
    fn handle_dump(&mut self) -> StepResult {
        println!("EXIT STATE:");
        println!("PC: 0x{:04x}", self.pc);
        println!("PC: 0x{:04x}", self.sp);
        println!(" A: 0x{:02x}", self.a);
        println!(" X: 0x{:02x}", self.x);
        println!(" Y: 0x{:02x}", self.y);
        println!(" P: 0x{:02x}", self.p.bits);

        for addr in (0..0xC000).step_by(4) {
            print!("  ${addr:04x}:");
            for i in 0..4 {
                let v = self.read8(addr + i);
                print!(" {v:02x}");
            }
            println!();
        }

        std::process::exit(0);
    }

    pub fn handle_vmcall(&mut self, call_num: u8) -> StepResult {
        match call_num {
            VMCALL_ARGS => self.handle_args(),
            VMCALL_EXIT => self.handle_exit(),
            VMCALL_OPEN => self.handle_open(),
            VMCALL_CLOSE => self.handle_close(),
            VMCALL_READ => self.handle_read(),
            VMCALL_WRITE => self.handle_write(),
            VMCALL_BREAK => StepResult::Vmbreak,
            VMCALL_DUMP => self.handle_dump(),
            _ => StepResult::UnhandledVmcall,
        }
    }
}
