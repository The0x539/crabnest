use super::{Mos6502, StepResult};

const VMCALL_ARGS: u8 = 0;
const VMCALL_EXIT: u8 = 1;
const VMCALL_OPEN: u8 = 2;
const VMCALL_CLOSE: u8 = 3;
const VMCALL_READ: u8 = 4;
const VMCALL_WRITE: u8 = 5;
const VMCALL_BREAK: u8 = 6;
const VMCALL_DUMP: u8 = 7;

fn unwrap_result(r: nix::Result<usize>) -> u16 {
    match r {
        Ok(n) => n as u16,
        Err(e) => e as i32 as u16,
    }
}

impl Mos6502 {
    fn get_ax(&self) -> u16 {
        u16::from_le_bytes([self.a, self.x])
    }

    fn set_ax(&mut self, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.a = lo;
        self.x = hi;
    }

    fn read_zp16(&self, addr: u8) -> u16 {
        let lo = self.read8(addr as u16);
        let hi = self.read8(addr as u16 + 1);
        u16::from_le_bytes([lo, hi])
    }

    // Parmesan?
    fn pop_parm(&mut self, incr: u16) -> u16 {
        let sp = self.read_zp16(0x00);
        let val = u16::from_le_bytes([self.read8(sp), self.read8(sp + 1)]);
        self.write16(0x0000, sp + incr);
        val
    }

    fn handle_args(&mut self) -> StepResult {
        todo!()
    }
    fn handle_exit(&mut self) -> StepResult {
        println!("Received Paravirtual Exit Request. Goodbye.");
        std::process::exit(1);
    }
    fn handle_open(&mut self) -> StepResult {
        todo!()
    }
    fn handle_close(&mut self) -> StepResult {
        todo!()
    }
    fn handle_read(&mut self) -> StepResult {
        let count = self.get_ax() as usize;
        let mut buf = self.pop_parm(2);
        let fd = self.pop_parm(2) as i32;

        let mut data = vec![0; count];
        let ret = nix::unistd::read(fd, &mut data);

        if let Ok(r) = ret {
            for i in 0..r {
                self.write8(buf, data[i]);
                buf += 1;
            }
        }

        self.set_ax(unwrap_result(ret));

        StepResult::Success
    }
    fn handle_write(&mut self) -> StepResult {
        let count = self.get_ax() as usize;
        let mut buf = self.pop_parm(2);
        let fd = self.pop_parm(3) as i32;

        let mut data = vec![0; count];
        for i in 0..count {
            data[i] = self.read8(buf);
            buf += 1;
        }
        let ret = unwrap_result(nix::unistd::write(fd, &data));

        self.set_ax(ret);

        StepResult::Success
    }
    fn handle_dump(&mut self) -> StepResult {
        println!("EXIT STATE:");
        println!("PC: 0x{:04x}", self.pc);
        println!("SP: 0x{:04x}", self.sp);
        println!(" A: 0x{:02x}", self.a);
        println!(" X: 0x{:02x}", self.x);
        println!(" Y: 0x{:02x}", self.y);
        println!(" P: 0x{:02x}", self.p.bits);

        for addr in (0..0x3000).step_by(4) {
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
