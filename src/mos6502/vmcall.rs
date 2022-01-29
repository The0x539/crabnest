use std::fs::File;

#[cfg(unix)]
use std::{
    ffi::OsString,
    os::unix::prelude::{IntoRawFd, OpenOptionsExt, OsStringExt},
};

#[cfg(windows)]
use std::io::{Read, Write};

use super::{Mos6502, StepResult};

const VMCALL_ARGS: u8 = 0;
const VMCALL_EXIT: u8 = 1;
const VMCALL_OPEN: u8 = 2;
const VMCALL_CLOSE: u8 = 3;
const VMCALL_READ: u8 = 4;
const VMCALL_WRITE: u8 = 5;
const VMCALL_BREAK: u8 = 6;
const VMCALL_DUMP: u8 = 7;

#[cfg(unix)]
fn unwrap_result(r: nix::Result<usize>) -> u16 {
    match r {
        Ok(n) => n as u16,
        Err(e) => e as i32 as u16,
    }
}

#[cfg(windows)]
fn unwrap_result(r: std::io::Result<usize>) -> u16 {
    match r {
        Ok(n) => n as u16,
        Err(e) => e.raw_os_error().unwrap() as u16,
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
        let argv = self.get_ax();
        let mut sp = self.read_zp16(0x00);
        let mut args = sp - (self.paravirt_args.len() as u16 + 1) * 2;

        self.write16(argv, args);

        sp = args;

        // This is doing something fairly simple, but it's expressed in a way that I can't really make sense of.
        for i in 0..self.paravirt_args.len() {
            let thearg = &self.paravirt_args[i];
            sp -= thearg.len() as u16 + 1;
            for (spi, b) in thearg.bytes().enumerate() {
                self.bus.borrow_mut().write(sp + spi as u16, b);
            }
            self.bus.borrow_mut().write(sp + thearg.len() as u16, 0);

            self.write16(args, sp);
            args += 2;
        }

        self.write16(0x0000, sp);
        self.set_ax(self.paravirt_args.len() as u16);

        StepResult::Success
    }

    fn handle_exit(&mut self) -> StepResult {
        println!("Received Paravirtual Exit Request. Goodbye.");
        std::process::exit(1);
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

    fn handle_open(&mut self) -> StepResult {
        let mode = self.pop_parm(self.y as u16 - 4);
        let flags = self.pop_parm(2);
        let name = self.pop_parm(2);

        let mut path = Vec::new();
        for addr in name.. {
            let byte = self.read8(addr);
            if byte == b'\0' {
                break;
            }
            path.push(byte);
        }
        #[cfg(windows)]
        let path = String::from_utf8(path).unwrap(/* ¯\_(ツ)_/¯ */);
        #[cfg(unix)]
        let path = OsString::from_vec(path);

        let mut opts = File::options();

        macro_rules! flag {
            ($mask:literal, $method:ident) => {
                if flags & $mask != 0 {
                    opts.$method(true);
                }
            };
        }

        flag!(0x01, read);
        flag!(0x02, write);
        flag!(0x10, create);
        flag!(0x20, truncate);
        flag!(0x40, append);
        flag!(0x80, create_new);

        #[cfg(unix)]
        opts.mode(mode as u32);
        #[cfg(windows)]
        let _ = mode;

        let ret = match opts.open(path) {
            Ok(f) => {
                #[cfg(unix)]
                let fd = f.into_raw_fd() as u16;
                #[cfg(windows)]
                let fd = self.open_files.insert(f) as u16 + 3;
                fd
            }
            Err(e) => e.raw_os_error().unwrap() as u16,
        };
        self.set_ax(ret);

        StepResult::Success
    }

    fn handle_close(&mut self) -> StepResult {
        let fd = self.get_ax();

        #[cfg(unix)]
        let ret = unwrap_result(nix::unistd::close(fd as i32).map(|()| 0));

        #[cfg(windows)]
        let ret = match fd {
            0 | 1 | 2 => -1_i32 as u16, // whatever
            n => match self.open_files.try_remove(n as usize - 3) {
                Some(_) => 0,
                None => -1_i32 as u16,
            },
        };

        self.set_ax(ret);
        StepResult::Success
    }

    fn handle_read(&mut self) -> StepResult {
        let count = self.get_ax() as usize;
        let mut buf = self.pop_parm(2);
        let fd = self.pop_parm(2) as i32;

        let mut data = vec![0; count];

        #[cfg(unix)]
        let ret = nix::unistd::read(fd, &mut data);

        #[cfg(windows)]
        let ret = match fd {
            0 | 1 | 2 => std::io::stdin().read(&mut data),
            n => {
                if let Some(f) = self.open_files.get_mut(n as usize - 3) {
                    f.read(&mut data)
                } else {
                    Err(std::io::Error::from_raw_os_error(-1))
                }
            }
        };

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

        #[cfg(unix)]
        let ret = nix::unistd::write(fd, &data);

        #[cfg(windows)]
        let ret = match fd {
            0 | 1 => std::io::stdout().write(&data),
            2 => std::io::stderr().write(&data),
            n => {
                if let Some(f) = self.open_files.get_mut(n as usize - 3) {
                    f.write(&data)
                } else {
                    Err(std::io::Error::from_raw_os_error(-1))
                }
            }
        };

        self.set_ax(unwrap_result(ret));

        StepResult::Success
    }
}
