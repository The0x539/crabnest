use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

#[cfg(feature = "cyclecheck")]
use std::{cell::Cell, rc::Rc};

use sdl2::{keyboard::Scancode, EventPump};

use crate::membus::{MemBus, MemRead, MemWrite};
use crate::mos6502::{self, Mos6502};
use crate::reset_manager::{Reset, ResetManager};
use crate::timekeeper::Timekeeper;
use crate::{r, R};

pub const CONTROLLER_NBUTTONS: usize = 8;

#[allow(dead_code)]
pub enum Button {
    A = 0,
    B = 1,
    Select = 2,
    Start = 3,
    Up = 4,
    Down = 5,
    Left = 6,
    Right = 7,
}

pub struct IoReg {
    cpu_mem: R<MemBus>,
    tk: R<Timekeeper>,
    #[cfg(feature = "cyclecheck")]
    cpu_last_takeover_delay: Rc<Cell<u64>>,
    event_pump: R<EventPump>,
    controller_strobe: bool,
    controller_shiftregs: [u8; 2],
    controller_mappings: [[Scancode; 2]; CONTROLLER_NBUTTONS],
}

impl IoReg {
    pub fn new(
        rm: &R<ResetManager>,
        cpu: &R<Mos6502>,
        event_pump: R<EventPump>,
        cscheme_path: &Path,
    ) -> io::Result<R<Self>> {
        let io = r(Self {
            cpu_mem: cpu.borrow().bus.clone(),
            tk: cpu.borrow().tk.clone(),
            #[cfg(feature = "cyclecheck")]
            cpu_last_takeover_delay: cpu.borrow().last_takeover_delay.clone(),
            event_pump,
            controller_strobe: false,
            controller_shiftregs: [0, 0],
            controller_mappings: [[Scancode::A; 2]; CONTROLLER_NBUTTONS],
        });
        rm.borrow_mut().add_device(&io);

        let mut f = BufReader::new(File::open(cscheme_path)?);

        let mut line_buffer = String::new();
        for playernum in 0..2 {
            for buttonnum in 0..CONTROLLER_NBUTTONS {
                line_buffer.clear();
                f.read_line(&mut line_buffer)?;
                let line = line_buffer.trim_end();
                let scancode = Scancode::from_name(line).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Error parsing {cscheme_path:?}: unknown key '{line}'"),
                    )
                })?;
                io.borrow_mut().controller_mappings[buttonnum][playernum] = scancode;
            }
        }

        Ok(io)
    }

    fn set_strobe(&mut self, val: bool) {
        if self.controller_strobe && !val {
            self.tk.borrow_mut().sync();

            let mut ev = self.event_pump.borrow_mut();
            ev.pump_events();

            let kbstate = ev.keyboard_state();
            for j in 0..2 {
                for i in 0..CONTROLLER_NBUTTONS {
                    let button = self.controller_mappings[i][j];
                    self.controller_shiftregs[j] |=
                        (kbstate.is_scancode_pressed(button) as u8) << i as u8;
                }
            }
        }
        self.controller_strobe = val;
    }
}

impl Reset for IoReg {
    fn reset(&mut self) {
        self.controller_shiftregs = [0, 0];
        self.controller_strobe = false;
    }
}

impl MemRead for IoReg {
    fn read(&mut self, addr: u16, lanemask: &mut u8) -> u8 {
        match addr {
            0x16 | 0x17 => {
                let i = addr as usize - 0x16;
                *lanemask = 0x1F;

                let bit: u8;
                if self.controller_strobe {
                    self.tk.borrow_mut().sync();

                    let mut ev = self.event_pump.borrow_mut();
                    ev.pump_events();
                    let kbstate = ev.keyboard_state();

                    let button = self.controller_mappings[i][Button::A as usize];
                    bit = kbstate.is_scancode_pressed(button) as u8;
                } else {
                    bit = self.controller_shiftregs[i] & 0x01;
                    self.controller_shiftregs[i] >>= 1;
                }

                bit
            }
            _ => {
                *lanemask = 0x00;
                0x00
            }
        }
    }
}

impl MemWrite for IoReg {
    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x16 => self.set_strobe(val & 0x01 != 0),

            0x14 => {
                let readaddr = (val as u16) << 8;

                macro_rules! tick {
                    () => {
                        self.tk.borrow_mut().advance_clk(mos6502::CLK_DIVISOR);
                        #[cfg(feature = "cyclecheck")]
                        {
                            self.cpu_last_takeover_delay
                                .set(self.cpu_last_takeover_delay.get() + 1);
                        }
                    };
                }

                tick!();
                if (self.tk.borrow().clk_cyclenum / mos6502::CLK_DIVISOR) % 2 != 0 {
                    tick!();
                }

                let mem = self.cpu_mem.borrow();
                for i in 0..256 {
                    let byte = mem.read(readaddr + i);
                    tick!();
                    mem.write(0x2004, byte);
                    tick!();
                }
            }

            _ => (),
        }
    }
}
