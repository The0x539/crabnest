use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

use sdl2::{keyboard::Scancode, EventPump};

use crate::membus::{MemRead, MemWrite};
use crate::mos6502::Mos6502;
use crate::reset_manager::{Reset, ResetManager};
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
    cpu: R<Mos6502>,
    event_pump: R<EventPump>,
    controller_strobe: bool,
    controller_shiftregs: [u8; 2],
    controller_mappings: [[Scancode; 2]; CONTROLLER_NBUTTONS],
}

impl IoReg {
    fn new(
        rm: &R<ResetManager>,
        cpu: &R<Mos6502>,
        event_pump: R<EventPump>,
        cscheme_path: &Path,
    ) -> io::Result<R<Self>> {
        let io = r(Self {
            cpu: cpu.clone(),
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

    pub fn setup(
        rm: &R<ResetManager>,
        cpu: &R<Mos6502>,
        event_pump: &R<EventPump>,
        cscheme_path: &Path,
    ) -> io::Result<()> {
        let io = Self::new(rm, cpu, event_pump.clone(), cscheme_path)?;

        let cpu = cpu.borrow_mut();
        let mut bus = cpu.bus.borrow_mut();

        bus.set_read_handler(0x40, &io, 0);
        bus.set_write_handler(0x40, &io, 0);

        Ok(())
    }

    fn set_strobe(&mut self, val: bool) {
        if self.controller_strobe && !val {
            self.cpu.borrow_mut().tk.borrow_mut().sync();

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
                    self.cpu.borrow_mut().tk.borrow_mut().sync();

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

                let mut cpu = self.cpu.borrow_mut();
                cpu.advance_clk(1);

                // cpu.last_takeover_delay += 1;

                for i in 0..256 {
                    let byte = cpu.read8(readaddr + i);
                    cpu.advance_clk(1);
                    cpu.write8(0x2004, byte);
                    cpu.advance_clk(1);
                    // cpu.last_takeover_delay += 2;
                }
            }

            _ => (),
        }
    }
}
