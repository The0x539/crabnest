use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

#[cfg(feature = "cyclecheck")]
use std::{cell::Cell, rc::Rc};

use ouroboros::self_referencing;
use sdl2::controller::{Axis, Button, GameController};
use sdl2::keyboard::KeyboardState;
use sdl2::Sdl;
use sdl2::{keyboard::Scancode, EventPump};

use crate::membus::{MemBus, MemRead, MemWrite};
use crate::mos6502::{self, Mos6502};
use crate::reset_manager::{Reset, ResetManager};
use crate::timekeeper::Timekeeper;
use crate::{r, R};

// A, B, SELECT, START, UP, DOWN, LEFT, RIGHT
pub const CONTROLLER_NBUTTONS: usize = 8;
const GAMEPAD_MAPPINGS: [Button; CONTROLLER_NBUTTONS] = [
    Button::A,
    Button::B,
    Button::Back,
    Button::Start,
    Button::DPadUp,
    Button::DPadDown,
    Button::DPadLeft,
    Button::DPadRight,
];

const FOURSCORE_SIGNATURE: [u32; 2] = [0b00001000, 0b00000100];

struct WorldAccess {
    tk: R<Timekeeper>,
    event_pump: R<EventPump>,
    keyboard_mappings: [[Scancode; 4]; CONTROLLER_NBUTTONS],
    controllers: Vec<GameController>,
}

impl WorldAccess {
    fn input_state(&mut self) -> InputState<'_> {
        self.tk.borrow_mut().sync();
        let mut event_pump = self.event_pump.borrow_mut();
        event_pump.pump_events();
        InputState::new(
            event_pump,
            &self.keyboard_mappings,
            &self.controllers,
            |pump| pump.keyboard_state(),
        )
    }
}

fn joystick(controller: &GameController, button: Button) -> bool {
    const THRESHOLD: i16 = 8192; // 25%
    let (axis, sign) = match button {
        Button::DPadUp => (Axis::LeftY, -1),
        Button::DPadDown => (Axis::LeftY, 1),
        Button::DPadLeft => (Axis::LeftX, -1),
        Button::DPadRight => (Axis::LeftX, 1),
        _ => return false,
    };
    let val = controller.axis(axis);
    val.signum() == sign && val.saturating_abs() > THRESHOLD
}

#[self_referencing]
struct InputState<'a> {
    event_pump: std::cell::RefMut<'a, EventPump>,
    keyboard_mappings: &'a [[Scancode; 4]; CONTROLLER_NBUTTONS],
    controllers: &'a [GameController],
    #[borrows(event_pump)]
    #[covariant]
    keyboard_state: KeyboardState<'this>,
}

impl InputState<'_> {
    fn pressed(&self, player: usize, button: usize) -> bool {
        self.with(|this| {
            if let Some(controller) = this.controllers.get(player) {
                let sdl_button = GAMEPAD_MAPPINGS[button];
                if controller.button(sdl_button) || joystick(controller, sdl_button) {
                    return true;
                }
            }
            let scancode = this.keyboard_mappings[button][player];
            this.keyboard_state.is_scancode_pressed(scancode)
        })
    }
}

pub struct IoReg {
    cpu_mem: R<MemBus>,
    world: WorldAccess,
    #[cfg(feature = "cyclecheck")]
    cpu_last_takeover_delay: Rc<Cell<u64>>,
    controller_strobe: bool,
    controller_shiftregs: [u32; 2],
}

impl IoReg {
    pub fn new(
        rm: &R<ResetManager>,
        cpu: &mut Mos6502,
        sdl: &Sdl,
        event_pump: R<EventPump>,
        cscheme_path: &Path,
    ) -> io::Result<R<Self>> {
        let mut keyboard_mappings = [[Scancode::A; 4]; CONTROLLER_NBUTTONS];

        let mut f = BufReader::new(File::open(cscheme_path)?);

        let mut line_buffer = String::new();
        'eof: for playernum in 0..4 {
            for buttonnum in 0..CONTROLLER_NBUTTONS {
                line_buffer.clear();
                match f.read_line(&mut line_buffer) {
                    Ok(_) => (),
                    Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => break 'eof,
                    Err(e) => return Err(e),
                }
                let line = line_buffer.trim_end();
                if line.is_empty() {
                    break 'eof;
                }

                let err_msg = || format!("Error parsing {cscheme_path:?}: unknown key '{line}'");
                let err = || io::Error::new(io::ErrorKind::InvalidData, err_msg());

                let scancode = Scancode::from_name(line).ok_or_else(err)?;
                keyboard_mappings[buttonnum][playernum] = scancode;
            }
        }

        let controller_subsystem = sdl
            .game_controller()
            .expect("Couldn't initialize controller subsystem");

        let num_controllers = controller_subsystem
            .num_joysticks()
            .expect("Couldn't get controller count")
            .min(4);

        let controllers = (0..num_controllers)
            .map(|n| {
                controller_subsystem
                    .open(n)
                    .expect("Couldn't open controller")
            })
            .collect::<Vec<_>>();

        let io = r(Self {
            cpu_mem: cpu.bus.clone(),
            world: WorldAccess {
                tk: cpu.tk.clone(),
                event_pump,
                controllers,
                keyboard_mappings,
            },
            #[cfg(feature = "cyclecheck")]
            cpu_last_takeover_delay: cpu.last_takeover_delay.clone(),
            controller_strobe: false,
            controller_shiftregs: [0, 0],
        });
        rm.borrow_mut().add_device(&io);

        Ok(io)
    }

    fn set_strobe(&mut self, val: bool) {
        if self.controller_strobe && !val {
            self.controller_shiftregs = FOURSCORE_SIGNATURE;
            let input = self.world.input_state();
            for player in [2, 3, 0, 1] {
                for button in (0..CONTROLLER_NBUTTONS).rev() {
                    let bit = input.pressed(player, button);
                    let reg = &mut self.controller_shiftregs[player % 2];
                    *reg = (*reg << 1) | bit as u32;
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

                let bit;
                if self.controller_strobe {
                    let input = self.world.input_state();
                    bit = input.pressed(i, 0 /* A */);
                } else {
                    bit = self.controller_shiftregs[i] & 0x01 != 0;
                    self.controller_shiftregs[i] >>= 1;
                }

                bit as u8
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

                let tk = &self.world.tk;

                macro_rules! tick {
                    () => {
                        tk.borrow_mut().advance_clk(mos6502::CLK_DIVISOR);
                        #[cfg(feature = "cyclecheck")]
                        {
                            self.cpu_last_takeover_delay
                                .set(self.cpu_last_takeover_delay.get() + 1);
                        }
                    };
                }

                tick!();
                if (tk.borrow().clk_cyclenum / mos6502::CLK_DIVISOR) % 2 != 0 {
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
