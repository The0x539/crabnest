mod dmc;
mod noise;
mod pulse;
mod regs;
mod triangle;

use self::{dmc::Dmc, noise::Noise, pulse::Pulse, regs::*, triangle::Triangle};

use bytemuck::Zeroable;
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    Sdl,
};

use crate::{
    membus::{MemRead, MemWrite},
    mos6502::{self, IrqLine, Mos6502},
    reset_manager::ResetManager,
    timekeeper::Timed,
    util::{r, R},
};

const QUARTER_FRAME: u64 = 358000 / 4;

const LENGTH_COUNTERS: [u8; 32] = [
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22,
    192, 24, 72, 26, 16, 28, 32, 30,
];

#[derive(Default)]
struct Envelope {
    start_flag: bool,
    divider: u8,
    decay: u8,
}

impl Envelope {
    fn quarter_frame(&mut self, loop_flag: bool, period: u8) {
        if self.start_flag {
            self.start_flag = false;
            self.divider = period;
            self.decay = 15;
            return;
        }

        if self.divider == 0 {
            self.divider = period;
            if self.decay == 0 {
                if loop_flag {
                    self.decay = 15;
                }
            } else {
                self.decay -= 1;
            }
        } else {
            self.divider -= 1;
        }
    }

    fn output(&self, active: bool, constant: bool, volume: u8) -> u8 {
        match (active, constant) {
            (true, true) => volume,
            (true, false) => self.decay,
            (false, _) => 0,
        }
    }
}

trait Channel {
    type Control;
    fn set_lc(&mut self, val: u8);
    fn quarter_frame(&mut self, control: &Self::Control);
    fn half_frame(&mut self, control: &mut Self::Control);
    fn tick(&mut self, control: &Self::Control);
    fn sample(&self, control: &Self::Control) -> u8;
}

struct FrameCounter {
    timer: u8,
    irq_line: IrqLine,
}

impl FrameCounter {
    fn update_irq(&mut self, control: &FrameCounterControl) {
        if control.disable_int() {
            self.irq_line.clear();
        } else if !control.sequence() && self.timer == 3 {
            self.irq_line.raise();
        }
    }

    fn step(&mut self, control: &FrameCounterControl) {
        self.timer += 1;
        // 5 steps if 1, 4 steps if 0
        self.timer %= 4 + control.sequence() as u8;

        self.update_irq(control);
    }

    fn quarter(&self, control: &FrameCounterControl) -> bool {
        if control.sequence() {
            self.timer != 3
        } else {
            true
        }
    }

    fn half(&self, control: &FrameCounterControl) -> bool {
        if control.sequence() {
            self.timer == 1 || self.timer == 4
        } else {
            self.timer == 1 || self.timer == 3
        }
    }
}

pub struct Apu {
    regs: ApuRegs,

    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
    dmc: Dmc,

    frame_counter: FrameCounter,

    samples: Vec<f32>,
    queue: AudioQueue<f32>,
}

impl Apu {
    fn do_frame_tick(&mut self) {
        let fc = &mut self.frame_counter;
        let fcc = &self.regs.frame_counter;
        fc.step(fcc);

        if fc.quarter(fcc) {
            self.pulse1.quarter_frame(&self.regs.pulse1);
            self.pulse2.quarter_frame(&self.regs.pulse2);
            self.triangle.quarter_frame(&self.regs.triangle);
            self.noise.quarter_frame(&self.regs.noise);
            self.dmc.quarter_frame(&self.regs.dmc);
        }
        if fc.half(fcc) {
            self.pulse1.half_frame(&mut self.regs.pulse1);
            self.pulse2.half_frame(&mut self.regs.pulse2);
            self.triangle.half_frame(&mut self.regs.triangle);
            self.noise.half_frame(&mut self.regs.noise);
            self.dmc.half_frame(&mut self.regs.dmc);
        }

        self.queue.queue(&self.samples);
        self.samples.clear();
    }

    fn mix_sample(&mut self) {
        let triangle = self.triangle.sample(&self.regs.triangle) as f64;
        let noise = self.noise.sample(&self.regs.noise) as f64;
        let dmc = self.dmc.sample(&self.regs.dmc) as f64;
        let pulse1 = self.pulse1.sample(&self.regs.pulse1) as f64;
        let pulse2 = self.pulse2.sample(&self.regs.pulse2) as f64;

        let tnd_out = if triangle + noise + dmc != 0.0 {
            159.79 / (1. / (triangle / 8227. + noise / 12241. + dmc / 22638.) + 100.)
        } else {
            0.0
        };
        let pulse_out = if pulse1 + pulse2 != 0.0 {
            95.88 / (8128. / (pulse1 + pulse2) + 100.)
        } else {
            0.0
        };

        let output = pulse_out + tnd_out;

        self.samples.push(output as f32);
    }

    pub fn new(
        sdl: &Sdl,
        // TODO: actually use this (the thing I've done with timers might make it tricky)
        _rm: &R<ResetManager>,
        cpu: &Mos6502,
    ) -> R<Self> {
        let audio = sdl.audio().expect("Could not init SDL audio");
        let queue = audio
            .open_queue(
                None,
                &AudioSpecDesired {
                    freq: Some(44100),
                    channels: Some(1),
                    samples: None,
                },
            )
            .expect("Could not open audio queue");

        queue.resume();

        let frame_counter = FrameCounter {
            timer: 0,
            irq_line: cpu.get_irq_line(),
        };

        let apu = Apu {
            regs: Zeroable::zeroed(),

            pulse1: Pulse::new(false),
            pulse2: Pulse::new(true),
            triangle: Triangle::default(),
            noise: Noise::new(),
            dmc: Dmc::new(cpu),

            frame_counter,

            samples: Vec::with_capacity(65536),
            queue,
        };

        let apu = r(apu);

        let mut tk = cpu.tk.borrow_mut();
        tk.add_timer(ApuCycleTimer {
            apu: apu.clone(),
            even_cycle: false,
            countdown: mos6502::CLK_DIVISOR,
        });
        tk.add_timer(ApuFrameTimer {
            apu: apu.clone(),
            countdown: QUARTER_FRAME,
        });
        tk.add_timer(ApuSampleTimer {
            apu: apu.clone(),
            countdown: 487,
        });

        apu
    }
}

impl MemRead for Apu {
    fn read(&mut self, addr: u16, lane_mask: &mut u8) -> u8 {
        if addr == 0x15 {
            *lane_mask = 0xFF;
            let mut status = ApuStatus::new();
            status.set_frame_int(self.frame_counter.irq_line.clear());

            status.set_dmc_int(self.dmc.irq_active());

            status.set_noise_lc_st(self.noise.active());
            status.set_tri_lc_st(self.triangle.active());
            status.set_p2_lc_st(self.pulse2.active());
            status.set_p1_lc_st(self.pulse1.active());

            status.set_dmc_active(self.dmc.has_more_bytes());

            bytemuck::cast(status)
        } else {
            *lane_mask = 0;
            0
        }
    }
}

impl MemWrite for Apu {
    fn write(&mut self, addr: u16, data: u8) {
        if (0x00..=0x17).contains(&addr) {
            bytemuck::bytes_of_mut(&mut self.regs)[addr as usize] = data;
        } else {
            return;
        }
        let length_counter = LENGTH_COUNTERS[data as usize >> 3];
        match addr {
            0x01 => self.pulse1.reload_sweep(),
            0x03 => {
                self.pulse1.set_lc(length_counter);
            }
            0x05 => self.pulse2.reload_sweep(),
            0x07 => self.pulse2.set_lc(length_counter),
            0x0B => self.triangle.set_lc(length_counter),
            0x0F => self.noise.set_lc(length_counter),
            0x10 => {
                if !self.regs.dmc.irq() {
                    self.dmc.clear_irq();
                }
            }
            0x11 => self.dmc.set_output_level(self.regs.dmc.direct_load()),
            0x12 => {
                let s_addr = self.regs.dmc.s_addr() as u16 * 64 + 0xC000;
                self.dmc.set_sample_start(s_addr);
            }
            0x13 => {
                let length = self.regs.dmc.s_len() as u16 * 16 + 1;
                self.dmc.set_sample_length(length);
            }
            0x15 => {
                self.dmc.clear_irq();
                if self.regs.control.dmc_lc_en() {
                    self.dmc.restart();
                }
            }
            0x17 => {
                self.frame_counter.timer = 0;
                if self.regs.frame_counter.sequence() {
                    self.pulse1.quarter_frame(&self.regs.pulse1);
                    self.pulse2.quarter_frame(&self.regs.pulse2);
                    self.triangle.quarter_frame(&self.regs.triangle);
                    self.noise.quarter_frame(&self.regs.noise);
                    self.dmc.quarter_frame(&self.regs.dmc);

                    self.pulse1.half_frame(&mut self.regs.pulse1);
                    self.pulse2.half_frame(&mut self.regs.pulse2);
                    self.triangle.half_frame(&mut self.regs.triangle);
                    self.noise.half_frame(&mut self.regs.noise);
                    self.dmc.half_frame(&mut self.regs.dmc);
                }
                self.frame_counter.update_irq(&self.regs.frame_counter);
                // TODO: wonky *delayed* reset
            }
            _ => (),
        }
    }
}

pub struct ApuCycleTimer {
    apu: R<Apu>,
    even_cycle: bool,
    countdown: u64,
}

impl Timed for ApuCycleTimer {
    fn fire(&mut self) {
        self.countdown = mos6502::CLK_DIVISOR;

        let Apu {
            ref regs,
            ref mut pulse1,
            ref mut pulse2,
            ref mut triangle,
            ref mut noise,
            ref mut dmc,
            ..
        } = &mut *self.apu.borrow_mut();

        if !regs.control.p1_lc_en() {
            pulse1.deactivate();
        }
        if !regs.control.p2_lc_en() {
            pulse2.deactivate();
        }
        if !regs.control.tri_lc_en() {
            triangle.deactivate();
        }
        if !regs.control.noise_lc_en() {
            noise.deactivate();
        }
        if !regs.control.dmc_lc_en() {
            // TODO: ???
        }

        triangle.tick(&regs.triangle);
        if self.even_cycle {
            pulse1.tick(&regs.pulse1);
            pulse2.tick(&regs.pulse2);
            noise.tick(&regs.noise);
            dmc.tick(&regs.dmc);
        }

        self.even_cycle = !self.even_cycle;
    }

    fn countdown(&self) -> u64 {
        self.countdown
    }

    fn advance_countdown(&mut self, n: u64) {
        self.countdown -= n;
    }
}

pub struct ApuFrameTimer {
    apu: R<Apu>,
    countdown: u64,
}

impl Timed for ApuFrameTimer {
    fn fire(&mut self) {
        self.countdown = QUARTER_FRAME;
        self.apu.borrow_mut().do_frame_tick();
    }

    fn countdown(&self) -> u64 {
        self.countdown
    }

    fn advance_countdown(&mut self, n: u64) {
        self.countdown -= n;
    }
}

pub struct ApuSampleTimer {
    apu: R<Apu>,
    countdown: u64,
}

impl Timed for ApuSampleTimer {
    fn fire(&mut self) {
        self.countdown = 487;
        self.apu.borrow_mut().mix_sample();
    }

    fn countdown(&self) -> u64 {
        self.countdown
    }

    fn advance_countdown(&mut self, n: u64) {
        self.countdown -= n;
    }
}
