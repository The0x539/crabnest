#![allow(dead_code)]

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    Sdl,
};

use crate::{
    membus::{MemBus, MemRead, MemWrite},
    mos6502::{self, IrqLine, Mos6502},
    r,
    reset_manager::ResetManager,
    timekeeper::Timed,
    R,
};

const QUARTER_FRAME: u64 = 358000 / 4;

const LENGTH_COUNTERS: [u8; 32] = [
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22,
    192, 24, 72, 26, 16, 28, 32, 30,
];
const PULSE_SEQUENCES: [[u8; 8]; 4] = [
    [0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 1, 1],
    [0, 0, 0, 0, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 0, 0],
];
const DMC_RATES: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
];
const NOISE_PERIODS: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct PulseDuty {
    volume: B4,
    constant: bool,
    lch: bool,
    duty: B2,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Sweep {
    shift_count: B3,
    negative: bool,
    period: B3,
    enabled: bool,
}

#[bitfield(bits = 16)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Timer {
    timer: B11,
    load: B5,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct PulseControl {
    duty: PulseDuty,
    sweep: Sweep,
    timer: Timer,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct TriangleCounter {
    reload: B7,
    control: bool,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct TriangleControl {
    counter: TriangleCounter,
    _padding: u8,
    timer: Timer,
}

#[bitfield(bits = 32)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct NoiseControl {
    volume: B4,
    constant: bool,
    lch: bool,
    #[skip]
    __: B2,

    #[skip]
    __: B8,

    period: B4,
    #[skip]
    __: B3,
    mode: bool,

    #[skip]
    __: B3,
    load: B5,
}

#[bitfield(bits = 32)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct DmcControl {
    freq_idx: B4,
    #[skip]
    __: B2,
    repeat: bool,
    irq: bool,

    direct_load: B7,
    #[skip]
    __: B1,

    s_addr: u8,
    s_len: u8,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct ApuControl {
    p1_lc_en: bool,
    p2_lc_en: bool,
    tri_lc_en: bool,
    noise_lc_en: bool,
    dmc_lc_en: bool,
    #[skip]
    __: B3,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct ApuStatus {
    p1_lc_st: bool,
    p2_lc_st: bool,
    tri_lc_st: bool,
    noise_lc_st: bool,
    dmc_active: bool,
    #[skip]
    __: B1,
    frame_int: bool,
    dmc_int: bool,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct FrameCounterControl {
    #[skip]
    __: B6,
    disable_int: bool,
    sequence: bool,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct ApuRegs {
    pulse1: PulseControl,
    pulse2: PulseControl,
    triangle: TriangleControl,
    noise: NoiseControl,
    dmc: DmcControl,
    _padding1: u8,
    control: ApuControl,
    _padding2: u8,
    frame_counter: FrameCounterControl,
}

static_assertions::assert_eq_size!(ApuRegs, [u8; 0x18]);

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

#[derive(Default)]
struct Pulse {
    sequence: [u8; 8],
    sequence_pos: usize,
    timer: u16,
    length_counter: u8,
    sweep_counter: u8,
    reload_sweep: bool,
    is_pulse2: bool,
    envelope: Envelope,
}

impl Pulse {
    fn target_period(&self, control: &PulseControl) -> u16 {
        let raw_period = control.timer.timer();
        let change_amount = raw_period >> control.sweep.shift_count();
        match (control.sweep.negative(), self.is_pulse2) {
            (true, true) => raw_period - change_amount - 1,
            (true, false) => raw_period - change_amount,
            (false, _) => raw_period + change_amount,
        }
    }

    fn update_period(&mut self, control: &mut PulseControl) {
        let period = self.target_period(control);
        if control.sweep.enabled() && self.sweep_counter == 0 && period <= 0x07FF {
            control.timer.set_timer(period);
        }
        if self.sweep_counter == 0 || self.reload_sweep {
            self.reload_sweep = false;
            self.sweep_counter = control.sweep.period();
        } else {
            self.sweep_counter -= 1;
        }
    }
}

impl Channel for Pulse {
    type Control = PulseControl;

    fn set_lc(&mut self, val: u8) {
        self.length_counter = val;
        self.sequence_pos = 0;
        self.envelope.start_flag = true;
    }

    fn quarter_frame(&mut self, control: &Self::Control) {
        self.envelope
            .quarter_frame(control.duty.lch(), control.duty.volume());
    }

    fn half_frame(&mut self, control: &mut Self::Control) {
        if !control.duty.lch() {
            self.length_counter = self.length_counter.saturating_sub(1);
        }
        self.update_period(control);
    }

    fn tick(&mut self, control: &Self::Control) {
        let period = control.timer.timer();

        if self.timer == 0 {
            self.timer = period;
            self.sequence_pos = self.sequence_pos.checked_sub(1).unwrap_or(7);
        } else {
            self.timer -= 1;
        }

        self.sequence = PULSE_SEQUENCES[control.duty.duty() as usize];
    }

    fn sample(&self, control: &Self::Control) -> u8 {
        if self.length_counter == 0 {
            return 0;
        }

        if control.timer.timer() < 8 || self.target_period(control) > 0x07FF {
            return 0;
        }

        self.envelope.output(
            self.sequence[self.sequence_pos] != 0,
            control.duty.constant(),
            control.duty.volume(),
        )
    }
}

#[derive(Default)]
struct Triangle {
    sequence_pos: u8,
    timer: u16,
    length_counter: u8,
    linear_counter: u8,
    linear_counter_reload: bool,
}

impl Channel for Triangle {
    type Control = TriangleControl;

    fn set_lc(&mut self, val: u8) {
        self.length_counter = val;
        self.linear_counter_reload = true;
    }

    fn quarter_frame(&mut self, control: &Self::Control) {
        if self.linear_counter_reload {
            self.linear_counter = control.counter.reload();
        } else {
            self.linear_counter = self.linear_counter.saturating_sub(1);
        }

        if !control.counter.control() {
            self.linear_counter_reload = false;
        }
    }

    fn half_frame(&mut self, control: &mut Self::Control) {
        if !control.counter.control() {
            self.length_counter = self.length_counter.saturating_sub(1);
        }
    }

    fn tick(&mut self, control: &Self::Control) {
        if self.timer == 0 {
            self.timer = control.timer.timer() + 1;
            if self.linear_counter != 0 && self.length_counter != 0 {
                self.sequence_pos = (self.sequence_pos + 1) % 32;
            }
        } else {
            self.timer -= 1;
        }
    }

    fn sample(&self, _control: &Self::Control) -> u8 {
        match self.sequence_pos {
            n @ 0..=15 => 15 - n,
            n @ 16..=31 => n - 16,
            32.. => unreachable!(),
        }
    }
}

#[derive(Default)]
struct Noise {
    envelope: Envelope,
    length_counter: u8,
    timer: u16,
    shift_reg: u16,
}

impl Noise {
    fn sr_bit(&self, n: u8) -> u16 {
        (self.shift_reg >> n) & 1
    }
}

impl Channel for Noise {
    type Control = NoiseControl;

    fn set_lc(&mut self, val: u8) {
        self.length_counter = val;
        self.envelope.start_flag = true;
    }

    fn quarter_frame(&mut self, control: &Self::Control) {
        self.envelope.quarter_frame(control.lch(), control.volume());
    }

    fn half_frame(&mut self, _control: &mut NoiseControl) {
        self.length_counter = self.length_counter.saturating_sub(1);
    }

    fn tick(&mut self, control: &Self::Control) {
        if self.timer == 0 {
            self.timer = NOISE_PERIODS[control.period() as usize];
            let feedback = self.sr_bit(0) ^ self.sr_bit(if control.mode() { 6 } else { 1 });
            self.shift_reg = (self.shift_reg >> 1) | (feedback << 14);
        } else {
            self.timer -= 1;
        }
    }

    fn sample(&self, control: &Self::Control) -> u8 {
        if self.length_counter == 0 {
            return 0;
        }
        self.envelope
            .output(self.sr_bit(0) != 0, control.lch(), control.volume())
    }
}

struct DmcMemReader {
    mem: R<MemBus>,
    start: u16,
    addr: u16,
    len: u16,
    bytes_remaining: u16,
}

impl DmcMemReader {
    fn next(&mut self, control: &DmcControl) -> Option<u8> {
        if self.bytes_remaining == 0 && control.repeat() {
            self.addr = self.start;
            self.bytes_remaining = self.len;
        }

        if self.bytes_remaining > 0 {
            // TODO: "stall the CPU"
            let val = self.mem.borrow().read(self.addr);
            self.bytes_remaining -= 1;
            self.addr = self.addr.checked_add(1).unwrap_or(0x8000);
            Some(val)
        } else {
            None
        }
    }
}

#[derive(Default)]
struct DmcOutputUnit {
    rsr: u8,
    bits_remaining: u8,
    output_level: u8,
    silence: bool,
}

struct Dmc {
    mem_reader: DmcMemReader,
    interrupt_flag: bool,
    sample_buffer: Option<u8>,
    timer: u8,
    output_unit: DmcOutputUnit,
}

impl Dmc {
    fn empty_sample_buffer(&mut self, control: &DmcControl) -> Option<u8> {
        let val = self.sample_buffer;
        self.sample_buffer = self.mem_reader.next(control);
        val
    }
}

impl Channel for Dmc {
    type Control = DmcControl;

    fn set_lc(&mut self, _val: u8) {}

    fn quarter_frame(&mut self, _control: &Self::Control) {}

    fn half_frame(&mut self, _control: &mut Self::Control) {}

    fn tick(&mut self, control: &Self::Control) {
        if self.timer == 0 {
            self.timer = (DMC_RATES[control.freq_idx() as usize] / 2) as u8;

            if !self.output_unit.silence {
                let v = &mut self.output_unit.output_level;
                if self.output_unit.rsr & 1 != 0 {
                    if *v <= 125 {
                        *v += 2;
                    }
                } else {
                    if *v >= 2 {
                        *v -= 2;
                    }
                }
            }

            self.output_unit.rsr >>= 1;

            if self.output_unit.bits_remaining == 0 {
                self.output_unit.bits_remaining = 8;
                if let Some(v) = self.empty_sample_buffer(control) {
                    self.output_unit.rsr = v;
                    self.output_unit.silence = false;
                } else {
                    self.output_unit.silence = true
                }
            } else {
                self.output_unit.bits_remaining -= 1;
            }
        } else {
            self.timer -= 1;
        }
    }

    fn sample(&self, _control: &Self::Control) -> u8 {
        self.output_unit.output_level
    }
}

fn inv_or_zero(n: f64) -> f64 {
    if n == 0.0 {
        0.0
    } else {
        1.0 / n
    }
}

struct FrameCounter {
    timer: u8,
    irq_line: IrqLine,
}

impl FrameCounter {
    fn step(&mut self, control: &FrameCounterControl) {
        self.timer += 1;
        // 5 steps if 1, 4 steps if 0
        self.timer %= 4 + control.sequence() as u8;

        if control.disable_int() {
            self.irq_line.clear();
        } else if !control.sequence() && self.timer == 3 {
            self.irq_line.raise();
        }
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
        cpu: &R<Mos6502>,
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

        let cpu = cpu.borrow();

        let dmc = Dmc {
            mem_reader: DmcMemReader {
                mem: cpu.bus.clone(),
                start: 0,
                addr: 0,
                len: 0,
                bytes_remaining: 0,
            },
            interrupt_flag: false,
            sample_buffer: None,
            timer: 0,
            output_unit: Default::default(),
        };

        let frame_counter = FrameCounter {
            timer: 0,
            irq_line: cpu.get_irq_line(),
        };

        let mut apu = Apu {
            regs: Zeroable::zeroed(),

            pulse1: Default::default(),
            pulse2: Default::default(),
            triangle: Default::default(),
            noise: Default::default(),
            dmc,

            frame_counter,

            samples: Vec::with_capacity(65536),
            queue,
        };

        apu.pulse2.is_pulse2 = true;
        apu.noise.shift_reg = 1;

        let apu = r(apu);

        let mut tk = cpu.tk.borrow_mut();
        tk.add_timer(r(ApuCycleTimer {
            apu: apu.clone(),
            even_cycle: false,
            countdown: mos6502::CLK_DIVISOR,
        }));
        tk.add_timer(r(ApuFrameTimer {
            apu: apu.clone(),
            countdown: QUARTER_FRAME,
        }));
        tk.add_timer(r(ApuSampleTimer {
            apu: apu.clone(),
            countdown: 487,
        }));

        apu
    }
}

impl MemRead for Apu {
    fn read(&mut self, addr: u16, lane_mask: &mut u8) -> u8 {
        if addr == 0x15 {
            *lane_mask = 0xFF;
            let mut status = ApuStatus::new();
            status.set_frame_int(self.frame_counter.irq_line.clear());

            // TODO: this should be an actual interrupt line
            status.set_dmc_int(self.dmc.interrupt_flag);

            status.set_noise_lc_st(self.noise.length_counter > 0);
            status.set_tri_lc_st(self.triangle.length_counter > 0);
            status.set_p2_lc_st(self.pulse2.length_counter > 0);
            status.set_p1_lc_st(self.pulse1.length_counter > 0);

            status.set_dmc_active(self.dmc.mem_reader.bytes_remaining > 0);

            status.bytes[0]
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
            0x01 => self.pulse1.reload_sweep = true,
            0x03 => {
                self.pulse1.set_lc(length_counter);
            }
            0x05 => self.pulse2.reload_sweep = true,
            0x07 => self.pulse2.set_lc(length_counter),
            0x0B => self.triangle.set_lc(length_counter),
            0x0F => self.noise.set_lc(length_counter),
            0x11 => self.dmc.output_unit.output_level = self.regs.dmc.direct_load(),
            0x12 => {
                let addr = self.regs.dmc.s_addr() as u16 * 64 + 0xC000;
                let addr = 0xC000 + addr * 64;
                self.dmc.mem_reader.start = addr;
                self.dmc.mem_reader.addr = addr;
            }
            0x13 => {
                let len = self.regs.dmc.s_len() as u16 * 16 + 1;
                self.dmc.mem_reader.len = len;
                self.dmc.mem_reader.bytes_remaining = len;
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
                // TODO: wonky *delayed* reset
            }
            _ => (),
        }
    }
}

struct ApuCycleTimer {
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
            pulse1.length_counter = 0;
        }
        if !regs.control.p2_lc_en() {
            pulse2.length_counter = 0;
        }
        if !regs.control.tri_lc_en() {
            triangle.length_counter = 0;
        }
        if !regs.control.noise_lc_en() {
            noise.length_counter = 0;
        }
        if !regs.control.dmc_lc_en() {
            // ???
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

    fn countdown(&self) -> &u64 {
        &self.countdown
    }

    fn countdown_mut(&mut self) -> &mut u64 {
        &mut self.countdown
    }
}

struct ApuFrameTimer {
    apu: R<Apu>,
    countdown: u64,
}

impl Timed for ApuFrameTimer {
    fn fire(&mut self) {
        self.countdown = QUARTER_FRAME;
        self.apu.borrow_mut().do_frame_tick();
    }

    fn countdown(&self) -> &u64 {
        &self.countdown
    }

    fn countdown_mut(&mut self) -> &mut u64 {
        &mut self.countdown
    }
}

struct ApuSampleTimer {
    apu: R<Apu>,
    countdown: u64,
}

impl Timed for ApuSampleTimer {
    fn fire(&mut self) {
        self.countdown = 487;
        self.apu.borrow_mut().mix_sample();
    }

    fn countdown(&self) -> &u64 {
        &self.countdown
    }

    fn countdown_mut(&mut self) -> &mut u64 {
        &mut self.countdown
    }
}
