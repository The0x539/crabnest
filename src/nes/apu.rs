#![allow(dead_code)]

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;

use crate::membus::{MemRead, MemWrite};

const CLK_DIVISOR: u64 = 24;

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

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct PulseDuty {
    duty: B2,
    lch: bool,
    constant: bool,
    volume: B4,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Sweep {
    enabled: bool,
    period: B3,
    negative: bool,
    shift_count: B3,
}

#[bitfield(bits = 16)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct Timer {
    lo: B8,
    load: B5,
    hi: B3,
}

impl Timer {
    fn set(&mut self, val: u16) {
        assert!(val <= 0b0000_0111_1111_1111);
        self.set_lo(val as u8);
        self.set_hi((val >> 8) as u8);
    }

    fn get(&self) -> u16 {
        self.lo() as u16 | ((self.hi() as u16) << 8)
    }
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
    control: bool,
    reload: B7,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct TriangleControl {
    counter: TriangleCounter,
    _padding: u8,
    timer: Timer,
}

#[bitfield(bits = 24)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct NoiseControl {
    #[skip]
    __: B2,
    lch: bool,
    constant: bool,
    volume: B4,

    mode: bool,
    #[skip]
    __: B3,
    period: B4,

    load: B5,
    #[skip]
    __: B3,
}

#[bitfield(bits = 32)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct DmcControl {
    irq: bool,
    repeat: bool,
    #[skip]
    __: B2,
    freq_idx: B4,

    #[skip]
    __: B1,
    direct_load: B7,

    s_addr: u8,
    s_len: u8,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct ApuControl {
    #[skip]
    __: B3,
    dmc_lc_en: bool,
    noise_lc_en: bool,
    tri_lc_en: bool,
    p2_lc_en: bool,
    p1_lc_en: bool,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct ApuStatus {
    dmc_int: bool,
    frame_int: bool,
    #[skip]
    __: B1,
    dmc_active: bool,
    noise_lc_st: bool,
    tri_lc_st: bool,
    p2_lc_st: bool,
    p1_lc_st: bool,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
struct FrameCounter {
    sequence: bool,
    disable_int: bool,
    #[skip]
    __: B6,
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
    frame_counter: FrameCounter,
}

struct Envelope {
    start_flag: bool,
    divider: u8,
    decay: u8,
}

impl Envelope {
    fn frame(&mut self, loop_flag: bool, period: u8) {
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
    fn frame(&mut self, control: &Self::Control);
    fn tick(&mut self, control: &Self::Control);
    fn sample(&self, control: &Self::Control) -> u8;
}

struct Pulse {
    sequence: [u8; 8],
    sequence_pos: usize,
    timer: u16,
    length_counter: u8,
    is_pulse2: bool,
    envelope: Envelope,
}

impl Pulse {
    fn period(&self, control: &PulseControl, raw_period: u16) -> u16 {
        if control.sweep.enabled() {
            let change_amount = raw_period >> control.sweep.shift_count();
            match (control.sweep.negative(), self.is_pulse2) {
                (true, true) => raw_period - change_amount - 1,
                (true, false) => raw_period - change_amount,
                (false, _) => raw_period + change_amount,
            }
        } else {
            raw_period
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

    fn frame(&mut self, control: &Self::Control) {
        if !control.duty.lch() {
            self.length_counter = self.length_counter.saturating_sub(1);
        }
        self.envelope
            .frame(control.duty.lch(), control.duty.volume());
    }

    fn tick(&mut self, control: &Self::Control) {
        let raw_period = control.timer.get();
        let period = self.period(control, raw_period);

        if self.timer == 0 {
            self.timer = period;
            self.sequence_pos = self.sequence_pos.checked_sub(1).unwrap_or(7);
        }

        self.sequence = PULSE_SEQUENCES[control.duty.duty() as usize];
    }

    fn sample(&self, control: &Self::Control) -> u8 {
        if self.length_counter == 0 {
            return 0;
        }

        let raw_period = control.timer.get();
        if raw_period < 8 {
            return 0;
        }
        let period = self.period(control, raw_period);
        if period < 8 || period > 0x07FF {
            return 0;
        }

        self.envelope.output(
            self.sequence[self.sequence_pos] != 0,
            control.duty.constant(),
            control.duty.volume(),
        )
    }
}

// this needs to tick at the rate of the CPU clock
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

    fn frame(&mut self, control: &Self::Control) {
        if self.linear_counter_reload {
            self.linear_counter = control.counter.reload();
        } else {
            self.linear_counter = self.linear_counter.saturating_sub(1);
        }

        if !control.counter.control() {
            self.linear_counter_reload = false;
            self.length_counter = self.length_counter.saturating_sub(1);
        }
    }

    fn tick(&mut self, control: &Self::Control) {
        if self.timer == 0 {
            self.timer = control.timer.get() + 1;
            self.sequence_pos = (self.sequence_pos + 1) % 32;
        } else {
            self.timer -= 1;
        }
    }

    fn sample(&self, _control: &Self::Control) -> u8 {
        if self.length_counter == 0 || self.linear_counter == 0 {
            0
        } else {
            match self.sequence_pos {
                n @ 0..=15 => 15 - n,
                n @ 16..=31 => n - 16,
                32.. => unreachable!(),
            }
        }
    }
}

struct Noise {
    envelope: Envelope,
    length_counter: u8,
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

    fn frame(&mut self, control: &Self::Control) {
        self.envelope.frame(control.lch(), control.volume());
    }

    fn tick(&mut self, control: &Self::Control) {
        let feedback = self.sr_bit(0) ^ self.sr_bit(if control.mode() { 6 } else { 1 });
        self.shift_reg = (self.shift_reg >> 1) | (feedback << 14);
    }

    fn sample(&self, control: &Self::Control) -> u8 {
        if self.length_counter == 0 {
            return 0;
        }
        self.envelope
            .output(self.sr_bit(0) != 0, control.lch(), control.volume())
    }
}

struct Apu {
    regs: ApuRegs,
    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
}

impl MemRead for Apu {
    fn read(&mut self, addr: u16, lane_mask: &mut u8) -> u8 {
        if addr == 0x15 {
            *lane_mask = 0xFF;
            let mut status = ApuStatus::new();
            // TODO: implement DMC and set this up accordingly
            status.set_dmc_int(false);
            // What is "the frame interrupt flag" and where do I clear it?
            status.set_frame_int(false);

            status.set_dmc_active(false);

            status.set_noise_lc_st(self.noise.length_counter > 0);
            status.set_tri_lc_st(self.triangle.length_counter > 0);
            status.set_p2_lc_st(self.pulse2.length_counter > 0);
            status.set_p1_lc_st(self.pulse1.length_counter > 0);

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
            0x03 => self.pulse1.set_lc(length_counter),
            0x07 => self.pulse2.set_lc(length_counter),
            0x0B => self.triangle.set_lc(length_counter),
            0x0F => self.noise.set_lc(length_counter),
            _ => (),
        }
    }
}
