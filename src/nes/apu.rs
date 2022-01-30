#![allow(dead_code)]

use modular_bitfield::prelude::*;

const CLK_DIVISOR: u64 = 24;

#[bitfield(bits = 8)]
struct PulseDuty {
    duty: B2,
    lch: bool,
    constant: bool,
    volume: B4,
}

#[bitfield(bits = 8)]
struct Sweep {
    enabled: bool,
    period: B3,
    negative: bool,
    shift_count: B3,
}

#[bitfield(bits = 16)]
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

#[repr(C)]
struct PulseControl {
    duty: PulseDuty,
    sweep: Sweep,
    timer: Timer,
}

#[bitfield(bits = 8)]
struct TriangleCounter {
    control: bool,
    reload: B7,
}

#[repr(C)]
struct TriangleControl {
    counter: TriangleCounter,
    timer: Timer,
}

#[bitfield(bits = 24)]
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
struct FrameCounter {
    sequence: bool,
    disable_int: bool,
    #[skip]
    __: B6,
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

        self.sequence = match control.duty.duty() {
            0 => [0, 0, 0, 0, 0, 0, 0, 1],
            1 => [0, 0, 0, 0, 0, 0, 1, 1],
            2 => [0, 0, 0, 0, 1, 1, 1, 1],
            3 => [1, 1, 1, 1, 1, 1, 0, 0],
            _ => unreachable!(),
        };
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
            self.timer = control.timer.get();
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
