use super::{regs::NoiseControl, Channel, Envelope};

const NOISE_PERIODS: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

#[derive(Default)]
pub struct Noise {
    envelope: Envelope,
    length_counter: u8,
    timer: u16,
    shift_reg: u16,
}

impl Noise {
    pub fn new() -> Self {
        Self {
            envelope: Envelope::default(),
            length_counter: 0,
            timer: 0,
            shift_reg: 1,
        }
    }

    // TODO: move these into the trait
    pub fn active(&self) -> bool {
        self.length_counter > 0
    }

    pub fn deactivate(&mut self) {
        self.length_counter = 0;
    }

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

    fn half_frame(&mut self, control: &mut NoiseControl) {
        if !control.lch() {
            self.length_counter = self.length_counter.saturating_sub(1);
        }
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
