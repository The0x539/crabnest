use super::{regs::PulseControl, Channel, Envelope};

const PULSE_SEQUENCES: [[u8; 8]; 4] = [
    [0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 1, 1],
    [0, 0, 0, 0, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 0, 0],
];

#[derive(Default)]
pub struct Pulse {
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
    pub fn new(is_pulse2: bool) -> Self {
        Self {
            sequence: [0; 8],
            sequence_pos: 0,
            timer: 0,
            length_counter: 0,
            sweep_counter: 0,
            reload_sweep: false,
            is_pulse2,
            envelope: Envelope::default(),
        }
    }

    pub fn reload_sweep(&mut self) {
        self.reload_sweep = true;
    }

    pub fn active(&self) -> bool {
        self.length_counter > 0
    }

    pub fn deactivate(&mut self) {
        self.length_counter = 0;
    }

    fn target_period(&self, control: &PulseControl) -> u16 {
        let raw_period = control.timer.timer();
        let change_amount = raw_period >> control.sweep.shift_count();
        match (control.sweep.negative(), self.is_pulse2) {
            (true, false) => raw_period - change_amount - 1,
            (true, true) => raw_period - change_amount,
            (false, _) => raw_period + change_amount,
        }
    }

    fn muted(&self, control: &PulseControl) -> bool {
        control.timer.timer() < 8 || self.target_period(control) > 0x7FF
    }

    fn update_period(&mut self, control: &mut PulseControl) {
        let period = self.target_period(control);

        if self.sweep_counter == 0 || self.reload_sweep {
            self.reload_sweep = false;
            self.sweep_counter = control.sweep.period() + 1;
        } else {
            self.sweep_counter -= 1;
        }

        if self.sweep_counter == 0
            && control.sweep.enabled()
            && !self.muted(control)
            && control.sweep.shift_count() != 0
        {
            control.timer.set_timer(period);
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
        if self.timer == 0 {
            self.timer = control.timer.timer();
            self.sequence_pos = self.sequence_pos.checked_sub(1).unwrap_or(7);
        } else {
            self.timer -= 1;
        }

        self.sequence = PULSE_SEQUENCES[control.duty.duty() as usize];
    }

    fn sample(&self, control: &Self::Control) -> u8 {
        if self.length_counter == 0 || self.muted(control) {
            return 0;
        }

        self.envelope.output(
            self.sequence[self.sequence_pos] != 0,
            control.duty.constant(),
            control.duty.volume(),
        )
    }
}
