use super::{Channel, TriangleControl};

#[derive(Default)]
pub struct Triangle {
    sequence_pos: u8,
    timer: u16,
    length_counter: u8,
    linear_counter: u8,
    linear_counter_reload: bool,
}

impl Triangle {
    pub fn active(&self) -> bool {
        self.length_counter > 0
    }

    pub fn deactivate(&mut self) {
        self.length_counter = 0;
    }
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
