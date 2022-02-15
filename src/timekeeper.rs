use std::time::{Duration, Instant};

use enum_dispatch::enum_dispatch;

use crate::nes::{
    apu::{ApuCycleTimer, ApuFrameTimer, ApuSampleTimer},
    ppu::Ppu,
};
use crate::reset_manager::{Reset, ResetManager};
use crate::{r, R};

#[enum_dispatch]
pub trait Timed: 'static + Into<TimedImpl> {
    fn fire(&mut self);
    fn countdown(&self) -> u64;
    fn advance_countdown(&mut self, n: u64);
}

#[enum_dispatch(Timed)]
pub enum TimedImpl {
    ApuCycleTimer,
    ApuFrameTimer,
    ApuSampleTimer,
    Ppu(R<Ppu>),
}

pub struct Timekeeper {
    timers: Vec<TimedImpl>,

    t_ref: Instant,
    t_pause: Instant,

    clk_rate: f64,
    pub clk_cyclenum: u64,
}

impl Timekeeper {
    pub fn new(rm: &R<ResetManager>, clk_rate: f64) -> R<Self> {
        let tk = r(Self {
            timers: vec![],

            t_ref: Instant::now(),
            t_pause: Instant::now(),

            clk_rate,
            clk_cyclenum: 0,
        });

        rm.borrow_mut().add_device(&tk);
        tk
    }

    pub fn add_timer(&mut self, timer: impl Timed) {
        self.timers.push(timer.into());
    }

    pub fn advance_clk(&mut self, ncycles: u64) {
        let mut mincount = ncycles;
        for timer in &self.timers {
            mincount = mincount.min(timer.countdown());
        }

        self.clk_cyclenum += mincount;

        for timer in &mut self.timers {
            timer.advance_countdown(mincount);
            if timer.countdown() == 0 {
                timer.fire();
            }
        }

        if mincount != ncycles {
            self.advance_clk(ncycles - mincount);
        }
    }

    pub fn sync(&mut self) {
        let expected_cycles = self.clk_rate * self.t_ref.elapsed().as_secs_f64();
        let excess_cycles = self.clk_cyclenum as f64 - expected_cycles;
        if excess_cycles > 0.0 {
            let excess_time = excess_cycles / self.clk_rate;
            std::thread::sleep(Duration::from_secs_f64(excess_time));
        }
    }

    pub fn pause(&mut self) {
        self.t_pause = Instant::now();
    }

    pub fn resume(&mut self) {
        self.t_ref += Instant::now() - self.t_pause;
    }
}

impl Reset for Timekeeper {
    fn reset(&mut self) {
        self.clk_cyclenum = 0;
        self.t_ref = Instant::now();
    }
}
