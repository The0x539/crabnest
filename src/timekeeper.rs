use std::time::{Duration, Instant};

use crate::reset_manager::{Reset, ResetManager};
use crate::{r, R};

pub trait Timed: 'static {
    fn fire(&mut self);
    fn countdown(&self) -> &u64;
    fn countdown_mut(&mut self) -> &mut u64; // ?
}

pub struct Timekeeper {
    timers: Vec<R<dyn Timed>>,

    t_ref: Instant,
    t_pause: Instant,

    clk_period: Duration,
    pub clk_cyclenum: u64,
}

impl Timekeeper {
    pub fn new(rm: &R<ResetManager>, clk_period: Duration) -> R<Self> {
        let tk = r(Self {
            timers: vec![],

            t_ref: Instant::now(),
            t_pause: Instant::now(),

            clk_period,
            clk_cyclenum: 0,
        });

        rm.borrow_mut().add_device(&tk);
        tk
    }

    pub fn add_timer(&mut self, timer: R<impl Timed>) {
        self.timers.push(timer as _);
    }

    pub fn advance_clk(&mut self, ncycles: u64) {
        let mincount = self
            .timers
            .iter()
            .map(|t| *t.borrow().countdown())
            .min()
            .unwrap_or(ncycles);

        self.clk_cyclenum += mincount;

        for timer in &self.timers {
            let mut timer = timer.borrow_mut();
            *timer.countdown_mut() -= mincount;
            if *timer.countdown() == 0 {
                timer.fire();
            }
        }

        if mincount != ncycles {
            self.advance_clk(ncycles - mincount);
        }
    }

    pub fn sync(&mut self) {
        let cyclenum = u32::try_from(self.clk_cyclenum).expect("too many cycles");

        let t_target = self.t_ref + self.clk_period * cyclenum;
        let t_now = Instant::now();
        if t_now < t_target {
            println!("{}us ahead of schedule", (t_target - t_now).as_micros());
            std::thread::sleep(t_target - t_now);
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
