use crate::reset_manager::{Reset, ResetManager};
use crate::{r, R};

pub trait Timed: 'static {
    fn fire(&mut self);
    fn countdown(&self) -> &u64;
    fn countdown_mut(&mut self) -> &mut u64; // ?
}

pub struct Timekeeper {
    timers: Vec<R<dyn Timed>>,

    t_ref: u32,
    t_pause: u32,

    clk_period: f64,
    clk_cyclenum: u64,
}

impl Timekeeper {
    pub fn new(rm: &R<ResetManager>, clk_period: f64) -> R<Self> {
        let tk = r(Self {
            timers: vec![],

            t_ref: 0,
            t_pause: 0,

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
        // TODO
    }

    pub fn pause(&mut self) {
        // TODO
    }

    pub fn resume(&mut self) {
        // TODO
    }
}

impl Reset for Timekeeper {
    fn reset(&mut self) {
        self.clk_cyclenum = 0;
        // self.t_ref = sdl::get_ticks();
    }
}
