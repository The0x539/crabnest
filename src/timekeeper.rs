use std::time::{Duration, Instant};

use crate::reset_manager::{Reset, ResetManager};
use crate::{r, R};

fn mul_duration(d: Duration, n: u64) -> Duration {
    let mut nanos = d.subsec_nanos() as u64 * n;
    let mut secs = d.as_secs();
    secs += nanos / 1_000_000_000;
    nanos %= 1_000_000_000;
    Duration::new(secs, nanos as u32)
}

pub trait Timed: 'static {
    fn fire(&mut self);
    fn countdown(&self) -> &u64;
    fn countdown_mut(&mut self) -> &mut u64; // ?
}

pub struct Timekeeper {
    timers: Vec<R<dyn Timed>>,

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

    pub fn add_timer(&mut self, timer: R<impl Timed>) {
        self.timers.push(timer as _);
    }

    pub fn advance_clk(&mut self, ncycles: u64) {
        let mincount = self
            .timers
            .iter()
            .map(|t| *t.borrow().countdown())
            .chain(std::iter::once(ncycles))
            .min()
            .unwrap();

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
