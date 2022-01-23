use crate::{r, R};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub trait Reset: 'static {
    fn reset(&mut self);
}

pub struct ResetManager {
    devices: Vec<Weak<RefCell<dyn Reset>>>,
}

impl ResetManager {
    pub fn new() -> R<Self> {
        r(Self { devices: vec![] })
    }

    pub fn add_device(&mut self, dev: &R<impl Reset>) {
        let weak = Rc::downgrade(dev);
        self.devices.push(weak as _);
    }

    pub fn issue_reset(&self) {
        for dev in &self.devices {
            if let Some(dev) = dev.upgrade() {
                dev.borrow_mut().reset()
            }
        }
    }
}
