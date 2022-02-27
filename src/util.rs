use std::{cell::RefCell, rc::Rc};

pub type R<T> = Rc<RefCell<T>>;

pub fn r<T>(val: T) -> R<T> {
    Rc::new(RefCell::new(val))
}
