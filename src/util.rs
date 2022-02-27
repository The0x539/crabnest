use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use bytemuck::Zeroable;

pub type R<T> = Rc<RefCell<T>>;

pub fn r<T>(val: T) -> R<T> {
    Rc::new(RefCell::new(val))
}

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct ArrayVec<T, const N: usize> {
    len: usize,
    data: [T; N], // for my purposes I don't really need uninit data
}

impl<T: Default, const N: usize> Default for ArrayVec<T, N> {
    fn default() -> Self {
        Self {
            len: 0,
            data: [(); N].map(|()| T::default()),
        }
    }
}

// SAFETY: All members of this struct (i.e., usize and [T; N]) implement Zeroable.
// The derive macro would work, but it doesn't emit a `T: Zeroable` bound.
unsafe impl<T: Zeroable, const N: usize> Zeroable for ArrayVec<T, N> {}

impl<T, const N: usize> ArrayVec<T, N> {
    pub fn capacity(&self) -> usize {
        N
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_full(&self) -> bool {
        self.len() == self.capacity()
    }

    pub fn clear(&mut self) {
        self.len = 0;
    }

    pub fn as_slice(&self) -> &[T] {
        &self.data[..self.len]
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.data[..self.len]
    }

    pub fn push(&mut self, value: T) {
        if self.is_full() {
            panic!("Capacity exceeded");
        } else {
            self.data[self.len] = value;
            self.len += 1;
        }
    }
}

impl<T, const N: usize> Deref for ArrayVec<T, N> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T, const N: usize> DerefMut for ArrayVec<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}
