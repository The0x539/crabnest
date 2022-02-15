#![allow(dead_code)]

use bytemuck::{Pod, Zeroable};
use modular_bitfield::prelude::*;

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct PulseDuty {
    pub volume: B4,
    pub constant: bool,
    pub lch: bool,
    pub duty: B2,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Sweep {
    pub shift_count: B3,
    pub negative: bool,
    pub period: B3,
    pub enabled: bool,
}

#[bitfield(bits = 16)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct Timer {
    pub timer: B11,
    pub load: B5,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct PulseControl {
    pub duty: PulseDuty,
    pub sweep: Sweep,
    pub timer: Timer,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct TriangleCounter {
    pub reload: B7,
    pub control: bool,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct TriangleControl {
    pub counter: TriangleCounter,
    _padding: u8,
    pub timer: Timer,
}

#[bitfield(bits = 32)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct NoiseControl {
    pub volume: B4,
    pub constant: bool,
    pub lch: bool,
    #[skip]
    __: B2,

    #[skip]
    __: B8,

    pub period: B4,
    #[skip]
    __: B3,
    pub mode: bool,

    #[skip]
    __: B3,
    pub load: B5,
}

#[bitfield(bits = 32)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct DmcControl {
    pub freq_idx: B4,
    #[skip]
    __: B2,
    pub repeat: bool,
    pub irq: bool,

    pub direct_load: B7,
    #[skip]
    __: B1,

    pub s_addr: u8,
    pub s_len: u8,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct ApuControl {
    pub p1_lc_en: bool,
    pub p2_lc_en: bool,
    pub tri_lc_en: bool,
    pub noise_lc_en: bool,
    pub dmc_lc_en: bool,
    #[skip]
    __: B3,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct ApuStatus {
    pub p1_lc_st: bool,
    pub p2_lc_st: bool,
    pub tri_lc_st: bool,
    pub noise_lc_st: bool,
    pub dmc_active: bool,
    #[skip]
    __: B1,
    pub frame_int: bool,
    pub dmc_int: bool,
}

#[bitfield(bits = 8)]
#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct FrameCounterControl {
    #[skip]
    __: B6,
    pub disable_int: bool,
    pub sequence: bool,
}

#[derive(Debug, Copy, Clone, Zeroable, Pod)]
#[repr(C)]
pub struct ApuRegs {
    pub pulse1: PulseControl,
    pub pulse2: PulseControl,
    pub triangle: TriangleControl,
    pub noise: NoiseControl,
    pub dmc: DmcControl,
    _padding1: u8,
    pub control: ApuControl,
    _padding2: u8,
    pub frame_counter: FrameCounterControl,
}

static_assertions::assert_eq_size!(ApuRegs, [u8; 0x18]);
