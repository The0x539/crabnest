#![allow(dead_code)]

use modular_bitfield::prelude::*;

#[bitfield(bits = 8)]
struct PulseDuty {
    duty: B2,
    lch: bool,
    constant: bool,
    volume: B4,
}

#[bitfield(bits = 8)]
struct Sweep {
    enabled: bool,
    period: B3,
    negative: bool,
    shift_count: B3,
}

#[bitfield(bits = 16)]
struct Timer {
    lo: B8,
    load: B5,
    hi: B3,
}

#[repr(C)]
struct PulseChannel {
    duty: PulseDuty,
    sweep: Sweep,
    timer: Timer,
}

#[bitfield(bits = 8)]
struct TriangleCounter {
    control: bool,
    reload: B7,
}

#[repr(C)]
struct TriangleChannel {
    counter: TriangleCounter,
    timer: Timer,
}

#[bitfield(bits = 24)]
struct NoiseChannel {
    #[skip]
    __: B2,
    lch: bool,
    constant: bool,
    volume: B4,

    mode: bool,
    #[skip]
    __: B3,
    period: B4,

    load: B5,
    #[skip]
    __: B3,
}

#[bitfield(bits = 32)]
struct DmcChannel {
    irq: bool,
    repeat: bool,
    #[skip]
    __: B2,
    freq_idx: B4,

    #[skip]
    __: B1,
    direct_load: B7,

    s_addr: u8,
    s_len: u8,
}

#[bitfield(bits = 8)]
struct ApuControl {
    #[skip]
    __: B3,
    dmc_lc_en: bool,
    noise_lc_en: bool,
    tri_lc_en: bool,
    p2_lc_en: bool,
    p1_lc_en: bool,
}

#[bitfield(bits = 8)]
struct ApuStatus {
    dmc_int: bool,
    frame_int: bool,
    #[skip]
    __: B1,
    dmc_active: bool,
    noise_lc_st: bool,
    tri_lc_st: bool,
    p2_lc_st: bool,
    p1_lc_st: bool,
}

#[bitfield(bits = 8)]
struct FrameCounter {
    sequence: bool,
    disable_int: bool,
    #[skip]
    __: B6,
}

#[repr(C)]
struct ApuState {
    pulse1: PulseChannel,
    pulse2: PulseChannel,
    triangle: TriangleChannel,
    noise: NoiseChannel,
    dmc: DmcChannel,

    _status: u8,
    frame_counter: FrameCounter,
}
