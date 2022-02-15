use crate::{
    membus::MemBus,
    mos6502::{IrqLine, Mos6502},
    R,
};

const DMC_RATES: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
];

use super::{regs::DmcControl, Channel};

struct MemReader {
    mem: R<MemBus>,
    start: u16,
    addr: u16,
    len: u16,
    bytes_remaining: u16,
    irq_line: IrqLine,
}

impl MemReader {
    fn next(&mut self, control: &DmcControl) -> Option<u8> {
        if self.bytes_remaining > 0 {
            // TODO: "stall the CPU"
            let val = self.mem.borrow().read(self.addr);
            self.addr = self.addr.checked_add(1).unwrap_or(0x8000);

            self.bytes_remaining -= 1;
            if self.bytes_remaining == 0 {
                if control.repeat() {
                    self.addr = self.start;
                    self.bytes_remaining = self.len;
                } else if control.irq() {
                    self.irq_line.raise();
                }
            }

            Some(val)
        } else {
            None
        }
    }
}

#[derive(Default)]
struct OutputUnit {
    rsr: u8,
    bits_remaining: u8,
    output_level: u8,
    silence: bool,
}

pub(super) struct Dmc {
    mem_reader: MemReader,
    sample_buffer: Option<u8>,
    timer: u8,
    output_unit: OutputUnit,
}

impl Dmc {
    pub fn new(cpu: &Mos6502) -> Self {
        let mem = cpu.bus.clone();
        let irq_line = cpu.get_irq_line();
        Self {
            mem_reader: MemReader {
                mem,
                start: 0,
                addr: 0,
                len: 0,
                bytes_remaining: 0,
                irq_line,
            },
            sample_buffer: None,
            timer: 0,
            output_unit: OutputUnit::default(),
        }
    }

    pub fn set_output_level(&mut self, val: u8) {
        self.output_unit.output_level = val;
    }

    pub fn set_sample_start(&mut self, addr: u16) {
        self.mem_reader.start = addr;
        self.mem_reader.addr = addr;
    }

    pub fn set_sample_length(&mut self, length: u16) {
        self.mem_reader.len = length;
        self.mem_reader.bytes_remaining = length;
    }

    pub fn clear_irq(&mut self) {
        self.mem_reader.irq_line.clear();
    }

    pub fn restart(&mut self) {
        if self.mem_reader.bytes_remaining == 0 {
            self.mem_reader.addr = self.mem_reader.start;
            self.mem_reader.bytes_remaining = self.mem_reader.len;
        }
    }

    pub fn irq_active(&self) -> bool {
        self.mem_reader.irq_line.active()
    }

    pub fn has_more_bytes(&self) -> bool {
        self.mem_reader.bytes_remaining > 0
    }

    fn empty_sample_buffer(&mut self, control: &DmcControl) -> Option<u8> {
        let val = self.sample_buffer;
        self.sample_buffer = self.mem_reader.next(control);
        val
    }
}

impl Channel for Dmc {
    type Control = DmcControl;

    fn set_lc(&mut self, _val: u8) {}

    fn quarter_frame(&mut self, _control: &Self::Control) {}

    fn half_frame(&mut self, _control: &mut Self::Control) {}

    fn tick(&mut self, control: &Self::Control) {
        if self.timer == 0 {
            self.timer = (DMC_RATES[control.freq_idx() as usize] / 2) as u8;

            if !self.output_unit.silence {
                let v = &mut self.output_unit.output_level;
                if self.output_unit.rsr & 1 != 0 {
                    if *v <= 125 {
                        *v += 2;
                    }
                } else {
                    if *v >= 2 {
                        *v -= 2;
                    }
                }
            }

            self.output_unit.rsr >>= 1;

            if self.output_unit.bits_remaining == 0 {
                self.output_unit.bits_remaining = 8;
                if let Some(v) = self.empty_sample_buffer(control) {
                    self.output_unit.rsr = v;
                    self.output_unit.silence = false;
                } else {
                    self.output_unit.silence = true
                }
            } else {
                self.output_unit.bits_remaining -= 1;
            }
        } else {
            self.timer -= 1;
        }
    }

    fn sample(&self, _control: &Self::Control) -> u8 {
        self.output_unit.output_level
    }
}
