#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Edge {
    Flat,
    Rising,
    Falling,
}

#[derive(Default)]
pub struct A12Watcher {
    last_cycle: usize,
    cycles_down: usize,
}

const MIN_DELAY: u8 = 10;

impl A12Watcher {
    pub fn update_vram_addr(&mut self, addr: u16, frame_cycle: usize) -> Edge {
        let mut result = Edge::Flat;

        if self.cycles_down > 0 {
            if self.last_cycle > frame_cycle {
                self.cycles_down += (89342 - self.last_cycle) + frame_cycle;
            } else {
                self.cycles_down += frame_cycle - self.last_cycle;
            }
        }

        if addr & 0x1000 == 0 {
            if self.cycles_down == 0 {
                self.cycles_down = 1;
                result = Edge::Falling;
            }
        } else {
            if self.cycles_down > MIN_DELAY as usize {
                result = Edge::Rising;
            }
            self.cycles_down = 0;
        }
        self.last_cycle = frame_cycle;

        result
    }
}
