use super::{Mos6502, StatReg, StepResult};

#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) enum AddrMode {
    None,
    Abs,
    AbsX,
    AbsY,
    Acc,
    Imm,
    Impl,
    IdxInd,
    Ind,
    IndIdx,
    Rel,
    ZeroP,
    ZeroPX,
    ZeroPY,
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[rustfmt::skip]
pub(super) enum Instr {
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI,
    BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI,
    CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR,
    INC, INX, INY, JMP, JSR, LDA, LDX, LDY,
    LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL,
    ROR, RTI, RTS, SBC, SEC, SED, SEI, STA,
    STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,

    VMC, IUU, 
}

#[rustfmt::skip]
const INSTR_CYCLES: [u8; 256] = [
    7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
];

#[rustfmt::skip]
const INSTRS: [Instr; 256] = {
    use Instr::IUU as ___;
    use Instr::*;
    [
        //0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
        BRK, ORA, ___, ___, ___, ORA, ASL, ___, PHP, ORA, ASL, ___, ___, ORA, ASL, ___, // 0
        BPL, ORA, ___, ___, ___, ORA, ASL, ___, CLC, ORA, ___, ___, ___, ORA, ASL, ___, // 1
        JSR, AND, ___, ___, BIT, AND, ROL, ___, PLP, AND, ROL, ___, BIT, AND, ROL, ___, // 2
        BMI, AND, ___, ___, ___, AND, ROL, ___, SEC, AND, ___, ___, ___, AND, ROL, ___, // 3
        RTI, EOR, ___, ___, ___, EOR, LSR, ___, PHA, EOR, LSR, ___, JMP, EOR, LSR, ___, // 4
        BVC, EOR, ___, ___, ___, EOR, LSR, ___, CLI, EOR, ___, ___, ___, EOR, LSR, ___, // 5
        RTS, ADC, ___, ___, ___, ADC, ROR, ___, PLA, ADC, ROR, ___, JMP, ADC, ROR, ___, // 6
        BVS, ADC, ___, ___, ___, ADC, ROR, ___, SEI, ADC, ___, ___, ___, ADC, ROR, ___, // 7
        VMC, STA, ___, ___, STY, STA, STX, ___, DEY, STA, TXA, ___, STY, STA, STX, ___, // 8
        BCC, STA, ___, ___, STY, STA, STX, ___, TYA, STA, TXS, ___, ___, STA, STX, ___, // 9
        LDY, LDA, LDX, ___, LDY, LDA, LDX, ___, TAY, LDA, TAX, ___, LDY, LDA, LDX, ___, // A
        BCS, LDA, ___, ___, LDY, LDA, LDX, ___, CLV, LDA, TSX, ___, LDY, LDA, LDX, ___, // B
        CPY, CMP, ___, ___, CPY, CMP, DEC, ___, INY, CMP, DEX, ___, CPY, CMP, DEC, ___, // C
        BNE, CMP, ___, ___, ___, CMP, DEC, ___, CLD, CMP, ___, ___, ___, CMP, DEC, ___, // D
        CPX, SBC, ___, ___, CPX, SBC, INC, ___, INX, SBC, NOP, ___, CPX, SBC, INC, ___, // E
        BEQ, SBC, ___, ___, ___, SBC, INC, ___, SED, SBC, ___, ___, ___, SBC, INC, ___, // F
    ]
};

#[rustfmt::skip]
const ADDR_MODES: [AddrMode; 256] = {
    use AddrMode::{
        Abs as ABS, AbsX as ABX, AbsY as ABY, Acc as ACC, IdxInd as XIN, Imm as IMM, Impl as IMP,
        Ind as IND, IndIdx as YIN, None as ___, Rel as REL, ZeroP as ZPG, ZeroPX as ZPX,
        ZeroPY as ZPY,
    };
    [
        //0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
        IMP, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, ACC, ___, ___, ABS, ABS, ___, // 0
        REL, YIN, ___, ___, ___, ZPX, ZPX, ___, IMP, ABY, ___, ___, ___, ABX, ABX, ___, // 1
        ABS, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, ACC, ___, ABS, ABS, ABS, ___, // 2
        REL, YIN, ___, ___, ___, ZPX, ZPX, ___, IMP, ABY, ___, ___, ___, ABX, ABX, ___, // 3
        IMP, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, ACC, ___, ABS, ABS, ABS, ___, // 4
        REL, YIN, ___, ___, ___, ZPX, ZPX, ___, IMP, ABY, ___, ___, ___, ABX, ABX, ___, // 5
        IMP, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, ACC, ___, IND, ABS, ABS, ___, // 6
        REL, YIN, ___, ___, ___, ZPX, ZPX, ___, IMP, ABY, ___, ___, ___, ABX, ABX, ___, // 7
        IMM, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, ___, IMP, ___, ABS, ABS, ABS, ___, // 8
        REL, YIN, ___, ___, ZPX, ZPX, ZPY, ___, IMP, ABY, IMP, ___, ___, ABX, ABX, ___, // 9
        IMM, XIN, IMM, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, IMP, ___, ABS, ABS, ABS, ___, // A
        REL, YIN, ___, ___, ZPX, ZPX, ZPY, ___, IMP, ABY, IMP, ___, ABX, ABX, ABY, ___, // B
        IMM, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, IMP, ___, ABS, ABS, ABS, ___, // C
        REL, YIN, ___, ___, ___, ZPX, ZPX, ___, IMP, ABY, ___, ___, ___, ABX, ABX, ___, // D
        IMM, XIN, ___, ___, ZPG, ZPG, ZPG, ___, IMP, IMM, IMP, ___, ABS, ABS, ABS, ___, // E
        REL, YIN, ___, ___, ___, ZPX, ZPX, ___, IMP, ABY, ___, ___, ___, ABX, ABX, ___, // F
    ]
};

impl Mos6502 {
    pub fn instr_repr(&self, addr: u16) -> String {
        let opcode = self.read8(addr);
        let instr = INSTRS[opcode as usize];
        let mode = ADDR_MODES[opcode as usize];

        if instr == Instr::IUU {
            return format!("ILL ({opcode:02X})");
        }

        let op8 = || self.read8(addr + 1);
        let op16 = || self.read16(addr + 1);

        match mode {
            AddrMode::None => format!("{:?} ?", instr),
            AddrMode::Abs => format!("{:?} ${:04X}", instr, op16()),
            AddrMode::AbsX => format!("{:?} ${:04X},X", instr, op16()),
            AddrMode::AbsY => format!("{:?} ${:04X},Y", instr, op16()),
            AddrMode::Acc => format!("{:?} A", instr),
            AddrMode::Imm => format!("{:?} #{:02X}", instr, op8()),
            AddrMode::Impl => format!("{:?}", instr),
            AddrMode::IdxInd => format!("{:?} (${:02X},X)", instr, op8()),
            AddrMode::Ind => format!("{:?} (${:02X})", instr, op16()),
            AddrMode::IndIdx => format!("{:?} (${:02X}),Y", instr, op8()),
            AddrMode::Rel => format!("{:?} ${:02X}", instr, op8() as i8),
            AddrMode::ZeroP => format!("{:?} ${:02X}", instr, op8()),
            AddrMode::ZeroPX => format!("{:?} ${:02X},X", instr, op8()),
            AddrMode::ZeroPY => format!("{:?} ${:02X},Y", instr, op8()),
        }
    }

    pub fn step(&mut self) -> StepResult {
        let oldpc = self.pc;
        let opcode = self.read8pc();
        let mode = ADDR_MODES[opcode as usize];
        let instr = INSTRS[opcode as usize];

        let mut addr = 0;
        let mut imm8 = 0;

        match mode {
            AddrMode::Abs => addr = self.read16pc(),
            AddrMode::AbsX => addr = self.read16pc() + self.x as u16,
            AddrMode::AbsY => addr = self.read16pc() + self.y as u16,
            AddrMode::Acc | AddrMode::Impl => (),
            AddrMode::Imm => imm8 = self.read8pc(),
            AddrMode::Ind => {
                let ind_addr = self.read16pc();
                addr = self.buggy_read16(ind_addr);
            }
            AddrMode::IdxInd => {
                let ind_addr = self.read8pc() as u16 + self.x as u16;
                addr = self.read16(ind_addr);
            }
            AddrMode::IndIdx => {
                let ind_addr = self.read8pc() as u16;
                addr = self.read16(ind_addr) + self.y as u16;
            }
            AddrMode::Rel => addr = (self.pc as i16 + self.read8pc() as i8 as i16) as u16,
            AddrMode::ZeroP => addr = self.read8pc() as u16,
            AddrMode::ZeroPX => addr = (self.read8pc() + self.x) as u16,
            AddrMode::ZeroPY => addr = (self.read8pc() + self.y) as u16,

            AddrMode::None => return StepResult::IllegalInstruction,
        }

        macro_rules! val8 {
            () => {
                match mode {
                    AddrMode::Acc => self.a,
                    AddrMode::Imm => imm8,
                    _ => self.read8(addr),
                }
            };
        }
        macro_rules! set8 {
            ($val:ident) => {
                match mode {
                    AddrMode::Acc => {
                        self.a = $val;
                        self.setp($val);
                    }
                    _ => {
                        self.write8(addr, $val);
                        self.setp($val);
                    }
                }
            };
        }
        macro_rules! transfer {
            ($src:ident -> $dst:ident) => {{
                self.$dst = self.$src;
                self.setp(self.$dst);
            }};
        }
        macro_rules! compare {
            ($reg:ident) => {{
                let val = val8!();
                self.setc(self.$reg >= val);
                self.setp(self.$reg - val);
            }};
        }
        macro_rules! branch {
            ($flag:ident, $expected:literal) => {{
                if self.p.contains(StatReg::$flag) == $expected {
                    self.pc = addr + 1;
                }
            }};
        }

        use Instr::*;
        match instr {
            VMC => return self.handle_vmcall(imm8),

            BRK => {
                self.push16(self.pc + 1);
                self.push8((self.p | StatReg::B).bits);
                self.pc = self.read16(0xFFFE);
            }

            RTI => {
                self.p.bits = self.pop8();
                self.pc = self.pop16() - 1;
            }

            NOP => (),

            // loads
            LDA => {
                self.a = val8!();
                self.setp(self.a);
            }
            LDX => {
                self.x = val8!();
                self.setp(self.x);
            }
            LDY => {
                self.y = val8!();
                self.setp(self.y);
            }

            // stores
            STA => self.write8(addr, self.a),
            STX => self.write8(addr, self.x),
            STY => self.write8(addr, self.y),

            // transfers
            TAX => transfer!(a -> x),
            TXA => transfer!(x -> a),
            TAY => transfer!(a -> y),
            TYA => transfer!(y -> a),
            TSX => transfer!(sp -> x),
            TXS => transfer!(x -> sp),

            // arithmetic
            ADC => {
                let addend = val8!();
                let sum = self.a as u16 + addend as u16 + self.getc() as u16;
                self.setc(sum > 0xFF);
                self.p.set(StatReg::V, sum > 0x7F && addend <= 0x7F); // ???
                self.a = sum as u8;
                self.setp(self.a);
            }
            SBC => {
                let a = self.a as i16;
                let b = val8!() as i16;
                let c = 1 - self.getc() as i16;
                let diff = a - b - c;
                self.setc(diff >= 0);
                self.a = diff as u8;
                self.setp(self.a);
            }

            // bitwise ops
            AND => {
                self.a &= val8!();
                self.setp(self.a);
            }
            ORA => {
                self.a |= val8!();
                self.setp(self.a);
            }
            EOR => {
                self.a ^= val8!();
                self.setp(self.a);
            }

            // bitshift ops
            ASL => {
                let mut val = val8!();
                self.setc(val & 0x80 != 0);
                val <<= 1;
                set8!(val);
            }
            LSR => {
                let mut val = val8!();
                self.setc(val & 0x01 != 0);
                val >>= 1;
                set8!(val);
            }
            ROL => {
                let mut val = val8!();
                let carry = self.getc();
                self.setc(val & 0x80 != 0);
                nval = (val << 1) | carry;
                set8!(val);
            }
            ROR => {
                let mut val = val8!();
                let carry = self.getc();
                self.setc(val & 0x01 != 0);
                val = (val >> 1) | (carry << 7);
                set8!(val);
            }

            // memory inc/dec
            INC => {
                let v = val8!() + 1;
                set8!(v);
            }
            DEC => {
                let v = val8!() - 1;
                set8!(v);
            }

            // register inc/dec
            INX => {
                self.x += 1;
                self.setp(self.x);
            }
            DEX => {
                self.x -= 1;
                self.setp(self.x);
            }
            INY => {
                self.y += 1;
                self.setp(self.y);
            }
            DEY => {
                self.y -= 1;
                self.setp(self.y);
            }

            // flag setters
            SEC => self.p.insert(StatReg::C),
            CLC => self.p.remove(StatReg::C),
            SED => self.p.insert(StatReg::D),
            CLD => self.p.remove(StatReg::D),

            // stack ops
            PHA => self.push8(self.a),
            PLA => {
                self.a = self.pop8();
                self.setp(self.a);
            }
            PHP => self.push8((self.p | StatReg::B).bits),
            PLP => {
                self.p.bits = self.pop8();
                self.p.remove(StatReg::B);
            }

            // jumps
            JMP => self.pc = addr,
            JSR => {
                self.push16(self.pc - 1);
                self.pc = addr;
            }
            RTS => {
                self.pc = self.pop16() + 1;
            }

            // comparison
            CMP => compare!(a),
            CPX => compare!(x),
            CPY => compare!(y),

            // branches
            BCC => branch!(C, false),
            BCS => branch!(C, true),
            BNE => branch!(Z, false),
            BEQ => branch!(Z, true),
            BPL => branch!(N, false),
            BMI => branch!(N, true),
            BVC => branch!(V, false),
            BVS => branch!(V, true),

            BIT => {
                let val = val8!();
                self.p.set(StatReg::N, val & 0x80 != 0);
                self.p.set(StatReg::V, val & 0x40 != 0);
                self.p.set(StatReg::Z, val == self.a);
            }

            _ => eprintln!("{} NYI ({:02X})", self.instr_repr(oldpc), opcode),
        }

        self.advance_clk(INSTR_CYCLES[opcode as usize] as usize);
        StepResult::Success
    }

    fn setp(&mut self, val: u8) {
        self.p.set(StatReg::Z, val == 0);
        self.p.set(StatReg::N, (val as i8) < 0);
    }

    fn setc(&mut self, val: bool) {
        self.p.set(StatReg::C, val);
    }

    fn getc(&self) -> u8 {
        self.p.contains(StatReg::C) as u8
    }
}
