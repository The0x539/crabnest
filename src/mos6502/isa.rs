use super::{Mos6502, StepResult};

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
        macro_rules! val16 {
            () => {
                match mode {
                    AddrMode::Acc => self.a as u16,
                    AddrMode::Imm => imm8 as u16,
                    _ => self.read16(addr),
                }
            };
        }

        use Instr::*;
        match instr {
            VMC => return self.handle_vmcall(imm8),
            _ => todo!(),
        }

        self.advance_clk(INSTR_CYCLES[opcode as usize] as usize);
        StepResult::Success
    }
}
