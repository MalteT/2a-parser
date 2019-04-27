use pad::PadStr;
use colored::Colorize;

use std::fmt;

use super::*;

const COMMENT_WIDTH: usize = 30;
const INST_WIDTH: usize = 4;

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Byte::Constant(nr) => write!(f, "{:>02X}", nr),
            Byte::Label(label) => write!(f, "{}", label),
        }
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Word::Constant(nr) => write!(f, "{:>04X}", nr),
            Word::Label(label) => write!(f, "{}", label),
        }
        .into()
    }
}

impl fmt::Display for Stacksize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stacksize::_16 => "16",
                Stacksize::_32 => "32",
                Stacksize::_48 => "48",
                Stacksize::_64 => "64",
                Stacksize::NotSet => "NOSET",
            }
        )
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::R0 => write!(f, "R0"),
            Register::R1 => write!(f, "R1"),
            Register::R2 => write!(f, "R2"),
            Register::R3 => write!(f, "R3"),
        }
    }
}

impl fmt::Display for MemAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemAddress::Label(label) => write!(f, "({})", label),
            MemAddress::Byte(byte) => write!(f, "({})", byte),
            MemAddress::Register(reg) => write!(f, "({})", reg),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::AsmOrigin(byte) => write!(f, ".ORG {}", byte),
            Instruction::AsmByte(byte) => write!(f, ".BYTE {}", byte),
            Instruction::AsmDefineBytes(bytes) => {
                write!(f, ".DB ")?;
                let last = bytes.last();
                for byte in &bytes[..bytes.len() - 1] {
                    write!(f, "{}, ", byte)?;
                }
                write!(f, "{}", last.expect("No bytes to define"))
            }
            Instruction::AsmDefineWords(words) => {
                write!(f, ".DW ")?;
                let last = words.last();
                for word in &words[..words.len() - 1] {
                    write!(f, "{}, ", word)?;
                }
                write!(f, "{}", last.expect("No words to define"))
            }
            Instruction::AsmEquals(label, byte) => write!(f, ".EQU {} {}", label, byte),
            Instruction::AsmStacksize(size) => write!(f, "*STACKSIZE {}", size),
            Instruction::Clr(reg) => write!(f, "CLR {}", reg),
            Instruction::Add(reg1, reg2) => write!(f, "ADD {}, {}", reg1, reg2),
            Instruction::Adc(reg1, reg2) => write!(f, "ADC {}, {}", reg1, reg2),
            Instruction::Sub(reg1, reg2) => write!(f, "SUB {}, {}", reg1, reg2),
            Instruction::Mul(reg1, reg2) => write!(f, "MUL {}, {}", reg1, reg2),
            Instruction::Div(reg1, reg2) => write!(f, "DIV {}, {}", reg1, reg2),
            Instruction::Inc(reg) => write!(f, "INC {}", reg),
            Instruction::Dec(dst) => write!(f, "DEC {}", dst),
            Instruction::Neg(reg) => write!(f, "NEG {}", reg),
            Instruction::And(reg1, reg2) => write!(f, "AND {}, {}", reg1, reg2),
            Instruction::Or(reg1, reg2) => write!(f, "OR {}, {}", reg1, reg2),
            Instruction::Xor(reg1, reg2) => write!(f, "XOR {}, {}", reg1, reg2),
            Instruction::Com(reg) => write!(f, "COM {}", reg),
            Instruction::Bits(dst, src) => write!(f, "BITS {}, {}", dst, src),
            Instruction::Bitc(dst, src) => write!(f, "BITC {}, {}", dst, src),
            Instruction::Tst(reg) => write!(f, "TST {}", reg),
            Instruction::Cmp(dst, src) => write!(f, "CMP {}, {}", dst, src),
            Instruction::Bitt(dst, src) => write!(f, "BITT {}, {}", dst, src),
            Instruction::Lsr(reg) => write!(f, "LSR {}", reg),
            Instruction::Asr(reg) => write!(f, "ASR {}", reg),
            Instruction::Lsl(reg) => write!(f, "LSL {}", reg),
            Instruction::Rrc(reg) => write!(f, "RRC {}", reg),
            Instruction::Rlc(reg) => write!(f, "RLC {}", reg),
            Instruction::Mov(dst, src) => write!(f, "MOV {}, {}", dst, src),
            Instruction::LdByte(reg, byte) => write!(f, "LD {}, {}", reg, byte),
            Instruction::LdMemAddress(reg, mem) => write!(f, "LD {}, {}", reg, mem),
            Instruction::St(mem, reg) => write!(f, "ST {}, {}", mem, reg),
            Instruction::Push(reg) => write!(f, "PUSH {}", reg),
            Instruction::Pop(reg) => write!(f, "POP {}", reg),
            Instruction::PushF => write!(f, "PUSHF"),
            Instruction::PopF => write!(f, "POPF"),
            Instruction::Ldsp(src) => write!(f, "LDSP {}", src),
            Instruction::Ldfr(src) => write!(f, "LDFR {}", src),
            Instruction::Jmp(mem) => write!(f, "JMP {}", mem),
            Instruction::Jcs(label) => write!(f, "JCS {}", label),
            Instruction::Jcc(label) => write!(f, "JCC {}", label),
            Instruction::Jzs(label) => write!(f, "JZS {}", label),
            Instruction::Jzc(label) => write!(f, "JZC {}", label),
            Instruction::Jns(label) => write!(f, "JNS {}", label),
            Instruction::Jnc(label) => write!(f, "JNC {}", label),
            Instruction::Jr(label) => write!(f, "JR {}", label),
            Instruction::Call(label) => write!(f, "CALL {}", label),
            Instruction::Ret => write!(f, "RET"),
            Instruction::RetI => write!(f, "RETI"),
            Instruction::Stop => write!(f, "STOP"),
            Instruction::Nop => write!(f, "NOP"),
            Instruction::Ei => write!(f, "EI"),
            Instruction::Di => write!(f, "DI"),
        }
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Source::Register(reg) => write!(f, "{}", reg),
            Source::MemAddress(mem) => write!(f, "{}", mem),
            Source::Byte(byte) => write!(f, "{}", byte),
            Source::RegisterDI(reg) => write!(f, "{}", reg),
            Source::RegisterDDI(reg) => write!(f, "{}", reg),
        }
    }
}

impl fmt::Display for Destination {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Destination::Register(reg) => write!(f, "{}", reg),
            Destination::MemAddress(mem) => write!(f, "{}", mem),
            Destination::Byte(byte) => write!(f, "{}", byte),
            Destination::RegisterDI(reg) => write!(f, "{}", reg),
            Destination::RegisterDDI(reg) => write!(f, "{}", reg),
        }
    }
}

impl fmt::Display for RegisterDI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}+)", self.0)
    }
}

impl fmt::Display for RegisterDDI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(({}+))", self.0)
    }
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Line::Empty(Some(comment)) => {
                let comment = format!("; {}", comment);
                write!(f, "{}", comment.dimmed())
            }
            Line::Empty(None) => write!(f, ""),
            Line::Label(label, Some(comment)) => {
                let out = format!("{}:", label);
                let comment = format!("; {}", comment);
                write!(f, "{}{}", out.pad_to_width(COMMENT_WIDTH), comment.dimmed())
            }
            Line::Label(label, None) => write!(f, "{}:", label),
            Line::Instruction(inst, Some(comment)) => {
                let inst = format!("{}", inst);
                let comment = format!("; {}", comment);
                write!(
                    f,
                    "{}{}{}",
                    "".pad_to_width(INST_WIDTH),
                    inst.pad_to_width(COMMENT_WIDTH - INST_WIDTH),
                    comment.dimmed()
                )
            }
            Line::Instruction(inst, None) => write!(f, "{}", inst),
        }
    }
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#! mrasm\n")?;
        for line in &self.lines {
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}


#[cfg(test)]
mod test {
    use crate::asm::display::Format;
    use crate::asm::*;

    macro_rules! s {
        ($val:expr, $expect:expr) => {
            let formatted = format!("{}", $val);
            assert_eq!(
                formatted,
                $expect.to_string(),
                "{:#?} did not match {:#?}",
                $val,
                $expect
            );
        };
    }

    #[test]
    fn test_display_byte() {
        s!(Byte::Constant(16).format(), "0x10");
        s!(Byte::Constant(255).format(), "0xFF");
        s!(Byte::Constant(15).format(), "0x0F");
    }

    #[test]
    fn test_display_stacksize() {
        s!(Stacksize::_16.format(), "16");
        s!(Stacksize::_32.format(), "32");
        s!(Stacksize::_48.format(), "48");
        s!(Stacksize::_64.format(), "64");
        s!(Stacksize::NotSet.format(), "NOSET");
    }

    #[test]
    fn test_display_register() {
        s!(Register::R0, "R0");
        s!(Register::R1, "R1");
        s!(Register::R2, "R2");
        s!(Register::R3, "R3");
    }

    #[test]
    fn test_display_memaddress() {
        s!(MemAddress::Label("label".to_string()), "(label)");
        s!(MemAddress::Label("TEST".to_string()), "(TEST)");
        s!(MemAddress::Byte(0xff.into()), "(0xFF)");
        s!(MemAddress::Byte(10.into()), "(0x0A)");
    }

    #[test]
    fn test_display_instruction() {
        s!(Instruction::AsmOrigin(17.into()), ".ORG\t0x11");
        s!(Instruction::AsmByte(0x0A.into()), ".BYTE\t0x0A");
        s!(
            Instruction::AsmDefineBytes(vec![0.into(), 255.into(), 33.into(), 1.into()]),
            ".DB\t0x00, 0xFF, 0x21, 0x01"
        );
        s!(
            Instruction::AsmDefineWords(vec![
                0.into(),
                (255 * 255).into(),
                33.into(),
                (0x1000).into()
            ]),
            ".DW\t0x0000, 0xFE01, 0x0021, 0x1000"
        );
        s!(
            Instruction::AsmEquals("label".into(), 0xf1.into()),
            ".EQU\tlabel\t0xF1"
        );
        s!(Instruction::AsmStacksize(Stacksize::_32), "*STACKSIZE\t32");
        s!(
            Instruction::AsmStacksize(Stacksize::NotSet),
            "*STACKSIZE\tNOSET"
        );
        s!(Instruction::Clr(Register::R3), "CLR\tR3");
        s!(Instruction::Inc(Register::R2), "INC\tR2");
        s!(
            Instruction::St(MemAddress::Label("test".into()), Register::R0),
            "ST\t(test),\tR0"
        );
        s!(
            Instruction::St(MemAddress::Byte(0x03.into()), Register::R1),
            "ST\t(0x03),\tR1"
        );
        s!(Instruction::Jr("ReL".into()), "JR\tReL");
    }
}
