use std::fmt;
use either::Either;
use super::{Byte, Word, Stacksize, Register, MemAddress, Instruction};

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:>02X}", self.0)
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:>04X}", self.0)
    }
}

impl fmt::Display for Stacksize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            match self {
                Stacksize::Size16 => "16",
                Stacksize::Size32 => "32",
                Stacksize::Size48 => "48",
                Stacksize::Size64 => "64",
                Stacksize::SizeNotSet => "NOSET",
            })
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
            MemAddress::Address(addr) => write!(f, "({})", addr),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::AsmOrigin(byte) => write!(f, ".ORG\t{}", byte),
            Instruction::AsmByte(byte) => write!(f, ".BYTE\t{}", byte),
            Instruction::AsmDefineBytes(bytes) => {
                write!(f, ".DB\t")?;
                let last = bytes.last();
                for byte in &bytes[..bytes.len() - 1] {
                    write!(f, "{}, ", byte)?;
                }
                write!(f, "{}", last.expect("No bytes to define"))
            },
            Instruction::AsmDefineWords(words) => {
                write!(f, ".DW\t")?;
                let last = words.last();
                for word in &words[..words.len() - 1] {
                    write!(f, "{}, ", word)?;
                }
                write!(f, "{}", last.expect("No words to define"))
            },
            Instruction::AsmEquals(label, byte) => write!(f, ".EQU\t{}\t{}", label, byte),
            Instruction::AsmStacksize(size) => write!(f, "*STACKSIZE\t{}", size),
            Instruction::Clear(reg) => write!(f, "CLR\t{}", reg),
            Instruction::Increase(reg) => write!(f, "INC\t{}", reg),
            Instruction::Store(mem, reg) => write!(f, "ST\t{},\t{}", mem, reg),
            Instruction::JumpRelative(mem) => write!(f, "JR\t{}", mem),
        }
    }
}

impl fmt::Display for super::Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#! mrasm\n")?;
        for (line, comment) in &self.lines {
            match line {
                Either::Left(ins) => {
                    write!(f, "\t{}\t;{}", ins, comment)?;
                },
                Either::Right(label) => {
                    write!(f, "{}:\t\t;{}", label, comment)?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! s {
        ($val:expr, $expect:expr) => {
            let formatted = format!("{}", $val);
            assert_eq!(formatted, $expect.to_string(),
                "{:#?} did not match {:#?}",
                $val, $expect);
        }
    }

    #[test]
    fn test_display_byte() {
        s!(Byte(16), "0x10");
        s!(Byte(255), "0xFF");
        s!(Byte(15), "0x0F");
    }

    #[test]
    fn test_display_stacksize() {
        s!(Stacksize::Size16, "16");
        s!(Stacksize::Size32, "32");
        s!(Stacksize::Size48, "48");
        s!(Stacksize::Size64, "64");
        s!(Stacksize::SizeNotSet, "NOSET");
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
        s!(MemAddress::Address(0xff.into()), "(0xFF)");
        s!(MemAddress::Address(10.into()), "(0x0A)");
    }

    #[test]
    fn test_display_instruction() {
        s!(Instruction::AsmOrigin(17.into()),
           ".ORG\t0x11");
        s!(Instruction::AsmByte(0x0A.into()),
           ".BYTE\t0x0A");
        s!(Instruction::AsmDefineBytes(vec![0.into(), 255.into(), 33.into(), 1.into()]),
           ".DB\t0x00, 0xFF, 0x21, 0x01");
        s!(Instruction::AsmDefineWords(vec![0.into(), (255 * 255).into(),
                                            33.into(), (0x1000).into()]),
           ".DW\t0x0000, 0xFE01, 0x0021, 0x1000");
        s!(Instruction::AsmEquals("label".into(), 0xf1.into()),
           ".EQU\tlabel\t0xF1");
        s!(Instruction::AsmStacksize(Stacksize::Size32),
           "*STACKSIZE\t32");
        s!(Instruction::AsmStacksize(Stacksize::SizeNotSet),
           "*STACKSIZE\tNOSET");
        s!(Instruction::Clear(Register::R3),
           "CLR\tR3");
        s!(Instruction::Increase(Register::R2),
           "INC\tR2");
        s!(Instruction::Store(MemAddress::Label("test".into()), Register::R0),
           "ST\t(test),\tR0");
        s!(Instruction::Store(MemAddress::Address(0x03.into()), Register::R1),
           "ST\t(0x03),\tR1");
        s!(Instruction::JumpRelative(MemAddress::Label("ReL".into())),
           "JR\tReL");
    }
}
