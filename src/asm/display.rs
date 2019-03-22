use std::fmt;
use either::Either;
use super::{Stacksize, Register, MemAddress, Instruction};

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
            MemAddress::Address(addr) => write!(f, "({:X})", addr),
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
                for byte in bytes {
                    write!(f, "{}, ", byte)?;
                }
                write!(f, "{}", last.expect("No bytes to define"))
            },
            Instruction::AsmDefineWords(words) => {
                write!(f, ".DW\t")?;
                let last = words.last();
                for word in words {
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
