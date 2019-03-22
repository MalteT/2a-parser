use either::Either;
use std::ops::{Deref, DerefMut};

type Line = Either<Instruction, Label>;
type Comment = String;
type Label = String;
type Address = Byte;

mod display;

/// A single byte.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Byte(u8);

/// A single word.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Word(u16);

/// The different stack sizes the Stack may have.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stacksize {
    /// 16 byte stack.
    Size16,
    /// 32 byte stack.
    Size32,
    /// 48 byte stack.
    Size48,
    /// 64 byte stack.
    Size64,
    /// Unlimited stack.
    SizeNotSet,
}

/// Possible register values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    /// Register 0.
    R0,
    /// Register 1.
    R1,
    /// Register 2.
    R2,
    /// Register 3 and program counter (PC).
    R3
}

/// Memory address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MemAddress {
    /// Referencing a Label.
    Label(Label),
    /// Direct address in memory.
    Address(Address),
}

/// Possible instructions for the assembler.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// Set program origin.
    AsmOrigin(Address),
    /// Define byte.
    AsmByte(Byte),
    /// Define multiple bytes.
    AsmDefineBytes(Vec<Byte>),
    /// Define multiple words.
    AsmDefineWords(Vec<Word>),
    /// Make label equivalent to byte.
    AsmEquals(Label, Byte),
    /// Define stacksize.
    AsmStacksize(Stacksize),
    /// Clear the register.
    Clear(Register),
    /// Increase the register by 1.
    Increase(Register),
    /// Store the register in memory.
    Store(MemAddress, Register),
    /// Relative jump to position.
    JumpRelative(MemAddress),
}

/// Represenation of a Minirechner2a ASM program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Asm {
    pub lines: Vec<(Line, Comment)>,
}

impl Deref for Byte {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Byte {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<u8> for Byte {
    fn from(byte: u8) -> Self {
        Byte(byte)
    }
}

impl From<Byte> for u8 {
    fn from(byte: Byte) -> u8 {
        byte.0
    }
}

impl Deref for Word {
    type Target = u16;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Word {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<u16> for Word {
    fn from(word: u16) -> Self {
        Word(word)
    }
}

impl From<Word> for u16 {
    fn from(word: Word) -> u16 {
        word.0
    }
}
