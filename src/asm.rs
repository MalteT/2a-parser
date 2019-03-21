
/// The different stack sizes the Stack may have.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Stacksize {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Register {
    /// Register 0.
    R0,
    /// Register 1.
    R1,
    /// Register 2.
    R2,
    /// Register 3 and program counter (PC).
    R3
}

/// Possible instructions for the assembler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Instruction {
    ///
    EmptyLine,
}
