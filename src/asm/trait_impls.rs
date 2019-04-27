use super::*;

impl From<u8> for Byte {
    fn from(byte: u8) -> Self {
        Byte::Constant(byte)
    }
}

impl From<u16> for Word {
    fn from(word: u16) -> Self {
        Word::Constant(word)
    }
}

impl From<Register> for Source {
    fn from(register: Register) -> Self {
        Source::Register(register)
    }
}

impl From<RegisterDI> for Source {
    fn from(registerdi: RegisterDI) -> Self {
        Source::RegisterDI(registerdi)
    }
}

impl From<RegisterDDI> for Source {
    fn from(registerddi: RegisterDDI) -> Self {
        Source::RegisterDDI(registerddi)
    }
}

impl From<MemAddress> for Source {
    fn from(memory_addr: MemAddress) -> Self {
        Source::MemAddress(memory_addr)
    }
}

impl From<Byte> for Source {
    fn from(byte: Byte) -> Self {
        Source::Byte(byte)
    }
}

impl From<Register> for Destination {
    fn from(register: Register) -> Self {
        Destination::Register(register)
    }
}

impl From<RegisterDI> for Destination {
    fn from(registerdi: RegisterDI) -> Self {
        Destination::RegisterDI(registerdi)
    }
}

impl From<RegisterDDI> for Destination {
    fn from(registerddi: RegisterDDI) -> Self {
        Destination::RegisterDDI(registerddi)
    }
}

impl From<MemAddress> for Destination {
    fn from(memory_addr: MemAddress) -> Self {
        Destination::MemAddress(memory_addr)
    }
}

impl From<Byte> for Destination {
    fn from(byte: Byte) -> Self {
        Destination::Byte(byte)
    }
}

impl From<Register> for RegisterDI {
    fn from(register: Register) -> Self {
        RegisterDI(register)
    }
}

impl From<Register> for RegisterDDI {
    fn from(register: Register) -> Self {
        RegisterDDI(register)
    }
}

impl From<RegisterDI> for RegisterDDI {
    fn from(registerdi: RegisterDI) -> Self {
        RegisterDDI(registerdi.0)
    }
}

impl From<Byte> for MemAddress {
    fn from(byte: Byte) -> Self {
        MemAddress::Byte(byte)
    }
}

impl From<Register> for MemAddress {
    fn from(register: Register) -> Self {
        MemAddress::Register(register)
    }
}

impl From<Label> for MemAddress {
    fn from(label: Label) -> Self {
        MemAddress::Label(label)
    }
}