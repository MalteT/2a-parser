use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

use crate::asm::*;

mod error;
#[cfg(test)]
mod tests;

pub use error::ParserError;

type ParseResult<T> = Result<T, ParserError>;

#[derive(Parser)]
#[grammar = "../mrasm.pest"]
pub struct AsmParser;

/// Parse inner elements of a [`Pair`] into a tuple.
///
/// # Example
///
/// ```
/// ```
macro_rules! inner_tuple {
    ($rule:expr;
     $($($expected:pat )|+ => $function:ident $( | $error:literal)?);* $(;)?) => {
        {
            let mut inner = $rule.into_inner();
            // return tuple
            (
                $(
                    {
                        #[allow(unused_variables)]
                        let e1 = format!("No inner rule. '{:?}' expected '{}'",
                                         $rule,
                                         stringify!($($expected),+));
                        #[allow(unused_variables)]
                        let e2 = format!("Wrong rule found inside '{:?}'. Expected '{:?}'",
                                         $rule,
                                         stringify!($($expected),+));
                        $(let e1: String = $error.into();)?
                        $(let e2: String = $error.into();)?
                        let inner = inner.next().expect(&e1);
                        use Rule::*;
                        #[allow(unreachable_patterns)]
                        match inner.as_rule() {
                            $($expected)|+ => $function(inner),
                            _ => panic!(e2)
                        }
                    }
                ),*
            )
        }
    }
}

/// Helper function for [`inner_tuple`] macro.
/// This function is the identity for all input.
fn id<T>(element: T) -> T {
    element
}

/// Helper function for [`inner_tuple`] macro.
/// This function discards it's input.
fn ignore<T>(_: T) -> () {
    ()
}

impl AsmParser {
    /// Parse a valid Minirechner 2a assembler file.
    ///
    /// # Arguments
    /// - `input`: The [`str`] to parse.
    ///
    /// # Returns
    /// - The parsed [`assembler program`](Asm) or a [`ParserError`] or
    /// - a [`ParseError`]
    pub fn parse(input: &str) -> ParseResult<Asm> {
        let mut lines = vec![];
        let parsed = <Self as Parser<Rule>>::parse(Rule::file, input)?;
        // iterate over lines, skipping the header
        for line in parsed.skip(1) {
            if line.as_rule() == Rule::line {
                lines.push(parse_line(line)?);
            }
        }
        Ok(Asm { lines })
    }
}
/// Parse an assembler instruction line into a valid type.
///
/// # Arguments
/// - `line`: The [`Rule`] pair to be pressed into form.
///
/// # Returns
/// - Some tuple of a [`Line`] and a [`Comment`] or
/// - None, if the line is empty or
/// - a [`ParseError`]
fn parse_line(line: Pair<Rule>) -> ParseResult<Line> {
    let line = line.into_inner();
    let mut ret = Line::Empty(None);
    // Possible elements in a line:
    // - ws
    // - label
    // - instruction
    // - comment
    // Mutate ret accordingly
    for element in line {
        ret = match element.as_rule() {
            Rule::ws => ret,
            // The label or instruction rule comes first and they occur
            // exclusive so replacing is just fine.
            Rule::label => Line::Label(parse_label(element), None),
            Rule::instruction => Line::Instruction(parse_instruction(element)?, None),
            // comment can only occur once.
            // So it has to be THE comment.
            Rule::comment => {
                let c = Some(parse_comment(element));
                match ret {
                    Line::Empty(_) => Line::Empty(c),
                    Line::Instruction(i, _) => Line::Instruction(i, c),
                    Line::Label(l, _) => Line::Label(l, c),
                }
            }
            _ => unreachable!(),
        }
    }
    Ok(ret)
}
/// Parse a label rule into a [`Label`].
///
/// # Arguments
/// - `label`: The [`Rule`] pair to be parsed.
///
/// # Returns
/// - The parsed `Label`
fn parse_label(label: Pair<Rule>) -> Label {
    let (label, _) = inner_tuple! { label;
        raw_label => parse_raw_label;
        colon => ignore
    };
    label
}
/// Parse a raw_label rule into a [`Label`].
///
/// # Arguments
/// - `label`: The [`Rule`] pair to be parsed.
///
/// # Returns
/// - The parsed `Label`
fn parse_raw_label(label: Pair<Rule>) -> Label {
    label.as_str().into()
}
/// Parse an instruction rule into an [`Instruction`].
///
/// # Arguments
/// - `instruction`: The [`Rule`] pair to be parsed.
///
/// # Returns
/// - The parsed `Instruction`, or
/// - a [`ParseError`]
fn parse_instruction(instruction: Pair<Rule>) -> ParseResult<Instruction> {
    let instruction = instruction
        .into_inner()
        .next()
        .expect("an instruction rule should have an actual instruction");
    let instruction = match instruction.as_rule() {
        Rule::org => parse_instruction_org(instruction)?,
        Rule::byte => parse_instruction_byte(instruction)?,
        Rule::db => parse_instruction_db(instruction)?,
        Rule::dw => parse_instruction_dw(instruction)?,
        Rule::equ => parse_instruction_equ(instruction)?,
        Rule::stacksize => parse_instruction_stacksize(instruction),
        Rule::clr => parse_instruction_clr(instruction),
        Rule::add => parse_instruction_add(instruction),
        Rule::adc => parse_instruction_adc(instruction),
        Rule::sub => parse_instruction_sub(instruction),
        Rule::mul => parse_instruction_mul(instruction),
        Rule::div => parse_instruction_div(instruction),
        Rule::inc => parse_instruction_inc(instruction),
        Rule::dec => parse_instruction_dec(instruction)?,
        Rule::neg => parse_instruction_neg(instruction),
        Rule::and => parse_instruction_and(instruction),
        Rule::or => parse_instruction_or(instruction),
        Rule::xor => parse_instruction_xor(instruction),
        Rule::com => parse_instruction_com(instruction),
        Rule::bits => parse_instruction_bits(instruction)?,
        Rule::bitc => parse_instruction_bitc(instruction)?,
        Rule::tst => parse_instruction_tst(instruction),
        Rule::cmp => parse_instruction_cmp(instruction)?,
        Rule::bitt => parse_instruction_bitt(instruction)?,
        Rule::lsr => parse_instruction_lsr(instruction),
        Rule::asr => parse_instruction_asr(instruction),
        Rule::lsl => parse_instruction_lsl(instruction),
        Rule::rrc => parse_instruction_rrc(instruction),
        Rule::rlc => parse_instruction_rlc(instruction),
        Rule::mov => parse_instruction_mov(instruction)?,
        Rule::ld_const => parse_instruction_ld_const(instruction)?,
        Rule::ld_memory => parse_instruction_ld_memory(instruction)?,
        Rule::st => parse_instruction_st(instruction)?,
        Rule::push => parse_instruction_push(instruction),
        Rule::pop => parse_instruction_pop(instruction),
        Rule::pushf => parse_instruction_pushf(),
        Rule::popf => parse_instruction_popf(),
        Rule::ldsp => parse_instruction_ldsp(instruction)?,
        Rule::ldfr => parse_instruction_ldfr(instruction)?,
        Rule::jmp => parse_instruction_jmp(instruction)?,
        Rule::jcs => parse_instruction_jcs(instruction),
        Rule::jcc => parse_instruction_jcc(instruction),
        Rule::jzs => parse_instruction_jzs(instruction),
        Rule::jzc => parse_instruction_jzc(instruction),
        Rule::jns => parse_instruction_jns(instruction),
        Rule::jnc => parse_instruction_jnc(instruction),
        Rule::jr => parse_instruction_jr(instruction),
        Rule::call => parse_instruction_call(instruction),
        Rule::ret => parse_instruction_ret(),
        Rule::reti => parse_instruction_reti(),
        Rule::stop => parse_instruction_stop(),
        Rule::nop => parse_instruction_nop(),
        Rule::ei => parse_instruction_ei(),
        Rule::di => parse_instruction_di(),
        _ => unreachable!(),
    };
    Ok(instruction)
}
/// Parse an org rule into an [`Instruction`].
fn parse_instruction_org(org: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, constant) = inner_tuple! { org;
        ws          => ignore;
        constant    => parse_byte;
    };
    Ok(Instruction::AsmOrigin(constant?))
}
/// Parse a constant rule into a [`Byte`].
fn parse_byte(constant: Pair<Rule>) -> ParseResult<Byte> {
    let inner = inner_tuple! { constant;
        constant_bin | constant_hex | constant_dec | raw_label => id;
    };
    match inner.as_rule() {
        Rule::constant_bin => u8::from_str_radix(&inner.as_str()[2..], 2)
            .map(|nr| Byte::Constant(nr))
            .map_err(|e| ParserError::NotAByte(inner.as_str().into(), e)),
        Rule::constant_hex => u8::from_str_radix(&inner.as_str()[2..], 16)
            .map(|nr| Byte::Constant(nr))
            .map_err(|e| ParserError::NotAByte(inner.as_str().into(), e)),
        Rule::constant_dec => u8::from_str_radix(&inner.as_str(), 10)
            .map(|nr| Byte::Constant(nr))
            .map_err(|e| ParserError::NotAByte(inner.as_str().into(), e)),
        Rule::raw_label => Ok(Byte::Label(parse_raw_label(inner))),
        _ => unreachable!(),
    }
}
/// Parse a constant rule into a [`Byte`].
fn parse_word(constant: Pair<Rule>) -> ParseResult<Word> {
    let inner = inner_tuple! { constant;
        constant_bin | constant_hex | constant_dec | raw_label => id;
    };
    match inner.as_rule() {
        Rule::constant_bin => u16::from_str_radix(&inner.as_str()[1..], 2)
            .map(|nr| Word::Constant(nr))
            .map_err(|e| ParserError::NotAByte(inner.as_str().into(), e)),
        Rule::constant_hex => u16::from_str_radix(&inner.as_str()[1..], 16)
            .map(|nr| Word::Constant(nr))
            .map_err(|e| ParserError::NotAByte(inner.as_str().into(), e)),
        Rule::constant_dec => u16::from_str_radix(&inner.as_str(), 10)
            .map(|nr| Word::Constant(nr))
            .map_err(|e| ParserError::NotAByte(inner.as_str().into(), e)),
        Rule::raw_label => Ok(Word::Label(parse_raw_label(inner))),
        _ => unreachable!(),
    }
}
/// Parse a byte rule into an [`Instruction`].
fn parse_instruction_byte(byte: Pair<Rule>) -> ParseResult<Instruction> {
    let byte = inner_tuple! { byte;
        constant => parse_byte;
    };
    Ok(Instruction::AsmByte(byte?))
}
/// Parse a db rule into an [`Instruction`].
fn parse_instruction_db(db: Pair<Rule>) -> ParseResult<Instruction> {
    let mut results = db
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::constant)
        .map(|byte| parse_byte(byte));
    for result in results.by_ref() {
        if result.is_err() {
            return Err(result.unwrap_err());
        }
    }
    let bytes = results.map(|result| result.unwrap()).collect();
    Ok(Instruction::AsmDefineBytes(bytes))
}
/// Parse a dw rule into an [`Instruction`].
fn parse_instruction_dw(dw: Pair<Rule>) -> ParseResult<Instruction> {
    let mut results = dw
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::constant)
        .map(|word| parse_word(word));
    for result in results.by_ref() {
        if result.is_err() {
            return Err(result.unwrap_err());
        }
    }
    let words = results.map(|result| result.unwrap()).collect();
    Ok(Instruction::AsmDefineWords(words))
}
/// Parse an equ rule into an [`Instruction`].
fn parse_instruction_equ(equ: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, label, _, constant) = inner_tuple! { equ;
        ws          => ignore;
        raw_label   => parse_raw_label;
        ws          => ignore;
        constant    => parse_byte;
    };
    Ok(Instruction::AsmEquals(label, constant?))
}
/// Parse a stacksize rule into an [`Instruction`].
fn parse_instruction_stacksize(instruction: Pair<Rule>) -> Instruction {
    let (_, stacksize) = inner_tuple! { instruction;
        ws              => ignore;
        raw_stacksize   => parse_raw_stacksize;
    };
    Instruction::AsmStacksize(stacksize)
}
/// Parse a raw_stacksize rule into a [`Stacksize`].
fn parse_raw_stacksize(stacksize: Pair<Rule>) -> Stacksize {
    let stacksize = stacksize.as_str().to_lowercase();
    match stacksize.as_str() {
        "16" => Stacksize::_16,
        "32" => Stacksize::_32,
        "48" => Stacksize::_48,
        "64" => Stacksize::_64,
        "noset" => Stacksize::NotSet,
        _ => unreachable!(),
    }
}
/// Parse a clr rule into an [`Instruction`].
fn parse_instruction_clr(instruction: Pair<Rule>) -> Instruction {
    let (_, register) = inner_tuple! { instruction;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Clr(register)
}
/// Parse a register rule into a [`Register`].
fn parse_register(register: Pair<Rule>) -> Register {
    let reg = register.as_str().to_lowercase();
    match reg.as_str() {
        "r0" => Register::R0,
        "r1" => Register::R1,
        "r2" => Register::R2,
        "r3" => Register::R3,
        _ => unreachable!(),
    }
}
/// Parse an add rule into an [`Instruction`].
fn parse_instruction_add(add: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { add;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Add(reg1, reg2)
}
/// Parse an adc rule into an [`Instruction`].
fn parse_instruction_adc(adc: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { adc;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Adc(reg1, reg2)
}
/// Parse a sub rule into an [`Instruction`].
fn parse_instruction_sub(sub: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { sub;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Sub(reg1, reg2)
}
/// Parse a mul rule into an [`Instruction`].
fn parse_instruction_mul(mul: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { mul;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Mul(reg1, reg2)
}
/// Parse a div rule into an [`Instruction`].
fn parse_instruction_div(div: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { div;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Div(reg1, reg2)
}
/// Parse an inc rule into an [`Instruction`].
fn parse_instruction_inc(inc: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { inc;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Inc(reg)
}
/// Parse a dec rule into an [`Instruction`].
fn parse_instruction_dec(dec: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, source) = inner_tuple! { dec;
        ws      => ignore;
        source  => parse_source;
    };
    Ok(Instruction::Dec(source?))
}
/// Parse a source rule into a [`Source`].
fn parse_source(source: Pair<Rule>) -> ParseResult<Source> {
    let inner = source
        .into_inner()
        .next()
        .expect("source needs an inner element");
    let source = match inner.as_rule() {
        Rule::register => parse_register(inner).into(),
        Rule::registerdi => parse_register_di(inner).into(),
        Rule::registerddi => parse_register_ddi(inner).into(),
        Rule::memory => parse_memory(inner)?.into(),
        Rule::constant => parse_byte(inner)?.into(),
        _ => unreachable!(),
    };
    Ok(source)
}
/// Parse a registerdi rule into a [`RegisterDI`].
fn parse_register_di(registerdi: Pair<Rule>) -> RegisterDI {
    let (_, register, _, _) = inner_tuple! { registerdi;
        oparen      => ignore;
        register    => parse_register;
        plus        => ignore;
        cparen      => ignore;
    };
    register.into()
}
/// Parse a registerddi rule into a [`RegisterDDI`].
fn parse_register_ddi(registerddi: Pair<Rule>) -> RegisterDDI {
    let (_, register, _) = inner_tuple! { registerddi;
        oparen      => ignore;
        registerdi  => parse_register_di;
        cparen      => ignore;
    };
    register.into()
}
/// Parse a memory rule into a [`MemAddress`].
fn parse_memory(memory: Pair<Rule>) -> ParseResult<MemAddress> {
    let (_, inner, _) = inner_tuple! { memory;
        oparen                                                  => ignore;
        register | registerdi | registerddi | memory | constant => id;
        cparen                                                  => ignore;
    };
    let memory = match inner.as_rule() {
        Rule::constant => parse_byte(inner)?.into(),
        Rule::register => parse_register(inner).into(),
        Rule::raw_label => parse_raw_label(inner).into(),
        _ => unreachable!(),
    };
    Ok(memory)
}
/// Parse a neg rule into an [`Instruction`].
fn parse_instruction_neg(neg: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { neg;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Neg(reg)
}
/// Parse an and rule into an [`Instruction`].
fn parse_instruction_and(and: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { and;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::And(reg1, reg2)
}
/// Parse an or rule into an [`Instruction`].
fn parse_instruction_or(or: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { or;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Or(reg1, reg2)
}
/// Parse an xor rule into an [`Instruction`].
fn parse_instruction_xor(xor: Pair<Rule>) -> Instruction {
    let (_, reg1, _, reg2) = inner_tuple! { xor;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        register    => parse_register;
    };
    Instruction::Xor(reg1, reg2)
}
/// Parse a com rule into an [`Instruction`].
fn parse_instruction_com(com: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { com;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Com(reg)
}
/// Parse a bits rule into an [`Instruction`].
fn parse_instruction_bits(bits: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, dst, _, src) = inner_tuple! { bits;
        ws          => ignore;
        destination => parse_destination;
        comma       => ignore;
        source      => parse_source;
    };
    Ok(Instruction::Bits(dst?, src?))
}
/// Parse a destination rule into a [`Destination`].
fn parse_destination(destination: Pair<Rule>) -> ParseResult<Destination> {
    let inner = destination
        .into_inner()
        .next()
        .expect("source needs an inner element");
    let destination = match inner.as_rule() {
        Rule::register => parse_register(inner).into(),
        Rule::registerdi => parse_register_di(inner).into(),
        Rule::registerddi => parse_register_ddi(inner).into(),
        Rule::memory => parse_memory(inner)?.into(),
        Rule::constant => parse_byte(inner)?.into(),
        _ => unreachable!(),
    };
    Ok(destination)
}
/// Parse a bitc rule into an [`Instruction`].
fn parse_instruction_bitc(bitc: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, dst, _, src) = inner_tuple! { bitc;
        ws          => ignore;
        destination => parse_destination;
        comma       => ignore;
        source      => parse_source;
    };
    Ok(Instruction::Bitc(dst?, src?))
}
/// Parse a tst rule into an [`Instruction`].
fn parse_instruction_tst(tst: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { tst;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Tst(reg)
}
/// Parse a cmp rule into an [`Instruction`].
fn parse_instruction_cmp(cmp: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, dst, _, src) = inner_tuple! { cmp;
        ws          => ignore;
        destination => parse_destination;
        comma       => ignore;
        source      => parse_source;
    };
    Ok(Instruction::Cmp(dst?, src?))
}
/// Parse a bitt rule into an [`Instruction`].
fn parse_instruction_bitt(bitt: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, dst, _, src) = inner_tuple! { bitt;
        ws          => ignore;
        destination => parse_destination;
        comma       => ignore;
        source      => parse_source;
    };
    Ok(Instruction::Bitt(dst?, src?))
}
/// Parse a lsr rule into an [`Instruction`].
fn parse_instruction_lsr(lsr: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { lsr;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Lsr(reg)
}
/// Parse an asr rule into an [`Instruction`].
fn parse_instruction_asr(asr: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { asr;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Asr(reg)
}
/// Parse a lsl rule into an [`Instruction`].
fn parse_instruction_lsl(lsl: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { lsl;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Lsl(reg)
}
/// Parse a rrc rule into an [`Instruction`].
fn parse_instruction_rrc(rrc: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { rrc;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Rrc(reg)
}
/// Parse a rlc rule into an [`Instruction`].
fn parse_instruction_rlc(rlc: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { rlc;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Rlc(reg)
}
/// Parse a mov rule into an [`Instruction`].
fn parse_instruction_mov(mov: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, dst, _, src) = inner_tuple! { mov;
        ws          => ignore;
        destination => parse_destination;
        comma       => ignore;
        source      => parse_source;
    };
    Ok(Instruction::Mov(dst?, src?))
}
/// Parse a ld_const rule into an [`Instruction`].
fn parse_instruction_ld_const(ld_const: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, reg, _, byte) = inner_tuple! { ld_const;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        constant    => parse_byte;
    };
    Ok(Instruction::LdByte(reg, byte?))
}
/// Parse a ld_memory rule into an [`Instruction`].
fn parse_instruction_ld_memory(ld_memory: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, reg, _, mem) = inner_tuple! { ld_memory;
        ws          => ignore;
        register    => parse_register;
        comma       => ignore;
        memory      => parse_memory;
    };
    Ok(Instruction::LdMemAddress(reg, mem?))
}
/// Parse a st rule into an [`Instruction`].
fn parse_instruction_st(st: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, mem, _, reg) = inner_tuple! { st;
        ws          => ignore;
        memory      => parse_memory;
        comma       => ignore;
        register    => parse_register;
    };
    Ok(Instruction::St(mem?, reg))
}
/// Parse a push rule into an [`Instruction`].
fn parse_instruction_push(push: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { push;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Push(reg)
}
/// Parse a pop rule into an [`Instruction`].
fn parse_instruction_pop(pop: Pair<Rule>) -> Instruction {
    let (_, reg) = inner_tuple! { pop;
        ws          => ignore;
        register    => parse_register;
    };
    Instruction::Pop(reg)
}
/// Parse a pushf rule into an [`Instruction`].
fn parse_instruction_pushf() -> Instruction {
    Instruction::PushF
}
/// Parse a popf rule into an [`Instruction`].
fn parse_instruction_popf() -> Instruction {
    Instruction::PopF
}
/// Parse a ldsp rule into an [`Instruction`].
fn parse_instruction_ldsp(ldsp: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, src) = inner_tuple! { ldsp;
        ws      => ignore;
        source  => parse_source;
    };
    Ok(Instruction::Ldsp(src?))
}
/// Parse a ldfr rule into an [`Instruction`].
fn parse_instruction_ldfr(ldfr: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, src) = inner_tuple! { ldfr;
        ws      => ignore;
        source  => parse_source;
    };
    Ok(Instruction::Ldfr(src?))
}
/// Parse a jmp rule into an [`Instruction`].
fn parse_instruction_jmp(jmp: Pair<Rule>) -> ParseResult<Instruction> {
    let (_, mem) = inner_tuple! { jmp;
        ws      => ignore;
        memory  => parse_memory;
    };
    Ok(Instruction::Jmp(mem?))
}
/// Parse a jcs rule into an [`Instruction`].
fn parse_instruction_jcs(jcs: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jcs;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jcs(label)
}
/// Parse a jcc rule into an [`Instruction`].
fn parse_instruction_jcc(jcc: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jcc;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jcc(label)
}
/// Parse a jzs rule into an [`Instruction`].
fn parse_instruction_jzs(jzs: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jzs;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jzs(label)
}
/// Parse a jzc rule into an [`Instruction`].
fn parse_instruction_jzc(jzc: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jzc;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jzc(label)
}
/// Parse a jns rule into an [`Instruction`].
fn parse_instruction_jns(jns: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jns;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jns(label)
}
/// Parse a jnc rule into an [`Instruction`].
fn parse_instruction_jnc(jnc: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jnc;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jnc(label)
}
/// Parse a jr rule into an [`Instruction`].
fn parse_instruction_jr(jr: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { jr;
        ws      => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Jr(label)
}
/// Parse a call rule into an [`Instruction`].
fn parse_instruction_call(call: Pair<Rule>) -> Instruction {
    let (_, label) = inner_tuple! { call;
        ws          => ignore;
        raw_label   => parse_raw_label;
    };
    Instruction::Call(label)
}
/// Parse a ret rule into an [`Instruction`].
fn parse_instruction_ret() -> Instruction {
    Instruction::Ret
}
/// Parse a reti rule into an [`Instruction`].
fn parse_instruction_reti() -> Instruction {
    Instruction::RetI
}
/// Parse a stop rule into an [`Instruction`].
fn parse_instruction_stop() -> Instruction {
    Instruction::Stop
}
/// Parse a nop rule into an [`Instruction`].
fn parse_instruction_nop() -> Instruction {
    Instruction::Nop
}
/// Parse an ei rule into an [`Instruction`].
fn parse_instruction_ei() -> Instruction {
    Instruction::Ei
}
/// Parse a di rule into an [`Instruction`].
fn parse_instruction_di() -> Instruction {
    Instruction::Di
}
/// Parse a comment rule into a [`Comment`].
///
/// # Arguments
/// - `comment`: The [`Rule`] pair to be parsed.
///
/// # Returns
/// - The parsed `Comment`
fn parse_comment(comment: Pair<Rule>) -> Comment {
    let (_, comment) = inner_tuple! { comment;
        semicolon   => ignore;
        rest        => id;
    };
    comment.as_str().trim_matches(|c| " \t;".contains(c)).into()
}
