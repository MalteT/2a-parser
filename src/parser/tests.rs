use super::AsmParser;
use pest::Parser;
use super::Rule;

macro_rules! parse {
    ($rule:expr, $val:expr) => {
        let res = AsmParser::parse($rule, $val);
        let s = format!("{} did not parse {}.",
                        stringify!($rule), stringify!($val));
        match res {
            Ok(_) => (),
            Err(_) => panic!("{}\n{:#?}", s, res),
        }
    };
    ($rule:expr, $val:expr, $expected:expr) => {
        let res = AsmParser::parse($rule, $val);
        let s = format!("{} did not parse {}.",
                        stringify!($rule), stringify!($val));
        match res {
            Ok(ref r) => assert_eq!(r.as_str(), $expected),
            Err(_) => panic!("{}\n{:#?}", s, res),
        }
    }
}

macro_rules! parse_err {
    ($rule:expr, $val:expr) => {
        let res = AsmParser::parse($rule, $val);
        let s = format!("{} parsed {}.",
                        stringify!($rule), stringify!($val));
        match res {
            Err(_) => (),
            Ok(_) => panic!("{}\n{:#?}", s, res),
        }
    }
}

#[test]
fn test_whitespace() {
    use Rule::WHITESPACE;
    parse!(WHITESPACE, " ");
    parse!(WHITESPACE, "\t");
    parse_err!(WHITESPACE, "whitespace");
}

#[test]
fn test_eol() {
    use Rule::eol;
    parse!(eol, "\n");
    parse!(eol, "\r\n");
    parse_err!(eol, "\t");
}

#[test]
fn test_colon() {
    use Rule::colon;
    parse!(colon, ":");
    parse_err!(colon, ";");
}

#[test]
fn test_semicolon() {
    use Rule::semicolon;
    parse!(semicolon, ";");
    parse_err!(semicolon, ":");
}

#[test]
fn test_ws() {
    use Rule::ws;
    parse!(ws, " ");
    parse!(ws, "\t");
    parse!(ws, " \t  \t");
    parse_err!(ws, "");
    parse_err!(ws, "_");
}

#[test]
fn test_number_bin() {
    use Rule::number_bin;
    parse!(number_bin, "0b10010");
    parse!(number_bin, "0b0");
    parse!(number_bin, "0b11111111");
    parse!(number_bin, "0b100000000");
    parse_err!(number_bin, "0x10");
    parse_err!(number_bin, "10");
    parse_err!(number_bin, "0b2");
}

#[test]
fn test_number_hex() {
    use Rule::number_hex;
    parse!(number_hex, "0xFF");
    parse!(number_hex, "0xf0");
    parse!(number_hex, "0x0");
    parse!(number_hex, "0x123456789");
    parse!(number_hex, "0xabcdef");
    parse!(number_hex, "0xffg", "0xff");
    parse_err!(number_hex, "ff");
    parse_err!(number_hex, "10");
    parse_err!(number_hex, "0bff");
}

#[test]
fn test_number_dec() {
    use Rule::number_dec;
    parse!(number_dec, "0");
    parse!(number_dec, "10");
    parse!(number_dec, "0123456789");
    parse!(number_dec, "01a1", "01");
    parse!(number_dec, "0123456789a", "0123456789");
    parse!(number_dec, "0xff", "0");
    parse!(number_dec, "0b10", "0");
    parse_err!(number_dec, "");
}

#[test]
fn test_number() {
    use Rule::number;
    parse!(number, "0");
    parse!(number, "0b0");
    parse!(number, "0x0");
    parse!(number, "0b10");
    parse!(number, "0x10");
    parse!(number, "010");
}

#[test]
fn test_label() {
    use Rule::label;
    parse!(label, "abcdefghijklmnopqrstuvwxyz");
    parse!(label, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    parse!(label, "0123456789");
    parse!(label, "abc0123456789def");
    parse_err!(label, "RLDK0JF");
    parse_err!(label, "PCLD0KJF");
    parse_err!(label, "SPLD0KJF");
}

#[test]
fn test_rest() {
    use Rule::rest;
    parse!(rest, "sdlkfj sldkj3k j23lk4 j2l3kj szx/c.,76%&^%()");
    parse!(rest, "\t !@#$%^&*()_74125896300-.<>|");
    parse!(rest, "rest\ntest", "rest");
    parse!(rest, "rest\r\ntest", "rest");
    parse!(rest, "rest\rtest", "rest");
}

#[test]
fn test_stacksize() {
    use Rule::stacksize;
    parse!(stacksize, "16");
    parse!(stacksize, "32");
    parse!(stacksize, "48");
    parse!(stacksize, "64");
    parse!(stacksize, "noset");
    parse!(stacksize, "nOSet");
    parse!(stacksize, "NOSET");
    parse_err!(stacksize, "17");
    parse_err!(stacksize, "128");
    parse_err!(stacksize, "set");
}

#[test]
fn test_register() {
    use Rule::register;
    parse!(register, "r0");
    parse!(register, "R0");
    parse!(register, "r1");
    parse!(register, "R1");
    parse!(register, "r2");
    parse!(register, "R2");
    parse!(register, "r3");
    parse!(register, "R3");
    parse_err!(register, "R4");
    parse_err!(register, "r4");
    parse_err!(register, "r");
}

#[test]
fn test_comment() {
    use Rule::comment;
    parse!(comment, "; a comment", "; a comment");
    parse!(comment, ";comment\tcomment", ";comment\tcomment");
    parse!(comment, ";comment\ntest", ";comment");
    parse!(comment, ";comment\r\ntest", ";comment");
    parse!(comment, ";comment\rtest", ";comment");
    parse_err!(comment, ": comment");
}

#[test]
fn test_target() {
    use Rule::target;
    parse!(target, "label:", "label:");
    parse_err!(target, "label :");
    parse_err!(target, "label\n:");
}

#[test]
fn test_header() {
    use Rule::header;
    parse!(header, "#! mrasm\n");
    parse!(header, "#! mrasm");
    parse_err!(header, "#!mrasm");
    parse_err!(header, " #! mrasm");
    parse_err!(header, "\t#! mrasm");
}

#[test]
fn test_asm_org() {
    use Rule::asm_org;
    parse!(asm_org, ".ORG 16");
    parse!(asm_org, ".org 0x16");
    parse!(asm_org, ".ORG\t0b100");
    parse!(asm_org, ".org 16\n", ".org 16");
    parse_err!(asm_org, ".org20");
}

#[test]
fn test_asm_byte() {
    use Rule::asm_byte;
    parse!(asm_byte, ".BYTE 222");
    parse!(asm_byte, ".byTe\t0xff");
    parse!(asm_byte, ".bYte\t 0b1011");
    parse!(asm_byte, ".byte\t 0b1011");
    parse_err!(asm_byte, ".byte22");
}

#[test]
fn test_asm_db() {
    use Rule::asm_db;
    parse!(asm_db, ".DB 20, 0xff, 0b11110");
    parse!(asm_db, ".db 100\t,0xf\t ,\t\t0b1");
    parse!(asm_db, ".db 100\t0xf\t ,\t\t0b1", ".db 100");
}

#[test]
fn test_asm_dw() {
    use Rule::asm_dw;
    parse!(asm_dw, ".DW 20, 0xff, 0b11110");
    parse!(asm_dw, ".dw 100\t,0xf\t ,\t\t0b1");
    parse!(asm_dw, ".dW 100\t0xf\t ,\t\t0b1", ".dW 100");
}

#[test]
fn test_asm_equ() {
    use Rule::asm_equ;
    parse!(asm_equ, ".EQU label 0xf");
    parse!(asm_equ, ".equ rest 10");
    parse_err!(asm_equ, ".eq rest 10");
    parse_err!(asm_equ, ".equ rest, 20");
    parse_err!(asm_equ, ".equ rest, 0xff");
    parse_err!(asm_equ, ".equ 0xff");
    parse_err!(asm_equ, ".equ label");
}

#[test]
fn test_asm_stack() {
    use Rule::asm_stack;
    parse!(asm_stack, "*STACKSIZE 16");
    parse!(asm_stack, "*stacksize noset");
    parse_err!(asm_stack, "*stacksize 17");
    parse_err!(asm_stack, "*STACKSIZE48");
}

#[test]
fn test_ins_inc() {
    use Rule::ins_inc;
    parse!(ins_inc, "INC R0");
    parse!(ins_inc, "inc r2");
    parse!(ins_inc, "inc R1");
    parse!(ins_inc, "inc r3");
    parse_err!(ins_inc, "inc 0xff");
    parse_err!(ins_inc, "inc r4");
    parse_err!(ins_inc, "INCR2");
}

#[test]
fn test_line() {
    use Rule::line;
    parse!(line, "HERE:\t\t; jump here", "HERE:\t\t; jump here");
    parse!(line, "INC R2 ; increase r2", "INC R2 ; increase r2");
}

#[test]
fn test_file() {
    use Rule::file;
    parse!(file, "#! mrasm\nHERE:    ;jump here\ninc r0;increase this\n\n");
}
