use super::AsmParser;
use super::Rule;
use pest::Parser;

macro_rules! parse {
    ($rule:expr, $val:expr) => {
        let res = <AsmParser as Parser<Rule>>::parse($rule, $val);
        let s = format!("{} did not parse {}.", stringify!($rule), stringify!($val));
        match res {
            Ok(_) => (),
            Err(e) => panic!("{}\n{}", s, e),
        }
    };
    ($rule:expr, $val:expr, $expected:expr) => {
        let res = <AsmParser as Parser<Rule>>::parse($rule, $val);
        let s = format!("{} did not parse {}.", stringify!($rule), stringify!($val));
        match res {
            Ok(ref r) => assert_eq!(r.as_str(), $expected),
            Err(e) => panic!("{}\n{}", s, e),
        }
    };
}

macro_rules! parse_err {
    ($rule:expr, $val:expr) => {
        let res = <AsmParser as Parser<Rule>>::parse($rule, $val);
        let s = format!("{} parsed {}.", stringify!($rule), stringify!($val));
        match res {
            Err(_) => (),
            Ok(_) => panic!("{}\n{:#?}", s, res),
        }
    };
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
fn test_constant_bin() {
    use Rule::constant_bin;
    parse!(constant_bin, "0b10010");
    parse!(constant_bin, "0b0");
    parse!(constant_bin, "0b11111111");
    parse!(constant_bin, "0b100000000");
    parse_err!(constant_bin, "0x10");
    parse_err!(constant_bin, "10");
    parse_err!(constant_bin, "0b2");
}

#[test]
fn test_constant_hex() {
    use Rule::constant_hex;
    parse!(constant_hex, "0xFF");
    parse!(constant_hex, "0xf0");
    parse!(constant_hex, "0x0");
    parse!(constant_hex, "0x123456789");
    parse!(constant_hex, "0xabcdef");
    parse!(constant_hex, "0xffg", "0xff");
    parse_err!(constant_hex, "ff");
    parse_err!(constant_hex, "10");
    parse_err!(constant_hex, "0bff");
}

#[test]
fn test_constant_dec() {
    use Rule::constant_dec;
    parse!(constant_dec, "0");
    parse!(constant_dec, "10");
    parse!(constant_dec, "011", "011");
    parse!(constant_dec, "0xff", "0");
    parse!(constant_dec, "0b10", "0");
    parse!(constant_dec, "0123456789", "0123");
    parse_err!(constant_dec, "");
}

#[test]
fn test_constant() {
    use Rule::constant;
    parse!(constant, "0");
    parse!(constant, "0b0");
    parse!(constant, "0x0");
    parse!(constant, "0b10");
    parse!(constant, "0x10");
    parse!(constant, "010");
}

#[test]
fn test_raw_label() {
    use Rule::raw_label;
    parse!(raw_label, "abcdefghijklmnopqrstuvwxyz");
    parse!(raw_label, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    parse!(raw_label, "0123456789");
    parse!(raw_label, "abc0123456789def");
    parse_err!(raw_label, "RLDK0JF");
    parse_err!(raw_label, "PCLD0KJF");
    parse_err!(raw_label, "SPLD0KJF");
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
    use Rule::raw_stacksize;
    parse!(raw_stacksize, "16");
    parse!(raw_stacksize, "32");
    parse!(raw_stacksize, "48");
    parse!(raw_stacksize, "64");
    parse!(raw_stacksize, "noset");
    parse!(raw_stacksize, "nOSet");
    parse!(raw_stacksize, "NOSET");
    parse_err!(raw_stacksize, "17");
    parse_err!(raw_stacksize, "128");
    parse_err!(raw_stacksize, "set");
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
fn test_label() {
    use Rule::label;
    parse!(label, "label:", "label:");
    parse_err!(label, "label :");
    parse_err!(label, "label\n:");
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
fn test_org() {
    use Rule::org;
    parse!(org, ".ORG 16");
    parse!(org, ".org 0x16");
    parse!(org, ".ORG\t0b100");
    parse!(org, ".org 16\n", ".org 16");
    parse_err!(org, ".org20");
}

#[test]
fn test_byte() {
    use Rule::byte;
    parse!(byte, ".BYTE 222");
    parse!(byte, ".byTe\t0xff");
    parse!(byte, ".bYte\t 0b1011");
    parse!(byte, ".byte\t 0b1011");
    parse_err!(byte, ".byte22");
}

#[test]
fn test_db() {
    use Rule::db;
    parse!(db, ".DB 20, 0xff, 0b11110");
    parse!(db, ".db 100\t,0xf\t ,\t\t0b1");
    parse!(db, ".db 100\t0xf\t ,\t\t0b1", ".db 100");
}

#[test]
fn test_dw() {
    use Rule::dw;
    parse!(dw, ".DW 20, 0xff, 0b11110");
    parse!(dw, ".dw 100\t,0xf\t ,\t\t0b1");
    parse!(dw, ".dW 100\t0xf\t ,\t\t0b1", ".dW 100");
}

#[test]
fn test_equ() {
    use Rule::equ;
    parse!(equ, ".EQU label 0xf");
    parse!(equ, ".equ test 10");
    parse_err!(equ, ".eq rest 10");
    parse_err!(equ, ".equ test, 20");
    parse_err!(equ, ".equ rest, 0xff");
    parse_err!(equ, ".equ 0xff");
    parse_err!(equ, ".equ label");
}

#[test]
fn test_stack() {
    use Rule::stacksize;
    parse!(stacksize, "*STACKSIZE 16");
    parse!(stacksize, "*stacksize noset");
    parse_err!(stacksize, "*stacksize 17");
    parse_err!(stacksize, "*STACKSIZE48");
}

#[test]
fn test_inc() {
    use Rule::inc;
    parse!(inc, "INC R0");
    parse!(inc, "inc r2");
    parse!(inc, "inc R1");
    parse!(inc, "inc r3");
    parse_err!(inc, "inc 0xff");
    parse_err!(inc, "inc r4");
    parse_err!(inc, "INCR2");
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
    parse!(
        file,
        "#! mrasm\nHERE:    ;jump here\ninc r0;increase this\n\n"
    );
}
