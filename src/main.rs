use mr2a_asm_parser::parser::AsmParser;

use std::env::args;
use std::fs::read_to_string;

enum Return {
    Valid,
    Invalid,
    FileNotFound,
}

fn run() -> Return {
    let args = args().skip(1);
    let mut ret = Return::Valid;

    for arg in args {
        match read_to_string(arg.clone()) {
            Ok(content) => {
                let parsed = AsmParser::parse(&content);
                match parsed {
                    Ok(asm) => {
                        #[cfg(feature = "formatting")]
                        println!("{}", asm);
                        ret = Return::Valid;
                    }
                    Err(e) => {
                        eprintln!("\n--> {}:", arg);
                        eprintln!("{}\n", e);
                        ret = Return::Invalid;
                    }
                }
            }
            Err(_) => {
                eprintln!("'{}': Reading failed", arg);
                ret = Return::FileNotFound;
            }
        }
    }

    ret
}

fn main() {
    std::process::exit(match run() {
        Return::Valid => 0,
        Return::Invalid => 1,
        Return::FileNotFound => 2,
    })
}
