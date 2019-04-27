use asm_parser::parser::AsmParser;

const PROGRAM: &str = include_str!("../programs/03-SS17-5-a2c.asm");

fn main() {
    let parsed = AsmParser::parse(PROGRAM);

    match parsed {
        Ok(parsed) => {
            let s = format!("{}", parsed);
            println!("{}", s);
        }
        Err(e) => println!("{}", e),
    }
}
