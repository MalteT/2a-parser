//! # Minirechner2a ASM parser
//!
//! Crate to parse assembly programs into a usable [structure](Asm).

pub mod asm;
pub mod parser;

#[cfg(test)]
mod tests {
    use super::parser::AsmParser;

    #[test]
    fn test_all_programs() -> Result<(), failure::Error> {
        use std::fs;
        use std::io::Read;

        for entry in fs::read_dir("./programs")? {
            let entry = entry?;
            if entry.file_type()?.is_file() {
                println!("{:?}", entry.file_name());
                let mut f = fs::File::open(entry.path())?;
                let mut s = String::new();
                f.read_to_string(&mut s)?;
                match AsmParser::parse(&s) {
                    Ok(_) => {}
                    Err(e) => {
                        println!("{}", e);
                        return Err(e.into());
                    }
                }
            }
        }
        Ok(())
    }
}
