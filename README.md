# Minirechner2a ASM parser

Crate to parse assembly programs into a usable structure.
Supports optional formatting using the `formatting` flag.

## Example

```rust
use mr2a_asm_parser::parser::AsmParser;

fn main() {
    let asm = r##"#! mrasm

        .ORG 0

        CLR R0
    LOOP:
        ST (0xFF), R0
        ST (0xF0), R0
        INC R0
        JR LOOP
    "##;

    let parsed = AsmParser::parse(asm);

    match parsed {
        Ok(parsed) => {
            #[cfg(feature = "formatting")]
            println!("{}", parsed);
            #[cfg(not(feature = "formatting"))]
            println!("{:?}", parsed);
        },
        Err(e) => panic!("Whoooops {}", e),
    }
}
```
