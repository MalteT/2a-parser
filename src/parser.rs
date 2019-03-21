use pest_derive::Parser;

#[cfg(test)]
mod tests;

#[derive(Parser)]
#[grammar = "../mrasm.pest"]
pub struct AsmParser;
