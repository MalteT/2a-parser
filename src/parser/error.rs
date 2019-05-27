use failure::Fail;
use pest::error::Error as PestError;

use std::fmt;

use super::Rule;

#[derive(Debug, Fail)]
pub enum ParserError {
    //#[fail(display = "failed to parse input \n{}", _0)]
    InvalidSyntax(#[fail(reason)] PestError<Rule>),
}

macro_rules! map {
    ( $error:expr; $( $($rule:expr),* => $str:expr );* ) => {
        {
            use pest::error::ErrorVariant as EV;

            fn match_until_found(e: &PestError<Rule>) -> String {
                match &e.variant {
                    EV::ParsingError{ negatives: _, positives } => {
                        $(
                            let mut contains_all = true;
                            let slice = [ $($rule,)* ];
                            for el in &slice {
                                if ! positives.contains(el) {
                                    contains_all = false;
                                    break;
                                }
                            }
                            if contains_all {
                                return $str.into();
                            }
                        )*
                        format!("Did not match these: {:?}", positives)
                    },
                    EV::CustomError{ message } => return message.clone(),
                }
            }
            let mut e = $error;
            let s = match_until_found(&e);
            let variant = EV::CustomError { message: s };
            e.variant = variant;
            e
        }
    }
}

impl From<PestError<Rule>> for ParserError {
    fn from(e: PestError<Rule>) -> Self {
        println!("{:?}", e);
        use Rule::*;
        // TODO: More of these helpful messages!
        let e = map! { e;
            header => "All source files have to begin with '#! mrasm'";
            oparen => "Expected an openining parenthesis '('";
            cparen => "Expected a closing parenthesis ')'";
            source => "Expected a general source (i.e. 'R0', '(R1+)', '((R0+))', '(LABEL)', '0x34', etc)";
            register => "Expected a register ('R0' - 'R3')";
            constant => "Expected a number between 0 and 255. (i.e. '0xF0', '0b110', '13')";
            constant_bin => "Expected a binary number between 0 and 255. (i.e. '0b110')";
            EOI, instruction, comment, label => "Typo in instruction? Or missing colon after label?"
        };
        ParserError::InvalidSyntax(e)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        println!("{:#?}", self);
        match self {
            ParserError::InvalidSyntax(inner) => write!(f, "{}", inner),
        }
    }
}
