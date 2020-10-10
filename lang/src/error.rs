use crate::syntax::parse::{ParseError, ParseErrorVariant};
use crate::syntax::tree::Loc;

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
}

impl CompileError {
    pub fn error_message(self, path: &str, input: &str) -> String {
        match self {
            CompileError::ParseError(err) => format!("{}", err),
        }
    }
}
