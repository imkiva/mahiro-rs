use crate::parse::ParseError;

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
}
