use crate::syntax::parse::ParseError;
use crate::check::CheckError;

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    CheckError(CheckError),
}
