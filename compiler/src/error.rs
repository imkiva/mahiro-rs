use crate::syntax::parse::ParseError;
use crate::syntax::desugar::DesugarError;

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    DesugarError(DesugarError),
}
