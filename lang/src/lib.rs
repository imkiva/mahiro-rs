#[macro_use]
extern crate lalrpop_util;

pub mod syntax;
pub mod error;

pub struct Compiler;
pub enum CompileError {
    ParseError,
}

pub type CompileResult<S> = Result<S, CompileError>;

impl Compiler {
}
