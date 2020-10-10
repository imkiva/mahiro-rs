extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::error::CompileError;
use crate::syntax::parse::MahiroParser;
use crate::syntax::tree::Program;

pub mod syntax;
pub mod error;

pub struct Compiler;

pub type CompileResult<S> = Result<S, CompileError>;

impl Compiler {
    pub fn compile_to_ast(src: &str) -> CompileResult<Program> {
        let program = MahiroParser::ast(src)?;
        Ok(program)
    }
}
