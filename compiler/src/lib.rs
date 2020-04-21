extern crate pest;
#[macro_use]
extern crate pest_derive;

pub(crate) mod syntax;
pub mod error;
mod capi;

use crate::error::CompileError;
use crate::syntax::tree::Program;
use crate::syntax::parse::CsParser;
use crate::syntax::desugar::Desugar;

pub struct Compiler;

pub type CompileResult<S> = Result<S, CompileError>;

impl Compiler {
    pub fn compile_to_ast(src: &str) -> CompileResult<Program> {
        let program = CsParser::ast(src)?;
        let program = Desugar::desugar(program)?;
        Ok(program)
    }
}
