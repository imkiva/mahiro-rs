extern crate pest;
#[macro_use]
extern crate pest_derive;

mod capi;
pub(crate) mod check;
pub mod error;
pub(crate) mod ir;
pub(crate) mod syntax;

use crate::check::Checker;
use crate::error::CompileError;
use crate::syntax::desugar::Desugar;
use crate::syntax::optimize::{OptimizeLevel, Optimizer};
use crate::syntax::parse::CsParser;
use crate::syntax::tree::Program;

pub struct Compiler;

pub type CompileResult<S> = Result<S, CompileError>;

impl Compiler {
    pub fn compile_to_ast(src: &str) -> CompileResult<Program> {
        let program = CsParser::ast(src)?;
        let program = Desugar::desugar(program)?;
        let program = Checker::check(program)?;
        let program = Optimizer::run(program, OptimizeLevel::Basic)?;
        Ok(program)
    }
}
