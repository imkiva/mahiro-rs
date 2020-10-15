#[macro_use]
extern crate lalrpop_util;

use crate::syntax::{
  parser::{MahiroParseError, MahiroParser},
  tree::Module,
};

pub mod error;
pub mod sugar;
pub mod syntax;

pub struct Compiler;

#[derive(Debug, Clone)]
pub enum CompileError {
  ParseError(MahiroParseError),
}

pub type CompileResult<S> = std::result::Result<S, CompileError>;

impl Compiler {
  pub fn compile(src: &str) -> CompileResult<Module> {
    let m = MahiroParser::parse_module(src)?;
    Ok(m)
  }
}
