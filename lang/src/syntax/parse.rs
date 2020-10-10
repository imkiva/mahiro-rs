use crate::error::CompileError;
use crate::CompileResult;
use pest::error::Error;
use pest::error::ErrorVariant;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::collections::VecDeque;
use crate::syntax::tree::Program;

pub type ParseErrorVariant = ErrorVariant<Rule>;
pub type ParseError = Error<Rule>;

#[derive(Parser)]
#[grammar = "syntax/grammar.pest"]
pub struct MahiroParser;

impl MahiroParser {
    pub fn ast(src: &str) -> CompileResult<Program> {
        Ok(())
    }
}
