use crate::tree::*;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error;
use pest::error::ErrorVariant;

pub type ParseErrorVariant = ErrorVariant<Rule>;
pub type ParseError = Error<Rule>;

#[derive(Debug)]
pub struct CompileError(pub ParseError);

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CsParser;

trait ParseTo<T> {
    fn parse_to(self) -> T;
}

impl ParseTo<Expr> for Pair<'_, Rule> {
    fn parse_to(self) -> Expr {
        unimplemented!()
    }
}

impl ParseTo<Stmt> for Pair<'_, Rule> {
    fn parse_to(self) -> Stmt {
        unimplemented!()
    }
}

impl ParseTo<Program> for Pairs<'_, Rule> {
    fn parse_to(self) -> Program {
        unimplemented!()
    }
}

impl CsParser {
    pub fn ast(input: &str) -> CompileResult<Program> {
        let parse = CsParser::parse(Rule::program, input);
        let pairs = parse.map_err(|e| CompileError(e))?;
        Ok(pairs.parse_to())
    }
}
