use crate::tree::*;

use pest::Parser;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CsParser;

impl Into<Expr> for Pair<'_, Rule> {
    fn into(self) -> Expr {
        unimplemented!()
    }
}

impl Into<Stmt> for Pair<'_, Rule> {
    fn into(self) -> Stmt {
        unimplemented!()
    }
}

impl CsParser {
}
