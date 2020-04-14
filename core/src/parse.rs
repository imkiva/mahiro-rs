use crate::tree::*;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error;
use pest::error::ErrorVariant;
use crate::tree::Entry::{HeaderEntry, StmtEntry};
use crate::tree::Header::{Using, Import, Package};

pub type ParseErrorVariant = ErrorVariant<Rule>;
pub type ParseError = Error<Rule>;

#[derive(Debug)]
pub struct CompileError(pub ParseError);

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CsParser;

/// macro that extract the first child of a node
macro_rules! fst {
    ($e:expr) => (($e).into_inner().into_iter().next());
}

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

impl ParseTo<Header> for Pair<'_, Rule> {
    fn parse_to(self) -> Header {
        let child = self.into_inner().into_iter().next().unwrap();
        match child.as_rule() {
            Rule::using_decl => Using(fst!(child).unwrap().parse_to()),
            Rule::import_decl => {
                let mut iter = child.into_inner().into_iter();
                Import(iter.next().unwrap().parse_to(),
                       iter.next().map(|id| id.parse_to()))
            }
            Rule::package_decl => Package(fst!(child).unwrap().parse_to()),
            _ => unreachable!(),
        }
    }
}

impl ParseTo<String> for Pair<'_, Rule> {
    fn parse_to(self) -> String {
        match self.as_rule() {
            Rule::mod_name => self.into_inner().into_iter()
                .map(|id| id.parse_to())
                .collect::<Vec<String>>()
                .join("."),

            Rule::id => self.as_str().to_owned(),

            _ => unimplemented!()
        }
    }
}

impl ParseTo<Program> for Pairs<'_, Rule> {
    fn parse_to(self) -> Program {
        self.into_iter()
            .flat_map(|item| item.into_inner())
            .filter_map(|node| match node.as_rule() {
                Rule::header => Some(HeaderEntry(node.parse_to())),
                Rule::stmt => Some(StmtEntry(node.parse_to())),
                Rule::EOI => None,
                _ => unreachable!(),
            })
            .collect()
    }
}

impl CsParser {
    pub fn ast(input: &str) -> CompileResult<Program> {
        let parse = CsParser::parse(Rule::program, input);
        let pairs = parse.map_err(|e| CompileError(e))?;
        Ok(pairs.parse_to())
    }
}
