use std::fmt::{Display, Formatter};
use crate::syntax::tree::{Loc, Program, Stmt, Entry, Header, Body};
use crate::CompileResult;
use crate::error::CompileError;
use crate::syntax::tree::Entry::*;
use crate::syntax::tree::Header::*;
use crate::syntax::tree::Stmt::*;

pub struct Desugar;

impl Desugar {
    pub fn desugar(input: Program) -> CompileResult<Program> {
        desugar_main(input)
    }
}

/// Converts sugar to normal expressions/statements.
/// Sugars are:
/// - VarList
/// - ForEach
/// - VarInit::Structured
/// - Header::Import(_, None)
/// - Expr::Ternary
/// - Expr::Question
/// - Expr::Group,
/// - Expr::Literal,
/// - Lit::Array
/// - Lit::Pair
fn desugar_main(input: Program) -> CompileResult<Program> {
    Ok(input)
}

