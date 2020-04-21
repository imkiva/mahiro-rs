use std::fmt::{Display, Formatter};
use crate::syntax::tree::{Loc, Program};
use crate::CompileResult;
use crate::error::CompileError;

#[derive(Debug)]
pub struct DesugarError {
    file: String,
    variant: DesugarErrorVariant,
    loc: Option<Loc>,
}

#[derive(Debug)]
pub enum DesugarErrorVariant {}

impl Display for DesugarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "In file {}", self.file)?;
        match self.loc {
            Some(loc) => write!(f, ":{}:{}", (loc.0).0, (loc.0).1)?,
            _ => (),
        };
        write!(f, "\n\t{:?}\n", self.variant)
    }
}

impl DesugarError {
    pub fn with_path(mut self, path: &str) -> Self {
        self.file = path.to_string();
        self
    }
}

pub struct Desugar;

impl Desugar {
    pub fn desugar(input: Program) -> CompileResult<Program> {
        desugar_main(input).map_err(|e| CompileError::DesugarError(e))
    }
}

struct DesugarState {}

type DesugarMonad<S = DesugarState> = Result<S, DesugarError>;

/// Converts sugar to normal expressions/statements.
/// Sugars are:
/// - Stmt::VarList
/// - Stmt::ForEach
/// - VarInit::Structured
/// - Header::Import(_, Some(_))
/// - Expr::Ternary
/// - Expr::Question
/// - Expr::Group,
/// - Expr::Literal,
/// - Lit::Array
/// - Lit::Pair
fn desugar_main(input: Program) -> DesugarMonad<Program> {
    Ok(input)
}
