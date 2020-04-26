use std::fmt::{Display, Formatter};
use crate::syntax::tree::{AbsLoc, Program};
use crate::CompileResult;

mod ctx;
mod checker;

/// The semantic checker that checks:
/// - Redefinition of identifiers?
/// - Are all break/continue(s) inside loop statements?
pub struct Checker;

#[derive(Debug)]
pub struct CheckError {
    pub file: String,
    pub variant: CheckErrorVariant,
}

#[derive(Debug, Clone)]
pub enum CheckErrorVariant {
    RedefVar(Option<AbsLoc>, Option<AbsLoc>, String),
}

impl Display for CheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "In file {}: {}\n", self.file, self.variant)
    }
}

impl Display for CheckErrorVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Check Error: ")?;
        match self {
            CheckErrorVariant::RedefVar(_, _, name) =>
                write!(f, "redefinition of '{}'", name.as_str()),
        }
    }
}

impl Checker {
    pub fn check(input: Program) -> CompileResult<Program> {
        checker::checker_main(&input)?;
        Ok(input)
    }
}

impl CheckError {
    pub fn with_path(mut self, path: &str) -> Self {
        self.file = path.to_string();
        self
    }
}
