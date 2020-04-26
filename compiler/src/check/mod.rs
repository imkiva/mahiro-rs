use std::fmt::{Display, Formatter};
use crate::syntax::tree::{AbsLoc, Program};
use crate::CompileResult;

mod ctx;
mod checker;

#[cfg(test)]
mod tests;

/// The semantic checker that checks:
/// - Redefinition of identifiers?
/// - Are all break/continue(s) inside loop statements?
pub struct Checker;

#[derive(Debug)]
pub struct CheckError {
    pub file: Option<String>,
    pub variant: CheckErrorVariant,
}

#[derive(Debug, Clone)]
pub enum CheckErrorVariant {
    Redefinition(Option<AbsLoc>, Option<AbsLoc>, String),
    DanglingLoopControl(Option<AbsLoc>, String),
}

impl Display for CheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "In file {}: {}\n",
               match &self.file {
                   Some(f) => f.as_str(),
                   _ => "<unknown-source>"
               },
               self.variant)
    }
}

impl Display for CheckErrorVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Check Error: ")?;
        match self {
            CheckErrorVariant::Redefinition(_, _, name) =>
                write!(f, "redefinition of '{}'", name.as_str()),
            CheckErrorVariant::DanglingLoopControl(_, code) =>
                write!(f, "'{}' should be inside loop statement", code.as_str()),
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
    pub fn new(variant: CheckErrorVariant) -> Self {
        CheckError {
            file: None,
            variant,
        }
    }

    pub fn with_path(mut self, path: &str) -> Self {
        self.file = Some(path.to_string());
        self
    }
}
