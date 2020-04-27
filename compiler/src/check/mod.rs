use std::fmt::{Display, Formatter};
use crate::syntax::tree::{Loc, Program};
use crate::CompileResult;
use crate::check::infer_check::Type;

mod context;
mod check;
mod infer_check;

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
    Redefinition(Loc, Loc, String),
    DanglingLoopControl(Loc, String),
    BottomTypedExpr(Loc),
    TypeMismatch(Loc, Option<Type>, Type),
    ArgcMismatch(Loc, usize, usize),
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
            CheckErrorVariant::BottomTypedExpr(_) =>
                write!(f, "the type of expression cannot be void"),
            CheckErrorVariant::TypeMismatch(_, expected, actual) =>
                match expected {
                    Some(expected) =>
                        write!(f, "expected type '{:?}', but got '{:?}'", expected, actual),
                    _ => write!(f, "unexpected type '{:?}'", actual)
                }

            CheckErrorVariant::ArgcMismatch(_, expected, actual) =>
                write!(f, "expected {:?} {} , but provided {:?}",
                       expected,
                       if *expected <= 1 { "argument" } else { "arguments" },
                       actual),
        }
    }
}

impl Checker {
    pub fn check(input: Program) -> CompileResult<Program> {
        check::checker_main(&input)?;
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
