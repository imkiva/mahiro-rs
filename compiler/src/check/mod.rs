use std::fmt::{Display, Formatter};
use crate::syntax::tree::AbsLoc;

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

impl CheckError {
    pub fn with_path(mut self, path: &str) -> Self {
        self.file = path.to_string();
        self
    }
}
