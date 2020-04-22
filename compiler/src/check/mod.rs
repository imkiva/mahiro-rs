use std::fmt::{Display, Formatter};
use crate::syntax::tree::Loc;

#[derive(Debug)]
pub struct CheckError {
    file: String,
    variant: CheckErrorVariant,
    loc: Option<Loc>,
}

#[derive(Debug)]
pub enum CheckErrorVariant {}

impl Display for CheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "In file {}", self.file)?;
        match self.loc {
            Some(loc) => write!(f, ":{}:{}", (loc.0).0, (loc.0).1)?,
            _ => (),
        };
        write!(f, "\n\t{:?}\n", self.variant)
    }
}

impl CheckError {
    pub fn with_path(mut self, path: &str) -> Self {
        self.file = path.to_string();
        self
    }
}
