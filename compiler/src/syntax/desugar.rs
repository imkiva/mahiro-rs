use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct DesugarError {
    file: String,
    variant: DesugarErrorVariant,
}

#[derive(Debug)]
pub enum DesugarErrorVariant {}

struct State {}

type Monad = Result<State, DesugarError>;

impl Display for DesugarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "In file {}\n\t{:?}", self.file, self.variant)
    }
}

impl DesugarError {
    pub fn with_path(mut self, path: &str) -> Self {
        self.file = path.to_string();
        self
    }
}
