use crate::ir::cfg::CodeUnit;

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub name: String,
    pub argc: usize,
    pub code: CodeUnit,
}

impl Function {
    pub fn new() -> Self {
        Self::default()
    }
}


