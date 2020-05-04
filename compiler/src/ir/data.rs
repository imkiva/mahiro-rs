use crate::ir::cfg::CodeUnit;

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub name: String,
    pub argc: usize,
    pub code: CodeUnit,
}

#[derive(Debug, Clone)]
pub enum Field {
    MemberVar(String),
    MemberFunc(Function),
}

#[derive(Debug, Clone, Default)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}

impl Function {
    pub fn new() -> Self {
        Self::default()
    }
}
