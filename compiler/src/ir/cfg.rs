use crate::ir::IR;
use crate::syntax::tree::Program;

pub type BBUnderlyingID = u32;

#[derive(Debug, Clone)]
pub enum BBID {
    This,
    That(BBUnderlyingID),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BBUnderlyingID,
    pub ir: Vec<IR>,
}

#[derive(Debug, Clone)]
pub struct CodeUnit {
    pub blocks: Vec<BasicBlock>,
    pub children: Vec<CodeUnit>,
}

impl CodeUnit {
    fn new() -> Self {
        Self {
            blocks: Default::default(),
            children: Default::default(),
        }
    }

    pub fn from(prog: Program) -> Self {
        Self::new()
    }
}
