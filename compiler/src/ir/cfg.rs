use crate::ir::IR;
use crate::syntax::tree::{Program, Stmt};

pub type BBUnderlyingID = u32;

#[derive(Debug, Clone, Copy)]
pub enum BBID {
    This,
    That(BBUnderlyingID),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub ir: Vec<IR>,
    pub id: BBUnderlyingID,
    pub succ: BBID,
    pub pred: BBID,
}

#[derive(Debug, Clone, Default)]
pub struct CodeUnit {
    pub locals: usize,
    pub blocks: Vec<BasicBlock>,
    pub children: Vec<CodeUnit>,
}

impl CodeUnit {
    pub fn new() -> Self {
        Self::default()
    }
}
