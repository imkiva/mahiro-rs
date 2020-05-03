use crate::ir::IR;
use crate::syntax::tree::Program;

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

struct MacroAssembler {
    editing: BasicBlock,
}

impl MacroAssembler {
    pub fn new() -> Self {
        Self {
            editing: BasicBlock {
                ir: Vec::new(),
                id: 0,
                succ: BBID::This,
                pred: BBID::This
            }
        }
    }

    pub fn with_id(mut self, id: BBUnderlyingID) -> Self {
        self.editing.id = id;
        self
    }

    pub fn with_pred(mut self, block: BBID) -> Self {
        self.editing.pred = block;
        self
    }

    pub fn with_succ(mut self, block: BBID) -> Self {
        self.editing.succ = block;
        self
    }

    pub fn emit(&mut self, ir: IR) {
        self.editing.ir.push(ir);
    }
}

