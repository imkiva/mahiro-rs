use crate::ir::cfg::{BasicBlock, BBID, BBUnderlyingID};
use crate::ir::{IR, LocalIndex};

pub struct MacroAssembler {
    editing: BasicBlock,
    locals_map: Vec<String>
}

impl MacroAssembler {
    pub fn new() -> Self {
        Self {
            editing: BasicBlock {
                ir: Vec::new(),
                id: 0,
                succ: BBID::This,
                pred: BBID::This,
            },
            locals_map: Vec::new(),
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

    pub fn new_local(&mut self, name: &str) -> LocalIndex {
        let idx = self.locals_map.len() as LocalIndex;
        self.locals_map.push(name.to_owned());
        idx
    }
}
