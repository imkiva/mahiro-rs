use std::collections::VecDeque;

use crate::{
  memory::{CallerRegisters, GeneralRegisters, Stack, Value},
  Opcode,
};

#[derive(Debug, Clone, Default)]
pub struct Frame {
  pub regs: GeneralRegisters,
}

#[derive(Debug)]
pub struct Executor<'a> {
  stack: &'a Stack,
  frame: &'a Frame,
  caller_regs: &'a CallerRegisters,
  code: &'a Vec<Opcode>,
}

#[derive(Debug, Clone)]
pub struct MahiroVM {
  stack: Stack,
  frame: VecDeque<Frame>,
  caller_regs: CallerRegisters,
  exception: Option<Value>,
}

impl MahiroVM {
  pub fn new() -> MahiroVM {
    MahiroVM {
      stack: Default::default(),
      frame: Default::default(),
      caller_regs: Default::default(),
      exception: Option::None,
    }
  }

  fn push(&mut self, v: Value) {
    self.stack.region.push(v)
  }

  fn pop(&mut self) -> Value {
    match self.stack.region.pop() {
      Some(v) => v,
      None => {
        self.exception = Some(Value::Offset(0));
        Value::Offset(0)
      }
    }
  }
}
