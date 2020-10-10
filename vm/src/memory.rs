use crate::{Offset, NUM_ARGUMENT_REG, NUM_FLOAT_REG, NUM_GENERAL_REG, NUM_RETURN_REG};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub enum Value {
    Int64(i64),
    UInt64(u64),
    Float(f64),
    Offset(Offset),
}

#[derive(Debug, Clone, Copy)]
pub struct GeneralRegisters {
    pub r: [Value; NUM_GENERAL_REG],
    pub f: [Value; NUM_FLOAT_REG],
}

#[derive(Debug, Clone, Copy)]
pub struct CallerRegisters {
    pub x: [Value; NUM_RETURN_REG],
    pub a: [Value; NUM_ARGUMENT_REG],
}

#[derive(Debug, Clone, Default)]
pub struct Stack {
    pub region: Vec<Value>,
}

impl Default for GeneralRegisters {
    fn default() -> Self {
        GeneralRegisters {
            r: [Value::Offset(0); NUM_GENERAL_REG],
            f: [Value::Offset(0); NUM_FLOAT_REG],
        }
    }
}

impl Default for CallerRegisters {
    fn default() -> Self {
        CallerRegisters {
            x: [Value::Offset(0); NUM_RETURN_REG],
            a: [Value::Offset(0); NUM_ARGUMENT_REG],
        }
    }
}
