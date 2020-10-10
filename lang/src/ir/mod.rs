pub mod asm;
pub mod cfg;
pub mod data;
pub mod translate;

pub type LocalIndex = u16;
pub type PoolIndex = u16;
use cfg::BBID;

#[derive(Debug, Clone)]
pub enum IRCmp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    InstanceOf,
}

#[derive(Debug, Clone)]
pub enum IRUnary {
    Neg,
    Not,
    TypeId,
}

#[derive(Debug, Clone)]
pub enum IRBinary {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum IR {
    Nop,
    Cmp(IRCmp),
    Unary(IRUnary),
    Binary(IRBinary),

    // Duplicate stack top element
    Dup,
    // Swap the top 2 elements of the stack
    Swap,
    // Drop the stack top
    Pop,
    // store stack top to local variable
    LocalStore(LocalIndex),
    // load local variable to stack
    LocalLoad(LocalIndex),
    // store stack top to array
    ArrayStore,
    // load from array to stack top
    ArrayLoad,
    // store stack top to object field
    FieldStore(PoolIndex),
    // load object field to stack top
    FieldLoad(PoolIndex),

    // push null to stack
    ConstNull,
    // push const number (converted from i16) to stack
    Const16(i16),
    // push const number (converted from i32) to stack
    Const32(i32),
    // push const number to stack
    ConstNum(f64),
    // load string constant to stack
    ConstStringLoad(PoolIndex),

    // jump if stack top is true
    IfTrue(BBID),
    // jump if stack top is false
    IfFalse(BBID),
    // jump if stack top is null
    IfNull(BBID),
    // jump if stack top is not null
    IfNonNull(BBID),
    // jump if stack top are 0
    IFEq(BBID),
    // jump if stack top are not 0
    IfNe(BBID),
    // jump if stack top < 0
    IfLt(BBID),
    // jump if stack top <= 0
    IfLe(BBID),
    // jump if stack top > 0
    IfGt(BBID),
    // jump if stack top >= 0
    IfGe(BBID),
    // jump unconditionally
    Jump(BBID),

    // return from function
    ReturnVoid,
    // return from function with stack top
    Return,

    // Allocate new object
    New,

    // Resolve symbol by name
    Resolve(PoolIndex),
}
