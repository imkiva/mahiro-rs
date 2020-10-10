pub mod memory;
pub mod binfmt;
pub mod vm;

const NUM_GENERAL_REG: usize = 32;
const NUM_FLOAT_REG: usize = 32;
const NUM_RETURN_REG: usize = 8;
const NUM_ARGUMENT_REG: usize = 32;

type Offset = usize;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpReg {
    /// general purpose register, from r0 to r31
    R(u8),
    /// general purpose floating point register, from f0 to f31,
    F(u8),
    /// return values register, from x0 to x7
    X(u8),
    /// argument values register, from a0 to a31
    A(u8),
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum OpValue {
    Reg(OpReg),
    Imm(i64),
    ImmF(f64),
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Opcode {
    Nop,
    Mov(OpReg, OpValue),
    Push(OpValue),
    Pop(OpReg),
    Swap,

    Add(OpReg, OpValue, OpValue),
    Sub(OpReg, OpValue, OpValue),
    Mul(OpReg, OpValue, OpValue),
    Div(OpReg, OpValue, OpValue),
    Mod(OpReg, OpValue, OpValue),
    Neg(OpReg),
    LogicalNot(OpReg),
    LogicalAnd(OpReg, OpValue, OpValue),
    LogicalOr(OpReg, OpValue, OpValue),
    Xor(OpReg, OpValue, OpValue),
    BitwiseAnd(OpReg, OpValue, OpValue),
    BitwiseOr(OpReg, OpValue, OpValue),
    BitwiseNot(OpReg, OpValue, OpValue),
    ShiftLeft(OpReg, OpValue, OpValue),
    ShiftRight(OpReg, OpValue, OpValue),
    Compare(OpReg, OpValue, OpValue),

    Jump(Offset),
    JumpIfZero(OpReg, Offset),
    JumpIfNotZero(OpReg, Offset),
    JumpIfGreaterThanZero(OpReg, Offset),
    JumpIfLessThanZero(OpReg, Offset),
    Call(Offset),
    Return,

    Hlt,
}
