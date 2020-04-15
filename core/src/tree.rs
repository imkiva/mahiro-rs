pub type Name = String;

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Lit {
    Number(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Array(Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Null,
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Param {
    Normal(Name),
    Varargs(Name),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Expr {
    Literal(Lit),
    Lambda(Vec<Param>, Box<Expr>),
    Id(Name),
    Group(Vec<Expr>),

    Assign(Op, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Unary(Op, Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),

    // cond ? if_true : if_false
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    // cond ? if_false
    Question(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum OpFix {
    Prefix,
    Postfix,
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Op {
    // binary
    // also unary positive
    Add,
    // also unary negative
    Sub,
    // also unary deref
    Mul,
    Div,
    Pow,
    Mod,
    Assign,
    AddAss,
    SubAss,
    MulAss,
    DivAss,
    PowAss,
    ModAss,
    And,
    Or,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Access,
    Index,
    // unary
    Not,
    Typeid,
    New,
    GcNew,
    Flatten,
    Inc(OpFix),
    Dec(OpFix),
}

pub type Body = Vec<Stmt>;

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Stmt {
    // declaration statements
    // var x = 1
    Var(Name, Expr),
    // var a = 1, b = 2, c = 3, ...
    VarList(Vec<(Name, Expr)>),
    Bind(Vec<Name>, Expr),
    Func(Name, Vec<Param>, Body),
    Namespace(Name, Body),
    Struct(Name, Option<Name>, Body),
    Block(Body),

    // common statements
    Return(Option<Expr>),
    Throw(Expr),
    Try(Body, Name, Body),
    If(Expr, Body, Option<Body>),
    Switch(Expr, Vec<Case>),
    While(Expr, Body),
    Loop(Option<Expr>, Body),
    For(Name, Expr, Expr, Expr, Body),
    ForEach(Name, Expr, Body),
    Break,
    Continue,

    // expression statement
    ExprStmt(Expr),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Case {
    Sth(Expr),
    Default(Body),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Header {
    Package(Name),
    Using(Name),
    Import(Name, Option<Name>),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Entry {
    HeaderEntry(Header),
    StmtEntry(Stmt),
}

pub type Program = Vec<Entry>;
