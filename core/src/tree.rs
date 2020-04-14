pub type Name = String;

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Lit {
    Number(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Array(Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Null,
}

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Op {
    // binary
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Access,
    Index,
    // unary
    Neg,
    Deref,
    Not,
    Typeid,
    New,
    GcNew,
    Flatten,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Lambda(Vec<Name>, Box<Expr>),
    Id(Name),
    Group(Vec<Expr>),

    Assign(Op, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Unary(Op, Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

pub type Body = Vec<Stmt>;

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Stmt {
    // declaration statements
    Var(Name, Expr),
    Bind(Vec<Name>, Expr),
    Func(Name, Vec<Name>, Body),
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

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Case {
    Sth(Expr),
    Default(Body),
}

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Header {
    Package(Name),
    Using(Name),
    Import(Name, Option<Name>),
}

#[derive(Debug, PartialOrd, PartialEq)]
pub enum Entry {
    HeaderEntry(Header),
    StmtEntry(Stmt),
}

pub type Program = Vec<Entry>;
