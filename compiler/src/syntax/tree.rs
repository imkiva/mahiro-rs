use std::cmp::Ordering;

pub type Line = usize;
pub type Col = usize;
pub type Pos = (Line, Col);
pub type Loc = (Pos, Pos);
pub type AbsLoc = (usize, usize);

#[derive(Debug, Clone)]
pub struct Ident {
    /// Text of the identifier
    pub text: String,
    /// Human-readable location of the text
    pub loc: Option<Loc>,
    /// Internal representation of location in pest-rs,
    /// we need this for better error reporting.
    pub abs_loc: Option<AbsLoc>,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.text.eq(&other.text)
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.text.partial_cmp(&other.text)
    }
}

impl Ident {
    pub fn new(span: pest::Span, text: &str) -> Self {
        Ident {
            text: text.to_string(),
            loc: Some((span.start_pos().line_col(),
                       span.end_pos().line_col())),
            abs_loc: Some((span.start(), span.end())),
        }
    }

    pub fn only(text: &str) -> Self {
        Ident {
            text: text.to_string(),
            loc: None,
            abs_loc: None,
        }
    }

    pub fn same_loc(&self, text: &str) -> Self {
        Ident {
            text: text.to_string(),
            loc: self.loc.clone(),
            abs_loc: self.abs_loc.clone(),
        }
    }
}

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
    Normal(Ident),
    Varargs(Ident),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Expr {
    Literal(Lit),
    Lambda(Option<Vec<Ident>>, Vec<Param>, Box<Expr>),
    Alloc(Box<Expr>, Vec<Expr>),
    Id(Ident),
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
    Flatten,
}

pub type Body = Vec<Stmt>;

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Stmt {
    // declaration statements
    Var(VarInit),
    VarList(Vec<VarInit>),
    Func(Ident, Vec<Param>, Body),
    Namespace(Ident, Body),
    Struct(Ident, Option<Expr>, Body),
    Block(Body),

    // common statements
    Return(Option<Expr>),
    Throw(Expr),
    Try(Body, Ident, Body),
    If(Expr, Body, Option<Body>),
    Switch(Expr, Vec<Case>),
    While(Expr, Body),
    Loop(Option<Expr>, Body),
    For(Ident, Expr, Expr, Expr, Body),
    ForEach(Ident, Expr, Body),
    Break,
    Continue,

    // expression statement
    ExprStmt(Expr),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum VarInit {
    Simple(Ident, Expr),
    Structured(Vec<Ident>, Expr),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Case {
    Sth(Expr, Body),
    Dft(Body),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Header {
    Package(Ident),
    Using(Ident),
    Import(Ident, Option<Ident>),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Entry {
    HeaderEntry(Header),
    StmtEntry(Stmt),
}

pub type Program = Vec<Entry>;
