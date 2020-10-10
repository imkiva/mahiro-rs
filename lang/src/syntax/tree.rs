use std::cmp::Ordering;
use std::hash::Hash;

#[derive(Clone, Copy, Debug)]
pub enum Loc {
    Injected,
    InSource(usize, usize),
}

pub(crate) trait ToLoc {
    fn to_loc(&self) -> Loc;
}

#[derive(Debug, Clone)]
pub struct Ident {
    /// Text of the identifier
    pub text: String,
    /// Internal representation of location in pest-rs,
    /// we need this for better error reporting.
    pub abs_loc: Loc,
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
    Literal(Loc, Lit),
    Lambda(Loc, Option<Vec<Ident>>, Vec<Param>, Box<Expr>),
    Alloc(Loc, Box<Expr>, Vec<Expr>),
    Id(Ident),
    Group(Loc, Vec<Expr>),

    Assign(Loc, Op, Box<Expr>, Box<Expr>),
    Apply(Loc, Box<Expr>, Vec<Expr>),
    Unary(Loc, Op, Box<Expr>),
    Binary(Loc, Op, Box<Expr>, Box<Expr>),

    // cond ? if_true : if_false
    Ternary(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    // value ? if_null
    Question(Loc, Box<Expr>, Box<Expr>),
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
    Return(Loc, Option<Expr>),
    Throw(Expr),
    Try(Body, Ident, Body),
    If(Expr, Body, Option<Body>),
    Switch(Expr, Vec<Case>),
    While(Expr, Body),
    Loop(Option<Expr>, Body),
    For(Ident, Expr, Expr, Expr, Body),
    ForEach(Ident, Expr, Body),
    Break(Ident),
    Continue(Ident),

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

impl<'a> ToLoc for pest::Span<'a> {
    fn to_loc(&self) -> Loc {
        Loc::InSource(self.start(), self.end())
    }
}

impl PartialEq for Loc {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl PartialOrd for Loc {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

impl Eq for Loc {}

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

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text.hash(state)
    }
}

impl ToLoc for Expr {
    fn to_loc(&self) -> Loc {
        match self {
            Expr::Literal(loc, _) => loc.clone(),
            Expr::Lambda(loc, _, _, _) => loc.clone(),
            Expr::Alloc(loc, _, _) => loc.clone(),
            Expr::Id(id) => id.abs_loc.clone(),
            Expr::Group(loc, _) => loc.clone(),
            Expr::Assign(loc, _, _, _) => loc.clone(),
            Expr::Apply(loc, _, _) => loc.clone(),
            Expr::Unary(loc, _, _) => loc.clone(),
            Expr::Binary(loc, _, _, _) => loc.clone(),
            Expr::Ternary(loc, _, _, _) => loc.clone(),
            Expr::Question(loc, _, _) => loc.clone(),
        }
    }
}

impl ToLoc for Param {
    fn to_loc(&self) -> Loc {
        match self {
            Param::Normal(id) | Param::Varargs(id) => id.to_loc(),
        }
    }
}

impl ToLoc for Ident {
    fn to_loc(&self) -> Loc {
        self.abs_loc.clone()
    }
}

impl Ident {
    pub fn new(span: pest::Span, text: &str) -> Self {
        Ident {
            text: text.to_string(),
            abs_loc: span.to_loc(),
        }
    }

    pub fn only(text: &str) -> Self {
        Ident {
            text: text.to_string(),
            abs_loc: Loc::Injected,
        }
    }

    pub fn same_loc(&self, text: &str) -> Self {
        Ident {
            text: text.to_string(),
            abs_loc: self.abs_loc.clone(),
        }
    }
}
