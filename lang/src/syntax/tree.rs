use std::fmt::Display;

pub type Ident = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Unit,
  Int8,
  Int16,
  Int32,
  Int64,
  UInt8,
  UInt16,
  UInt32,
  UInt64,
  Float32,
  Float64,
  Char,
  Bool,
  String,
  SelfType,
  Generic(Box<Type>, Vec<Type>),
  User(Ident),
  Tuple(Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct Module {
  pub name: Ident,
  pub decl: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub struct FnSig {
  pub generic: Vec<GenericParam>,
  pub name: Ident,
  pub param: Vec<Param>,
  pub ret: Type,
  pub async_fn: bool,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
  pub sig: FnSig,
  pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
  pub generic: Vec<GenericParam>,
  pub name: Type,
  pub fields: Vec<Param>,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
  pub generic: Vec<GenericParam>,
  pub name: Type,
  pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct PatEnumVariant<F> {
  pub name: Ident,
  pub fields: Vec<F>,
}

pub type EnumVariant = PatEnumVariant<Ident>;
pub type LetPatEnumVariant = PatEnumVariant<LetPattern>;
pub type MatchPatEnumVariant = PatEnumVariant<MatchPattern>;

#[derive(Debug, Clone)]
pub struct ImplDecl {
  pub generic: Vec<GenericParam>,
  pub trait_name: Type,
  pub for_type: Option<Type>,
  pub fns: Vec<FnDecl>,
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
  pub generic: Vec<GenericParam>,
  pub name: Type,
  pub fns: Vec<FnSig>,
}

#[derive(Debug, Clone)]
pub enum Decl {
  Import(Vec<Ident>, Ident),
  Fn(FnDecl),
  Struct(StructDecl),
  Enum(EnumDecl),
  Impl(ImplDecl),
  Trait(TraitDecl),
}

#[derive(Debug, Clone)]
pub enum Stmt {
  Return(Option<Expr>),
  Let(LetPattern, Expr),
  Assign(AssignOp, Ident, Expr),
  Expr(Expr),
  ResultExpr(Expr),
  Break(Option<Expr>),
  Continue,
}

#[derive(Debug, Clone)]
pub enum LetPattern {
  Id(Param),
  Enum(LetPatEnumVariant),
  Tuple(Vec<LetPattern>),
  Wildcard,
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
  Id(Param),
  Enum(MatchPatEnumVariant),
  Tuple(Vec<MatchPattern>),
  Lit(Lit),
  Wildcard,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
  pub pat: MatchPattern,
  pub action: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Param {
  pub name: Ident,
  pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
  pub name: Ident,
  pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone)]
pub enum Constraint {
  MustImpl(Type),
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
  LogicalOr,
  LogicalAnd,
  Gt,
  Lt,
  Ge,
  Le,
  Eq,
  Ne,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Not,
  Positive,
  Negative,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
  Assign,
  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  ModAssign,
}

#[derive(Debug, Clone)]
pub enum Lit {
  LitUnit,
  LitInt(i32),
  LitFloat(f32),
  LitChar(char),
  LitBool(bool),
  LitString(String),
  LitArray(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
  If(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>),
  While(Box<Expr>, Vec<Stmt>),
  For(Ident, Box<Expr>, Vec<Stmt>),
  Match(Box<Expr>, Vec<MatchCase>),
  Binary(BinaryOp, Box<Expr>, Box<Expr>),
  Unary(UnaryOp, Box<Expr>),
  Lit(Lit),
  Tuple(Vec<Expr>),
  Lambda(Vec<Param>, Vec<Stmt>),
  Id(Ident),
  Apply(Box<Expr>, Vec<Expr>),
  MemberApply(Box<Expr>, Ident, Vec<Expr>),
  Member(Box<Expr>, Ident),
  Index(Box<Expr>, Box<Expr>),
  TupleIndex(Box<Expr>, usize),
}

impl Type {
  pub fn from_simple_type_name(s: &str) -> Type {
    match s {
      "Unit" => Type::Unit,
      "Int8" => Type::Int8,
      "Int16" => Type::Int16,
      "Int32" => Type::Int32,
      "Int64" => Type::Int64,
      "UInt8" => Type::UInt8,
      "UInt16" => Type::UInt16,
      "UInt32" => Type::UInt32,
      "UInt64" => Type::UInt64,
      "Float32" => Type::Float32,
      "Float64" => Type::Float64,
      "Char" => Type::Char,
      "Bool" => Type::Bool,
      "String" => Type::String,
      "Self" => Type::SelfType,
      _ => Type::User(s.to_string()),
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn comma_sep(v: &Vec<Type>) -> String {
      v.iter()
        .map(|t| format!("{}", t))
        .collect::<Vec<_>>()
        .join(", ")
    }

    match self {
      Type::Unit => write!(f, "Unit"),
      Type::Int8 => write!(f, "Int8"),
      Type::Int16 => write!(f, "Int16"),
      Type::Int32 => write!(f, "Int32"),
      Type::Int64 => write!(f, "Int64"),
      Type::UInt8 => write!(f, "UInt8"),
      Type::UInt16 => write!(f, "UInt16"),
      Type::UInt32 => write!(f, "UInt32"),
      Type::UInt64 => write!(f, "UInt64"),
      Type::Float32 => write!(f, "Float32"),
      Type::Float64 => write!(f, "Float64"),
      Type::Char => write!(f, "Char"),
      Type::Bool => write!(f, "Bool"),
      Type::String => write!(f, "String"),
      Type::SelfType => write!(f, "Self"),
      Type::Generic(ctor, param) => write!(f, "{}<{}>", ctor.as_ref(), comma_sep(param),),
      Type::User(ty) => write!(f, "{}", ty),
      Type::Tuple(t) => write!(f, "({})", comma_sep(t),),
    }
  }
}

impl Display for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BinaryOp::LogicalOr => write!(f, "||"),
      BinaryOp::LogicalAnd => write!(f, "&&"),
      BinaryOp::Gt => write!(f, ">"),
      BinaryOp::Lt => write!(f, "<"),
      BinaryOp::Ge => write!(f, ">="),
      BinaryOp::Le => write!(f, "<="),
      BinaryOp::Eq => write!(f, "=="),
      BinaryOp::Ne => write!(f, "!="),
      BinaryOp::Add => write!(f, "+"),
      BinaryOp::Sub => write!(f, "-"),
      BinaryOp::Mul => write!(f, "*"),
      BinaryOp::Div => write!(f, "/"),
      BinaryOp::Mod => write!(f, "%"),
    }
  }
}

impl Display for UnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      UnaryOp::Not => write!(f, "!"),
      UnaryOp::Positive => write!(f, "+"),
      UnaryOp::Negative => write!(f, "-"),
    }
  }
}

impl Display for AssignOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AssignOp::Assign => write!(f, "="),
      AssignOp::AddAssign => write!(f, "+="),
      AssignOp::SubAssign => write!(f, "-="),
      AssignOp::MulAssign => write!(f, "*="),
      AssignOp::DivAssign => write!(f, "/="),
      AssignOp::ModAssign => write!(f, "%="),
    }
  }
}
