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
pub struct EnumVariant {
  pub name: Ident,
  pub fields: Vec<Ident>,
}

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
  Break(Option<Expr>),
  Continue,
}

#[derive(Debug, Clone)]
pub enum LetPattern {
  Id(Param),
  Enum(EnumVariant),
  Tuple(Vec<LetPattern>),
  Wildcard,
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
  Id(Param),
  Enum(EnumVariant),
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
