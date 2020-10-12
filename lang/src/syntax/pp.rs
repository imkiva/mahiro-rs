use crate::syntax::tree::{
  Constraint, Decl, EnumDecl, Expr, FnDecl, FnSig, GenericParam, Ident, ImplDecl, LetPattern, Lit,
  MatchCase, MatchPattern, Module, Param, PatEnumVariant, Stmt, StructDecl, TraitDecl,
};

pub struct PrettyPrinter {
  pub indent_per_level: usize,
}

struct State {
  pub indent_level: usize,
  pub indent_per_level: usize,
}

trait PrettyPrint {
  fn pretty_print(&self) -> String;
}

trait JoinablePrettyPrint {
  fn join_pretty_print(&self, delim: &str) -> String;
}

impl PrettyPrinter {
  pub fn new(indent: usize) -> PrettyPrinter {
    PrettyPrinter {
      indent_per_level: indent,
    }
  }

  pub fn print_module(&self, m: &Module) {
    let mut state = State::new(self.indent_per_level);
    state.print_module(m);
  }

  pub fn print_decl(&self, decl: &Decl) {
    let mut state = State::new(self.indent_per_level);
    state.print_decl(decl);
  }
}

impl State {
  pub fn new(indent_per_level: usize) -> State {
    State {
      indent_level: 0,
      indent_per_level,
    }
  }

  fn print_indent(&self) {
    let spaces = self.indent_level * self.indent_per_level;
    print!("{:>width$}", "", width = spaces);
  }

  pub fn print_module(&mut self, m: &Module) {
    println!("module {} where", m.name);
    m.decl.iter().for_each(|decl| self.print_decl(decl));
  }

  pub fn print_decl(&mut self, d: &Decl) {
    match d {
      Decl::Import(m, name) => {
        self.print_indent();
        println!("import {} as {}", m.join("."), name,)
      }
      Decl::Fn(decl) => self.print_fn(decl),
      Decl::Struct(decl) => self.print_struct(decl),
      Decl::Enum(decl) => self.print_enum(decl),
      Decl::Impl(decl) => self.print_impl(decl),
      Decl::Trait(decl) => self.print_trait(decl),
    }
  }

  fn print_fn_sig(&self, sig: &FnSig) {
    self.print_indent();
    print!(
      "{}fn {}({}) -> {}",
      if sig.async_fn { "async " } else { "" },
      sig.name,
      sig.param.pretty_print(),
      sig.ret
    );
  }

  pub fn print_fn(&mut self, f: &FnDecl) {
    self.print_fn_sig(&f.sig);
    println!(" {{");

    self.indent_level += 1;
    for stmt in &f.body {
      self.print_stmt(stmt);
    }
    self.indent_level -= 1;

    self.print_indent();
    println!("}}");
  }

  pub fn print_struct(&mut self, s: &StructDecl) {
    self.print_indent();
    println!(
      "struct {}{} {{",
      s.name,
      match s.generic.len() {
        0 => "".to_string(),
        _ => format!("<{}>", s.generic.pretty_print()),
      },
    );

    self.indent_level += 1;
    for param in &s.fields {
      self.print_indent();
      println!("{},", param.pretty_print());
    }
    self.indent_level -= 1;

    self.print_indent();
    println!("}}");
  }

  pub fn print_enum(&mut self, e: &EnumDecl) {
    self.print_indent();
    println!(
      "enum {}{} {{",
      e.name,
      match e.generic.len() {
        0 => "".to_string(),
        _ => format!("<{}>", e.generic.pretty_print()),
      },
    );

    self.indent_level += 1;
    for v in &e.variants {
      self.print_indent();
      println!("{},", v.pretty_print());
    }
    self.indent_level -= 1;

    self.print_indent();
    println!("}}");
  }

  pub fn print_impl(&mut self, i: &ImplDecl) {
    println!(
      "impl{} {} {} {{",
      match i.generic.len() {
        0 => "".to_string(),
        _ => format!("<{}>", i.generic.pretty_print()),
      },
      i.trait_name,
      match &i.for_type {
        Some(ty) => format!("for {}", ty),
        None => "".to_string(),
      }
    );

    self.indent_level += 1;
    i.fns.iter().for_each(|f| self.print_fn(f));
    self.indent_level -= 1;

    println!("}}");
  }

  pub fn print_trait(&mut self, t: &TraitDecl) {
    self.print_indent();
    println!(
      "trait {}{} {{",
      t.name,
      match t.generic.len() {
        0 => "".to_string(),
        _ => format!("<{}>", t.generic.pretty_print()),
      },
    );

    self.indent_level += 1;
    t.fns.iter().for_each(|sig| {
      self.print_fn_sig(sig);
      println!(";");
    });
    self.indent_level -= 1;

    self.print_indent();
    println!("}}");
  }

  pub fn print_stmt(&mut self, s: &Stmt) {
    self.print_indent();
    match s {
      Stmt::Return(Some(e)) => {
        print!("return ");
        self.print_expr(e);
        println!(";");
      }
      Stmt::Return(None) => {
        println!("return;");
      }
      Stmt::Let(pat, e) => {
        print!("let {} = ", pat.pretty_print());
        self.print_expr(e);
        println!(";");
      }
      Stmt::Assign(op, id, e) => {
        println!("{} {} ", id, op);
        self.print_expr(e);
        println!(";");
      }
      Stmt::Expr(e) => {
        self.print_expr(e);
        println!(";");
      }
      Stmt::ResultExpr(e) => {
        self.print_expr(e);
        println!();
      }
      Stmt::Break(Some(e)) => {
        println!("break ");
        self.print_expr(e);
        println!(";");
      }
      Stmt::Break(None) => {
        println!("break;");
      }
      Stmt::Continue => {
        println!("continue;");
      }
    }
  }

  pub fn print_expr(&mut self, e: &Expr) {
    match e {
      Expr::If(cond, t, f) => {
        print!("if ");
        self.print_expr(cond.as_ref());
        print!(" ");
        self.print_body(t);
        if let Some(el) = f {
          print!(" else ");
          self.print_body(el);
        }
      }
      Expr::While(cond, body) => {
        print!("while ");
        self.print_expr(cond.as_ref());
        print!(" ");
        self.print_body(body);
      }
      Expr::For(id, e, body) => {
        print!("for {} in ", id);
        self.print_expr(e.as_ref());
        print!(" ");
        self.print_body(body);
      }
      Expr::Match(e, cases) => {
        print!("match ");
        self.print_expr(e.as_ref());
        print!(" ");
        self.print_match_cases(cases);
      }
      Expr::Binary(op, lhs, rhs) => {
        self.print_expr(lhs.as_ref());
        print!(" {} ", op);
        self.print_expr(rhs.as_ref());
      }
      Expr::Unary(op, e) => {
        print!("{}", op);
        self.print_expr(e.as_ref());
      }
      Expr::Lit(Lit::LitArray(a)) => {
        print!("[");
        for (i, x) in a.iter().enumerate() {
          self.print_expr(x);
          print!("{}", if i == (a.len() - 1) { "" } else { "," });
        }
        print!("]");
      }
      Expr::Lit(lit) => print!("{}", lit.pretty_print()),
      Expr::Tuple(t) => {
        print!("(");
        for (i, x) in t.iter().enumerate() {
          self.print_expr(x);
          print!("{}", if i == (t.len() - 1) { "" } else { "," });
        }
        print!(")");
      }
      Expr::Lambda(p, body) => {
        print!("|{}| ", p.join_pretty_print(", "));
        self.print_body(body);
      }
      Expr::Id(id) => print!("{}", id),
      Expr::Apply(e, a) => {
        self.print_expr(e.as_ref());
        print!("(");
        for (i, x) in a.iter().enumerate() {
          self.print_expr(x);
          print!("{}", if i == (a.len() - 1) { "" } else { "," });
        }
        print!(")");
      }
      Expr::MemberApply(e, m, a) => {
        self.print_expr(e.as_ref());
        print!(".{}(", m);
        for (i, x) in a.iter().enumerate() {
          self.print_expr(x);
          print!("{}", if i == (a.len() - 1) { "" } else { "," });
        }
        print!(")");
      }
      Expr::Member(e, m) => {
        self.print_expr(e.as_ref());
        print!(".{}", m);
      }
      Expr::Index(e, i) => {
        self.print_expr(e.as_ref());
        print!("[");
        self.print_expr(i.as_ref());
        print!("]");
      }
    }
  }

  fn print_body(&mut self, body: &Vec<Stmt>) {
    println!("{{");

    self.indent_level += 1;
    body.iter().for_each(|stmt| {
      self.print_stmt(stmt);
    });
    self.indent_level -= 1;

    self.print_indent();
    print!("}}");
  }

  fn print_match_cases(&mut self, cases: &Vec<MatchCase>) {
    println!("{{");

    self.indent_level += 1;
    cases.iter().for_each(|case| {
      self.print_indent();
      print!("{} => ", case.pat.pretty_print());
      self.print_body(&case.action);
      println!();
    });
    self.indent_level -= 1;

    self.print_indent();
    print!("}}");
  }
}

impl<T> PrettyPrint for Vec<T>
where
  T: PrettyPrint,
{
  fn pretty_print(&self) -> String {
    self.join_pretty_print(", ")
  }
}

impl<T> JoinablePrettyPrint for Vec<T>
where
  T: PrettyPrint,
{
  fn join_pretty_print(&self, delim: &str) -> String {
    self
      .iter()
      .map(|p| p.pretty_print())
      .collect::<Vec<_>>()
      .join(delim)
  }
}

impl PrettyPrint for Param {
  fn pretty_print(&self) -> String {
    format!(
      "{}{}",
      self.name,
      match &self.ty {
        None => "".to_string(),
        Some(ty) => format!(": {}", ty),
      }
    )
  }
}

impl PrettyPrint for GenericParam {
  fn pretty_print(&self) -> String {
    format!(
      "{}{}",
      self.name,
      match self.constraints.len() {
        0 => "".to_string(),
        _ => self.constraints.join_pretty_print(" + "),
      }
    )
  }
}

impl PrettyPrint for Constraint {
  fn pretty_print(&self) -> String {
    match self {
      Constraint::MustImpl(ty) => format!("{}", ty),
    }
  }
}

impl PrettyPrint for Ident {
  fn pretty_print(&self) -> String {
    self.clone()
  }
}

impl<T: PrettyPrint> PrettyPrint for PatEnumVariant<T> {
  fn pretty_print(&self) -> String {
    format!(
      "{}{}",
      self.name,
      match self.fields.len() {
        0 => "".to_string(),
        _ => format!("({})", self.fields.join_pretty_print(", ")),
      }
    )
  }
}

impl PrettyPrint for LetPattern {
  fn pretty_print(&self) -> String {
    match self {
      LetPattern::Id(p) => p.pretty_print(),
      LetPattern::Enum(e) => e.pretty_print(),
      LetPattern::Tuple(t) => format!("({})", t.join_pretty_print(", ")),
      LetPattern::Wildcard => "_".to_string(),
    }
  }
}

impl PrettyPrint for MatchPattern {
  fn pretty_print(&self) -> String {
    match self {
      MatchPattern::Id(p) => p.pretty_print(),
      MatchPattern::Enum(e) => e.pretty_print(),
      MatchPattern::Tuple(t) => format!("({})", t.join_pretty_print(", ")),
      MatchPattern::Wildcard => "_".to_string(),
      MatchPattern::Lit(lit) => lit.pretty_print(),
    }
  }
}

impl PrettyPrint for Lit {
  fn pretty_print(&self) -> String {
    match self {
      Lit::LitUnit => "()".to_string(),
      Lit::LitInt(i) => format!("{}", i),
      Lit::LitFloat(f) => format!("{}", f),
      Lit::LitChar(c) => format!("{}", c),
      Lit::LitBool(b) => format!("{}", b),
      Lit::LitString(s) => format!("\"{}\"", s),
      Lit::LitArray(_) => unreachable!("should be handled in the caller"),
    }
  }
}
