use crate::syntax::tree::{Constraint, Decl, EnumDecl, EnumVariant, FnDecl, FnSig, GenericParam, ImplDecl, Module, Param, Stmt, StructDecl, TraitDecl, Type};

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
        println!("import {} as {}", m.join("."), name, )
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
      self.print_indent();
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
    println!("<Stmt>");
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

impl PrettyPrint for EnumVariant {
  fn pretty_print(&self) -> String {
    format!(
      "{}{}",
      self.name,
      match self.fields.len() {
        0 => "".to_string(),
        _ => format!("({})", self.fields.join(", ")),
      }
    )
  }
}
