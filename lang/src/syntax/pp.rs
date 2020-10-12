use crate::syntax::tree::{Decl, EnumDecl, FnDecl, ImplDecl, Module, StructDecl, TraitDecl};

pub struct PrettyPrinter {
  pub indent_per_level: usize,
}

struct State {
  pub level: usize,
  pub indent_per_level: usize,
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
      level: 0,
      indent_per_level,
    }
  }
  
  pub fn print_module(&mut self, m: &Module) {
    println!("module {} where", m.name);
    m.decl.iter().for_each(|decl| self.print_decl(decl));
  }
  
  pub fn print_decl(&mut self, d: &Decl) {
    match d {
      Decl::Import(m, name) => println!("import {} as {}", m.iter().join("."), name, ),
      Decl::Fn(decl) => self.print_fn(decl),
      Decl::Struct(decl) => self.print_struct(decl),
      Decl::Enum(decl) => self.print_enum(decl),
      Decl::Impl(decl) => self.print_impl(decl),
      Decl::Trait(decl) => self.print_trait(decl),
    }
  }
  
  pub fn print_fn(&mut self, f: &FnDecl) {
  
  }
  
  pub fn print_struct(&mut self, s: &StructDecl) {}
  pub fn print_enum(&mut self, e: &EnumDecl) {}
  pub fn print_impl(&mut self, i: &ImplDecl) {}
  pub fn print_trait(&mut self, t: &TraitDecl) {}
}
