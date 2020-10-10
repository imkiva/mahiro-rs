use crate::check::infer_check::Type;
use crate::syntax::tree::Ident;
use std::collections::{HashMap, VecDeque};

#[derive(Clone, Debug)]
pub enum ScopeId {
    Global,
    UnnamedBlock,
    Func(String),
    Namespace(String),
    Struct(String),
}

#[derive(Clone, Debug)]
struct Scope {
    /// Scope id
    id: ScopeId,
    /// All defined identifiers
    defined_idents: HashMap<Ident, Type>,
    /// Are we inside a loop statement?
    in_loop: bool,
}

#[derive(Clone, Debug)]
pub struct CheckContext {
    /// Scope stack
    scope: VecDeque<Scope>,
    /// Indentation depth we are in,
    /// used for tracing check
    depth: usize,
    /// Are we tracing the check process?
    trace_check: bool,
    /// Indent for each depth
    trace_base_indent: usize,
}

impl Default for CheckContext {
    fn default() -> Self {
        Self {
            scope: Default::default(),
            depth: 0,
            trace_check: false,
            trace_base_indent: 2,
        }
    }
}

impl Scope {
    pub fn new(id: ScopeId) -> Self {
        Scope {
            id,
            defined_idents: Default::default(),
            in_loop: false,
        }
    }

    pub fn lookup_ident(&self, ident: &Ident) -> Option<(&Ident, &Type)> {
        self.defined_idents.get_key_value(ident)
    }

    pub fn define(&mut self, ident: &Ident, idtype: &Type, force: bool) -> Option<&Ident> {
        if force || !self.defined_idents.contains_key(&ident) {
            let _ = self.defined_idents.insert(ident.clone(), idtype.clone());
            None
        } else {
            self.lookup_ident(&ident).map(|p| p.0)
        }
    }

    pub fn enter_loop(&mut self) {
        if self.in_loop {
            unreachable!("Nested-loop should be in a new scope");
        }
        self.in_loop = true;
    }

    pub fn leave_loop(&mut self) {
        if !self.in_loop {
            unreachable!("Leaving a loop that doesn't exists");
        }
        self.in_loop = false;
    }
}

impl CheckContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Lookup identifier by name and return the name and type.
    pub fn lookup_in_current(&self, ident: &Ident) -> Option<(&Ident, &Type)> {
        self.current_scope().lookup_ident(ident)
    }

    /// Lookup identifier by name and return the name and type.
    /// This function searches symbol recursively.
    pub fn lookup(&self, ident: &Ident) -> Option<(&Ident, &Type)> {
        for scope in &self.scope {
            match scope.lookup_ident(ident) {
                None => (),
                some => return some,
            }
        }
        None
    }

    /// Define new symbol in current scope
    /// Return previous defined identifier if redefinition detected.
    pub fn define_in_current(&mut self, ident: &Ident, idtype: &Type) -> Option<&Ident> {
        self.current_scope_mut().define(ident, idtype, false)
    }

    /// Define new symbol in current scope forcibly
    /// Return previous defined identifier if redefinition detected.
    pub fn rewrite_in_current(&mut self, ident: &Ident, idtype: &Type) {
        let _ = self.current_scope_mut().define(ident, idtype, true);
    }

    pub fn is_tracing(&self) -> bool {
        self.trace_check
    }

    pub fn set_tracing(&mut self, tracing: bool) {
        self.trace_check = tracing;
    }

    pub fn set_tracing_indent(&mut self, indent: usize) {
        self.trace_base_indent = indent;
    }

    pub fn trace<F>(&self, f: F)
    where
        F: FnOnce() -> (),
    {
        if self.is_tracing() {
            let spaces = (self.depth - 1) * self.trace_base_indent;
            for _ in 0..spaces {
                print!(" ");
            }
            f();
        }
    }

    fn trace_scope(&self, scope_id: &ScopeId) {
        match scope_id {
            ScopeId::Global => println!("Global"),
            ScopeId::Func(name) => println!("Function '{}'", name.as_str()),
            ScopeId::Namespace(name) => println!("Namespace '{}'", name.as_str()),
            ScopeId::Struct(name) => println!("Struct '{}'", name.as_str()),
            ScopeId::UnnamedBlock => println!("Block"),
        }
    }

    pub fn enter_scope(&mut self, scope_id: ScopeId) {
        self.depth += 1;
        self.trace(|| {
            print!("> ");
            self.trace_scope(&scope_id);
        });

        self.scope.push_front(Scope::new(scope_id));
    }

    pub fn leave_scope(&mut self) {
        if let Some(scope) = self.scope.pop_front() {
            self.trace(|| {
                print!("< ");
                self.trace_scope(&scope.id);
            });
        } else {
            unreachable!("Checking stack underflow");
        }
        self.depth -= 1;
    }

    pub fn enter_loop(&mut self) {
        self.trace(|| {
            print!("> Loop body");
        });
        self.depth += 1;
        self.current_scope_mut().enter_loop();
    }

    pub fn leave_loop(&mut self) {
        self.trace(|| {
            print!("< Loop body");
        });
        self.depth -= 1;
        self.current_scope_mut().leave_loop();
    }

    pub fn is_in_loop(&self) -> bool {
        self.current_scope().in_loop
    }

    pub fn is_in_function(&self) -> bool {
        for scope in &self.scope {
            match scope.id {
                ScopeId::Func(_) => return true,
                _ => (),
            }
        }
        false
    }

    fn current_scope(&self) -> &Scope {
        self.scope.front().expect("Checking stack underflow")
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope.front_mut().expect("Checking stack underflow")
    }
}
