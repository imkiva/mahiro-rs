use std::collections::{VecDeque, HashSet};
use crate::syntax::tree::Ident;

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
    defined_idents: HashSet<Ident>,
    /// Are we inside a loop statement?
    in_loop: bool,
}

#[derive(Clone, Debug, Default)]
pub struct CheckContext {
    scope: VecDeque<Scope>,
}

impl Scope {
    pub fn new(id: ScopeId) -> Self {
        Scope {
            id,
            defined_idents: Default::default(),
            in_loop: false,
        }
    }

    /// Lookup identifier by type.
    pub fn lookup_ident(&self, ident: &Ident) -> Option<&Ident> {
        self.defined_idents.get(ident)
    }

    /// Define new symbol in current scope
    /// Return previous defined identifier if redefinition detected.
    pub fn define(&mut self, ident: &Ident) -> Option<&Ident> {
        if self.defined_idents.contains(&ident) {
            self.lookup_ident(&ident)
        } else {
            let _ = self.defined_idents.insert(ident.clone());
            None
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

    pub fn enter_scope(&mut self, scope_id: ScopeId) {
        self.scope.push_front(Scope::new(scope_id));
    }

    pub fn leave_scope(&mut self) {
        if let None = self.scope.pop_front() {
            unreachable!("Checking stack underflow");
        }
    }

    fn current_scope(&self) -> &Scope {
        self.scope.front().expect("Checking stack underflow")
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope.front_mut().expect("Checking stack underflow")
    }

    pub fn lookup_in_current(&self, ident: &Ident) -> Option<&Ident> {
        self.current_scope().lookup_ident(ident)
    }

    pub fn lookup(&self, ident: &Ident) -> Option<&Ident> {
        for scope in &self.scope {
            match scope.lookup_ident(ident) {
                None => (),
                some => return some,
            }
        }
        None
    }

    pub fn define_in_current(&mut self, ident: &Ident) -> Option<&Ident> {
        self.current_scope_mut().define(ident)
    }

    pub fn is_in_loop(&self) -> bool {
        self.current_scope().in_loop
    }

    pub fn enter_loop(&mut self) {
        self.current_scope_mut().enter_loop();
    }

    pub fn leave_loop(&mut self) {
        self.current_scope_mut().leave_loop();
    }
}
