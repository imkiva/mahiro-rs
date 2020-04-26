use std::collections::{HashMap, VecDeque};
use crate::syntax::tree::Ident;

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum IdType {
    Var,
    Func,
    Struct,
    Namespace,
}

#[derive(Clone, Debug, Default)]
struct Scope {
    /// All defined identifiers
    defined_idents: HashMap<Ident, IdType>,
    /// Are we inside a loop statement?
    in_loop: bool,
}

#[derive(Clone, Debug, Default)]
pub struct CheckContext {
    scope: VecDeque<Scope>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    /// Lookup identifier by type.
    pub fn lookup_ident(&self, ident: &Ident, idtype: IdType) -> Option<&Ident> {
        match self.defined_idents.get_key_value(ident) {
            Some((id, def_type)) if def_type == &idtype => Some(id),
            _ => None,
        }
    }

    /// Define new symbol in current scope
    /// Return previous defined identifier if redefinition detected.
    pub fn define(&mut self, ident: Ident, idtype: IdType) -> Option<&Ident> {
        if self.defined_idents.contains_key(&ident) {
            self.lookup_ident(&ident, idtype)
        } else {
            let _ = self.defined_idents.insert(ident, idtype);
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

    pub fn enter_scope(&mut self) {
        self.scope.push_front(Scope::new());
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

    pub fn lookup_in_current(&self, ident: &Ident, idtype: IdType) -> Option<&Ident> {
        self.current_scope().lookup_ident(ident, idtype)
    }

    pub fn lookup(&self, ident: &Ident, idtype: IdType) -> Option<&Ident> {
        for scope in &self.scope {
            match scope.lookup_ident(ident, idtype) {
                None => (),
                some => return some,
            }
        }
        None
    }

    pub fn define_in_current(&mut self, ident: Ident, idtype: IdType) -> Option<&Ident> {
        self.current_scope_mut().define(ident, idtype)
    }
}
