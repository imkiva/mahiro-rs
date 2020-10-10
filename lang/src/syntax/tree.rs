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

pub type Program = ();
