lalrpop_mod!(pub grammar);

pub use grammar::*;

mod utils {
    use crate::syntax::tree::{Expr, Ident};

    pub enum Postfix {
        Apply(Vec<Expr>),
        Member(Ident),
        Index(Expr),
    }
}
