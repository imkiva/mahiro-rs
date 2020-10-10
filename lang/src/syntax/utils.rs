use crate::syntax::tree::Expr::{Apply, Binary, Id};
use crate::syntax::tree::Stmt::Var;
use crate::syntax::tree::VarInit::Simple;
use crate::syntax::tree::{Expr, Ident, Loc, Op, Stmt};

pub(crate) const INJECT_PREFIX: &'static str = "__compiler_injected__";

pub(crate) fn iterator_begin(iterable: Expr) -> Expr {
    apply_on(iterable, "iterate", vec![])
}

pub(crate) fn iterator_next(iter: Ident) -> Expr {
    apply_on(Id(iter), "next", vec![])
}

pub(crate) fn iterator_is_valid(iter: Ident) -> Expr {
    apply_on(Id(iter), "is_valid", vec![])
}

pub(crate) fn iterator_get_data(iter: Ident) -> Expr {
    apply_on(Id(iter), "get", vec![])
}

pub(crate) fn builtin_pair_type() -> Expr {
    access_global("pair")
}

pub(crate) fn builtin_array_type() -> Expr {
    access_global("array")
}

pub(crate) fn access_global(name: &str) -> Expr {
    Binary(
        Loc::Injected,
        Op::Access,
        Box::new(Id(Ident::only("global"))),
        Box::new(Id(Ident::only(name))),
    )
}

pub(crate) fn apply_on(expr: Expr, name: &str, args: Vec<Expr>) -> Expr {
    Apply(
        Loc::Injected,
        Box::new(Binary(
            Loc::Injected,
            Op::Access,
            Box::new(expr),
            Box::new(Id(Ident::only(name))),
        )),
        args,
    )
}

pub(crate) fn init_var(id: Ident, expr: Expr) -> Stmt {
    Var(Simple(id, expr))
}

pub(crate) fn assoc_id_from(id: &Ident) -> Ident {
    Ident::only(&format!("{}{}", INJECT_PREFIX, id.text.as_str()))
}
