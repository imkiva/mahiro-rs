use std::fmt::{Display, Formatter};
use crate::syntax::tree::{Loc, Program, Stmt, Entry, Header, Body, Expr, Ident, Op, Case};
use crate::CompileResult;
use crate::error::CompileError;
use crate::syntax::tree::Entry::*;
use crate::syntax::tree::Header::*;
use crate::syntax::tree::Stmt::*;
use crate::syntax::tree::Expr::{Apply, Binary, Id, Assign, Literal, Unary};
use crate::syntax::tree::Case::{Sth, Dft};
use crate::syntax::tree::Lit::{Array, Pair};

pub struct Desugar;

pub(crate) const INJECT_PREFIX: &'static str = "__compiler_injected__";

impl Desugar {
    pub fn desugar(input: Program) -> CompileResult<Program> {
        desugar_main(input)
    }
}

/// Converts sugar to normal expressions/statements.
/// Sugars are:
/// - Stmt::ForEach
/// - Header::Import(_, None)
/// - Expr::Literal(Array(_))
/// - Expr::Literal(Pair(_, _))
/// - Expr::Unary(Op::Add, _)
/// - Expr::Unary(Op::Sub, _)
fn desugar_main(input: Program) -> CompileResult<Program> {
    Ok(input.desugar())
}

trait Desugarable {
    fn desugar(self) -> Self;
}

impl<T: Desugarable> Desugarable for Vec<T> {
    fn desugar(self) -> Self {
        self.into_iter().map(|e| e.desugar()).collect()
    }
}

impl<T: Desugarable> Desugarable for Box<T> {
    fn desugar(self) -> Self {
        Box::new((*self).desugar())
    }
}

impl<T: Desugarable> Desugarable for Option<T> {
    fn desugar(self) -> Self {
        match self {
            Some(t) => Some(t.desugar()),
            None => None,
        }
    }
}

impl Desugarable for Entry {
    fn desugar(self) -> Self {
        match self {
            StmtEntry(stmt) => StmtEntry(stmt.desugar()),
            HeaderEntry(hdr) => HeaderEntry(hdr.desugar()),
        }
    }
}

impl Desugarable for Header {
    fn desugar(self) -> Self {
        match self {
            Import(name, None) =>
                Import(name.clone(), Some(name)),
            hdr => hdr,
        }
    }
}

impl Desugarable for Stmt {
    fn desugar(self) -> Self {
        match self {
            ForEach(id, expr, body) => {
                // inject hidden iterator object
                let iter_id = assoc_id_from(&id);
                let init = iterator_begin(expr.clone());
                let cond = iterator_is_valid(iter_id.clone());
                let step = iterator_next(iter_id.clone());
                let element_init = assign_to_id(id, iterator_get_data(iter_id.clone()));

                // inject element initialization body
                let body = body.desugar();
                let mut desugared_body = Vec::with_capacity(1 + body.len());
                desugared_body.push(ExprStmt(element_init));
                desugared_body.extend(body.into_iter());

                // convert to for loop
                For(iter_id, init, cond, step, desugared_body)
            }

            Var(v) =>
                Var(v),

            VarList(vars) =>
                VarList(vars),

            Func(name, param, body) =>
                Func(name, param, body.desugar()),

            Return(expr) =>
                Return(expr.desugar()),

            Throw(expr) =>
                Throw(expr.desugar()),

            Try(tbody, id, cbody) =>
                Try(tbody.desugar(), id, cbody.desugar()),

            If(cond, t, f) =>
                If(cond.desugar(), t.desugar(), f.desugar()),

            Switch(expr, cases) =>
                Switch(expr.desugar(), cases.desugar()),

            While(cond, body) =>
                While(cond.desugar(), body.desugar()),

            Loop(cond, body) =>
                Loop(cond.desugar(), body.desugar()),

            For(id, init, cond, step, body) =>
                For(id, init.desugar(), cond.desugar(), step.desugar(), body.desugar()),

            ExprStmt(expr) =>
                ExprStmt(expr.desugar()),

            Namespace(id, body) =>
                Namespace(id, body.desugar()),

            Struct(id, extends, body) =>
                Struct(id, extends.desugar(), body.desugar()),

            Block(body) =>
                Block(body.desugar()),

            Break => Break,
            Continue => Continue,
        }
    }
}

impl Desugarable for Expr {
    fn desugar(self) -> Self {
        self
        // match self {
        //     Literal(Array(elem)) => Apply(),
        //     Literal(Pair(_, _)) => {}
        //     Unary(Op::Add, _) => {}
        //     Unary(Op::Sub, _) => {}
        //     expr => expr,
        // }
    }
}

impl Desugarable for Case {
    fn desugar(self) -> Self {
        match self {
            Sth(test, body) => Sth(test.desugar(), body.desugar()),
            Dft(body) => Dft(body.desugar()),
        }
    }
}

fn iterator_begin(iterable: Expr) -> Expr {
    apply_on(iterable, "iterate", vec![])
}

fn iterator_next(iter: Ident) -> Expr {
    apply_on(Id(iter), "next", vec![])
}

fn iterator_is_valid(iter: Ident) -> Expr {
    apply_on(Id(iter), "is_valid", vec![])
}

fn iterator_get_data(iter: Ident) -> Expr {
    apply_on(Id(iter), "get", vec![])
}

fn apply_on(expr: Expr, name: &str, args: Vec<Expr>) -> Expr {
    Apply(Box::new(
        Binary(Op::Access,
               Box::new(expr),
               Box::new(Id(Ident::only(name))))),
          args)
}

fn assign_to_id(id: Ident, expr: Expr) -> Expr {
    Assign(Op::Assign, Box::new(Id(id)), Box::new(expr))
}

fn assoc_id_from(id: &Ident) -> Ident {
    Ident::only(&format!("{}{}", INJECT_PREFIX, id.text.as_str()))
}
