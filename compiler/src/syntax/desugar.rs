use crate::syntax::tree::{Program, Stmt, Entry, Header, Expr, Op, Case, VarInit};
use crate::CompileResult;
use crate::syntax::tree::Entry::*;
use crate::syntax::tree::Header::*;
use crate::syntax::tree::Stmt::*;
use crate::syntax::tree::Expr::{Literal, Unary, Alloc};
use crate::syntax::tree::Case::{Sth, Dft};
use crate::syntax::tree::Lit::{Array, Pair, Number};
use crate::syntax::utils::*;
use crate::syntax::tree::VarInit::{Simple, Structured};

pub struct Desugar;

impl Desugar {
    /// Converts sugar to normal expressions/statements.
    /// Sugars are:
    /// - Stmt::ForEach
    /// - Header::Import(_, None)
    /// - Expr::Literal(Array(_))
    /// - Expr::Literal(Pair(_, _))
    /// - Expr::Unary(Op::Add, Literal(Number(_)))
    /// - Expr::Unary(Op::Sub, Literal(Number(_)))
    pub fn desugar(input: Program) -> CompileResult<Program> {
        desugar_main(input)
    }
}

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
                let element_init = init_var(id, iterator_get_data(iter_id.clone()));

                // inject element initialization body
                let body = body.desugar();
                let mut desugared_body = Vec::with_capacity(1 + body.len());
                desugared_body.push(element_init);
                desugared_body.extend(body.into_iter());

                // convert to for loop
                For(iter_id, init, cond, step, desugared_body)
            }

            Var(v) =>
                Var(v.desugar()),

            VarList(vars) =>
                VarList(vars.desugar()),

            Func(name, param, body) =>
                Func(name, param, body.desugar()),

            Return(expr) =>
                Return(expr.desugar()),

            Throw(expr) =>
                Throw(expr.desugar()),

            Try(loc, tbody, id, cbody) =>
                Try(loc, tbody.desugar(), id, cbody.desugar()),

            If(loc, cond, t, f) =>
                If(loc, cond.desugar(), t.desugar(), f.desugar()),

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

            Break(id) => Break(id),
            Continue(id) => Continue(id),
        }
    }
}

impl Desugarable for Expr {
    fn desugar(self) -> Self {
        match self {
            Literal(loc, Array(elem)) =>
                Alloc(loc, Box::new(builtin_array_type()), elem.desugar()),

            Literal(loc, Pair(k, v)) =>
                Alloc(loc, Box::new(builtin_pair_type()), vec![(*k).desugar(), (*v).desugar()]),

            Unary(loc, Op::Add, e) => {
                match e.as_ref() {
                    Literal(_, Number(n)) => Literal(loc, Number(n.abs())),
                    _ => Unary(loc, Op::Add, e),
                }
            }

            Unary(loc, Op::Sub, e) => {
                match e.as_ref() {
                    Literal(_, Number(n)) => Literal(loc, Number(-n.clone())),
                    _ => Unary(loc, Op::Add, e),
                }
            }

            expr => expr,
        }
    }
}

impl Desugarable for VarInit {
    fn desugar(self) -> Self {
        match self {
            Simple(id, v) => Simple(id, v.desugar()),
            Structured(ids, v) => Structured(ids, v.desugar()),
        }
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
