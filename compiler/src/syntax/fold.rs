use crate::syntax::tree::{Expr, Entry, Stmt, Op, VarInit, Case, Body};
use crate::syntax::tree::Entry::StmtEntry;
use crate::syntax::tree::Stmt::{Var, VarList, Func, Namespace, Struct, Block, Return, Throw, Try, If, Switch, While, Loop, For, ForEach, ExprStmt};
use crate::syntax::tree::Expr::{Literal, Lambda, Alloc, Id, Group, Assign, Apply, Unary, Binary, Ternary, Question};
use crate::syntax::tree::Lit::{Bool, Number, Array, Pair};
use crate::syntax::tree::VarInit::{Simple, Structured};
use crate::syntax::tree::Case::{Sth, Dft};

pub trait FoldContext {
    fn try_resolve_constant(&self, name: &str) -> Option<Expr>;

    fn needs_new_scope(&self, body: &Body) -> bool;
}

/// Some complex nodes can be simplified to a simpler one.
pub(crate) trait Foldable where Self: std::marker::Sized {
    type Output;

    fn fold(self) -> Self::Output {
        self.fold_with(None)
    }

    fn fold_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output;
}

/// Some complex nodes can be eliminated.
pub(crate) trait Eliminable where Self: std::marker::Sized {
    type Output;

    fn eliminate(self) -> Self::Output {
        self.eliminate_with(None)
    }

    fn eliminate_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output;
}

impl<T> Foldable for Box<T>
    where T: Foldable<Output=T> {
    type Output = Box<T>;

    fn fold_with(self: Self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        Box::new((*self).fold_with(ctx))
    }
}

impl<T> Foldable for Vec<T>
    where T: Foldable<Output=T> {
    type Output = Vec<T>;

    fn fold_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        self.into_iter()
            .map(|t| t.fold_with(ctx))
            .collect()
    }
}

impl<T> Foldable for Option<T>
    where T: Foldable<Output=T> {
    type Output = Option<T>;

    fn fold_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        match self {
            Some(t) => Some(t.fold_with(ctx)),
            _ => None,
        }
    }
}

impl<T> Eliminable for Vec<T>
    where T: Eliminable<Output=Option<T>> {
    type Output = Vec<T>;

    fn eliminate_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        self.into_iter()
            .filter_map(|t| t.eliminate_with(ctx))
            .collect()
    }
}

impl<T> Eliminable for Option<T>
    where T: Eliminable<Output=T> {
    type Output = Option<T>;

    fn eliminate_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        match self {
            Some(t) => Some(t.eliminate_with(ctx)),
            _ => None,
        }
    }
}

impl Eliminable for Entry {
    type Output = Option<Entry>;

    fn eliminate_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        match self {
            StmtEntry(stmt) => match stmt.eliminate_with(ctx) {
                Some(stmt) => Some(StmtEntry(stmt)),
                _ => None,
            },
            entry => Some(entry)
        }
    }
}

impl Eliminable for Stmt {
    type Output = Option<Stmt>;

    fn eliminate_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        #[cfg(debug_assertions)]
        match &self {
            // no optimization for syntax sugars
            // and they should not be reachable here.
            ForEach(_, _, _) => unreachable!("sanity check"),
            _ => (),
        }

        match self {
            Var(v) => Some(Var(v.fold_with(ctx))),
            VarList(vs) => Some(VarList(vs.fold_with(ctx))),
            Return(expr) => Some(Return(expr.fold_with(ctx))),
            Throw(expr) => Some(Throw(expr.fold_with(ctx))),
            ExprStmt(expr) => Some(ExprStmt(expr.fold_with(ctx))),

            Block(body) => {
                let mut body = body.eliminate_with(ctx);
                match (body.len(), ctx.map(|c| c.needs_new_scope(&body))) {
                    (0, _) => None,
                    // TODO: flatten the whole body if the body doesn't need a new scope
                    (1, Some(false)) => body.pop(),
                    _ => Some(Block(body)),
                }
            }

            Func(id, param, body) =>
                Some(Func(id, param, body.eliminate_with(ctx))),

            Namespace(id, body) =>
                Some(Namespace(id, body.eliminate_with(ctx))),

            Struct(id, ex, body) =>
                Some(Struct(id, ex.fold_with(ctx), body.eliminate_with(ctx))),

            Try(tbody, id, cbody) =>
                Some(Try(tbody.eliminate_with(ctx), id, cbody.eliminate_with(ctx))),

            If(cond, t, f) => {
                let cond = cond.fold_with(ctx);
                match &cond {
                    Literal(Bool(true)) => Block(t).eliminate_with(ctx),
                    Literal(Bool(false)) => match f {
                        Some(body) => Block(body).eliminate_with(ctx),
                        _ => None,
                    },
                    _ => Some(If(cond, t.eliminate_with(ctx), {
                        let f = f.eliminate_with(ctx);
                        match &f {
                            Some(v) if v.len() == 0 => None,
                            _ => f,
                        }
                    })),
                }
            }

            While(cond, body) => {
                let cond = cond.fold_with(ctx);
                match &cond {
                    Literal(Bool(true)) => Some(Loop(None, body.eliminate_with(ctx))),
                    Literal(Bool(false)) => None,
                    _ => Some(While(cond, body.eliminate_with(ctx))),
                }
            }

            Loop(Some(cond), body) => {
                let cond = cond.fold_with(ctx);
                match &cond {
                    Literal(Bool(true)) => Block(body).eliminate_with(ctx),
                    _ => Some(Loop(Some(cond), body.eliminate_with(ctx))),
                }
            }

            For(id, init, cond, step, body) => {
                let cond = cond.fold_with(ctx);
                match &cond {
                    Literal(Bool(true)) => {
                        let mut new_body = body.eliminate_with(ctx);
                        new_body.push(ExprStmt(step.fold_with(ctx)));
                        Some(Block(vec![
                            Var(Simple(id, init.fold_with(ctx))),
                            Loop(None, new_body)
                        ]))
                    }

                    Literal(Bool(false)) => None,

                    _ => Some(For(id, init.fold_with(ctx), cond,
                                  step.fold_with(ctx),
                                  body.eliminate_with(ctx))),
                }
            }

            Switch(expr, cases) =>
                Some(Switch(expr.fold_with(ctx), cases.fold_with(ctx))),

            stmt => Some(stmt),
        }
    }
}

impl Foldable for VarInit {
    type Output = VarInit;

    fn fold_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        match self {
            Simple(id, val) => Simple(id, val.fold_with(ctx)),
            Structured(id, val) => Structured(id, val.fold_with(ctx)),
        }
    }
}

impl Foldable for Case {
    type Output = Case;

    fn fold_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        match self {
            Sth(test, body) => Sth(test.fold_with(ctx), body.eliminate_with(ctx)),
            Dft(body) => Dft(body.eliminate_with(ctx)),
        }
    }
}

impl Foldable for Expr {
    type Output = Expr;

    fn fold_with(self, ctx: Option<&dyn FoldContext>) -> Self::Output {
        #[cfg(debug_assertions)]
        match &self {
            // no optimization for syntax sugars
            // and they should not be reachable here.
            Unary(Op::Add, rhs) |
            Unary(Op::Sub, rhs) => {
                match rhs.as_ref() {
                    Literal(Number(_)) => unreachable!("sanity check"),
                    _ => (),
                }
            }
            Literal(Array(_)) |
            Literal(Pair(_, _)) =>
                unreachable!("sanity check"),
            _ => (),
        }

        match self {
            Unary(op, operand) =>
                fold_unary(op, *operand.fold_with(ctx), ctx),

            Binary(op, lhs, rhs) =>
                fold_binary(op, *lhs.fold_with(ctx), *rhs.fold_with(ctx), ctx),

            Id(id) => {
                match ctx.map(|c| c.try_resolve_constant(id.text.as_str())) {
                    Some(Some(constant)) => constant,
                    _ => Id(id)
                }
            }

            Lambda(cap, params, body) =>
                Lambda(cap, params, body.fold_with(ctx)),

            Alloc(ty, args) =>
                Alloc(ty.fold_with(ctx), args.fold_with(ctx)),

            Group(exprs) =>
                Group(exprs.fold_with(ctx)),

            Assign(op, lhs, rhs) =>
                Assign(op, lhs.fold_with(ctx), rhs.fold_with(ctx)),

            Apply(f, args) =>
                Apply(f.fold_with(ctx), args.fold_with(ctx)),

            Ternary(cond, t, f) =>
                Ternary(cond.fold_with(ctx), t.fold_with(ctx), f.fold_with(ctx)),

            Question(cond, f) =>
                Question(cond.fold_with(ctx), f.fold_with(ctx)),

            expr => expr,
        }
    }
}

fn fold_unary(op: Op, operand: Expr, _: Option<&dyn FoldContext>) -> Expr {
    match (&op, &operand) {
        (Op::Not, Literal(Bool(b))) =>
            Literal(Bool(!b.clone())),
        _ => Unary(op, Box::new(operand)),
    }
}

fn fold_binary(op: Op, lhs: Expr, rhs: Expr, _: Option<&dyn FoldContext>) -> Expr {
    match (&op, &lhs, &rhs) {
        (Op::And, Literal(Bool(false)), _) => Literal(Bool(false)),
        (Op::And, Literal(Bool(true)), _) => rhs,
        (Op::Or, Literal(Bool(true)), _) => Literal(Bool(true)),
        (Op::Or, Literal(Bool(false)), _) => rhs,

        (Op::Add, Literal(Number(a)), Literal(Number(b))) =>
            Literal(Number(a + b)),
        (Op::Sub, Literal(Number(a)), Literal(Number(b))) =>
            Literal(Number(a - b)),
        (Op::Mul, Literal(Number(a)), Literal(Number(b))) =>
            Literal(Number(a * b)),
        (Op::Div, Literal(Number(a)), Literal(Number(b))) =>
            Literal(Number(a / b)),
        (Op::Mod, Literal(Number(a)), Literal(Number(b))) =>
            Literal(Number(a % b)),
        (Op::Pow, Literal(Number(a)), Literal(Number(b))) =>
            Literal(Number(f64::powf(*a, *b))),

        (Op::Eq, Literal(a), Literal(b)) =>
            Literal(Bool(*a == *b)),
        (Op::Ne, Literal(a), Literal(b)) =>
            Literal(Bool(*a != *b)),
        (Op::Ge, Literal(a), Literal(b)) =>
            Literal(Bool(*a >= *b)),
        (Op::Gt, Literal(a), Literal(b)) =>
            Literal(Bool(*a > *b)),
        (Op::Le, Literal(a), Literal(b)) =>
            Literal(Bool(*a <= *b)),
        (Op::Lt, Literal(a), Literal(b)) =>
            Literal(Bool(*a < *b)),

        _ => Binary(op, Box::new(lhs), Box::new(rhs)),
    }
}
