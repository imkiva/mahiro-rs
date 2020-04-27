use crate::check::context::{CheckContext, ScopeId};
use crate::syntax::tree::{Expr, Lit, Loc, ToLoc};
use crate::CompileResult;
use crate::check::check::{check_params, raise_bottom_typed_expr_error, raise_type_mismatch_error, raise_argc_mismatch_error};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Any,
    Number,
    String,
    Char,
    Bool,
    Array,
    Pair,
    Applicable(Box<Type>, Vec<Type>),
}

impl Type {
    pub fn not_void(&self, loc: &Loc) -> CompileResult<()> {
        match self {
            Type::Void => raise_bottom_typed_expr_error(loc.clone()),
            _ => Ok(())
        }
    }

    pub fn is_applicable(&self, loc: &Loc) -> CompileResult<()> {
        match self {
            Type::Any |
            Type::Applicable(_, _) => Ok(()),
            _ => raise_type_mismatch_error(loc.clone(), None, self.clone())
        }
    }

    pub fn against(&self, expected: &Type, loc: &Loc) -> CompileResult<()> {
        match (self, expected) {
            (Type::Any, _) |
            (_, Type::Any) => Ok(()),
            (lhs, rhs) if lhs == rhs => Ok(()),
            _ => {
                raise_type_mismatch_error(loc.clone(),
                                          Some(expected.clone()),
                                          self.clone())?;
                unreachable!()
            }
        }
    }
}

pub fn check_expr(ctx: &mut CheckContext, expr: &Expr) -> CompileResult<Type> {
    match expr {
        Expr::Literal(_, lit) => check_lit(ctx, lit),
        Expr::Alloc(_, _, _) => Ok(Type::Any),

        Expr::Lambda(_, _, params, body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_params(ctx, params)?;
            let args = params.iter().map(|_| Type::Any).collect();
            let ret = check_expr(ctx, body.as_ref())?;
            ctx.leave_scope();
            Ok(Type::Applicable(Box::new(ret), args))
        }

        Expr::Id(id) => match ctx.lookup(id) {
            Some((_, t)) => Ok(t.clone()),
            _ => Ok(Type::Any),
        },

        Expr::Group(_, exprs) => {
            let mut t = Type::Void;
            for expr in exprs {
                t = check_expr(ctx, expr)?;
            }
            Ok(t)
        }

        Expr::Assign(_, _, lhs, rhs) => {
            check_expr(ctx, rhs.as_ref())?.not_void(&rhs.as_ref().to_loc())?;
            check_expr(ctx, lhs.as_ref())
        }

        Expr::Apply(loc, id, args) => {
            let t = check_expr(ctx, id.as_ref())?;
            t.is_applicable(&id.as_ref().to_loc())?;
            match t {
                Type::Any => Ok(Type::Any),
                Type::Applicable(ret, types) => {
                    if args.len() != types.len() {
                        raise_argc_mismatch_error(loc.clone(),
                                                  types.len(),
                                                  args.len())?;
                    }
                    for (arg, t) in args.iter().zip(types) {
                        check_expr(ctx, arg)?.against(&t, &arg.to_loc())?;
                    }
                    Ok(ret.as_ref().clone())
                }
                _ => unreachable!(),
            }
        }

        // TODO
        // Expr::Unary(_, _) => {}
        // Expr::Binary(_, _, _) => {}
        // Expr::Ternary(_, _, _) => {}
        // Expr::Question(_, _) => {}
        _ => Ok(Type::Any),
    }
}

fn check_lit(_: &mut CheckContext, lit: &Lit) -> CompileResult<Type> {
    match lit {
        Lit::Number(_) => Ok(Type::Number),
        Lit::Bool(_) => Ok(Type::Bool),
        Lit::Char(_) => Ok(Type::Char),
        Lit::Str(_) => Ok(Type::String),
        _ => Ok(Type::Any),
    }
}
