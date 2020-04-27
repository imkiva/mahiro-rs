use crate::check::context::{CheckContext, ScopeId};
use crate::syntax::tree::{Expr, Lit, Loc, ToLoc, Op};
use crate::CompileResult;
use crate::check::check::{check_params, raise_bottom_typed_expr_error, raise_type_mismatch_error, raise_argc_mismatch_error};
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub ty: Types,
    pub loc: Loc,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Types {
    Void,
    Any,
    Number,
    String,
    Char,
    Bool,
    Applicable(Box<Type>, Vec<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Types::Void => write!(f, "void"),
            Types::Any => write!(f, "any"),
            Types::Number => write!(f, "number"),
            Types::String => write!(f, "string"),
            Types::Char => write!(f, "char"),
            Types::Bool => write!(f, "bool"),
            Types::Applicable(ret, args) => {
                let args = args.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "({}) -> {}", args.as_str(), ret.as_ref())
            }
        }
    }
}

impl Types {
    pub fn with_loc(self, loc: Loc) -> Type {
        Type {
            ty: self,
            loc,
        }
    }

    pub fn into_type(self) -> Type {
        Type {
            ty: self,
            loc: Loc::Injected,
        }
    }
}

impl Type {
    pub fn not_void(&self) -> CompileResult<()> {
        match self.ty {
            Types::Void => raise_bottom_typed_expr_error(self.loc.clone()),
            _ => Ok(())
        }
    }

    pub fn is_applicable(&self, loc: &Loc) -> CompileResult<()> {
        match self.ty {
            Types::Any |
            Types::Applicable(_, _) => Ok(()),
            _ => raise_type_mismatch_error(loc.clone(), None, self.clone())
        }
    }

    pub fn against(&self, expected: &Type) -> CompileResult<Type> {
        match (&self.ty, &expected.ty) {
            (Types::Any, t) |
            (t, Types::Any) if *t != Types::Void => Ok(self.clone()),

            (lhs, rhs) if lhs == rhs => Ok(self.clone()),

            _ => {
                raise_type_mismatch_error(self.loc.clone(),
                                          Some(expected.clone()),
                                          self.clone())?;
                unreachable!()
            }
        }
    }
}

pub fn check_expr(ctx: &mut CheckContext, expr: &Expr) -> CompileResult<Type> {
    match expr {
        Expr::Literal(loc, lit) => check_lit(ctx, loc.clone(), lit),
        Expr::Alloc(loc, _, _) => Ok(Types::Any.with_loc(loc.clone())),

        Expr::Lambda(loc, _, params, body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_params(ctx, params)?;
            let args = params.iter()
                .map(|p| Types::Any.with_loc(p.to_loc()))
                .collect();
            let ret = check_expr(ctx, body.as_ref())?;
            ctx.leave_scope();
            Ok(Types::Applicable(Box::new(ret), args).with_loc(loc.clone()))
        }

        Expr::Id(id) => match ctx.lookup(id) {
            Some((_, t)) => {
                let mut t = t.clone();
                t.loc = id.to_loc();
                Ok(t)
            },
            _ => Ok(Types::Any.with_loc(id.to_loc())),
        },

        Expr::Group(loc, exprs) => {
            let mut t = Types::Void.into_type();
            for expr in exprs {
                t = check_expr(ctx, expr)?;
            }
            t.loc = loc.clone();
            Ok(t)
        }

        Expr::Assign(loc, _, lhs, rhs) => {
            check_expr(ctx, rhs.as_ref())?.not_void()?;
            let mut t = check_expr(ctx, lhs.as_ref())?;
            t.loc = loc.clone();
            Ok(t)
        }

        Expr::Apply(loc, id, args) => {
            let t = check_expr(ctx, id.as_ref())?;
            t.is_applicable(&id.as_ref().to_loc())?;
            match t.ty {
                Types::Any => Ok(Types::Any.with_loc(loc.clone())),
                Types::Applicable(ret, types) => {
                    if args.len() != types.len() {
                        raise_argc_mismatch_error(loc.clone(),
                                                  types.len(),
                                                  args.len())?;
                    }
                    for (arg, t) in args.iter().zip(types) {
                        check_expr(ctx, arg)?.against(&t)?;
                    }
                    let mut ret = ret.as_ref().clone();
                    ret.loc = loc.clone();
                    Ok(ret)
                }
                _ => unreachable!(),
            }
        }

        Expr::Unary(loc, op, expr) => {
            let t = check_expr(ctx, expr.as_ref())?;
            t.not_void()?;
            match op {
                Op::Not => t.against(&Types::Bool.into_type()),
                _ => Ok(Types::Any.with_loc(loc.clone()))
            }
        }

        Expr::Binary(loc, op, lhs, rhs) => {
            let mut t = check_expr(ctx, lhs.as_ref())?;
            t.loc = loc.clone();
            t.not_void()?;
            match op {
                Op::Access => {
                    check_expr(ctx, rhs.as_ref())?.not_void()?;
                    Ok(Types::Any.with_loc(loc.clone()))
                }
                Op::Index => {
                    check_expr(ctx, rhs.as_ref())?.against(&Types::Number.into_type())?;
                    Ok(Types::Any.with_loc(loc.clone()))
                }
                _ => {
                    check_expr(ctx, rhs.as_ref())?.against(&t)?;
                    Ok(t)
                }
            }
        }

        Expr::Ternary(loc, cond, t, f) => {
            check_expr(ctx, cond.as_ref())?.against(&Types::Bool.into_type())?;
            let t_ret = check_expr(ctx, t.as_ref())?;
            t_ret.not_void()?;
            check_expr(ctx, f.as_ref())?.against(&t_ret)?;
            Ok(Types::Any.with_loc(loc.clone()))
        }

        Expr::Question(loc, value, null) => {
            check_expr(ctx, value.as_ref())?.not_void()?;
            check_expr(ctx, null.as_ref())?.not_void()?;
            Ok(Types::Any.with_loc(loc.clone()))
        }
    }
}

fn check_lit(_: &mut CheckContext, loc: Loc, lit: &Lit) -> CompileResult<Type> {
    match lit {
        Lit::Number(_) => Ok(Types::Number.with_loc(loc)),
        Lit::Bool(_) => Ok(Types::Bool.with_loc(loc)),
        Lit::Char(_) => Ok(Types::Char.with_loc(loc)),
        Lit::Str(_) => Ok(Types::String.with_loc(loc)),
        _ => Ok(Types::Any.with_loc(loc)),
    }
}
