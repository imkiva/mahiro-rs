use crate::CompileResult;
use crate::syntax::tree::{Program, Entry, Header, Stmt, VarInit, Ident, Expr, Param, Body, Case, Loc, ToLoc};
use crate::check::context::{CheckContext, ScopeId};
use crate::error::CompileError;
use crate::check::{CheckError, CheckErrorVariant};
use crate::check::infer_check::Type;

pub fn checker_main(input: &Program) -> CompileResult<()> {
    let mut ctx = CheckContext::new();
    ctx.enter_scope(ScopeId::Global);
    check_program(&mut ctx, input)?;
    ctx.leave_scope();
    Ok(())
}

fn check_program(ctx: &mut CheckContext, prog: &Program) -> CompileResult<()> {
    for entry in prog {
        match entry {
            Entry::HeaderEntry(hdr) => check_header(ctx, hdr)?,
            Entry::StmtEntry(stmt) => {
                check_stmt(ctx, stmt)?;
                ()
            }
        }
    }
    Ok(())
}

fn check_header(ctx: &mut CheckContext, hdr: &Header) -> CompileResult<()> {
    match hdr {
        Header::Package(_) => Ok(()),
        Header::Using(_) => Ok(()),
        Header::Import(_, Some(id)) => {
            check_redefinition(ctx, id, &Type::Any)
        }
        Header::Import(_, _) => unreachable!("Desugar bug"),
    }
}

fn check_stmt(ctx: &mut CheckContext, stmt: &Stmt) -> CompileResult<Type> {
    match stmt {
        Stmt::Var(var) => {
            check_var_init(ctx, var)?;
            Ok(Type::Void)
        }

        Stmt::VarList(vars) => {
            check_vars_init(ctx, vars)?;
            Ok(Type::Void)
        }

        Stmt::Func(id, params, body) => {
            // first of all, build a fake function type
            let args: Vec<Type> = params.iter().map(|_| Type::Any).collect();
            check_redefinition(ctx, id, &Type::Applicable(Box::new(Type::Any), args.clone()))?;

            // second, enter the function scope and check params
            ctx.enter_scope(ScopeId::Func(id.text.clone()));
            check_params(ctx, params)?;

            // now check the body, if recursion happens, we can only check
            // whether argument count is matched this time.
            let ret = check_body(ctx, body)?;

            // replace the fake function type with our new one.
            // this is defined in the function's scope, so
            // no redefinition will be reported.
            check_redefinition(ctx, id, &Type::Applicable(Box::new(ret), args.clone()))?;

            // recheck the body and get the real return type
            // in theory, this is meaningful only when:
            // 1. recursion happens
            // 2. body redefines a var whose name is same with function's
            let ret = check_body(ctx, body)?;

            // leave the function scope and overwrite the fake function type
            // we shouldn't use check_redefinition because what we are doing
            // is actually redefining the function itself.
            ctx.leave_scope();
            ctx.define_in_current(id, &Type::Applicable(Box::new(ret), args.clone()));
            Ok(Type::Void)
        }

        Stmt::Namespace(id, body) => {
            check_redefinition(ctx, id, &Type::Any)?;
            ctx.enter_scope(ScopeId::Namespace(id.text.clone()));
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::Struct(id, extends, body) => {
            // TODO: store struct information
            check_redefinition(ctx, id, &Type::Any)?;
            if let Some(expr) = extends {
                check_expr(ctx, expr)?;
            }
            ctx.enter_scope(ScopeId::Struct(id.text.clone()));
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::Block(body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::Return(expr) => {
            if let Some(expr) = expr {
                let t = check_expr(ctx, expr)?;
                t.not_void(expr.to_loc())?;
                Ok(t)
            } else {
                Ok(Type::Void)
            }
        }

        Stmt::Throw(expr) => {
            check_expr(ctx, expr)?.not_void(expr.to_loc())?;
            Ok(Type::Void)
        }

        Stmt::Try(tbody, id, cbody) => {
            // check try-body
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_body(ctx, tbody)?;
            ctx.leave_scope();
            // check catch-body
            ctx.enter_scope(ScopeId::UnnamedBlock);
            // fyou dynamic typing! exception type?
            check_redefinition(ctx, id, &Type::Any)?;
            check_body(ctx, cbody)?;
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::If(cond, t, f) => {
            check_expr(ctx, cond)?.against(Type::Bool, cond.to_loc())?;
            // check if-true branch
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_body(ctx, t)?;
            ctx.leave_scope();
            // check if-false branch
            if let Some(else_body) = f {
                ctx.enter_scope(ScopeId::UnnamedBlock);
                check_body(ctx, else_body)?;
                ctx.leave_scope();
            }
            Ok(Type::Void)
        }

        Stmt::Switch(expr, cases) => {
            check_expr(ctx, expr)?;
            check_switch_cases(ctx, cases)?;
            Ok(Type::Void)
        }

        Stmt::While(cond, body) => {
            check_expr(ctx, cond)?.against(Type::Bool, cond.to_loc())?;
            ctx.enter_scope(ScopeId::UnnamedBlock);
            ctx.enter_loop();
            check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::Loop(cond, body) => {
            if let Some(cond) = cond {
                check_expr(ctx, cond)?.against(Type::Bool, cond.to_loc())?;
            }
            ctx.enter_scope(ScopeId::UnnamedBlock);
            ctx.enter_loop();
            check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::For(id, init, cond, step, body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let t = check_expr(ctx, init)?;
            check_redefinition(ctx, id, &t)?;
            check_expr(ctx, cond)?.against(Type::Bool, cond.to_loc())?;
            check_expr(ctx, step)?;
            ctx.enter_loop();
            check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(Type::Void)
        }

        Stmt::Break(id) |
        Stmt::Continue(id) => {
            if !ctx.is_in_loop() {
                raise_dangling_loop_control_error(id)?;
            }
            Ok(Type::Void)
        }

        Stmt::ExprStmt(expr) => check_expr(ctx, expr),

        Stmt::ForEach(_, _, _) => unreachable!("Desugar bug"),
    }
}

fn check_vars_init(ctx: &mut CheckContext, vars_init: &Vec<VarInit>) -> CompileResult<()> {
    for var in vars_init {
        check_var_init(ctx, var)?;
    }
    Ok(())
}

fn check_var_init(ctx: &mut CheckContext, var_init: &VarInit) -> CompileResult<()> {
    match var_init {
        VarInit::Simple(id, expr) => {
            let t = check_expr(ctx, expr)?;
            check_redefinition(ctx, id, &t)?;
            Ok(())
        }
        VarInit::Structured(ids, expr) => {
            // fyou dynamic typing!
            // we can do nothing with this type!
            let _ = check_expr(ctx, expr)?;
            for id in ids {
                // fyou dynamic typing!
                // we can't know what's inside at compile time
                check_redefinition(ctx, id, &Type::Any)?;
            }
            Ok(())
        }
    }
}

pub(crate) fn check_params(ctx: &mut CheckContext, params: &Vec<Param>) -> CompileResult<()> {
    for param in params {
        match param {
            Param::Normal(id) => check_redefinition(ctx, id, &Type::Any)?,
            Param::Varargs(id) => check_redefinition(ctx, id, &Type::Any)?,
        }
    }
    Ok(())
}

fn check_body(ctx: &mut CheckContext, body: &Body) -> CompileResult<Type> {
    let mut t = Type::Void;
    for stmt in body {
        let new = check_stmt(ctx, stmt)?;
        if new != Type::Void {
            t = new;
        }
    }
    Ok(t)
}

fn check_switch_cases(ctx: &mut CheckContext, cases: &Vec<Case>) -> CompileResult<()> {
    for case in cases {
        match case {
            Case::Sth(expr, body) => {
                check_expr(ctx, expr)?;
                ctx.enter_scope(ScopeId::UnnamedBlock);
                check_body(ctx, body)?;
                ctx.leave_scope();
            }
            Case::Dft(body) => {
                ctx.enter_scope(ScopeId::UnnamedBlock);
                check_body(ctx, body)?;
                ctx.leave_scope();
            }
        }
    }
    Ok(())
}

fn check_expr(ctx: &mut CheckContext, expr: &Expr) -> CompileResult<Type> {
    crate::check::infer_check::check_expr(ctx, expr)
}

fn check_redefinition(ctx: &mut CheckContext, id: &Ident, idtype: &Type) -> CompileResult<()> {
    match ctx.define_in_current(id, idtype) {
        Some(prev) => raise_redefinition_error(prev, id),
        None => Ok(()),
    }
}

#[inline]
pub(crate) fn raise_redefinition_error(prev: &Ident, curr: &Ident) -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::Redefinition(
            prev.abs_loc, curr.abs_loc, curr.text.clone(),
        )
    )))
}

#[inline]
pub(crate) fn raise_dangling_loop_control_error(ident: &Ident) -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::DanglingLoopControl(
            ident.abs_loc, ident.text.clone(),
        )
    )))
}

#[inline]
pub(crate) fn raise_bottom_typed_expr_error(loc: Loc) -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::BottomTypedExpr(
            loc
        )
    )))
}

#[inline]
pub(crate) fn raise_type_mismatch_error(loc: Loc, expected: Option<Type>, actual: Type)
                                        -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::TypeMismatch(
            loc, expected, actual,
        )
    )))
}

#[inline]
pub(crate) fn raise_argc_mismatch_error(loc: Loc, expected: usize, actual: usize)
                                        -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::ArgcMismatch(
            loc, expected, actual,
        )
    )))
}

