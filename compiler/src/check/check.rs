use crate::CompileResult;
use crate::syntax::tree::{Program, Entry, Header, Stmt, VarInit, Ident, Expr, Param, Body, Case, Loc, ToLoc};
use crate::check::context::{CheckContext, ScopeId};
use crate::error::CompileError;
use crate::check::{CheckError, CheckErrorVariant};
use crate::check::infer_check::{Type, Types};

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
            check_redefinition(ctx, id, &Types::Any.into_type())
        }
        Header::Import(_, _) => unreachable!("Desugar bug"),
    }
}

fn check_stmt(ctx: &mut CheckContext, stmt: &Stmt) -> CompileResult<Type> {
    match stmt {
        Stmt::Var(var) => {
            check_var_init(ctx, var)?;
            Ok(Types::Void.into_type())
        }

        Stmt::VarList(vars) => {
            check_vars_init(ctx, vars)?;
            Ok(Types::Void.into_type())
        }

        Stmt::Func(id, params, body) => {
            // first of all, build a fake function type
            let args: Vec<Type> = params.iter()
                .map(|p| Types::Any.with_loc(p.to_loc()))
                .collect();
            check_redefinition(ctx, id, &Types::Applicable(
                Box::new(Types::Any.into_type()), args.clone()).with_loc(id.to_loc()))?;

            // second, enter the function scope and check params
            ctx.enter_scope(ScopeId::Func(id.text.clone()));
            check_params(ctx, params)?;

            // now check the body, if recursion happens, we can only check
            // whether argument count is matched this time.
            let ret = check_body(ctx, body)?;

            // replace the fake function type with our new one.
            // this is defined in the function's scope, so
            // no redefinition will be reported.
            check_redefinition(ctx, id, &Types::Applicable(Box::new(ret), args.clone())
                .with_loc(id.to_loc()))?;

            // recheck the body and get the real return type
            // in theory, this is meaningful only when:
            // 1. recursion happens
            // 2. body redefines a var whose name is same with function's
            let ret = check_body(ctx, body)?;

            // leave the function scope and overwrite the fake function type
            // we shouldn't use check_redefinition because what we are doing
            // is actually redefining the function itself.
            ctx.leave_scope();
            ctx.rewrite_in_current(id,
                                   &Types::Applicable(Box::new(ret), args.clone())
                                       .with_loc(id.to_loc()));
            Ok(Types::Void.into_type())
        }

        Stmt::Namespace(id, body) => {
            check_redefinition(ctx, id, &Types::Any.with_loc(id.to_loc()))?;
            ctx.enter_scope(ScopeId::Namespace(id.text.clone()));
            let t = check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(t)
        }

        Stmt::Struct(id, extends, body) => {
            // TODO: store struct information
            check_redefinition(ctx, id, &Types::Any.with_loc(id.to_loc()))?;
            if let Some(expr) = extends {
                check_expr(ctx, expr)?;
            }
            ctx.enter_scope(ScopeId::Struct(id.text.clone()));
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(Types::Void.into_type())
        }

        Stmt::Block(body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let t = check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(t)
        }

        Stmt::Return(loc, expr) => {
            if let Some(expr) = expr {
                let mut t = check_expr(ctx, expr)?;
                t.loc = expr.to_loc();
                t.not_void()?;
                Ok(t)
            } else {
                Ok(Types::Void.with_loc(loc.clone()))
            }
        }

        Stmt::Throw(expr) => {
            check_expr(ctx, expr)?.not_void()?;
            Ok(Types::Void.into_type())
        }

        Stmt::Try(tbody, id, cbody) => {
            // check try-body
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let t_ret = check_body(ctx, tbody)?;
            ctx.leave_scope();
            // check catch-body
            ctx.enter_scope(ScopeId::UnnamedBlock);
            // fyou dynamic typing! exception type?
            check_redefinition(ctx, id, &Types::Any.with_loc(id.to_loc()))?;
            let c_ret = check_body(ctx, cbody)?;
            c_ret.against(&t_ret)?;
            ctx.leave_scope();
            Ok(t_ret)
        }

        Stmt::If(cond, t, f) => {
            check_expr(ctx, cond)?.against(&Types::Bool.into_type())?;
            // check if-true branch
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let t_ret = check_body(ctx, t)?;
            ctx.leave_scope();
            // check if-false branch
            if let Some(else_body) = f {
                ctx.enter_scope(ScopeId::UnnamedBlock);
                let f_ret = check_body(ctx, else_body)?;
                f_ret.against(&t_ret)?;
                ctx.leave_scope();
            }

            Ok(t_ret)
        }

        Stmt::Switch(expr, cases) => {
            check_expr(ctx, expr)?.not_void()?;
            let t = check_switch_cases(ctx, cases)?;
            Ok(t)
        }

        Stmt::While(cond, body) => {
            check_expr(ctx, cond)?.against(&Types::Bool.into_type())?;
            ctx.enter_scope(ScopeId::UnnamedBlock);
            ctx.enter_loop();
            let t = check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(t)
        }

        Stmt::Loop(cond, body) => {
            if let Some(cond) = cond {
                check_expr(ctx, cond)?.against(&Types::Bool.into_type())?;
            }
            ctx.enter_scope(ScopeId::UnnamedBlock);
            ctx.enter_loop();
            let t = check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(t)
        }

        Stmt::For(id, init, cond, step, body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let t = check_expr(ctx, init)?;
            check_redefinition(ctx, id, &t)?;
            check_expr(ctx, cond)?.against(&Types::Bool.into_type())?;
            check_expr(ctx, step)?;
            ctx.enter_loop();
            let t = check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(t)
        }

        Stmt::Break(id) |
        Stmt::Continue(id) => {
            if !ctx.is_in_loop() {
                raise_dangling_loop_control_error(id)?;
            }
            Ok(Types::Void.into_type())
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
            let mut t = check_expr(ctx, expr)?;
            t.loc = expr.to_loc();
            t.not_void()?;
            check_redefinition(ctx, id, &t)?;
            Ok(())
        }
        VarInit::Structured(ids, expr) => {
            // fyou dynamic typing!
            // we can do nothing with this type!
            let mut t = check_expr(ctx, expr)?;
            t.loc = expr.to_loc();
            t.not_void()?;
            for id in ids {
                // fyou dynamic typing!
                // we can't know what's inside at compile time
                check_redefinition(ctx, id, &Types::Any.with_loc(id.to_loc()))?;
            }
            Ok(())
        }
    }
}

pub(crate) fn check_params(ctx: &mut CheckContext, params: &Vec<Param>) -> CompileResult<()> {
    for param in params {
        match param {
            Param::Normal(id) =>
                check_redefinition(ctx, id, &Types::Any.with_loc(id.to_loc()))?,
            Param::Varargs(id) =>
                check_redefinition(ctx, id, &Types::Any.with_loc(id.to_loc()))?,
        }
    }
    Ok(())
}

fn check_body(ctx: &mut CheckContext, body: &Body) -> CompileResult<Type> {
    // TODO: block location
    let mut t = Types::Void.into_type();
    for stmt in body {
        let new = check_stmt(ctx, stmt)?;
        if new.ty != Types::Void {
            t = new;
        }
    }
    Ok(t)
}

fn check_switch_cases(ctx: &mut CheckContext, cases: &Vec<Case>) -> CompileResult<Type> {
    let mut t = Types::Any.into_type();
    for case in cases {
        let new = check_switch_case(ctx, case)?;
        new.against(&t)?;
        t = new;
    }
    Ok(t)
}

fn check_switch_case(ctx: &mut CheckContext, case: &Case) -> CompileResult<Type> {
    match case {
        Case::Sth(expr, body) => {
            check_expr(ctx, expr)?.not_void()?;
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let new = check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(new)
        }
        Case::Dft(body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            let new = check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(new)
        }
    }
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

