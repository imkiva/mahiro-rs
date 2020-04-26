use crate::CompileResult;
use crate::syntax::tree::{Program, Entry, Header, Stmt, VarInit, Ident, Expr, Param, Body, Case};
use crate::check::ctx::{CheckContext, ScopeId};
use crate::error::CompileError;
use crate::check::{CheckError, CheckErrorVariant};

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
            Entry::StmtEntry(stmt) => check_stmt(ctx, stmt)?,
        }
    }
    Ok(())
}

fn check_header(ctx: &mut CheckContext, hdr: &Header) -> CompileResult<()> {
    match hdr {
        Header::Package(_) => Ok(()),
        Header::Using(_) => Ok(()),
        Header::Import(_, Some(id)) => {
            check_redefinition(ctx, id)
        }
        Header::Import(_, _) => unreachable!("Desugar bug"),
    }
}

fn check_stmt(ctx: &mut CheckContext, stmt: &Stmt) -> CompileResult<()> {
    match stmt {
        Stmt::Var(var) => check_var_init(ctx, var),
        Stmt::VarList(vars) => check_vars_init(ctx, vars),
        Stmt::Func(id, params, body) => {
            check_redefinition(ctx, id)?;
            ctx.enter_scope(ScopeId::Func(id.text.clone()));
            check_params(ctx, params)?;
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(())
        }
        Stmt::Namespace(id, body) => {
            check_redefinition(ctx, id)?;
            ctx.enter_scope(ScopeId::Namespace(id.text.clone()));
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(())
        }
        Stmt::Struct(id, extends, body) => {
            check_redefinition(ctx, id)?;
            if let Some(expr) = extends {
                check_expr(ctx, expr)?;
            }
            ctx.enter_scope(ScopeId::Struct(id.text.clone()));
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(())
        }
        Stmt::Block(body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_body(ctx, body)?;
            ctx.leave_scope();
            Ok(())
        }
        Stmt::Return(expr) => {
            if let Some(expr) = expr {
                check_expr(ctx, expr)?;
            }
            Ok(())
        }
        Stmt::Throw(expr) => check_expr(ctx, expr),
        Stmt::Try(tbody, id, cbody) => {
            // check try-body
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_body(ctx, tbody)?;
            ctx.leave_scope();
            // check catch-body
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_redefinition(ctx, id)?;
            check_body(ctx, cbody)?;
            ctx.leave_scope();
            Ok(())
        }
        Stmt::If(cond, t, f) => {
            check_expr(ctx, cond)?;
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
            Ok(())
        }
        Stmt::Switch(expr, cases) => {
            check_expr(ctx, expr)?;
            check_switch_cases(ctx, cases)
        }
        Stmt::While(cond, body) => {
            check_expr(ctx, cond)?;
            ctx.enter_scope(ScopeId::UnnamedBlock);
            ctx.enter_loop();
            check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(())
        }
        Stmt::Loop(cond, body) => {
            if let Some(cond) = cond {
                check_expr(ctx, cond)?;
            }
            ctx.enter_scope(ScopeId::UnnamedBlock);
            ctx.enter_loop();
            check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(())
        }
        Stmt::For(id, init, cond, step, body) => {
            ctx.enter_scope(ScopeId::UnnamedBlock);
            check_redefinition(ctx, id)?;
            check_expr(ctx, init)?;
            check_expr(ctx, cond)?;
            check_expr(ctx, step)?;
            ctx.enter_loop();
            check_body(ctx, body)?;
            ctx.leave_loop();
            ctx.leave_scope();
            Ok(())
        }
        Stmt::Break(id) |
        Stmt::Continue(id) => {
            if ctx.is_in_loop() {
                Ok(())
            } else {
                raise_dangling_loop_control_error(id)
            }
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
            check_redefinition(ctx, id)?;
            check_expr(ctx, expr)
        }
        VarInit::Structured(ids, expr) => {
            for id in ids {
                check_redefinition(ctx, id)?;
            }
            check_expr(ctx, expr)
        }
    }
}

fn check_params(ctx: &mut CheckContext, params: &Vec<Param>) -> CompileResult<()> {
    for param in params {
        match param {
            Param::Normal(id) => check_redefinition(ctx, id)?,
            Param::Varargs(id) => check_redefinition(ctx, id)?,
        }
    }
    Ok(())
}

fn check_body(ctx: &mut CheckContext, body: &Body) -> CompileResult<()> {
    for stmt in body {
        check_stmt(ctx, stmt)?;
    }
    Ok(())
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

fn check_expr(_: &mut CheckContext, _: &Expr) -> CompileResult<()> {
    Ok(())
}

fn check_redefinition(ctx: &mut CheckContext, id: &Ident) -> CompileResult<()> {
    match ctx.define_in_current(id) {
        Some(prev) => raise_redefinition_error(prev, id),
        None => Ok(()),
    }
}

#[inline]
fn raise_redefinition_error(prev: &Ident, curr: &Ident) -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::Redefinition(
            prev.abs_loc, curr.abs_loc, curr.text.clone(),
        )
    )))
}

#[inline]
fn raise_dangling_loop_control_error(ident: &Ident) -> CompileResult<()> {
    Err(CompileError::CheckError(CheckError::new(
        CheckErrorVariant::DanglingLoopControl(
            ident.abs_loc, ident.text.clone(),
        )
    )))
}
