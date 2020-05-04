use crate::syntax::tree::{Stmt, Param, Body, VarInit, Expr, Lit};
use crate::ir::data::{Function, Struct};
use crate::ir::cfg::CodeUnit;
use crate::ir::asm::MacroAssembler;
use crate::ir::{IR, PoolIndex};

#[derive(Debug, Clone)]
struct Translator {
    masm: MacroAssembler,
    string_pool: Vec<String>,
    funcs: Vec<Function>,
    structs: Vec<Struct>,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            masm: MacroAssembler::new(),
            string_pool: Vec::new(),
            funcs: Vec::new(),
            structs: Vec::new(),
        }
    }

    pub fn const_string_load(&mut self, str: &str) {
        let idx = self.new_string_constant(str);
        self.emit(IR::ConstStringLoad(idx));
    }

    pub fn emit(&mut self, ir: IR) {
        self.masm.emit(ir);
    }

    pub fn new_string_constant(&mut self, str: &str) -> PoolIndex {
        let idx = self.string_pool.len() as PoolIndex;
        self.string_pool.push(str.to_owned());
        idx
    }

    pub fn asm(&mut self) -> &mut MacroAssembler {
        &mut self.masm
    }
}

fn translate_func(func: Stmt) -> Function {
    let (name, params, body) =
        match func {
            Stmt::Func(name, params, body)
            => (name, params, body),
            _ => unreachable!("not a func"),
        };

    let mut compiled = Function::new();
    compiled.name = name.text;
    compiled.argc = params.len();
    compiled.code = translate_func_body(params, body);
    compiled
}

fn translate_func_body(params: Vec<Param>, body: Body) -> CodeUnit {
    let mut tr = Translator::new();
    for stmt in body {
        match stmt {
            Stmt::Var(var) => translate_var_init(&mut tr, &var),
            Stmt::VarList(vars) =>
                vars.iter().for_each(|v|
                    translate_var_init(&mut tr, v)),

            Stmt::Block(_) => {}
            Stmt::Return(_, _) => {}
            Stmt::Throw(_) => {}
            Stmt::Try(_, _, _) => {}
            Stmt::If(_, _, _) => {}
            Stmt::Switch(_, _) => {}
            Stmt::While(_, _) => {}
            Stmt::Loop(_, _) => {}
            Stmt::For(_, _, _, _, _) => {}
            Stmt::ForEach(_, _, _) => {}
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::ExprStmt(_) => {}
            _ => unreachable!("not func body"),
        }
    }

    CodeUnit::new()
}

fn translate_var_init(tr: &mut Translator, var: &VarInit) {
    match var {
        VarInit::Simple(name, expr) => {
            translate_expr(tr, expr);
            let idx = tr.asm().new_local(name.text.as_str());
            tr.emit(IR::LocalStore(idx));
        }

        VarInit::Structured(names, expr) => {
            translate_expr(tr, expr);
            for (name, index) in names.iter().zip(0..names.len()) {
                let idx = tr.asm().new_local(name.text.as_str());
                tr.emit(IR::Dup);
                tr.emit(IR::Const16(index as i16));
                tr.emit(IR::ArrayLoad);
                tr.emit(IR::LocalStore(idx));
            }
            tr.emit(IR::Pop);
        }
    }
}

fn translate_expr(tr: &mut Translator, expr: &Expr) {
    match expr {
        Expr::Literal(_, lit) => translate_lit(tr, lit),

        Expr::Lambda(_, _, _, _) => {}

        Expr::Alloc(_, ty, args) => {
            args.iter().rev().for_each(|arg| translate_expr(tr, arg));
            translate_expr(tr, ty.as_ref());
            tr.emit(IR::Const16(args.len() as i16));
            tr.emit(IR::New);
        }

        Expr::Id(name) => {
            match tr.asm().find_local(name.text.as_str()) {
                Some(idx) => tr.emit(IR::LocalLoad(idx)),
                _ => {
                    let idx = tr.new_string_constant(name.text.as_str());
                    tr.emit(IR::Resolve(idx));
                },
            }
        }

        Expr::Group(_, exprs) => {
            exprs.iter()
                .take(exprs.len() - 1)
                .for_each(|expr| {
                    translate_expr(tr, expr);
                    tr.emit(IR::Pop);
                });
            translate_expr(tr, exprs.last().unwrap());
        }

        Expr::Assign(_, _, _, _) => {}
        Expr::Apply(_, _, _) => {}
        Expr::Unary(_, _, _) => {}
        Expr::Binary(_, _, _, _) => {}
        Expr::Ternary(_, _, _, _) => {}
        Expr::Question(_, _, _) => {}
    }
}

fn translate_lit(tr: &mut Translator, lit: &Lit) {
    match lit {
        Lit::Number(n) => tr.emit(IR::ConstNum(*n)),
        Lit::Bool(b) => tr.emit(IR::Const16(if *b { 1 } else { 0 })),
        Lit::Char(c) => tr.emit(IR::Const16(*c as u32 as i16)),
        Lit::Str(s) => tr.const_string_load(s.as_str()),
        Lit::Null => tr.emit(IR::ConstNull),
        _ => unreachable!("Desugar bug")
    }
}


