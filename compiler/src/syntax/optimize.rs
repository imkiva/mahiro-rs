use crate::syntax::tree::{Program, Expr, Body};
use crate::syntax::fold::{FoldContext, Eliminable};
use crate::syntax::tree::Stmt::{Var, VarList, Break, Continue, ExprStmt};

pub enum OptimizeLevel {
    /// Disable any kind of optimization
    Disabled,
    /// Only the most common optimizations are allowed
    Basic,
    /// Optimize as much as possible
    Aggressive,
    /// Just do it! Leave me alone!
    JustDoIt,
}

pub struct Optimizer;

impl Optimizer {
    pub fn run(input: Program, level: OptimizeLevel) -> Program {
        match &level {
            OptimizeLevel::Disabled => input,
            OptimizeLevel::Basic => input.eliminate(),
            OptimizeLevel::Aggressive |
            OptimizeLevel::JustDoIt => {
                let mut ctx = OptimizeContext::new(level);
                ctx.prepare(&input);
                input.eliminate_with(Some(&ctx))
            }
        }
    }
}

struct OptimizeContext {
    level: OptimizeLevel
}

impl OptimizeContext {
    fn new(level: OptimizeLevel) -> OptimizeContext {
        OptimizeContext {
            level
        }
    }

    fn prepare(&mut self, _program: &Program) {}
}

impl FoldContext for OptimizeContext {
    fn try_resolve_constant(&self, _name: &str) -> Option<Expr> {
        None
    }

    fn needs_new_scope(&self, body: &Body) -> bool {
        // this is used for `unblock`
        // if the body's scope is same with parent's
        // enabled if level >= OptimizeLevel::Aggressive
        match self.level {
            OptimizeLevel::Aggressive |
            OptimizeLevel::JustDoIt => needs_new_scope(body),
            _ => false
        }
    }
}

fn needs_new_scope(body: &Body) -> bool {
    for stmt in body {
        match stmt {
            ExprStmt(_) => (),
            _ => return true,
        }
    }
    false
}
