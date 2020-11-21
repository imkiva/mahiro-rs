use crate::syntax::tree::{Decl, FnSig, Stmt, Expr, Param};

pub struct DesugarAsync;

impl DesugarAsync {
  pub fn desugar_async(sig: &FnSig, body: Vec<Stmt>) -> (Vec<Stmt>, Vec<Decl>) {
    (body, vec![])
  }
}
