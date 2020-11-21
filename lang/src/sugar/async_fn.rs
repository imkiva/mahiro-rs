use crate::syntax::tree::{Decl, FnSig, Stmt, Expr, Param};

pub struct DesugarAsync;

impl DesugarAsync {
  pub fn desugar_async(_: &FnSig, _: Vec<Stmt>) -> (Vec<Stmt>, Vec<Decl>) {
    unimplemented!("async/await is not supported yet")
  }
}
