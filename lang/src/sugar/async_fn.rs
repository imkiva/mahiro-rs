use crate::syntax::tree::{Decl, FnSig, Stmt};

pub struct DesugarAsync;

impl DesugarAsync {
  pub fn desugar_async(sig: &FnSig, body: Vec<Stmt>) -> (Vec<Stmt>, Vec<Decl>) {
    (body, vec![])
  }
}
