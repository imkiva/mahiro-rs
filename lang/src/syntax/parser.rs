use lalrpop_util::{lexer::Token, ParseError as PE, ParseError};

use crate::{
  syntax::{
    parser::utils::{State, UserError},
    tree::Module,
  },
  CompileError, CompileResult,
};

lalrpop_mod!(pub grammar);
pub use grammar::*;

pub struct MahiroParser;

#[derive(Debug, Clone)]
pub struct MahiroParseError {
  pub start: usize,
  pub end: usize,
  pub kind: MahiroParseErrorKind,
}

#[derive(Debug, Clone)]
pub enum MahiroParseErrorKind {
  InvalidToken,
  UnexpectedEOF,
  UnexpectedToken(String),
  LiteralTooLarge,
}

impl MahiroParser {
  pub fn parse_module(src: &str) -> CompileResult<Module> {
    let mut state = State::new();
    match ModuleParser::new().parse(&mut state, src) {
      Ok(module) => {
        let mut module: Module = module;
        let mut decl = state.extra_decl();
        decl.extend(module.decl.into_iter());
        module.decl = decl;
        Ok(module)
      }
      Err(e) => {
        let e: PE<usize, Token, UserError> = e;
        match e {
          ParseError::InvalidToken { location: l } => {
            Err(CompileError::ParseError(MahiroParseError {
              start: l,
              end: l,
              kind: MahiroParseErrorKind::InvalidToken,
            }))
          }
          ParseError::UnrecognizedEOF {
            location: l,
            expected: _,
          } => Err(CompileError::ParseError(MahiroParseError {
            start: l,
            end: l,
            kind: MahiroParseErrorKind::UnexpectedEOF,
          })),
          ParseError::UnrecognizedToken {
            token: (start, t, end),
            expected: _,
          }
          | ParseError::ExtraToken {
            token: (start, t, end),
          } => Err(CompileError::ParseError(MahiroParseError {
            start,
            end,
            kind: MahiroParseErrorKind::UnexpectedToken(t.to_string()),
          })),
          ParseError::User {
            error: UserError { kind, location: l },
          } => Err(CompileError::ParseError(MahiroParseError {
            start: l,
            end: l,
            kind,
          })),
        }
      }
    }
  }
}

mod utils {
  use crate::{
    sugar::async_fn::DesugarAsync,
    syntax::{
      parser::MahiroParseErrorKind,
      tree::{Decl, Expr, FnSig, Ident, Stmt},
    },
  };

  #[derive(Debug, Clone)]
  pub struct State {
    extra_decl: Vec<Decl>,
  }

  #[derive(Debug, Clone)]
  pub struct UserError {
    pub kind: MahiroParseErrorKind,
    pub location: usize,
  }

  #[derive(Debug, Clone)]
  pub enum Postfix {
    Apply(Vec<Expr>),
    Member(Ident),
    Index(Expr),
  }

  impl State {
    pub fn new() -> Self {
      State { extra_decl: vec![] }
    }

    pub fn extra_decl(self) -> Vec<Decl> {
      self.extra_decl
    }

    pub fn desugar_async_fn(&mut self, sig: &FnSig, body: Vec<Stmt>) -> Vec<Stmt> {
      let (new_body, decl) = DesugarAsync::desugar_async(sig, body);
      self.extra_decl.extend(decl.into_iter());
      new_body
    }
  }
}
