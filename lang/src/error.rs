use std::fmt::{Display, Formatter};

use crate::{
  syntax::parser::{MahiroParseError, MahiroParseErrorKind},
  CompileError,
};

impl CompileError {
  pub fn error_message(&self, input: &str, path: &str) -> String {
    match self {
      CompileError::ParseError(e) => self.format_parse_error(e, input, path),
    }
  }

  fn format_parse_error(&self, e: &MahiroParseError, input: &str, path: &str) -> String {
    type Error = pest::error::Error<()>;
    type ErrorVariant = pest::error::ErrorVariant<()>;
    let start = pest::Position::new(input, e.start).unwrap();
    let end = pest::Position::new(input, e.end).unwrap();
    let error = Error::new_from_span(
      ErrorVariant::CustomError {
        message: format!("{}", e.kind),
      },
      start.span(&end),
    );
    format!("{}", error.with_path(path))
  }
}

impl Display for MahiroParseErrorKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      MahiroParseErrorKind::InvalidToken => write!(f, "Invalid token"),
      MahiroParseErrorKind::UnexpectedEOF => write!(f, "Unexpected EOF"),
      MahiroParseErrorKind::UnexpectedToken(t) => write!(f, "Unexpected '{}'", t),
      MahiroParseErrorKind::LiteralTooLarge => write!(f, "Literal is too large"),
    }
  }
}
