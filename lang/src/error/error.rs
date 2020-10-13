// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! Types for different kinds of parsing failures.

use crate::error::span::Span;
use std::{cmp, error, fmt, fmt::Debug, hash::Hash, mem};

/// Parse-related error type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error {
  /// Variant of the error
  pub variant: ErrorVariant,
  /// Location within the input string
  pub location: InputLocation,
  /// Line/column within the input string
  pub line_col: LineColLocation,
  path: Option<String>,
  line: String,
  continued_line: Option<String>,
}

/// Different kinds of parsing errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ErrorVariant {
  /// Custom error with a message
  CustomError {
    /// Short explanation
    message: String,
  },
}

/// Where an `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[allow(dead_code)]
pub enum InputLocation {
  /// `Error` was created by `Error::new_from_pos`
  Pos(usize),
  /// `Error` was created by `Error::new_from_span`
  Span((usize, usize)),
}

/// Line/column where an `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[allow(dead_code)]
pub enum LineColLocation {
  /// Line/column pair if `Error` was created by `Error::new_from_pos`
  Pos((usize, usize)),
  /// Line/column pairs if `Error` was created by `Error::new_from_span`
  Span((usize, usize), (usize, usize)),
}

impl Error {
  /// Creates `Error` from `ErrorVariant` and `Span`.
  ///
  /// # Examples
  ///
  /// ```
  /// # use pest::error::{Error, ErrorVariant};
  /// # use pest::{Position, Span};
  /// # #[allow(non_camel_case_types)]
  /// # #[allow(dead_code)]
  /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
  /// # enum Rule {
  /// #     open_paren,
  /// #     closed_paren
  /// # }
  /// # let input = "";
  /// # let start = Position::from_start(input);
  /// # let end = start.clone();
  /// # let span = start.span(&end);
  /// let error = Error::new_from_span(
  ///     ErrorVariant::ParsingError {
  ///         positives: vec![Rule::open_paren],
  ///         negatives: vec![Rule::closed_paren]
  ///     },
  ///     span
  /// );
  ///
  /// println!("{}", error);
  /// ```
  #[allow(clippy::needless_pass_by_value)]
  pub fn new_from_span(variant: ErrorVariant, span: Span) -> Error {
    let end = span.end_pos();

    let mut end_line_col = end.line_col();
    // end position is after a \n, so we want to point to the visual lf symbol
    if end_line_col.1 == 1 {
      let mut visual_end = end.clone();
      visual_end.skip_back(1);
      let lc = visual_end.line_col();
      end_line_col = (lc.0, lc.1 + 1);
    };

    let mut line_iter = span.lines();
    let start_line = visualize_whitespace(line_iter.next().unwrap_or(""));
    let continued_line = line_iter.last().map(visualize_whitespace);

    Error {
      variant,
      location: InputLocation::Span((span.start(), end.pos())),
      path: None,
      line: start_line,
      continued_line,
      line_col: LineColLocation::Span(span.start_pos().line_col(), end_line_col),
    }
  }

  /// Returns `Error` variant with `path` which is shown when formatted with
  /// `Display`.
  ///
  /// # Examples
  ///
  /// ```
  /// # use pest::error::{Error, ErrorVariant};
  /// # use pest::Position;
  /// # #[allow(non_camel_case_types)]
  /// # #[allow(dead_code)]
  /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
  /// # enum Rule {
  /// #     open_paren,
  /// #     closed_paren
  /// # }
  /// # let input = "";
  /// # let pos = Position::from_start(input);
  /// Error::new_from_pos(
  ///     ErrorVariant::ParsingError {
  ///         positives: vec![Rule::open_paren],
  ///         negatives: vec![Rule::closed_paren]
  ///     },
  ///     pos
  /// ).with_path("file.rs");
  /// ```
  pub fn with_path(mut self, path: &str) -> Error {
    self.path = Some(path.to_owned());

    self
  }

  fn start(&self) -> (usize, usize) {
    match self.line_col {
      LineColLocation::Pos(line_col) => line_col,
      LineColLocation::Span(start_line_col, _) => start_line_col,
    }
  }

  fn spacing(&self) -> String {
    let line = match self.line_col {
      LineColLocation::Pos((line, _)) => line,
      LineColLocation::Span((start_line, _), (end_line, _)) => cmp::max(start_line, end_line),
    };

    let line_str_len = format!("{}", line).len();

    let mut spacing = String::new();
    for _ in 0..line_str_len {
      spacing.push(' ');
    }

    spacing
  }

  fn underline(&self) -> String {
    let mut underline = String::new();

    let mut start = self.start().1;
    let end = match self.line_col {
      LineColLocation::Span(_, (_, mut end)) => {
        let inverted_cols = start > end;
        if inverted_cols {
          mem::swap(&mut start, &mut end);
          start -= 1;
          end += 1;
        }

        Some(end)
      }
      _ => None,
    };
    let offset = start - 1;
    let line_chars = self.line.chars();

    for c in line_chars.take(offset) {
      match c {
        '\t' => underline.push('\t'),
        _ => underline.push(' '),
      }
    }

    if let Some(end) = end {
      if end - start > 1 {
        underline.push('^');
        for _ in 2..(end - start) {
          underline.push('-');
        }
        underline.push('^');
      } else {
        underline.push('^');
      }
    } else {
      underline.push_str("^---")
    }

    underline
  }

  fn message(&self) -> String {
    match self.variant {
      ErrorVariant::CustomError { ref message } => message.clone(),
    }
  }

  pub(crate) fn format(&self) -> String {
    let spacing = self.spacing();
    let path = self
      .path
      .as_ref()
      .map(|path| format!("{}:", path))
      .unwrap_or_default();

    let pair = (self.line_col.clone(), &self.continued_line);
    if let (LineColLocation::Span(_, end), &Some(ref continued_line)) = pair {
      let has_line_gap = end.0 - self.start().0 > 1;
      if has_line_gap {
        format!(
          "{s    }--> {p}{ls}:{c}\n\
                     {s    } |\n\
                     {ls:w$} | {line}\n\
                     {s    } | ...\n\
                     {le:w$} | {continued_line}\n\
                     {s    } | {underline}\n\
                     {s    } |\n\
                     {s    } = {message}",
          s = spacing,
          w = spacing.len(),
          p = path,
          ls = self.start().0,
          le = end.0,
          c = self.start().1,
          line = self.line,
          continued_line = continued_line,
          underline = self.underline(),
          message = self.message()
        )
      } else {
        format!(
          "{s    }--> {p}{ls}:{c}\n\
                     {s    } |\n\
                     {ls:w$} | {line}\n\
                     {le:w$} | {continued_line}\n\
                     {s    } | {underline}\n\
                     {s    } |\n\
                     {s    } = {message}",
          s = spacing,
          w = spacing.len(),
          p = path,
          ls = self.start().0,
          le = end.0,
          c = self.start().1,
          line = self.line,
          continued_line = continued_line,
          underline = self.underline(),
          message = self.message()
        )
      }
    } else {
      format!(
        "{s}--> {p}{l}:{c}\n\
                 {s} |\n\
                 {l} | {line}\n\
                 {s} | {underline}\n\
                 {s} |\n\
                 {s} = {message}",
        s = spacing,
        p = path,
        l = self.start().0,
        c = self.start().1,
        line = self.line,
        underline = self.underline(),
        message = self.message()
      )
    }
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.format())
  }
}

impl<'i> error::Error for Error {
  fn description(&self) -> &str {
    match self.variant {
      ErrorVariant::CustomError { ref message } => message,
    }
  }
}

fn visualize_whitespace(input: &str) -> String {
  input.to_owned().replace('\r', "␍").replace('\n', "␊")
}
