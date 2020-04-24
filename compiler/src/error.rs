use crate::syntax::parse::{ParseError, ParseErrorVariant};
use crate::check::CheckError;
use crate::syntax::tree::AbsLoc;
use crate::check::CheckErrorVariant::RedefVar;

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    CheckError(CheckError),
}

impl CompileError {
    pub fn error_message(self, path: &str, input: &str) -> String {
        match self {
            CompileError::ParseError(err) =>
                format!("{}", err),

            CompileError::CheckError(err) =>
                format_check_error(err, path, input),
        }
    }
}

fn format_check_error(err: CheckError, path: &str, input: &str) -> String {
    match err.variant {
        RedefVar(Some(first), Some(redef), _) =>
            format_check_error_with_loc_range(err, path, input, first, redef),
        _ => format!("{}", err.with_path(path)),
    }
}

fn format_check_error_with_loc_range(err: CheckError, path: &str, input: &str,
                                     loc1: AbsLoc, loc2: AbsLoc) -> String {
    format_check_error_with_loc(err, path, input, (loc1.0, loc2.0))
}

fn format_check_error_with_loc(err: CheckError, path: &str, input: &str, loc: AbsLoc) -> String {
    let start = pest::Position::new(input, loc.0).unwrap();
    let end = pest::Position::new(input, loc.1).unwrap();
    let error = ParseError::new_from_span(
        ParseErrorVariant::CustomError {
            message: format!("{}", err.variant),
        },
        start.span(&end),
    );
    format!("{}", error.with_path(path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::parse::CsParser;
    use crate::syntax::tree::Entry::StmtEntry;
    use crate::syntax::tree::Stmt::Var;
    use crate::syntax::tree::VarInit::Simple;

    #[test]
    fn error_without_loc() {
        let input = "\
        var nice = 1\n\
        var b = 114514\n\
        var c = b + nice\n\
        var nice = b";
        let err = CheckError {
            file: "".to_string(),
            variant: RedefVar(None, None, "nice".into()),
        };
        let err = CompileError::CheckError(err);
        let msg = err.error_message("<stdin>", input);
        println!("{}", msg.as_str());

        assert_eq!(
            msg,
            vec![
                "In file <stdin>: Check Error: redefinition of 'nice'",
                ""
            ].join("\n")
        );
    }

    #[test]
    fn error_with_loc() {
        let input = "\
        var nice = 1\n\
        var b = 114514\n\
        var c = b + nice\n\
        var nice = b";

        let prog = CsParser::ast(input).unwrap();
        let nice1 = prog.get(0).unwrap();
        let nice2 = prog.get(3).unwrap();

        let pos1 = match nice1 {
            StmtEntry(Var(Simple(id, _))) => id.abs_loc.clone(),
            _ => unreachable!(),
        };

        let pos2 = match nice2 {
            StmtEntry(Var(Simple(id, _))) => id.abs_loc.clone(),
            _ => unreachable!(),
        };

        let err = CheckError {
            file: "".to_string(),
            variant: RedefVar(pos1, pos2, "nice".into()),
        };

        let err = CompileError::CheckError(err);
        let msg = err.error_message("<stdin>", input);
        println!("{}", msg.as_str());

        assert_eq!(
            msg,
            vec![
                " --> <stdin>:1:5",
                "  |",
                "1 | var nice = 1␊",
                "  | ...",
                "4 | var nice = b",
                "  |     ^",
                "  |",
                "  = Check Error: redefinition of 'nice'",
            ].join("\n")
        );
    }
}
