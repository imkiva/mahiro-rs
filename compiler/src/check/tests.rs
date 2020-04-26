mod checker {
    use crate::CompileResult;
    use crate::syntax::parse::CsParser;
    use crate::syntax::desugar::Desugar;
    use crate::syntax::optimize::{Optimizer, OptimizeLevel};
    use crate::check::Checker;

    #[test]
    fn redef_imported_1() {
        assert_err("\
        import streams\n\
        import base64\n\
        import boy\n\
        import next\n\
        var streams = 1\n", vec![
            " --> <stdin>:1:8",
            "  |",
            "1 | import streams␊",
            "  | ...",
            "5 | var streams = 1␊",
            "  |    ^---^",
            "  |",
            "  = Check Error: redefinition of 'streams'"
        ].join("\n"));
    }

    #[test]
    fn redef_imported_2() {
        assert_err("\
        import streams\n\
        import base64\n\
        import boy\n\
        import next\n\
        var base64 = 1\n", vec![
            " --> <stdin>:2:8",
            "  |",
            "2 | import base64␊",
            "  | ...",
            "5 | var base64 = 1␊",
            "  |    ^---^",
            "  |",
            "  = Check Error: redefinition of 'base64'"
        ].join("\n"));
    }

    #[test]
    fn redef_imported_3() {
        assert_err("\
        import streams\n\
        import base64\n\
        import boy\n\
        import next\n\
        var boy = 1\n", vec![
            " --> <stdin>:3:8",
            "  |",
            "3 | import boy␊",
            "  | ...",
            "5 | var boy = 1␊",
            "  |    ^---^",
            "  |",
            "  = Check Error: redefinition of 'boy'"
        ].join("\n"));
    }

    #[test]
    fn redef_imported_4() {
        assert_err("\
        import streams\n\
        import base64\n\
        import boy\n\
        import next\n\
        var next = 1\n", vec![
            " --> <stdin>:4:8",
            "  |",
            "4 | import next␊",
            "5 | var next = 1␊",
            "  |    ^---^",
            "  |",
            "  = Check Error: redefinition of 'next'"
        ].join("\n"));
    }

    #[test]
    fn redef_qualified_import() {
        assert_err("\
        import streams as sss\n\
        import base64\n\
        import boy\n\
        import next\n\
        var sss = 1\n", vec![
            " --> <stdin>:1:19",
            "  |",
            "1 | import streams as sss␊",
            "  | ...",
            "5 | var sss = 1␊",
            "  |    ^--------------^",
            "  |",
            "  = Check Error: redefinition of 'sss'"
        ].join("\n"));
    }

    #[test]
    fn redef_var() {
        assert_err("\
        var nice = 1\n\
        var b = 114514\n\
        var c = b + nice\n\
        var nice = b", vec![
            " --> <stdin>:1:5",
            "  |",
            "1 | var nice = 1␊",
            "  | ...",
            "4 | var nice = b",
            "  |     ^",
            "  |",
            "  = Check Error: redefinition of 'nice'"
        ].join("\n"));
    }

    #[test]
    fn redef_var_with_gap() {
        assert_err("\
        var nice_ma_fei = 1\n\
        var b = 114514\n\
        import fuck\n\
        var c = b + nice\n\
        var nice_ma_fei = b", vec![
            " --> <stdin>:1:5",
            "  |",
            "1 | var nice_ma_fei = 1␊",
            "  | ...",
            "5 | var nice_ma_fei = b",
            "  |     ^",
            "  |",
            "  = Check Error: redefinition of 'nice_ma_fei'"
        ].join("\n"));
    }

    #[test]
    fn redef_struct_with_var() {
        assert_err("\
        import boynextdoor\n\
        struct code\n\
        \n\
        end\n\
        var code = 1", vec![
            " --> <stdin>:2:8",
            "  |",
            "2 | struct code␊",
            "  | ...",
            "5 | var code = 1",
            "  |    ^---^",
            "  |",
            "  = Check Error: redefinition of 'code'"
        ].join("\n"));
    }

    #[test]
    fn redef_struct_with_struct() {
        assert_err("\
        import boynextdoor\n\
        struct code\n\
        \n\
        end\n\
        struct code\n\
        end\n", vec![
            " --> <stdin>:2:8",
            "  |",
            "2 | struct code␊",
            "  | ...",
            "5 | struct code␊",
            "  |        ^",
            "  |",
            "  = Check Error: redefinition of 'code'"
        ].join("\n"));
    }

    #[test]
    fn redef_var_in_struct() {
        assert_err("\
        import boynextdoor\n\
        struct code\n\
            var a = null\n\
            var a = 1\n\
            var b = a
        \n\
        end\n", vec![
            " --> <stdin>:3:5",
            "  |",
            "3 | var a = null␊",
            "4 | var a = 1␊",
            "  |     ^",
            "  |",
            "  = Check Error: redefinition of 'a'"
        ].join("\n"));
    }

    #[test]
    fn break_outside_loop() {
        assert_err("break", vec![
            " --> <stdin>:1:1",
            "  |",
            "1 | break",
            "  | ^---^",
            "  |",
            "  = Check Error: 'break' should be inside loop statement",
        ].join("\n"));
    }

    #[test]
    fn continue_outside_loop() {
        assert_err("continue", vec![
            " --> <stdin>:1:1",
            "  |",
            "1 | continue",
            "  | ^------^",
            "  |",
            "  = Check Error: 'continue' should be inside loop statement",
        ].join("\n"));
    }

    fn assert_err(input: &str, err: String) {
        assert_eq!(check(input), err)
    }

    fn check(input: &str) -> String {
        let program = CsParser::ast(input).unwrap();
        let program = Desugar::desugar(program).unwrap();
        let program = Checker::check(program);
        assert!(program.is_err());
        let msg = program.unwrap_err().error_message("<stdin>", input);
        println!("{}", msg.as_str());
        msg
    }
}
