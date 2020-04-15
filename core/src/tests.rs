#[cfg(test)]
mod parse {
    use crate::parse::*;
    use crate::tree::Header::{Import, Using, Package};
    use crate::tree::{Program, Op};
    use crate::tree::Entry::{HeaderEntry, StmtEntry};
    use crate::tree::Stmt::{Throw, Var, Bind, VarList};
    use crate::tree::Expr::{Id, Literal, Unary, Lambda, Binary, Apply};
    use crate::tree::Lit::{Null, Number, Bool, Str, Char, Array, Pair};
    use crate::tree::Param::{Normal, Varargs};

    #[test]
    fn works() {
        assert_eq!(1, 1)
    }

    #[test]
    fn parse_import() {
        let prog = parse("import streams");
        assert_eq!(prog, vec![HeaderEntry(Import("streams".into(), None))])
    }

    #[test]
    fn parse_import_dot() {
        let prog = parse("import codec.base64");
        assert_eq!(prog, vec![HeaderEntry(Import("codec.base64".into(), None))])
    }

    #[test]
    fn parse_import_as() {
        let prog = parse("import streams as s");
        assert_eq!(prog, vec![HeaderEntry(Import("streams".into(),
                                                 Some("s".into())))])
    }

    #[test]
    fn parse_import_dot_as() {
        let prog = parse("import codec.base64 as bbb");
        assert_eq!(prog, vec![HeaderEntry(Import("codec.base64".into(),
                                                 Some("bbb".into())))])
    }

    #[test]
    fn parse_using() {
        let prog = parse("using fuck");
        assert_eq!(prog, vec![HeaderEntry(Using("fuck".into()))])
    }

    #[test]
    fn parse_package() {
        let prog = parse("package fuck");
        assert_eq!(prog, vec![HeaderEntry(Package("fuck".into()))])
    }

    #[test]
    fn parse_mixed_headers() {
        let prog = parse(
            "package fuck\n\
            import junk as jk\n\
            import lolita as loli\n\
            import boynextdoor\n\
            using hololive\n\
            package homolive");
        assert_eq!(prog, vec![
            HeaderEntry(Package("fuck".into())),
            HeaderEntry(Import("junk".into(), Some("jk".into()))),
            HeaderEntry(Import("lolita".into(), Some("loli".into()))),
            HeaderEntry(Import("boynextdoor".into(), None)),
            HeaderEntry(Using("hololive".into())),
            HeaderEntry(Package("homolive".into())),
        ])
    }

    #[test]
    fn parse_throw() {
        let prog = parse("throw boynextdoor");
        assert_eq!(prog, vec![
            StmtEntry(Throw(Id("boynextdoor".into())))
        ]);
    }

    #[test]
    fn parse_cross_line_stmt() {
        let prog = parse(
            "@begin\n\
            throw boynextdoor\n\
            @end");
        assert_eq!(prog, vec![
            StmtEntry(Throw(Id("boynextdoor".into())))
        ]);
    }

    #[test]
    fn parse_var_decl_null() {
        let prog = parse("var a = null");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Null)))
        ])
    }

    #[test]
    fn parse_multi_var_decl_null() {
        let prog = parse("var a = null, b = null, c = null");
        assert_eq!(prog, vec![
            StmtEntry(VarList(vec![
                ("a".into(), Literal(Null)),
                ("b".into(), Literal(Null)),
                ("c".into(), Literal(Null)),
            ]))
        ])
    }

    #[test]
    fn parse_var_binding_decl_null() {
        let prog = parse("var (a, b, c) = null");
        assert_eq!(prog, vec![
            StmtEntry(Bind(vec!["a".into(), "b".into(), "c".into()],
                           Literal(Null)))
        ])
    }

    #[test]
    fn parse_number_literal_without_pe() {
        let prog = parse(
            "var a = 100.65\n\
            var b = 0\n\
            var c = -1000\n\
            var d = -10086.123\n\
            var e = 114514\n\
            var f = +132\n\
            var g = +114514.1232\n\
            var h = +0.0\n\
            var i = -0.0\n\
            var j = +0\n\
            var k = -0\n");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Number(100.65)))),
            StmtEntry(Var("b".into(), Literal(Number(0 as f64)))),
            StmtEntry(Var("c".into(), Unary(Op::Sub, Box::new(Literal(Number(1000 as f64)))))),
            StmtEntry(Var("d".into(), Unary(Op::Sub, Box::new(Literal(Number(10086.123 as f64)))))),
            StmtEntry(Var("e".into(), Literal(Number(114514 as f64)))),
            StmtEntry(Var("f".into(), Unary(Op::Add, Box::new(Literal(Number(132 as f64)))))),
            StmtEntry(Var("g".into(), Unary(Op::Add, Box::new(Literal(Number(114514.1232)))))),
            StmtEntry(Var("h".into(), Unary(Op::Add, Box::new(Literal(Number(0 as f64)))))),
            StmtEntry(Var("i".into(), Unary(Op::Sub, Box::new(Literal(Number(0 as f64)))))),
            StmtEntry(Var("j".into(), Unary(Op::Add, Box::new(Literal(Number(0 as f64)))))),
            StmtEntry(Var("k".into(), Unary(Op::Sub, Box::new(Literal(Number(0 as f64)))))),
        ]);
    }

    #[test]
    fn parse_bool_literal() {
        let prog = parse(
            "var a = true\n\
            var b = false");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Bool(true)))),
            StmtEntry(Var("b".into(), Literal(Bool(false)))),
        ]);
    }

    #[test]
    fn parse_null_literal() {
        let prog = parse(
            "var a = null");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Null))),
        ]);
    }

    #[test]
    fn parse_string_literal() {
        let prog = parse(
            "var a = \"boy next door\"");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Str("boy next door".into())))),
        ]);
    }

    #[test]
    fn parse_string_literal_with_escape() {
        let prog = parse(
            "var a = \"boy\\nnext\\ndoor\"");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Str("boy\nnext\ndoor".into())))),
        ]);
    }

    #[test]
    fn parse_char_literal() {
        let prog = parse(
            "var a = 'Z'");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Char('Z')))),
        ]);
    }

    #[test]
    fn parse_char_literal_with_escape() {
        let prog = parse(
            "var a = '\\t' \n\
            var b = '\\'' \n\
            var c = '\\n' \n\
            var d = '\\\\' \n\
            ");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Char('\t')))),
            StmtEntry(Var("b".into(), Literal(Char('\'')))),
            StmtEntry(Var("c".into(), Literal(Char('\n')))),
            StmtEntry(Var("d".into(), Literal(Char('\\')))),
        ]);
    }

    #[test]
    fn parse_array_literal() {
        let prog = parse(
            "var a = {}\n\
            var b = {1, 2, 3}");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Array(vec![])))),
            StmtEntry(Var("b".into(), Literal(Array(vec![
                Literal(Number(1 as f64)),
                Literal(Number(2 as f64)),
                Literal(Number(3 as f64)),
            ])))),
        ]);
    }

    #[test]
    fn parse_array_pair_literal() {
        let prog = parse(
            "var a = {0: a, 1: b, 2: c}");
        assert_eq!(prog, vec![
            StmtEntry(Var("a".into(), Literal(Array(vec![
                Literal(Pair(Box::new(Literal(Number(0 as f64))), Box::new(Id("a".into())))),
                Literal(Pair(Box::new(Literal(Number(1 as f64))), Box::new(Id("b".into())))),
                Literal(Pair(Box::new(Literal(Number(2 as f64))), Box::new(Id("c".into())))),
            ])))),
        ]);
    }

    #[test]
    fn parse_lambda() {
        let prog = parse(
            "var id = [](a) -> a");
        assert_eq!(prog, vec![
            StmtEntry(Var("id".into(), Lambda(
                vec![Normal("a".into())],
                Box::new(Id("a".into())),
            )))
        ]);
    }

    #[test]
    fn parse_lambda_with_varargs() {
        let prog = parse(
            "var format = [](fmt, ...arg) -> echo(fmt, arg...)");
        assert_eq!(prog, vec![
            StmtEntry(Var("format".into(), Lambda(
                vec![
                    Normal("fmt".into()),
                    Varargs("arg".into()),
                ],
                Box::new(Apply(
                    Box::new(Id("echo".into())),
                    vec![
                        Id("fmt".into()),
                        Unary(Op::Flatten, Box::new(Id("arg".into()))),
                    ],
                )),
            )))
        ]);
    }

    fn parse(input: &str) -> Program {
        CsParser::ast(input).expect("Compile Error")
    }
}
