#[cfg(test)]
mod parse {
    use crate::parse::*;
    use crate::tree::Header::{Import, Using, Package};
    use crate::tree::{Program, Op, OpFix, Stmt};
    use crate::tree::Entry::{HeaderEntry, StmtEntry};
    use crate::tree::Stmt::{Throw, Var, VarList, ExprStmt, Func, Namespace, Block, Struct, Return, Try};
    use crate::tree::Expr::{Id, Literal, Unary, Lambda, Binary, Apply, Question, Ternary, Assign};
    use crate::tree::Lit::{Null, Number, Bool, Str, Char, Array, Pair};
    use crate::tree::Param::{Normal, Varargs};
    use crate::tree::VarInit::{Simple, Structured};
    use crate::tree::Op::Access;

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
            StmtEntry(Var(Simple("a".into(), Literal(Null)))),
        ])
    }

    #[test]
    fn parse_multi_var_decl_null() {
        let prog = parse("var a = null, b = null, c = null");
        assert_eq!(prog, vec![
            StmtEntry(VarList(vec![
                Simple("a".into(), Literal(Null)),
                Simple("b".into(), Literal(Null)),
                Simple("c".into(), Literal(Null)),
            ]))
        ])
    }

    #[test]
    fn parse_var_binding_decl_null() {
        let prog = parse("var (a, b, c) = null");
        assert_eq!(prog, vec![
            StmtEntry(Var(Structured(
                vec!["a".into(), "b".into(), "c".into()],
                Literal(Null),
            ))),
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
            StmtEntry(Var(Simple("a".into(), Literal(Number(100.65))))),
            StmtEntry(Var(Simple("b".into(), Literal(Number(0 as f64))))),
            StmtEntry(Var(Simple("c".into(), Unary(Op::Sub, Box::new(Literal(Number(1000 as f64))))))),
            StmtEntry(Var(Simple("d".into(), Unary(Op::Sub, Box::new(Literal(Number(10086.123 as f64))))))),
            StmtEntry(Var(Simple("e".into(), Literal(Number(114514 as f64))))),
            StmtEntry(Var(Simple("f".into(), Unary(Op::Add, Box::new(Literal(Number(132 as f64))))))),
            StmtEntry(Var(Simple("g".into(), Unary(Op::Add, Box::new(Literal(Number(114514.1232))))))),
            StmtEntry(Var(Simple("h".into(), Unary(Op::Add, Box::new(Literal(Number(0 as f64))))))),
            StmtEntry(Var(Simple("i".into(), Unary(Op::Sub, Box::new(Literal(Number(0 as f64))))))),
            StmtEntry(Var(Simple("j".into(), Unary(Op::Add, Box::new(Literal(Number(0 as f64))))))),
            StmtEntry(Var(Simple("k".into(), Unary(Op::Sub, Box::new(Literal(Number(0 as f64))))))),
        ]);
    }

    #[test]
    fn parse_bool_literal() {
        let prog = parse(
            "var a = true\n\
            var b = false");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Bool(true))))),
            StmtEntry(Var(Simple("b".into(), Literal(Bool(false))))),
        ]);
    }

    #[test]
    fn parse_null_literal() {
        let prog = parse(
            "var a = null");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Null)))),
        ]);
    }

    #[test]
    fn parse_string_literal() {
        let prog = parse(
            "var a = \"boy next door\"");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Str("boy next door".into()))))),
        ]);
    }

    #[test]
    fn parse_string_literal_with_escape() {
        let prog = parse(
            "var a = \"boy\\nnext\\ndoor\"");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Str("boy\nnext\ndoor".into()))))),
        ]);
    }

    #[test]
    fn parse_char_literal() {
        let prog = parse(
            "var a = 'Z'");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Char('Z'))))),
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
            StmtEntry(Var(Simple("a".into(), Literal(Char('\t'))))),
            StmtEntry(Var(Simple("b".into(), Literal(Char('\''))))),
            StmtEntry(Var(Simple("c".into(), Literal(Char('\n'))))),
            StmtEntry(Var(Simple("d".into(), Literal(Char('\\'))))),
        ]);
    }

    #[test]
    fn parse_array_literal() {
        let prog = parse(
            "var a = {}\n\
            var b = {1, 2, 3}");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Array(vec![]))))),
            StmtEntry(Var(Simple("b".into(), Literal(Array(vec![
                Literal(Number(1 as f64)),
                Literal(Number(2 as f64)),
                Literal(Number(3 as f64)),
            ]))))),
        ]);
    }

    #[test]
    fn parse_array_pair_literal() {
        let prog = parse(
            "var a = {0: a, 1: b, 2: c}");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Literal(Array(vec![
                Literal(Pair(Box::new(Literal(Number(0 as f64))), Box::new(Id("a".into())))),
                Literal(Pair(Box::new(Literal(Number(1 as f64))), Box::new(Id("b".into())))),
                Literal(Pair(Box::new(Literal(Number(2 as f64))), Box::new(Id("c".into())))),
            ]))))),
        ]);
    }

    #[test]
    fn parse_lambda() {
        let prog = parse(
            "var id = [](a) -> a");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("id".into(), Lambda(
                vec![Normal("a".into())],
                Box::new(Id("a".into())),
            )))),
        ]);
    }

    #[test]
    fn parse_lambda_with_varargs() {
        let prog = parse(
            "var format = [](fmt, ...arg) -> echo(fmt, arg...)");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("format".into(), Lambda(
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
            )))),
        ]);
    }

    #[test]
    fn parse_lambda_with_primary_expr() {
        let prog = parse(
            "var show = [](a) -> system.out.println(a)");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("show".into(), Lambda(
                vec![
                    Normal("a".into()),
                ],
                Box::new(Apply(
                    Box::new(Binary(
                        Op::Access,
                        Box::new(Binary(
                            Op::Access,
                            Box::new(Id("system".into())),
                            Box::new(Id("out".into())),
                        )),
                        Box::new(Id("println".into())))
                    ),
                    vec![
                        Id("a".into()),
                    ],
                )),
            )))),
        ]);
    }

    #[test]
    fn parse_pre_inc_dec() {
        let prog = parse(
            "var a = ++x\n\
            var b = y++\n\
            var c = w--\n\
            var d = --w\n\
            ");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Unary(Op::Inc(OpFix::Prefix), Box::new(Id("x".into())))))),
            StmtEntry(Var(Simple("b".into(), Unary(Op::Inc(OpFix::Postfix), Box::new(Id("y".into())))))),
            StmtEntry(Var(Simple("c".into(), Unary(Op::Dec(OpFix::Postfix), Box::new(Id("w".into())))))),
            StmtEntry(Var(Simple("d".into(), Unary(Op::Dec(OpFix::Prefix), Box::new(Id("w".into())))))),
        ]);
    }

    #[test]
    fn parse_question_expr() {
        let prog = parse(
            "var r = a ? b");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("r".into(), Question(
                Box::new(Id("a".into())),
                Box::new(Id("b".into())))))),
        ]);
    }

    #[test]
    fn parse_ternary_expr() {
        let prog = parse(
            "var r = a ? b : c");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("r".into(), Ternary(
                Box::new(Id("a".into())),
                Box::new(Id("b".into())),
                Box::new(Id("c".into())))))),
        ]);
    }

    #[test]
    fn parse_binary_expr() {
        let prog = parse(
            "var a = 1 + 2 * 3 + 4");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple("a".into(), Binary(
                Op::Add,
                Box::new(Binary(
                    Op::Add,
                    Box::new(Literal(Number(1.0))),
                    Box::new(Binary(
                        Op::Mul,
                        Box::new(Literal(Number(2.0))),
                        Box::new(Literal(Number(3.0))))),
                )),
                Box::new(Literal(Number(4.0))))))
            ),
        ]);
    }

    #[test]
    fn parse_expr_stmt() {
        let prog = parse("\
        a++\n\
        ++a\n\
        exit()\n\
        a = 100\n"
        );
        assert_eq!(prog, vec![
            StmtEntry(ExprStmt(Unary(Op::Inc(OpFix::Postfix), Box::new(Id("a".into()))))),
            StmtEntry(ExprStmt(Unary(Op::Inc(OpFix::Prefix), Box::new(Id("a".into()))))),
            StmtEntry(ExprStmt(Apply(Box::new(Id("exit".into())), vec![]))),
            StmtEntry(ExprStmt(Assign(Op::Assign,
                                      Box::new(Id("a".into())),
                                      Box::new(Literal(Number(100.0)))))),
        ]);
    }

    #[test]
    fn parse_func_decl() {
        let prog = parse("\
        function jiegebuyao()\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Func("jiegebuyao".into(), vec![], vec![]))
        ]);
    }

    #[test]
    fn parse_func_decl_with_args() {
        let prog = parse("\
        function jiegebuyao(a, b, c)\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Func("jiegebuyao".into(),
                           vec![
                               Normal("a".into()),
                               Normal("b".into()),
                               Normal("c".into())],
                           vec![],
            ))
        ]);
    }

    #[test]
    fn parse_func_decl_with_varargs() {
        let prog = parse("\
        function jiegebuyao(a, b, ...c)\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Func("jiegebuyao".into(),
                           vec![
                               Normal("a".into()),
                               Normal("b".into()),
                               Varargs("c".into())],
                           vec![],
            ))
        ]);
    }

    #[test]
    fn parse_func_decl_with_single_varargs() {
        let prog = parse("\
        function jiegebuyao(...x)\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Func("jiegebuyao".into(),
                           vec![Varargs("x".into())],
                           vec![],
            ))
        ]);
    }

    #[test]
    fn parse_empty_namespace_decl() {
        let prog = parse("\
        namespace jiegebuyao\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Namespace("jiegebuyao".into(),
                                vec![],
            ))
        ]);
    }

    #[test]
    fn parse_namespace_decl() {
        let prog = parse("\
        namespace jiegebuyao\n\
            constant awsl = jiegebuyao\n\
            constant binbin = 114514\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Namespace("jiegebuyao".into(), vec![
                Var(Simple("awsl".into(), Id("jiegebuyao".into()))),
                Var(Simple("binbin".into(), Literal(Number(114514 as f64)))),
            ]))
        ]);
    }

    #[test]
    fn parse_struct_decl() {
        let prog = parse("\
        struct jiegebuyao\n\
            constant awsl = jiegebuyao\n\
            constant binbin = 114514\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Struct("jiegebuyao".into(), None, vec![
                Var(Simple("awsl".into(), Id("jiegebuyao".into()))),
                Var(Simple("binbin".into(), Literal(Number(114514 as f64)))),
            ]))
        ]);
    }

    #[test]
    fn parse_struct_decl_with_extends() {
        let prog = parse("\
        struct jiegebuyao extends boynextdoor\n\
            constant awsl = jiegebuyao\n\
            constant binbin = 114514\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Struct("jiegebuyao".into(), Some(Id("boynextdoor".into())), vec![
                Var(Simple("awsl".into(), Id("jiegebuyao".into()))),
                Var(Simple("binbin".into(), Literal(Number(114514 as f64)))),
            ]))
        ]);
    }

    #[test]
    fn parse_struct_decl_with_complex_extends() {
        let prog = parse("\
        struct jiegebuyao extends resolve(\"boy\")\n\
            constant awsl = jiegebuyao\n\
            constant binbin = 114514\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Struct(
                "jiegebuyao".into(),
                Some(Apply(
                    Box::new(Id("resolve".into())),
                    vec![Literal(Str("boy".into()))],
                )),
                vec![
                    Var(Simple("awsl".into(), Id("jiegebuyao".into()))),
                    Var(Simple("binbin".into(), Literal(Number(114514 as f64)))),
                ],
            ))
        ]);
    }

    #[test]
    fn parse_empty_block_decl() {
        let prog = parse("\
        block\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Block(
                vec![],
            ))
        ]);
    }

    #[test]
    fn parse_return() {
        let prog = parse("return");
        assert_eq!(prog, vec![
            StmtEntry(Return(None))
        ]);
    }

    #[test]
    fn parse_return_sth() {
        let prog = parse("return i_single_push_minato_aqua");
        assert_eq!(prog, vec![
            StmtEntry(Return(Some(Id("i_single_push_minato_aqua".into()))))
        ]);
    }

    #[test]
    fn parse_return_in_func() {
        let prog = parse("\
        function jiegebuyao()\n\
            return\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Func("jiegebuyao".into(), vec![], vec![
                Return(None),
            ]))
        ]);
    }

    #[test]
    fn parse_try_catch() {
        let prog = parse("\
        try\n\
            get()\n\
        catch e\n\
            e.printStackTrace()\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(Try(
                vec![
                    ExprStmt(Apply(Box::new(Id("get".into())), vec![]))
                ],
                "e".into(),
                vec![
                    ExprStmt(Apply(
                        Box::new(Binary(
                            Op::Access,
                            Box::new(Id("e".into())),
                            Box::new(Id("printStackTrace".into())))),
                        vec![]))
                ],
            ))
        ]);
    }

    fn parse(input: &str) -> Program {
        CsParser::ast(input).expect("Compile Error")
    }
}
