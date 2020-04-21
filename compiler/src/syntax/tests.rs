#[cfg(test)]
mod parse {
    use crate::syntax::parse::*;
    use crate::syntax::tree::Ident;
    use crate::syntax::tree::Header::{Import, Using, Package};
    use crate::syntax::tree::{Program, Op};
    use crate::syntax::tree::Entry::{HeaderEntry, StmtEntry};
    use crate::syntax::tree::Stmt::{Throw, Var, VarList, ExprStmt, Func, Namespace, Block, Struct, Return, Try, If, While, Loop, Break, Continue, For, ForEach};
    use crate::syntax::tree::Expr::{Id, Literal, Unary, Lambda, Binary, Apply, Question, Ternary, Assign};
    use crate::syntax::tree::Lit::{Null, Number, Bool, Str, Char, Array, Pair};
    use crate::syntax::tree::Param::{Normal, Varargs};
    use crate::syntax::tree::VarInit::{Simple, Structured};

    #[test]
    fn works() {
        assert_eq!(1, 1)
    }

    #[test]
    fn parse_import() {
        let prog = parse("import streams");
        assert_eq!(prog, vec![HeaderEntry(Import(Ident::only("streams"), None))])
    }

    #[test]
    fn parse_import_dot() {
        let prog = parse("import codec.base64");
        assert_eq!(prog, vec![HeaderEntry(Import(Ident::only("codec.base64"), None))])
    }

    #[test]
    fn parse_import_as() {
        let prog = parse("import streams as s");
        assert_eq!(prog, vec![HeaderEntry(Import(Ident::only("streams"),
                                                 Some(Ident::only("s"))))])
    }

    #[test]
    fn parse_import_dot_as() {
        let prog = parse("import codec.base64 as bbb");
        assert_eq!(prog, vec![HeaderEntry(Import(Ident::only("codec.base64"),
                                                 Some(Ident::only("bbb"))))])
    }

    #[test]
    fn parse_using() {
        let prog = parse("using fuck");
        assert_eq!(prog, vec![HeaderEntry(Using(Ident::only("fuck")))])
    }

    #[test]
    fn parse_package() {
        let prog = parse("package fuck");
        assert_eq!(prog, vec![HeaderEntry(Package(Ident::only("fuck")))])
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
            HeaderEntry(Package(Ident::only("fuck"))),
            HeaderEntry(Import(Ident::only("junk"), Some(Ident::only("jk")))),
            HeaderEntry(Import(Ident::only("lolita"), Some(Ident::only("loli")))),
            HeaderEntry(Import(Ident::only("boynextdoor"), None)),
            HeaderEntry(Using(Ident::only("hololive"))),
            HeaderEntry(Package(Ident::only("homolive"))),
        ])
    }

    #[test]
    fn parse_throw() {
        let prog = parse("throw boynextdoor");
        assert_eq!(prog, vec![
            StmtEntry(Throw(Id(Ident::only("boynextdoor"))))
        ]);
    }

    #[test]
    fn parse_cross_line_stmt() {
        let prog = parse(
            "@begin\n\
            throw boynextdoor\n\
            @end");
        assert_eq!(prog, vec![
            StmtEntry(Throw(Id(Ident::only("boynextdoor"))))
        ]);
    }

    #[test]
    fn parse_var_decl_null() {
        let prog = parse("var a = null");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Null)))),
        ])
    }

    #[test]
    fn parse_multi_var_decl_null() {
        let prog = parse("var a = null, b = null, c = null");
        assert_eq!(prog, vec![
            StmtEntry(VarList(vec![
                Simple(Ident::only("a"), Literal(Null)),
                Simple(Ident::only("b"), Literal(Null)),
                Simple(Ident::only("c"), Literal(Null)),
            ]))
        ])
    }

    #[test]
    fn parse_var_binding_decl_null() {
        let prog = parse("var (a, b, c) = null");
        assert_eq!(prog, vec![
            StmtEntry(Var(Structured(
                vec![Ident::only("a"), Ident::only("b"), Ident::only("c")],
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
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Number(100.65))))),
            StmtEntry(Var(Simple(Ident::only("b"), Literal(Number(0 as f64))))),
            StmtEntry(Var(Simple(Ident::only("c"), Unary(Op::Sub, Box::new(Literal(Number(1000 as f64))))))),
            StmtEntry(Var(Simple(Ident::only("d"), Unary(Op::Sub, Box::new(Literal(Number(10086.123 as f64))))))),
            StmtEntry(Var(Simple(Ident::only("e"), Literal(Number(114514 as f64))))),
            StmtEntry(Var(Simple(Ident::only("f"), Unary(Op::Add, Box::new(Literal(Number(132 as f64))))))),
            StmtEntry(Var(Simple(Ident::only("g"), Unary(Op::Add, Box::new(Literal(Number(114514.1232))))))),
            StmtEntry(Var(Simple(Ident::only("h"), Unary(Op::Add, Box::new(Literal(Number(0 as f64))))))),
            StmtEntry(Var(Simple(Ident::only("i"), Unary(Op::Sub, Box::new(Literal(Number(0 as f64))))))),
            StmtEntry(Var(Simple(Ident::only("j"), Unary(Op::Add, Box::new(Literal(Number(0 as f64))))))),
            StmtEntry(Var(Simple(Ident::only("k"), Unary(Op::Sub, Box::new(Literal(Number(0 as f64))))))),
        ]);
    }

    #[test]
    fn parse_bool_literal() {
        let prog = parse(
            "var a = true\n\
            var b = false");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Bool(true))))),
            StmtEntry(Var(Simple(Ident::only("b"), Literal(Bool(false))))),
        ]);
    }

    #[test]
    fn parse_null_literal() {
        let prog = parse(
            "var a = null");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Null)))),
        ]);
    }

    #[test]
    fn parse_string_literal() {
        let prog = parse(
            "var a = \"boy next door\"");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Str("boy next door".into()))))),
        ]);
    }

    #[test]
    fn parse_string_literal_with_escape() {
        let prog = parse(
            "var a = \"boy\\nnext\\ndoor\"");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Str("boy\nnext\ndoor".into()))))),
        ]);
    }

    #[test]
    fn parse_char_literal() {
        let prog = parse(
            "var a = 'Z'");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Char('Z'))))),
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
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Char('\t'))))),
            StmtEntry(Var(Simple(Ident::only("b"), Literal(Char('\''))))),
            StmtEntry(Var(Simple(Ident::only("c"), Literal(Char('\n'))))),
            StmtEntry(Var(Simple(Ident::only("d"), Literal(Char('\\'))))),
        ]);
    }

    #[test]
    fn parse_array_literal() {
        let prog = parse(
            "var a = {}\n\
            var b = {1, 2, 3}");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Array(vec![]))))),
            StmtEntry(Var(Simple(Ident::only("b"), Literal(Array(vec![
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
            StmtEntry(Var(Simple(Ident::only("a"), Literal(Array(vec![
                Literal(Pair(Box::new(Literal(Number(0 as f64))), Box::new(Id(Ident::only("a"))))),
                Literal(Pair(Box::new(Literal(Number(1 as f64))), Box::new(Id(Ident::only("b"))))),
                Literal(Pair(Box::new(Literal(Number(2 as f64))), Box::new(Id(Ident::only("c"))))),
            ]))))),
        ]);
    }

    #[test]
    fn parse_lambda() {
        let prog = parse(
            "var id = [](a) -> a");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("id"), Lambda(
                None,
                vec![Normal(Ident::only("a"))],
                Box::new(Id(Ident::only("a"))),
            )))),
        ]);
    }

    #[test]
    fn parse_lambda_with_varargs() {
        let prog = parse(
            "var format = [](fmt, ...arg) -> echo(fmt, arg...)");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("format"), Lambda(
                None,
                vec![
                    Normal(Ident::only("fmt")),
                    Varargs(Ident::only("arg")),
                ],
                Box::new(Apply(
                    Box::new(Id(Ident::only("echo"))),
                    vec![
                        Id(Ident::only("fmt")),
                        Unary(Op::Flatten, Box::new(Id(Ident::only("arg")))),
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
            StmtEntry(Var(Simple(Ident::only("show"), Lambda(
                None,
                vec![
                    Normal(Ident::only("a")),
                ],
                Box::new(Apply(
                    Box::new(Binary(
                        Op::Access,
                        Box::new(Binary(
                            Op::Access,
                            Box::new(Id(Ident::only("system"))),
                            Box::new(Id(Ident::only("out"))),
                        )),
                        Box::new(Id(Ident::only("println"))))
                    ),
                    vec![
                        Id(Ident::only("a")),
                    ],
                )),
            )))),
        ]);
    }

    #[test]
    fn parse_lambda_with_single_capture() {
        let prog = parse("\
        var lam = [lam]() -> lam\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("lam"), Lambda(
                Some(vec![Ident::only("lam")]),
                vec![],
                Box::new(Id(Ident::only("lam"))),
            )))),
        ]);
    }

    #[test]
    fn parse_lambda_with_captures() {
        let prog = parse("\
        var lam = [a, b, c]() -> lam\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("lam"), Lambda(
                Some(vec![
                    Ident::only("a"),
                    Ident::only("b"),
                    Ident::only("c"),
                ]),
                vec![],
                Box::new(Id(Ident::only("lam"))),
            )))),
        ]);
    }

    #[test]
    fn parse_lambda_with_captures_and_args() {
        let prog = parse("\
        var lam = [a, b, c](x, y, z) -> lam\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("lam"), Lambda(
                Some(vec![
                    Ident::only("a"),
                    Ident::only("b"),
                    Ident::only("c"),
                ]),
                vec![
                    Normal(Ident::only("x")),
                    Normal(Ident::only("y")),
                    Normal(Ident::only("z")),
                ],
                Box::new(Id(Ident::only("lam"))),
            )))),
        ]);
    }

    #[test]
    fn parse_question_expr() {
        let prog = parse(
            "var r = a ? b");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("r"), Question(
                Box::new(Id(Ident::only("a"))),
                Box::new(Id(Ident::only("b"))))))),
        ]);
    }

    #[test]
    fn parse_ternary_expr() {
        let prog = parse(
            "var r = a ? b : c");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("r"), Ternary(
                Box::new(Id(Ident::only("a"))),
                Box::new(Id(Ident::only("b"))),
                Box::new(Id(Ident::only("c"))))))),
        ]);
    }

    #[test]
    fn parse_binary_expr() {
        let prog = parse(
            "var a = 1 + 2 * 3 + 4");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("a"), Binary(
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
        exit()\n\
        a = 100\n"
        );
        assert_eq!(prog, vec![
            StmtEntry(ExprStmt(Apply(Box::new(Id(Ident::only("exit"))), vec![]))),
            StmtEntry(ExprStmt(Assign(Op::Assign,
                                      Box::new(Id(Ident::only("a"))),
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
            StmtEntry(Func(Ident::only("jiegebuyao"), vec![], vec![]))
        ]);
    }

    #[test]
    fn parse_func_decl_with_args() {
        let prog = parse("\
        function jiegebuyao(a, b, c)\n\
        end\n\
        ");
        assert_eq!(prog, vec![
            StmtEntry(Func(Ident::only("jiegebuyao"),
                           vec![
                               Normal(Ident::only("a")),
                               Normal(Ident::only("b")),
                               Normal(Ident::only("c"))],
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
            StmtEntry(Func(Ident::only("jiegebuyao"),
                           vec![
                               Normal(Ident::only("a")),
                               Normal(Ident::only("b")),
                               Varargs(Ident::only("c"))],
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
            StmtEntry(Func(Ident::only("jiegebuyao"),
                           vec![Varargs(Ident::only("x"))],
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
            StmtEntry(Namespace(Ident::only("jiegebuyao"),
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
            StmtEntry(Namespace(Ident::only("jiegebuyao"), vec![
                Var(Simple(Ident::only("awsl"), Id(Ident::only("jiegebuyao")))),
                Var(Simple(Ident::only("binbin"), Literal(Number(114514 as f64)))),
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
            StmtEntry(Struct(Ident::only("jiegebuyao"), None, vec![
                Var(Simple(Ident::only("awsl"), Id(Ident::only("jiegebuyao")))),
                Var(Simple(Ident::only("binbin"), Literal(Number(114514 as f64)))),
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
            StmtEntry(Struct(Ident::only("jiegebuyao"), Some(Id(Ident::only("boynextdoor"))), vec![
                Var(Simple(Ident::only("awsl"), Id(Ident::only("jiegebuyao")))),
                Var(Simple(Ident::only("binbin"), Literal(Number(114514 as f64)))),
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
                Ident::only("jiegebuyao"),
                Some(Apply(
                    Box::new(Id(Ident::only("resolve"))),
                    vec![Literal(Str("boy".into()))],
                )),
                vec![
                    Var(Simple(Ident::only("awsl"), Id(Ident::only("jiegebuyao")))),
                    Var(Simple(Ident::only("binbin"), Literal(Number(114514 as f64)))),
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
            StmtEntry(Return(Some(Id(Ident::only("i_single_push_minato_aqua")))))
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
            StmtEntry(Func(Ident::only("jiegebuyao"), vec![], vec![
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
                    ExprStmt(Apply(Box::new(Id(Ident::only("get"))), vec![]))
                ],
                Ident::only("e"),
                vec![
                    ExprStmt(Apply(
                        Box::new(Binary(
                            Op::Access,
                            Box::new(Id(Ident::only("e"))),
                            Box::new(Id(Ident::only("printStackTrace"))))),
                        vec![]))
                ],
            ))
        ]);
    }

    #[test]
    fn parse_if() {
        let prog = parse("\
        if a > 1 && a < 10\n\
            yes()\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(
                If(Binary(
                    Op::And,
                    Box::new(Binary(
                        Op::Gt,
                        Box::new(Id(Ident::only("a"))),
                        Box::new(Literal(Number(1.0))))),
                    Box::new(Binary(
                        Op::Lt,
                        Box::new(Id(Ident::only("a"))),
                        Box::new(Literal(Number(10.0)))))),
                   vec![
                       ExprStmt(Apply(Box::new(Id(Ident::only("yes"))), vec![])),
                   ],
                   None)
            )
        ]);
    }

    #[test]
    fn parse_if_else() {
        let prog = parse("\
        if a > 1 && a < 10\n\
            yes()\n\
        else\n\
            no()\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(
                If(Binary(
                    Op::And,
                    Box::new(Binary(
                        Op::Gt,
                        Box::new(Id(Ident::only("a"))),
                        Box::new(Literal(Number(1.0))))),
                    Box::new(Binary(
                        Op::Lt,
                        Box::new(Id(Ident::only("a"))),
                        Box::new(Literal(Number(10.0)))))),
                   vec![
                       ExprStmt(Apply(Box::new(Id(Ident::only("yes"))), vec![])),
                   ],
                   Some(vec![
                       ExprStmt(Apply(Box::new(Id(Ident::only("no"))), vec![])),
                   ])))
        ]);
    }

    #[test]
    fn parse_while() {
        let prog = parse("\
        while a > 1 && a < 10\n\
            yes()\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(
                While(
                    Binary(Op::And,
                           Box::new(Binary(
                               Op::Gt,
                               Box::new(Id(Ident::only("a"))),
                               Box::new(Literal(Number(1.0))))),
                           Box::new(Binary(
                               Op::Lt,
                               Box::new(Id(Ident::only("a"))),
                               Box::new(Literal(Number(10.0)))))),
                    vec![
                        ExprStmt(Apply(Box::new(Id(Ident::only("yes"))), vec![])),
                    ],
                )
            )
        ]);
    }

    #[test]
    fn parse_loop() {
        let prog = parse("\
        loop\n\
            yes()\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(
                Loop(
                    None,
                    vec![
                        ExprStmt(Apply(Box::new(Id(Ident::only("yes"))), vec![])),
                    ],
                )
            )
        ]);
    }

    #[test]
    fn parse_loop_until() {
        let prog = parse("\
        loop\n\
            yes()\n\
        until a <= 1 || a >= 10");
        assert_eq!(prog, vec![
            StmtEntry(
                Loop(
                    Some(Binary(Op::Or,
                                Box::new(Binary(
                                    Op::Le,
                                    Box::new(Id(Ident::only("a"))),
                                    Box::new(Literal(Number(1.0))))),
                                Box::new(Binary(
                                    Op::Ge,
                                    Box::new(Id(Ident::only("a"))),
                                    Box::new(Literal(Number(10.0))))))),
                    vec![
                        ExprStmt(Apply(Box::new(Id(Ident::only("yes"))), vec![])),
                    ],
                )
            )
        ]);
    }

    #[test]
    fn parse_break() {
        let prog = parse("\
        while true\n\
            break\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(While(Literal(Bool(true)), vec![Break]))
        ]);
    }

    #[test]
    fn parse_continue() {
        let prog = parse("\
        while true\n\
            continue\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(While(Literal(Bool(true)), vec![Continue]))
        ]);
    }

    #[test]
    fn parse_break_and_continue() {
        let prog = parse("\
        while true\n\
            continue\n\
            break\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(While(Literal(Bool(true)), vec![Continue, Break]))
        ]);
    }

    #[test]
    fn parse_for() {
        let prog = parse("\
        var sum = 0\n\
        for i = 0, i < 100, i += 1\n\
            sum += i\n\
        end");
        assert_eq!(prog, vec![
            StmtEntry(Var(Simple(Ident::only("sum"), Literal(Number(0.0))))),
            StmtEntry(For(
                Ident::only("i"),
                Literal(Number(0.0)),
                Binary(Op::Lt, Box::new(Id(Ident::only("i"))), Box::new(Literal(Number(100.0)))),
                Assign(Op::AddAss, Box::new(Id(Ident::only("i"))), Box::new(Literal(Number(1.0)))),
                vec![
                    ExprStmt(Assign(Op::AddAss,
                                    Box::new(Id(Ident::only("sum"))),
                                    Box::new(Id(Ident::only("i")))))
                ])
            ),
        ]);
    }

    #[test]
    fn parse_for_each() {
        let prog = parse("\
        foreach i in range(10) do love(i)");
        assert_eq!(prog, vec![
            StmtEntry(ForEach(
                Ident::only("i"),
                Apply(Box::new(Id(Ident::only("range"))), vec![Literal(Number(10.0))]),
                vec![ExprStmt(Apply(Box::new(Id(Ident::only("love"))), vec![Id(Ident::only("i"))]))],
            ))
        ]);
    }

    #[test]
    fn with_location() {
        let prog = parse("var a = b");
        let a = prog.get(0).unwrap();

        match a {
            StmtEntry(Var(Simple(Ident { text: _, loc: Some(a) },
                                 Id(Ident { text: _, loc: Some(b) })))) => {
                assert_eq!(a.clone(), ((1, 5), (1, 6)));
                assert_eq!(b.clone(), ((1, 9), (1, 10)));
            }
            _ => unreachable!()
        }
    }

    fn parse(input: &str) -> Program {
        CsParser::ast(input).expect("Compile Error")
    }
}
