use crate::tree::*;

use std::collections::VecDeque;
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error;
use pest::error::ErrorVariant;
use crate::tree::Entry::{HeaderEntry, StmtEntry};
use crate::tree::Header::{Using, Import, Package};
use crate::tree::Param::{Normal, Varargs};
use crate::tree::VarInit::{Simple, Structured};
use crate::tree::Case::{Sth, Dft};
use crate::tree::Lit::{Null, Number, Bool, Str,
                       Char, Array};
use crate::tree::Expr::{Literal, Ternary, Question, Binary,
                        Unary, Id, Lambda, Group,
                        Apply, Assign};
use crate::tree::Stmt::{Break, Continue, Throw, Return,
                        VarList, Var, Func, Struct,
                        Namespace, Block, If, While,
                        For, ForEach, Loop,
                        Try, ExprStmt, Switch};

pub type ParseErrorVariant = ErrorVariant<Rule>;
pub type ParseError = Error<Rule>;

#[derive(Debug)]
pub struct CompileError(pub ParseError);

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CsParser;

/// macro that extract the first child of a node
macro_rules! fst {
    ($e:expr) => (($e).into_inner().into_iter().next());
}

trait ParseTo<T> {
    fn parse_to(self) -> T;
}

//////////////////////////////// Program

impl ParseTo<Program> for Pairs<'_, Rule> {
    fn parse_to(self) -> Program {
        self.into_iter()
            .flat_map(|item| item.into_inner())
            .filter_map(|node| match node.as_rule() {
                Rule::header => Some(HeaderEntry(node.parse_to())),
                Rule::stmt => Some(StmtEntry(node.parse_to())),
                Rule::EOI => None,
                _ => unreachable!(),
            })
            .collect()
    }
}

//////////////////////////////// Expression

impl ParseTo<Expr> for Pair<'_, Rule> {
    fn parse_to(self) -> Expr {
        match self.as_rule() {
            Rule::expr => fst!(self).unwrap().parse_to(),
            Rule::ternary_expr => {
                let mut iter = self.into_inner().into_iter();
                let expr: Expr = iter.next().unwrap().parse_to();
                match iter.next().map(|ternary| ternary.parse_to()) {
                    Some(Literal(Lit::Pair(t, f))) => Ternary(expr.into(), t, f),
                    Some(f) => Question(expr.into(), f.into()),
                    _ => expr
                }
            }

            // binary operators
            Rule::logic_or_expr |
            Rule::logic_and_expr |
            Rule::relation_expr |
            Rule::add_expr |
            Rule::mul_expr |
            Rule::pow_expr => {
                let mut exprs = VecDeque::new();
                let mut ops = Vec::new();

                for inner in self.into_inner() {
                    match inner.as_rule() {
                        Rule::logic_and_expr => exprs.push_back(inner),
                        Rule::relation_expr => exprs.push_back(inner),
                        Rule::add_expr => exprs.push_back(inner),
                        Rule::mul_expr => exprs.push_back(inner),
                        Rule::pow_expr => exprs.push_back(inner),
                        Rule::unary_expr => exprs.push_back(inner),

                        Rule::or_op => ops.push(inner),
                        Rule::and_op => ops.push(inner),
                        Rule::relation_op => ops.push(inner),
                        Rule::add_op => ops.push(inner),
                        Rule::mul_op => ops.push(inner),
                        Rule::pow_op => ops.push(inner),

                        Rule::logic_or_expr => unreachable!("sanity check: logic_or_expr"),
                        _ => unreachable!("unsatisfied binary expr"),
                    }
                }

                let lhs = exprs.pop_front().unwrap().parse_to();

                ops.into_iter().fold(lhs, |lhs, op| {
                    let rhs = exprs.pop_front().unwrap().parse_to();
                    Binary(op.parse_to(),
                           Box::new(lhs),
                           Box::new(rhs))
                })
            }

            Rule::unary_expr => {
                let mut iter = self.into_inner().into_iter();
                let first = iter.next().unwrap();
                match first.as_rule() {
                    Rule::unary_op => {
                        let expr = iter.next().unwrap().parse_to();
                        Unary(first.parse_to(), Box::new(expr))
                    }
                    Rule::primary_expr => {
                        let expr = first.parse_to();
                        match iter.next() {
                            Some(inc_dec) => Unary(inc_dec.parse_to(), Box::new(expr)),
                            _ => expr,
                        }
                    }
                    _ => unreachable!(),
                }
            }

            Rule::primary_expr => {
                let mut iter = self.into_inner().into_iter();
                let result = iter.next().unwrap().parse_to();
                iter.flat_map(|postfix| postfix.into_inner())
                    .fold(result, build_primary_expr)
            }

            // sub-rule of Rule::primary_expr
            Rule::primary_prefix => {
                let iter = self.into_inner().into_iter();
                let first = iter.peek().unwrap();
                match first.as_rule() {
                    Rule::literal |
                    Rule::lambda => first.parse_to(),
                    Rule::id => Id(first.as_str().into()),
                    Rule::expr => Group(iter.map(|expr| expr.parse_to()).collect()),
                    _ => unreachable!(),
                }
            }

            // sub-rule of Rule::primary_postfix
            Rule::primary_postfix => unreachable!("sanity check"),

            // sub-rule of Rule::primary_prefix
            Rule::literal => {
                let child = fst!(self).unwrap();
                match child.as_rule() {
                    Rule::number_lit => Literal(Number(child.as_str().parse::<f64>().unwrap())),
                    Rule::bool_lit => Literal(Bool(child.as_str().parse::<bool>().unwrap())),
                    Rule::null_lit => Literal(Null),
                    Rule::string_lit => {
                        // remove quote marks
                        let s = child.as_str().to_owned();
                        let s = s[1..s.len() - 1].into();
                        Literal(Str(unescape(s)))
                    }
                    Rule::char_lit => {
                        let s = child.as_str().to_owned();
                        let peek = s[1..2].parse::<char>().unwrap();
                        match peek {
                            '\\' => Literal(Char(unescape_char(s[2..3].parse::<char>().unwrap()))),
                            _ => Literal(Char(peek)),
                        }
                    }
                    Rule::array_lit => child.parse_to(),
                    _ => unreachable!(),
                }
            }

            // sub-rule of Rule::literal
            Rule::array_lit => {
                match fst!(self) {
                    Some(args) => Literal(Array(args.parse_to())),
                    _ => Literal(Array(vec![])),
                }
            }

            // sub-rule of Rule::primary_prefix
            Rule::lambda => {
                let mut iter = self.into_inner().into_iter();
                let callable_params = iter.next().unwrap();
                let expr = iter.next().unwrap();
                Lambda(callable_params.parse_to(), Box::new(expr.parse_to()))
            }

            _ => unreachable!("run out of expr compiler"),
        }
    }
}

impl ParseTo<Header> for Pair<'_, Rule> {
    fn parse_to(self) -> Header {
        let child = fst!(self).unwrap();
        match child.as_rule() {
            Rule::using_decl => Using(fst!(child).unwrap().parse_to()),
            Rule::import_decl => {
                let mut iter = child.into_inner().into_iter();
                Import(iter.next().unwrap().parse_to(),
                       iter.next().map(|id| id.parse_to()))
            }
            Rule::package_decl => Package(fst!(child).unwrap().parse_to()),
            _ => unreachable!(),
        }
    }
}

impl ParseTo<Name> for Pair<'_, Rule> {
    fn parse_to(self) -> String {
        match self.as_rule() {
            Rule::mod_name => self.into_inner().into_iter()
                .map(|id| id.parse_to())
                .collect::<Vec<String>>()
                .join("."),

            Rule::id => self.as_str().to_owned(),

            _ => unreachable!()
        }
    }
}

impl ParseTo<Vec<Name>> for Pair<'_, Rule> {
    fn parse_to(self) -> Vec<Name> {
        match self.as_rule() {
            Rule::params => self.into_inner().into_iter()
                .map(|id| id.parse_to())
                .collect(),

            _ => unreachable!(),
        }
    }
}

impl ParseTo<Vec<Param>> for Pair<'_, Rule> {
    fn parse_to(self) -> Vec<Param> {
        match self.as_rule() {
            Rule::params => self.into_inner().into_iter()
                .map(|id| Normal(id.parse_to()))
                .collect(),

            Rule::callable_params => {
                self.into_inner().into_iter()
                    .flat_map(|child| match child.as_rule() {
                        Rule::params => child.parse_to(),
                        Rule::varargs_param => vec![Varargs(fst!(child).unwrap().parse_to())],
                        _ => unreachable!(),
                    })
                    .collect()
            }

            _ => unreachable!(),
        }
    }
}

impl ParseTo<Vec<Expr>> for Pair<'_, Rule> {
    fn parse_to(self) -> Vec<Expr> {
        match self.as_rule() {
            Rule::args => self.into_inner().into_iter()
                .map(|expr| expr.parse_to())
                .collect(),

            _ => unreachable!(),
        }
    }
}

impl ParseTo<Op> for Pair<'_, Rule> {
    fn parse_to(self) -> Op {
        match self.as_str() {
            "and" | "&&" => Op::And,
            "or" | "||" => Op::Or,
            "not" | "!" => Op::Not,
            ">=" => Op::Ge,
            ">" => Op::Gt,
            "<=" => Op::Le,
            "<" => Op::Lt,
            "==" => Op::Eq,
            "!=" => Op::Ne,
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "^" => Op::Pow,
            "%" => Op::Mod,
            "=" => Op::Assign,
            "+=" => Op::AddAss,
            "-=" => Op::SubAss,
            "*=" => Op::MulAss,
            "/=" => Op::DivAss,
            "^=" => Op::PowAss,
            "%=" => Op::ModAss,
            "new" => Op::New,
            "gcnew" => Op::GcNew,
            "typeid" => Op::Typeid,

            // TODO: consider making it elegant
            // or using a more portable way.
            // this may be changed in the future
            "++" => Op::Inc(
                if self.as_rule() == Rule::unary_op {
                    OpFix::Prefix
                } else {
                    OpFix::Postfix
                }
            ),

            "--" => Op::Dec(
                if self.as_rule() == Rule::unary_op {
                    OpFix::Prefix
                } else {
                    OpFix::Postfix
                }
            ),

            _ => unreachable!(),
        }
    }
}

fn unescape(input: &str) -> String {
    let mut str = String::with_capacity(input.len());
    let mut escape = false;
    for ch in input.chars() {
        if escape {
            escape = false;
            str.push(unescape_char(ch));
        } else {
            match ch {
                '\\' => escape = true,
                _ => str.push(ch),
            }
        }
    }
    str
}

fn unescape_char(ch: char) -> char {
    match ch {
        't' => '\t',
        'n' => '\n',
        'r' => '\r',
        'a' => '\u{07}',
        'b' => '\u{08}',
        'f' => '\u{0C}',
        'v' => '\u{0B}',
        '0' => '\0',
        '\'' => '\'',
        '\"' => '\"',
        '\\' => '\\',
        _ => ch,
    }
}

fn build_primary_expr(prefix: Expr, postfix: Pair<Rule>) -> Expr {
    match postfix.as_rule() {
        Rule::apply => Apply(Box::new(prefix),
                             fst!(postfix)
                                 .map(|args| args.parse_to())
                                 .unwrap_or_default()),

        Rule::index_access => Binary(Op::Index,
                                     Box::new(prefix),
                                     Box::new(fst!(postfix).unwrap().parse_to())),

        Rule::member_access => Binary(Op::Access,
                                      Box::new(prefix),
                                      Box::new(Id(fst!(postfix).unwrap().parse_to()))),

        Rule::mapping => {
            let key = Box::new(prefix);
            let value = Box::new(fst!(postfix).unwrap().parse_to());
            Literal(Lit::Pair(key, value))
        }

        Rule::assign => {
            let mut iter = postfix.into_inner().into_iter();
            let op = iter.next().unwrap();
            let expr = iter.next().unwrap();
            Assign(op.parse_to(), Box::new(prefix), Box::new(expr.parse_to()))
        }

        Rule::flatten => Unary(Op::Flatten, Box::new(prefix)),

        _ => unreachable!(),
    }
}

//////////////////////////////// Statement

impl ParseTo<Stmt> for Pair<'_, Rule> {
    fn parse_to(self) -> Stmt {
        match self.as_rule() {
            Rule::stmt => fst!(self).unwrap().parse_to(),
            Rule::cross_line_stmt => fst!(self).unwrap().parse_to(),
            Rule::primary_stmt => fst!(self).unwrap().parse_to(),
            Rule::throw_stmt => Throw(fst!(self).unwrap().parse_to()),
            Rule::return_stmt => Return(fst!(self).map(|expr| expr.parse_to())),

            // variable declaration
            Rule::var_decl => {
                let mut vars: Vec<Pair<Rule>> = self.into_inner().into_iter().collect();
                if vars.len() == 1 {
                    vars.pop().unwrap().parse_to()
                } else {
                    VarList(vars.into_iter().map(|init|
                        match init.parse_to() {
                            Stmt::Var(init) => init,
                            _ => unreachable!()
                        })
                        .collect())
                }
            }
            // sub-rule of var_decl
            Rule::var_init => {
                let mut iter = self.into_inner().into_iter();
                let first = iter.next().unwrap();
                let expr = iter.next().unwrap().parse_to();
                match first.as_rule() {
                    Rule::id => Var(Simple(first.parse_to(), expr)),
                    Rule::params => Var(Structured(first.parse_to(), expr)),
                    _ => unreachable!(),
                }
            }

            Rule::func_decl => {
                let mut iter = self.into_inner().into_iter();
                let id = iter.next().unwrap().parse_to();
                let callable_params = iter.next().unwrap().parse_to();
                let body = iter.next().unwrap().parse_to();
                Func(id, callable_params, body)
            }

            Rule::struct_decl => {
                let mut iter = self.into_inner().into_iter();
                let id = iter.next().unwrap().parse_to();
                let extends = match iter.peek().map(|p| p.as_rule()) {
                    Some(Rule::expr) => iter.next().map(|expr| expr.parse_to()),
                    _ => None,
                };
                let body = iter.next().unwrap().parse_to();
                Struct(id, extends, body)
            }

            Rule::namespace_decl => {
                let mut iter = self.into_inner().into_iter();
                let id = iter.next().unwrap().parse_to();
                let body = iter.next().unwrap().parse_to();
                Namespace(id, body)
            }

            Rule::block_decl => Block(fst!(self).unwrap().parse_to()),

            Rule::if_stmt => {
                let mut iter = self.into_inner().into_iter();
                let cond = iter.next().unwrap().parse_to();
                let t = iter.next().unwrap().parse_to();
                let f = iter.next().map(|f| f.parse_to());
                If(cond, t, f)
            }

            Rule::while_stmt => {
                let mut iter = self.into_inner().into_iter();
                let cond = iter.next().unwrap();
                let body = iter.next().unwrap();
                While(cond.parse_to(), body.parse_to())
            }

            Rule::switch_stmt => {
                let mut iter = self.into_inner().into_iter();
                let expr = iter.next().unwrap().parse_to();
                let cases = iter.map(|case| case.parse_to()).collect();
                Switch(expr, cases)
            }

            Rule::for_stmt => {
                let mut iter = self.into_inner().into_iter();
                let id = iter.next().unwrap().parse_to();
                let init = iter.next().unwrap().parse_to();
                let cond = iter.next().unwrap().parse_to();
                let post = iter.next().unwrap().parse_to();
                let body = iter.next().unwrap().parse_to();
                For(id, init, cond, post, body)
            }

            Rule::for_each_stmt => {
                let mut iter = self.into_inner().into_iter();
                let id = iter.next().unwrap().parse_to();
                let expr = iter.next().unwrap().parse_to();
                let body = iter.next().unwrap().parse_to();
                ForEach(id, expr, body)
            }

            Rule::loop_until_stmt => {
                let mut iter = self.into_inner().into_iter();
                let body = iter.next().unwrap().parse_to();
                let cond = iter.next().map(|expr| expr.parse_to());
                Loop(cond, body)
            }

            Rule::loop_control => fst!(self).unwrap().parse_to(),
            Rule::break_ => Break,
            Rule::continue_ => Continue,

            Rule::try_stmt => {
                let mut iter = self.into_inner().into_iter();
                let try_body = iter.next().unwrap().parse_to();
                let id = iter.next().unwrap().parse_to();
                let catch_body = iter.next().unwrap().parse_to();
                Try(try_body, id, catch_body)
            }

            Rule::expr_stmt => {
                let mut iter = self.into_inner().into_iter();
                let first = iter.next().unwrap();
                match first.as_rule() {
                    Rule::primary_prefix => {
                        let prefix = first.parse_to();
                        match iter.peek().unwrap().as_rule() {
                            Rule::inc_dec =>
                                ExprStmt(Unary(iter.next().unwrap().parse_to(),
                                               Box::new(prefix))),

                            Rule::primary_postfix =>
                                ExprStmt(iter.flat_map(|postfix| postfix.into_inner())
                                    .fold(prefix, build_primary_expr)),

                            _ => unreachable!(),
                        }
                    }

                    Rule::inc_dec => {
                        let op = match first.parse_to() {
                            Op::Inc(_) => Op::Inc(OpFix::Prefix),
                            Op::Dec(_) => Op::Dec(OpFix::Prefix),
                            _ => unreachable!(),
                        };
                        ExprStmt(Unary(op, Box::new(iter.next().unwrap().parse_to())))
                    }

                    _ => unreachable!(),
                }
            }

            _ => unreachable!()
        }
    }
}

impl ParseTo<Vec<Stmt>> for Pair<'_, Rule> {
    fn parse_to(self) -> Vec<Stmt> {
        match self.as_rule() {
            Rule::common_body => self.into_inner().into_iter()
                .map(|stmt| stmt.parse_to())
                .collect(),

            Rule::struct_body => self.into_inner().into_iter()
                .map(|decl| decl.parse_to())
                .collect(),

            Rule::for_body => {
                let first = fst!(self).unwrap();
                match first.as_rule() {
                    Rule::stmt => vec![first.parse_to()],
                    Rule::common_body => first.parse_to(),
                    _ => unreachable!(),
                }
            }

            _ => unreachable!()
        }
    }
}

impl ParseTo<Case> for Pair<'_, Rule> {
    fn parse_to(self) -> Case {
        match self.as_rule() {
            Rule::switch_case => fst!(self).unwrap().parse_to(),
            Rule::sth_case => {
                let mut iter = self.into_inner().into_iter();
                let expr = iter.next().unwrap().parse_to();
                let body = iter.next()
                    .map(|body| body.parse_to())
                    .unwrap_or_default();
                Sth(expr, body)
            }
            Rule::default_case => {
                Dft(fst!(self).map(|body| body.parse_to()).unwrap_or_default())
            }
            _ => unreachable!(),
        }
    }
}

impl CsParser {
    pub fn ast(input: &str) -> CompileResult<Program> {
        let parse = CsParser::parse(Rule::program, input);
        let pairs = parse.map_err(|e| CompileError(e))?;
        Ok(pairs.parse_to())
    }
}
