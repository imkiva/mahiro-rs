use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct CovScriptParser;
