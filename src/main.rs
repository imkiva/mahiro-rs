use std::{env, fs};
use covscript_compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.last().unwrap();
    let contents = fs::read_to_string(file.as_str())
        .expect("Something went wrong reading the file");
    if let Err(e) = Compiler::compile_to_ast(contents.as_str()) {
        println!("{}", e.error_message(file.as_str(), contents.as_str()));
    }
}
