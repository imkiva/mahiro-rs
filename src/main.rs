use mahiro_lang::syntax::parser::ModuleParser;

fn main() {
    let m = ModuleParser::new().parse(r#"
    module Main where
    fn main() {
        println("hello");
    }
    "#);
    println!("{:#?}", m);
}
