use mahiro_lang::syntax::parser::ModuleParser;

fn main() {
    let m = ModuleParser::new().parse(r#"
    module Main where
    enum Option<T> {
        Some(T),
        None,
    }
    trait Hello {
        fn say();
        fn nice(a: UInt32) -> Bool;
    }
    fn main() {
        println("hello");
    }
    "#);
    println!("{:#?}", m);
}
