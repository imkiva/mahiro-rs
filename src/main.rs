use mahiro_lang::{CompileResult, Compiler};
use mahiro_lang::syntax::pp::PrettyPrinter;

fn main() -> CompileResult<()> {
  let m = Compiler::compile(
    r#"
    module Main where
    enum Option<T> {
        Some(T),
        None,
    }
    trait Hello {
        fn say();
        fn nice(a: UInt32) -> Option<Option<Int32>>;
    }
    fn main() {
        println("hello");
    }
    "#,
  )?;
  println!("{:#?}", m);
  
  println!("Pretty Print");
  println!("============");
  let pp = PrettyPrinter::new(2);
  pp.print_module(&m);
  
  Ok(())
}
