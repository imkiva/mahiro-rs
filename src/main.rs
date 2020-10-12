use mahiro_lang::{CompileResult, Compiler};

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
        fn nice(a: UInt32) -> Bool;
    }
    fn main() {
        println("hello")
    }
    "#,
  )?;
  println!("{:#?}", m);

  Ok(())
}
