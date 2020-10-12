use mahiro_lang::{syntax::pp::PrettyPrinter, CompileResult, Compiler};

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
        if a {
            fuck;
        } else {
            shit;
        };
        while true {
            fuck;
            shit;
        };
        for i in shit {
            i.drop();
            i.take();
        };
        match nice() {
            Some(a) => fuck,
            Some(_) => shit,
            Some((_, _, a, _, _)) => shit,
            None => apple,
            _ => {
                return "hello world";
            }
        };
        let Some(t) = a;
        let Some(_) = a;
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
