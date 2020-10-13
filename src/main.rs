use mahiro_lang::{syntax::pp::PrettyPrinter, CompileResult, Compiler};

fn main() -> CompileResult<()> {
  let input = r#"
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
        }
        while true {
            fuck;
            shit;
        }
        for i in shit {
            i.drop();
            i.take();
        }
        match nice() {
            Some(a) => fuck,
            Some(_) => shit,
            Some((_, _, a, _, _)) => shit,
            None => apple,
            _ => {
                return "hello world";
            }
        }
        let Some(_) = a;
        let Some(_) = a;
        let a = compile(it)?;
    }
    "#;
  
  let m = Compiler::compile(input);
  
  match m {
    Ok(m) => {
      println!("{:#?}", m);
  
      println!("Pretty Print");
      println!("============");
      let pp = PrettyPrinter::new(2);
      pp.print_module(&m);
    },
    
    Err(e) => {
      println!("{}", e.error_message(input, "<stdin>"));
    }
  }
  
  Ok(())
}
