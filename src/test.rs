use crate::run_on_file;
use crate::util::{StringWriter, Void};

fn test_file_should_succeed(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut writer = StringWriter::new();
    // let mut io = crate::runtime::TestIO::new();
    let mut io = crate::runtime::InMemoryIO::new();

    match run_on_file(&mut io, filename, &mut writer) {
        Err(err) => {
            println!("{}", writer.to_string());
            panic!();
        }
        Ok(code) => {
            println!("return code: {}", code);
            if code != 0 {
                println!("logs:\n{}", io.log.to_string());
                println!("stdout:\n{}", io.out.to_string());
                println!("stderr:\n{}", io.err.to_string());
                panic!();
            }
        }
    }

    // let filename = String::from(filename);
    // assert!(output.to_string() == read_to_string(filename + ".out").expect("why did this fail?"));
}

fn test_file_should_fail(filename: &str) {
    let mut writer = StringWriter::new();

    match run_on_file(Void::new(), filename, &mut writer) {
        Err(_) => println!("{}", writer.to_string()),
        _ => {
            panic!("should have failed");
        }
    }
}

#[test]
fn test_hello_world() {
    test_file_should_succeed("test/hello_world.c");
}
