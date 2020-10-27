use crate::filedb::FileDb;
use crate::interpreter::Runtime;
use crate::runtime::DebugIO;
use crate::util::StringWriter;
use crate::{compile, emit_err};
use core::mem;
use std::fs::read_to_string;

fn test_file_should_succeed(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new();
    let mut writer = StringWriter::new();
    let mut io = DebugIO::new();

    files.add_from_fs(filename).unwrap();

    let program = match compile(&mut files) {
        Ok(program) => program,
        Err(errs) => {
            emit_err(&errs, &files, &mut writer);
            println!("{}", writer.into_string());
            panic!();
        }
    };
    mem::drop(files);

    let mut runtime = Runtime::new(program, &mut io);
    let code = match runtime.run() {
        Ok(code) => code,
        Err(err) => {
            for (idx, op) in program.ops.iter().enumerate() {
                println!("op {}: {:?}", idx, op);
            }

            println!("{:?}\n", err);
            panic!();
        }
    };

    println!("return code: {}", code);
    if code != 0 {
        for (idx, op) in program.ops.iter().enumerate() {
            println!("op {}: {:?}", idx, op);
        }

        panic!();
    }

    let output = io.out.0.to_string();
    match read_to_string(String::from(filename) + ".out") {
        Ok(expected) => {
            if output != expected.replace("\r\n", "\n") {
                for (idx, op) in program.ops.iter().enumerate() {
                    println!("op {}: {:?}", idx, op);
                }

                panic!();
            }
        }
        Err(_) => {}
    }
}

fn test_file_compile_should_fail(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new();
    let mut writer = StringWriter::new();

    files.add_from_fs(filename).unwrap();

    match compile(&mut files) {
        Err(errs) => {
            emit_err(&errs, &files, &mut writer);
            println!("{}", writer.to_string());
        }
        _ => panic!("should have failed"),
    }
}

#[test]
fn test_hello_world() {
    test_file_should_succeed("test/hello_world.c");
}

#[test]
fn test_assign() {
    test_file_should_succeed("test/assign.c");
}

#[test]
fn test_structs() {
    test_file_should_succeed("test/structs.c");
}

#[test]
fn test_includes() {
    test_file_should_succeed("test/includes.c");
}

#[test]
fn test_control_flow() {
    test_file_should_succeed("test/control_flow.c");
}

#[test]
fn test_macros() {
    test_file_should_succeed("test/macros.c");
}
