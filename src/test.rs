use crate::filedb::FileDb;
use crate::interpreter::{render_err, Runtime};
use crate::util::*;
use crate::{compile, emit_err};
use core::mem;
use std::fs::read_to_string;

fn test_file_should_succeed(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new(true);
    let mut writer = StringWriter::new();

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

    let mut runtime = Runtime::new(program, StringArray::new());

    let code = match runtime.run(&mut writer) {
        Ok(c) => c,
        Err(err) => {
            let print = render_err(&err, &runtime.memory.callstack, &program);
            for (idx, op) in program.ops.iter().enumerate() {
                println!("op {}: {:?}", idx, op);
            }

            panic!("{}", print);
        }
    };

    println!("return code: {}", code);
    if code != 0 {
        for (idx, op) in program.ops.iter().enumerate() {
            println!("op {}: {:?}", idx, op);
        }

        panic!();
    }

    let output = writer.into_string();
    match read_to_string(String::from(filename) + ".out") {
        Ok(expected) => {
            if output != expected.replace("\r\n", "\n") {
                // for (idx, op) in program.ops.iter().enumerate() {
                //     println!("op {}: {:?}", idx, op);
                // }

                println!("left: {:?}\nright: {:?}", output, expected);
                panic!();
            }
        }
        Err(_) => {}
    }
}

fn test_file_compile_should_fail(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new(true);
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

fn test_file_runtime_should_fail(filename: &str, expected_err: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new(true);
    let mut writer = StringWriter::new();

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

    let mut runtime = Runtime::new(program, StringArray::new());
    let code = match runtime.run(&mut writer) {
        Ok(code) => {
            for (idx, op) in program.ops.iter().enumerate() {
                println!("op {}: {:?}", idx, op);
            }

            panic!();
        }
        Err(err) => {
            for (idx, op) in program.ops.iter().enumerate() {
                println!("op {}: {:?}", idx, op);
            }

            println!("{:?}\n", err);
            assert_eq!(err.short_name, expected_err);
        }
    };
}

macro_rules! gen_test_should_succeed {
    ( $( $ident:ident ),* ) => {
        $(
            #[test]
            fn $ident() {
                test_file_should_succeed(concat!("test/", stringify!($ident), ".c"));
            }
        )*
    };
}

macro_rules! gen_test_runtime_should_fail {
    ( $( ($ident:ident, $expr:expr ) ),* ) => {
        $(
            #[test]
            fn $ident() {
                test_file_runtime_should_fail(concat!("test/", stringify!($ident), ".c"), $expr);
            }
        )*
    };
}

gen_test_should_succeed!(hello_world, assign, structs, includes, control_flow, macros);
gen_test_runtime_should_fail!((stack_locals, "InvalidPointer"));
