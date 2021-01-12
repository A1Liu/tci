use crate::filedb::*;
use crate::interpreter::Runtime;
use crate::runtime::*;
use crate::util::*;
use crate::{compile, emit_err};
use core::mem;
use std::fs::read_to_string;

fn test_file_should_succeed(files: &mut FileDb, output_file: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut writer = StringWriter::new();

    let program = match compile(files) {
        Ok(program) => program,
        Err(errs) => {
            emit_err(&errs, &files, &mut writer);
            println!("{}", writer.into_string());
            panic!();
        }
    };

    mem::drop(files);

    // for (idx, op) in program.ops.iter().enumerate() {
    //     println!("op {}: {:?}", idx, op.op);
    // }

    let mut runtime = Runtime::new(program, StringArray::new());

    let diag = runtime.run(&mut writer, &mut smol::io::repeat(0));
    let code = match diag.status {
        RuntimeStatus::Exited(code) => code,
        RuntimeStatus::ErrorExited(err) => {
            println!("error is: {:?}", err);
            1
        }
        x => panic!("runtime status is: {:?}", x),
    };

    println!("return code: {}", code);
    let output = writer.into_string();

    if code != 0 {
        println!("{}", output);
        println!("pc: {}", runtime.pc());
        panic!();
    }

    println!("{}", output);
    match read_to_string(output_file) {
        Ok(expected) => {
            if output != expected.replace("\r\n", "\n") {
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
    let diag = runtime.run(&mut writer, &mut smol::io::repeat(0));
    for (idx, op) in runtime.program.ops.iter().enumerate() {
        println!("op {}: {:?}", idx, op);
    }

    let err = match diag.status {
        RuntimeStatus::ErrorExited(err) => err,
        x => panic!("{:?}", x),
    };

    assert_eq!(err.short_name, expected_err);
}

macro_rules! gen_test_should_succeed {
    ( $( $ident:tt ),* ) => {
        $(
            gen_test_should_succeed!(@S, $ident);
        )*
    };
    (@S, ( $folder:literal, $name:ident, $( $ident:ident),* ) ) => {
            #[test]
            fn $name() {
                let mut files = FileDb::new(true);
                files.add_from_fs(concat!("test/", $folder, stringify!($name), ".c")).unwrap();
                $(
                files.add_from_fs(concat!("test/", $folder, stringify!($ident), ".c")).unwrap();
                )*

                test_file_should_succeed(&mut files,concat!("test/", $folder, stringify!($name), ".c.out"));
            }
    };
    (@S, $ident:ident) => {
            #[test]
            fn $ident() {
                let mut files = FileDb::new(true);
                files.add_from_fs(concat!("test/", stringify!($ident), ".c")).unwrap();
                test_file_should_succeed(&mut files,concat!("test/", stringify!($ident), ".c.out"));
            }
    };

}

// macro_rules! gen_test_runtime_should_fail {
//     ( $( ($ident:ident, $expr:expr ) ),* ) => {
//         $(
//             #[test]
//             fn $ident() {
//                 test_file_runtime_should_fail(concat!("test/", stringify!($ident), ".c"), $expr);
//             }
//         )*
//     };
// }

gen_test_should_succeed!(
    hello_world,
    assign,
    structs,
    includes,
    control_flow,
    macros,
    binary_search,
    bitwise_operators,
    bool_operators,
    assign_operators,
    exit,
    ("dyn_array_ptr/", dyn_array_ptr, main),
    ("arrays/", arrays, main),
    ("statics/", statics, main)
);

// gen_test_runtime_should_fail!((stack_locals, "InvalidPointer"));
