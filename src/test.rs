use crate::filedb::*;
use crate::runtime::*;
use crate::util::*;
use crate::{compile, emit_err};
use interloc::*;
use std::alloc::System;
use std::fs::{read_dir, read_to_string};

fn test_file_should_succeed(files: &FileDb, output_file: Option<&str>) {
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

    println!("compiled");
    let mut runtime = Kernel::new(&program);

    match runtime.run() {
        Ok(0) => {}
        Ok(code) => {
            let mut writer = StringWriter::new();
            for TS(_, s) in &runtime.events() {
                writer.write(s.as_bytes()).unwrap();
            }

            println!("\n{}", writer.into_string());
            println!("\n{:?}", runtime.files);
            panic!("Nonzero return code");
        }
        Err(err) => {
            let mut writer = StringWriter::new();
            for TS(_, s) in &runtime.events() {
                writer.write(s.as_bytes()).unwrap();
            }

            println!("\n{}", writer.into_string());
            let s = print_error(&err, &runtime.memory, files);
            println!("{}", s);
            panic!("\n{:?}", runtime.files);
        }
    };

    let mut writer = StringWriter::new();
    for TS(_, s) in &runtime.events() {
        writer.write(s.as_bytes()).unwrap();
    }

    let output = writer.into_string();

    println!("{}", output);
    if let Some(output_file) = output_file {
        match read_to_string(output_file) {
            Ok(expected) => {
                if output != expected.replace("\r\n", "\n") {
                    println!("left:  {:?}\nright: {:?}", output, expected);
                    panic!();
                }
            }
            Err(_) => {}
        }
    }
}

// fn test_file_compile_should_fail(filename: &str) {
//     let config = codespan_reporting::term::Config::default();
//     let mut files = FileDb::new(true);
//     let mut writer = StringWriter::new();
//
//     files.add_from_fs(filename).unwrap();
//
//     match compile(&mut files) {
//         Err(errs) => {
//             emit_err(&errs, &files, &mut writer);
//             println!("{}", writer.to_string());
//         }
//         _ => panic!("should have failed"),
//     }
// }

// fn test_file_runtime_should_fail(filename: &str, expected_err: &str) {
//     let config = codespan_reporting::term::Config::default();
//     let mut files = FileDb::new(true);
//     let mut writer = StringWriter::new();
//
//     files.add_from_fs(filename).unwrap();
//
//     let program = match compile(&mut files) {
//         Ok(program) => program,
//         Err(errs) => {
//             emit_err(&errs, &files, &mut writer);
//             println!("{}", writer.into_string());
//             panic!();
//         }
//     };
//     mem::drop(files);
//
//     let mut runtime = Runtime::new(program, StringArray::new());
//     let diag = runtime.run(&mut writer);
//     for (idx, op) in runtime.program.ops.iter().enumerate() {
//         println!("op {}: {:?}", idx, op);
//     }
//
//     let err = match diag.status {
//         RuntimeStatus::ErrorExited(err) => err,
//         x => panic!("{:?}", x),
//     };
//
//     assert_eq!(err.short_name, expected_err);
// }

macro_rules! gen_test_should_succeed {
    ( $( $ident:ident ),* ) => {
        $(
            gen_test_should_succeed!(@S, $ident);
        )*
    };
    (@S, $ident:ident) => {
            #[test]
            fn $ident() {
                use std::path::Path;

                let file_path = concat!("lib/test/", stringify!($ident), ".c");
                let folder_path = concat!("lib/test/", stringify!($ident));

                let mut files = FileDb::new();
                if Path::new(file_path).exists() {
                    let out_path = concat!("lib/test/", stringify!($ident), ".c.out");
                    files.add(file_path, &read_to_string(file_path).unwrap()).unwrap();
                    test_file_should_succeed(&files, Some(out_path));
                    return;
                }

                let mut out_path = None;
                for entry in read_dir(Path::new(folder_path)).unwrap() {
                    let path = entry.unwrap().path();
                    let file_path = path.to_str().unwrap();
                    if file_path.ends_with(".out") {
                        out_path = Some(path);
                        continue;
                    }

                    files.add(file_path, &read_to_string(file_path).unwrap()).unwrap();
                }

                let out_path_ref = out_path.as_ref().map(|a| a.to_str()).flatten();
                test_file_should_succeed(&files, out_path_ref);
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
    dyn_array_ptr,
    arrays,
    statics,
    memory,
    files
);

// gen_test_runtime_should_fail!((stack_locals, "InvalidPointer"));
//
//

pub struct TestMonitor {
    local: ThreadMonitor,
}

impl TestMonitor {
    // This needs to be const to be usable in static functions
    pub const fn new() -> Self {
        Self {
            local: ThreadMonitor::new(),
        }
    }

    pub fn local_info(&self) -> interloc::AllocInfo {
        self.local.info()
    }
}

// use std::sync::atomic::*;
// static SHOULD_FAIL: AtomicBool = AtomicBool::new(false);

impl AllocMonitor for TestMonitor {
    fn monitor(&self, layout: Layout, action: AllocAction) {
        // if let AllocAction::Realloc { .. } = action {
        //     if SHOULD_FAIL.load(Ordering::SeqCst) {
        //         unsafe { std::ptr::null_mut::<i32>().write(42) };
        //     }
        // }

        self.local.monitor(layout, action);
    }
}

pub static TEST_MONITOR: TestMonitor = TestMonitor::new();

#[global_allocator]
static GLOBAL: InterAlloc<System, TestMonitor> = InterAlloc {
    inner: System,
    monitor: &TEST_MONITOR,
};

pub fn before_alloc() -> interloc::AllocInfo {
    // SHOULD_FAIL.store(true, Ordering::SeqCst);
    TEST_MONITOR.local_info()
}

pub fn after_alloc<T>(obj: T, before: interloc::AllocInfo) -> interloc::AllocInfo {
    core::mem::drop(obj);
    let diff = TEST_MONITOR.local_info().relative_to(&before);
    assert!(
        diff.bytes_alloc == diff.bytes_dealloc,
        "Diff is {:#?} before is {:?}",
        diff,
        before
    );

    return diff;
}
