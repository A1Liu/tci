use crate::filedb::FileDb;
use crate::interpreter::Runtime;
use crate::runtime::RuntimeIO;
use crate::util::*;
use crate::{compile, emit_err};
use core::mem;
use std::fs::read_to_string;
use std::io;

pub struct DebugSW(pub StringWriter);

impl io::Write for DebugSW {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // let len = io::stdout().write(buf)?;
        // return self.0.write(&buf[..len]);
        return self.0.write(buf);
    }

    fn flush(&mut self) -> io::Result<()> {
        return io::stdout().flush();
    }
}

pub struct DebugIO {
    pub out: DebugSW,
    pub log: DebugSW,
    pub err: DebugSW,
}

impl DebugIO {
    pub fn new() -> Self {
        Self {
            out: DebugSW(StringWriter::new()),
            log: DebugSW(StringWriter::new()),
            err: DebugSW(StringWriter::new()),
        }
    }
}

impl RuntimeIO for &mut DebugIO {
    type Out = DebugSW;
    type Log = DebugSW;
    type Err = DebugSW;

    fn out(&mut self) -> &mut Self::Out {
        return &mut self.out;
    }
    fn err(&mut self) -> &mut Self::Log {
        return &mut self.err;
    }
    fn log(&mut self) -> &mut Self::Err {
        return &mut self.log;
    }
}

impl RuntimeIO for DebugIO {
    type Out = DebugSW;
    type Log = DebugSW;
    type Err = DebugSW;

    fn out(&mut self) -> &mut Self::Out {
        return &mut self.out;
    }
    fn err(&mut self) -> &mut Self::Log {
        return &mut self.err;
    }
    fn log(&mut self) -> &mut Self::Err {
        return &mut self.log;
    }
}

fn test_file_should_succeed(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new(true);
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

    let mut runtime = Runtime::new(program, &mut io, StringArray::new());
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

fn test_file_runtime_should_fail(filename: &str) {
    let config = codespan_reporting::term::Config::default();
    let mut files = FileDb::new(true);
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

    let mut runtime = Runtime::new(program, &mut io, StringArray::new());
    let code = match runtime.run() {
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
            assert_eq!(err.short_name, "InvalidPointer");
        }
    };
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

#[test]
fn test_stack_locals() {
    test_file_runtime_should_fail("test/stack_locals.c");
}
