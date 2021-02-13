#[macro_use]
pub mod error;

pub mod fs;
pub mod interpreter;
pub mod kernel;
pub mod memory;
pub mod types;

pub use error::*;
pub use fs::*;
pub use interpreter::*;
pub use kernel::*;
pub use memory::*;
pub use types::*;

use crate::filedb::FileDb;
use crate::util::*;
use alloc::string::String;

pub fn print_error(err: &IError, memory: &Memory, files: &FileDb) -> String {
    let mut out = StringWriter::new();

    write!(out, "{}: {}\n", err.short_name, err.message).unwrap();

    for frame in memory.callstack.iter().skip(1) {
        files.write_loc(&mut out, frame.loc).unwrap();
        write!(out, "\n").unwrap();
        files.display_loc(&mut out, frame.loc).unwrap();
    }

    if memory.loc != NO_FILE {
        files.write_loc(&mut out, memory.loc).unwrap();
        write!(out, "\n").unwrap();
        files.display_loc(&mut out, memory.loc).unwrap();
    }

    return out.to_string();
}
