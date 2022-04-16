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
use alloc::string::String;

pub fn print_error(err: &IError, memory: &Memory, files: &FileDb) -> String {
    use crate::util::term::*;
    use crate::util::*;

    let mut out = String::new();

    write!(out, "{}: {}\n", err.short_name, err.message).unwrap();

    for frame in memory.callstack.iter().skip(1) {
        Diagnostic::new()
            .with_labels(vec![Label::new(frame.loc.file, frame.loc)])
            .render(files, &mut out)
            .unwrap();
    }

    if memory.loc != NO_FILE {
        Diagnostic::new()
            .with_labels(vec![Label::new(memory.loc.file, memory.loc)])
            .render(files, &mut out)
            .unwrap();
    }

    return out;
}
