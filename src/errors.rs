use codespan_reporting::diagnostic::Diagnostic;
use std::ops::Range;

pub struct Error {
    pub message: String,
    pub sections: Vec<(Range<u32>, String)>,
}

impl Error {
    pub fn new(message: String, sections: Vec<(Range<u32>, String)>) -> Error {
        Self { message, sections }
    }
}
