use std::ops::Range;

pub struct Error {
    message: String,
    sections: Vec<(Range<u32>, String)>,
}

impl Error {
    pub fn new(message: String, sections: Vec<(Range<u32>, String)>) -> Error {
        Self { message, sections }
    }
}
