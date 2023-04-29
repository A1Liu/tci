#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub start: u32,
}

impl Error {
    pub fn new(s: &str, start: u32) -> Error {
        return Error {
            message: s.to_string(),
            start,
        };
    }
}
