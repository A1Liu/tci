// translate file by removing all the `\'-`\n' characters,
// then during lexing, get the set of file start-points
// to use during error generation.

#[derive(Debug, Clone)]
pub struct ErrorContext {
    translation_unit: u32,
    errors: Vec<Error>,
}

impl ErrorContext {
    pub fn new(translation_unit: u32) -> Self {
        return Self {
            translation_unit,
            errors: Vec::new(),
        };
    }

    pub fn add(&mut self, err: Error) {
        self.errors.push(err);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Any { message: &'static str, idx: u32 },

    UnrecognizedCharacter { idx: u32 },

    UnrecognizedToken { idx: u32 },
}

impl Error {
    pub fn message(&self) -> String {
        use Error::*;

        match *self {
            Any { message, idx } => format!("{}", message),

            UnrecognizedCharacter { idx } => format!("unrecognized character"),
            UnrecognizedToken { idx } => format!("unrecognized token"),
        }
    }
}
