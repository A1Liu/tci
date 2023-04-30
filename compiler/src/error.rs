// Book-keeping to track which ranges belong to which file, so that we can
// compute file and line number from `start`
#[derive(Debug, Clone, Copy)]
pub struct FileStarts {
    pub index: u32,
    pub file: u32,

    // TODO: For now we assume that there's no escaped newlines in the source (`\` followed by newline)
    // when that feature is added, we'd then need to go back in and fix up the file_index further.
    pub file_index: usize,
}

#[derive(Debug, Clone)]
pub struct TranslationUnitDebugInfo {
    pub file_starts: Vec<FileStarts>,
}

#[derive(Debug, Clone)]
pub struct ErrorContext {
    translation_unit: TranslationUnitDebugInfo,
    errors: Vec<Error>,
}

impl ErrorContext {
    pub fn new(translation_unit: TranslationUnitDebugInfo) -> Self {
        return Self {
            translation_unit,
            errors: Vec::new(),
        };
    }

    pub fn add(&mut self, err: Error) {
        self.errors.push(err);
    }

    pub fn todo(&mut self, message: &'static str) {
        self.errors.push(Error::Todo { message });
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Todo { message: &'static str },

    UnrecognizedCharacter { idx: u32 },

    UnrecognizedToken { idx: u32 },
}

impl Error {
    pub fn message(&self) -> String {
        use Error::*;

        match *self {
            Todo { message } => format!("{}", message),

            UnrecognizedCharacter { idx } => format!("unrecognized character"),
            UnrecognizedToken { idx } => format!("unrecognized token"),
        }
    }
}
