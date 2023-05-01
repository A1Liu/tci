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

#[derive(Debug)]
pub enum Error {
    Todo(&'static str),

    UnrecognizedCharacter { idx: u32 },

    UnrecognizedToken { idx: u32 },
}

impl Error {
    pub fn message(&self) -> String {
        use Error::*;

        match *self {
            Todo(message) => format!("{}", message),

            UnrecognizedCharacter { idx } => format!("unrecognized character"),
            UnrecognizedToken { idx } => format!("unrecognized token"),
        }
    }
}
