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

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ErrorKind {
    Todo(String),

    DidntRun,
    NotImplemented(String),

    UnrecognizedCharacter { idx: u32 },

    UnrecognizedToken { idx: u32 },
}

macro_rules! error {
    ($e:ident) => {
        Error::new(crate::error::ErrorKind::$e)
    };
    ($e:ident $t:tt) => {
        Error::new(crate::error::ErrorKind::$e $t)
    };
}

macro_rules! throw {
    ($e:ident) => {
        return Err(Error::new(crate::error::ErrorKind::$e))
    };
    ($e:ident $t:tt) => {
        return Err(Error::new(crate::error::ErrorKind::$e $t))
    };
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,

    backtrace: Option<std::backtrace::Backtrace>,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        return Error {
            kind,

            #[cfg(debug_assertions)]
            backtrace: Some(std::backtrace::Backtrace::capture()),

            #[cfg(not(debug_assertions))]
            backtrace: None,
        };
    }

    pub fn message(&self) -> String {
        use ErrorKind::*;

        match &self.kind {
            Todo(message) => format!("{}", message),

            DidntRun => format!("compiler phase didn't run"),
            NotImplemented(message) => format!("{}", message),

            UnrecognizedCharacter { idx } => format!("unrecognized character"),
            UnrecognizedToken { idx } => format!("unrecognized token"),
        }
    }
}
