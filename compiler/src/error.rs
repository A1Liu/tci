use codespan_reporting::diagnostic::{Diagnostic, Label};

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

pub struct FileRange {
    pub file: u32,
    pub start: usize,
}

impl TranslationUnitDebugInfo {
    pub fn diagnostic(&self, err: &Error) -> Diagnostic<u32> {
        return Diagnostic::error()
            .with_message(err.message())
            .with_code(err.code())
            .with_labels(err.labels(self));
    }

    pub fn token_range(&self, start: u32) -> FileRange {
        // TODO: binary search
        let mut previous = self.file_starts[0];
        for file_start in &self.file_starts {
            if file_start.index > start {
                break;
            }

            previous = *file_start;
        }

        return FileRange {
            file: previous.file,
            start: previous.file_index + (start as usize - previous.index as usize),
        };
    }
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ErrorKind {
    Todo(String),

    DidntRun,
    NotImplemented(String),

    InvalidCharacterSequence { seq: String, index: u32 },
}

macro_rules! error {
    ($e:ident ( $str:literal )) => {
        Error::new(crate::error::ErrorKind::$e ( $str.to_string() ))
    };
    ($e:ident) => {
        Error::new(crate::error::ErrorKind::$e)
    };
    ($e:ident $t:tt) => {
        Error::new(crate::error::ErrorKind::$e $t)
    };
}

macro_rules! throw {
    ($($e:tt)*) => {
        { return Err(error!($($e)*)); }
    };
}

impl ErrorKind {
    pub fn message(&self) -> String {
        use ErrorKind::*;

        match self {
            Todo(message) => format!("{}", message),

            DidntRun => format!("compiler phase didn't run"),
            NotImplemented(message) => format!("{}", message),

            InvalidCharacterSequence { seq, index } => format!("'{}' isn't valid", seq),
        }
    }

    pub fn code(&self) -> &'static str {
        use ErrorKind::*;

        match self {
            Todo(message) => "001",

            DidntRun => "000",
            NotImplemented(message) => "002",

            InvalidCharacterSequence { seq, index } => "100",
        }
    }

    pub fn labels(&self, tu: &TranslationUnitDebugInfo) -> Vec<Label<u32>> {
        use ErrorKind::*;

        let mut labels = Vec::new();

        match self {
            Todo(message) => {}
            DidntRun => {}
            NotImplemented(message) => {}

            InvalidCharacterSequence { seq, index } => {
                let range = tu.token_range(*index);
                labels.push(Label::primary(
                    range.file,
                    range.start..(range.start + seq.len()),
                ));
            }
        }

        return labels;
    }
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
        return self.kind.message();
    }

    pub fn code(&self) -> &'static str {
        return self.kind.code();
    }

    pub fn labels(&self, tu: &TranslationUnitDebugInfo) -> Vec<Label<u32>> {
        return self.kind.labels(tu);
    }
}
