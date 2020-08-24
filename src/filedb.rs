use crate::buckets::*;
use codespan_reporting::files::{line_starts, Files};
use core::mem;

#[derive(Debug, Clone)]
pub struct File<'a> {
    /// The name of the file.
    pub name: &'a str,
    /// The source code of the file.
    pub source: &'a str,
    /// The starting byte indices in the source code.
    pub line_starts: &'a [usize],
}

impl<'a> File<'a> {
    /// Create a new source file.
    pub fn new(buckets: BucketListRef<'a>, name: &str, source: &str) -> Self {
        Self {
            name: buckets.add_str(name),
            line_starts: buckets.add_array(line_starts(source.as_ref()).collect()),
            source: buckets.add_str(source),
        }
    }

    pub fn new_frame(frame: &mut Frame<'a>, name: &str, source: &str) -> Self {
        Self {
            name: frame.add_str(name),
            line_starts: frame.add_array(line_starts(source.as_ref()).collect()),
            source: frame.add_str(source),
        }
    }

    /// Return the name of the file.
    pub fn name(&self) -> &'a str {
        self.name
    }

    /// Return the source of the file.
    pub fn source(&self) -> &'a str {
        self.source
    }

    fn line_start(&self, line_index: usize) -> Option<usize> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => self.line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(self.source.len()),
            Ordering::Greater => None,
        }
    }
}

impl<'a> Files<'a> for File<'a> {
    type FileId = ();
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, (): ()) -> Option<&'a str> {
        Some(self.name)
    }

    fn source(&self, (): ()) -> Option<&'a str> {
        Some(self.source)
    }

    fn line_index(&self, (): (), byte_index: usize) -> Option<usize> {
        match self.line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_range(&self, (): (), line_index: usize) -> Option<core::ops::Range<usize>> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;

        Some(line_start..next_line_start)
    }
}

#[derive(Debug, Clone)]
pub struct FileDb<'a> {
    pub size: usize,
    pub files: Vec<File<'a>>,
}

impl<'a> FileDb<'a> {
    /// Create a new files database.
    pub fn new() -> Self {
        Self {
            size: 0,
            files: Vec::new(),
        }
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again.
    pub fn add(&mut self, buckets: BucketListRef<'a>, name: &str, source: &str) -> u32 {
        let file_id = self.files.len() as u32;
        self.size += name.len() + source.len() + mem::size_of::<File>();
        self.files.push(File::new(buckets, name, source));
        file_id
    }

    /// Iterate through the files contained in this instance
    pub fn iter(&self) -> core::ops::Range<u32> {
        return 0..(self.files.len() as u32);
    }

    /// Get the file corresponding to the given id.
    pub fn get(&self, file_id: u32) -> Option<&File<'a>> {
        self.files.get(file_id as usize)
    }
}

impl<'a> Files<'a> for FileDb<'a> {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.name())
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.source().as_ref())
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        self.files.get(file_id as usize)?.line_index((), byte_index)
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        self.files.get(file_id as usize)?.line_range((), line_index)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FileDbRef<'a> {
    pub files: &'a [File<'a>],
}

impl<'a> FileDbRef<'a> {
    /// Create a new files database.
    pub fn new<'b>(buckets: BucketListRef<'a>, files: &FileDb<'b>) -> Self {
        let files = files.files.iter();
        let files = files.map(|file| File::new(buckets, file.name, file.source));
        let files = buckets.add_array(files.collect());
        Self { files }
    }

    /// Create a new files database.
    pub fn new_frame<'b>(frame: &mut Frame<'a>, files: &FileDb<'b>) -> Self {
        let files = files.files.iter();
        let files = files.map(|file| File::new_frame(frame, file.name, file.source));
        let files = files.collect();
        let files = frame.add_array(files);
        Self { files }
    }

    pub fn clone_frame<'b>(frame: &mut Frame<'b>, files: Self) -> FileDbRef<'b> {
        let files = files.files.iter();
        let files = files.map(|file| File::new_frame(frame, file.name, file.source));
        let files = files.collect();
        let files = frame.add_array(files);
        FileDbRef { files }
    }

    /// Iterate through the files contained in this instance
    pub fn iter(&self) -> core::ops::Range<u32> {
        return 0..(self.files.len() as u32);
    }

    /// Get the file corresponding to the given id.
    pub fn get(&self, file_id: u32) -> Option<&File<'a>> {
        self.files.get(file_id as usize)
    }
}

impl<'a> Files<'a> for FileDbRef<'a> {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.name())
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.source().as_ref())
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        self.files.get(file_id as usize)?.line_index((), byte_index)
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        self.files.get(file_id as usize)?.line_range((), line_index)
    }
}
