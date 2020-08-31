use crate::buckets::*;
use crate::util::*;
use codespan_reporting::files::{line_starts, Files};
use core::{mem, ops, str};
use std::collections::HashMap;
use std::fs::read_to_string;

#[derive(Debug, Clone)]
pub struct File<'a> {
    /// The source code of the file.
    pub _source: &'a str,
    /// The starting byte indices in the source code.
    pub _line_starts: &'a [usize],
}

pub struct FileDb<'a> {
    pub buckets: BucketListRef<'a>,
    pub _size: usize,
    pub files: HashMap<u32, File<'a>>,
    pub translate: HashMap<&'a str, u32>,
    pub names: Vec<CodeLoc>,
}

pub const MAIN_SYMBOL: u32 = 0;
pub const VA_LIST_SYMBOL: u32 = 1;
pub const PRINTF_SYMBOL: u32 = 2;
pub const EXIT_SYMBOL: u32 = 3;

pub const STATIC_FILE: u32 = !0;
pub const NOT_A_FILE: u32 = !0;

pub const STATICS: &str = "main,va_list,printf,exit";
//                        "012345678901234567890123"

impl<'a> FileDb<'a> {
    /// Create a new files database.
    pub fn new(files: &[&str]) -> Self {
        let mut string = String::new();
        macro_rules! add_static_name {
            ($arg:expr) => {{
                let begin = string.len();
                string.push_str($arg);
                let end = string.len();
                begin..end
            }};
        }

        let main = add_static_name!("main");
        let va_list = add_static_name!("va_list");
        let printf = add_static_name!("printf");
        let exit = add_static_name!("exit");

        let mut file_data = Vec::new();
        for file_name in files {
            file_data.push((add_static_name!(file_name), file_name));
        }

        let line_starts: Vec<usize> = line_starts(&string).collect();
        let _size = string.len() + line_starts.len() * 8 + mem::size_of::<File>();
        let buckets = BucketList::with_capacity(16 * 1024 * 1024);
        let file = File {
            _source: buckets.add_str(&string),
            _line_starts: buckets.add_array(line_starts),
        };

        let mut files = HashMap::new();
        files.insert(STATIC_FILE, file);

        let mut new_self = Self {
            buckets,
            _size,
            files,
            translate: HashMap::new(),
            names: Vec::new(),
        };

        assert_eq!(MAIN_SYMBOL, new_self.translate_add(main, STATIC_FILE));

        assert_eq!(VA_LIST_SYMBOL, new_self.translate_add(va_list, STATIC_FILE));

        assert_eq!(PRINTF_SYMBOL, new_self.translate_add(printf, STATIC_FILE));
        assert_eq!(EXIT_SYMBOL, new_self.translate_add(exit, STATIC_FILE));

        for (name_range, name) in file_data {
            let sym = new_self.translate_add(name_range, STATIC_FILE);
            let input = read_to_string(&name).unwrap();
            new_self.add(sym, &input);
        }

        new_self
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, &'a str)> {
        let iter = self.files.iter();
        let sources: Vec<(u32, &'a str)> = iter
            .filter(|(id, file)| **id != STATIC_FILE)
            .map(|(id, file)| (*id, file._source))
            .collect();

        return sources.into_iter();
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again.
    pub fn add(&mut self, file_name: u32, source: &str) -> u32 {
        let file_id = self.files.len() as u32;
        let file = File {
            _source: self.buckets.add_str(source),
            _line_starts: self.buckets.add_array(line_starts(source).collect()),
        };

        let size = file._source.len() + file._line_starts.len() * 8 + mem::size_of::<File>();
        self._size += size;
        self.files.insert(file_name, file);
        file_id
    }

    #[inline]
    pub fn translate_add(&mut self, range: ops::Range<usize>, file: u32) -> u32 {
        let cloc = l(range.start as u32, range.end as u32, file); // TODO check for overflow
        return self.translate_add_cloc(cloc);
    }

    #[inline]
    pub fn translate_add_cloc(&mut self, cloc: CodeLoc) -> u32 {
        let range: ops::Range<usize> = cloc.into();
        let text = self.files.get(&cloc.file).unwrap()._source;
        let text = unsafe { str::from_utf8_unchecked(&text.as_bytes()[range]) };

        if let Some(id) = self.translate.get(text) {
            return *id;
        } else {
            let idx = self.names.len() as u32;
            self.names.push(cloc);
            self.translate.insert(text, idx);
            self._size += mem::size_of::<ShortSlice<u8>>();
            return idx;
        }
    }

    pub fn size(&self) -> usize {
        return self._size + mem::size_of::<File>() + mem::size_of::<ShortSlice<u8>>();
    }
}

impl<'a> Files<'a> for FileDb<'a> {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        let cloc = self.names.get(file_id as usize)?;
        let range: ops::Range<usize> = (*cloc).into();
        let bytes = &self.files.get(&cloc.file)?._source.as_bytes()[range];
        Some(unsafe { str::from_utf8_unchecked(bytes) })
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        println!("getting: {}, from: {:?}", file_id, self.files);
        Some(self.files.get(&file_id)?._source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let line_starts = self.files.get(&file_id)?._line_starts;
        match line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        use std::cmp::Ordering;

        let file = self.files.get(&file_id)?;
        let line_start = |idx: usize| match idx.cmp(&file._line_starts.len()) {
            Ordering::Less => file._line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(file._source.len()),
            Ordering::Greater => None,
        };
        let start = line_start(line_index)?;
        let next_start = line_start(line_index + 1)?;
        Some(start..next_start)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FileDbRef<'a> {
    pub files: &'a [File<'a>],
    pub symbols: &'a [ShortSlice<'a, u8>],
}

impl<'a> FileDbRef<'a> {
    /// Create a new files database.
    pub fn new_from_frame(frame: &mut Frame<'a>, db: &FileDb) -> Self {
        let mut file_sources = Vec::new();
        let mut file_source_indices = HashMap::new();
        let mut add_source = |id: u32, file: &File| {
            let idx = file_sources.len();
            file_sources.push(File {
                _source: frame.add_str(file._source),
                _line_starts: frame.add_slice(&file._line_starts),
            });
            file_source_indices.insert(id, idx);
        };

        for (id, file) in db.files.iter() {
            add_source(*id, file);
        }

        let mut symbols = Vec::new();
        for (id, symbol) in db.names.iter().enumerate() {
            let id = id as u32;
            let range: ops::Range<usize> = (*symbol).into();
            let bytes = &file_sources[file_source_indices[&symbol.file]]
                ._source
                .as_bytes()[range];

            let file_id = if let Some(file_id) = file_source_indices.get(&id) {
                *file_id as u32
            } else {
                NOT_A_FILE
            };

            symbols.push(ShortSlice::new(bytes, file_id));
        }

        let files = frame.add_array(file_sources);
        let symbols = frame.add_array(symbols);
        Self { files, symbols }
    }

    pub fn get_symbol(&self, symbol: u32) -> Option<&'a str> {
        return Some(unsafe { str::from_utf8_unchecked(&self.symbols.get(symbol as usize)?) });
    }
}

impl<'a> Files<'a> for FileDbRef<'a> {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        self.get_symbol(file_id)
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        let file_idx = self.symbols.get(file_id as usize)?;

        Some(self.files.get(file_idx.meta as usize)?._source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let file_idx = self.symbols.get(file_id as usize)?;
        let line_starts = self.files.get(file_idx.meta as usize)?._line_starts;
        match line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        use std::cmp::Ordering;

        let file_idx = self.symbols.get(file_id as usize)?;
        let file = self.files.get(file_idx.meta as usize)?;
        let line_start = |idx: usize| match idx.cmp(&file._line_starts.len()) {
            Ordering::Less => file._line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(file._source.len()),
            Ordering::Greater => None,
        };
        let start = line_start(line_index)?;
        let next_start = line_start(line_index + 1)?;
        Some(start..next_start)
    }
}
