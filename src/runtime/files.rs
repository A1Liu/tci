use super::types::*;
use std::collections::BTreeMap;

pub struct FileSystem {
    pub files: Vec<Vec<u8>>,
    pub names: BTreeMap<String, usize>,
    pub size: usize,
}

impl FileSystem {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            names: BTreeMap::new(),
            size: 0,
        }
    }

    pub fn open(&self, name: &[u8]) -> Result<u32, FileError> {
        let name = core::str::from_utf8(name).map_err(|e| FileError::NameNotUTF8)?;

        let idx = 3 + *self.names.get(name).ok_or(FileError::DoesntExist)?;
        return Ok(idx as u32);
    }

    pub fn open_create(&mut self, name: &[u8]) -> Result<u32, FileError> {
        let name = core::str::from_utf8(name).map_err(|e| FileError::NameNotUTF8)?;

        let idx = self.names.get(name).map(|a| 3 + *a).unwrap_or_else(|| {
            let len = self.files.len();
            self.files.push(Vec::new());
            len + 3
        });

        if self.files.len() > 4000 {
            return Err(FileError::TooManyFiles);
        }

        return Ok(idx as u32);
    }

    pub fn open_create_clear(&mut self, name: &[u8]) -> Result<u32, FileError> {
        let name = core::str::from_utf8(name).map_err(|e| FileError::NameNotUTF8)?;

        let idx = self.names.get(name).map(|a| 3 + *a).unwrap_or_else(|| {
            let len = self.files.len();
            self.files.push(Vec::new());
            len + 3
        });

        if self.files.len() > 4000 {
            return Err(FileError::TooManyFiles);
        }

        self.size -= self.files[idx].len();
        self.files[idx].clear();

        return Ok(idx as u32);
    }

    pub fn len(&self, fd: u32) -> Result<u32, FileError> {
        let fd = fd as usize;
        if fd < 3 {
            return Err(FileError::DoesntExist);
        }

        let file = self.files.get(fd - 3).ok_or(FileError::DoesntExist)?;
        return Ok(file.len() as u32);
    }

    pub fn read_file_range(&self, fd: u32, begin: u32, len: u32) -> Result<&[u8], FileError> {
        let (fd, begin, len) = (fd as usize, begin as usize, len as usize);
        if fd < 3 {
            return Err(FileError::DoesntExist);
        }

        let file = self.files.get(fd - 3).ok_or(FileError::DoesntExist)?;
        let from_buffer = file
            .get(begin..(begin + len))
            .ok_or(FileError::OutOfRange)?;
        return Ok(from_buffer);
    }

    pub fn write_to_file_range(
        &mut self,
        fd: u32,
        begin: u32,
        buffer: &[u8],
    ) -> Result<u32, FileError> {
        let (fd, begin) = (fd as usize, begin as usize);
        if fd < 3 {
            return Err(FileError::DoesntExist);
        }

        let file = self.files.get_mut(fd - 3).ok_or(FileError::DoesntExist)?;
        let to_buffer = file.get_mut(begin..).ok_or(FileError::OutOfRange)?;
        let copy_len = std::cmp::min(to_buffer.len(), buffer.len());
        let (copy, extend) = buffer.split_at(copy_len);
        to_buffer[..copy_len].copy_from_slice(copy);

        self.size += extend.len();
        if self.size > 1024 * 1024 * 64 {
            return Err(FileError::FilesToLarge);
        }

        file.extend_from_slice(extend);
        return Ok(file.len() as u32);
    }

    pub fn append_to_file(&mut self, fd: u32, buffer: &[u8]) -> Result<u32, FileError> {
        let fd = fd as usize;
        let file = self.files.get_mut(fd).ok_or(FileError::DoesntExist)?;

        self.size += buffer.len();
        if self.size > 1024 * 1024 * 64 {
            return Err(FileError::FilesToLarge);
        }

        file.extend_from_slice(buffer);
        return Ok(file.len() as u32);
    }
}
