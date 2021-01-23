use super::types::*;
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct TestFS {
    pub files: Vec<Vec<u8>>,
    pub names: BTreeMap<String, usize>,
    pub size: usize,
}

impl TestFS {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            names: BTreeMap::new(),
            size: 0,
        }
    }

    pub fn open(&self, name: &[u8]) -> Result<u32, EcallError> {
        let name = core::str::from_utf8(name).map_err(|e| EcallError::NameNotUTF8)?;

        let idx = *self.names.get(name).ok_or(EcallError::DoesntExist)?;
        return Ok(idx as u32);
    }

    pub fn open_create(&mut self, name: &[u8]) -> Result<u32, EcallError> {
        let name = core::str::from_utf8(name).map_err(|e| EcallError::NameNotUTF8)?;

        let idx = self.names.get(name).map(|a| *a).unwrap_or_else(|| {
            let idx = self.files.len();
            self.files.push(Vec::new());
            self.names.insert(name.to_string(), idx);
            idx
        });

        if self.files.len() > 4000 {
            return Err(EcallError::TooManyFiles);
        }

        return Ok(idx as u32);
    }

    pub fn open_create_clear(&mut self, name: &[u8]) -> Result<u32, EcallError> {
        let name = core::str::from_utf8(name).map_err(|e| EcallError::NameNotUTF8)?;

        let idx = self.names.get(name).map(|a| *a).unwrap_or_else(|| {
            let idx = self.files.len();
            self.files.push(Vec::new());
            self.names.insert(name.to_string(), idx);
            idx
        });

        if self.files.len() > 4000 {
            return Err(EcallError::TooManyFiles);
        }

        self.size -= self.files[idx].len();
        self.files[idx].clear();

        return Ok(idx as u32);
    }

    pub fn len(&self, fd: u32) -> Result<u32, EcallError> {
        let fd = fd as usize;

        let file = self.files.get(fd).ok_or(EcallError::DoesntExist)?;
        return Ok(file.len() as u32);
    }

    pub fn read_file_range(&self, fd: u32, begin: u32, len: u32) -> Result<&[u8], EcallError> {
        let (fd, begin, len) = (fd as usize, begin as usize, len as usize);

        let file = self.files.get(fd).ok_or(EcallError::DoesntExist)?;
        let from_buffer = file
            .get(begin..std::cmp::min(begin + len, file.len()))
            .ok_or(EcallError::OutOfRange)?;
        return Ok(from_buffer);
    }

    pub fn write_to_file_range(
        &mut self,
        fd: u32,
        begin: u32,
        buffer: &[u8],
    ) -> Result<u32, EcallError> {
        let (fd, begin) = (fd as usize, begin as usize);

        let file = self.files.get_mut(fd).ok_or(EcallError::DoesntExist)?;
        let to_buffer = file.get_mut(begin..).ok_or(EcallError::OutOfRange)?;
        let copy_len = std::cmp::min(to_buffer.len(), buffer.len());
        let (copy, extend) = buffer.split_at(copy_len);
        to_buffer[..copy_len].copy_from_slice(copy);

        self.size += extend.len();
        if self.size > 1024 * 1024 * 64 {
            return Err(EcallError::FilesTooLarge);
        }

        file.extend_from_slice(extend);
        return Ok(file.len() as u32);
    }

    pub fn append_to_file(&mut self, fd: u32, buffer: &[u8]) -> Result<u32, EcallError> {
        let fd = fd as usize;
        let file = self.files.get_mut(fd).ok_or(EcallError::DoesntExist)?;

        self.size += buffer.len();
        if self.size > 1024 * 1024 * 64 {
            return Err(EcallError::FilesTooLarge);
        }

        file.extend_from_slice(buffer);
        return Ok(file.len() as u32);
    }
}
