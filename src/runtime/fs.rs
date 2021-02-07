use super::types::*;
use crate::util::*;

// this system is complicated because it has to be.
// TODO add file descriptor garbage collection in form of freelist
#[derive(Debug)]
pub struct FileSystem {
    pub files: TaggedMultiVec<bool, u8>, // maps external file id -> freelist index [data]
    pub names: HashMap<String, u32>,     // maps external file path -> external file id
    pub size: usize,
}

impl FileSystem {
    pub fn new() -> Self {
        Self {
            files: TaggedMultiVec::new(),
            names: HashMap::new(),
            size: 0,
        }
    }

    pub fn open(&self, name: &str) -> Result<u32, EcallError> {
        let idx = *self.names.get(name).ok_or(EcallError::DoesntExist)?;
        return Ok(idx as u32);
    }

    pub fn remove(&mut self, name: &str) -> Result<(), EcallError> {
        let fd = self.names.remove(name).ok_or(EcallError::DoesntExist)?;
        let mut file = self.files.get_mut(fd as usize).unwrap();
        file.clear_pod();
        file.shrink_to_fit();
        *file.tag_mut() = false;
        return Ok(());
    }

    pub fn open_create(&mut self, name: &str) -> Result<u32, EcallError> {
        let idx = self.names.get(name).map(|a| *a).unwrap_or_else(|| {
            let idx = self.files.len();
            self.files.push(true, Vec::new());
            self.names.insert(name.to_string(), idx as u32);
            idx as u32
        });

        if self.files.len() > 4000 {
            return Err(EcallError::TooManyFiles);
        }

        return Ok(idx);
    }

    pub fn open_create_clear(&mut self, name: &str) -> Result<u32, EcallError> {
        let idx_o = self.names.get(name).map(|a| *a);
        let idx = idx_o.unwrap_or_else(|| {
            let idx = self.files.len();
            self.files.push(true, Vec::new());
            self.names.insert(name.to_string(), idx as u32);
            idx as u32
        });

        if self.files.len() > 4000 {
            return Err(EcallError::TooManyFiles);
        }

        self.files.get_mut(idx as usize).unwrap().clear();
        return Ok(idx);
    }

    pub fn read_file_range(&self, fd: u32, begin: u32, len: u32) -> Result<&[u8], EcallError> {
        let (fd, begin, len) = (fd as usize, begin as usize, len as usize);

        let file = self.files.get(fd).ok_or(EcallError::DoesntExist)?;
        if !*file.tag {
            return Err(EcallError::DoesntExist);
        }
        let file = file.data;

        let from_buffer_o = file.get(begin..std::cmp::min(begin + len, file.len()));
        let from_buffer = from_buffer_o.ok_or(EcallError::OutOfRange)?;
        return Ok(from_buffer);
    }

    pub fn write_to_file_range(
        &mut self,
        fd: u32,
        begin: u32,
        buffer: &[u8],
    ) -> Result<u32, EcallError> {
        let (fd, begin) = (fd as usize, begin as usize);

        let mut file = self.files.get_mut(fd).ok_or(EcallError::DoesntExist)?;
        if !*file.tag() {
            return Err(EcallError::DoesntExist);
        }

        let to_buffer = file.get_mut(begin..).ok_or(EcallError::OutOfRange)?;
        let copy_len = std::cmp::min(to_buffer.len(), buffer.len());
        let (copy, extend) = buffer.split_at(copy_len);

        if self.size + extend.len() > 1024 * 1024 * 64 {
            return Err(EcallError::FilesTooLarge);
        }

        self.size += extend.len();
        to_buffer[..copy_len].copy_from_slice(copy);
        file.extend_from_slice(extend);

        return Ok(file.len() as u32);
    }

    pub fn append_to_file(&mut self, fd: u32, buffer: &[u8]) -> Result<u32, EcallError> {
        if self.size + buffer.len() > 1024 * 1024 * 64 {
            return Err(EcallError::FilesTooLarge);
        }

        self.size += buffer.len();
        #[rustfmt::skip]
        let mut file = self.files.get_mut(fd as usize).ok_or(EcallError::DoesntExist)?;
        if !*file.tag() {
            return Err(EcallError::DoesntExist);
        }

        file.extend_from_slice(buffer);
        return Ok(file.len() as u32);
    }
}
