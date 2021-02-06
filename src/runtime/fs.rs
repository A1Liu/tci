use super::types::*;
use crate::util::*;

const FILE_INIT: [u8; 512] = [0; 512];

// this system is complicated because it has to be.
#[derive(Debug)]
pub struct FileSystem {
    pub data: TaggedMultiVec<usize, u8>, // maps internal file id -> freelist index [data]
    pub files: Vec<u32>,                 // maps external file id -> internal file id
    pub names: HashMap<String, u32>,     // public information
    pub freelist_head: usize,
    pub size: usize,
}

impl FileSystem {
    pub fn new() -> Self {
        Self {
            data: TaggedMultiVec::new(),
            files: Vec::new(),
            names: HashMap::new(),
            freelist_head: !0,
            size: 0,
        }
    }

    pub fn open(&self, name: &str) -> Result<u32, EcallError> {
        let idx = *self.names.get(name).ok_or(EcallError::DoesntExist)?;
        return Ok(idx as u32);
    }

    pub fn open_create(&mut self, name: &str) -> Result<u32, EcallError> {
        let idx = self.names.get(name).map(|a| *a).unwrap_or_else(|| {
            let internal_idx = self.data.len();
            self.data.push(!0, Vec::new());
            let idx = self.files.len();
            self.files.push(internal_idx as u32);
            self.names.insert(name.to_string(), idx as u32);
            idx as u32
        });

        if self.files.len() > 4000 {
            return Err(EcallError::TooManyFiles);
        }

        return Ok(idx);
    }

    pub fn open_create_clear(&mut self, name: &str) -> Result<u32, EcallError> {
        let idx = self.names.get(name).map(|a| *a).unwrap_or_else(|| {
            let internal_idx = self.data.len();
            self.data.push(!0, Vec::new());
            let idx = self.files.len();
            self.files.push(internal_idx as u32);
            self.names.insert(name.to_string(), idx as u32);
            idx as u32
        });

        if self.files.len() > 4000 {
            return Err(EcallError::TooManyFiles);
        }

        self.data.get_mut(idx as usize).unwrap().clear();
        return Ok(idx);
    }

    pub fn read_file_range(&self, fd: u32, begin: u32, len: u32) -> Result<&[u8], EcallError> {
        let (fd, begin, len) = (fd as usize, begin as usize, len as usize);

        let file = *self.files.get(fd).ok_or(EcallError::DoesntExist)?;
        let data = self.data.get(file as usize).unwrap();
        let from_buffer_o = data.data.get(begin..std::cmp::min(begin + len, data.len()));
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

        let internal = *self.files.get_mut(fd).ok_or(EcallError::DoesntExist)?;
        let mut file = self.data.get_mut(internal as usize).unwrap();
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
        let internal = *self.files.get_mut(fd as usize).ok_or(EcallError::DoesntExist)?;
        let mut file = self.data.get_mut(internal as usize).unwrap();

        file.extend_from_slice(buffer);
        return Ok(file.len() as u32);
    }
}
