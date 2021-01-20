use crate::util::*;
use std::collections::BTreeMap;

pub struct FileSystem {
    pub files: Vec<Vec<u8>>,
    pub names: BTreeMap<String, n32>,
}
