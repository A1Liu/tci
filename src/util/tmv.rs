use super::vec::*;
use core::mem::MaybeUninit;
use core::{fmt, mem, ops, ptr};

#[derive(Debug, Clone, Copy)]
pub struct TMVec<'a, T, E> {
    pub tag: &'a T,
    pub data: &'a [E],
}

pub struct TMVecMut<'a, T, E> {
    tmv: &'a mut TaggedMultiVec<T, E>,
    idx: usize,
}

pub struct TMVBlock<T> {
    tag: T,
    elem_idx: usize,
    elem_len: usize,
    elem_capa: usize,
}

pub struct TaggedMultiVec<T, E> {
    elements: Vec<MaybeUninit<E>>,
    tags: Vec<TMVBlock<T>>,
    size: usize,
}

pub struct TMVIter<'a, T, E> {
    tmv: &'a TaggedMultiVec<T, E>,
    idx: usize,
}

impl<T, E> TaggedMultiVec<T, E> {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            tags: Vec::new(),
            size: 0,
        }
    }

    pub fn len(&self) -> usize {
        return self.tags.len();
    }

    pub fn compact_gc(&mut self) {
        struct TMVBlock {
            tag_idx: usize,
            elem_idx: usize,
            elem_len: usize,
            elem_capa: usize,
        }

        let mut blocks = Vec::new();
        for (tag_idx, block) in self.tags.iter().enumerate() {
            blocks.push(TMVBlock {
                tag_idx,
                elem_idx: block.elem_idx,
                elem_len: block.elem_len,
                elem_capa: block.elem_capa,
            });
        }

        // need to traverse the blocks in order
        blocks.sort_by_key(|b| b.elem_idx);

        let mut write = 0;
        for block in &mut blocks {
            let to_space = unsafe { mem::transmute(&mut self.elements[write]) };
            unsafe { ptr::copy(&self.elements[block.elem_idx], to_space, block.elem_len) };

            self.tags[block.tag_idx].elem_idx = write;
            write += block.elem_capa;
        }

        debug_assert_eq!(write, self.size);

        unsafe { self.elements.set_len(write) };
    }

    pub fn reserve_gc(&mut self, len: usize) {
        if self.size + len <= self.elements.capacity() {
            self.compact_gc();
            return;
        }

        let tags = &mut self.tags;
        vec_reserve_gc(&mut self.elements, len, |old, new| unsafe {
            let mut write = 0;

            for block in tags {
                if block.elem_len != 0 {
                    let to_space = mem::transmute(&mut new[write]);
                    ptr::copy_nonoverlapping(&old[block.elem_idx], to_space, block.elem_len);
                }

                block.elem_idx = write;
                write += block.elem_capa;
            }

            debug_assert!(write <= new.len());
            write
        });
    }

    pub fn reserve_element_gc(&mut self, idx: usize, additional: usize) {
        let (elem_len, elem_capa) = (self.tags[idx].elem_len, self.tags[idx].elem_capa);

        if elem_len + additional <= elem_capa {
            return;
        }

        let new_capa = std::cmp::max(elem_len * 3 / 2 + additional, elem_capa * 3 / 2);
        let new_capa = std::cmp::max(new_capa, 8);
        if self.elements.len() + new_capa < self.elements.capacity() {
            let (elem_idx, new_idx) = (self.tags[idx].elem_idx, self.elements.len());
            for read in elem_idx..(elem_idx + elem_len) {
                let elem = unsafe { ptr::read(&self.elements[read]) };
                self.elements.push(elem);
            }

            for _ in elem_len..new_capa {
                self.elements.push(MaybeUninit::uninit());
            }

            self.tags[idx].elem_idx = new_idx;
            self.tags[idx].elem_capa = new_capa;
            self.size += new_capa - elem_capa;
            return;
        }

        if self.size + new_capa <= self.elements.capacity() {
            self.compact_gc();
            let (elem_idx, new_idx) = (self.tags[idx].elem_idx, self.elements.len());
            for read in elem_idx..(elem_idx + elem_len) {
                let elem = unsafe { ptr::read(&self.elements[read]) };
                self.elements.push(elem);
            }

            for _ in elem_len..new_capa {
                self.elements.push(MaybeUninit::uninit());
            }

            self.tags[idx].elem_idx = new_idx;
            self.tags[idx].elem_capa = new_capa;
            self.size += new_capa - elem_capa;
            return;
        }

        let (size, tags) = (&mut self.size, &mut self.tags);
        vec_reserve_gc(&mut self.elements, new_capa, |old, new| unsafe {
            let mut write = 0;

            for (block_idx, block) in tags.iter_mut().enumerate() {
                if block.elem_len != 0 {
                    let to_space = mem::transmute(&mut new[write]);
                    ptr::copy_nonoverlapping(&old[block.elem_idx], to_space, block.elem_len);
                }

                if block_idx == idx {
                    block.elem_capa = new_capa;
                }

                block.elem_idx = write;
                write += block.elem_capa;
            }

            debug_assert!(write <= new.len());
            *size = write;
            write
        });
    }

    pub fn pop(&mut self) -> Option<T> {
        let block = self.tags.pop()?;

        for idx in block.elem_idx..(block.elem_idx + block.elem_len) {
            unsafe { ptr::drop_in_place(self.elements[idx].as_mut_ptr()) };
        }

        self.size -= block.elem_capa;
        return Some(block.tag);
    }

    pub fn push(&mut self, tag: T, data: Vec<E>) {
        let (elem_len, elem_capa) = (data.len(), data.capacity());
        self.reserve_gc(elem_capa);

        let (elem_idx, capa) = (self.elements.len(), self.elements.capacity());
        debug_assert!(elem_idx + elem_capa <= capa);

        for e in data {
            self.elements.push(MaybeUninit::new(e));
        }

        for _ in elem_len..elem_capa {
            self.elements.push(MaybeUninit::uninit());
        }

        debug_assert_eq!(capa, self.elements.capacity());
        self.tags.push(TMVBlock {
            tag,
            elem_idx,
            elem_len,
            elem_capa,
        });

        self.size += elem_capa;
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<TMVecMut<T, E>> {
        if idx >= self.tags.len() {
            return None;
        }

        return Some(TMVecMut { tmv: self, idx });
    }

    pub fn get(&self, idx: usize) -> Option<TMVec<T, E>> {
        let block = self.tags.get(idx)?;
        let tag = &block.tag;
        let elements = &self.elements[block.elem_idx..(block.elem_idx + block.elem_len)];
        let data = unsafe { mem::transmute::<&[MaybeUninit<E>], &[E]>(elements) };

        return Some(TMVec { tag, data });
    }
}

impl<T, E> Drop for TaggedMultiVec<T, E> {
    fn drop(&mut self) {
        while let Some(t) = self.pop() {}
    }
}

impl<'a, T, E> TMVecMut<'a, T, E> {
    pub fn tag(&self) -> &T {
        return &self.tmv.tags[self.idx].tag;
    }

    pub fn tag_mut(&mut self) -> &mut T {
        return &mut self.tmv.tags[self.idx].tag;
    }

    pub fn len(&self) -> usize {
        return self.tmv.tags[self.idx].elem_len;
    }

    pub fn capacity(&self) -> usize {
        return self.tmv.tags[self.idx].elem_capa;
    }

    pub fn push(&mut self, value: E) {
        self.tmv.reserve_element_gc(self.idx, 1);
        let block = &mut self.tmv.tags[self.idx];

        debug_assert!(block.elem_len < block.elem_capa);
        debug_assert!(
            block.elem_capa + block.elem_idx <= self.tmv.elements.len(),
            "{} + {} > {}",
            block.elem_capa,
            block.elem_idx,
            self.tmv.elements.len()
        );

        self.tmv.elements[block.elem_idx + block.elem_len] = MaybeUninit::new(value);
        block.elem_len += 1;
    }

    pub fn clear(&mut self) {
        while let Some(e) = self.pop() {}
    }

    pub fn pop(&mut self) -> Option<E> {
        let block = &mut self.tmv.tags[self.idx];
        if block.elem_len == 0 {
            return None;
        }

        let elem =
            unsafe { ptr::read(self.tmv.elements[block.elem_idx + block.elem_len].as_mut_ptr()) };
        block.elem_len -= 1;
        return Some(elem);
    }

    pub fn as_slice(&self) -> &[E] {
        let block = &self.tmv.tags[self.idx];
        let elements = &self.tmv.elements[block.elem_idx..(block.elem_idx + block.elem_len)];
        let data = unsafe { mem::transmute::<&[MaybeUninit<E>], &[E]>(elements) };
        return data;
    }

    pub fn as_slice_mut(&mut self) -> &mut [E] {
        let block = &self.tmv.tags[self.idx];
        let elements = &mut self.tmv.elements[block.elem_idx..(block.elem_idx + block.elem_len)];
        let data = unsafe { mem::transmute::<&mut [MaybeUninit<E>], &mut [E]>(elements) };
        return data;
    }
}

impl<'a, T, E> TMVecMut<'a, T, E>
where
    E: Clone,
{
    pub fn extend_from_slice(&mut self, slice: &[E]) {
        let slice_len = slice.len();
        self.tmv.reserve_element_gc(self.idx, slice_len);
        let block = &mut self.tmv.tags[self.idx];

        debug_assert!(block.elem_len + slice_len <= block.elem_capa);
        debug_assert!(block.elem_capa + block.elem_idx <= self.tmv.elements.len());

        for elem in slice {
            self.tmv.elements[block.elem_idx + block.elem_len] = MaybeUninit::new(elem.clone());
            block.elem_len += 1;
        }
    }
}

impl<'a, T, E> ops::Deref for TMVecMut<'a, T, E> {
    type Target = [E];

    fn deref(&self) -> &Self::Target {
        return self.as_slice();
    }
}

impl<'a, T, E> ops::DerefMut for TMVecMut<'a, T, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        return self.as_slice_mut();
    }
}

impl<'a, T, E> ops::Deref for TMVec<'a, T, E> {
    type Target = [E];

    fn deref(&self) -> &Self::Target {
        return self.data;
    }
}

impl<T, E> fmt::Debug for TaggedMultiVec<T, E>
where
    T: fmt::Debug,
    E: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_list().entries(self.into_iter()).finish()
    }
}

impl<'a, T, E> Iterator for TMVIter<'a, T, E> {
    type Item = TMVec<'a, T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tmv.get(self.idx)?;
        self.idx += 1;
        return Some(val);
    }
}

impl<'a, T, E> IntoIterator for &'a TaggedMultiVec<T, E> {
    type Item = TMVec<'a, T, E>;
    type IntoIter = TMVIter<'a, T, E>;

    fn into_iter(self) -> Self::IntoIter {
        return TMVIter { tmv: self, idx: 0 };
    }
}

#[test]
fn test_tmv() {
    use crate::test::*;
    fn new_vec() -> Vec<u32> {
        let mut vec = Vec::new();
        vec.reserve(64);
        for i in 0..64 {
            vec.push(i);
        }

        return vec;
    }

    fn new_vec2() -> Vec<Vec<u32>> {
        let mut vec = Vec::new();
        vec.reserve(64);
        for _ in 0..64 {
            vec.push(new_vec());
        }

        return vec;
    }

    let before = before_alloc();

    let mut tmv: TaggedMultiVec<u32, Vec<u32>> = TaggedMultiVec::new();
    tmv.push(12, new_vec2());
    tmv.push(12, new_vec2());
    tmv.push(12, new_vec2());
    tmv.push(12, new_vec2());

    let mut vec = tmv.get_mut(1).unwrap();
    for _ in 0..64 {
        vec.push(new_vec());
    }

    let mut vec = tmv.get_mut(2).unwrap();
    for _ in 0..64 {
        vec.push(new_vec());
    }

    let diff = after_alloc(tmv, before);
    println!("diff is {:?}", diff);
}
