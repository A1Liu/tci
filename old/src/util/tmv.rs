use super::vec::*;
use alloc::alloc::{dealloc, Layout};
use alloc::vec::Vec;
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

    pub fn with_capacity(tags: usize, elements: usize) -> Self {
        Self {
            tags: Vec::with_capacity(tags),
            elements: Vec::with_capacity(elements),
            size: 0,
        }
    }

    pub fn len(&self) -> usize {
        return self.tags.len();
    }

    pub fn compact_gc(&mut self) {
        let mut blocks = Vec::new();
        for (tag_idx, block) in self.tags.iter().enumerate() {
            struct TMVBlock {
                tag_idx: usize,
                elem_idx: usize,
            }

            blocks.push(TMVBlock {
                tag_idx,
                elem_idx: block.elem_idx,
            });
        }

        // need to traverse the blocks in order
        blocks.sort_by_key(|b| b.elem_idx);

        let (mut write, tags, buffer) = (0, &mut self.tags, self.elements.as_mut_ptr());
        for block in &mut blocks {
            let from = buffer.wrapping_add(block.elem_idx);
            let to = buffer.wrapping_add(write);

            unsafe { ptr::copy(from, to, tags[block.tag_idx].elem_len) };

            tags[block.tag_idx].elem_idx = write;
            write += tags[block.tag_idx].elem_capa;
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

        let new_capa = core::cmp::max(elem_len * 3 / 2 + additional, elem_capa * 3 / 2);
        let new_capa = core::cmp::max(new_capa, 8);

        if self.size + new_capa <= self.elements.capacity() {
            if self.elements.len() + new_capa > self.elements.capacity() {
                self.compact_gc();
            }

            debug_assert!(self.elements.len() + new_capa <= self.elements.capacity());
            let (elem_idx, new_idx) = (self.tags[idx].elem_idx, self.elements.len());
            let elems = &mut self.elements;
            unsafe { elems.set_len(elems.len() + new_capa) };
            unsafe { ptr::copy_nonoverlapping(&elems[elem_idx], &mut elems[new_idx], elem_len) };

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

    pub fn push<'a>(&'a mut self, tag: T, data: Vec<E>) -> TMVecMut<'a, T, E> {
        let (elem_len, elem_capa, elem_ptr) = (data.len(), data.capacity(), data.as_ptr());
        self.reserve_gc(elem_capa);

        let (to_buf, elems) = (self.elements.as_mut_ptr(), &mut self.elements);
        let (elem_idx, to_ptr) = (elems.len(), to_buf.wrapping_add(elems.len()));
        debug_assert!(elem_idx + elem_capa <= elems.capacity());

        unsafe { elems.set_len(elems.len() + elem_capa) };
        unsafe { ptr::copy_nonoverlapping(elem_ptr, mem::transmute(to_ptr), elem_len) };

        if data.capacity() != 0 {
            unsafe { dealloc(elem_ptr as *mut u8, Layout::array::<E>(elem_capa).unwrap()) };
        }
        mem::forget(data);

        let idx = self.tags.len();
        self.tags.push(TMVBlock {
            tag,
            elem_idx,
            elem_len,
            elem_capa,
        });

        self.size += elem_capa;

        return TMVecMut { tmv: self, idx };
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

    pub fn last_mut(&mut self) -> Option<TMVecMut<T, E>> {
        if self.tags.len() == 0 {
            return None;
        }

        let idx = self.len() - 1;
        return Some(TMVecMut { tmv: self, idx });
    }
}

impl<T, E> TaggedMultiVec<T, E>
where
    E: Clone,
{
    pub fn push_from(&mut self, tag: T, data: &[E]) {
        self.reserve_gc(data.len());

        let (elem_idx, capa) = (self.elements.len(), self.elements.capacity());
        debug_assert!(elem_idx + data.len() <= capa);

        for e in data {
            self.elements.push(MaybeUninit::new(e.clone()));
        }

        debug_assert_eq!(capa, self.elements.capacity());
        self.tags.push(TMVBlock {
            tag,
            elem_idx,
            elem_len: data.len(),
            elem_capa: data.len(),
        });

        self.size += data.len();
    }
}

impl<T, E> Drop for TaggedMultiVec<T, E> {
    fn drop(&mut self) {
        while let Some(t) = self.pop() {}
    }
}

impl<T, E> Clone for TaggedMultiVec<T, E>
where
    T: Clone,
    E: Clone,
{
    fn clone(&self) -> Self {
        let mut new = Self::with_capacity(self.tags.len(), self.size);

        for tvec in self {
            new.push_from(tvec.tag.clone(), tvec.data);
        }

        return new;
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

        self.tmv.elements[block.elem_idx + block.elem_len] = MaybeUninit::new(value);
        block.elem_len += 1;
    }

    pub fn clear(&mut self) {
        while let Some(e) = self.pop() {}
    }

    pub fn shrink_to_fit(&mut self) {
        let block = &mut self.tmv.tags[self.idx];
        self.tmv.size -= block.elem_capa - block.elem_len;
        block.elem_capa = block.elem_len;
    }

    pub fn pop(&mut self) -> Option<E> {
        let block = &mut self.tmv.tags[self.idx];
        if block.elem_len == 0 {
            return None;
        }

        block.elem_len -= 1;
        let elem_ptr = self.tmv.elements[block.elem_idx + block.elem_len].as_mut_ptr();
        return Some(unsafe { ptr::read(elem_ptr) });
    }
}

impl<'a, T, E> ops::Deref for TMVecMut<'a, T, E> {
    type Target = [E];

    fn deref(&self) -> &Self::Target {
        let block = &self.tmv.tags[self.idx];
        let elements = &self.tmv.elements[block.elem_idx..(block.elem_idx + block.elem_len)];
        let data = unsafe { mem::transmute::<&[MaybeUninit<E>], &[E]>(elements) };
        return data;
    }
}

impl<'a, T, E> ops::DerefMut for TMVecMut<'a, T, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let block = &self.tmv.tags[self.idx];
        let elements = &mut self.tmv.elements[block.elem_idx..(block.elem_idx + block.elem_len)];
        let data = unsafe { mem::transmute::<&mut [MaybeUninit<E>], &mut [E]>(elements) };
        return data;
    }
}

impl<'a, T, E> TMVecMut<'a, T, E>
where
    E: Copy,
{
    pub fn clear_pod(&mut self) {
        self.tmv.tags[self.idx].elem_len = 0;
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
fn kitchen_sink() {
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
    vec.clear();
    vec.shrink_to_fit();

    let mut vec = tmv.get_mut(0).unwrap();
    for _ in 0..64 {
        vec.push(new_vec());
    }

    let tmv2 = tmv.clone();

    let diff = after_alloc((tmv, tmv2), before);

    // we should never call out to realloc during this test; this is important
    // because Vec::reserve calls out to realloc, but since our reserve performs
    // compaction, it doesn't. We want to make sure that Vec::reserve isn't ever called.
    assert!(diff.realloc == 0);

    println!("diff is {:?}", diff);
}
