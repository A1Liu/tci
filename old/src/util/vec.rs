use super::general::*;
use alloc::alloc::{alloc, dealloc, Layout};
use core::{fmt, marker, mem, ops, ptr, slice, str};

pub fn vec_reserve_gc<T, F>(vec: &mut Vec<T>, additional: usize, gc: F)
where
    F: FnOnce(&[T], &mut [mem::MaybeUninit<T>]) -> usize,
{
    let (len, old_capa) = (vec.len(), vec.capacity());
    if len + additional <= old_capa {
        return;
    }

    let new_capa = core::cmp::max(len * 3 / 2 + additional, vec.capacity() * 3 / 2);
    let new_capa = core::cmp::max(new_capa, 8);
    let new_layout = Layout::array::<T>(new_capa).unwrap();

    let new_buf = unsafe {
        let buf = alloc(new_layout) as *mut mem::MaybeUninit<T>;
        slice::from_raw_parts_mut(buf, new_capa)
    };

    let mut elements_vec = mem::replace(vec, Vec::new());
    let elements = unsafe { slice::from_raw_parts_mut(elements_vec.as_mut_ptr(), len) };
    mem::forget(elements_vec);

    let new_len = gc(elements, new_buf);

    let (new_buf, old_buf) = (new_buf.as_mut_ptr(), elements.as_mut_ptr() as *mut u8);
    unsafe {
        if old_capa != 0 {
            dealloc(old_buf, Layout::array::<T>(old_capa).unwrap());
        }
        *vec = Vec::from_raw_parts(new_buf as *mut T, new_len, new_capa);
    }
}

pub struct VecU8 {
    pub data: Vec<u8>,
}

impl VecU8 {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn push<T: Copy + 'static>(&mut self, t: T) {
        let from_bytes = any_as_u8_slice(&t);
        self.data.extend_from_slice(from_bytes);
    }

    pub fn push_aligned<T: Copy + 'static>(&mut self, t: T) {
        self.align(mem::align_of::<T>());
        let from_bytes = any_as_u8_slice(&t);
        self.data.extend_from_slice(from_bytes);
    }

    pub fn append(&mut self, data: &mut Vec<u8>) {
        self.data.append(data);
    }

    pub fn align(&mut self, align: usize) {
        let aligned = align_usize(self.data.len(), align);
        self.data.resize(aligned, 0);
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct TE<T, E>(pub T, pub [E]);

pub struct TaggedMultiArray<T, E> {
    elements: Vec<u8>,
    tags: Vec<(usize, usize)>,
    phantom: marker::PhantomData<(T, E)>,
}

pub struct TMAIter<'a, T, E> {
    tma: &'a TaggedMultiArray<T, E>,
    idx: usize,
}

pub struct TMAIterMut<'a, T, E> {
    tma: &'a mut TaggedMultiArray<T, E>,
    idx: usize,
}

impl<T, E> TaggedMultiArray<T, E> {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            tags: Vec::new(),
            phantom: marker::PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        return self.tags.len();
    }

    pub fn push(&mut self, tag: T, data: Vec<E>) {
        let begin = align_usize(self.elements.len(), mem::align_of::<T>());
        let len = data.len();
        self.elements.resize(begin, 0);
        self.elements.extend_from_slice(any_as_u8_slice(&tag));
        mem::forget(tag);
        let elem_begin = align_usize(self.elements.len(), mem::align_of::<E>());
        self.elements.resize(elem_begin, 0);

        for elem in data {
            self.elements.extend_from_slice(any_as_u8_slice(&elem));
            mem::forget(elem);
        }

        self.tags.push((begin, len));
    }

    pub fn push_from(&mut self, tag: T, data: &[E])
    where
        E: Clone,
    {
        let begin = align_usize(self.elements.len(), mem::align_of::<T>());
        let len = data.len();
        self.elements.resize(begin, 0);
        self.elements.extend_from_slice(any_as_u8_slice(&tag));
        mem::forget(tag);
        let elem_begin = align_usize(self.elements.len(), mem::align_of::<E>());
        self.elements.resize(elem_begin, 0);

        for elem in data {
            let elem = elem.clone();
            self.elements.extend_from_slice(any_as_u8_slice(&elem));
            mem::forget(elem);
        }

        self.tags.push((begin, len));
    }

    pub fn pop(&mut self) -> Option<T> {
        let (idx, len) = self.tags.pop()?;

        let ptr = &mut self.elements[idx] as *mut u8 as *mut T;
        let TE(tag, elems) = unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, len);
            mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr)
        };

        let tag = unsafe { ptr::read(tag) };
        for e in elems.iter_mut() {
            unsafe { ptr::drop_in_place(e) };
        }

        self.elements.resize(idx, 0);
        return Some(tag);
    }

    pub fn last_mut(&mut self) -> Option<&mut TE<T, E>> {
        let (idx, len) = *self.tags.last()?;
        let ptr = &mut self.elements[idx] as *mut u8 as *mut T;
        unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, len);
            return Some(mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr));
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut TE<T, E>> {
        let (idx, len) = *self.tags.get(idx)?;
        let ptr = &mut self.elements[idx] as *mut u8 as *mut T;
        unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, len);
            return Some(mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr));
        }
    }

    pub fn get(&self, idx: usize) -> Option<&TE<T, E>> {
        let (idx, len) = *self.tags.get(idx)?;
        let ptr = &self.elements[idx] as *const u8 as *const T;
        unsafe {
            let fat_ptr = slice::from_raw_parts(ptr, len);
            return Some(mem::transmute::<&[T], &TE<T, E>>(fat_ptr));
        }
    }
}
impl<T, E> fmt::Debug for TaggedMultiArray<T, E>
where
    T: fmt::Debug,
    E: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_list().entries(self.into_iter()).finish()
    }
}

impl<T, E> ops::Index<u32> for TaggedMultiArray<T, E> {
    type Output = TE<T, E>;

    fn index(&self, idx: u32) -> &TE<T, E> {
        return self.get(idx as usize).unwrap();
    }
}

impl<T, E> ops::IndexMut<u32> for TaggedMultiArray<T, E> {
    fn index_mut(&mut self, idx: u32) -> &mut TE<T, E> {
        return self.get_mut(idx as usize).unwrap();
    }
}

impl<T, E> ops::Index<usize> for TaggedMultiArray<T, E> {
    type Output = TE<T, E>;

    fn index(&self, idx: usize) -> &TE<T, E> {
        return self.get(idx).unwrap();
    }
}

impl<T, E> ops::IndexMut<usize> for TaggedMultiArray<T, E> {
    fn index_mut(&mut self, idx: usize) -> &mut TE<T, E> {
        return self.get_mut(idx).unwrap();
    }
}

impl<'a, T, E> Iterator for TMAIterMut<'a, T, E> {
    type Item = &'a mut TE<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tma.get_mut(self.idx)? as *mut TE<T, E>;
        self.idx += 1;
        return Some(unsafe { &mut *val });
    }
}

impl<'a, T, E> Iterator for TMAIter<'a, T, E> {
    type Item = &'a TE<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tma.get(self.idx)?;
        self.idx += 1;
        return Some(val);
    }
}

impl<'a, T, E> IntoIterator for &'a TaggedMultiArray<T, E> {
    type Item = &'a TE<T, E>;
    type IntoIter = TMAIter<'a, T, E>;

    fn into_iter(self) -> Self::IntoIter {
        return TMAIter { tma: self, idx: 0 };
    }
}

impl<'a, T, E> IntoIterator for &'a mut TaggedMultiArray<T, E> {
    type Item = &'a mut TE<T, E>;
    type IntoIter = TMAIterMut<'a, T, E>;

    fn into_iter(self) -> Self::IntoIter {
        return TMAIterMut { tma: self, idx: 0 };
    }
}

impl<T, E> Drop for TaggedMultiArray<T, E> {
    fn drop(&mut self) {
        while let Some(_) = self.pop() {}
    }
}

#[repr(C)]
pub struct TS<T>(pub T, pub str);

#[derive(Debug)]
pub struct StringArray<T> {
    pub data: TaggedMultiArray<T, u8>,
}

pub struct SAIter<'a, T> {
    sa: &'a StringArray<T>,
    idx: usize,
}

impl<T> StringArray<T> {
    pub fn new() -> Self {
        Self {
            data: TaggedMultiArray::new(),
        }
    }

    pub fn len(&self) -> usize {
        return self.data.len();
    }

    pub fn push(&mut self, tag: T, data: &str) {
        self.data.push_from(tag, data.as_bytes());
    }

    pub fn get(&self, idx: usize) -> Option<&TS<T>> {
        let ts = self.data.get(idx)?;
        return Some(unsafe { mem::transmute(ts) });
    }
}

impl<T> ops::Index<usize> for StringArray<T> {
    type Output = TS<T>;

    fn index(&self, idx: usize) -> &TS<T> {
        return self.get(idx).unwrap();
    }
}

impl<'a, T> Iterator for SAIter<'a, T> {
    type Item = &'a TS<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.sa.get(self.idx)?;
        self.idx += 1;
        return Some(val);
    }
}

impl<'a, T> IntoIterator for &'a StringArray<T> {
    type Item = &'a TS<T>;
    type IntoIter = SAIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        return SAIter { sa: self, idx: 0 };
    }
}
