use std::alloc::{alloc, dealloc, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::{cmp, mem, ptr, slice, str};

const INITIAL_BUCKET_SIZE: usize = 2048 - mem::size_of::<BucketListInner>();

#[inline]
pub fn grow_array(len: usize) -> usize {
    if len > usize::MAX / 3 * 2 {
        panic!("length would probably overflow here");
    }
    return len / 2 + len;
}

#[repr(C)]
pub struct BucketListInner {
    pub next: AtomicPtr<BucketListInner>,
    pub bump: AtomicPtr<u8>,
    pub len: usize,
    pub array_begin: (),
}

struct Bump {
    ptr: NonNull<u8>,
    next_bump: NonNull<u8>,
}

pub struct Frame<'a> {
    data: &'a mut [u8],
    bump: usize,
}

#[repr(C)]
pub struct BucketList<'a> {
    unused: PhantomData<&'a u8>,
    pub data: BucketListInner,
}

#[derive(Clone, Copy)]
pub struct BucketListRef<'a> {
    buckets: NonNull<BucketList<'a>>,
}

impl<'a> BucketListRef<'a> {
    pub unsafe fn dealloc(self) -> Option<Self> {
        todo!()
    }
}

impl BucketListInner {
    unsafe fn bump_size_align(bump: *const u8, end: *const u8, layout: Layout) -> Option<Bump> {
        let required_offset = bump.align_offset(layout.align());
        if required_offset == usize::MAX {
            return None;
        }

        let bump = bump.add(required_offset);
        let end_alloc = bump.add(layout.size());
        if end_alloc as usize > end as usize {
            return None;
        }

        return Some(Bump {
            ptr: NonNull::new_unchecked(bump as *mut u8),
            next_bump: NonNull::new_unchecked(end_alloc as *mut u8),
        });
    }

    pub unsafe fn make_next(&self, min_layout: Layout) -> (*mut BucketListInner, Layout) {
        let bucket_align = cmp::max(min_layout.align(), mem::align_of::<BucketListInner>());
        let inner_size = cmp::max(bucket_align, mem::size_of::<BucketListInner>());
        let new_len = cmp::max(grow_array(self.len), min_layout.size());
        let bucket_size = inner_size + new_len;

        let next_layout = match Layout::from_size_align(bucket_size, bucket_align) {
            Ok(x) => x,
            Err(_) => return (ptr::null_mut(), min_layout),
        };

        let new_buffer = &mut *(alloc(next_layout) as *mut BucketListInner);
        let next_array_begin = &mut new_buffer.array_begin as *mut () as *mut u8;
        new_buffer.next = AtomicPtr::new(ptr::null_mut());
        new_buffer.bump = AtomicPtr::new(next_array_begin);
        new_buffer.len = new_len;
        return (new_buffer, next_layout);
    }

    pub unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let array_begin = &self.array_begin as *const () as *const u8;
        let bucket_end = array_begin.add(self.len);
        let mut bump = self.bump.load(Ordering::SeqCst);

        while let Some(Bump { ptr, next_bump }) = Self::bump_size_align(bump, bucket_end, layout) {
            if let Err(ptr) = self.bump.compare_exchange_weak(
                bump,
                next_bump.as_ptr(),
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                bump = ptr;
            } else {
                return ptr.as_ptr();
            }
        }

        let mut next = self.next.load(Ordering::SeqCst);
        if next.is_null() {
            let (new_buffer, new_layout) = self.make_next(layout);
            if let Err(ptr) = self.next.compare_exchange(
                ptr::null_mut(),
                new_buffer,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                dealloc(new_buffer as *mut u8, new_layout);
                next = ptr;
            } else {
                next = new_buffer;
            }
        }

        return (&*next).alloc(layout);
    }
}

impl<'a> Deref for BucketListRef<'a> {
    type Target = BucketList<'a>;
    fn deref(&self) -> &Self::Target {
        return unsafe { self.buckets.as_ref() };
    }
}

impl<'a> BucketListRef<'a> {
    pub fn next(&self) -> Option<Self> {
        let next = NonNull::new(self.data.next.load(Ordering::SeqCst));
        if let Some(next) = next {
            unsafe {
                return Some(Self {
                    buckets: NonNull::new_unchecked(next.as_ptr() as *mut BucketList),
                });
            }
        }

        return None;
    }

    pub fn force_next(&self) -> Self {
        let inner = &self.data;
        let mut next = inner.next.load(Ordering::SeqCst);
        unsafe {
            if next.is_null() {
                let (new_buffer, new_layout) = inner.make_next(Layout::new::<()>());
                if let Err(ptr) = inner.next.compare_exchange(
                    ptr::null_mut(),
                    new_buffer,
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                ) {
                    dealloc(new_buffer as *mut u8, new_layout);
                    next = ptr;
                } else {
                    next = new_buffer;
                }
            }

            return Self {
                buckets: NonNull::new_unchecked(next as *mut BucketList),
            };
        }
    }
}

impl<'a> BucketList<'a> {
    pub fn new() -> BucketListRef<'a> {
        return Self::with_capacity(INITIAL_BUCKET_SIZE);
    }

    pub fn with_capacity(capacity: usize) -> BucketListRef<'a> {
        let bucket_align = mem::align_of::<BucketListInner>();
        let bucket_size = mem::size_of::<BucketListInner>() + capacity;
        unsafe {
            let layout = Layout::from_size_align_unchecked(bucket_size, bucket_align);
            let new = &mut *(alloc(layout) as *mut Self);
            new.data.next = AtomicPtr::new(ptr::null_mut());
            new.data.bump = AtomicPtr::new(&mut new.data.array_begin as *mut () as *mut u8);
            new.data.len = capacity;
            return BucketListRef {
                buckets: NonNull::new_unchecked(new as *mut Self),
            };
        }
    }

    pub fn alloc_frame(&self, layout: Layout) -> Frame<'a> {
        unsafe {
            let data = slice::from_raw_parts_mut(self.data.alloc(layout), layout.size());
            return Frame { data, bump: 0 };
        }
    }

    pub fn add<T>(&self, t: T) -> &'a mut T {
        unsafe {
            let location = self.data.alloc(Layout::new::<T>()) as *mut T;
            ptr::write(location, t);
            return &mut *location;
        }
    }

    pub fn add_array<T>(&self, vec: Vec<T>) -> &'a mut [T] {
        unsafe {
            let len = vec.len();
            let layout =
                Layout::from_size_align_unchecked(mem::size_of::<T>() * len, mem::align_of::<T>());
            let block = self.data.alloc(layout) as *mut T;
            let mut location = block;
            for t in vec {
                ptr::write(location, t);
                location = location.add(1);
            }
            return slice::from_raw_parts_mut(block, len);
        }
    }

    pub fn add_slice<T>(&self, slice: &[T]) -> &'a mut [T]
    where
        T: Clone,
    {
        let len = slice.len();
        let (size, align) = (mem::size_of::<T>(), mem::align_of::<T>());
        let layout = unsafe { Layout::from_size_align_unchecked(size * len, align) };
        let block = unsafe { self.data.alloc(layout) as *mut T };
        let mut location = block;
        for t in slice {
            unsafe { ptr::write(location, t.clone()) };
            location = unsafe { location.add(1) };
        }
        return unsafe { slice::from_raw_parts_mut(block, len) };
    }

    pub fn add_str(&self, values: &str) -> &'a mut str {
        let values = values.as_bytes();
        return unsafe { str::from_utf8_unchecked_mut(self.add_slice(values)) };
    }
}

impl<'a> Frame<'a> {
    // This could use &self if the bump were atomic
    pub fn alloc(&mut self, layout: Layout) -> *mut u8 {
        // todo do alignment stuff here
        let bump_ptr = &mut self.data[self.bump] as *mut u8;

        let required_offset = bump_ptr.align_offset(layout.align());
        if required_offset == usize::MAX {
            panic!("Frame: couldn't get aligned pointer");
        }

        self.bump += required_offset;
        let bump = self.bump;
        self.bump += layout.size();
        if self.bump > self.data.len() {
            panic!(
                "allocated past end of frame: at byte {} of frame with size {}",
                self.bump,
                self.data.len()
            );
        }

        &mut self.data[bump] as *mut u8
    }

    pub fn add<T>(&mut self, t: T) -> &'a mut T {
        unsafe {
            let location = self.alloc(Layout::new::<T>()) as *mut T;
            ptr::write(location, t);
            return &mut *location;
        }
    }

    pub fn add_array<T>(&mut self, vec: Vec<T>) -> &'a mut [T] {
        unsafe {
            let len = vec.len();
            let layout =
                Layout::from_size_align_unchecked(mem::size_of::<T>() * len, mem::align_of::<T>());
            let block = self.alloc(layout) as *mut T;
            let mut location = block;
            for t in vec {
                ptr::write(location, t);
                location = location.add(1);
            }
            return slice::from_raw_parts_mut(block, len);
        }
    }

    pub fn add_slice<T>(&mut self, slice: &[T]) -> &'a mut [T]
    where
        T: Clone,
    {
        let len = slice.len();
        let (size, align) = (mem::size_of::<T>(), mem::align_of::<T>());
        let layout = unsafe { Layout::from_size_align_unchecked(size * len, align) };
        let block = self.alloc(layout) as *mut T;
        let mut location = block;
        for t in slice {
            unsafe { ptr::write(location, t.clone()) };
            location = unsafe { location.add(1) };
        }
        return unsafe { slice::from_raw_parts_mut(block, len) };
    }

    pub fn add_str(&mut self, values: &str) -> &'a mut str {
        let values = values.as_bytes();
        return unsafe { str::from_utf8_unchecked_mut(self.add_slice(values)) };
    }
}

#[test]
fn test_bucket_list() {
    let bucket_list = BucketList::with_capacity(24);
    let vec = bucket_list.add(Vec::<usize>::new());
    let num = bucket_list.add(12);
    vec.push(*num);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);

    println!("boring");
}
