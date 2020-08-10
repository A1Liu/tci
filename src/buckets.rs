use std::alloc::{alloc, dealloc, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
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
    pub ref_count: AtomicUsize,
    pub next: AtomicPtr<BucketListInner>,
    pub bump: AtomicPtr<u8>,
    pub len: usize,
    pub array_begin: (),
}

struct Bump {
    ptr: NonNull<u8>,
    next_bump: NonNull<u8>,
}

#[repr(C)]
pub struct BucketList<'a> {
    unused: PhantomData<&'a u8>,
    pub data: BucketListInner,
}

pub struct BucketListRef<'a> {
    buckets: NonNull<BucketList<'a>>,
}

impl BucketListInner {
    unsafe fn bump_size_align(bump: *const u8, end: *const u8, layout: Layout) -> Result<Bump, ()> {
        let required_offset = bump.align_offset(layout.align());
        if required_offset == usize::MAX {
            return Err(());
        }

        let bump = bump.add(required_offset);
        let end_alloc = bump.add(layout.size());
        if end_alloc as usize > end as usize {
            return Err(());
        }

        return Ok(Bump {
            ptr: NonNull::new_unchecked(bump as *mut u8),
            next_bump: NonNull::new_unchecked(end_alloc as *mut u8),
        });
    }

    pub unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let array_begin = &self.array_begin as *const () as *const u8;
        let bucket_end = array_begin.add(self.len);
        let mut bump = self.bump.load(Ordering::SeqCst);

        while let Ok(Bump { ptr, next_bump }) = Self::bump_size_align(bump, bucket_end, layout) {
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

        let next = self.next.load(Ordering::SeqCst);
        if !next.is_null() {
            return (&*next).alloc(layout);
        }

        let bucket_align = cmp::max(layout.align(), mem::align_of::<BucketListInner>());
        let inner_size = cmp::max(bucket_align, mem::size_of::<BucketListInner>());
        let new_len = cmp::max(grow_array(self.len), layout.size());
        let bucket_size = inner_size + new_len;

        let next_layout = match Layout::from_size_align(bucket_size, bucket_align) {
            Ok(x) => x,
            Err(_) => return ptr::null_mut(),
        };

        let new_buffer = &mut *(alloc(next_layout) as *mut BucketListInner);
        let next_array_begin = &mut new_buffer.array_begin as *mut () as *mut u8;
        new_buffer.next = AtomicPtr::new(ptr::null_mut());
        new_buffer.bump = AtomicPtr::new(next_array_begin.add(layout.size()));
        new_buffer.len = new_len;

        let mut target = &self.next;
        while let Err(ptr) = target.compare_exchange_weak(
            ptr::null_mut(),
            new_buffer,
            Ordering::SeqCst,
            Ordering::SeqCst,
        ) {
            target = &(&*ptr).next;
        }

        return next_array_begin;
    }
}

impl<'a> Deref for BucketListRef<'a> {
    type Target = BucketList<'a>;
    fn deref(&self) -> &Self::Target {
        return unsafe { self.buckets.as_ref() };
    }
}

impl<'a> Drop for BucketListRef<'a> {
    fn drop(&mut self) {
        let mut buckets = &unsafe { self.buckets.as_ref() }.data;
        let mut ref_count = buckets.ref_count.fetch_sub(1, Ordering::SeqCst);
        while ref_count == 1 {
            unsafe {
                let next_buckets = buckets.next.load(Ordering::SeqCst);

                let bucket_size = buckets.len + mem::size_of::<BucketListInner>();
                let bucket_align = mem::align_of::<BucketListInner>();
                let layout = Layout::from_size_align_unchecked(bucket_size, bucket_align);
                dealloc(
                    buckets as *const BucketListInner as *mut BucketListInner as *mut u8,
                    layout,
                );

                if next_buckets.is_null() {
                    break;
                }

                buckets = &*next_buckets;
                ref_count = buckets.ref_count.fetch_sub(1, Ordering::SeqCst);
            }
        }
    }
}

impl<'a> Clone for BucketListRef<'a> {
    fn clone(&self) -> Self {
        unsafe {
            self.buckets
                .as_ref()
                .data
                .ref_count
                .fetch_add(1, Ordering::SeqCst);
        }

        Self {
            buckets: self.buckets.clone(),
        }
    }
}

impl<'a> BucketListRef<'a> {
    pub fn next(&self) -> Option<Self> {
        let next = NonNull::new(self.data.next.load(Ordering::SeqCst));
        if let Some(next) = next {
            unsafe {
                next.as_ref().ref_count.fetch_add(1, Ordering::SeqCst);
                return Some(Self {
                    buckets: NonNull::new_unchecked(next.as_ptr() as *mut BucketList),
                });
            }
        }

        return None;
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
            new.data.ref_count = AtomicUsize::new(1);
            new.data.next = AtomicPtr::new(ptr::null_mut());
            new.data.bump = AtomicPtr::new(&mut new.data.array_begin as *mut () as *mut u8);
            new.data.len = capacity;
            return BucketListRef {
                buckets: NonNull::new_unchecked(new as *mut Self),
            };
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

#[test]
fn test_bucket_list() {
    let bucket_list = BucketList::new();
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
