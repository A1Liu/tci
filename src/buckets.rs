use alloc::alloc::{alloc, dealloc, Layout, LayoutError};
use core::marker::PhantomData;
use core::ops::Deref;
use core::ptr::NonNull;
use core::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use core::{cmp, fmt, mem, ptr, slice, str};

use alloc::vec::Vec;

const INITIAL_BUCKET_SIZE: usize = 2048 - mem::size_of::<BucketListInner>();

#[repr(C)]
pub struct IStr {
    pub len: usize,
    chars: (),
}

impl PartialEq for IStr {
    fn eq(&self, other: &Self) -> bool {
        return self.as_str() == other.as_str();
    }
}

impl fmt::Debug for IStr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return self.as_str().fmt(fmt);
    }
}

impl IStr {
    pub unsafe fn as_bytes_mut(&mut self) -> &mut [u8] {
        let begin = &mut self.chars as *mut () as *mut u8;
        return slice::from_raw_parts_mut(begin, self.len);
    }

    pub fn as_str(&self) -> &str {
        let begin = &self.chars as *const () as *const u8;
        let slice = unsafe { slice::from_raw_parts(begin, self.len) };
        return unsafe { str::from_utf8_unchecked(slice) };
    }
}

pub struct Frame<'a> {
    pub data: &'a mut [u8],
    pub bump: AtomicUsize,
}

pub trait Allocator<'a> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8;

    fn add<T>(&self, t: T) -> &'a mut T {
        unsafe {
            let location = self.alloc(Layout::new::<T>()) as *mut T;
            ptr::write(location, t);
            return &mut *location;
        }
    }

    fn uninit(&self, len: usize, align: usize) -> Result<&'a mut [u8], LayoutError> {
        let layout = Layout::from_size_align(len, align)?;
        unsafe {
            let block = self.alloc(layout) as *mut u8;
            return Ok(slice::from_raw_parts_mut(block, len));
        }
    }

    fn build_array<T, F>(&self, len: usize, mut f: F) -> Result<&'a mut [T], LayoutError>
    where
        F: FnMut(usize) -> T,
    {
        unsafe {
            let layout = Layout::from_size_align(mem::size_of::<T>() * len, mem::align_of::<T>())?;
            let block = self.alloc(layout) as *mut T;
            let mut location = block;
            for idx in 0..len {
                ptr::write(location, f(idx));
                location = location.add(1);
            }
            return Ok(slice::from_raw_parts_mut(block, len));
        }
    }

    fn frame(&self, len: usize) -> Result<Frame<'a>, LayoutError> {
        let data = self.uninit(len, 16)?;
        let bump = AtomicUsize::new(0);
        return Ok(Frame { data, bump });
    }

    fn add_array<T>(&self, vec: Vec<T>) -> &'a mut [T]
    where
        T: Copy,
    {
        let mut vec = vec;
        let capa = vec.capacity();
        let array = self
            .build_array(vec.len(), |idx| unsafe { ptr::read(&vec[idx]) })
            .unwrap();

        let to_free = vec.as_mut_ptr() as *mut u8;
        if capa > 0 {
            unsafe { dealloc(to_free, Layout::array::<T>(capa).unwrap()) };
        }

        mem::forget(vec);
        return array;
    }

    fn add_slice<T>(&self, slice: &[T]) -> &'a mut [T]
    where
        T: Copy,
    {
        return self
            .build_array(slice.len(), |idx| slice[idx].clone())
            .unwrap();
    }

    fn add_str(&self, string: &str) -> &'a mut str {
        let string = string.as_bytes();
        return unsafe { str::from_utf8_unchecked_mut(self.add_slice(string)) };
    }

    fn add_i_str(&self, string: &str) -> &'a mut IStr {
        let (size, align) = (mem::size_of::<usize>(), mem::align_of::<usize>());
        let (bytes, string) = (self.uninit(size + string.len(), align), string.as_bytes());
        let (i_str, len) = (bytes.unwrap().as_mut_ptr() as *mut IStr, string.len());
        let i_str = unsafe { &mut *i_str };

        i_str.len = len;
        unsafe { i_str.as_bytes_mut() }.copy_from_slice(string);
        return i_str;
    }
}

pub trait CloneInto<'a> {
    type CloneOutput;

    fn clone_into_alloc(&self, alloc: &impl Allocator<'a>) -> Self::CloneOutput;
}

impl<'a, T> CloneInto<'a> for Option<T>
where
    T: CloneInto<'a>,
{
    type CloneOutput = Option<T::CloneOutput>;

    fn clone_into_alloc(&self, alloc: &impl Allocator<'a>) -> Self::CloneOutput {
        if let Some(a) = self.as_ref() {
            return Some(a.clone_into_alloc(alloc));
        }

        return None;
    }
}

impl<'a, T> Allocator<'a> for &T
where
    T: Allocator<'a>,
{
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        return (*self).alloc(layout);
    }
}

impl<'a, T> Allocator<'a> for &mut T
where
    T: Allocator<'a>,
{
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        return (&**self).alloc(layout);
    }
}

impl<'a> Allocator<'a> for Frame<'a> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        use Ordering::*;

        let mut bump = self.bump.load(Ordering::SeqCst);
        loop {
            let bump_ptr = self.data.as_ptr().add(bump);

            let required_offset = bump_ptr.align_offset(layout.align());
            if required_offset == usize::MAX {
                panic!("Frame: couldn't get aligned pointer");
            }

            let aligned = bump + required_offset;
            let next = aligned + layout.size();
            if next > self.data.len() {
                panic!(
                    "allocated past end of frame: at byte {} of frame with size {}",
                    next,
                    self.data.len()
                );
            }

            match self.bump.compare_exchange_weak(bump, next, SeqCst, SeqCst) {
                Ok(val) => {}
                Err(val) => {
                    bump = val;
                    continue;
                }
            }

            return &self.data[aligned] as *const u8 as *mut u8;
        }
    }
}

#[inline]
pub fn grow_array(len: usize) -> usize {
    if len >= usize::MAX / 3 * 2 {
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

#[repr(C)]
pub struct BucketList<'a> {
    unused: PhantomData<&'a u8>,
    pub data: BucketListInner,
}

#[derive(Clone, Copy)]
pub struct BucketListRef<'a> {
    buckets: NonNull<BucketList<'a>>,
}

impl<'a> Allocator<'a> for BucketListRef<'a> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        return self.deref().alloc(layout);
    }
}

unsafe impl<'a> Send for BucketListRef<'a> {}

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

    pub unsafe fn dealloc(&self) -> Option<BucketListRef<'a>> {
        let next = NonNull::new(self.data.next.load(Ordering::SeqCst));
        let bucket_align = mem::align_of::<BucketListInner>();
        let inner_size = cmp::max(bucket_align, mem::size_of::<BucketListInner>());
        let bucket_size = inner_size + self.data.len;
        let layout = Layout::from_size_align(bucket_size, bucket_align).unwrap();

        dealloc(self as *const BucketList as *mut u8, layout);

        return Some(BucketListRef {
            buckets: NonNull::new_unchecked(next?.as_ptr() as *mut BucketList),
        });
    }

    pub fn next(&self) -> Option<BucketListRef<'a>> {
        let next = NonNull::new(self.data.next.load(Ordering::SeqCst));
        if let Some(next) = next {
            unsafe {
                return Some(BucketListRef {
                    buckets: NonNull::new_unchecked(next.as_ptr() as *mut BucketList),
                });
            }
        }

        return None;
    }

    pub fn force_next(&self) -> BucketListRef<'a> {
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

            return BucketListRef {
                buckets: NonNull::new_unchecked(next as *mut BucketList),
            };
        }
    }
}

impl<'a> Allocator<'a> for BucketList<'a> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        return self.data.alloc(layout);
    }
}

pub struct BucketListFactory {
    pub begin: AtomicPtr<BucketList<'static>>,
    pub current: AtomicPtr<BucketList<'static>>,
}

impl BucketListFactory {
    pub fn new() -> Self {
        let begin = BucketList::new().buckets.as_ptr();

        Self {
            begin: AtomicPtr::new(begin),
            current: AtomicPtr::new(begin),
        }
    }

    pub fn with_capacity(capa: usize) -> Self {
        let begin = BucketList::with_capacity(capa).buckets.as_ptr();

        Self {
            begin: AtomicPtr::new(begin),
            current: AtomicPtr::new(begin),
        }
    }

    pub unsafe fn dealloc(&mut self) {
        while let Some(new) = (&mut *self.begin.load(Ordering::SeqCst)).dealloc() {
            self.begin.store(new.buckets.as_ptr(), Ordering::SeqCst);
        }
    }
}

impl Deref for BucketListFactory {
    type Target = BucketList<'static>;
    fn deref(&self) -> &Self::Target {
        let current = unsafe { &mut *self.current.load(Ordering::SeqCst) };

        if let Some(n) = current.next() {
            let result = self.current.compare_exchange(
                current,
                n.buckets.as_ptr(),
                Ordering::SeqCst,
                Ordering::SeqCst,
            );

            if result.is_ok() {
                return unsafe { &*n.buckets.as_ptr() };
            }
        }

        return current;
    }
}

#[test]
fn test_bucket_list() {
    use alloc::vec;

    let bucket_list = BucketList::with_capacity(24);
    let vec = bucket_list.add_array(Vec::<usize>::new());
    let num = bucket_list.add(12);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
    bucket_list.add_array(vec![12, 12, 31, 4123, 123, 5, 14, 5, 134, 5]);
}
