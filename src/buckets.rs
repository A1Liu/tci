use aliu::{Allocator, Global};
use alloc::alloc::Layout;
use core::{fmt, mem, slice, str};

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
    pub fn new<'a>(string: &str) -> &'a mut Self {
        return Self::with_allocator(string, Global);
    }

    pub fn with_allocator<'a>(string: &str, alloc: impl Allocator) -> &'a mut Self {
        let (size, align) = (mem::size_of::<usize>(), mem::align_of::<usize>());
        let layout = unsafe { Layout::from_size_align_unchecked(size + string.len(), align) };
        let (bytes, string) = (alloc.allocate(layout).unwrap(), string.as_bytes());
        let (i_str, len) = (bytes.as_ptr(), string.len());
        let i_str = unsafe { &mut *((&mut (&mut *i_str)[0]) as *mut u8 as *mut IStr) };

        i_str.len = len;
        unsafe { i_str.as_bytes_mut() }.copy_from_slice(string);
        return i_str;
    }

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

pub trait CloneInto<'a> {
    type CloneOutput;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self::CloneOutput;
}

impl<'a, T> CloneInto<'a> for Option<T>
where
    T: CloneInto<'a>,
{
    type CloneOutput = Option<T::CloneOutput>;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self::CloneOutput {
        if let Some(a) = self.as_ref() {
            return Some(a.clone_into_alloc(alloc));
        }

        return None;
    }
}
