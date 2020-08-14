use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};
use core::fmt;
use std::io;

// https://stackoverflow.com/questions/28127165/how-to-convert-struct-to-u8
pub unsafe fn any_as_u8_slice_mut<T: Sized + Copy>(p: &mut T) -> &mut [u8] {
    std::slice::from_raw_parts_mut(p as *mut T as *mut u8, std::mem::size_of::<T>())
}

pub fn any_as_u8_slice<T: Sized + Copy>(p: &T) -> &[u8] {
    unsafe { std::slice::from_raw_parts(p as *const T as *const u8, std::mem::size_of::<T>()) }
}

pub fn u32_to_u16_tup(value: u32) -> (u16, u16) {
    ((value >> 16) as u16, value as u16)
}

#[derive(Debug, Clone, Copy)]
pub struct FuncDesc {
    pub file: u32,
    pub name: u32,
}

impl FuncDesc {
    pub fn into_callframe(self, line: u32) -> CallFrame {
        CallFrame {
            file: self.file,
            name: self.name,
            line,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub file: u32,
    pub name: u32,
    pub line: u32,
}

#[derive(Clone, PartialEq, Copy)]
pub struct Range {
    pub start: u32,
    pub end: u32,
}

impl fmt::Debug for Range {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}..{}", self.start, self.end)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}..{}", self.start, self.end)
    }
}

pub fn r(start: u32, end: u32) -> Range {
    Range { start, end }
}

pub fn r_from(range1: Range, range2: Range) -> Range {
    Range {
        start: range1.start,
        end: range2.start,
    }
}

pub struct StringWriter {
    buf: Vec<u8>,
}

impl StringWriter {
    pub fn new() -> StringWriter {
        StringWriter {
            buf: Vec::with_capacity(8 * 1024),
        }
    }

    pub fn to_string(&self) -> String {
        if let Ok(s) = String::from_utf8(self.buf.clone()) {
            s
        } else {
            String::new()
        }
    }
}

impl io::Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        for b in buf {
            self.buf.push(*b);
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl WriteColor for StringWriter {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, color: &ColorSpec) -> io::Result<()> {
        return Ok(());
    }

    fn reset(&mut self) -> io::Result<()> {
        return Ok(());
    }
}

pub struct Void {
    unused: (),
}

impl Void {
    pub fn new() -> Self {
        return Self { unused: () };
    }
}

impl io::Write for Void {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
