use core::fmt;
use std::io::Write;

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

    pub fn to_string(self) -> String {
        if let Ok(s) = String::from_utf8(self.buf) {
            s
        } else {
            String::new()
        }
    }
}

impl Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        for b in buf {
            self.buf.push(*b);
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub struct Void {}

impl Void {
    pub fn new() -> Self {
        return Self {};
    }
}

impl Write for Void {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
