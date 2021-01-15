use super::error::*;
use super::memory::*;
use super::types::*;
use crate::util::*;
use core::mem;
use std::io::Write;

#[derive(Debug)]
pub struct Kernel {
    output: StringArray<WriteEvent>,
    memory: Memory,
}

impl Kernel {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            output: StringArray::new(),
            memory: Memory::new(&binary),
        }
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}

pub fn printf(
    sel: &mut Memory,
    out: &mut StringArray<WriteEvent>,
    format_ptr: VarPointer,
    args: usize,
) -> Result<i32, IError> {
    let mut string_out = StringWriter::new();
    let args = args as u16;
    let result = printf_internal(sel, format_ptr, args, &mut string_out);
    let string_out = string_out.into_string();
    let len = string_out.len() as i32; // TODO overflow

    out.push(WriteEvent::StdoutWrite, &string_out);
    result?;

    return Ok(len);
}

#[allow(unused_assignments)] // TODO remove this when we make this fully standard compliant
pub fn printf_internal(
    sel: &mut Memory,
    format_ptr: VarPointer,
    mut current_offset: u16,
    mut out: &mut StringWriter,
) -> Result<(), IError> {
    // OPTIMIZE This does an unnecessary linear scan
    let format_str = sel.cstring_bytes(format_ptr)?;
    let map_err = |err| ierror!("WriteFailed", "failed to write to stdout ({})", err);

    // CREDIT heavily inspired by https://github.com/mpaland/printf/blob/master/printf.c

    const FLAGS_ZEROPAD: u32 = 1;
    const FLAGS_LEFT: u32 = 2;
    const FLAGS_PLUS: u32 = 4;
    const FLAGS_SPACE: u32 = 8;
    const FLAGS_HASH: u32 = 16;
    const FLAGS_PRECISION: u32 = 32;
    const FLAGS_LONG: u32 = 64;
    const FLAGS_LONG_LONG: u32 = 128;

    let mut next_ptr = || {
        let var_ptr = VarPointer::new_stack(current_offset, 0);
        current_offset -= 1;
        return var_ptr;
    };

    let parse_int = |begin: usize| {
        let mut idx = begin;
        if format_str[idx] >= b'0' && format_str[idx] <= b'9' {
            let mut collect = 0;
            loop {
                collect *= 10;
                collect += (format_str[idx] - b'0') as usize;
                idx += 1;

                if format_str[idx] < b'0' || format_str[idx] > b'9' {
                    break;
                }
            }

            return Some((collect, idx - begin));
        }

        return None;
    };

    let mut idx = 0;
    while idx < format_str.len() {
        let mut idx2 = idx;
        while idx2 < format_str.len() && format_str[idx2] != b'%' {
            idx2 += 1;
        }

        write_utf8_lossy(&mut out, &format_str[idx..idx2]).map_err(map_err)?;

        if idx2 == format_str.len() {
            break;
        }

        // format_str[idx2] == b'%'

        idx2 += 1;
        if idx2 == format_str.len() {
            return Err(ierror!(
                "InvalidFormatString",
                "format string ends with a single '%'; to print out a '%' use '%%'"
            ));
        }

        // format specifier?  %[flags][width][.precision][length]
        let mut flags = 0;
        let mut width = 0;
        let mut precision = 0;

        loop {
            match format_str[idx2] {
                b'0' => flags |= FLAGS_ZEROPAD,
                b'-' => flags |= FLAGS_LEFT,
                b'+' => flags |= FLAGS_PLUS,
                b' ' => flags |= FLAGS_SPACE,
                b'#' => flags |= FLAGS_HASH,
                _ => break,
            }
            idx += 1;
        }

        if let Some((w, diff)) = parse_int(idx2) {
            idx2 += diff;
            width = w;
        } else if format_str[idx2] == b'*' {
            let mut next = i32::from_le(sel.read(next_ptr())?);
            if next < 0 {
                flags |= FLAGS_LEFT;
                next *= -1;
            }

            width = next as usize;
            idx2 += 1;
        }

        if format_str[idx2] == b'.' {
            flags |= FLAGS_PRECISION;
            idx2 += 1;

            if let Some((prec, diff)) = parse_int(idx2) {
                idx2 += diff;
                precision = prec;
            } else if format_str[idx2] == b'*' {
                let next = i32::from_le(sel.read(next_ptr())?);
                precision = if next > 0 { next } else { 0 } as usize;
                idx2 += 1;
            }
        }

        match format_str[idx2] {
            b'l' => {
                flags |= FLAGS_LONG;
                idx2 += 1;
                if format_str[idx2] == b'l' {
                    flags |= FLAGS_LONG_LONG;
                    idx2 += 1;
                }
            }
            _ => {}
        }

        match format_str[idx2] {
            b'u' => {
                let base = 10;
                flags &= !FLAGS_HASH;
                if (flags & FLAGS_PRECISION) != 0 {
                    flags &= !FLAGS_ZEROPAD;
                }
                flags &= !(FLAGS_PLUS | FLAGS_SPACE);

                if (flags & FLAGS_LONG_LONG) != 0 {
                    let value = u64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else if (flags & FLAGS_LONG) != 0 {
                    let value = u64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else {
                    let value = u32::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                }
            }
            b'i' | b'd' => {
                let base = 10;
                flags &= !FLAGS_HASH;
                if (flags & FLAGS_PRECISION) != 0 {
                    flags &= !FLAGS_ZEROPAD;
                }

                if (flags & FLAGS_LONG_LONG) != 0 {
                    let value = i64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else if (flags & FLAGS_LONG) != 0 {
                    let value = i64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else {
                    let value = i32::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                }
            }
            b'c' => {
                let value: u8 = sel.read(next_ptr())?;
                write!(&mut out, "{}", char::from(value)).map_err(map_err)?;
            }
            b'%' => {
                write_utf8_lossy(&mut out, &[b'%']).map_err(map_err)?;
            }
            b's' => {
                let char_ptr = sel.read(next_ptr())?;

                write_utf8_lossy(&mut out, sel.cstring_bytes(char_ptr)?).map_err(map_err)?;
            }
            byte => {
                return Err(ierror!(
                    "InvalidFormatString",
                    "got byte '{}' after '%'",
                    char::from(byte)
                ))
            }
        }

        idx = idx2 + 1;
    }

    return Ok(());
}
