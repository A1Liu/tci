use super::error::*;
use super::memory::*;
use super::types::*;
use crate::util::*;

pub fn run_op_count(memory: &mut Memory, count: u32) -> (u32, Result<Option<EcallExt>, IError>) {
    for idx in 0..count {
        match run_op(memory) {
            Ok(None) => {}
            Ok(Some(ecall)) => return (idx + 1, Ok(Some(ecall))),
            Err(ierr) => return (idx, Err(ierr)),
        }
    }

    return (count, Ok(None));
}

pub fn run_op(memory: &mut Memory) -> Result<Option<EcallExt>, IError> {
    let op: Opcode = memory.read_pc()?;

    match op {
        Opcode::Func => {
            // this opcode is handled by Memory
            return Err(ierror!(
                "InvalidOpcode",
                "Found Func opcode (this is an error in TCI)"
            ));
        }
        Opcode::Loc => {
            let loc = memory.read_pc()?;
            memory.set_loc(loc);
        }
        Opcode::StackAlloc => {
            let bytes: u32 = memory.read_pc()?;
            memory.add_stack_var(bytes)?;
        }
        Opcode::StackDealloc => {
            memory.pop_stack_var()?;
        }

        Opcode::Make8 => {
            let val: u8 = memory.read_pc()?;
            memory.push(val);
        }
        Opcode::Make16 => {
            let val: u16 = memory.read_pc()?;
            memory.push(val);
        }
        Opcode::Make32 => {
            let val: u32 = memory.read_pc()?;
            memory.push(val);
        }
        Opcode::Make64 => {
            let val: u64 = memory.read_pc()?;
            memory.push(val);
        }
        Opcode::MakeSp => {
            let var_offset: i16 = memory.read_pc()?;
            let stack_len = memory.stack.len() as u16;
            let var = (stack_len as i16 + var_offset) as u16;

            memory.push(VarPointer::new_stack(var, 0));
        }
        Opcode::MakeFp => {
            let var_offset: i16 = memory.read_pc()?;
            let var = (memory.fp as i16 + var_offset) as u16;

            memory.push(VarPointer::new_stack(var, 0));
        }

        Opcode::PushUndef => {
            let bytes: u32 = memory.read_pc()?;
            let stack_len = memory.expr_stack.len();
            memory.expr_stack.resize(stack_len + bytes as usize, 0);
        }
        Opcode::Pop => {
            let bytes = memory.read_pc()?;
            memory.pop_bytes(bytes)?;
        }
        Opcode::Swap => {
            let top_bytes = memory.read_pc()?;
            let bottom_bytes = memory.read_pc()?;
            memory.swap_bytes(top_bytes, bottom_bytes)?;
        }
        Opcode::Dup => {
            let bytes = memory.read_pc()?;
            memory.dup_bytes(bytes)?;
        }
        Opcode::PushDyn => {
            let ptr: VarPointer = memory.pop()?;
            let size: u32 = memory.pop()?;
            memory.read_bytes_to_stack(ptr, size)?;
        }

        Opcode::I8ToF32 => {
            let n: i8 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::U8ToF32 => {
            let n: u8 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::I8ToF64 => {
            let n: i8 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::U8ToF64 => {
            let n: i8 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::I16ToF32 => {
            let n: i16 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::U16ToF32 => {
            let n: u16 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::I16ToF64 => {
            let n: i16 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::U16ToF64 => {
            let n: i16 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::I32ToF32 => {
            let n: i32 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::U32ToF32 => {
            let n: u32 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::I32ToF64 => {
            let n: i32 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::U32ToF64 => {
            let n: i32 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::I64ToF32 => {
            let n: i64 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::U64ToF32 => {
            let n: u64 = memory.pop()?;
            memory.push(n as f32);
        }
        Opcode::I64ToF64 => {
            let n: i64 = memory.pop()?;
            memory.push(n as f64);
        }
        Opcode::U64ToF64 => {
            let n: i64 = memory.pop()?;
            memory.push(n as f64);
        }

        Opcode::F32ToI8 => {
            let f: f32 = memory.pop()?;
            memory.push(f as i8);
        }
        Opcode::F32ToU8 => {
            let f: f32 = memory.pop()?;
            memory.push(f as u8);
        }
        Opcode::F64ToI8 => {
            let f: f64 = memory.pop()?;
            memory.push(f as i8);
        }
        Opcode::F64ToU8 => {
            let f: f64 = memory.pop()?;
            memory.push(f as u8);
        }
        Opcode::F32ToI16 => {
            let f: f32 = memory.pop()?;
            memory.push(f as i16);
        }
        Opcode::F32ToU16 => {
            let f: f32 = memory.pop()?;
            memory.push(f as u16);
        }
        Opcode::F64ToI16 => {
            let f: f64 = memory.pop()?;
            memory.push(f as i16);
        }
        Opcode::F64ToU16 => {
            let f: f64 = memory.pop()?;
            memory.push(f as u16);
        }
        Opcode::F32ToI32 => {
            let f: f32 = memory.pop()?;
            memory.push(f as i32);
        }
        Opcode::F32ToU32 => {
            let f: f32 = memory.pop()?;
            memory.push(f as u32);
        }
        Opcode::F64ToI32 => {
            let f: f64 = memory.pop()?;
            memory.push(f as i32);
        }
        Opcode::F64ToU32 => {
            let f: f64 = memory.pop()?;
            memory.push(f as u32);
        }
        Opcode::F32ToI64 => {
            let f: f32 = memory.pop()?;
            memory.push(f as i64);
        }
        Opcode::F32ToU64 => {
            let f: f32 = memory.pop()?;
            memory.push(f as u64);
        }
        Opcode::F64ToI64 => {
            let f: f64 = memory.pop()?;
            memory.push(f as i64);
        }
        Opcode::F64ToU64 => {
            let f: f64 = memory.pop()?;
            memory.push(f as u64);
        }

        Opcode::F32ToF64 => {
            let f: f32 = memory.pop()?;
            memory.push(f as f64);
        }
        Opcode::F64ToF32 => {
            let f: f64 = memory.pop()?;
            memory.push(f as f32);
        }

        Opcode::SExtend8To16 => {
            let val = memory.pop::<i8>()?;
            memory.push(val as i16);
        }
        Opcode::SExtend8To32 => {
            let val = memory.pop::<i8>()?;
            memory.push(val as i32);
        }
        Opcode::SExtend8To64 => {
            let val = memory.pop::<i8>()?;
            memory.push(val as i64);
        }
        Opcode::SExtend16To32 => {
            let val: i16 = memory.pop()?;
            memory.push(val as i32);
        }
        Opcode::SExtend16To64 => {
            let val: i16 = memory.pop()?;
            memory.push(val as i64);
        }
        Opcode::SExtend32To64 => {
            let val: i32 = memory.pop()?;
            memory.push(val as i64);
        }

        Opcode::ZExtend8To16 => {
            let val = memory.pop::<u8>()?;
            memory.push(val as u16);
        }
        Opcode::ZExtend8To32 => {
            let val = memory.pop::<u8>()?;
            memory.push(val as u32);
        }
        Opcode::ZExtend8To64 => {
            let val = memory.pop::<u8>()?;
            memory.push(val as u64);
        }
        Opcode::ZExtend16To32 => {
            let val: u16 = memory.pop()?;
            memory.push(val as u32);
        }
        Opcode::ZExtend16To64 => {
            let val: u16 = memory.pop()?;
            memory.push(val as u64);
        }
        Opcode::ZExtend32To64 => {
            let val: u32 = memory.pop()?;
            memory.push(val as u64);
        }

        Opcode::Get => {
            let bytes = memory.read_pc()?;
            let ptr: VarPointer = memory.pop()?;

            memory.read_bytes_to_stack(ptr, bytes)?;
        }
        Opcode::Set => {
            let bytes = memory.read_pc()?;
            let ptr: VarPointer = memory.pop()?;
            memory.write_bytes_from_stack(ptr, bytes)?;
        }

        Opcode::BoolNorm8 => {
            let bytes: u8 = memory.pop()?;
            memory.push(if bytes != 0 { 1u8 } else { 0u8 });
        }
        Opcode::BoolNorm16 => {
            let bytes: u16 = memory.pop()?;
            memory.push(if bytes != 0 { 1u8 } else { 0u8 });
        }
        Opcode::BoolNorm32 => {
            let bytes: u32 = memory.pop()?;
            memory.push(if bytes != 0 { 1u8 } else { 0u8 });
        }
        Opcode::BoolNorm64 => {
            let bytes: u64 = memory.pop()?;
            memory.push(if bytes != 0 { 1u8 } else { 0u8 });
        }

        Opcode::BoolNot8 => {
            let bytes: u8 = memory.pop()?;
            memory.push(if bytes == 0 { 1u8 } else { 0u8 });
        }
        Opcode::BoolNot16 => {
            let bytes: u16 = memory.pop()?;
            memory.push(if bytes == 0 { 1u8 } else { 0u8 });
        }
        Opcode::BoolNot32 => {
            let bytes: u32 = memory.pop()?;
            memory.push(if bytes == 0 { 1u8 } else { 0u8 });
        }
        Opcode::BoolNot64 => {
            let bytes: u64 = memory.pop()?;
            memory.push(if bytes == 0 { 1u8 } else { 0u8 });
        }

        Opcode::CompLeqI8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqI16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqU16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqI32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqU32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqI64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqU64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqF32 => {
            let word2: f32 = memory.pop()?;
            let word1: f32 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLeqF64 => {
            let word2: f64 = memory.pop()?;
            let word1: f64 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }

        Opcode::CompLtI8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtI16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtU16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtI32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtU32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtI64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtU64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtF32 => {
            let word2: f32 = memory.pop()?;
            let word1: f32 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompLtF64 => {
            let word2: f64 = memory.pop()?;
            let word1: f64 = memory.pop()?;
            memory.push(if word1 < word2 { 1u8 } else { 0u8 });
        }

        Opcode::CompEq8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(if word1 == word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompEq16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(if word1 == word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompEq32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(if word1 == word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompEq64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(if word1 == word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompEqF32 => {
            let word2: f32 = memory.pop()?;
            let word1: f32 = memory.pop()?;
            memory.push(if word1 == word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompEqF64 => {
            let word2: f64 = memory.pop()?;
            let word1: f64 = memory.pop()?;
            memory.push(if word1 == word2 { 1u8 } else { 0u8 });
        }

        Opcode::CompNeq8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(if word1 != word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompNeq16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(if word1 != word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompNeq32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(if word1 != word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompNeq64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(if word1 != word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompNeqF32 => {
            let word2: f32 = memory.pop()?;
            let word1: f32 = memory.pop()?;
            memory.push(if word1 != word2 { 1u8 } else { 0u8 });
        }
        Opcode::CompNeqF64 => {
            let word2: f64 = memory.pop()?;
            let word1: f64 = memory.pop()?;
            memory.push(if word1 != word2 { 1u8 } else { 0u8 });
        }

        Opcode::Add8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1.wrapping_add(word2));
        }
        Opcode::Add16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;

            memory.push(word1.wrapping_add(word2));
        }
        Opcode::Add32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1.wrapping_add(word2));
        }
        Opcode::Add64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1.wrapping_add(word2));
        }
        Opcode::AddF32 => {
            let word2: f32 = memory.pop()?;
            let word1: f32 = memory.pop()?;
            memory.push(word1 + word2);
        }
        Opcode::AddF64 => {
            let word2: f64 = memory.pop()?;
            let word1: f64 = memory.pop()?;
            memory.push(word1 + word2);
        }

        Opcode::SubI8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubI16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubU16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubI32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubU32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubI64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubU64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1.wrapping_sub(word2));
        }
        Opcode::SubF32 => {
            let word2: f32 = memory.pop()?;
            let word1: f32 = memory.pop()?;
            memory.push(word1 - word2);
        }
        Opcode::SubF64 => {
            let word2: f64 = memory.pop()?;
            let word1: f64 = memory.pop()?;
            memory.push(word1 - word2);
        }

        Opcode::MulU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulI8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulI16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulU16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulU32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulI32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulI64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulU64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1.wrapping_mul(word2));
        }
        Opcode::MulF32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1 * word2);
        }
        Opcode::MulF64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 * word2);
        }

        Opcode::DivU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivI8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivI16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivU16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivU32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivI32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivI64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivU64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1.wrapping_div(word2));
        }
        Opcode::DivF32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1 / word2);
        }
        Opcode::DivF64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 / word2);
        }

        Opcode::ModU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModI8 => {
            let word2: i8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModI16 => {
            let word2: i16 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModU16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModU32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModI32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModI64 => {
            let word2: i64 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModU64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModF32 => {
            let word2: i32 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1 % word2);
        }
        Opcode::ModF64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 % word2);
        }

        Opcode::RShiftI8 => {
            let word2: u8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftI16 => {
            let word2: u8 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftU16 => {
            let word2: u8 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftI32 => {
            let word2: u8 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftU32 => {
            let word2: u8 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftI64 => {
            let word2: u8 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }
        Opcode::RShiftU64 => {
            let word2: u8 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1.wrapping_shr(word2 as u32));
        }

        Opcode::LShiftI8 => {
            let word2: u8 = memory.pop()?;
            let word1: i8 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftU8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftI16 => {
            let word2: u8 = memory.pop()?;
            let word1: i16 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftU16 => {
            let word2: u8 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftI32 => {
            let word2: u8 = memory.pop()?;
            let word1: i32 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftU32 => {
            let word2: u8 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftI64 => {
            let word2: u8 = memory.pop()?;
            let word1: i64 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }
        Opcode::LShiftU64 => {
            let word2: u8 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1.wrapping_shl(word2 as u32));
        }

        Opcode::BitAnd8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1 & word2);
        }
        Opcode::BitAnd16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1 & word2);
        }
        Opcode::BitAnd32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1 & word2);
        }
        Opcode::BitAnd64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 & word2);
        }

        Opcode::BitOr8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1 | word2);
        }
        Opcode::BitOr16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1 | word2);
        }
        Opcode::BitOr32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1 | word2);
        }
        Opcode::BitOr64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 | word2);
        }

        Opcode::BitXor8 => {
            let word2: u8 = memory.pop()?;
            let word1: u8 = memory.pop()?;
            memory.push(word1 ^ word2);
        }
        Opcode::BitXor16 => {
            let word2: u16 = memory.pop()?;
            let word1: u16 = memory.pop()?;
            memory.push(word1 ^ word2);
        }
        Opcode::BitXor32 => {
            let word2: u32 = memory.pop()?;
            let word1: u32 = memory.pop()?;
            memory.push(word1 ^ word2);
        }
        Opcode::BitXor64 => {
            let word2: u64 = memory.pop()?;
            let word1: u64 = memory.pop()?;
            memory.push(word1 ^ word2);
        }

        Opcode::BitNot8 => {
            let word: u8 = memory.pop()?;
            memory.push(!word);
        }
        Opcode::BitNot16 => {
            let word: u16 = memory.pop()?;
            memory.push(!word);
        }
        Opcode::BitNot32 => {
            let word: u32 = memory.pop()?;
            memory.push(!word);
        }
        Opcode::BitNot64 => {
            let word: u64 = memory.pop()?;
            memory.push(!word);
        }

        Opcode::Jump => {
            let target = memory.read_pc()?;
            memory.jump(target);
        }

        Opcode::JumpIfZero8 => {
            let target = memory.read_pc()?;
            let value: u8 = memory.pop()?;
            if value == 0 {
                memory.jump(target);
            }
        }
        Opcode::JumpIfZero16 => {
            let target = memory.read_pc()?;
            let value: u16 = memory.pop()?;
            if value == 0 {
                memory.jump(target);
            }
        }
        Opcode::JumpIfZero32 => {
            let target = memory.read_pc()?;
            let value: u32 = memory.pop()?;
            if value == 0 {
                memory.jump(target);
            }
        }
        Opcode::JumpIfZero64 => {
            let target = memory.read_pc()?;
            let value: u64 = memory.pop()?;
            if value == 0 {
                memory.jump(target);
            }
        }

        Opcode::JumpIfNotZero8 => {
            let target = memory.read_pc()?;
            let value: u8 = memory.pop()?;
            if value != 0 {
                memory.jump(target);
            }
        }
        Opcode::JumpIfNotZero16 => {
            let target = memory.read_pc()?;
            let value: u16 = memory.pop()?;
            if value != 0 {
                memory.jump(target);
            }
        }
        Opcode::JumpIfNotZero32 => {
            let target = memory.read_pc()?;
            let value: u32 = memory.pop()?;
            if value != 0 {
                memory.jump(target);
            }
        }
        Opcode::JumpIfNotZero64 => {
            let target = memory.read_pc()?;
            let value: u64 = memory.pop()?;
            if value != 0 {
                memory.jump(target);
            }
        }

        Opcode::Ret => {
            memory.ret()?;
        }
        Opcode::Call => {
            let func: VarPointer = memory.pop()?;
            memory.call(func)?;
        }

        Opcode::Throw => {
            let skip_frames: u32 = memory.pop()?;
            let message_ptr: VarPointer = memory.pop()?;
            let name_ptr: VarPointer = memory.pop()?;

            let message_bytes = memory.cstring_bytes(message_ptr)?;
            let name_bytes = memory.cstring_bytes(name_ptr)?;

            let mut message = String::new();
            string_append_utf8_lossy(&mut message, message_bytes);
            let mut name = String::new();
            string_append_utf8_lossy(&mut name, name_bytes);

            for _ in 0..skip_frames {
                memory.ret()?;
            }

            return Err(IError::new(name, message));
        }

        Opcode::AllocBegin => {
            let var_pointer: VarPointer = memory.pop()?;
            if memory.read::<u8>(var_pointer).is_ok() {
                memory.push(var_pointer.with_offset(0));
            } else {
                memory.push(0 as u64);
            }
        }
        Opcode::AllocEnd => {
            let var_pointer: VarPointer = memory.pop()?;
            if let Some(ptr) = memory.upper_bound(var_pointer) {
                memory.push(ptr);
            } else {
                memory.push(0 as u64);
            }
        }

        Opcode::HeapAlloc => {
            let skip: u32 = memory.pop()?;
            let size: u64 = memory.pop()?;
            let ptr = memory.add_heap_var(size as u32, skip)?;
            memory.push(ptr);
        }
        Opcode::HeapDealloc => {
            let skip: u32 = memory.pop()?;
            let ptr: VarPointer = memory.pop()?;
            memory.free(ptr, skip)?;
            memory.push(0u64);
        }

        Opcode::Ecall => match memory.pop()? {
            Ecall::Exit => {
                let exit: i32 = memory.pop()?;
                return Ok(Some(EcallExt::Exit(exit)));
            }

            Ecall::OpenFd => {
                let open_mode: OpenMode = memory.pop()?;
                let name: VarPointer = memory.pop()?;
                let name = memory.cstring_bytes(name)?.to_vec();
                match String::from_utf8(name) {
                    Ok(name) => return Ok(Some(EcallExt::OpenFd { name, open_mode })),
                    Err(e) => return Ok(Some(EcallExt::Error(EcallError::NameNotUTF8))),
                }
            }
            Ecall::ReadFd => {
                let len: u32 = memory.pop()?;
                let buf: VarPointer = memory.pop()?;
                let begin: u32 = memory.pop()?;
                let fd: u32 = memory.pop()?;

                #[rustfmt::skip]
                return Ok(Some(EcallExt::ReadFd { len, buf, begin, fd, }));
            }
            Ecall::WriteFd => {
                let len: u32 = memory.pop()?;
                let buf: VarPointer = memory.pop()?;
                let begin: u32 = memory.pop()?;
                let fd: u32 = memory.pop()?;
                let buf = memory.read_bytes(buf, len)?.to_vec();

                return Ok(Some(EcallExt::WriteFd { buf, begin, fd }));
            }
            Ecall::AppendFd => {
                let len: u32 = memory.pop()?;
                let buf: VarPointer = memory.pop()?;
                let fd: u32 = memory.pop()?;
                let buf = memory.read_bytes(buf, len)?.to_vec();

                return Ok(Some(EcallExt::AppendFd { buf, fd }));
            }

            call => {
                return ierr!(
                    "InvalidEnviromentCall",
                    "invalid ecall value of {}",
                    call as u32
                )
            }
        },

        Opcode::AssertStr => {
            let string = memory.pop()?;
            memory.cstring_bytes(string)?;
        }
    }

    return Ok(None);
}
