use std::panic::catch_unwind;
use libc::c_char;
use libc::c_uchar;
use libc::size_t;

use crate::parse::CsParser;
use crate::parse::CompileResult;
use crate::parse::ParseError;

#[repr(C)]
struct CCompileResult {
    error_occurred: i32,
    error_message: *const c_char,
    compiled_code: *const c_uchar,
    compiled_code_length: size_t,
}

#[no_mangle]
pub extern "C" fn compile_source(src: *const c_char) -> CCompileResult {
    let result = catch_unwind(|| {
    });
}
