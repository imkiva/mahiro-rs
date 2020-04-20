use std::panic::catch_unwind;
use std::ffi::CStr;
use libc::c_char;
use libc::c_uchar;
use libc::size_t;

use crate::parse::{CsParser, CompileError};
use crate::parse::CompileResult;
use crate::parse::ParseError;

const ERROR_KIND_NO_ERROR: i32 = 0;
const ERROR_KIND_PARSE: i32 = 1;
const ERROR_KIND_MALLOC: i32 = 2;

#[repr(C)]
pub enum CCompileResultKind {
    Success,
    ParseError,
    MallocError,
}

#[repr(C)]
pub struct CCompileResult {
    /// indicate compile result variant
    error_occurred: CCompileResultKind,

    /// store the error message.
    /// this is malloc-ed, the caller is
    /// responsible to call free() on this.
    error_message: *const c_char,

    /// store the compiled bytecode.
    /// this is malloc-ed, the caller is
    /// responsible to call free() on this.
    compiled_code: *const c_uchar,

    /// the size of the compiled code.
    compiled_code_length: size_t,
}

impl CCompileResult {
    pub fn from_error(msg: &str) -> Self {
        let len = msg.len();
        // '\0' takes 1 byte
        let buffer = malloc(len + 1);

        if buffer.is_null() {
            return CCompileResult {
                error_occurred: CCompileResultKind::MallocError,
                error_message: std::ptr::null(),
                compiled_code: std::ptr::null(),
                compiled_code_length: 0,
            };
        }

        unsafe { std::ptr::copy(msg.as_ptr() as *const c_char, buffer, len) }

        CCompileResult {
            error_occurred: CCompileResultKind::ParseError,
            error_message: buffer,
            compiled_code: std::ptr::null(),
            compiled_code_length: 0,
        }
    }

    pub fn from_compile_error(file: &str, err: CompileError) -> CCompileResult {
        let err = err.0;
        CCompileResult::from_error(&format!("{}", err.with_path(file)))
    }

    pub fn from_code(code: &[u8]) -> CCompileResult {
        let len = code.len();
        let buffer = malloc(len) as *mut c_uchar;

        if buffer.is_null() {
            return CCompileResult {
                error_occurred: CCompileResultKind::MallocError,
                error_message: std::ptr::null(),
                compiled_code: std::ptr::null(),
                compiled_code_length: 0,
            };
        }

        unsafe { std::ptr::copy(code.as_ptr() as *const c_uchar, buffer, len) }

        CCompileResult {
            error_occurred: CCompileResultKind::Success,
            error_message: std::ptr::null(),
            compiled_code: buffer,
            compiled_code_length: len,
        }
    }
}

fn malloc(size: size_t) -> *mut c_char {
    unsafe {
        let buffer = libc::malloc(size);
        libc::memset(buffer, 0, size);
        buffer as *mut c_char
    }
}

fn to_rust_str<'a>(cstr: *const c_char) -> &'a str {
    unsafe { CStr::from_ptr(cstr) }.to_str().unwrap()
}

#[no_mangle]
pub extern "C" fn compile_source(file: *const c_char, src: *const c_char) -> CCompileResult {
    let result = catch_unwind(|| {
        let file = to_rust_str(file);
        let src = to_rust_str(src);
        // TODO: codegen
        CCompileResult::from_error("unimplemented")
    });

    match result {
        Ok(r) => r,
        Err(e) => CCompileResult::from_error(&format!("{:?}", e)),
    }
}
