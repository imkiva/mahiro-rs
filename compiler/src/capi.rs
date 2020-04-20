use std::panic::catch_unwind;
use std::ffi::CStr;
use libc::c_char;
use libc::c_uchar;
use libc::size_t;

use crate::parse::{CsParser, CompileError};

#[repr(C)]
pub enum CCompileResultKind {
    Success,
    CompileError,
    MallocError,
    InternalError,
}

#[repr(C)]
pub struct CCompileResult {
    /// indicate compile result variant
    kind: CCompileResultKind,

    /// store the output message (nullable).
    /// this is malloc-ed, the caller is
    /// responsible to call free() on this.
    message: *const c_char,

    /// store the compiled bytecode.
    /// this is malloc-ed, the caller is
    /// responsible to call free() on this.
    compiled_code: *const c_uchar,

    /// the size of the compiled code.
    compiled_code_length: size_t,
}

#[allow(dead_code)]
impl CCompileResult {
    fn from_string(str: &str, kind: CCompileResultKind) -> Self {
        let len = str.len();
        // '\0' takes 1 byte
        let buffer = malloc(len + 1);

        if buffer.is_null() {
            return CCompileResult {
                kind: CCompileResultKind::MallocError,
                message: std::ptr::null(),
                compiled_code: std::ptr::null(),
                compiled_code_length: 0,
            };
        }

        unsafe { std::ptr::copy(str.as_ptr() as *const c_char, buffer, len) }

        CCompileResult {
            kind,
            message: buffer,
            compiled_code: std::ptr::null(),
            compiled_code_length: 0,
        }
    }

    pub fn from_compile_error(file: &str, err: CompileError) -> CCompileResult {
        let err = err.0;
        CCompileResult::from_string(&format!("{}", err.with_path(file)),
                                    CCompileResultKind::CompileError)
    }

    pub fn from_code(code: &[u8]) -> CCompileResult {
        let len = code.len();
        let buffer = malloc(len) as *mut c_uchar;

        if buffer.is_null() {
            return CCompileResult {
                kind: CCompileResultKind::MallocError,
                message: std::ptr::null(),
                compiled_code: std::ptr::null(),
                compiled_code_length: 0,
            };
        }

        unsafe { std::ptr::copy(code.as_ptr() as *const c_uchar, buffer, len) }

        CCompileResult {
            kind: CCompileResultKind::Success,
            message: std::ptr::null(),
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
        let _file = to_rust_str(file);
        let _src = to_rust_str(src);
        // TODO: codegen
        CCompileResult::from_string("unimplemented",
                                    CCompileResultKind::InternalError)
    });

    match result {
        Ok(r) => r,
        Err(e) => CCompileResult::from_string(
            &format!("{:?}", e),
            CCompileResultKind::InternalError),
    }
}

#[no_mangle]
pub extern "C" fn compile_to_ast(file: *const c_char, src: *const c_char) -> CCompileResult {
    let result = catch_unwind(|| {
        let file = to_rust_str(file);
        let src = to_rust_str(src);
        match CsParser::ast(src) {
            Ok(tree) => CCompileResult::from_string(
                &format!("{:#?}", tree),
                CCompileResultKind::Success,
            ),
            Err(err) => CCompileResult::from_compile_error(file, err),
        }
    });

    match result {
        Ok(r) => r,
        Err(e) => CCompileResult::from_string(
            &format!("{:?}", e),
            CCompileResultKind::InternalError),
    }
}
