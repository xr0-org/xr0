#![allow(non_camel_case_types, dead_code)]

use libc::{free, malloc};

use crate::ast::ast;
use crate::c_util::__assert_rtn;
use crate::util::{
    dynamic_str, strbuilder, strbuilder_build, strbuilder_create, strbuilder_printf,
};

mod inner;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct lexememarker {
    pub linenum: libc::c_int,
    pub column: libc::c_int,
    pub filename: *mut libc::c_char,
    pub flags: linemarker_flag,
}
pub type linemarker_flag = libc::c_uint;
pub const LM_FLAG_IMPLICIT_EXTERN: linemarker_flag = 8;
pub const LM_FLAG_SYS_HEADER: linemarker_flag = 4;
pub const LM_FLAG_RESUME_FILE: linemarker_flag = 2;
pub const LM_FLAG_NEW_FILE: linemarker_flag = 1;

#[no_mangle]
pub static mut marker: lexememarker = lexememarker {
    linenum: 0,
    column: 0,
    filename: 0 as *const libc::c_char as *mut libc::c_char,
    flags: 0 as linemarker_flag,
};
#[no_mangle]
pub unsafe fn lexloc() -> *mut lexememarker {
    if (marker.filename).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 7], &[libc::c_char; 7]>(b"lexloc\0")).as_ptr(),
            b"lex.l\0" as *const u8 as *const libc::c_char,
            171 as libc::c_int,
            b"marker.filename\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return lexememarker_copy(std::ptr::addr_of_mut!(marker));
}
#[no_mangle]
pub unsafe fn lexememarker_create(
    linenum: libc::c_int,
    column: libc::c_int,
    filename: *mut libc::c_char,
    flags: linemarker_flag,
) -> *mut lexememarker {
    let loc = malloc(::core::mem::size_of::<lexememarker>()) as *mut lexememarker;
    (*loc).linenum = linenum;
    (*loc).column = column;
    (*loc).filename = filename;
    (*loc).flags = flags;
    return loc;
}
#[no_mangle]
pub unsafe fn lexememarker_copy(mut loc: *mut lexememarker) -> *mut lexememarker {
    return lexememarker_create(
        (*loc).linenum,
        (*loc).column,
        dynamic_str((*loc).filename),
        (*loc).flags,
    );
}
#[no_mangle]
pub unsafe fn lexememarker_destroy(mut loc: *mut lexememarker) {
    free((*loc).filename as *mut libc::c_void);
    free(loc as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn lexememarker_str(mut loc: *mut lexememarker) -> *mut libc::c_char {
    let b: *mut strbuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%s:%d:%d\0" as *const u8 as *const libc::c_char,
        (*loc).filename,
        (*loc).linenum,
        (*loc).column,
    );
    return strbuilder_build(b);
}

static RESERVED: &[&str] = &[
    "auto", "axiom", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "lemma", "long", "register",
    "return", "short", "signed", "sizeof", "some", "static", "sfunc", "struct", "switch",
    "typedef", "union", "unsigned", "void", "volatile", "while",
];

pub fn parse_translation_unit(
    source: &str,
) -> Result<*mut ast, peg::error::ParseError<peg::str::LineCol>> {
    let env = inner::Env {
        reserved: RESERVED.iter().copied().collect(),
    };
    inner::c_parser::translation_unit(source, &env)
}
