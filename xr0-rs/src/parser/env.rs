use std::cell::RefCell;
use std::collections::HashSet;
use std::ffi::CStr;

use libc::{exit, fprintf, free, isdigit, isspace, malloc};

use crate::c_util::__stderrp;
use crate::util::{
    dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf, strbuilder_putc,
    StrBuilder,
};

pub struct Env {
    pub reserved: HashSet<&'static str>,
    pub typenames: RefCell<HashSet<Vec<u8>>>,
}

static RESERVED: &[&str] = &[
    "auto", "axiom", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "lemma", "long", "register",
    "return", "short", "signed", "sizeof", "some", "static", "sfunc", "struct", "switch",
    "typedef", "union", "unsigned", "void", "volatile", "while",
];

impl Env {
    pub fn new() -> Self {
        Env {
            reserved: RESERVED.iter().copied().collect(),
            typenames: RefCell::new(HashSet::new()),
        }
    }

    pub unsafe fn lexloc(&self, _p: usize) -> *mut LexemeMarker {
        lexloc()
    }

    pub unsafe fn add_typename(&self, name: *const libc::c_char) {
        let name = CStr::from_ptr(name);
        let mut typenames = self.typenames.borrow_mut();
        typenames.insert(name.to_bytes().to_vec());
    }

    pub unsafe fn is_typename(&self, name: *const libc::c_char) -> bool {
        let name = CStr::from_ptr(name);
        let typenames = self.typenames.borrow();
        typenames.contains(name.to_bytes())
    }

    pub unsafe fn newline(&self) {
        marker.linenum += 1;
    }

    pub unsafe fn directive(&self, d: &str) {
        let mut iter = d.bytes().map(|b| b as libc::c_char);
        assert_eq!(iter.next(), Some(b'#' as libc::c_char));
        marker = process_linemarker(iter);
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct LexemeMarker {
    pub linenum: libc::c_int,
    pub column: libc::c_int,
    pub filename: *mut libc::c_char,
    pub flags: LineMarkerFlag,
}
pub type LineMarkerFlag = libc::c_uint;
pub const LM_FLAG_IMPLICIT_EXTERN: LineMarkerFlag = 8;
pub const LM_FLAG_SYS_HEADER: LineMarkerFlag = 4;
pub const LM_FLAG_RESUME_FILE: LineMarkerFlag = 2;
pub const LM_FLAG_NEW_FILE: LineMarkerFlag = 1;

#[allow(non_upper_case_globals)]
pub static mut marker: LexemeMarker = LexemeMarker {
    linenum: 0,
    column: 0,
    filename: 0 as *const libc::c_char as *mut libc::c_char,
    flags: 0 as LineMarkerFlag,
};

pub unsafe fn lexloc() -> *mut LexemeMarker {
    if (marker.filename).is_null() as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    return lexememarker_copy(std::ptr::addr_of_mut!(marker));
}

pub unsafe fn lexememarker_create(
    linenum: libc::c_int,
    column: libc::c_int,
    filename: *mut libc::c_char,
    flags: LineMarkerFlag,
) -> *mut LexemeMarker {
    let loc = malloc(::core::mem::size_of::<LexemeMarker>()) as *mut LexemeMarker;
    (*loc).linenum = linenum;
    (*loc).column = column;
    (*loc).filename = filename;
    (*loc).flags = flags;
    return loc;
}

pub unsafe fn lexememarker_copy(loc: *mut LexemeMarker) -> *mut LexemeMarker {
    return lexememarker_create(
        (*loc).linenum,
        (*loc).column,
        dynamic_str((*loc).filename),
        (*loc).flags,
    );
}

pub unsafe fn lexememarker_destroy(loc: *mut LexemeMarker) {
    free((*loc).filename as *mut libc::c_void);
    free(loc as *mut libc::c_void);
}

pub unsafe fn lexememarker_str(loc: *mut LexemeMarker) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%s:%d:%d\0" as *const u8 as *const libc::c_char,
        (*loc).filename,
        (*loc).linenum,
        (*loc).column,
    );
    return strbuilder_build(b);
}

pub unsafe fn process_linemarker(mut bytes: impl Iterator<Item = libc::c_char>) -> LexemeMarker {
    let mut c: libc::c_char = bytes.next().unwrap_or(0);
    if isspace(c as libc::c_int) == 0 {
        fprintf(
            __stderrp,
            b"expected space before line number\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    while isspace(c as libc::c_int) != 0 {
        c = bytes.next().unwrap_or(0);
    }
    if isdigit(c as libc::c_int) == 0 {
        fprintf(
            __stderrp,
            b"expected line number in line marker\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    let mut linenum: libc::c_int = 0 as libc::c_int;
    while isdigit(c as libc::c_int) != 0 {
        linenum *= 10 as libc::c_int;
        linenum += c as libc::c_int - '0' as i32;
        c = bytes.next().unwrap_or(0);
    }
    if isspace(c as libc::c_int) == 0 {
        fprintf(
            __stderrp,
            b"expected space before file name\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    while isspace(c as libc::c_int) != 0 {
        c = bytes.next().unwrap_or(0);
    }
    if c as libc::c_int != '"' as i32 {
        fprintf(
            __stderrp,
            b"file name must begin with opening quote\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    let b: *mut StrBuilder = strbuilder_create();
    loop {
        c = bytes.next().unwrap_or(0);
        if !(c as libc::c_int != '"' as i32) {
            break;
        }
        strbuilder_putc(b, c);
    }
    let name: *mut libc::c_char = strbuilder_build(b);
    let mut flags: libc::c_int = 0 as libc::c_int;
    c = bytes.next().unwrap_or(0);
    if isspace(c as libc::c_int) != 0 {
        while c != 0 {
            if isdigit(c as libc::c_int) != 0 {
                match c as libc::c_int {
                    49 => {
                        flags |= LM_FLAG_NEW_FILE as libc::c_int;
                    }
                    50 => {
                        flags |= LM_FLAG_RESUME_FILE as libc::c_int;
                    }
                    51 => {
                        flags |= LM_FLAG_SYS_HEADER as libc::c_int;
                    }
                    52 => {
                        flags |= LM_FLAG_IMPLICIT_EXTERN as libc::c_int;
                    }
                    _ => {
                        fprintf(
                            __stderrp,
                            b"invalid flag `%c'\n\0" as *const u8 as *const libc::c_char,
                            c as libc::c_int,
                        );
                        exit(1 as libc::c_int);
                    }
                }
            }
            c = bytes.next().unwrap_or(0);
        }
    }
    LexemeMarker {
        linenum,
        column: 0 as libc::c_int,
        filename: name,
        flags: flags as LineMarkerFlag,
    }
}
