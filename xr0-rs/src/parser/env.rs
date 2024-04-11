use std::cell::RefCell;
use std::collections::HashSet;
use std::ffi::CStr;

use libc::{exit, fprintf, free, isdigit, isspace, malloc};

use crate::c_util::{__assert_rtn, __stderrp};
use crate::util::{
    dynamic_str, strbuilder, strbuilder_build, strbuilder_create, strbuilder_printf,
    strbuilder_putc,
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

    pub unsafe fn lexloc(&self, _p: usize) -> *mut lexememarker {
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
pub unsafe fn lexememarker_copy(loc: *mut lexememarker) -> *mut lexememarker {
    return lexememarker_create(
        (*loc).linenum,
        (*loc).column,
        dynamic_str((*loc).filename),
        (*loc).flags,
    );
}
#[no_mangle]
pub unsafe fn lexememarker_destroy(loc: *mut lexememarker) {
    free((*loc).filename as *mut libc::c_void);
    free(loc as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn lexememarker_str(loc: *mut lexememarker) -> *mut libc::c_char {
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

pub unsafe fn process_linemarker(mut bytes: impl Iterator<Item = libc::c_char>) -> lexememarker {
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
    let b: *mut strbuilder = strbuilder_create();
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
    lexememarker {
        linenum,
        column: 0 as libc::c_int,
        filename: name,
        flags: flags as linemarker_flag,
    }
}
