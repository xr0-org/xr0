#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use std::ptr::addr_of_mut;

// NOTE: fgetc may be slow.
use libc::{
    clearerr, exit, ferror, fgetc, fileno, fprintf, fread, free, fwrite, isatty, malloc, memset,
    putchar, realloc, strlen, FILE,
};

use crate::util::{
    dynamic_str, map, map_create, map_destroy, map_get, map_set, strbuilder_build,
    strbuilder_create, strbuilder_printf, strbuilder_putc,
};
use crate::StrBuilder;

use crate::c_util::{
    _DefaultRuneLocale, __assert_rtn, __darwin_ct_rune_t, __error, __maskrune, __stderrp, __stdinp,
    __stdoutp, size_t,
};

type uint8_t = u8;
type int16_t = i16;

pub type flex_uint8_t = uint8_t;
pub type flex_int16_t = int16_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct yy_buffer_state {
    pub yy_input_file: *mut FILE,
    pub yy_ch_buf: *mut libc::c_char,
    pub yy_buf_pos: *mut libc::c_char,
    pub yy_buf_size: libc::c_int,
    pub yy_n_chars: yy_size_t,
    pub yy_is_our_buffer: libc::c_int,
    pub yy_is_interactive: libc::c_int,
    pub yy_at_bol: libc::c_int,
    pub yy_bs_lineno: libc::c_int,
    pub yy_bs_column: libc::c_int,
    pub yy_fill_buffer: libc::c_int,
    pub yy_buffer_status: libc::c_int,
}
pub type yy_size_t = size_t;
pub type YY_BUFFER_STATE = *mut yy_buffer_state;
pub type YY_CHAR = flex_uint8_t;
pub type yy_state_type = libc::c_int;

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

pub type ictype = libc::c_uint;
pub const IC_PRED: ictype = 8;
pub const IC_TYPE: ictype = 2;
pub const IC_NONE: ictype = 1;

#[inline]
unsafe extern "C" fn isascii(mut _c: libc::c_int) -> libc::c_int {
    return (_c & !(0x7f as libc::c_int) == 0 as libc::c_int) as libc::c_int;
}

#[inline]
unsafe extern "C" fn __istype(mut _c: __darwin_ct_rune_t, mut _f: libc::c_ulong) -> libc::c_int {
    return if isascii(_c) != 0 {
        (_DefaultRuneLocale.__runetype[_c as usize] as libc::c_ulong & _f != 0) as libc::c_int
    } else {
        (__maskrune(_c, _f) != 0) as libc::c_int
    };
}
#[inline]
unsafe extern "C" fn __isctype(
    mut _c: __darwin_ct_rune_t,
    mut _f: libc::c_ulong,
) -> __darwin_ct_rune_t {
    return if _c < 0 as libc::c_int || _c >= (1 as libc::c_int) << 8 as libc::c_int {
        0 as libc::c_int
    } else {
        (_DefaultRuneLocale.__runetype[_c as usize] as libc::c_ulong & _f != 0) as libc::c_int
    };
}
#[no_mangle]
#[inline]
#[linkage = "external"]
pub unsafe extern "C" fn isdigit(mut _c: libc::c_int) -> libc::c_int {
    return __isctype(_c, 0x400 as libc::c_long as libc::c_ulong);
}
#[no_mangle]
#[inline]
#[linkage = "external"]
pub unsafe extern "C" fn isspace(mut _c: libc::c_int) -> libc::c_int {
    return __istype(_c, 0x4000 as libc::c_long as libc::c_ulong);
}
#[no_mangle]
pub unsafe extern "C" fn yywrap() -> libc::c_int {
    return 1 as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn comment() {
    let mut c: libc::c_char = 0;
    let mut c1: libc::c_char = 0;
    loop {
        c = input() as libc::c_char;
        if !(c as libc::c_int != '*' as i32 && c as libc::c_int != -(1 as libc::c_int)) {
            break;
        }
        putchar(c as libc::c_int);
    }
    c1 = input() as libc::c_char;
    if c1 as libc::c_int != '/' as i32 && c as libc::c_int != -(1 as libc::c_int) {
        yyunput(c1 as libc::c_int, yytext);
        return comment();
    }
    if c as libc::c_int != -(1 as libc::c_int) {
        putchar(c1 as libc::c_int);
    }
}

#[no_mangle]
pub static mut marker: lexememarker = lexememarker {
    linenum: 0,
    column: 0,
    filename: 0 as *const libc::c_char as *mut libc::c_char,
    flags: 0 as linemarker_flag,
};

#[no_mangle]
pub unsafe extern "C" fn lexloc() -> *mut lexememarker {
    if (marker.filename).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 7], &[libc::c_char; 7]>(b"lexloc\0")).as_ptr(),
            b"lex.l\0" as *const u8 as *const libc::c_char,
            172 as libc::c_int,
            b"marker.filename\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return lexememarker_copy(addr_of_mut!(marker));
}

#[no_mangle]
pub unsafe extern "C" fn process_linemarker() -> lexememarker {
    let mut c: libc::c_char = input() as libc::c_char;
    if isspace(c as libc::c_int) == 0 {
        fprintf(
            __stderrp,
            b"expected space before line number\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    while isspace(c as libc::c_int) != 0 {
        c = input() as libc::c_char;
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
        c = input() as libc::c_char;
    }
    if isspace(c as libc::c_int) == 0 {
        fprintf(
            __stderrp,
            b"expected space before file name\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    while isspace(c as libc::c_int) != 0 {
        c = input() as libc::c_char;
    }
    if c as libc::c_int != '"' as i32 {
        fprintf(
            __stderrp,
            b"file name must begin with opening quote\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    let mut b: *mut StrBuilder = strbuilder_create();
    loop {
        c = input() as libc::c_char;
        if !(c as libc::c_int != '"' as i32) {
            break;
        }
        strbuilder_putc(b, c);
    }
    let mut name: *mut libc::c_char = strbuilder_build(b);
    c = input() as libc::c_char;
    if isspace(c as libc::c_int) == 0 {
        fprintf(
            __stderrp,
            b"expected space before flags\n\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    let mut flags: libc::c_int = 0 as libc::c_int;
    while c as libc::c_int != '\n' as i32 {
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
        c = input() as libc::c_char;
    }
    return {
        let mut init = lexememarker {
            linenum: linenum,
            column: 0 as libc::c_int,
            filename: name,
            flags: flags as linemarker_flag,
        };
        init
    };
}
static mut yy_buffer_stack_top: size_t = 0 as libc::c_int as size_t;
static mut yy_buffer_stack_max: size_t = 0 as libc::c_int as size_t;
static mut yy_buffer_stack: *mut YY_BUFFER_STATE =
    0 as *const YY_BUFFER_STATE as *mut YY_BUFFER_STATE;
#[no_mangle]
pub unsafe extern "C" fn preproc() {
    if !(marker.filename).is_null() {
        free(marker.filename as *mut libc::c_void);
    }
    marker = process_linemarker();
}
#[no_mangle]
pub unsafe extern "C" fn count() {
    let mut s: *mut libc::c_char = yytext;
    while *s != 0 {
        match *s as libc::c_int {
            10 => {
                marker.column = 0 as libc::c_int;
                marker.linenum += 1;
                marker.linenum;
            }
            9 => {
                marker.column += 8 as libc::c_int - marker.column % 8 as libc::c_int;
            }
            _ => {
                marker.column += 1;
                marker.column;
            }
        }
        s = s.offset(1);
    }
}
static mut yy_hold_char: libc::c_char = 0;
static mut yy_n_chars: yy_size_t = 0;
#[no_mangle]
pub static mut yyleng: yy_size_t = 0;
static mut yy_c_buf_p: *mut libc::c_char = 0 as *const libc::c_char as *mut libc::c_char;
static mut yy_init: libc::c_int = 0 as libc::c_int;
static mut yy_start: libc::c_int = 0 as libc::c_int;
static mut yy_did_buffer_switch_on_eof: libc::c_int = 0;
#[no_mangle]
pub unsafe extern "C" fn check_type() -> libc::c_long {
    if !(map_get(table, yytext)).is_null() {
        return 285 as libc::c_int as libc::c_long;
    }
    match installclass as libc::c_uint {
        2 => {
            map_set(
                table,
                dynamic_str(yytext),
                1 as libc::c_int as *mut libc::c_void,
            );
            installclass = IC_NONE;
        }
        _ => {}
    }
    return 258 as libc::c_int as libc::c_long;
}
#[no_mangle]
pub static mut table: *mut map = 0 as *const map as *mut map;
#[no_mangle]
pub static mut installclass: ictype = 0 as ictype;
#[no_mangle]
pub unsafe extern "C" fn lex_begin() {
    table = map_create();
    installclass = IC_NONE;
}
#[no_mangle]
pub unsafe extern "C" fn lex_finish() {
    if !(marker.filename).is_null() {
        free(marker.filename as *mut libc::c_void);
    }
    map_destroy(table);
}
#[no_mangle]
pub unsafe extern "C" fn lexememarker_create(
    mut linenum: libc::c_int,
    mut column: libc::c_int,
    mut filename: *mut libc::c_char,
    mut flags: linemarker_flag,
) -> *mut lexememarker {
    let mut loc: *mut lexememarker =
        malloc(::core::mem::size_of::<lexememarker>()) as *mut lexememarker;
    (*loc).linenum = linenum;
    (*loc).column = column;
    (*loc).filename = filename;
    (*loc).flags = flags;
    return loc;
}
#[no_mangle]
pub static mut yyin: *mut FILE = 0 as *const FILE as *mut FILE;
#[no_mangle]
pub static mut yyout: *mut FILE = 0 as *const FILE as *mut FILE;
#[no_mangle]
pub unsafe extern "C" fn lexememarker_copy(mut loc: *mut lexememarker) -> *mut lexememarker {
    return lexememarker_create(
        (*loc).linenum,
        (*loc).column,
        dynamic_str((*loc).filename),
        (*loc).flags,
    );
}
#[no_mangle]
pub static mut yylineno: libc::c_int = 1 as libc::c_int;
#[no_mangle]
pub unsafe extern "C" fn lexememarker_destroy(mut loc: *mut lexememarker) {
    free((*loc).filename as *mut libc::c_void);
    free(loc as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn lexememarker_str(mut loc: *mut lexememarker) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%s:%d:%d\0" as *const u8 as *const libc::c_char,
        (*loc).filename,
        (*loc).linenum,
        (*loc).column,
    );
    return strbuilder_build(b);
}
static mut yy_accept: [flex_int16_t; 275] = [
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    102 as libc::c_int as flex_int16_t,
    100 as libc::c_int as flex_int16_t,
    99 as libc::c_int as flex_int16_t,
    99 as libc::c_int as flex_int16_t,
    85 as libc::c_int as flex_int16_t,
    100 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    98 as libc::c_int as flex_int16_t,
    91 as libc::c_int as flex_int16_t,
    84 as libc::c_int as flex_int16_t,
    100 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    89 as libc::c_int as flex_int16_t,
    88 as libc::c_int as flex_int16_t,
    76 as libc::c_int as flex_int16_t,
    87 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    90 as libc::c_int as flex_int16_t,
    45 as libc::c_int as flex_int16_t,
    45 as libc::c_int as flex_int16_t,
    77 as libc::c_int as flex_int16_t,
    73 as libc::c_int as flex_int16_t,
    92 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    93 as libc::c_int as flex_int16_t,
    96 as libc::c_int as flex_int16_t,
    97 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    82 as libc::c_int as flex_int16_t,
    94 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    74 as libc::c_int as flex_int16_t,
    95 as libc::c_int as flex_int16_t,
    75 as libc::c_int as flex_int16_t,
    86 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    50 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    58 as libc::c_int as flex_int16_t,
    75 as libc::c_int as flex_int16_t,
    67 as libc::c_int as flex_int16_t,
    59 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    54 as libc::c_int as flex_int16_t,
    65 as libc::c_int as flex_int16_t,
    55 as libc::c_int as flex_int16_t,
    66 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    2 as libc::c_int as flex_int16_t,
    57 as libc::c_int as flex_int16_t,
    49 as libc::c_int as flex_int16_t,
    44 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    45 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    45 as libc::c_int as flex_int16_t,
    82 as libc::c_int as flex_int16_t,
    74 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    69 as libc::c_int as flex_int16_t,
    71 as libc::c_int as flex_int16_t,
    70 as libc::c_int as flex_int16_t,
    62 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    60 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    14 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    61 as libc::c_int as flex_int16_t,
    68 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    51 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    49 as libc::c_int as flex_int16_t,
    44 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    47 as libc::c_int as flex_int16_t,
    43 as libc::c_int as flex_int16_t,
    53 as libc::c_int as flex_int16_t,
    52 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    49 as libc::c_int as flex_int16_t,
    47 as libc::c_int as flex_int16_t,
    43 as libc::c_int as flex_int16_t,
    6 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    9 as libc::c_int as flex_int16_t,
    10 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    16 as libc::c_int as flex_int16_t,
    17 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    21 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    25 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    39 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    4 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    7 as libc::c_int as flex_int16_t,
    8 as libc::c_int as flex_int16_t,
    11 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    19 as libc::c_int as flex_int16_t,
    24 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    33 as libc::c_int as flex_int16_t,
    28 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    37 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    41 as libc::c_int as flex_int16_t,
    5 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    15 as libc::c_int as flex_int16_t,
    18 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    27 as libc::c_int as flex_int16_t,
    29 as libc::c_int as flex_int16_t,
    30 as libc::c_int as flex_int16_t,
    32 as libc::c_int as flex_int16_t,
    34 as libc::c_int as flex_int16_t,
    35 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    3 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    13 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    36 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    12 as libc::c_int as flex_int16_t,
    26 as libc::c_int as flex_int16_t,
    38 as libc::c_int as flex_int16_t,
    40 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
];
static mut yy_ec: [YY_CHAR; 256] = [
    0 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    2 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    2 as libc::c_int as YY_CHAR,
    2 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    2 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    5 as libc::c_int as YY_CHAR,
    6 as libc::c_int as YY_CHAR,
    7 as libc::c_int as YY_CHAR,
    8 as libc::c_int as YY_CHAR,
    9 as libc::c_int as YY_CHAR,
    10 as libc::c_int as YY_CHAR,
    11 as libc::c_int as YY_CHAR,
    12 as libc::c_int as YY_CHAR,
    13 as libc::c_int as YY_CHAR,
    14 as libc::c_int as YY_CHAR,
    15 as libc::c_int as YY_CHAR,
    16 as libc::c_int as YY_CHAR,
    17 as libc::c_int as YY_CHAR,
    18 as libc::c_int as YY_CHAR,
    19 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    20 as libc::c_int as YY_CHAR,
    21 as libc::c_int as YY_CHAR,
    22 as libc::c_int as YY_CHAR,
    23 as libc::c_int as YY_CHAR,
    24 as libc::c_int as YY_CHAR,
    25 as libc::c_int as YY_CHAR,
    26 as libc::c_int as YY_CHAR,
    27 as libc::c_int as YY_CHAR,
    28 as libc::c_int as YY_CHAR,
    28 as libc::c_int as YY_CHAR,
    28 as libc::c_int as YY_CHAR,
    28 as libc::c_int as YY_CHAR,
    29 as libc::c_int as YY_CHAR,
    30 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    32 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    33 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    34 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    35 as libc::c_int as YY_CHAR,
    36 as libc::c_int as YY_CHAR,
    37 as libc::c_int as YY_CHAR,
    38 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    39 as libc::c_int as YY_CHAR,
    40 as libc::c_int as YY_CHAR,
    41 as libc::c_int as YY_CHAR,
    42 as libc::c_int as YY_CHAR,
    43 as libc::c_int as YY_CHAR,
    44 as libc::c_int as YY_CHAR,
    45 as libc::c_int as YY_CHAR,
    46 as libc::c_int as YY_CHAR,
    47 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    48 as libc::c_int as YY_CHAR,
    49 as libc::c_int as YY_CHAR,
    50 as libc::c_int as YY_CHAR,
    51 as libc::c_int as YY_CHAR,
    52 as libc::c_int as YY_CHAR,
    53 as libc::c_int as YY_CHAR,
    31 as libc::c_int as YY_CHAR,
    54 as libc::c_int as YY_CHAR,
    55 as libc::c_int as YY_CHAR,
    56 as libc::c_int as YY_CHAR,
    57 as libc::c_int as YY_CHAR,
    58 as libc::c_int as YY_CHAR,
    59 as libc::c_int as YY_CHAR,
    60 as libc::c_int as YY_CHAR,
    61 as libc::c_int as YY_CHAR,
    62 as libc::c_int as YY_CHAR,
    63 as libc::c_int as YY_CHAR,
    64 as libc::c_int as YY_CHAR,
    65 as libc::c_int as YY_CHAR,
    66 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
];
static mut yy_meta: [YY_CHAR; 67] = [
    0 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    2 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    3 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    4 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
    1 as libc::c_int as YY_CHAR,
];
static mut yy_base: [flex_int16_t; 279] = [
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    404 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    379 as libc::c_int as flex_int16_t,
    62 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    44 as libc::c_int as flex_int16_t,
    61 as libc::c_int as flex_int16_t,
    366 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    377 as libc::c_int as flex_int16_t,
    57 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    58 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    90 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    375 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    375 as libc::c_int as flex_int16_t,
    54 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    89 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    374 as libc::c_int as flex_int16_t,
    27 as libc::c_int as flex_int16_t,
    343 as libc::c_int as flex_int16_t,
    74 as libc::c_int as flex_int16_t,
    59 as libc::c_int as flex_int16_t,
    65 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    344 as libc::c_int as flex_int16_t,
    84 as libc::c_int as flex_int16_t,
    60 as libc::c_int as flex_int16_t,
    352 as libc::c_int as flex_int16_t,
    90 as libc::c_int as flex_int16_t,
    333 as libc::c_int as flex_int16_t,
    342 as libc::c_int as flex_int16_t,
    340 as libc::c_int as flex_int16_t,
    345 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    84 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    387 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    108 as libc::c_int as flex_int16_t,
    128 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    372 as libc::c_int as flex_int16_t,
    132 as libc::c_int as flex_int16_t,
    339 as libc::c_int as flex_int16_t,
    333 as libc::c_int as flex_int16_t,
    347 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    140 as libc::c_int as flex_int16_t,
    168 as libc::c_int as flex_int16_t,
    121 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    173 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    361 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    360 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    127 as libc::c_int as flex_int16_t,
    347 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    326 as libc::c_int as flex_int16_t,
    334 as libc::c_int as flex_int16_t,
    337 as libc::c_int as flex_int16_t,
    324 as libc::c_int as flex_int16_t,
    339 as libc::c_int as flex_int16_t,
    326 as libc::c_int as flex_int16_t,
    332 as libc::c_int as flex_int16_t,
    318 as libc::c_int as flex_int16_t,
    319 as libc::c_int as flex_int16_t,
    316 as libc::c_int as flex_int16_t,
    316 as libc::c_int as flex_int16_t,
    319 as libc::c_int as flex_int16_t,
    316 as libc::c_int as flex_int16_t,
    313 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    312 as libc::c_int as flex_int16_t,
    317 as libc::c_int as flex_int16_t,
    315 as libc::c_int as flex_int16_t,
    151 as libc::c_int as flex_int16_t,
    308 as libc::c_int as flex_int16_t,
    312 as libc::c_int as flex_int16_t,
    96 as libc::c_int as flex_int16_t,
    313 as libc::c_int as flex_int16_t,
    132 as libc::c_int as flex_int16_t,
    315 as libc::c_int as flex_int16_t,
    308 as libc::c_int as flex_int16_t,
    144 as libc::c_int as flex_int16_t,
    96 as libc::c_int as flex_int16_t,
    313 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    302 as libc::c_int as flex_int16_t,
    315 as libc::c_int as flex_int16_t,
    308 as libc::c_int as flex_int16_t,
    189 as libc::c_int as flex_int16_t,
    209 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    178 as libc::c_int as flex_int16_t,
    175 as libc::c_int as flex_int16_t,
    217 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    304 as libc::c_int as flex_int16_t,
    303 as libc::c_int as flex_int16_t,
    315 as libc::c_int as flex_int16_t,
    310 as libc::c_int as flex_int16_t,
    298 as libc::c_int as flex_int16_t,
    157 as libc::c_int as flex_int16_t,
    312 as libc::c_int as flex_int16_t,
    310 as libc::c_int as flex_int16_t,
    306 as libc::c_int as flex_int16_t,
    298 as libc::c_int as flex_int16_t,
    304 as libc::c_int as flex_int16_t,
    307 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    293 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    294 as libc::c_int as flex_int16_t,
    298 as libc::c_int as flex_int16_t,
    295 as libc::c_int as flex_int16_t,
    284 as libc::c_int as flex_int16_t,
    289 as libc::c_int as flex_int16_t,
    285 as libc::c_int as flex_int16_t,
    287 as libc::c_int as flex_int16_t,
    294 as libc::c_int as flex_int16_t,
    293 as libc::c_int as flex_int16_t,
    279 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    289 as libc::c_int as flex_int16_t,
    279 as libc::c_int as flex_int16_t,
    283 as libc::c_int as flex_int16_t,
    287 as libc::c_int as flex_int16_t,
    289 as libc::c_int as flex_int16_t,
    278 as libc::c_int as flex_int16_t,
    221 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    282 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    232 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    225 as libc::c_int as flex_int16_t,
    238 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    227 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    273 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    265 as libc::c_int as flex_int16_t,
    273 as libc::c_int as flex_int16_t,
    262 as libc::c_int as flex_int16_t,
    269 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    263 as libc::c_int as flex_int16_t,
    260 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    259 as libc::c_int as flex_int16_t,
    259 as libc::c_int as flex_int16_t,
    271 as libc::c_int as flex_int16_t,
    255 as libc::c_int as flex_int16_t,
    267 as libc::c_int as flex_int16_t,
    257 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    261 as libc::c_int as flex_int16_t,
    266 as libc::c_int as flex_int16_t,
    265 as libc::c_int as flex_int16_t,
    263 as libc::c_int as flex_int16_t,
    249 as libc::c_int as flex_int16_t,
    248 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    236 as libc::c_int as flex_int16_t,
    248 as libc::c_int as flex_int16_t,
    237 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    236 as libc::c_int as flex_int16_t,
    243 as libc::c_int as flex_int16_t,
    245 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    235 as libc::c_int as flex_int16_t,
    236 as libc::c_int as flex_int16_t,
    240 as libc::c_int as flex_int16_t,
    230 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    224 as libc::c_int as flex_int16_t,
    228 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    236 as libc::c_int as flex_int16_t,
    230 as libc::c_int as flex_int16_t,
    232 as libc::c_int as flex_int16_t,
    215 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    213 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    203 as libc::c_int as flex_int16_t,
    203 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    193 as libc::c_int as flex_int16_t,
    169 as libc::c_int as flex_int16_t,
    168 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    173 as libc::c_int as flex_int16_t,
    158 as libc::c_int as flex_int16_t,
    128 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    131 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    113 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    98 as libc::c_int as flex_int16_t,
    47 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    405 as libc::c_int as flex_int16_t,
    294 as libc::c_int as flex_int16_t,
    298 as libc::c_int as flex_int16_t,
    300 as libc::c_int as flex_int16_t,
    69 as libc::c_int as flex_int16_t,
];
static mut yy_def: [flex_int16_t; 279] = [
    0 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    278 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    278 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    0 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
];
static mut yy_nxt: [flex_int16_t; 472] = [
    0 as libc::c_int as flex_int16_t,
    4 as libc::c_int as flex_int16_t,
    5 as libc::c_int as flex_int16_t,
    6 as libc::c_int as flex_int16_t,
    7 as libc::c_int as flex_int16_t,
    8 as libc::c_int as flex_int16_t,
    9 as libc::c_int as flex_int16_t,
    10 as libc::c_int as flex_int16_t,
    11 as libc::c_int as flex_int16_t,
    12 as libc::c_int as flex_int16_t,
    13 as libc::c_int as flex_int16_t,
    14 as libc::c_int as flex_int16_t,
    15 as libc::c_int as flex_int16_t,
    16 as libc::c_int as flex_int16_t,
    17 as libc::c_int as flex_int16_t,
    18 as libc::c_int as flex_int16_t,
    19 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    21 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    24 as libc::c_int as flex_int16_t,
    25 as libc::c_int as flex_int16_t,
    26 as libc::c_int as flex_int16_t,
    27 as libc::c_int as flex_int16_t,
    28 as libc::c_int as flex_int16_t,
    29 as libc::c_int as flex_int16_t,
    30 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    32 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    33 as libc::c_int as flex_int16_t,
    4 as libc::c_int as flex_int16_t,
    34 as libc::c_int as flex_int16_t,
    35 as libc::c_int as flex_int16_t,
    36 as libc::c_int as flex_int16_t,
    37 as libc::c_int as flex_int16_t,
    38 as libc::c_int as flex_int16_t,
    39 as libc::c_int as flex_int16_t,
    40 as libc::c_int as flex_int16_t,
    41 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    43 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    44 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    45 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    47 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    49 as libc::c_int as flex_int16_t,
    50 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    31 as libc::c_int as flex_int16_t,
    51 as libc::c_int as flex_int16_t,
    52 as libc::c_int as flex_int16_t,
    53 as libc::c_int as flex_int16_t,
    54 as libc::c_int as flex_int16_t,
    57 as libc::c_int as flex_int16_t,
    59 as libc::c_int as flex_int16_t,
    60 as libc::c_int as flex_int16_t,
    61 as libc::c_int as flex_int16_t,
    66 as libc::c_int as flex_int16_t,
    140 as libc::c_int as flex_int16_t,
    71 as libc::c_int as flex_int16_t,
    68 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    76 as libc::c_int as flex_int16_t,
    90 as libc::c_int as flex_int16_t,
    91 as libc::c_int as flex_int16_t,
    85 as libc::c_int as flex_int16_t,
    67 as libc::c_int as flex_int16_t,
    69 as libc::c_int as flex_int16_t,
    70 as libc::c_int as flex_int16_t,
    96 as libc::c_int as flex_int16_t,
    62 as libc::c_int as flex_int16_t,
    57 as libc::c_int as flex_int16_t,
    97 as libc::c_int as flex_int16_t,
    77 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    273 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    86 as libc::c_int as flex_int16_t,
    93 as libc::c_int as flex_int16_t,
    87 as libc::c_int as flex_int16_t,
    88 as libc::c_int as flex_int16_t,
    73 as libc::c_int as flex_int16_t,
    58 as libc::c_int as flex_int16_t,
    94 as libc::c_int as flex_int16_t,
    74 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    102 as libc::c_int as flex_int16_t,
    112 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    75 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    125 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    103 as libc::c_int as flex_int16_t,
    113 as libc::c_int as flex_int16_t,
    99 as libc::c_int as flex_int16_t,
    104 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    105 as libc::c_int as flex_int16_t,
    58 as libc::c_int as flex_int16_t,
    127 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    100 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    82 as libc::c_int as flex_int16_t,
    106 as libc::c_int as flex_int16_t,
    101 as libc::c_int as flex_int16_t,
    107 as libc::c_int as flex_int16_t,
    110 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    108 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    57 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    115 as libc::c_int as flex_int16_t,
    111 as libc::c_int as flex_int16_t,
    116 as libc::c_int as flex_int16_t,
    117 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    272 as libc::c_int as flex_int16_t,
    164 as libc::c_int as flex_int16_t,
    118 as libc::c_int as flex_int16_t,
    173 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    174 as libc::c_int as flex_int16_t,
    119 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    126 as libc::c_int as flex_int16_t,
    120 as libc::c_int as flex_int16_t,
    82 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    165 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    129 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    58 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    135 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    271 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    167 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    270 as libc::c_int as flex_int16_t,
    129 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    269 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    135 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    138 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    138 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    168 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    171 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    160 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    176 as libc::c_int as flex_int16_t,
    172 as libc::c_int as flex_int16_t,
    176 as libc::c_int as flex_int16_t,
    268 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    161 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    192 as libc::c_int as flex_int16_t,
    193 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    267 as libc::c_int as flex_int16_t,
    181 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    266 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    183 as libc::c_int as flex_int16_t,
    265 as libc::c_int as flex_int16_t,
    183 as libc::c_int as flex_int16_t,
    264 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    181 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    263 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    222 as libc::c_int as flex_int16_t,
    185 as libc::c_int as flex_int16_t,
    222 as libc::c_int as flex_int16_t,
    185 as libc::c_int as flex_int16_t,
    262 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    261 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    260 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    185 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    185 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    259 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    258 as libc::c_int as flex_int16_t,
    130 as libc::c_int as flex_int16_t,
    257 as libc::c_int as flex_int16_t,
    256 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    255 as libc::c_int as flex_int16_t,
    254 as libc::c_int as flex_int16_t,
    253 as libc::c_int as flex_int16_t,
    252 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    251 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    250 as libc::c_int as flex_int16_t,
    249 as libc::c_int as flex_int16_t,
    136 as libc::c_int as flex_int16_t,
    248 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    247 as libc::c_int as flex_int16_t,
    246 as libc::c_int as flex_int16_t,
    245 as libc::c_int as flex_int16_t,
    244 as libc::c_int as flex_int16_t,
    182 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    243 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    92 as libc::c_int as flex_int16_t,
    92 as libc::c_int as flex_int16_t,
    242 as libc::c_int as flex_int16_t,
    241 as libc::c_int as flex_int16_t,
    240 as libc::c_int as flex_int16_t,
    239 as libc::c_int as flex_int16_t,
    238 as libc::c_int as flex_int16_t,
    237 as libc::c_int as flex_int16_t,
    236 as libc::c_int as flex_int16_t,
    235 as libc::c_int as flex_int16_t,
    234 as libc::c_int as flex_int16_t,
    233 as libc::c_int as flex_int16_t,
    232 as libc::c_int as flex_int16_t,
    231 as libc::c_int as flex_int16_t,
    230 as libc::c_int as flex_int16_t,
    229 as libc::c_int as flex_int16_t,
    228 as libc::c_int as flex_int16_t,
    227 as libc::c_int as flex_int16_t,
    226 as libc::c_int as flex_int16_t,
    225 as libc::c_int as flex_int16_t,
    224 as libc::c_int as flex_int16_t,
    221 as libc::c_int as flex_int16_t,
    220 as libc::c_int as flex_int16_t,
    219 as libc::c_int as flex_int16_t,
    218 as libc::c_int as flex_int16_t,
    217 as libc::c_int as flex_int16_t,
    216 as libc::c_int as flex_int16_t,
    215 as libc::c_int as flex_int16_t,
    214 as libc::c_int as flex_int16_t,
    213 as libc::c_int as flex_int16_t,
    212 as libc::c_int as flex_int16_t,
    211 as libc::c_int as flex_int16_t,
    210 as libc::c_int as flex_int16_t,
    209 as libc::c_int as flex_int16_t,
    208 as libc::c_int as flex_int16_t,
    207 as libc::c_int as flex_int16_t,
    206 as libc::c_int as flex_int16_t,
    205 as libc::c_int as flex_int16_t,
    204 as libc::c_int as flex_int16_t,
    203 as libc::c_int as flex_int16_t,
    202 as libc::c_int as flex_int16_t,
    201 as libc::c_int as flex_int16_t,
    200 as libc::c_int as flex_int16_t,
    199 as libc::c_int as flex_int16_t,
    198 as libc::c_int as flex_int16_t,
    197 as libc::c_int as flex_int16_t,
    196 as libc::c_int as flex_int16_t,
    195 as libc::c_int as flex_int16_t,
    194 as libc::c_int as flex_int16_t,
    191 as libc::c_int as flex_int16_t,
    190 as libc::c_int as flex_int16_t,
    189 as libc::c_int as flex_int16_t,
    188 as libc::c_int as flex_int16_t,
    187 as libc::c_int as flex_int16_t,
    180 as libc::c_int as flex_int16_t,
    179 as libc::c_int as flex_int16_t,
    178 as libc::c_int as flex_int16_t,
    175 as libc::c_int as flex_int16_t,
    170 as libc::c_int as flex_int16_t,
    169 as libc::c_int as flex_int16_t,
    166 as libc::c_int as flex_int16_t,
    163 as libc::c_int as flex_int16_t,
    162 as libc::c_int as flex_int16_t,
    159 as libc::c_int as flex_int16_t,
    158 as libc::c_int as flex_int16_t,
    157 as libc::c_int as flex_int16_t,
    156 as libc::c_int as flex_int16_t,
    155 as libc::c_int as flex_int16_t,
    154 as libc::c_int as flex_int16_t,
    153 as libc::c_int as flex_int16_t,
    152 as libc::c_int as flex_int16_t,
    151 as libc::c_int as flex_int16_t,
    150 as libc::c_int as flex_int16_t,
    149 as libc::c_int as flex_int16_t,
    148 as libc::c_int as flex_int16_t,
    147 as libc::c_int as flex_int16_t,
    146 as libc::c_int as flex_int16_t,
    145 as libc::c_int as flex_int16_t,
    144 as libc::c_int as flex_int16_t,
    143 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    142 as libc::c_int as flex_int16_t,
    141 as libc::c_int as flex_int16_t,
    133 as libc::c_int as flex_int16_t,
    132 as libc::c_int as flex_int16_t,
    131 as libc::c_int as flex_int16_t,
    128 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    124 as libc::c_int as flex_int16_t,
    123 as libc::c_int as flex_int16_t,
    122 as libc::c_int as flex_int16_t,
    121 as libc::c_int as flex_int16_t,
    114 as libc::c_int as flex_int16_t,
    109 as libc::c_int as flex_int16_t,
    98 as libc::c_int as flex_int16_t,
    95 as libc::c_int as flex_int16_t,
    89 as libc::c_int as flex_int16_t,
    84 as libc::c_int as flex_int16_t,
    65 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    55 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    3 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
];
static mut yy_chk: [flex_int16_t; 472] = [
    0 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    1 as libc::c_int as flex_int16_t,
    8 as libc::c_int as flex_int16_t,
    11 as libc::c_int as flex_int16_t,
    11 as libc::c_int as flex_int16_t,
    12 as libc::c_int as flex_int16_t,
    17 as libc::c_int as flex_int16_t,
    278 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    19 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    21 as libc::c_int as flex_int16_t,
    28 as libc::c_int as flex_int16_t,
    28 as libc::c_int as flex_int16_t,
    26 as libc::c_int as flex_int16_t,
    17 as libc::c_int as flex_int16_t,
    19 as libc::c_int as flex_int16_t,
    19 as libc::c_int as flex_int16_t,
    36 as libc::c_int as flex_int16_t,
    12 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    36 as libc::c_int as flex_int16_t,
    21 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    269 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    26 as libc::c_int as flex_int16_t,
    32 as libc::c_int as flex_int16_t,
    26 as libc::c_int as flex_int16_t,
    26 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    8 as libc::c_int as flex_int16_t,
    32 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    39 as libc::c_int as flex_int16_t,
    44 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    20 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    52 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    39 as libc::c_int as flex_int16_t,
    44 as libc::c_int as flex_int16_t,
    38 as libc::c_int as flex_int16_t,
    40 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    40 as libc::c_int as flex_int16_t,
    56 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    38 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    40 as libc::c_int as flex_int16_t,
    38 as libc::c_int as flex_int16_t,
    41 as libc::c_int as flex_int16_t,
    43 as libc::c_int as flex_int16_t,
    23 as libc::c_int as flex_int16_t,
    41 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    93 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    43 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    64 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    268 as libc::c_int as flex_int16_t,
    117 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    123 as libc::c_int as flex_int16_t,
    63 as libc::c_int as flex_int16_t,
    123 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    52 as libc::c_int as flex_int16_t,
    46 as libc::c_int as flex_int16_t,
    22 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    117 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    93 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    266 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    119 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    264 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    262 as libc::c_int as flex_int16_t,
    81 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    72 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    78 as libc::c_int as flex_int16_t,
    119 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    80 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    122 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    138 as libc::c_int as flex_int16_t,
    138 as libc::c_int as flex_int16_t,
    114 as libc::c_int as flex_int16_t,
    79 as libc::c_int as flex_int16_t,
    129 as libc::c_int as flex_int16_t,
    122 as libc::c_int as flex_int16_t,
    129 as libc::c_int as flex_int16_t,
    261 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    129 as libc::c_int as flex_int16_t,
    129 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    114 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    148 as libc::c_int as flex_int16_t,
    148 as libc::c_int as flex_int16_t,
    140 as libc::c_int as flex_int16_t,
    140 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    260 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    253 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    135 as libc::c_int as flex_int16_t,
    250 as libc::c_int as flex_int16_t,
    135 as libc::c_int as flex_int16_t,
    249 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    135 as libc::c_int as flex_int16_t,
    135 as libc::c_int as flex_int16_t,
    83 as libc::c_int as flex_int16_t,
    140 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    248 as libc::c_int as flex_int16_t,
    137 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    134 as libc::c_int as flex_int16_t,
    140 as libc::c_int as flex_int16_t,
    176 as libc::c_int as flex_int16_t,
    176 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    183 as libc::c_int as flex_int16_t,
    183 as libc::c_int as flex_int16_t,
    181 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    181 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    245 as libc::c_int as flex_int16_t,
    181 as libc::c_int as flex_int16_t,
    181 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    244 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    242 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    222 as libc::c_int as flex_int16_t,
    222 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    139 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    241 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    240 as libc::c_int as flex_int16_t,
    177 as libc::c_int as flex_int16_t,
    239 as libc::c_int as flex_int16_t,
    238 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    237 as libc::c_int as flex_int16_t,
    234 as libc::c_int as flex_int16_t,
    233 as libc::c_int as flex_int16_t,
    230 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    229 as libc::c_int as flex_int16_t,
    186 as libc::c_int as flex_int16_t,
    228 as libc::c_int as flex_int16_t,
    227 as libc::c_int as flex_int16_t,
    184 as libc::c_int as flex_int16_t,
    221 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    219 as libc::c_int as flex_int16_t,
    218 as libc::c_int as flex_int16_t,
    217 as libc::c_int as flex_int16_t,
    215 as libc::c_int as flex_int16_t,
    223 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    275 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    214 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    276 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    277 as libc::c_int as flex_int16_t,
    213 as libc::c_int as flex_int16_t,
    212 as libc::c_int as flex_int16_t,
    211 as libc::c_int as flex_int16_t,
    210 as libc::c_int as flex_int16_t,
    208 as libc::c_int as flex_int16_t,
    207 as libc::c_int as flex_int16_t,
    206 as libc::c_int as flex_int16_t,
    205 as libc::c_int as flex_int16_t,
    204 as libc::c_int as flex_int16_t,
    203 as libc::c_int as flex_int16_t,
    201 as libc::c_int as flex_int16_t,
    199 as libc::c_int as flex_int16_t,
    198 as libc::c_int as flex_int16_t,
    195 as libc::c_int as flex_int16_t,
    194 as libc::c_int as flex_int16_t,
    193 as libc::c_int as flex_int16_t,
    192 as libc::c_int as flex_int16_t,
    189 as libc::c_int as flex_int16_t,
    188 as libc::c_int as flex_int16_t,
    180 as libc::c_int as flex_int16_t,
    179 as libc::c_int as flex_int16_t,
    178 as libc::c_int as flex_int16_t,
    175 as libc::c_int as flex_int16_t,
    174 as libc::c_int as flex_int16_t,
    173 as libc::c_int as flex_int16_t,
    172 as libc::c_int as flex_int16_t,
    171 as libc::c_int as flex_int16_t,
    170 as libc::c_int as flex_int16_t,
    169 as libc::c_int as flex_int16_t,
    168 as libc::c_int as flex_int16_t,
    167 as libc::c_int as flex_int16_t,
    166 as libc::c_int as flex_int16_t,
    165 as libc::c_int as flex_int16_t,
    164 as libc::c_int as flex_int16_t,
    163 as libc::c_int as flex_int16_t,
    162 as libc::c_int as flex_int16_t,
    161 as libc::c_int as flex_int16_t,
    160 as libc::c_int as flex_int16_t,
    159 as libc::c_int as flex_int16_t,
    158 as libc::c_int as flex_int16_t,
    156 as libc::c_int as flex_int16_t,
    154 as libc::c_int as flex_int16_t,
    153 as libc::c_int as flex_int16_t,
    152 as libc::c_int as flex_int16_t,
    151 as libc::c_int as flex_int16_t,
    150 as libc::c_int as flex_int16_t,
    149 as libc::c_int as flex_int16_t,
    147 as libc::c_int as flex_int16_t,
    146 as libc::c_int as flex_int16_t,
    145 as libc::c_int as flex_int16_t,
    144 as libc::c_int as flex_int16_t,
    143 as libc::c_int as flex_int16_t,
    133 as libc::c_int as flex_int16_t,
    132 as libc::c_int as flex_int16_t,
    131 as libc::c_int as flex_int16_t,
    124 as libc::c_int as flex_int16_t,
    121 as libc::c_int as flex_int16_t,
    120 as libc::c_int as flex_int16_t,
    118 as libc::c_int as flex_int16_t,
    116 as libc::c_int as flex_int16_t,
    115 as libc::c_int as flex_int16_t,
    113 as libc::c_int as flex_int16_t,
    112 as libc::c_int as flex_int16_t,
    111 as libc::c_int as flex_int16_t,
    109 as libc::c_int as flex_int16_t,
    108 as libc::c_int as flex_int16_t,
    107 as libc::c_int as flex_int16_t,
    106 as libc::c_int as flex_int16_t,
    105 as libc::c_int as flex_int16_t,
    104 as libc::c_int as flex_int16_t,
    103 as libc::c_int as flex_int16_t,
    102 as libc::c_int as flex_int16_t,
    101 as libc::c_int as flex_int16_t,
    100 as libc::c_int as flex_int16_t,
    99 as libc::c_int as flex_int16_t,
    98 as libc::c_int as flex_int16_t,
    97 as libc::c_int as flex_int16_t,
    96 as libc::c_int as flex_int16_t,
    94 as libc::c_int as flex_int16_t,
    91 as libc::c_int as flex_int16_t,
    87 as libc::c_int as flex_int16_t,
    75 as libc::c_int as flex_int16_t,
    74 as libc::c_int as flex_int16_t,
    73 as libc::c_int as flex_int16_t,
    71 as libc::c_int as flex_int16_t,
    58 as libc::c_int as flex_int16_t,
    50 as libc::c_int as flex_int16_t,
    49 as libc::c_int as flex_int16_t,
    48 as libc::c_int as flex_int16_t,
    47 as libc::c_int as flex_int16_t,
    45 as libc::c_int as flex_int16_t,
    42 as libc::c_int as flex_int16_t,
    37 as libc::c_int as flex_int16_t,
    35 as libc::c_int as flex_int16_t,
    27 as libc::c_int as flex_int16_t,
    24 as libc::c_int as flex_int16_t,
    16 as libc::c_int as flex_int16_t,
    13 as libc::c_int as flex_int16_t,
    7 as libc::c_int as flex_int16_t,
    3 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
    274 as libc::c_int as flex_int16_t,
];
static mut yy_last_accepting_state: yy_state_type = 0;
static mut yy_last_accepting_cpos: *mut libc::c_char =
    0 as *const libc::c_char as *mut libc::c_char;
#[no_mangle]
pub static mut yy_flex_debug: libc::c_int = 0 as libc::c_int;
#[no_mangle]
pub static mut yytext: *mut libc::c_char = 0 as *const libc::c_char as *mut libc::c_char;
#[no_mangle]
pub unsafe extern "C" fn yylex() -> libc::c_int {
    let mut yy_amount_of_matched_text: libc::c_int = 0;
    let mut yy_next_state: yy_state_type = 0;
    let mut current_block: u64;
    let mut yy_current_state: yy_state_type = 0;
    let mut yy_cp: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut yy_bp: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut yy_act: libc::c_int = 0;
    if yy_init == 0 {
        yy_init = 1 as libc::c_int;
        if yy_start == 0 {
            yy_start = 1 as libc::c_int;
        }
        if yyin.is_null() {
            yyin = __stdinp;
        }
        if yyout.is_null() {
            yyout = __stdoutp;
        }
        if if !yy_buffer_stack.is_null() {
            *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
        } else {
            0 as YY_BUFFER_STATE
        }
        .is_null()
        {
            yyensure_buffer_stack();
            let ref mut fresh0 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
            *fresh0 = yy_create_buffer(yyin, 16384 as libc::c_int);
        }
        yy_load_buffer_state();
    }
    loop {
        yy_cp = yy_c_buf_p;
        *yy_cp = yy_hold_char;
        yy_bp = yy_cp;
        yy_current_state = yy_start;
        '_yy_match: loop {
            loop {
                let mut yy_c: YY_CHAR = yy_ec[*yy_cp as YY_CHAR as usize];
                if yy_accept[yy_current_state as usize] != 0 {
                    yy_last_accepting_state = yy_current_state;
                    yy_last_accepting_cpos = yy_cp;
                }
                while yy_chk[(yy_base[yy_current_state as usize] as libc::c_int
                    + yy_c as libc::c_int) as usize] as libc::c_int
                    != yy_current_state
                {
                    yy_current_state = yy_def[yy_current_state as usize] as libc::c_int;
                    if yy_current_state >= 275 as libc::c_int {
                        yy_c = yy_meta[yy_c as usize];
                    }
                }
                yy_current_state = yy_nxt[(yy_base[yy_current_state as usize] as libc::c_int
                    + yy_c as libc::c_int) as usize]
                    as yy_state_type;
                yy_cp = yy_cp.offset(1);
                if !(yy_base[yy_current_state as usize] as libc::c_int != 405 as libc::c_int) {
                    break;
                }
            }
            '_yy_find_action: loop {
                yy_act = yy_accept[yy_current_state as usize] as libc::c_int;
                if yy_act == 0 as libc::c_int {
                    yy_cp = yy_last_accepting_cpos;
                    yy_current_state = yy_last_accepting_state;
                    yy_act = yy_accept[yy_current_state as usize] as libc::c_int;
                }
                yytext = yy_bp;
                yyleng = yy_cp.offset_from(yy_bp) as libc::c_long as yy_size_t;
                yy_hold_char = *yy_cp;
                *yy_cp = '\0' as i32 as libc::c_char;
                yy_c_buf_p = yy_cp;
                loop {
                    match yy_act {
                        0 => {
                            *yy_cp = yy_hold_char;
                            yy_cp = yy_last_accepting_cpos;
                            yy_current_state = yy_last_accepting_state;
                            continue '_yy_find_action;
                        }
                        1 => {
                            preproc();
                            break '_yy_match;
                        }
                        2 => {
                            comment();
                            break '_yy_match;
                        }
                        3 => {
                            count();
                            return 323 as libc::c_int;
                        }
                        4 => {
                            count();
                            return 324 as libc::c_int;
                        }
                        5 => {
                            count();
                            return 325 as libc::c_int;
                        }
                        6 => {
                            count();
                            return 289 as libc::c_int;
                        }
                        7 => {
                            count();
                            return 306 as libc::c_int;
                        }
                        8 => {
                            count();
                            return 321 as libc::c_int;
                        }
                        9 => {
                            count();
                            return 310 as libc::c_int;
                        }
                        10 => {
                            count();
                            return 291 as libc::c_int;
                        }
                        11 => {
                            count();
                            return 299 as libc::c_int;
                        }
                        12 => {
                            count();
                            return 320 as libc::c_int;
                        }
                        13 => {
                            count();
                            return 311 as libc::c_int;
                        }
                        14 => {
                            count();
                            return 316 as libc::c_int;
                        }
                        15 => {
                            count();
                            return 298 as libc::c_int;
                        }
                        16 => {
                            count();
                            return 313 as libc::c_int;
                        }
                        17 => {
                            count();
                            return 304 as libc::c_int;
                        }
                        18 => {
                            count();
                            return 287 as libc::c_int;
                        }
                        19 => {
                            count();
                            return 297 as libc::c_int;
                        }
                        20 => {
                            count();
                            return 317 as libc::c_int;
                        }
                        21 => {
                            count();
                            return 319 as libc::c_int;
                        }
                        22 => {
                            count();
                            return 312 as libc::c_int;
                        }
                        23 => {
                            count();
                            return 293 as libc::c_int;
                        }
                        24 => {
                            count();
                            return 307 as libc::c_int;
                        }
                        25 => {
                            count();
                            return 294 as libc::c_int;
                        }
                        26 => {
                            count();
                            return 290 as libc::c_int;
                        }
                        27 => {
                            count();
                            return 322 as libc::c_int;
                        }
                        28 => {
                            count();
                            return 292 as libc::c_int;
                        }
                        29 => {
                            count();
                            return 295 as libc::c_int;
                        }
                        30 => {
                            count();
                            return 262 as libc::c_int;
                        }
                        31 => {
                            count();
                            return 318 as libc::c_int;
                        }
                        32 => {
                            count();
                            return 288 as libc::c_int;
                        }
                        33 => {
                            count();
                            return 308 as libc::c_int;
                        }
                        34 => {
                            count();
                            return 302 as libc::c_int;
                        }
                        35 => {
                            count();
                            return 314 as libc::c_int;
                        }
                        36 => {
                            count();
                            return 286 as libc::c_int;
                        }
                        37 => {
                            count();
                            return 303 as libc::c_int;
                        }
                        38 => {
                            count();
                            return 296 as libc::c_int;
                        }
                        39 => {
                            count();
                            return 301 as libc::c_int;
                        }
                        40 => {
                            count();
                            return 300 as libc::c_int;
                        }
                        41 => {
                            count();
                            return 315 as libc::c_int;
                        }
                        42 => {
                            count();
                            return check_type() as libc::c_int;
                        }
                        43 => {
                            count();
                            return 259 as libc::c_int;
                        }
                        44 => {
                            count();
                            return 259 as libc::c_int;
                        }
                        45 => {
                            count();
                            return 259 as libc::c_int;
                        }
                        46 => {
                            count();
                            return 260 as libc::c_int;
                        }
                        47 => {
                            count();
                            return 259 as libc::c_int;
                        }
                        48 => {
                            count();
                            return 259 as libc::c_int;
                        }
                        49 => {
                            count();
                            return 259 as libc::c_int;
                        }
                        50 => {
                            count();
                            return 261 as libc::c_int;
                        }
                        51 => {
                            count();
                            return 305 as libc::c_int;
                        }
                        52 => {
                            count();
                            return 281 as libc::c_int;
                        }
                        53 => {
                            count();
                            return 280 as libc::c_int;
                        }
                        54 => {
                            count();
                            return 278 as libc::c_int;
                        }
                        55 => {
                            count();
                            return 279 as libc::c_int;
                        }
                        56 => {
                            count();
                            return 275 as libc::c_int;
                        }
                        57 => {
                            count();
                            return 276 as libc::c_int;
                        }
                        58 => {
                            count();
                            return 277 as libc::c_int;
                        }
                        59 => {
                            count();
                            return 282 as libc::c_int;
                        }
                        60 => {
                            count();
                            return 283 as libc::c_int;
                        }
                        61 => {
                            count();
                            return 284 as libc::c_int;
                        }
                        62 => {
                            count();
                            return 267 as libc::c_int;
                        }
                        63 => {
                            count();
                            return 266 as libc::c_int;
                        }
                        64 => {
                            count();
                            return 264 as libc::c_int;
                        }
                        65 => {
                            count();
                            return 265 as libc::c_int;
                        }
                        66 => {
                            count();
                            return 263 as libc::c_int;
                        }
                        67 => {
                            count();
                            return 273 as libc::c_int;
                        }
                        68 => {
                            count();
                            return 274 as libc::c_int;
                        }
                        69 => {
                            count();
                            return 268 as libc::c_int;
                        }
                        70 => {
                            count();
                            return 269 as libc::c_int;
                        }
                        71 => {
                            count();
                            return 270 as libc::c_int;
                        }
                        72 => {
                            count();
                            return 271 as libc::c_int;
                        }
                        73 => {
                            count();
                            return ';' as i32;
                        }
                        74 => {
                            count();
                            return '{' as i32;
                        }
                        75 => {
                            count();
                            return '}' as i32;
                        }
                        76 => {
                            count();
                            return ',' as i32;
                        }
                        77 => {
                            count();
                            return ':' as i32;
                        }
                        78 => {
                            count();
                            return '=' as i32;
                        }
                        79 => {
                            count();
                            return '(' as i32;
                        }
                        80 => {
                            count();
                            return ')' as i32;
                        }
                        81 => {
                            count();
                            return '[' as i32;
                        }
                        82 => {
                            count();
                            return ']' as i32;
                        }
                        83 => {
                            count();
                            return '.' as i32;
                        }
                        84 => {
                            count();
                            return '&' as i32;
                        }
                        85 => {
                            count();
                            return '!' as i32;
                        }
                        86 => {
                            count();
                            return '~' as i32;
                        }
                        87 => {
                            count();
                            return '-' as i32;
                        }
                        88 => {
                            count();
                            return '+' as i32;
                        }
                        89 => {
                            count();
                            return '*' as i32;
                        }
                        90 => {
                            count();
                            return '/' as i32;
                        }
                        91 => {
                            count();
                            return '%' as i32;
                        }
                        92 => {
                            count();
                            return '<' as i32;
                        }
                        93 => {
                            count();
                            return '>' as i32;
                        }
                        94 => {
                            count();
                            return '^' as i32;
                        }
                        95 => {
                            count();
                            return '|' as i32;
                        }
                        96 => {
                            count();
                            return '?' as i32;
                        }
                        97 => {
                            count();
                            return 272 as libc::c_int;
                        }
                        98 => {
                            count();
                            return 309 as libc::c_int;
                        }
                        99 => {
                            count();
                            break '_yy_match;
                        }
                        100 => {
                            break '_yy_match;
                        }
                        101 => {
                            fwrite(yytext as *const libc::c_void, yyleng as usize, 1, yyout);
                            break '_yy_match;
                        }
                        103 => return 0 as libc::c_int,
                        102 => {
                            yy_amount_of_matched_text = yy_cp.offset_from(yytext) as libc::c_long
                                as libc::c_int
                                - 1 as libc::c_int;
                            *yy_cp = yy_hold_char;
                            if (**yy_buffer_stack.offset(yy_buffer_stack_top as isize))
                                .yy_buffer_status
                                == 0 as libc::c_int
                            {
                                yy_n_chars = (**yy_buffer_stack
                                    .offset(yy_buffer_stack_top as isize))
                                .yy_n_chars;
                                let ref mut fresh1 = (**yy_buffer_stack
                                    .offset(yy_buffer_stack_top as isize))
                                .yy_input_file;
                                *fresh1 = yyin;
                                (**yy_buffer_stack.offset(yy_buffer_stack_top as isize))
                                    .yy_buffer_status = 1 as libc::c_int;
                            }
                            if yy_c_buf_p
                                <= &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize))
                                    .yy_ch_buf)
                                    .offset(yy_n_chars as isize)
                                    as *mut libc::c_char
                            {
                                yy_next_state = 0;
                                yy_c_buf_p = yytext.offset(yy_amount_of_matched_text as isize);
                                yy_current_state = yy_get_previous_state();
                                yy_next_state = yy_try_NUL_trans(yy_current_state);
                                yy_bp = yytext.offset(0 as libc::c_int as isize);
                                if yy_next_state != 0 {
                                    current_block = 4009691900104957566;
                                    break;
                                } else {
                                    current_block = 6344031133004074802;
                                    break;
                                }
                            } else {
                                match yy_get_next_buffer() {
                                    1 => {
                                        yy_did_buffer_switch_on_eof = 0 as libc::c_int;
                                        if yywrap() != 0 {
                                            yy_c_buf_p = yytext.offset(0 as libc::c_int as isize);
                                            yy_act = 102 as libc::c_int
                                                + (yy_start - 1 as libc::c_int) / 2 as libc::c_int
                                                + 1 as libc::c_int;
                                        } else {
                                            if yy_did_buffer_switch_on_eof == 0 {
                                                yyrestart(yyin);
                                            }
                                            break '_yy_match;
                                        }
                                    }
                                    0 => {
                                        yy_c_buf_p =
                                            yytext.offset(yy_amount_of_matched_text as isize);
                                        yy_current_state = yy_get_previous_state();
                                        yy_cp = yy_c_buf_p;
                                        yy_bp = yytext.offset(0 as libc::c_int as isize);
                                        break '_yy_find_action;
                                    }
                                    2 => {
                                        yy_c_buf_p = &mut *((**yy_buffer_stack
                                            .offset(yy_buffer_stack_top as isize))
                                        .yy_ch_buf)
                                            .offset(yy_n_chars as isize)
                                            as *mut libc::c_char;
                                        yy_current_state = yy_get_previous_state();
                                        yy_cp = yy_c_buf_p;
                                        yy_bp = yytext.offset(0 as libc::c_int as isize);
                                        continue '_yy_find_action;
                                    }
                                    _ => {
                                        break '_yy_match;
                                    }
                                }
                            }
                        }
                        _ => {
                            yy_fatal_error(
                                b"fatal flex scanner internal error--no action found\0" as *const u8
                                    as *const libc::c_char,
                            );
                        }
                    }
                }
                match current_block {
                    6344031133004074802 => {
                        yy_cp = yy_c_buf_p;
                    }
                    _ => {
                        yy_c_buf_p = yy_c_buf_p.offset(1);
                        yy_cp = yy_c_buf_p;
                        yy_current_state = yy_next_state;
                        break;
                    }
                }
            }
        }
    }
}
unsafe extern "C" fn yy_get_next_buffer() -> libc::c_int {
    let mut dest: *mut libc::c_char =
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf;
    let mut source: *mut libc::c_char = yytext;
    let mut number_to_move: libc::c_int = 0;
    let mut i: libc::c_int = 0;
    let mut ret_val: libc::c_int = 0;
    if yy_c_buf_p
        > &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
            .offset(yy_n_chars.wrapping_add(1 as libc::c_int as yy_size_t) as isize)
            as *mut libc::c_char
    {
        yy_fatal_error(
            b"fatal flex scanner internal error--end of buffer missed\0" as *const u8
                as *const libc::c_char,
        );
    }
    if (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_fill_buffer == 0 as libc::c_int {
        if yy_c_buf_p.offset_from(yytext) as libc::c_long - 0 as libc::c_int as libc::c_long
            == 1 as libc::c_int as libc::c_long
        {
            return 1 as libc::c_int;
        } else {
            return 2 as libc::c_int;
        }
    }
    number_to_move = (yy_c_buf_p.offset_from(yytext) as libc::c_long
        - 1 as libc::c_int as libc::c_long) as libc::c_int;
    i = 0 as libc::c_int;
    while i < number_to_move {
        let fresh2 = source;
        source = source.offset(1);
        let fresh3 = dest;
        dest = dest.offset(1);
        *fresh3 = *fresh2;
        i += 1;
    }
    if (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buffer_status == 2 as libc::c_int
    {
        yy_n_chars = 0 as libc::c_int as yy_size_t;
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_n_chars = yy_n_chars;
    } else {
        let mut num_to_read: yy_size_t = ((**yy_buffer_stack.offset(yy_buffer_stack_top as isize))
            .yy_buf_size
            - number_to_move
            - 1 as libc::c_int) as yy_size_t;
        while num_to_read <= 0 as libc::c_int as yy_size_t {
            let mut b: YY_BUFFER_STATE = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
            let mut yy_c_buf_p_offset: libc::c_int =
                yy_c_buf_p.offset_from((*b).yy_ch_buf) as libc::c_long as libc::c_int;
            if (*b).yy_is_our_buffer != 0 {
                let mut new_size: yy_size_t = ((*b).yy_buf_size * 2 as libc::c_int) as yy_size_t;
                if new_size <= 0 as libc::c_int as yy_size_t {
                    (*b).yy_buf_size += (*b).yy_buf_size / 8 as libc::c_int;
                } else {
                    (*b).yy_buf_size *= 2 as libc::c_int;
                }
                (*b).yy_ch_buf = yyrealloc(
                    (*b).yy_ch_buf as *mut libc::c_void,
                    ((*b).yy_buf_size + 2 as libc::c_int) as yy_size_t,
                ) as *mut libc::c_char;
            } else {
                (*b).yy_ch_buf = 0 as *mut libc::c_char;
            }
            if ((*b).yy_ch_buf).is_null() {
                yy_fatal_error(
                    b"fatal error - scanner input buffer overflow\0" as *const u8
                        as *const libc::c_char,
                );
            }
            yy_c_buf_p =
                &mut *((*b).yy_ch_buf).offset(yy_c_buf_p_offset as isize) as *mut libc::c_char;
            num_to_read = ((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_size
                - number_to_move
                - 1 as libc::c_int) as yy_size_t;
        }
        if num_to_read > 8192 as libc::c_int as yy_size_t {
            num_to_read = 8192 as libc::c_int as yy_size_t;
        }
        if (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_is_interactive != 0 {
            let mut c: libc::c_int = '*' as i32;
            let mut n: yy_size_t = 0;
            n = 0 as libc::c_int as yy_size_t;
            while n < num_to_read
                && {
                    c = fgetc(yyin);
                    c != -(1 as libc::c_int)
                }
                && c != '\n' as i32
            {
                *(&mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
                    .offset(number_to_move as isize) as *mut libc::c_char)
                    .offset(n as isize) = c as libc::c_char;
                n = n.wrapping_add(1);
            }
            if c == '\n' as i32 {
                let fresh4 = n;
                n = n.wrapping_add(1);
                *(&mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
                    .offset(number_to_move as isize) as *mut libc::c_char)
                    .offset(fresh4 as isize) = c as libc::c_char;
            }
            if c == -(1 as libc::c_int) && ferror(yyin) != 0 {
                yy_fatal_error(
                    b"input in flex scanner failed\0" as *const u8 as *const libc::c_char,
                );
            }
            yy_n_chars = n;
        } else {
            *__error() = 0 as libc::c_int;
            loop {
                yy_n_chars = fread(
                    &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
                        .offset(number_to_move as isize) as *mut libc::c_char
                        as *mut libc::c_void,
                    1,
                    num_to_read as usize,
                    yyin,
                ) as libc::c_int as yy_size_t;
                if !(yy_n_chars == 0 as libc::c_int as yy_size_t && ferror(yyin) != 0) {
                    break;
                }
                if *__error() != 4 as libc::c_int {
                    yy_fatal_error(
                        b"input in flex scanner failed\0" as *const u8 as *const libc::c_char,
                    );
                } else {
                    *__error() = 0 as libc::c_int;
                    clearerr(yyin);
                }
            }
        }
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_n_chars = yy_n_chars;
    }
    if yy_n_chars == 0 as libc::c_int as yy_size_t {
        if number_to_move == 0 as libc::c_int {
            ret_val = 1 as libc::c_int;
            yyrestart(yyin);
        } else {
            ret_val = 2 as libc::c_int;
            (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buffer_status =
                2 as libc::c_int;
        }
    } else {
        ret_val = 0 as libc::c_int;
    }
    if yy_n_chars.wrapping_add(number_to_move as yy_size_t)
        > (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_size as yy_size_t
    {
        let mut new_size_0: yy_size_t = yy_n_chars
            .wrapping_add(number_to_move as yy_size_t)
            .wrapping_add(yy_n_chars >> 1 as libc::c_int);
        let ref mut fresh5 = (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf;
        *fresh5 = yyrealloc(
            (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf as *mut libc::c_void,
            new_size_0,
        ) as *mut libc::c_char;
        if ((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf).is_null() {
            yy_fatal_error(
                b"out of dynamic memory in yy_get_next_buffer()\0" as *const u8
                    as *const libc::c_char,
            );
        }
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_size =
            new_size_0.wrapping_sub(2 as libc::c_int as yy_size_t) as libc::c_int;
    }
    yy_n_chars = yy_n_chars.wrapping_add(number_to_move as yy_size_t);
    *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
        .offset(yy_n_chars as isize) = 0 as libc::c_int as libc::c_char;
    *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
        .offset(yy_n_chars.wrapping_add(1 as libc::c_int as yy_size_t) as isize) =
        0 as libc::c_int as libc::c_char;
    yytext = &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
        .offset(0 as libc::c_int as isize) as *mut libc::c_char;
    return ret_val;
}
unsafe extern "C" fn yy_get_previous_state() -> yy_state_type {
    let mut yy_current_state: yy_state_type = 0;
    let mut yy_cp: *mut libc::c_char = 0 as *mut libc::c_char;
    yy_current_state = yy_start;
    yy_cp = yytext.offset(0 as libc::c_int as isize);
    while yy_cp < yy_c_buf_p {
        let mut yy_c: YY_CHAR = (if *yy_cp as libc::c_int != 0 {
            yy_ec[*yy_cp as YY_CHAR as usize] as libc::c_int
        } else {
            1 as libc::c_int
        }) as YY_CHAR;
        if yy_accept[yy_current_state as usize] != 0 {
            yy_last_accepting_state = yy_current_state;
            yy_last_accepting_cpos = yy_cp;
        }
        while yy_chk
            [(yy_base[yy_current_state as usize] as libc::c_int + yy_c as libc::c_int) as usize]
            as libc::c_int
            != yy_current_state
        {
            yy_current_state = yy_def[yy_current_state as usize] as libc::c_int;
            if yy_current_state >= 275 as libc::c_int {
                yy_c = yy_meta[yy_c as usize];
            }
        }
        yy_current_state = yy_nxt
            [(yy_base[yy_current_state as usize] as libc::c_int + yy_c as libc::c_int) as usize]
            as yy_state_type;
        yy_cp = yy_cp.offset(1);
    }
    return yy_current_state;
}
unsafe extern "C" fn yy_try_NUL_trans(mut yy_current_state: yy_state_type) -> yy_state_type {
    let mut yy_is_jam: libc::c_int = 0;
    let mut yy_cp: *mut libc::c_char = yy_c_buf_p;
    let mut yy_c: YY_CHAR = 1 as libc::c_int as YY_CHAR;
    if yy_accept[yy_current_state as usize] != 0 {
        yy_last_accepting_state = yy_current_state;
        yy_last_accepting_cpos = yy_cp;
    }
    while yy_chk[(yy_base[yy_current_state as usize] as libc::c_int + yy_c as libc::c_int) as usize]
        as libc::c_int
        != yy_current_state
    {
        yy_current_state = yy_def[yy_current_state as usize] as libc::c_int;
        if yy_current_state >= 275 as libc::c_int {
            yy_c = yy_meta[yy_c as usize];
        }
    }
    yy_current_state = yy_nxt
        [(yy_base[yy_current_state as usize] as libc::c_int + yy_c as libc::c_int) as usize]
        as yy_state_type;
    yy_is_jam = (yy_current_state == 274 as libc::c_int) as libc::c_int;
    return if yy_is_jam != 0 {
        0 as libc::c_int
    } else {
        yy_current_state
    };
}
unsafe extern "C" fn yyunput(mut c: libc::c_int, mut yy_bp: *mut libc::c_char) {
    let mut yy_cp: *mut libc::c_char = 0 as *mut libc::c_char;
    yy_cp = yy_c_buf_p;
    *yy_cp = yy_hold_char;
    if yy_cp
        < ((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
            .offset(2 as libc::c_int as isize)
    {
        let mut number_to_move: yy_size_t = yy_n_chars.wrapping_add(2 as libc::c_int as yy_size_t);
        let mut dest: *mut libc::c_char =
            &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf).offset(
                ((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_size
                    + 2 as libc::c_int) as isize,
            ) as *mut libc::c_char;
        let mut source: *mut libc::c_char =
            &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
                .offset(number_to_move as isize) as *mut libc::c_char;
        while source > (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf {
            source = source.offset(-1);
            dest = dest.offset(-1);
            *dest = *source;
        }
        yy_cp = yy_cp.offset(dest.offset_from(source) as libc::c_long as libc::c_int as isize);
        yy_bp = yy_bp.offset(dest.offset_from(source) as libc::c_long as libc::c_int as isize);
        yy_n_chars =
            (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_size as yy_size_t;
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_n_chars = yy_n_chars;
        if yy_cp
            < ((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
                .offset(2 as libc::c_int as isize)
        {
            yy_fatal_error(
                b"flex scanner push-back overflow\0" as *const u8 as *const libc::c_char,
            );
        }
    }
    yy_cp = yy_cp.offset(-1);
    *yy_cp = c as libc::c_char;
    yytext = yy_bp;
    yy_hold_char = *yy_cp;
    yy_c_buf_p = yy_cp;
}
unsafe extern "C" fn input() -> libc::c_int {
    let mut c: libc::c_int = 0;
    *yy_c_buf_p = yy_hold_char;
    if *yy_c_buf_p as libc::c_int == 0 as libc::c_int {
        if yy_c_buf_p
            < &mut *((**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_ch_buf)
                .offset(yy_n_chars as isize) as *mut libc::c_char
        {
            *yy_c_buf_p = '\0' as i32 as libc::c_char;
        } else {
            let mut offset: yy_size_t = yy_c_buf_p.offset_from(yytext) as libc::c_long as yy_size_t;
            yy_c_buf_p = yy_c_buf_p.offset(1);
            let mut current_block_10: u64;
            match yy_get_next_buffer() {
                2 => {
                    yyrestart(yyin);
                    current_block_10 = 11022919869772457102;
                }
                1 => {
                    current_block_10 = 11022919869772457102;
                }
                0 => {
                    yy_c_buf_p = yytext.offset(offset as isize);
                    current_block_10 = 7746791466490516765;
                }
                _ => {
                    current_block_10 = 7746791466490516765;
                }
            }
            match current_block_10 {
                7746791466490516765 => {}
                _ => {
                    if yywrap() != 0 {
                        return 0 as libc::c_int;
                    }
                    if yy_did_buffer_switch_on_eof == 0 {
                        yyrestart(yyin);
                    }
                    return input();
                }
            }
        }
    }
    c = *(yy_c_buf_p as *mut libc::c_uchar) as libc::c_int;
    *yy_c_buf_p = '\0' as i32 as libc::c_char;
    yy_c_buf_p = yy_c_buf_p.offset(1);
    yy_hold_char = *yy_c_buf_p;
    return c;
}
#[no_mangle]
pub unsafe extern "C" fn yyrestart(mut input_file: *mut FILE) {
    if if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        yyensure_buffer_stack();
        let ref mut fresh6 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
        *fresh6 = yy_create_buffer(yyin, 16384 as libc::c_int);
    }
    yy_init_buffer(
        if !yy_buffer_stack.is_null() {
            *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
        } else {
            0 as YY_BUFFER_STATE
        },
        input_file,
    );
    yy_load_buffer_state();
}
#[no_mangle]
pub unsafe extern "C" fn yy_switch_to_buffer(mut new_buffer: YY_BUFFER_STATE) {
    yyensure_buffer_stack();
    if (if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }) == new_buffer
    {
        return;
    }
    if !if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        *yy_c_buf_p = yy_hold_char;
        let ref mut fresh7 = (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_pos;
        *fresh7 = yy_c_buf_p;
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_n_chars = yy_n_chars;
    }
    let ref mut fresh8 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
    *fresh8 = new_buffer;
    yy_load_buffer_state();
    yy_did_buffer_switch_on_eof = 1 as libc::c_int;
}
unsafe extern "C" fn yy_load_buffer_state() {
    yy_n_chars = (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_n_chars;
    yy_c_buf_p = (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_pos;
    yytext = yy_c_buf_p;
    yyin = (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_input_file;
    yy_hold_char = *yy_c_buf_p;
}
#[no_mangle]
pub unsafe extern "C" fn yy_create_buffer(
    mut file: *mut FILE,
    mut size: libc::c_int,
) -> YY_BUFFER_STATE {
    let mut b: YY_BUFFER_STATE = 0 as *mut yy_buffer_state;
    b = yyalloc(::core::mem::size_of::<yy_buffer_state>() as libc::c_ulong) as YY_BUFFER_STATE;
    if b.is_null() {
        yy_fatal_error(
            b"out of dynamic memory in yy_create_buffer()\0" as *const u8 as *const libc::c_char,
        );
    }
    (*b).yy_buf_size = size;
    (*b).yy_ch_buf =
        yyalloc(((*b).yy_buf_size + 2 as libc::c_int) as yy_size_t) as *mut libc::c_char;
    if ((*b).yy_ch_buf).is_null() {
        yy_fatal_error(
            b"out of dynamic memory in yy_create_buffer()\0" as *const u8 as *const libc::c_char,
        );
    }
    (*b).yy_is_our_buffer = 1 as libc::c_int;
    yy_init_buffer(b, file);
    return b;
}
#[no_mangle]
pub unsafe extern "C" fn yy_delete_buffer(mut b: YY_BUFFER_STATE) {
    if b.is_null() {
        return;
    }
    if b == (if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }) {
        let ref mut fresh9 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
        *fresh9 = 0 as YY_BUFFER_STATE;
    }
    if (*b).yy_is_our_buffer != 0 {
        yyfree((*b).yy_ch_buf as *mut libc::c_void);
    }
    yyfree(b as *mut libc::c_void);
}
unsafe extern "C" fn yy_init_buffer(mut b: YY_BUFFER_STATE, mut file: *mut FILE) {
    let mut oerrno: libc::c_int = *__error();
    yy_flush_buffer(b);
    (*b).yy_input_file = file;
    (*b).yy_fill_buffer = 1 as libc::c_int;
    if b != (if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }) {
        (*b).yy_bs_lineno = 1 as libc::c_int;
        (*b).yy_bs_column = 0 as libc::c_int;
    }
    (*b).yy_is_interactive = if !file.is_null() {
        (isatty(fileno(file)) > 0 as libc::c_int) as libc::c_int
    } else {
        0 as libc::c_int
    };
    *__error() = oerrno;
}
#[no_mangle]
pub unsafe extern "C" fn yy_flush_buffer(mut b: YY_BUFFER_STATE) {
    if b.is_null() {
        return;
    }
    (*b).yy_n_chars = 0 as libc::c_int as yy_size_t;
    *((*b).yy_ch_buf).offset(0 as libc::c_int as isize) = 0 as libc::c_int as libc::c_char;
    *((*b).yy_ch_buf).offset(1 as libc::c_int as isize) = 0 as libc::c_int as libc::c_char;
    (*b).yy_buf_pos = &mut *((*b).yy_ch_buf).offset(0 as libc::c_int as isize) as *mut libc::c_char;
    (*b).yy_at_bol = 1 as libc::c_int;
    (*b).yy_buffer_status = 0 as libc::c_int;
    if b == (if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }) {
        yy_load_buffer_state();
    }
}
#[no_mangle]
pub unsafe extern "C" fn yypush_buffer_state(mut new_buffer: YY_BUFFER_STATE) {
    if new_buffer.is_null() {
        return;
    }
    yyensure_buffer_stack();
    if !if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        *yy_c_buf_p = yy_hold_char;
        let ref mut fresh10 = (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_buf_pos;
        *fresh10 = yy_c_buf_p;
        (**yy_buffer_stack.offset(yy_buffer_stack_top as isize)).yy_n_chars = yy_n_chars;
    }
    if !if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        yy_buffer_stack_top = yy_buffer_stack_top.wrapping_add(1);
    }
    let ref mut fresh11 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
    *fresh11 = new_buffer;
    yy_load_buffer_state();
    yy_did_buffer_switch_on_eof = 1 as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn yypop_buffer_state() {
    if if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        return;
    }
    yy_delete_buffer(if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    });
    let ref mut fresh12 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
    *fresh12 = 0 as YY_BUFFER_STATE;
    if yy_buffer_stack_top > 0 as libc::c_int as size_t {
        yy_buffer_stack_top = yy_buffer_stack_top.wrapping_sub(1);
    }
    if !if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        yy_load_buffer_state();
        yy_did_buffer_switch_on_eof = 1 as libc::c_int;
    }
}
unsafe extern "C" fn yyensure_buffer_stack() {
    let mut num_to_alloc: yy_size_t = 0;
    if yy_buffer_stack.is_null() {
        num_to_alloc = 1 as libc::c_int as yy_size_t;
        yy_buffer_stack = yyalloc(
            num_to_alloc
                .wrapping_mul(::core::mem::size_of::<*mut yy_buffer_state>() as libc::c_ulong),
        ) as *mut *mut yy_buffer_state;
        if yy_buffer_stack.is_null() {
            yy_fatal_error(
                b"out of dynamic memory in yyensure_buffer_stack()\0" as *const u8
                    as *const libc::c_char,
            );
        }
        memset(
            yy_buffer_stack as *mut libc::c_void,
            0 as libc::c_int,
            (num_to_alloc as usize).wrapping_mul(::core::mem::size_of::<*mut yy_buffer_state>()),
        );
        yy_buffer_stack_max = num_to_alloc;
        yy_buffer_stack_top = 0 as libc::c_int as size_t;
        return;
    }
    if yy_buffer_stack_top >= yy_buffer_stack_max.wrapping_sub(1 as libc::c_int as size_t) {
        let mut grow_size: yy_size_t = 8 as libc::c_int as yy_size_t;
        num_to_alloc = yy_buffer_stack_max.wrapping_add(grow_size);
        yy_buffer_stack = yyrealloc(
            yy_buffer_stack as *mut libc::c_void,
            num_to_alloc
                .wrapping_mul(::core::mem::size_of::<*mut yy_buffer_state>() as libc::c_ulong),
        ) as *mut *mut yy_buffer_state;
        if yy_buffer_stack.is_null() {
            yy_fatal_error(
                b"out of dynamic memory in yyensure_buffer_stack()\0" as *const u8
                    as *const libc::c_char,
            );
        }
        memset(
            yy_buffer_stack.offset(yy_buffer_stack_max as isize) as *mut libc::c_void,
            0 as libc::c_int,
            (grow_size as usize).wrapping_mul(::core::mem::size_of::<*mut yy_buffer_state>()),
        );
        yy_buffer_stack_max = num_to_alloc;
    }
}
#[no_mangle]
pub unsafe extern "C" fn yy_scan_buffer(
    mut base: *mut libc::c_char,
    mut size: yy_size_t,
) -> YY_BUFFER_STATE {
    let mut b: YY_BUFFER_STATE = 0 as *mut yy_buffer_state;
    if size < 2 as libc::c_int as yy_size_t
        || *base.offset(size.wrapping_sub(2 as libc::c_int as yy_size_t) as isize) as libc::c_int
            != 0 as libc::c_int
        || *base.offset(size.wrapping_sub(1 as libc::c_int as yy_size_t) as isize) as libc::c_int
            != 0 as libc::c_int
    {
        return 0 as YY_BUFFER_STATE;
    }
    b = yyalloc(::core::mem::size_of::<yy_buffer_state>() as libc::c_ulong) as YY_BUFFER_STATE;
    if b.is_null() {
        yy_fatal_error(
            b"out of dynamic memory in yy_scan_buffer()\0" as *const u8 as *const libc::c_char,
        );
    }
    (*b).yy_buf_size = size.wrapping_sub(2 as libc::c_int as yy_size_t) as libc::c_int;
    (*b).yy_ch_buf = base;
    (*b).yy_buf_pos = (*b).yy_ch_buf;
    (*b).yy_is_our_buffer = 0 as libc::c_int;
    (*b).yy_input_file = 0 as *mut FILE;
    (*b).yy_n_chars = (*b).yy_buf_size as yy_size_t;
    (*b).yy_is_interactive = 0 as libc::c_int;
    (*b).yy_at_bol = 1 as libc::c_int;
    (*b).yy_fill_buffer = 0 as libc::c_int;
    (*b).yy_buffer_status = 0 as libc::c_int;
    yy_switch_to_buffer(b);
    return b;
}
#[no_mangle]
pub unsafe extern "C" fn yy_scan_string(mut yystr: *const libc::c_char) -> YY_BUFFER_STATE {
    return yy_scan_bytes(yystr, strlen(yystr) as yy_size_t);
}
#[no_mangle]
pub unsafe extern "C" fn yy_scan_bytes(
    mut yybytes: *const libc::c_char,
    mut _yybytes_len: yy_size_t,
) -> YY_BUFFER_STATE {
    let mut b: YY_BUFFER_STATE = 0 as *mut yy_buffer_state;
    let mut buf: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut n: yy_size_t = 0;
    let mut i: yy_size_t = 0;
    n = _yybytes_len.wrapping_add(2 as libc::c_int as yy_size_t);
    buf = yyalloc(n) as *mut libc::c_char;
    if buf.is_null() {
        yy_fatal_error(
            b"out of dynamic memory in yy_scan_bytes()\0" as *const u8 as *const libc::c_char,
        );
    }
    i = 0 as libc::c_int as yy_size_t;
    while i < _yybytes_len {
        *buf.offset(i as isize) = *yybytes.offset(i as isize);
        i = i.wrapping_add(1);
    }
    let ref mut fresh13 =
        *buf.offset(_yybytes_len.wrapping_add(1 as libc::c_int as yy_size_t) as isize);
    *fresh13 = 0 as libc::c_int as libc::c_char;
    *buf.offset(_yybytes_len as isize) = *fresh13;
    b = yy_scan_buffer(buf, n);
    if b.is_null() {
        yy_fatal_error(b"bad buffer in yy_scan_bytes()\0" as *const u8 as *const libc::c_char);
    }
    (*b).yy_is_our_buffer = 1 as libc::c_int;
    return b;
}
unsafe extern "C" fn yy_fatal_error(mut msg: *const libc::c_char) -> ! {
    fprintf(
        __stderrp,
        b"%s\n\0" as *const u8 as *const libc::c_char,
        msg,
    );
    exit(2 as libc::c_int);
}
#[no_mangle]
pub unsafe extern "C" fn yyget_lineno() -> libc::c_int {
    return yylineno;
}
#[no_mangle]
pub unsafe extern "C" fn yyget_in() -> *mut FILE {
    return yyin;
}
#[no_mangle]
pub unsafe extern "C" fn yyget_out() -> *mut FILE {
    return yyout;
}
#[no_mangle]
pub unsafe extern "C" fn yyget_leng() -> yy_size_t {
    return yyleng;
}
#[no_mangle]
pub unsafe extern "C" fn yyget_text() -> *mut libc::c_char {
    return yytext;
}
#[no_mangle]
pub unsafe extern "C" fn yyset_lineno(mut _line_number: libc::c_int) {
    yylineno = _line_number;
}

#[no_mangle]
pub unsafe extern "C" fn yyset_in(mut _in_str: *mut FILE) {
    yyin = _in_str;
}

#[no_mangle]
pub unsafe extern "C" fn yyset_out(mut _out_str: *mut FILE) {
    yyout = _out_str;
}

#[no_mangle]
pub unsafe extern "C" fn yyget_debug() -> libc::c_int {
    return yy_flex_debug;
}

#[no_mangle]
pub unsafe extern "C" fn yyset_debug(mut _bdebug: libc::c_int) {
    yy_flex_debug = _bdebug;
}

unsafe extern "C" fn yy_init_globals() -> libc::c_int {
    yy_buffer_stack = 0 as *mut YY_BUFFER_STATE;
    yy_buffer_stack_top = 0 as libc::c_int as size_t;
    yy_buffer_stack_max = 0 as libc::c_int as size_t;
    yy_c_buf_p = 0 as *mut libc::c_char;
    yy_init = 0 as libc::c_int;
    yy_start = 0 as libc::c_int;
    yyin = 0 as *mut FILE;
    yyout = 0 as *mut FILE;
    return 0 as libc::c_int;
}

#[no_mangle]
pub unsafe extern "C" fn yylex_destroy() -> libc::c_int {
    while !if !yy_buffer_stack.is_null() {
        *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
    } else {
        0 as YY_BUFFER_STATE
    }
    .is_null()
    {
        yy_delete_buffer(if !yy_buffer_stack.is_null() {
            *yy_buffer_stack.offset(yy_buffer_stack_top as isize)
        } else {
            0 as YY_BUFFER_STATE
        });
        let ref mut fresh14 = *yy_buffer_stack.offset(yy_buffer_stack_top as isize);
        *fresh14 = 0 as YY_BUFFER_STATE;
        yypop_buffer_state();
    }
    yyfree(yy_buffer_stack as *mut libc::c_void);
    yy_buffer_stack = 0 as *mut YY_BUFFER_STATE;
    yy_init_globals();
    return 0 as libc::c_int;
}

#[no_mangle]
pub unsafe extern "C" fn yyalloc(mut size: yy_size_t) -> *mut libc::c_void {
    return malloc(size as usize);
}

#[no_mangle]
pub unsafe extern "C" fn yyrealloc(
    mut ptr: *mut libc::c_void,
    mut size: yy_size_t,
) -> *mut libc::c_void {
    return realloc(ptr, size as usize);
}

#[no_mangle]
pub unsafe extern "C" fn yyfree(mut ptr: *mut libc::c_void) {
    free(ptr as *mut libc::c_char as *mut libc::c_void);
}
