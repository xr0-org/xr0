#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::c_util::{__assert_rtn, vfprintf, vprintf};
use libc::{
    calloc, fclose, free, malloc, open_memstream, realloc, snprintf, strcat, strcmp, strcpy,
    strlen, strncpy, FILE,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct map {
    pub entry: *mut entry,
    pub n: libc::c_int,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct entry {
    pub key: *mut libc::c_char,
    pub value: *const libc::c_void,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct StrBuilder {
    pub cap: usize,
    pub buf: *mut libc::c_char,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct error {
    pub msg: *mut libc::c_char,
    pub inner: *mut error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct string_arr {
    pub n: libc::c_int,
    pub s: *mut *mut libc::c_char,
}

#[no_mangle]
pub unsafe fn dynamic_str(mut s: *const libc::c_char) -> *mut libc::c_char {
    let mut len = strlen(s).wrapping_add(1);
    let mut t: *mut libc::c_char =
        malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul(len)) as *mut libc::c_char;
    strncpy(t, s, len);
    return t;
}

unsafe fn entry_create(mut key: *const libc::c_char, mut value: *const libc::c_void) -> entry {
    assert!(!key.is_null());
    let mut init = entry {
        key: key as *mut libc::c_char,
        value: value,
    };
    init
}

unsafe fn entry_destroy(mut e: entry) {
    free(e.key as *mut libc::c_void);
}

#[no_mangle]
pub unsafe fn map_create() -> *mut map {
    return calloc(1, ::core::mem::size_of::<map>()) as *mut map;
}

#[no_mangle]
pub unsafe fn map_destroy(mut map: *mut map) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*map).n {
        entry_destroy(*((*map).entry).offset(i as isize));
        i += 1;
    }
    free((*map).entry as *mut libc::c_void);
    free(map as *mut libc::c_void);
}

unsafe fn map_getindex(mut map: *mut map, mut key: *const libc::c_char) -> libc::c_int {
    if key.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"map_getindex\0")).as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            51 as libc::c_int,
            b"key != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*map).n {
        if strcmp((*((*map).entry).offset(i as isize)).key, key) == 0 as libc::c_int {
            return i;
        }
        i += 1;
    }
    return -(1 as libc::c_int);
}

#[no_mangle]
pub unsafe fn map_get(mut map: *mut map, mut key: *const libc::c_char) -> *mut libc::c_void {
    let mut index: libc::c_int = map_getindex(map, key);
    if index != -(1 as libc::c_int) {
        return (*((*map).entry).offset(index as isize)).value as *mut libc::c_void;
    }
    return 0 as *mut libc::c_void;
}

#[no_mangle]
pub unsafe fn map_set(
    mut map: *mut map,
    mut key: *const libc::c_char,
    mut value: *const libc::c_void,
) {
    let mut index: libc::c_int = map_getindex(map, key);
    if index >= 0 as libc::c_int {
        let ref mut fresh0 = (*((*map).entry).offset(index as isize)).value;
        *fresh0 = value;
        return;
    }
    (*map).n += 1;
    (*map).entry = realloc(
        (*map).entry as *mut libc::c_void,
        (::core::mem::size_of::<entry>() as libc::c_ulong).wrapping_mul((*map).n as libc::c_ulong)
            as usize,
    ) as *mut entry;
    *((*map).entry).offset(((*map).n - 1 as libc::c_int) as isize) = entry_create(key, value);
}

#[no_mangle]
pub unsafe fn strbuilder_create() -> *mut StrBuilder {
    let mut b: *mut StrBuilder = malloc(::core::mem::size_of::<StrBuilder>()) as *mut StrBuilder;
    (*b).cap = 100 as libc::c_int as usize;
    (*b).buf = malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul((*b).cap))
        as *mut libc::c_char;
    *((*b).buf).offset(0 as libc::c_int as isize) = '\0' as i32 as libc::c_char;
    return b;
}

#[no_mangle]
pub unsafe fn strbuilder_build(mut b: *mut StrBuilder) -> *mut libc::c_char {
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 17], &[libc::c_char; 17]>(b"strbuilder_build\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            105 as libc::c_int,
            b"b != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut len = strlen((*b).buf).wrapping_add(1);
    let mut s: *mut libc::c_char =
        malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul(len)) as *mut libc::c_char;
    snprintf(
        s,
        len,
        b"%s\0" as *const u8 as *const libc::c_char,
        (*b).buf,
    );
    free((*b).buf as *mut libc::c_void);
    free(b as *mut libc::c_void);
    return s;
}

#[no_mangle]
pub unsafe fn strbuilder_preview(mut b: *mut StrBuilder) -> *mut libc::c_char {
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 19], &[libc::c_char; 19]>(b"strbuilder_preview\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            117 as libc::c_int,
            b"b != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut len = (strlen((*b).buf)).wrapping_add(1);
    let mut s: *mut libc::c_char =
        malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul(len)) as *mut libc::c_char;
    snprintf(
        s,
        len,
        b"%s\0" as *const u8 as *const libc::c_char,
        (*b).buf,
    );
    return s;
}

unsafe fn strbuilder_realloc(mut b: *mut StrBuilder, mut len: usize) {
    while (*b).cap <= len {
        (*b).cap = (*b).cap * 2 as libc::c_int as usize;
        (*b).buf = realloc(
            (*b).buf as *mut libc::c_void,
            (::core::mem::size_of::<libc::c_char>()).wrapping_mul((*b).cap),
        ) as *mut libc::c_char;
    }
}

unsafe fn strbuilder_append(mut b: *mut StrBuilder, mut s: *mut libc::c_char, mut len: usize) {
    let mut buflen: libc::c_int = strlen((*b).buf) as libc::c_int;
    strbuilder_realloc(b, (buflen as usize).wrapping_add(len));
    let mut newlen: usize = (buflen as usize)
        .wrapping_add(len)
        .wrapping_add(1 as libc::c_int as usize);
    strncpy(
        ((*b).buf).offset(buflen as isize),
        s,
        newlen.wrapping_sub(buflen as usize),
    );
}

#[no_mangle]
pub unsafe fn strbuilder_vprintf(
    mut b: *mut StrBuilder,
    mut fmt: *const libc::c_char,
    mut ap: ::core::ffi::VaList,
) -> libc::c_int {
    let mut len: usize = 0;
    let mut buf: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut out: *mut FILE = open_memstream(&mut buf, &mut len);
    let mut r: libc::c_int = vfprintf(out, fmt, ap.as_va_list());
    fclose(out);
    strbuilder_append(b, buf, len);
    free(buf as *mut libc::c_void);
    return r;
}

#[no_mangle]
pub unsafe extern "C" fn strbuilder_printf(
    mut b: *mut StrBuilder,
    mut fmt: *const libc::c_char,
    mut args: ...
) -> libc::c_int {
    let mut ap: ::core::ffi::VaListImpl;
    ap = args.clone();
    let mut r: libc::c_int = strbuilder_vprintf(b, fmt, ap.as_va_list());
    return r;
}

#[no_mangle]
pub unsafe fn strbuilder_puts(mut b: *mut StrBuilder, mut s: *mut libc::c_char) -> libc::c_int {
    let mut len: usize = strlen(s);
    strbuilder_append(b, s, len);
    return len as libc::c_int;
}

#[no_mangle]
pub unsafe fn strbuilder_putc(mut b: *mut StrBuilder, mut c: libc::c_char) {
    strbuilder_printf(
        b,
        b"%c\0" as *const u8 as *const libc::c_char,
        c as libc::c_int,
    );
}

#[no_mangle]
pub unsafe fn error_create(mut s: *mut libc::c_char) -> *mut error {
    let mut err: *mut error = calloc(1, ::core::mem::size_of::<error>()) as *mut error;
    (*err).msg = s;
    return err;
}

#[no_mangle]
pub unsafe fn error_prepend(mut e: *mut error, mut prefix: *mut libc::c_char) -> *mut error {
    let mut new_len: libc::c_int = (strlen(prefix))
        .wrapping_add(strlen((*e).msg))
        .wrapping_add(1)
        .wrapping_add(1) as libc::c_int;
    let mut new_msg: *mut libc::c_char = malloc(new_len as usize) as *mut libc::c_char;
    strcpy(new_msg, prefix);
    strcat(new_msg, (*e).msg);
    strcat(new_msg, b"\n\0" as *const u8 as *const libc::c_char);
    free(e as *mut libc::c_void);
    return error_create(new_msg);
}

#[no_mangle]
pub unsafe fn string_arr_create() -> *mut string_arr {
    let mut arr: *mut string_arr =
        calloc(1, ::core::mem::size_of::<string_arr>()) as *mut string_arr;
    if arr.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"string_arr_create\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            211 as libc::c_int,
            b"arr\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return arr;
}

#[no_mangle]
pub unsafe fn string_arr_destroy(mut arr: *mut string_arr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        free(*((*arr).s).offset(i as isize) as *mut libc::c_void);
        i += 1;
    }
    free((*arr).s as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}

#[no_mangle]
pub unsafe fn string_arr_s(mut arr: *mut string_arr) -> *mut *mut libc::c_char {
    return (*arr).s;
}

#[no_mangle]
pub unsafe fn string_arr_n(mut arr: *mut string_arr) -> libc::c_int {
    return (*arr).n;
}

#[no_mangle]
pub unsafe fn string_arr_append(mut arr: *mut string_arr, mut s: *mut libc::c_char) -> libc::c_int {
    (*arr).n += 1;
    (*arr).s = realloc(
        (*arr).s as *mut libc::c_void,
        (::core::mem::size_of::<string_arr>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut libc::c_char;
    if ((*arr).s).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"string_arr_append\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            242 as libc::c_int,
            b"arr->s\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut loc: libc::c_int = (*arr).n - 1 as libc::c_int;
    let ref mut fresh1 = *((*arr).s).offset(loc as isize);
    *fresh1 = s;
    return loc;
}

#[no_mangle]
pub unsafe fn string_arr_copy(mut old: *mut string_arr) -> *mut string_arr {
    let mut new: *mut string_arr = string_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        string_arr_append(new, dynamic_str(*((*old).s).offset(i as isize)));
        i += 1;
    }
    return new;
}

#[no_mangle]
pub unsafe fn string_arr_concat(
    mut s1: *mut string_arr,
    mut s2: *mut string_arr,
) -> *mut string_arr {
    if !(!s1.is_null() && !s2.is_null()) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"string_arr_concat\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            261 as libc::c_int,
            b"s1 && s2\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut new: *mut string_arr = string_arr_copy(s1);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*s2).n {
        string_arr_append(new, *((*s2).s).offset(i as isize));
        i += 1;
    }
    return new;
}

#[no_mangle]
pub unsafe fn string_arr_deque(mut arr: *mut string_arr) -> *mut libc::c_char {
    let mut ret: *mut libc::c_char = dynamic_str(*((*arr).s).offset(0 as libc::c_int as isize));
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n - 1 as libc::c_int {
        let ref mut fresh2 = *((*arr).s).offset(i as isize);
        *fresh2 = *((*arr).s).offset((i + 1 as libc::c_int) as isize);
        i += 1;
    }
    (*arr).n -= 1;
    (*arr).s = realloc(
        (*arr).s as *mut libc::c_void,
        (::core::mem::size_of::<*mut libc::c_char>() as libc::c_ulong)
            .wrapping_mul((*arr).n as libc::c_ulong) as usize,
    ) as *mut *mut libc::c_char;
    return ret;
}

#[no_mangle]
pub unsafe fn string_arr_contains(mut arr: *mut string_arr, mut s: *mut libc::c_char) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if strcmp(s, *((*arr).s).offset(i as isize)) == 0 as libc::c_int {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}

#[no_mangle]
pub unsafe fn string_arr_str(mut string_arr: *mut string_arr) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut s: *mut *mut libc::c_char = (*string_arr).s;
    let mut n: libc::c_int = (*string_arr).n;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut str: *mut libc::c_char = *s.offset(i as isize);
        strbuilder_printf(
            b,
            b"%s%s\0" as *const u8 as *const libc::c_char,
            str,
            if (i + 1 as libc::c_int) < n {
                b", \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            },
        );
        i += 1;
    }
    return strbuilder_build(b);
}

#[no_mangle]
pub static mut VERBOSE_MODE: libc::c_int = 0;

#[no_mangle]
pub unsafe extern "C" fn v_printf(mut fmt: *mut libc::c_char, mut args: ...) -> libc::c_int {
    if VERBOSE_MODE == 0 {
        return 0 as libc::c_int;
    }
    let mut ap: ::core::ffi::VaListImpl;
    ap = args.clone();
    let mut r: libc::c_int = vprintf(fmt, ap.as_va_list());
    return r;
}
