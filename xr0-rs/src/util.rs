#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case, non_upper_case_globals, unused_assignments, unused_mut)]
#![feature(c_variadic, extern_types)]
extern "C" {
    pub type __sFILEX;
    fn fclose(_: *mut FILE) -> libc::c_int;
    fn vfprintf(
        _: *mut FILE,
        _: *const libc::c_char,
        _: ::core::ffi::VaList,
    ) -> libc::c_int;
    fn vprintf(_: *const libc::c_char, _: ::core::ffi::VaList) -> libc::c_int;
    fn snprintf(
        _: *mut libc::c_char,
        _: libc::c_ulong,
        _: *const libc::c_char,
        _: ...
    ) -> libc::c_int;
    fn open_memstream(__bufp: *mut *mut libc::c_char, __sizep: *mut size_t) -> *mut FILE;
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn strcat(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
    fn strcpy(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    fn strncpy(
        _: *mut libc::c_char,
        _: *const libc::c_char,
        _: libc::c_ulong,
    ) -> *mut libc::c_char;
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
}
pub type __builtin_va_list = [__va_list_tag; 1];
#[derive(Copy, Clone)]
#[repr(C)]
pub struct __va_list_tag {
    pub gp_offset: libc::c_uint,
    pub fp_offset: libc::c_uint,
    pub overflow_arg_area: *mut libc::c_void,
    pub reg_save_area: *mut libc::c_void,
}
pub type __int64_t = libc::c_longlong;
pub type __darwin_size_t = libc::c_ulong;
pub type __darwin_va_list = __builtin_va_list;
pub type __darwin_off_t = __int64_t;
pub type va_list = __darwin_va_list;
pub type size_t = __darwin_size_t;
pub type fpos_t = __darwin_off_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct __sbuf {
    pub _base: *mut libc::c_uchar,
    pub _size: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct __sFILE {
    pub _p: *mut libc::c_uchar,
    pub _r: libc::c_int,
    pub _w: libc::c_int,
    pub _flags: libc::c_short,
    pub _file: libc::c_short,
    pub _bf: __sbuf,
    pub _lbfsize: libc::c_int,
    pub _cookie: *mut libc::c_void,
    pub _close: Option::<unsafe extern "C" fn(*mut libc::c_void) -> libc::c_int>,
    pub _read: Option::<
        unsafe extern "C" fn(
            *mut libc::c_void,
            *mut libc::c_char,
            libc::c_int,
        ) -> libc::c_int,
    >,
    pub _seek: Option::<
        unsafe extern "C" fn(*mut libc::c_void, fpos_t, libc::c_int) -> fpos_t,
    >,
    pub _write: Option::<
        unsafe extern "C" fn(
            *mut libc::c_void,
            *const libc::c_char,
            libc::c_int,
        ) -> libc::c_int,
    >,
    pub _ub: __sbuf,
    pub _extra: *mut __sFILEX,
    pub _ur: libc::c_int,
    pub _ubuf: [libc::c_uchar; 3],
    pub _nbuf: [libc::c_uchar; 1],
    pub _lb: __sbuf,
    pub _blksize: libc::c_int,
    pub _offset: fpos_t,
}
pub type FILE = __sFILE;
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
pub struct strbuilder {
    pub cap: size_t,
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
pub unsafe extern "C" fn dynamic_str(mut s: *const libc::c_char) -> *mut libc::c_char {
    let mut len: libc::c_int = (strlen(s))
        .wrapping_add(1 as libc::c_int as libc::c_ulong) as libc::c_int;
    let mut t: *mut libc::c_char = malloc(
        (::core::mem::size_of::<libc::c_char>() as libc::c_ulong)
            .wrapping_mul(len as libc::c_ulong),
    ) as *mut libc::c_char;
    strncpy(t, s, len as libc::c_ulong);
    return t;
}
unsafe extern "C" fn entry_create(
    mut key: *const libc::c_char,
    mut value: *const libc::c_void,
) -> entry {
    if key.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 13],
                &[libc::c_char; 13],
            >(b"entry_create\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            22 as libc::c_int,
            b"key != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    return {
        let mut init = entry {
            key: key as *mut libc::c_char,
            value: value,
        };
        init
    };
}
unsafe extern "C" fn entry_destroy(mut e: entry) {
    free(e.key as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn map_create() -> *mut map {
    return calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<map>() as libc::c_ulong,
    ) as *mut map;
}
#[no_mangle]
pub unsafe extern "C" fn map_destroy(mut map: *mut map) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*map).n {
        entry_destroy(*((*map).entry).offset(i as isize));
        i += 1;
        i;
    }
    free((*map).entry as *mut libc::c_void);
    free(map as *mut libc::c_void);
}
unsafe extern "C" fn map_getindex(
    mut map: *mut map,
    mut key: *const libc::c_char,
) -> libc::c_int {
    if key.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 13],
                &[libc::c_char; 13],
            >(b"map_getindex\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            51 as libc::c_int,
            b"key != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*map).n {
        if strcmp((*((*map).entry).offset(i as isize)).key, key) == 0 as libc::c_int {
            return i;
        }
        i += 1;
        i;
    }
    return -(1 as libc::c_int);
}
#[no_mangle]
pub unsafe extern "C" fn map_get(
    mut map: *mut map,
    mut key: *const libc::c_char,
) -> *mut libc::c_void {
    let mut index: libc::c_int = map_getindex(map, key);
    if index != -(1 as libc::c_int) {
        return (*((*map).entry).offset(index as isize)).value as *mut libc::c_void;
    }
    return 0 as *mut libc::c_void;
}
#[no_mangle]
pub unsafe extern "C" fn map_set(
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
    (*map)
        .entry = realloc(
        (*map).entry as *mut libc::c_void,
        (::core::mem::size_of::<entry>() as libc::c_ulong)
            .wrapping_mul((*map).n as libc::c_ulong),
    ) as *mut entry;
    *((*map).entry)
        .offset(((*map).n - 1 as libc::c_int) as isize) = entry_create(key, value);
}
#[no_mangle]
pub unsafe extern "C" fn strbuilder_create() -> *mut strbuilder {
    let mut b: *mut strbuilder = malloc(
        ::core::mem::size_of::<strbuilder>() as libc::c_ulong,
    ) as *mut strbuilder;
    (*b).cap = 100 as libc::c_int as size_t;
    (*b)
        .buf = malloc(
        (::core::mem::size_of::<libc::c_char>() as libc::c_ulong).wrapping_mul((*b).cap),
    ) as *mut libc::c_char;
    *((*b).buf).offset(0 as libc::c_int as isize) = '\0' as i32 as libc::c_char;
    return b;
}
#[no_mangle]
pub unsafe extern "C" fn strbuilder_build(mut b: *mut strbuilder) -> *mut libc::c_char {
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 17],
                &[libc::c_char; 17],
            >(b"strbuilder_build\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            105 as libc::c_int,
            b"b != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    let mut len: libc::c_int = (strlen((*b).buf))
        .wrapping_add(1 as libc::c_int as libc::c_ulong) as libc::c_int;
    let mut s: *mut libc::c_char = malloc(
        (::core::mem::size_of::<libc::c_char>() as libc::c_ulong)
            .wrapping_mul(len as libc::c_ulong),
    ) as *mut libc::c_char;
    snprintf(
        s,
        len as libc::c_ulong,
        b"%s\0" as *const u8 as *const libc::c_char,
        (*b).buf,
    );
    free((*b).buf as *mut libc::c_void);
    free(b as *mut libc::c_void);
    return s;
}
#[no_mangle]
pub unsafe extern "C" fn strbuilder_preview(
    mut b: *mut strbuilder,
) -> *mut libc::c_char {
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 19],
                &[libc::c_char; 19],
            >(b"strbuilder_preview\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            117 as libc::c_int,
            b"b != NULL\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    let mut len: libc::c_int = (strlen((*b).buf))
        .wrapping_add(1 as libc::c_int as libc::c_ulong) as libc::c_int;
    let mut s: *mut libc::c_char = malloc(
        (::core::mem::size_of::<libc::c_char>() as libc::c_ulong)
            .wrapping_mul(len as libc::c_ulong),
    ) as *mut libc::c_char;
    snprintf(
        s,
        len as libc::c_ulong,
        b"%s\0" as *const u8 as *const libc::c_char,
        (*b).buf,
    );
    return s;
}
unsafe extern "C" fn strbuilder_realloc(mut b: *mut strbuilder, mut len: size_t) {
    while (*b).cap <= len {
        (*b).cap = (*b).cap * 2 as libc::c_int as size_t;
        (*b)
            .buf = realloc(
            (*b).buf as *mut libc::c_void,
            (::core::mem::size_of::<libc::c_char>() as libc::c_ulong)
                .wrapping_mul((*b).cap),
        ) as *mut libc::c_char;
    }
}
unsafe extern "C" fn strbuilder_append(
    mut b: *mut strbuilder,
    mut s: *mut libc::c_char,
    mut len: size_t,
) {
    let mut buflen: libc::c_int = strlen((*b).buf) as libc::c_int;
    strbuilder_realloc(b, (buflen as size_t).wrapping_add(len));
    let mut newlen: size_t = (buflen as size_t)
        .wrapping_add(len)
        .wrapping_add(1 as libc::c_int as size_t);
    strncpy(
        ((*b).buf).offset(buflen as isize),
        s,
        newlen.wrapping_sub(buflen as size_t),
    );
}
#[no_mangle]
pub unsafe extern "C" fn strbuilder_vprintf(
    mut b: *mut strbuilder,
    mut fmt: *const libc::c_char,
    mut ap: ::core::ffi::VaList,
) -> libc::c_int {
    let mut len: size_t = 0;
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
    mut b: *mut strbuilder,
    mut fmt: *const libc::c_char,
    mut args: ...
) -> libc::c_int {
    let mut ap: ::core::ffi::VaListImpl;
    ap = args.clone();
    let mut r: libc::c_int = strbuilder_vprintf(b, fmt, ap.as_va_list());
    return r;
}
#[no_mangle]
pub unsafe extern "C" fn strbuilder_puts(
    mut b: *mut strbuilder,
    mut s: *mut libc::c_char,
) -> libc::c_int {
    let mut len: size_t = strlen(s);
    strbuilder_append(b, s, len);
    return len as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn strbuilder_putc(mut b: *mut strbuilder, mut c: libc::c_char) {
    strbuilder_printf(b, b"%c\0" as *const u8 as *const libc::c_char, c as libc::c_int);
}
#[no_mangle]
pub unsafe extern "C" fn error_create(mut s: *mut libc::c_char) -> *mut error {
    let mut err: *mut error = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<error>() as libc::c_ulong,
    ) as *mut error;
    (*err).msg = s;
    return err;
}
#[no_mangle]
pub unsafe extern "C" fn error_prepend(
    mut e: *mut error,
    mut prefix: *mut libc::c_char,
) -> *mut error {
    let mut new_len: libc::c_int = (strlen(prefix))
        .wrapping_add(strlen((*e).msg))
        .wrapping_add(1 as libc::c_int as libc::c_ulong)
        .wrapping_add(1 as libc::c_int as libc::c_ulong) as libc::c_int;
    let mut new_msg: *mut libc::c_char = malloc(new_len as libc::c_ulong)
        as *mut libc::c_char;
    strcpy(new_msg, prefix);
    strcat(new_msg, (*e).msg);
    strcat(new_msg, b"\n\0" as *const u8 as *const libc::c_char);
    free(e as *mut libc::c_void);
    return error_create(new_msg);
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_create() -> *mut string_arr {
    let mut arr: *mut string_arr = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<string_arr>() as libc::c_ulong,
    ) as *mut string_arr;
    if arr.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 18],
                &[libc::c_char; 18],
            >(b"string_arr_create\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            211 as libc::c_int,
            b"arr\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    return arr;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_destroy(mut arr: *mut string_arr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        free(*((*arr).s).offset(i as isize) as *mut libc::c_void);
        i += 1;
        i;
    }
    free((*arr).s as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_s(
    mut arr: *mut string_arr,
) -> *mut *mut libc::c_char {
    return (*arr).s;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_n(mut arr: *mut string_arr) -> libc::c_int {
    return (*arr).n;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_append(
    mut arr: *mut string_arr,
    mut s: *mut libc::c_char,
) -> libc::c_int {
    (*arr).n += 1;
    (*arr)
        .s = realloc(
        (*arr).s as *mut libc::c_void,
        (::core::mem::size_of::<string_arr>() as libc::c_ulong)
            .wrapping_mul((*arr).n as libc::c_ulong),
    ) as *mut *mut libc::c_char;
    if ((*arr).s).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 18],
                &[libc::c_char; 18],
            >(b"string_arr_append\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            242 as libc::c_int,
            b"arr->s\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    let mut loc: libc::c_int = (*arr).n - 1 as libc::c_int;
    let ref mut fresh1 = *((*arr).s).offset(loc as isize);
    *fresh1 = s;
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_copy(mut old: *mut string_arr) -> *mut string_arr {
    let mut new: *mut string_arr = string_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        string_arr_append(new, dynamic_str(*((*old).s).offset(i as isize)));
        i += 1;
        i;
    }
    return new;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_concat(
    mut s1: *mut string_arr,
    mut s2: *mut string_arr,
) -> *mut string_arr {
    if !(!s1.is_null() && !s2.is_null()) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 18],
                &[libc::c_char; 18],
            >(b"string_arr_concat\0"))
                .as_ptr(),
            b"util.c\0" as *const u8 as *const libc::c_char,
            261 as libc::c_int,
            b"s1 && s2\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    let mut new: *mut string_arr = string_arr_copy(s1);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*s2).n {
        string_arr_append(new, *((*s2).s).offset(i as isize));
        i += 1;
        i;
    }
    return new;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_deque(
    mut arr: *mut string_arr,
) -> *mut libc::c_char {
    let mut ret: *mut libc::c_char = dynamic_str(
        *((*arr).s).offset(0 as libc::c_int as isize),
    );
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n - 1 as libc::c_int {
        let ref mut fresh2 = *((*arr).s).offset(i as isize);
        *fresh2 = *((*arr).s).offset((i + 1 as libc::c_int) as isize);
        i += 1;
        i;
    }
    (*arr).n -= 1;
    (*arr)
        .s = realloc(
        (*arr).s as *mut libc::c_void,
        (::core::mem::size_of::<*mut libc::c_char>() as libc::c_ulong)
            .wrapping_mul((*arr).n as libc::c_ulong),
    ) as *mut *mut libc::c_char;
    return ret;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_contains(
    mut arr: *mut string_arr,
    mut s: *mut libc::c_char,
) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if strcmp(s, *((*arr).s).offset(i as isize)) == 0 as libc::c_int {
            return 1 as libc::c_int != 0;
        }
        i += 1;
        i;
    }
    return 0 as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn string_arr_str(
    mut string_arr: *mut string_arr,
) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
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
        i;
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub static mut VERBOSE_MODE: libc::c_int = 0;
#[no_mangle]
pub unsafe extern "C" fn v_printf(
    mut fmt: *mut libc::c_char,
    mut args: ...
) -> libc::c_int {
    if VERBOSE_MODE == 0 {
        return 0 as libc::c_int;
    }
    let mut ap: ::core::ffi::VaListImpl;
    ap = args.clone();
    let mut r: libc::c_int = vprintf(fmt, ap.as_va_list());
    return r;
}
