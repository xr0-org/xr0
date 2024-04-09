use libc::FILE;

extern "C" {
    pub static mut __stdinp: *mut libc::FILE;
    pub static mut __stdoutp: *mut libc::FILE;
    pub static mut __stderrp: *mut libc::FILE;

    pub fn __error() -> *mut libc::c_int;

    pub fn vfprintf(_: *mut FILE, _: *const libc::c_char, _: ::core::ffi::VaList) -> libc::c_int;
    pub fn vprintf(_: *const libc::c_char, _: ::core::ffi::VaList) -> libc::c_int;

    pub fn getopt(
        _: libc::c_int,
        _: *const *mut libc::c_char,
        _: *const libc::c_char,
    ) -> libc::c_int;
    pub static mut optarg: *mut libc::c_char;
    pub static mut optind: libc::c_int;

    pub fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;

    pub fn __maskrune(_: __darwin_ct_rune_t, _: libc::c_ulong) -> libc::c_int;
    pub static mut _DefaultRuneLocale: _RuneLocale;
}

pub type __uint32_t = libc::c_uint;
pub type __int64_t = libc::c_longlong;
pub type __darwin_ct_rune_t = libc::c_int;
pub type __darwin_size_t = libc::c_ulong;
pub type __darwin_wchar_t = libc::c_int;
pub type __darwin_rune_t = __darwin_wchar_t;
pub type __darwin_off_t = __int64_t;
pub type int16_t = libc::c_short;
pub type size_t = __darwin_size_t;
pub type fpos_t = __darwin_off_t;
pub type uint8_t = libc::c_uchar;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct _RuneEntry {
    pub __min: __darwin_rune_t,
    pub __max: __darwin_rune_t,
    pub __map: __darwin_rune_t,
    pub __types: *mut __uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _RuneRange {
    pub __nranges: libc::c_int,
    pub __ranges: *mut _RuneEntry,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _RuneCharClass {
    pub __name: [libc::c_char; 14],
    pub __mask: __uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _RuneLocale {
    pub __magic: [libc::c_char; 8],
    pub __encoding: [libc::c_char; 32],
    pub __sgetrune: Option<
        unsafe extern "C" fn(
            *const libc::c_char,
            __darwin_size_t,
            *mut *const libc::c_char,
        ) -> __darwin_rune_t,
    >,
    pub __sputrune: Option<
        unsafe extern "C" fn(
            __darwin_rune_t,
            *mut libc::c_char,
            __darwin_size_t,
            *mut *mut libc::c_char,
        ) -> libc::c_int,
    >,
    pub __invalid_rune: __darwin_rune_t,
    pub __runetype: [__uint32_t; 256],
    pub __maplower: [__darwin_rune_t; 256],
    pub __mapupper: [__darwin_rune_t; 256],
    pub __runetype_ext: _RuneRange,
    pub __maplower_ext: _RuneRange,
    pub __mapupper_ext: _RuneRange,
    pub __variable: *mut libc::c_void,
    pub __variable_len: libc::c_int,
    pub __ncharclasses: libc::c_int,
    pub __charclasses: *mut _RuneCharClass,
}
