use libc::FILE;

extern "C" {
    pub static mut __stdinp: *mut libc::FILE;
    pub static mut __stdoutp: *mut libc::FILE;
    pub static mut __stderrp: *mut libc::FILE;

    pub fn __error() -> *mut libc::c_int;

    pub fn vfprintf(_: *mut FILE, _: *const libc::c_char, _: ::core::ffi::VaList) -> libc::c_int;
}
