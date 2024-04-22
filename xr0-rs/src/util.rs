use std::borrow::Borrow;
use std::ffi::CStr;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::Hash;
use std::ptr;

use libc::{free, malloc, realloc, strcmp, strlen, strncpy};

pub struct Map {
    entry: *mut Entry,
    n: libc::c_int,
}

struct Entry {
    key: *mut libc::c_char,
    value: *const libc::c_void,
}

pub struct StrBuilder {
    s: String,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    #[allow(dead_code)]
    pub inner: *mut Error,
}

pub type Result<T, E = Box<Error>> = std::result::Result<T, E>;

pub struct StringArr {
    pub n: libc::c_int,
    pub s: *mut *mut libc::c_char,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

pub unsafe extern "C" fn dynamic_str(s: *const libc::c_char) -> *mut libc::c_char {
    let len = strlen(s).wrapping_add(1);
    let t: *mut libc::c_char =
        malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul(len)) as *mut libc::c_char;
    strncpy(t, s, len);
    t
}

pub fn to_c_str(s: &[u8]) -> *mut libc::c_char {
    let len = s.len();
    unsafe {
        let t = malloc(len + 1) as *mut libc::c_char;
        assert!(!t.is_null());
        libc::memcpy(
            t as *mut libc::c_void,
            &s[0] as *const u8 as *const libc::c_void,
            len,
        );
        *t.add(len) = 0;
        t
    }
}

unsafe fn entry_create(key: *const libc::c_char, value: *const libc::c_void) -> Entry {
    assert!(!key.is_null());
    Entry {
        key: key as *mut libc::c_char,
        value,
    }
}

unsafe fn entry_destroy(e: &Entry) {
    free(e.key as *mut libc::c_void);
}

impl Map {
    pub fn new() -> Box<Map> {
        Box::new(Map {
            entry: ptr::null_mut(),
            n: 0,
        })
    }

    #[allow(clippy::boxed_local)]
    pub unsafe fn destroy(self: Box<Map>) {
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < self.n {
            entry_destroy(&*(self.entry).offset(i as isize));
            i += 1;
        }
        free(self.entry as *mut libc::c_void);
    }

    unsafe fn getindex(&self, key: *const libc::c_char) -> libc::c_int {
        if key.is_null() {
            panic!("key is null");
        }
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < self.n {
            if strcmp((*self.entry.offset(i as isize)).key, key) == 0 as libc::c_int {
                return i;
            }
            i += 1;
        }
        -1
    }

    pub unsafe fn get(&self, key: *const libc::c_char) -> *mut libc::c_void {
        let index = self.getindex(key);
        if index != -1 {
            return (*self.entry.offset(index as isize)).value as *mut libc::c_void;
        }
        ptr::null_mut()
    }

    pub unsafe fn set(&mut self, key: *const libc::c_char, value: *const libc::c_void) {
        let index = self.getindex(key);
        if index >= 0 {
            (*self.entry.offset(index as isize)).value = value;
            return;
        }
        self.n += 1;
        self.entry = realloc(
            self.entry as *mut libc::c_void,
            (::core::mem::size_of::<Entry>() as libc::c_ulong).wrapping_mul(self.n as libc::c_ulong)
                as usize,
        ) as *mut Entry;
        *self.entry.offset((self.n - 1 as libc::c_int) as isize) = entry_create(key, value);
    }

    pub fn len(&self) -> libc::c_int {
        self.n
    }

    fn table(&self) -> &[Entry] {
        if self.n == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(self.entry, self.n as usize) }
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = *const libc::c_char> + '_ {
        self.table().iter().map(|e| e.key as *const libc::c_char)
    }

    pub fn values(&self) -> impl Iterator<Item = *const libc::c_void> + '_ {
        self.table().iter().map(|e| e.value)
    }

    pub fn pairs(&self) -> impl Iterator<Item = (*const libc::c_char, *const libc::c_void)> + '_ {
        self.table()
            .iter()
            .map(|e| (e.key as *const libc::c_char, e.value))
    }

    pub fn pairs_mut(
        &mut self,
    ) -> impl Iterator<Item = (*const libc::c_char, &'_ mut *const libc::c_void)> + '_ {
        let table = if self.n == 0 {
            &mut []
        } else {
            unsafe { std::slice::from_raw_parts_mut(self.entry, self.n as usize) }
        };
        table
            .iter_mut()
            .map(|e| (e.key as *const libc::c_char, &mut e.value))
    }
}

pub unsafe fn strbuilder_create() -> *mut StrBuilder {
    Box::into_raw(Box::new(StrBuilder {
        s: String::with_capacity(100),
    }))
}

pub unsafe fn strbuilder_build(b: *mut StrBuilder) -> OwningCStr {
    let mut v = Box::from_raw(b).s.into_bytes();
    v.push(0);
    v.shrink_to_fit();
    OwningCStr::new(v.leak().as_mut_ptr() as *mut libc::c_char)
}

pub unsafe fn strbuilder_append_string(b: *mut StrBuilder, s: String) {
    (*b).s.push_str(&s);
}

pub unsafe fn strbuilder_putc(b: *mut StrBuilder, c: libc::c_char) {
    (*b).s.push(c as u8 as char);
}

#[macro_export]
macro_rules! strbuilder_write {
    ($b:expr, $($fmt:tt)+) => {
        $crate::util::strbuilder_append_string($b, format!($($fmt)+))
    }
}

#[macro_export]
macro_rules! cstr {
    ($c:expr) => {{
        let c = $c;
        if c.is_null() {
            std::borrow::Cow::Borrowed("(null)")
        } else {
            std::ffi::CStr::from_ptr(c).to_string_lossy()
        }
    }};
}

impl Error {
    pub fn new(s: String) -> Box<Error> {
        Box::new(Error {
            msg: s,
            inner: ptr::null_mut(),
        })
    }
}

pub unsafe fn string_arr_create() -> Box<StringArr> {
    Box::new(StringArr {
        s: ptr::null_mut(),
        n: 0,
    })
}

pub unsafe fn string_arr_append(arr: &mut StringArr, s: *mut libc::c_char) -> libc::c_int {
    arr.n += 1;
    arr.s = realloc(
        arr.s as *mut libc::c_void,
        (::core::mem::size_of::<StringArr>()).wrapping_mul(arr.n as usize),
    ) as *mut *mut libc::c_char;
    if arr.s.is_null() {
        panic!();
    }
    let loc: libc::c_int = arr.n - 1 as libc::c_int;
    *arr.s.offset(loc as isize) = s;
    loc
}

pub unsafe fn string_arr_deque(arr: &mut StringArr) -> *mut libc::c_char {
    let ret: *mut libc::c_char = dynamic_str(*arr.s.offset(0 as libc::c_int as isize));
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < arr.n - 1 as libc::c_int {
        let s = *arr.s.offset((i + 1 as libc::c_int) as isize);
        *(arr.s).offset(i as isize) = s;
        i += 1;
    }
    arr.n -= 1;
    arr.s = realloc(
        arr.s as *mut libc::c_void,
        (::core::mem::size_of::<*mut libc::c_char>()).wrapping_mul(arr.n as usize),
    ) as *mut *mut libc::c_char;
    ret
}

pub unsafe fn string_arr_contains(arr: &StringArr, s: *mut libc::c_char) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < arr.n {
        if strcmp(s, *arr.s.offset(i as isize)) == 0 as libc::c_int {
            return true;
        }
        i += 1;
    }
    false
}

pub static mut VERBOSE_MODE: libc::c_int = 0;

#[macro_export]
macro_rules! vprintln {
    ( $( $args:tt )* ) => {
        if $crate::util::VERBOSE_MODE != 0 {
            println!($($args)*);
        }
    }
}

pub struct OwningCStr {
    ptr: *mut libc::c_char,
}

impl OwningCStr {
    pub unsafe fn new(ptr: *mut libc::c_char) -> Self {
        assert!(!ptr.is_null());
        if let Err(err) = CStr::from_ptr(ptr).to_str() {
            panic!("non-UTF-8 string {:?}: {err}", CStr::from_ptr(ptr));
        }
        OwningCStr { ptr }
    }

    pub unsafe fn empty() -> Self {
        OwningCStr {
            ptr: dynamic_str(b"\0" as *const u8 as *const libc::c_char),
        }
    }

    pub fn is_empty(&self) -> bool {
        unsafe { CStr::from_ptr(self.ptr).is_empty() }
    }

    pub fn copy(ptr: &CStr) -> Self {
        unsafe { Self::new(dynamic_str(ptr.as_ptr())) }
    }

    pub fn as_ptr(&self) -> *mut libc::c_char {
        self.ptr
    }

    pub fn into_ptr(self) -> *mut libc::c_char {
        let p = self.ptr;
        std::mem::forget(self);
        p
    }

    pub fn as_str(&self) -> &str {
        unsafe { CStr::from_ptr(self.ptr).to_str().unwrap() }
    }
}

impl Display for OwningCStr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Debug for OwningCStr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

impl PartialEq for OwningCStr {
    fn eq(&self, other: &OwningCStr) -> bool {
        unsafe { CStr::from_ptr(self.ptr) == CStr::from_ptr(other.ptr) }
    }
}

impl Drop for OwningCStr {
    fn drop(&mut self) {
        unsafe {
            free(self.ptr as *mut libc::c_void);
        }
    }
}

impl Clone for OwningCStr {
    fn clone(&self) -> OwningCStr {
        OwningCStr {
            ptr: unsafe { dynamic_str(self.ptr) },
        }
    }
}

/// Map that keeps entries in insertion order.
/// Implemented as an alist for now.
#[derive(Clone)]
pub struct InsertionOrderMap<K, V> {
    items: Vec<(K, V)>,
}

impl<K, V> Default for InsertionOrderMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> InsertionOrderMap<K, V> {
    pub fn new() -> Self {
        InsertionOrderMap { items: vec![] }
    }

    pub fn insert(&mut self, key: K, value: V)
    where
        K: Hash + Eq,
    {
        for (k, v) in &mut self.items {
            if *k == key {
                *v = value;
                return;
            }
        }
        self.items.push((key, value));
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Eq + ?Sized,
        K: Hash + Eq + Borrow<Q>,
    {
        for (k, v) in &self.items {
            if k.borrow() == key {
                return Some(v);
            }
        }
        None
    }
}

impl<'a, K, V> IntoIterator for &'a InsertionOrderMap<K, V> {
    type IntoIter = Iter<'a, K, V>;
    type Item = (&'a K, &'a V);

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            inner: self.items.iter(),
        }
    }
}

pub struct Iter<'a, K, V> {
    inner: std::slice::Iter<'a, (K, V)>,
}

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, v)| (k, v))
    }
}
