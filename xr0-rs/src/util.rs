use std::sync::atomic::AtomicBool;
use std::borrow::Borrow;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Deref;

pub struct StrBuilder {
    s: String,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
}

pub type Result<T, E = Box<Error>> = std::result::Result<T, E>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

/// Like Box<T>, but not necessarily owning; possibly borrowed. Useful when hacking together
/// existing AST bits with twine and duct tape to make a temporary new AST for something.
#[derive(Clone)]
pub enum SemiBox<'a, T> {
    Owned(Box<T>),
    Borrowed(&'a T),
}

impl<'a, T> Deref for SemiBox<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        match self {
            SemiBox::Owned(b) => b,
            SemiBox::Borrowed(r) => r,
        }
    }
}

impl<'a, T> SemiBox<'a, T> {
    pub fn reborrow<'parent>(self: &'parent SemiBox<'a, T>) -> SemiBox<'parent, T> {
        match self {
            SemiBox::Owned(b) => SemiBox::Borrowed(&**b),
            SemiBox::Borrowed(r) => SemiBox::Borrowed(r),
        }
    }
}

pub fn strbuilder_create() -> StrBuilder {
    StrBuilder {
        s: String::with_capacity(100),
    }
}

pub fn strbuilder_build(b: StrBuilder) -> String {
    b.s
}

pub fn strbuilder_append_string(b: &mut StrBuilder, s: String) {
    b.s.push_str(&s);
}

impl StrBuilder {
    pub fn push(&mut self, c: char) {
        self.s.push(c);
    }
}

#[macro_export]
macro_rules! strbuilder_write {
    ($b:expr, $($fmt:tt)+) => {
        $crate::util::strbuilder_append_string(&mut $b, format!($($fmt)+))
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
        })
    }
}

pub static VERBOSE_MODE: AtomicBool = AtomicBool::new(false);

#[macro_export]
macro_rules! vprintln {
    ( $( $args:tt )* ) => {
        if $crate::util::VERBOSE_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            println!($($args)*);
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

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn insert(&mut self, key: K, value: V)
    where
        K: Eq,
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
        Q: Eq + ?Sized,
        K: Eq + Borrow<Q>,
    {
        for (k, v) in &self.items {
            if k.borrow() == key {
                return Some(v);
            }
        }
        None
    }

    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        Q: Eq + ?Sized,
        K: Eq + Borrow<Q>,
    {
        for (k, v) in &mut self.items {
            if (k as &K).borrow() == key {
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

impl<'a, K, V> IntoIterator for &'a mut InsertionOrderMap<K, V> {
    type IntoIter = IterMut<'a, K, V>;
    type Item = (&'a K, &'a mut V);

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            inner: self.items.iter_mut(),
        }
    }
}

pub struct IterMut<'a, K, V> {
    inner: std::slice::IterMut<'a, (K, V)>,
}

impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(k, v)| -> Self::Item { (k, v) })
    }
}
