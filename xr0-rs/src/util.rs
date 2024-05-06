use std::borrow::Borrow;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Deref;
use std::sync::atomic::AtomicBool;

use crate::ast::AstExpr;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    inner: Option<Box<Error>>,
}

#[derive(Clone)]
pub enum ErrorKind {
    Custom(String),
    UndecideableCond(Box<AstExpr>),
}

pub type Result<T, E = Box<Error>> = std::result::Result<T, E>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            ErrorKind::Custom(msg) => write!(f, "{msg}"),
            ErrorKind::UndecideableCond(_) => write!(f, "undecideable condition"),
        }
    }
}

impl Error {
    pub fn new(s: String) -> Box<Error> {
        Box::new(Error {
            kind: ErrorKind::Custom(s),
            inner: None,
        })
    }

    pub fn undecideable_cond(expr: Box<AstExpr>) -> Box<Error> {
        Box::new(Error {
            kind: ErrorKind::UndecideableCond(expr),
            inner: None,
        })
    }

    pub fn wrap(self: Box<Self>, msg: String) -> Box<Error> {
        Box::new(Error {
            kind: ErrorKind::Custom(format!("{msg}{self}")),
            inner: Some(self),
        })
    }

    /// If `self` or any inner error is an UndecidebleCond error, returns the condition.
    /// Otherwise returns `Err(self)`.
    pub fn try_into_undecideable_cond(self) -> Result<Box<AstExpr>> {
        match self.kind {
            ErrorKind::UndecideableCond(cond) => Ok(cond),
            kind => match self.inner {
                None => Err(Box::new(Error { kind, inner: None })),
                Some(inner) => inner.try_into_undecideable_cond().map_err(|inner| {
                    Box::new(Error {
                        kind,
                        inner: Some(inner),
                    })
                }),
            },
        }
    }
}

impl Debug for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorKind::Custom(msg) => f.debug_tuple("ErrorKind::Custom").field(msg).finish(),
            ErrorKind::UndecideableCond(cond) => f
                .debug_tuple("ErrorKind::UndecideableCond")
                .field(&format!("{cond}"))
                .finish(),
        }
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

#[macro_export]
macro_rules! str_write {
    ($b:expr, $($fmt:tt)+) => {{
        let buf: &mut String = &mut $b;
        *buf += &format!($($fmt)+);
    }}
}

pub static VERBOSE_MODE: AtomicBool = AtomicBool::new(false);

#[macro_export]
macro_rules! vprintln {
    ( $( $args:tt )* ) => {
        if $crate::util::VERBOSE_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            eprintln!($($args)*);
        }
    }
}

#[macro_export]
macro_rules! vprint {
    ( $( $args:tt )* ) => {
        if $crate::util::VERBOSE_MODE.load(std::sync::atomic::Ordering::Relaxed) {
            eprint!($($args)*);
        }
    }
}

/// Map that keeps entries in insertion order.
/// Implemented as an alist.
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
