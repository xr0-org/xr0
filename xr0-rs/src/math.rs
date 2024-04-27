use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

#[derive(Clone)]
pub enum MathExpr {
    Atom(MathAtom),
    Sum(Box<MathExpr>, Box<MathExpr>),
    Neg(Box<MathExpr>),
}

#[derive(Clone)]
pub enum MathAtom {
    Nat(libc::c_uint),
    Variable(String),
}

type Map<'a> = HashMap<&'a str, i64>;

/// An integer expression of the form `v0 * k0 + v1 * k1 + ... + num`.
pub struct Tally<'a> {
    pub map: Map<'a>,
    pub num: libc::c_int,
}

pub fn math_eq(e1: &MathExpr, e2: &MathExpr) -> bool {
    // Note: Inefficient implementation in the original.
    math_le(e1, e2) && math_le(e2, e1)
}

pub fn math_lt(e1: &MathExpr, e2: &MathExpr) -> bool {
    // Note: Inefficient implementation in the original.
    math_le(e1, e2) && !math_eq(e1, e2)
}

pub fn math_gt(e1: &MathExpr, e2: &MathExpr) -> bool {
    math_lt(e2, e1)
}

pub fn math_ge(e1: &MathExpr, e2: &MathExpr) -> bool {
    math_le(e2, e1)
}

#[allow(dead_code)]
fn math_expr_fromint(i: libc::c_int) -> Box<MathExpr> {
    Box::new(if i < 0 {
        MathExpr::Neg(math_expr_fromint(-i))
    } else {
        MathExpr::Atom(MathAtom::Nat(i as libc::c_uint))
    })
}

#[allow(dead_code)]
fn math_expr_fromvartally(id: &str, num: libc::c_int) -> Box<MathExpr> {
    assert_ne!(num, 0);
    if num < 0 {
        return Box::new(MathExpr::Neg(math_expr_fromvartally(id, -num)));
    }
    // Note: In the original, the `id` is reused without copying. Destroying the expr would free it
    // `num` times.
    let mut e = Box::new(MathExpr::Atom(MathAtom::Variable(id.to_string())));
    for _ in 0..num - 1 {
        e = Box::new(MathExpr::Sum(
            e,
            Box::new(MathExpr::Atom(MathAtom::Variable(id.to_string()))),
        ));
    }
    e
}

pub fn math_le(e1: &MathExpr, e2: &MathExpr) -> bool {
    let d1 = tally(e1);
    let d2 = tally(e2);

    // Note: Original used a function, `variable_tally_eq`, which was quite buggy.
    d1.map == d2.map && d1.num <= d2.num
}

impl Display for MathExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            MathExpr::Atom(a) => write!(f, "{a}"),
            MathExpr::Sum(e1, e2) => {
                let s2 = format!("{e2}");
                let sign = if s2.starts_with('-') { "" } else { "+" };
                write!(f, "{e1}{sign}{s2}")
            }
            MathExpr::Neg(e) => write!(f, "-{e}"),
        }
    }
}

#[allow(dead_code)]
fn math_expr_nullablesum(e1: Option<Box<MathExpr>>, e2: Option<Box<MathExpr>>) -> Box<MathExpr> {
    match (e1, e2) {
        (None, None) => math_expr_fromint(0),
        (None, Some(e2)) => e2,
        (Some(e1), None) => e1,
        (Some(e1), Some(e2)) => Box::new(MathExpr::Sum(e1, e2)),
    }
}

fn tally(e: &MathExpr) -> Tally {
    match e {
        MathExpr::Atom(a) => atom_tally(a),
        MathExpr::Sum(e1, e2) => {
            let r1 = tally(e1);
            let r2 = tally(e2);
            Tally {
                map: map_sum(r1.map, r2.map),
                num: r1.num + r2.num,
            }
        }
        MathExpr::Neg(e) => neg_tally(e),
    }
}

fn map_sum<'a>(mut m1: Map<'a>, m2: Map<'a>) -> Map<'a> {
    use std::collections::hash_map::Entry;
    for (k, v) in m2.into_iter() {
        match m1.entry(k) {
            Entry::Vacant(e) => {
                e.insert(v);
            }
            Entry::Occupied(mut e) => {
                let sum = *e.get() + v;
                if sum == 0 {
                    e.remove();
                } else {
                    e.insert(sum);
                }
            }
        }
    }
    m1
}

fn neg_tally(e: &MathExpr) -> Tally {
    let mut r = tally(e);
    for v in r.map.values_mut() {
        let val = (*v).wrapping_neg();
        *v = val;
    }
    r.num = r.num.wrapping_neg();
    r
}

impl Display for MathAtom {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            MathAtom::Variable(id) => write!(f, "{id}"),
            MathAtom::Nat(i) => write!(f, "{i}"),
        }
    }
}

fn atom_tally(a: &MathAtom) -> Tally {
    match a {
        MathAtom::Nat(i) => Tally {
            map: Map::new(),
            num: *i as libc::c_int,
        },
        MathAtom::Variable(id) => Tally {
            map: map_fromvar(id.as_str()),
            num: 0 as libc::c_int,
        },
    }
}

fn map_fromvar(id: &str) -> Map {
    let mut m = Map::new();
    m.insert(id, 1);
    m
}
