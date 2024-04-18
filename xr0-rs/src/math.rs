use std::ffi::CStr;
use std::fmt::{self, Display, Formatter};

use libc::free;

use crate::util::{dynamic_str, Map};

#[derive(Clone)]
pub enum MathExpr {
    Atom(MathAtom),
    Sum(Box<MathExpr>, Box<MathExpr>),
    Neg(Box<MathExpr>),
}

pub enum MathAtom {
    Nat(libc::c_uint),
    Variable(*mut libc::c_char),
}

pub struct Tally {
    pub map: Box<Map>,
    pub num: libc::c_int,
}

pub unsafe fn math_eq(e1: &MathExpr, e2: &MathExpr) -> bool {
    math_le(e1, e2) && math_le(e2, e1)
}

pub unsafe fn math_lt(e1: &MathExpr, e2: &MathExpr) -> bool {
    math_le(e1, e2) && !math_eq(e1, e2)
}

pub unsafe fn math_gt(e1: &MathExpr, e2: &MathExpr) -> bool {
    math_lt(e2, e1)
}

pub unsafe fn math_ge(e1: &MathExpr, e2: &MathExpr) -> bool {
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
unsafe fn math_expr_fromvartally(id: *mut libc::c_char, num: libc::c_int) -> Box<MathExpr> {
    assert_ne!(num, 0);
    if num < 0 {
        return Box::new(MathExpr::Neg(math_expr_fromvartally(id, -num)));
    }
    let mut e = Box::new(MathExpr::Atom(MathAtom::Variable(id)));
    for _ in 0..num - 1 {
        e = Box::new(MathExpr::Sum(
            e,
            Box::new(MathExpr::Atom(MathAtom::Variable(id))),
        ));
    }
    e
}

pub unsafe fn math_le(e1: &MathExpr, e2: &MathExpr) -> bool {
    let d1 = tally(e1);
    let d2 = tally(e2);
    variable_tally_eq(d1.map, d2.map) && d1.num <= d2.num
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
unsafe fn math_expr_nullablesum(
    e1: Option<Box<MathExpr>>,
    e2: Option<Box<MathExpr>>,
) -> Box<MathExpr> {
    match (e1, e2) {
        (None, None) => math_expr_fromint(0),
        (None, Some(e2)) => e2,
        (Some(e1), None) => e1,
        (Some(e1), Some(e2)) => Box::new(MathExpr::Sum(e1, e2)),
    }
}

#[allow(dead_code)]
pub unsafe fn math_expr_simplify(raw: &MathExpr) -> Box<MathExpr> {
    let t = tally(raw);
    let m = &t.map;
    let mut expr: Option<Box<MathExpr>> = None;
    for (k, v) in m.pairs() {
        let v = v as isize;
        if v != 0 {
            // Note: Key is reused without copying it in the original.
            expr = Some(math_expr_nullablesum(
                expr,
                Some(math_expr_fromvartally(
                    k as *mut libc::c_char,
                    v as libc::c_int,
                )),
            ));
        }
    }
    let num: Option<Box<MathExpr>> = if t.num != 0 {
        Some(math_expr_fromint(t.num))
    } else {
        None
    };
    math_expr_nullablesum(expr, num)
}

unsafe fn tally(e: &MathExpr) -> Tally {
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

unsafe fn map_sum(m1: Box<Map>, m2: Box<Map>) -> Box<Map> {
    let mut m = Map::new();
    for (k, v) in m1.pairs() {
        m.set(dynamic_str(k), v);
    }
    for (k, v) in m2.pairs() {
        // yolo
        let val = (m.get(k) as isize).wrapping_add(v as isize) as *mut libc::c_void;
        m.set(dynamic_str(k), val);
    }
    m2.destroy();
    m1.destroy();
    m
}

unsafe fn neg_tally(e: &MathExpr) -> Tally {
    let mut r = tally(e);
    let m = &mut r.map;
    for (_, v) in m.pairs_mut() {
        let val = (*v as isize).wrapping_neg();
        *v = val as *mut libc::c_void;
    }
    r.num = r.num.wrapping_neg();
    r
}

unsafe fn variable_tally_eq(m1: Box<Map>, m2: Box<Map>) -> bool {
    // Note: Bug in the original.
    let mut res = true;
    for key in m1.keys() {
        if m1.get(key) != m2.get(key) {
            res = false;
            break;
        }
    }
    res = res && m1.len() == m2.len();
    m2.destroy();
    m1.destroy();
    res
}

impl Clone for MathAtom {
    fn clone(&self) -> MathAtom {
        match self {
            MathAtom::Nat(i) => MathAtom::Nat(*i),
            MathAtom::Variable(id) => MathAtom::Variable(unsafe { dynamic_str(*id) }),
        }
    }
}

impl Drop for MathAtom {
    fn drop(&mut self) {
        if let MathAtom::Variable(id) = self {
            unsafe {
                free(*id as *mut libc::c_void);
            }
        }
    }
}

impl Display for MathAtom {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            MathAtom::Variable(id) => {
                write!(f, "{}", unsafe { CStr::from_ptr(*id).to_string_lossy() })
            }
            MathAtom::Nat(i) => write!(f, "{i}"),
        }
    }
}

unsafe fn atom_tally(a: &MathAtom) -> Tally {
    match a {
        MathAtom::Nat(i) => Tally {
            map: Map::new(),
            num: *i as libc::c_int,
        },
        MathAtom::Variable(id) => Tally {
            map: map_fromvar(dynamic_str(*id)),
            num: 0 as libc::c_int,
        },
    }
}

unsafe fn map_fromvar(id: *mut libc::c_char) -> Box<Map> {
    let mut m = Map::new();
    m.set(id, 1 as isize as *mut libc::c_void);
    m
}
