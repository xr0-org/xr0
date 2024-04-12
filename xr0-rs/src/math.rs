#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use libc::{free, malloc};

use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf, Map};
use crate::StrBuilder;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct MathExpr {
    pub type_0: ExprType,
    pub c2rust_unnamed: C2RustUnnamed,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub a: *mut MathAtom,
    pub sum: C2RustUnnamed_0,
    pub negated: *mut MathExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub e1: *mut MathExpr,
    pub e2: *mut MathExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct MathAtom {
    pub type_0: AtomType,
    pub c2rust_unnamed: C2RustUnnamed_1,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_1 {
    pub i: libc::c_uint,
    pub v: *mut libc::c_char,
}
pub type AtomType = libc::c_uint;
pub const ATOM_VARIABLE: AtomType = 1;
pub const ATOM_NAT: AtomType = 0;
pub type ExprType = libc::c_uint;
pub const EXPR_NEG: ExprType = 2;
pub const EXPR_SUM: ExprType = 1;
pub const EXPR_ATOM: ExprType = 0;

pub struct Tally {
    pub map: Box<Map>,
    pub num: libc::c_int,
}

pub unsafe fn math_eq(e1: *mut MathExpr, e2: *mut MathExpr) -> bool {
    return math_le(e1, e2) as libc::c_int != 0 && math_le(e2, e1) as libc::c_int != 0;
}

pub unsafe fn math_lt(e1: *mut MathExpr, e2: *mut MathExpr) -> bool {
    return math_le(e1, e2) as libc::c_int != 0 && !math_eq(e1, e2);
}

pub unsafe fn math_gt(e1: *mut MathExpr, e2: *mut MathExpr) -> bool {
    return math_lt(e2, e1);
}

pub unsafe fn math_ge(e1: *mut MathExpr, e2: *mut MathExpr) -> bool {
    return math_le(e2, e1);
}
unsafe fn math_expr_fromint(i: libc::c_int) -> *mut MathExpr {
    if i < 0 as libc::c_int {
        return math_expr_neg_create(math_expr_fromint(-i));
    }
    return math_expr_atom_create(math_atom_nat_create(i as libc::c_uint));
}
unsafe fn math_expr_fromvartally(id: *mut libc::c_char, num: libc::c_int) -> *mut MathExpr {
    if !(num != 0 as libc::c_int) {
        panic!();
    }
    if num < 0 as libc::c_int {
        return math_expr_neg_create(math_expr_fromvartally(id, -num));
    }
    let mut e: *mut MathExpr = math_expr_atom_create(math_atom_variable_create(id));
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < num - 1 as libc::c_int {
        e = math_expr_sum_create(e, math_expr_atom_create(math_atom_variable_create(id)));
        i += 1;
    }
    return e;
}

pub unsafe fn math_le(e1: *mut MathExpr, e2: *mut MathExpr) -> bool {
    let d1: Tally = tally(e1);
    let d2: Tally = tally(e2);
    return variable_tally_eq(d1.map, d2.map) as libc::c_int != 0 && d1.num <= d2.num;
}

pub unsafe fn math_expr_atom_create(a: *mut MathAtom) -> *mut MathExpr {
    let e: *mut MathExpr = malloc(::core::mem::size_of::<MathExpr>()) as *mut MathExpr;
    (*e).type_0 = EXPR_ATOM;
    (*e).c2rust_unnamed.a = a;
    return e;
}

pub unsafe fn math_expr_sum_create(e1: *mut MathExpr, e2: *mut MathExpr) -> *mut MathExpr {
    let e: *mut MathExpr = malloc(::core::mem::size_of::<MathExpr>()) as *mut MathExpr;
    (*e).type_0 = EXPR_SUM;
    (*e).c2rust_unnamed.sum.e1 = e1;
    (*e).c2rust_unnamed.sum.e2 = e2;
    return e;
}

pub unsafe fn math_expr_neg_create(orig: *mut MathExpr) -> *mut MathExpr {
    let e: *mut MathExpr = malloc(::core::mem::size_of::<MathExpr>()) as *mut MathExpr;
    (*e).type_0 = EXPR_NEG;
    (*e).c2rust_unnamed.negated = orig;
    return e;
}

pub unsafe fn math_expr_copy(e: *mut MathExpr) -> *mut MathExpr {
    match (*e).type_0 {
        0 => return math_expr_atom_create(math_atom_copy((*e).c2rust_unnamed.a)),
        1 => {
            return math_expr_sum_create(
                math_expr_copy((*e).c2rust_unnamed.sum.e1),
                math_expr_copy((*e).c2rust_unnamed.sum.e2),
            );
        }
        _ => panic!(),
    }
}

pub unsafe fn math_expr_destroy(e: *mut MathExpr) {
    match (*e).type_0 {
        0 => {
            math_atom_destroy((*e).c2rust_unnamed.a);
        }
        1 => {
            math_expr_destroy((*e).c2rust_unnamed.sum.e1);
            math_expr_destroy((*e).c2rust_unnamed.sum.e2);
        }
        2 => {
            math_expr_destroy((*e).c2rust_unnamed.negated);
        }
        _ => panic!(),
    }
    free(e as *mut libc::c_void);
}

pub unsafe fn math_expr_str(e: *mut MathExpr) -> *mut libc::c_char {
    match (*e).type_0 {
        0 => math_atom_str((*e).c2rust_unnamed.a),
        1 => math_expr_sum_str(e),
        2 => math_expr_neg_str(e),
        _ => panic!(),
    }
}

unsafe fn math_expr_sum_str(e: *mut MathExpr) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let e1: *mut libc::c_char = math_expr_str((*e).c2rust_unnamed.sum.e1);
    let e2: *mut libc::c_char = math_expr_str((*e).c2rust_unnamed.sum.e2);
    let sign: *mut libc::c_char = (if *e2 as libc::c_int == '-' as i32 {
        b"\0" as *const u8 as *const libc::c_char
    } else {
        b"+\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    strbuilder_printf(
        b,
        b"(%s%s%s)\0" as *const u8 as *const libc::c_char,
        e1,
        sign,
        e2,
    );
    free(e2 as *mut libc::c_void);
    free(e1 as *mut libc::c_void);
    return strbuilder_build(b);
}
unsafe fn math_expr_neg_str(e: *mut MathExpr) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let orig: *mut libc::c_char = math_expr_str((*e).c2rust_unnamed.negated);
    strbuilder_printf(b, b"-%s\0" as *const u8 as *const libc::c_char, orig);
    free(orig as *mut libc::c_void);
    return strbuilder_build(b);
}
unsafe fn math_expr_nullablesum(e1: *mut MathExpr, e2: *mut MathExpr) -> *mut MathExpr {
    if e1.is_null() {
        return if !e2.is_null() {
            e2
        } else {
            math_expr_fromint(0 as libc::c_int)
        };
    }
    return if !e2.is_null() {
        math_expr_sum_create(e1, e2)
    } else {
        e1
    };
}

pub unsafe fn math_expr_simplify(raw: *mut MathExpr) -> *mut MathExpr {
    let t: Tally = tally(raw);
    let m = &t.map;
    let mut expr: *mut MathExpr = 0 as *mut MathExpr;
    for (k, v) in m.pairs() {
        if !v.is_null() {
            // XXX this cast to mut is suspicious; is the original making a mistake here?
            expr = math_expr_nullablesum(
                expr,
                math_expr_fromvartally(k as *mut libc::c_char, v as libc::c_long as libc::c_int),
            );
        }
    }
    let num: *mut MathExpr = if t.num != 0 {
        math_expr_fromint(t.num)
    } else {
        0 as *mut MathExpr
    };
    expr = math_expr_nullablesum(expr, num);
    if expr.is_null() {
        panic!();
    }
    return expr;
}

unsafe fn tally(e: *mut MathExpr) -> Tally {
    match (*e).type_0 {
        0 => atom_tally((*e).c2rust_unnamed.a),
        1 => sum_tally(e),
        2 => neg_tally(e),
        _ => panic!(),
    }
}

unsafe fn sum_tally(e: *mut MathExpr) -> Tally {
    if !((*e).type_0 as libc::c_uint == EXPR_SUM as libc::c_int as libc::c_uint) {
        panic!();
    }
    let r1: Tally = tally((*e).c2rust_unnamed.sum.e1);
    let r2: Tally = tally((*e).c2rust_unnamed.sum.e2);
    return {
        let init = Tally {
            map: map_sum(r1.map, r2.map),
            num: r1.num + r2.num,
        };
        init
    };
}

unsafe fn map_sum(m1: Box<Map>, m2: Box<Map>) -> Box<Map> {
    let mut m = Map::new();
    for (k, v) in m1.pairs() {
        m.set(dynamic_str(k), v);
    }
    for (k, v) in m2.pairs() {
        // XXX THIS IS NOT GREAT
        let val = v.offset(m.get(k) as libc::c_long as isize);
        m.set(dynamic_str(k), val);
    }
    m2.destroy();
    m1.destroy();
    return m;
}

unsafe fn neg_tally(e: *mut MathExpr) -> Tally {
    let mut r: Tally = tally((*e).c2rust_unnamed.negated);
    let m = &mut r.map;
    for (_, v) in m.pairs_mut() {
        let val: libc::c_long = -(*v as libc::c_long);
        *v = val as *mut libc::c_void;
    }
    r.num = -r.num;
    return r;
}

unsafe fn variable_tally_eq(m1: Box<Map>, m2: Box<Map>) -> bool {
    let mut res: bool = 0 as libc::c_int != 0;
    for key in m1.keys() {
        if m1.get(key) != m2.get(key) {
            res = 0 as libc::c_int != 0;
        }
    }
    res = m1.len() == m2.len();
    m2.destroy();
    m1.destroy();
    return res;
}

pub unsafe fn math_atom_nat_create(i: libc::c_uint) -> *mut MathAtom {
    let a: *mut MathAtom = malloc(::core::mem::size_of::<MathAtom>()) as *mut MathAtom;
    (*a).type_0 = ATOM_NAT;
    (*a).c2rust_unnamed.i = i;
    return a;
}

pub unsafe fn math_atom_variable_create(s: *mut libc::c_char) -> *mut MathAtom {
    let a: *mut MathAtom = malloc(::core::mem::size_of::<MathAtom>()) as *mut MathAtom;
    (*a).type_0 = ATOM_VARIABLE;
    (*a).c2rust_unnamed.v = s;
    return a;
}

pub unsafe fn math_atom_copy(a: *mut MathAtom) -> *mut MathAtom {
    match (*a).type_0 {
        0 => return math_atom_nat_create((*a).c2rust_unnamed.i),
        1 => return math_atom_variable_create(dynamic_str((*a).c2rust_unnamed.v)),
        _ => panic!(),
    }
}

pub unsafe fn math_atom_destroy(a: *mut MathAtom) {
    match (*a).type_0 {
        0 => {}
        1 => {
            free((*a).c2rust_unnamed.v as *mut libc::c_void);
        }
        _ => panic!(),
    }
    free(a as *mut libc::c_void);
}

pub unsafe fn math_atom_str(a: *mut MathAtom) -> *mut libc::c_char {
    if (*a).type_0 as libc::c_uint == ATOM_VARIABLE as libc::c_int as libc::c_uint {
        return dynamic_str((*a).c2rust_unnamed.v);
    }
    if !((*a).type_0 as libc::c_uint == ATOM_NAT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    } else {
    };
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%d\0" as *const u8 as *const libc::c_char,
        (*a).c2rust_unnamed.i,
    );
    return strbuilder_build(b);
}
unsafe fn atom_tally(a: *mut MathAtom) -> Tally {
    match (*a).type_0 {
        0 => Tally {
            map: Map::new(),
            num: (*a).c2rust_unnamed.i as libc::c_int,
        },
        1 => Tally {
            map: map_fromvar(dynamic_str((*a).c2rust_unnamed.v)),
            num: 0 as libc::c_int,
        },
        _ => panic!("invalid math_atom type"),
    }
}
unsafe fn map_fromvar(id: *mut libc::c_char) -> Box<Map> {
    let mut m = Map::new();
    m.set(id, 1 as libc::c_int as *mut libc::c_void);
    return m;
}
