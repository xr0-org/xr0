#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{free, malloc};

use crate::c_util::__assert_rtn;
use crate::util::{
    dynamic_str, entry, map, map_create, map_destroy, map_get, map_set, strbuilder_build,
    strbuilder_create, strbuilder_printf,
};
use crate::StrBuilder;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct MathExpr {
    pub type_0: expr_type,
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
    pub type_0: atom_type,
    pub c2rust_unnamed: C2RustUnnamed_1,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_1 {
    pub i: libc::c_uint,
    pub v: *mut libc::c_char,
}
pub type atom_type = libc::c_uint;
pub const ATOM_VARIABLE: atom_type = 1;
pub const ATOM_NAT: atom_type = 0;
pub type expr_type = libc::c_uint;
pub const EXPR_NEG: expr_type = 2;
pub const EXPR_SUM: expr_type = 1;
pub const EXPR_ATOM: expr_type = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tally {
    pub map: *mut map,
    pub num: libc::c_int,
}
#[no_mangle]
pub unsafe fn math_eq(mut e1: *mut MathExpr, mut e2: *mut MathExpr) -> bool {
    return math_le(e1, e2) as libc::c_int != 0 && math_le(e2, e1) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe fn math_lt(mut e1: *mut MathExpr, mut e2: *mut MathExpr) -> bool {
    return math_le(e1, e2) as libc::c_int != 0 && !math_eq(e1, e2);
}
#[no_mangle]
pub unsafe fn math_gt(mut e1: *mut MathExpr, mut e2: *mut MathExpr) -> bool {
    return math_lt(e2, e1);
}
#[no_mangle]
pub unsafe fn math_ge(mut e1: *mut MathExpr, mut e2: *mut MathExpr) -> bool {
    return math_le(e2, e1);
}
unsafe fn math_expr_fromint(mut i: libc::c_int) -> *mut MathExpr {
    if i < 0 as libc::c_int {
        return math_expr_neg_create(math_expr_fromint(-i));
    }
    return math_expr_atom_create(math_atom_nat_create(i as libc::c_uint));
}
unsafe fn math_expr_fromvartally(
    mut id: *mut libc::c_char,
    mut num: libc::c_int,
) -> *mut MathExpr {
    if !(num != 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"math_expr_fromvartally\0",
            ))
            .as_ptr(),
            b"math.c\0" as *const u8 as *const libc::c_char,
            52 as libc::c_int,
            b"num != 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
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
#[no_mangle]
pub unsafe fn math_le(mut e1: *mut MathExpr, mut e2: *mut MathExpr) -> bool {
    let mut d1: tally = tally(e1);
    let mut d2: tally = tally(e2);
    return variable_tally_eq(d1.map, d2.map) as libc::c_int != 0 && d1.num <= d2.num;
}
#[no_mangle]
pub unsafe fn math_expr_atom_create(mut a: *mut MathAtom) -> *mut MathExpr {
    let mut e: *mut MathExpr = malloc(::core::mem::size_of::<MathExpr>()) as *mut MathExpr;
    (*e).type_0 = EXPR_ATOM;
    (*e).c2rust_unnamed.a = a;
    return e;
}
#[no_mangle]
pub unsafe fn math_expr_sum_create(
    mut e1: *mut MathExpr,
    mut e2: *mut MathExpr,
) -> *mut MathExpr {
    let mut e: *mut MathExpr = malloc(::core::mem::size_of::<MathExpr>()) as *mut MathExpr;
    (*e).type_0 = EXPR_SUM;
    (*e).c2rust_unnamed.sum.e1 = e1;
    (*e).c2rust_unnamed.sum.e2 = e2;
    return e;
}
#[no_mangle]
pub unsafe fn math_expr_neg_create(mut orig: *mut MathExpr) -> *mut MathExpr {
    let mut e: *mut MathExpr = malloc(::core::mem::size_of::<MathExpr>()) as *mut MathExpr;
    (*e).type_0 = EXPR_NEG;
    (*e).c2rust_unnamed.negated = orig;
    return e;
}
#[no_mangle]
pub unsafe fn math_expr_copy(mut e: *mut MathExpr) -> *mut MathExpr {
    match (*e).type_0 as libc::c_uint {
        0 => return math_expr_atom_create(math_atom_copy((*e).c2rust_unnamed.a)),
        1 => {
            return math_expr_sum_create(
                math_expr_copy((*e).c2rust_unnamed.sum.e1),
                math_expr_copy((*e).c2rust_unnamed.sum.e2),
            );
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(
                        b"math_expr_copy\0",
                    ))
                    .as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    136 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe fn math_expr_destroy(mut e: *mut MathExpr) {
    match (*e).type_0 as libc::c_uint {
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
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(
                        b"math_expr_destroy\0",
                    ))
                    .as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    155 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    free(e as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn math_expr_str(mut e: *mut MathExpr) -> *mut libc::c_char {
    match (*e).type_0 as libc::c_uint {
        0 => return math_atom_str((*e).c2rust_unnamed.a),
        1 => return math_expr_sum_str(e),
        2 => return math_expr_neg_str(e),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"math_expr_str\0"))
                        .as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    177 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
unsafe fn math_expr_sum_str(mut e: *mut MathExpr) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut e1: *mut libc::c_char = math_expr_str((*e).c2rust_unnamed.sum.e1);
    let mut e2: *mut libc::c_char = math_expr_str((*e).c2rust_unnamed.sum.e2);
    let mut sign: *mut libc::c_char = (if *e2 as libc::c_int == '-' as i32 {
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
unsafe fn math_expr_neg_str(mut e: *mut MathExpr) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut orig: *mut libc::c_char = math_expr_str((*e).c2rust_unnamed.negated);
    strbuilder_printf(b, b"-%s\0" as *const u8 as *const libc::c_char, orig);
    free(orig as *mut libc::c_void);
    return strbuilder_build(b);
}
unsafe fn math_expr_nullablesum(
    mut e1: *mut MathExpr,
    mut e2: *mut MathExpr,
) -> *mut MathExpr {
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
#[no_mangle]
pub unsafe fn math_expr_simplify(mut raw: *mut MathExpr) -> *mut MathExpr {
    let mut t: tally = tally(raw);
    let mut m: *mut map = t.map;
    let mut expr: *mut MathExpr = 0 as *mut MathExpr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        if !(e.Value).is_null() {
            expr = math_expr_nullablesum(
                expr,
                math_expr_fromvartally(e.key, e.Value as libc::c_long as libc::c_int),
            );
        }
        i += 1;
    }
    let mut num: *mut MathExpr = if t.num != 0 {
        math_expr_fromint(t.num)
    } else {
        0 as *mut MathExpr
    };
    expr = math_expr_nullablesum(expr, num);
    if expr.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 19], &[libc::c_char; 19]>(b"math_expr_simplify\0"))
                .as_ptr(),
            b"math.c\0" as *const u8 as *const libc::c_char,
            244 as libc::c_int,
            b"expr\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return expr;
}
unsafe fn tally(mut e: *mut MathExpr) -> tally {
    match (*e).type_0 as libc::c_uint {
        0 => return atom_tally((*e).c2rust_unnamed.a),
        1 => return sum_tally(e),
        2 => return neg_tally(e),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 6], &[libc::c_char; 6]>(b"tally\0")).as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    269 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
unsafe fn sum_tally(mut e: *mut MathExpr) -> tally {
    if !((*e).type_0 as libc::c_uint == EXPR_SUM as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 10], &[libc::c_char; 10]>(b"sum_tally\0")).as_ptr(),
            b"math.c\0" as *const u8 as *const libc::c_char,
            279 as libc::c_int,
            b"e->type == EXPR_SUM\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut r1: tally = tally((*e).c2rust_unnamed.sum.e1);
    let mut r2: tally = tally((*e).c2rust_unnamed.sum.e2);
    return {
        let mut init = tally {
            map: map_sum(r1.map, r2.map),
            num: r1.num + r2.num,
        };
        init
    };
}
unsafe fn map_sum(mut m1: *mut map, mut m2: *mut map) -> *mut map {
    let mut m: *mut map = map_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m1).n {
        let mut e: entry = *((*m1).entry).offset(i as isize);
        map_set(m, dynamic_str(e.key), e.Value);
        i += 1;
    }
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < (*m2).n {
        let mut e_0: entry = *((*m2).entry).offset(i_0 as isize);
        map_set(
            m,
            dynamic_str(e_0.key),
            (e_0.Value).offset(map_get(m, e_0.key) as libc::c_long as isize),
        );
        i_0 += 1;
    }
    map_destroy(m2);
    map_destroy(m1);
    return m;
}
unsafe fn neg_tally(mut e: *mut MathExpr) -> tally {
    let mut r: tally = tally((*e).c2rust_unnamed.negated);
    let mut m: *mut map = r.map;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e_0: entry = *((*m).entry).offset(i as isize);
        let mut val: libc::c_long = -(map_get(m, e_0.key) as libc::c_long);
        map_set(m, e_0.key, val as *mut libc::c_void);
        i += 1;
    }
    r.num = -r.num;
    return r;
}
unsafe fn variable_tally_eq(mut m1: *mut map, mut m2: *mut map) -> bool {
    let mut res: bool = 0 as libc::c_int != 0;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m1).n {
        let mut key: *mut libc::c_char = (*((*m1).entry).offset(i as isize)).key;
        if map_get(m1, key) != map_get(m2, key) {
            res = 0 as libc::c_int != 0;
        }
        i += 1;
    }
    res = (*m1).n == (*m2).n;
    map_destroy(m2);
    map_destroy(m1);
    return res;
}
#[no_mangle]
pub unsafe fn math_atom_nat_create(mut i: libc::c_uint) -> *mut MathAtom {
    let mut a: *mut MathAtom = malloc(::core::mem::size_of::<MathAtom>()) as *mut MathAtom;
    (*a).type_0 = ATOM_NAT;
    (*a).c2rust_unnamed.i = i;
    return a;
}
#[no_mangle]
pub unsafe fn math_atom_variable_create(mut s: *mut libc::c_char) -> *mut MathAtom {
    let mut a: *mut MathAtom = malloc(::core::mem::size_of::<MathAtom>()) as *mut MathAtom;
    (*a).type_0 = ATOM_VARIABLE;
    (*a).c2rust_unnamed.v = s;
    return a;
}
#[no_mangle]
pub unsafe fn math_atom_copy(mut a: *mut MathAtom) -> *mut MathAtom {
    match (*a).type_0 as libc::c_uint {
        0 => return math_atom_nat_create((*a).c2rust_unnamed.i),
        1 => return math_atom_variable_create(dynamic_str((*a).c2rust_unnamed.v)),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(
                        b"math_atom_copy\0",
                    ))
                    .as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    381 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe fn math_atom_destroy(mut a: *mut MathAtom) {
    match (*a).type_0 as libc::c_uint {
        0 => {}
        1 => {
            free((*a).c2rust_unnamed.v as *mut libc::c_void);
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(
                        b"math_atom_destroy\0",
                    ))
                    .as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    395 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    free(a as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn math_atom_str(mut a: *mut MathAtom) -> *mut libc::c_char {
    if (*a).type_0 as libc::c_uint == ATOM_VARIABLE as libc::c_int as libc::c_uint {
        return dynamic_str((*a).c2rust_unnamed.v);
    }
    if !((*a).type_0 as libc::c_uint == ATOM_NAT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"math_atom_str\0"))
                .as_ptr(),
            b"math.c\0" as *const u8 as *const libc::c_char,
            407 as libc::c_int,
            b"a->type == ATOM_NAT\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%d\0" as *const u8 as *const libc::c_char,
        (*a).c2rust_unnamed.i,
    );
    return strbuilder_build(b);
}
unsafe fn atom_tally(mut a: *mut MathAtom) -> tally {
    match (*a).type_0 as libc::c_uint {
        0 => {
            return {
                let mut init = tally {
                    map: map_create(),
                    num: (*a).c2rust_unnamed.i as libc::c_int,
                };
                init
            };
        }
        1 => {
            return {
                let mut init = tally {
                    map: map_fromvar(dynamic_str((*a).c2rust_unnamed.v)),
                    num: 0 as libc::c_int,
                };
                init
            };
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 11], &[libc::c_char; 11]>(b"atom_tally\0"))
                        .as_ptr(),
                    b"math.c\0" as *const u8 as *const libc::c_char,
                    428 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
unsafe fn map_fromvar(mut id: *mut libc::c_char) -> *mut map {
    let mut m: *mut map = map_create();
    map_set(m, id, 1 as libc::c_int as *mut libc::c_void);
    return m;
}
