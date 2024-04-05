#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::{AstExpr, StrBuilder};

extern "C" {
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
    fn ast_expr_inverted_copy(expr: *mut AstExpr, invert: bool) -> *mut AstExpr;
    fn ast_expr_destroy(_: *mut AstExpr);
    fn ast_expr_str(_: *mut AstExpr) -> *mut libc::c_char;
    fn ast_expr_copy(_: *mut AstExpr) -> *mut AstExpr;
    fn ast_expr_equal(e1: *mut AstExpr, e2: *mut AstExpr) -> bool;
    fn strbuilder_printf(b: *mut StrBuilder, fmt: *const libc::c_char, _: ...) -> libc::c_int;
    fn dynamic_str(_: *const libc::c_char) -> *mut libc::c_char;
    fn strbuilder_create() -> *mut StrBuilder;
    fn strbuilder_build(b: *mut StrBuilder) -> *mut libc::c_char;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Props {
    pub n: libc::c_int,
    pub prop: *mut *mut AstExpr,
}
#[no_mangle]
pub unsafe extern "C" fn props_create() -> *mut Props {
    return calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<Props>() as libc::c_ulong,
    ) as *mut Props;
}
#[no_mangle]
pub unsafe extern "C" fn props_copy(mut old: *mut Props) -> *mut Props {
    let mut new: *mut Props = props_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        props_install(new, ast_expr_copy(*((*old).prop).offset(i as isize)));
        i += 1;
    }
    return new;
}
#[no_mangle]
pub unsafe extern "C" fn props_destroy(mut p: *mut Props) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        ast_expr_destroy(*((*p).prop).offset(i as isize));
        i += 1;
    }
    free(p as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn props_str(
    mut p: *mut Props,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    if (*p).n == 0 as libc::c_int {
        return dynamic_str(b"\0" as *const u8 as *const libc::c_char);
    }
    let mut b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%s\xE2\x8A\xA2 \0" as *const u8 as *const libc::c_char,
        indent,
    );
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        let mut e: *mut libc::c_char = ast_expr_str(*((*p).prop).offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%s\0" as *const u8 as *const libc::c_char,
            e,
            if (i + 1 as libc::c_int) < (*p).n {
                b", \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            },
        );
        free(e as *mut libc::c_void);
        i += 1;
    }
    strbuilder_printf(b, b"\n\0" as *const u8 as *const libc::c_char);
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe extern "C" fn props_n(mut p: *mut Props) -> libc::c_int {
    return (*p).n;
}
#[no_mangle]
pub unsafe extern "C" fn props_props(mut p: *mut Props) -> *mut *mut AstExpr {
    return (*p).prop;
}
#[no_mangle]
pub unsafe extern "C" fn props_install(mut p: *mut Props, mut e: *mut AstExpr) {
    if props_contradicts(p, e) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"props_install\0"))
                .as_ptr(),
            b"Props.c\0" as *const u8 as *const libc::c_char,
            71 as libc::c_int,
            b"!props_contradicts(p, e)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*p).n += 1;
    (*p).prop = realloc(
        (*p).prop as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstExpr>() as libc::c_ulong)
            .wrapping_mul((*p).n as libc::c_ulong),
    ) as *mut *mut AstExpr;
    let ref mut fresh0 = *((*p).prop).offset(((*p).n - 1 as libc::c_int) as isize);
    *fresh0 = e;
}
#[no_mangle]
pub unsafe extern "C" fn props_get(mut p: *mut Props, mut e: *mut AstExpr) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        if ast_expr_equal(e, *((*p).prop).offset(i as isize)) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn props_contradicts(mut p: *mut Props, mut p1: *mut AstExpr) -> bool {
    let mut not_p1: *mut AstExpr = ast_expr_inverted_copy(p1, 1 as libc::c_int != 0);
    let mut iscontradiction: bool = props_contradicts_actual(p, p1, not_p1);
    ast_expr_destroy(not_p1);
    return iscontradiction;
}
unsafe extern "C" fn props_contradicts_actual(
    mut p: *mut Props,
    mut p1: *mut AstExpr,
    mut not_p1: *mut AstExpr,
) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        let mut p2: *mut AstExpr = *((*p).prop).offset(i as isize);
        let mut not_p2: *mut AstExpr = ast_expr_inverted_copy(p2, 1 as libc::c_int != 0);
        let mut contra: bool = ast_expr_equal(p1, not_p2) as libc::c_int != 0
            || ast_expr_equal(not_p1, p2) as libc::c_int != 0;
        ast_expr_destroy(not_p2);
        if contra {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}
