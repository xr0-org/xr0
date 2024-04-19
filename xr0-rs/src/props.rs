#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use libc::{calloc, free, realloc};

use crate::ast::{
    ast_expr_copy, ast_expr_destroy, ast_expr_equal, ast_expr_inverted_copy, ast_expr_str,
};
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{AstExpr, StrBuilder};

#[derive(Copy, Clone)]
pub struct Props {
    pub n: libc::c_int,
    pub prop: *mut *mut AstExpr,
}

pub unsafe fn props_create() -> *mut Props {
    return calloc(1, ::core::mem::size_of::<Props>()) as *mut Props;
}

pub unsafe fn props_copy(old: *mut Props) -> *mut Props {
    let new: *mut Props = props_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        props_install(
            new,
            Box::into_raw(ast_expr_copy(&**((*old).prop).offset(i as isize))),
        );
        i += 1;
    }
    return new;
}

pub unsafe fn props_destroy(p: *mut Props) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        ast_expr_destroy(*((*p).prop).offset(i as isize));
        i += 1;
    }
    free(p as *mut libc::c_void);
}

pub unsafe fn props_str(p: *mut Props, indent: *mut libc::c_char) -> *mut libc::c_char {
    if (*p).n == 0 as libc::c_int {
        return dynamic_str(b"\0" as *const u8 as *const libc::c_char);
    }
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(
        b,
        b"%s\xE2\x8A\xA2 \0" as *const u8 as *const libc::c_char,
        indent,
    );
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        let e: *mut libc::c_char = ast_expr_str(&**((*p).prop).offset(i as isize));
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

pub unsafe fn props_n(p: *mut Props) -> libc::c_int {
    return (*p).n;
}

pub unsafe fn props_props(p: *mut Props) -> *mut *mut AstExpr {
    return (*p).prop;
}

pub unsafe fn props_install(p: *mut Props, e: *mut AstExpr) {
    if props_contradicts(p, &*e) {
        panic!();
    }
    (*p).n += 1;
    (*p).prop = realloc(
        (*p).prop as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstExpr>()).wrapping_mul((*p).n as usize),
    ) as *mut *mut AstExpr;
    let ref mut fresh0 = *((*p).prop).offset(((*p).n - 1 as libc::c_int) as isize);
    *fresh0 = e;
}

pub unsafe fn props_get(p: *mut Props, e: *mut AstExpr) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        if ast_expr_equal(&*e, &**((*p).prop).offset(i as isize)) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}

pub unsafe fn props_contradicts(p: *mut Props, p1: &AstExpr) -> bool {
    let not_p1 = ast_expr_inverted_copy(p1, true);
    props_contradicts_actual(p, p1, &not_p1)
}
unsafe fn props_contradicts_actual(p: *mut Props, p1: &AstExpr, not_p1: &AstExpr) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        let p2: *mut AstExpr = *((*p).prop).offset(i as isize);
        let not_p2 = ast_expr_inverted_copy(&*p2, true);
        let contra: bool = ast_expr_equal(&*p1, &not_p2) as libc::c_int != 0
            || ast_expr_equal(&*not_p1, &*p2) as libc::c_int != 0;
        drop(not_p2);
        if contra {
            return true;
        }
        i += 1;
    }
    false
}
