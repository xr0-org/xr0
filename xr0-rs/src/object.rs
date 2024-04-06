#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{calloc, free, malloc, realloc};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_difference_create,
    ast_expr_eq_create, ast_expr_ge_create, ast_expr_le_create, ast_expr_lt_create, ast_expr_str,
    ast_expr_sum_create, ast_type_struct_complete,
};
use crate::c_util::__assert_rtn;
use crate::state::location::{location_copy, location_destroy, location_references, location_str};
use crate::state::{state_alloc, state_dealloc, state_eval, state_getext, state_isdeallocand};
use crate::util::{dynamic_str, error, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::value::{
    value_abstractcopy, value_as_location, value_copy, value_destroy, value_ptr_create,
    value_references, value_referencesheap, value_str, value_struct_create, value_struct_member,
    value_struct_membertype,
};
use crate::{ast_type, AstExpr, Location, State, StrBuilder, Value};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Object {
    pub type_0: object_type,
    pub offset: *mut AstExpr,
    pub c2rust_unnamed: C2RustUnnamed,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub range: *mut Range,
    pub Value: *mut Value,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Range {
    pub size: *mut AstExpr,
    pub loc: *mut Location,
}
pub type object_type = libc::c_uint;
pub const OBJECT_DEALLOCAND_RANGE: object_type = 1;
pub const OBJECT_VALUE: object_type = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct object_arr {
    pub n: libc::c_int,
    pub Object: *mut *mut Object,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct object_result {
    pub val: *mut Object,
    pub err: *mut error,
}
#[no_mangle]
pub unsafe extern "C" fn object_value_create(
    mut offset: *mut AstExpr,
    mut v: *mut Value,
) -> *mut Object {
    let mut obj: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    if obj.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"object_value_create\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            49 as libc::c_int,
            b"obj\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*obj).offset = offset;
    (*obj).c2rust_unnamed.Value = v;
    (*obj).type_0 = OBJECT_VALUE;
    return obj;
}
#[no_mangle]
pub unsafe extern "C" fn object_range_create(
    mut offset: *mut AstExpr,
    mut r: *mut Range,
) -> *mut Object {
    if r.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"object_range_create\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            59 as libc::c_int,
            b"r\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut obj: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    if obj.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"object_range_create\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            61 as libc::c_int,
            b"obj\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*obj).offset = offset;
    (*obj).c2rust_unnamed.range = r;
    (*obj).type_0 = OBJECT_DEALLOCAND_RANGE;
    return obj;
}
#[no_mangle]
pub unsafe extern "C" fn object_destroy(mut obj: *mut Object) {
    match (*obj).type_0 as libc::c_uint {
        0 => {
            if !((*obj).c2rust_unnamed.Value).is_null() {
                value_destroy((*obj).c2rust_unnamed.Value);
            }
        }
        1 => {
            range_destroy((*obj).c2rust_unnamed.range);
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(
                        b"object_destroy\0",
                    ))
                    .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    81 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    ast_expr_destroy((*obj).offset);
    free(obj as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn object_copy(mut old: *mut Object) -> *mut Object {
    let mut new: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    (*new).offset = ast_expr_copy((*old).offset);
    (*new).type_0 = (*old).type_0;
    match (*old).type_0 as libc::c_uint {
        0 => {
            (*new).c2rust_unnamed.Value = if !((*old).c2rust_unnamed.Value).is_null() {
                value_copy((*old).c2rust_unnamed.Value)
            } else {
                0 as *mut Value
            };
        }
        1 => {
            (*new).c2rust_unnamed.range = range_copy((*old).c2rust_unnamed.range);
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"object_copy\0"))
                        .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    101 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    return new;
}
#[no_mangle]
pub unsafe extern "C" fn object_abstractcopy(
    mut old: *mut Object,
    mut s: *mut State,
) -> *mut Object {
    match (*old).type_0 as libc::c_uint {
        1 => return object_copy(old),
        0 => {
            return object_value_create(
                ast_expr_copy((*old).offset),
                if !((*old).c2rust_unnamed.Value).is_null() {
                    value_abstractcopy((*old).c2rust_unnamed.Value, s)
                } else {
                    0 as *mut Value
                },
            );
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(
                        b"object_abstractcopy\0",
                    ))
                    .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    119 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn object_str(mut obj: *mut Object) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(b, b"{\0" as *const u8 as *const libc::c_char);
    let mut offset: *mut libc::c_char = ast_expr_str((*obj).offset);
    strbuilder_printf(b, b"%s:\0" as *const u8 as *const libc::c_char, offset);
    free(offset as *mut libc::c_void);
    let mut inner: *mut libc::c_char = inner_str(obj);
    strbuilder_printf(b, b"<%s>\0" as *const u8 as *const libc::c_char, inner);
    free(inner as *mut libc::c_void);
    strbuilder_printf(b, b"}\0" as *const u8 as *const libc::c_char);
    return strbuilder_build(b);
}
unsafe extern "C" fn inner_str(mut obj: *mut Object) -> *mut libc::c_char {
    match (*obj).type_0 as libc::c_uint {
        0 => {
            return if !((*obj).c2rust_unnamed.Value).is_null() {
                value_str((*obj).c2rust_unnamed.Value)
            } else {
                dynamic_str(b"\0" as *const u8 as *const libc::c_char)
            };
        }
        1 => return range_str((*obj).c2rust_unnamed.range),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 10], &[libc::c_char; 10]>(b"inner_str\0"))
                        .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    150 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn object_referencesheap(mut obj: *mut Object, mut s: *mut State) -> bool {
    if !object_isvalue(obj) {
        return 1 as libc::c_int != 0;
    }
    return !((*obj).c2rust_unnamed.Value).is_null()
        && value_referencesheap((*obj).c2rust_unnamed.Value, s) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn object_hasvalue(mut obj: *mut Object) -> bool {
    if object_isvalue(obj) {
        return !((*obj).c2rust_unnamed.Value).is_null();
    }
    return 0 as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn object_isvalue(mut obj: *mut Object) -> bool {
    return (*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint;
}
#[no_mangle]
pub unsafe extern "C" fn object_as_value(mut obj: *mut Object) -> *mut Value {
    if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"object_as_value\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            182 as libc::c_int,
            b"obj->type == OBJECT_VALUE\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return (*obj).c2rust_unnamed.Value;
}
#[no_mangle]
pub unsafe extern "C" fn object_isdeallocand(mut obj: *mut Object, mut s: *mut State) -> bool {
    match (*obj).type_0 as libc::c_uint {
        0 => {
            return !((*obj).c2rust_unnamed.Value).is_null()
                && state_isdeallocand(s, value_as_location((*obj).c2rust_unnamed.Value))
                    as libc::c_int
                    != 0;
        }
        1 => return range_isdeallocand((*obj).c2rust_unnamed.range, s),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(
                        b"object_isdeallocand\0",
                    ))
                    .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    196 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn object_references(
    mut obj: *mut Object,
    mut loc: *mut Location,
    mut s: *mut State,
) -> bool {
    if (*obj).type_0 as libc::c_uint == OBJECT_DEALLOCAND_RANGE as libc::c_int as libc::c_uint {
        return range_references((*obj).c2rust_unnamed.range, loc, s);
    }
    if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"object_references\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            207 as libc::c_int,
            b"obj->type == OBJECT_VALUE\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut v: *mut Value = object_as_value(obj);
    return if !v.is_null() {
        value_references(v, loc, s) as libc::c_int
    } else {
        0 as libc::c_int
    } != 0;
}
#[no_mangle]
pub unsafe extern "C" fn object_assign(mut obj: *mut Object, mut val: *mut Value) -> *mut error {
    if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"object_assign\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            216 as libc::c_int,
            b"obj->type == OBJECT_VALUE\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*obj).c2rust_unnamed.Value = val;
    return 0 as *mut error;
}
unsafe extern "C" fn object_size(mut obj: *mut Object) -> *mut AstExpr {
    match (*obj).type_0 as libc::c_uint {
        0 => return ast_expr_constant_create(1 as libc::c_int),
        1 => return ast_expr_copy(range_size((*obj).c2rust_unnamed.range)),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"object_size\0"))
                        .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    235 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn object_lower(mut obj: *mut Object) -> *mut AstExpr {
    return (*obj).offset;
}
#[no_mangle]
pub unsafe extern "C" fn object_upper(mut obj: *mut Object) -> *mut AstExpr {
    return ast_expr_sum_create(ast_expr_copy((*obj).offset), object_size(obj));
}
#[no_mangle]
pub unsafe extern "C" fn object_contains(
    mut obj: *mut Object,
    mut offset: *mut AstExpr,
    mut s: *mut State,
) -> bool {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    let mut of: *mut AstExpr = offset;
    let mut e1: *mut AstExpr = ast_expr_le_create(ast_expr_copy(lw), ast_expr_copy(of));
    let mut e2: *mut AstExpr = ast_expr_lt_create(ast_expr_copy(of), ast_expr_copy(up));
    ast_expr_destroy(up);
    let mut contains: bool =
        state_eval(s, e1) as libc::c_int != 0 && state_eval(s, e2) as libc::c_int != 0;
    ast_expr_destroy(e2);
    ast_expr_destroy(e1);
    return contains;
}
#[no_mangle]
pub unsafe extern "C" fn object_contains_upperincl(
    mut obj: *mut Object,
    mut offset: *mut AstExpr,
    mut s: *mut State,
) -> bool {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    let mut of: *mut AstExpr = offset;
    return state_eval(s, ast_expr_le_create(lw, of)) as libc::c_int != 0
        && state_eval(s, ast_expr_le_create(of, up)) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn object_isempty(mut obj: *mut Object, mut s: *mut State) -> bool {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    return state_eval(s, ast_expr_eq_create(lw, up));
}
#[no_mangle]
pub unsafe extern "C" fn object_contig_precedes(
    mut before: *mut Object,
    mut after: *mut Object,
    mut s: *mut State,
) -> bool {
    let mut lw: *mut AstExpr = object_upper(before);
    let mut up: *mut AstExpr = (*after).offset;
    return state_eval(s, ast_expr_eq_create(lw, up));
}
#[no_mangle]
pub unsafe extern "C" fn object_issingular(mut obj: *mut Object, mut s: *mut State) -> bool {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    let mut lw_succ: *mut AstExpr =
        ast_expr_sum_create(lw, ast_expr_constant_create(1 as libc::c_int));
    return state_eval(s, ast_expr_eq_create(lw_succ, up));
}
#[no_mangle]
pub unsafe extern "C" fn object_upto(
    mut obj: *mut Object,
    mut excl_up: *mut AstExpr,
    mut s: *mut State,
) -> *mut Object {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    let mut prop0: *mut AstExpr = ast_expr_le_create(ast_expr_copy(lw), ast_expr_copy(excl_up));
    let mut prop1: *mut AstExpr = ast_expr_eq_create(ast_expr_copy(lw), ast_expr_copy(excl_up));
    let mut prop2: *mut AstExpr = ast_expr_eq_create(ast_expr_copy(up), ast_expr_copy(excl_up));
    let mut e0: bool = state_eval(s, prop0);
    let mut e1: bool = state_eval(s, prop1);
    let mut e2: bool = state_eval(s, prop2);
    ast_expr_destroy(prop2);
    ast_expr_destroy(prop1);
    ast_expr_destroy(prop0);
    ast_expr_destroy(up);
    if !e0 as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"object_upto\0")).as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            357 as libc::c_int,
            b"e0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if e1 {
        return 0 as *mut Object;
    }
    if e2 {
        if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
            as libc::c_int as libc::c_long
            != 0
        {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"object_upto\0"))
                    .as_ptr(),
                b"Object.c\0" as *const u8 as *const libc::c_char,
                364 as libc::c_int,
                b"obj->type == OBJECT_VALUE\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
        return object_value_create(
            ast_expr_copy((*obj).offset),
            value_copy((*obj).c2rust_unnamed.Value),
        );
    }
    return object_range_create(
        ast_expr_copy((*obj).offset),
        range_create(
            ast_expr_difference_create(excl_up, lw),
            value_as_location(state_alloc(s)),
        ),
    );
}
#[no_mangle]
pub unsafe extern "C" fn object_from(
    mut obj: *mut Object,
    mut incl_lw: *mut AstExpr,
    mut s: *mut State,
) -> *mut Object {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    let mut prop0: *mut AstExpr = ast_expr_ge_create(ast_expr_copy(incl_lw), ast_expr_copy(up));
    let mut prop1: *mut AstExpr = ast_expr_eq_create(ast_expr_copy(incl_lw), ast_expr_copy(lw));
    let mut e0: bool = state_eval(s, prop0);
    let mut e1: bool = state_eval(s, prop1);
    ast_expr_destroy(prop1);
    ast_expr_destroy(prop0);
    if e0 {
        ast_expr_destroy(up);
        return 0 as *mut Object;
    }
    if e1 {
        if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
            as libc::c_int as libc::c_long
            != 0
        {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"object_from\0"))
                    .as_ptr(),
                b"Object.c\0" as *const u8 as *const libc::c_char,
                403 as libc::c_int,
                b"obj->type == OBJECT_VALUE\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
        ast_expr_destroy(up);
        return object_value_create(
            ast_expr_copy(incl_lw),
            value_copy((*obj).c2rust_unnamed.Value),
        );
    }
    return object_range_create(
        ast_expr_copy(incl_lw),
        range_create(
            ast_expr_difference_create(up, ast_expr_copy(incl_lw)),
            value_as_location(state_alloc(s)),
        ),
    );
}
#[no_mangle]
pub unsafe extern "C" fn object_dealloc(mut obj: *mut Object, mut s: *mut State) -> *mut error {
    match (*obj).type_0 as libc::c_uint {
        0 => return state_dealloc(s, (*obj).c2rust_unnamed.Value),
        1 => return range_dealloc((*obj).c2rust_unnamed.range, s),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(
                        b"object_dealloc\0",
                    ))
                    .as_ptr(),
                    b"Object.c\0" as *const u8 as *const libc::c_char,
                    432 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn object_getmember(
    mut obj: *mut Object,
    mut t: *mut ast_type,
    mut member: *mut libc::c_char,
    mut s: *mut State,
) -> *mut Object {
    return value_struct_member(getorcreatestruct(obj, t, s), member);
}
unsafe extern "C" fn getorcreatestruct(
    mut obj: *mut Object,
    mut t: *mut ast_type,
    mut s: *mut State,
) -> *mut Value {
    let mut v: *mut Value = object_as_value(obj);
    if !v.is_null() {
        return v;
    }
    let mut complete: *mut ast_type = ast_type_struct_complete(t, state_getext(s));
    if complete.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"getorcreatestruct\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            454 as libc::c_int,
            b"complete\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    v = value_struct_create(complete);
    object_assign(obj, v);
    return v;
}
#[no_mangle]
pub unsafe extern "C" fn object_getmembertype(
    mut obj: *mut Object,
    mut t: *mut ast_type,
    mut member: *mut libc::c_char,
    mut s: *mut State,
) -> *mut ast_type {
    return value_struct_membertype(getorcreatestruct(obj, t, s), member);
}
#[no_mangle]
pub unsafe extern "C" fn object_result_error_create(mut err: *mut error) -> *mut object_result {
    if err.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"object_result_error_create\0",
            ))
            .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            478 as libc::c_int,
            b"err\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut r: *mut object_result =
        malloc(::core::mem::size_of::<object_result>()) as *mut object_result;
    (*r).val = 0 as *mut Object;
    (*r).err = err;
    return r;
}
#[no_mangle]
pub unsafe extern "C" fn object_result_value_create(mut val: *mut Object) -> *mut object_result {
    let mut r: *mut object_result =
        malloc(::core::mem::size_of::<object_result>()) as *mut object_result;
    (*r).val = val;
    (*r).err = 0 as *mut error;
    return r;
}
#[no_mangle]
pub unsafe extern "C" fn object_result_destroy(mut res: *mut object_result) {
    if !((*res).err).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 22], &[libc::c_char; 22]>(b"object_result_destroy\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            498 as libc::c_int,
            b"!res->err\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !((*res).val).is_null() {
        object_destroy((*res).val);
    }
    free(res as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn object_result_iserror(mut res: *mut object_result) -> bool {
    return !((*res).err).is_null();
}
#[no_mangle]
pub unsafe extern "C" fn object_result_as_error(mut res: *mut object_result) -> *mut error {
    if ((*res).err).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"object_result_as_error\0",
            ))
            .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            514 as libc::c_int,
            b"res->err\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return (*res).err;
}
#[no_mangle]
pub unsafe extern "C" fn object_result_as_value(mut res: *mut object_result) -> *mut Object {
    if !((*res).err).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"object_result_as_value\0",
            ))
            .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            521 as libc::c_int,
            b"!res->err\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return (*res).val;
}
#[no_mangle]
pub unsafe extern "C" fn object_result_hasvalue(mut res: *mut object_result) -> bool {
    if object_result_iserror(res) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"object_result_hasvalue\0",
            ))
            .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            528 as libc::c_int,
            b"!object_result_iserror(res)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return !((*res).val).is_null();
}
#[no_mangle]
pub unsafe extern "C" fn range_create(
    mut size: *mut AstExpr,
    mut loc: *mut Location,
) -> *mut Range {
    let mut r: *mut Range = malloc(::core::mem::size_of::<Range>()) as *mut Range;
    (*r).size = size;
    (*r).loc = loc;
    return r;
}
#[no_mangle]
pub unsafe extern "C" fn range_copy(mut r: *mut Range) -> *mut Range {
    return range_create(ast_expr_copy((*r).size), location_copy((*r).loc));
}
#[no_mangle]
pub unsafe extern "C" fn range_destroy(mut r: *mut Range) {
    ast_expr_destroy((*r).size);
    location_destroy((*r).loc);
    free(r as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn range_str(mut r: *mut Range) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut size: *mut libc::c_char = ast_expr_str((*r).size);
    let mut loc: *mut libc::c_char = location_str((*r).loc);
    strbuilder_printf(
        b,
        b"virt:%s@%s\0" as *const u8 as *const libc::c_char,
        size,
        loc,
    );
    free(loc as *mut libc::c_void);
    free(size as *mut libc::c_void);
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe extern "C" fn range_size(mut r: *mut Range) -> *mut AstExpr {
    return (*r).size;
}
#[no_mangle]
pub unsafe extern "C" fn range_dealloc(mut r: *mut Range, mut s: *mut State) -> *mut error {
    return state_dealloc(s, value_ptr_create((*r).loc));
}
#[no_mangle]
pub unsafe extern "C" fn range_isdeallocand(mut r: *mut Range, mut s: *mut State) -> bool {
    return state_isdeallocand(s, (*r).loc);
}
#[no_mangle]
pub unsafe extern "C" fn range_references(
    mut r: *mut Range,
    mut loc: *mut Location,
    mut s: *mut State,
) -> bool {
    return location_references((*r).loc, loc, s);
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_create() -> *mut object_arr {
    let mut arr: *mut object_arr =
        calloc(1, ::core::mem::size_of::<object_arr>()) as *mut object_arr;
    if arr.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"object_arr_create\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            606 as libc::c_int,
            b"arr\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return arr;
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_destroy(mut arr: *mut object_arr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        object_destroy(*((*arr).Object).offset(i as isize));
        i += 1;
    }
    free((*arr).Object as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_copy(mut arr: *mut object_arr) -> *mut object_arr {
    let mut copy: *mut object_arr = object_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        object_arr_append(copy, object_copy(*((*arr).Object).offset(i as isize)));
        i += 1;
    }
    return copy;
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_allocs(mut arr: *mut object_arr) -> *mut *mut Object {
    return (*arr).Object;
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_nallocs(mut arr: *mut object_arr) -> libc::c_int {
    return (*arr).n;
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_index(
    mut arr: *mut object_arr,
    mut offset: *mut AstExpr,
    mut State: *mut State,
) -> libc::c_int {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if object_contains(*((*arr).Object).offset(i as isize), offset, State) {
            return i;
        }
        i += 1;
    }
    return -(1 as libc::c_int);
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_index_upperincl(
    mut arr: *mut object_arr,
    mut offset: *mut AstExpr,
    mut State: *mut State,
) -> libc::c_int {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if object_contains_upperincl(*((*arr).Object).offset(i as isize), offset, State) {
            return i;
        }
        i += 1;
    }
    return -(1 as libc::c_int);
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_insert(
    mut arr: *mut object_arr,
    mut index: libc::c_int,
    mut obj: *mut Object,
) -> libc::c_int {
    (*arr).n += 1;
    (*arr).Object = realloc(
        (*arr).Object as *mut libc::c_void,
        (::core::mem::size_of::<*mut Object>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut Object;
    if ((*arr).Object).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"object_arr_insert\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            675 as libc::c_int,
            b"arr->Object\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut i: libc::c_int = (*arr).n - 1 as libc::c_int;
    while i > index {
        let ref mut fresh0 = *((*arr).Object).offset(i as isize);
        *fresh0 = *((*arr).Object).offset((i - 1 as libc::c_int) as isize);
        i -= 1;
    }
    let ref mut fresh1 = *((*arr).Object).offset(index as isize);
    *fresh1 = obj;
    return index;
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_append(
    mut arr: *mut object_arr,
    mut obj: *mut Object,
) -> libc::c_int {
    return object_arr_insert(arr, (*arr).n, obj);
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_remove(mut arr: *mut object_arr, mut index: libc::c_int) {
    let mut i: libc::c_int = index;
    while i < (*arr).n - 1 as libc::c_int {
        let ref mut fresh2 = *((*arr).Object).offset(i as isize);
        *fresh2 = *((*arr).Object).offset((i + 1 as libc::c_int) as isize);
        i += 1;
    }
    (*arr).n -= 1;
    (*arr).Object = realloc(
        (*arr).Object as *mut libc::c_void,
        (::core::mem::size_of::<*mut Object>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut Object;
    if !(!((*arr).Object).is_null() || (*arr).n == 0) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"object_arr_remove\0"))
                .as_ptr(),
            b"Object.c\0" as *const u8 as *const libc::c_char,
            696 as libc::c_int,
            b"arr->Object || !arr->n\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_nobjects(mut arr: *mut object_arr) -> libc::c_int {
    return (*arr).n;
}
#[no_mangle]
pub unsafe extern "C" fn object_arr_objects(mut arr: *mut object_arr) -> *mut *mut Object {
    return (*arr).Object;
}
