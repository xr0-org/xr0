#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use libc::{calloc, free, malloc, realloc};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_difference_create,
    ast_expr_eq_create, ast_expr_ge_create, ast_expr_le_create, ast_expr_lt_create, ast_expr_str,
    ast_expr_sum_create, ast_type_struct_complete,
};
use crate::state::location::{location_copy, location_destroy, location_references, location_str};
use crate::state::state::{
    state_alloc, state_dealloc, state_eval, state_getext, state_isdeallocand,
};
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Error, Result};
use crate::value::{
    value_abstractcopy, value_as_location, value_copy, value_destroy, value_ptr_create,
    value_references, value_referencesheap, value_str, value_struct_create, value_struct_member,
    value_struct_membertype,
};
use crate::{cstr, strbuilder_write, AstExpr, AstType, Location, State, StrBuilder, Value};

pub struct Object {
    pub r#type: ObjectType,
    pub offset: *mut AstExpr,
    pub c2rust_unnamed: ObjectKind,
}

pub union ObjectKind {
    pub range: *mut Range,
    pub value: *mut Value,
}

pub struct Range {
    pub size: *mut AstExpr,
    pub loc: *mut Location,
}
pub type ObjectType = libc::c_uint;
pub const OBJECT_DEALLOCAND_RANGE: ObjectType = 1;
pub const OBJECT_VALUE: ObjectType = 0;

pub struct ObjectArr {
    pub n: libc::c_int,
    pub object: *mut *mut Object,
}

pub struct ObjectResult {
    pub val: *mut Object,
    pub err: *mut Error,
}

pub unsafe fn object_value_create(offset: *mut AstExpr, v: *mut Value) -> *mut Object {
    let obj: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    if obj.is_null() {
        panic!();
    }
    (*obj).offset = offset;
    (*obj).c2rust_unnamed.value = v;
    (*obj).r#type = OBJECT_VALUE;
    return obj;
}

pub unsafe fn object_range_create(offset: *mut AstExpr, r: *mut Range) -> *mut Object {
    if r.is_null() {
        panic!();
    }
    let obj: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    if obj.is_null() {
        panic!();
    }
    (*obj).offset = offset;
    (*obj).c2rust_unnamed.range = r;
    (*obj).r#type = OBJECT_DEALLOCAND_RANGE;
    return obj;
}

pub unsafe fn object_destroy(obj: *mut Object) {
    match (*obj).r#type {
        0 => {
            if !((*obj).c2rust_unnamed.value).is_null() {
                value_destroy((*obj).c2rust_unnamed.value);
            }
        }
        1 => {
            range_destroy((*obj).c2rust_unnamed.range);
        }
        _ => panic!(),
    }
    ast_expr_destroy((*obj).offset);
    free(obj as *mut libc::c_void);
}

pub unsafe fn object_copy(old: *mut Object) -> *mut Object {
    let new: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    (*new).offset = Box::into_raw(ast_expr_copy(&*(*old).offset));
    (*new).r#type = (*old).r#type;
    match (*old).r#type {
        0 => {
            (*new).c2rust_unnamed.value = if !((*old).c2rust_unnamed.value).is_null() {
                value_copy(&*(*old).c2rust_unnamed.value)
            } else {
                ptr::null_mut()
            };
        }
        1 => {
            (*new).c2rust_unnamed.range = range_copy((*old).c2rust_unnamed.range);
        }
        _ => panic!(),
    }
    return new;
}

pub unsafe fn object_abstractcopy(old: *mut Object, s: *mut State) -> *mut Object {
    match (*old).r#type {
        1 => object_copy(old),
        0 => object_value_create(
            Box::into_raw(ast_expr_copy(&*(*old).offset)),
            if !((*old).c2rust_unnamed.value).is_null() {
                value_abstractcopy(&*(*old).c2rust_unnamed.value, s)
            } else {
                ptr::null_mut()
            },
        ),
        _ => panic!(),
    }
}

pub unsafe fn object_str(obj: *mut Object) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_write!(b, "{{");
    let offset: *mut libc::c_char = ast_expr_str(&*(*obj).offset);
    strbuilder_write!(b, "{}:", cstr!(offset));
    free(offset as *mut libc::c_void);
    let inner: *mut libc::c_char = inner_str(obj);
    strbuilder_write!(b, "<{}>", cstr!(inner));
    free(inner as *mut libc::c_void);
    strbuilder_write!(b, "}}");
    return strbuilder_build(b);
}

unsafe fn inner_str(obj: *mut Object) -> *mut libc::c_char {
    match (*obj).r#type {
        0 => {
            if !((*obj).c2rust_unnamed.value).is_null() {
                value_str((*obj).c2rust_unnamed.value)
            } else {
                dynamic_str(b"\0" as *const u8 as *const libc::c_char)
            }
        }
        1 => range_str((*obj).c2rust_unnamed.range),
        _ => panic!(),
    }
}

pub unsafe fn object_referencesheap(obj: *mut Object, s: *mut State) -> bool {
    if !object_isvalue(obj) {
        return true;
    }
    return !((*obj).c2rust_unnamed.value).is_null()
        && value_referencesheap(&*(*obj).c2rust_unnamed.value, s);
}

pub unsafe fn object_hasvalue(obj: *mut Object) -> bool {
    if object_isvalue(obj) {
        return !((*obj).c2rust_unnamed.value).is_null();
    }
    false
}

pub unsafe fn object_isvalue(obj: *mut Object) -> bool {
    return (*obj).r#type as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint;
}

pub unsafe fn object_as_value(obj: *mut Object) -> *mut Value {
    if !((*obj).r#type as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*obj).c2rust_unnamed.value;
}

pub unsafe fn object_isdeallocand(obj: *mut Object, s: *mut State) -> bool {
    match (*obj).r#type {
        0 => {
            !((*obj).c2rust_unnamed.value).is_null()
                && state_isdeallocand(s, value_as_location(&*(*obj).c2rust_unnamed.value))
        }
        1 => range_isdeallocand((*obj).c2rust_unnamed.range, s),
        _ => panic!(),
    }
}

pub unsafe fn object_references(obj: *mut Object, loc: *mut Location, s: *mut State) -> bool {
    if (*obj).r#type as libc::c_uint == OBJECT_DEALLOCAND_RANGE as libc::c_int as libc::c_uint {
        return range_references((*obj).c2rust_unnamed.range, loc, s);
    }
    if !((*obj).r#type as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
        panic!();
    }
    let v: *mut Value = object_as_value(obj);
    !v.is_null() && value_references(v, loc, s)
}

pub unsafe fn object_assign(obj: *mut Object, val: *mut Value) -> *mut Error {
    if !((*obj).r#type as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
        panic!();
    }
    (*obj).c2rust_unnamed.value = val;
    return ptr::null_mut();
}
unsafe fn object_size(obj: *mut Object) -> *mut AstExpr {
    match (*obj).r#type {
        0 => Box::into_raw(ast_expr_constant_create(1 as libc::c_int)),
        1 => Box::into_raw(ast_expr_copy(&*range_size((*obj).c2rust_unnamed.range))),
        _ => panic!(),
    }
}

pub unsafe fn object_lower(obj: *mut Object) -> *mut AstExpr {
    return (*obj).offset;
}

pub unsafe fn object_upper(obj: *mut Object) -> *mut AstExpr {
    Box::into_raw(ast_expr_sum_create(
        ast_expr_copy(&*(*obj).offset),
        Box::from_raw(object_size(obj)),
    ))
}

pub unsafe fn object_contains(obj: *mut Object, offset: &AstExpr, s: *mut State) -> bool {
    let lw: *mut AstExpr = (*obj).offset;
    let up: *mut AstExpr = object_upper(obj);
    let of = offset;
    // Note: Original leaks the expressions to avoid double-freeing subexpressions.
    let e1 = ast_expr_le_create(ast_expr_copy(&*lw), ast_expr_copy(of));
    let e2 = ast_expr_lt_create(ast_expr_copy(of), ast_expr_copy(&*up));
    ast_expr_destroy(up);
    let result = state_eval(s, &e1) && state_eval(s, &e2);
    std::mem::forget(e1);
    std::mem::forget(e2);
    result
}

pub unsafe fn object_contains_upperincl(obj: *mut Object, offset: &AstExpr, s: *mut State) -> bool {
    let lw: *mut AstExpr = (*obj).offset;
    let up: *mut AstExpr = object_upper(obj);
    let of: *mut AstExpr = offset as *const AstExpr as *mut AstExpr;
    // Note: Original leaks the expressions to avoid double-freeing subexpressions.
    let lower_bound_expr = ast_expr_le_create(Box::from_raw(lw), Box::from_raw(of));
    let upper_bound_expr = ast_expr_le_create(Box::from_raw(of), Box::from_raw(up));
    let result = state_eval(s, &lower_bound_expr) && state_eval(s, &upper_bound_expr);
    std::mem::forget(lower_bound_expr);
    std::mem::forget(upper_bound_expr);
    result
}

pub unsafe fn object_isempty(obj: *mut Object, s: *mut State) -> bool {
    let lw: *mut AstExpr = (*obj).offset;
    let up: *mut AstExpr = object_upper(obj);
    // Note: Original leaks the expression to avoid double-freeing subexpressions.
    let expr = ast_expr_eq_create(Box::from_raw(lw), Box::from_raw(up));
    let result = state_eval(s, &expr);
    std::mem::forget(expr);
    result
}

pub unsafe fn object_contig_precedes(
    before: *mut Object,
    after: *mut Object,
    s: *mut State,
) -> bool {
    let lw: *mut AstExpr = object_upper(before);
    let up: *mut AstExpr = (*after).offset;
    // Note: Original leaks the expression to avoid double-freeing subexpressions.
    let expr = ast_expr_eq_create(Box::from_raw(lw), Box::from_raw(up));
    let result = state_eval(s, &expr);
    std::mem::forget(expr);
    result
}

pub unsafe fn object_issingular(obj: *mut Object, s: *mut State) -> bool {
    let lw: *mut AstExpr = (*obj).offset;
    let up: *mut AstExpr = object_upper(obj);
    let lw_succ = ast_expr_sum_create(
        Box::from_raw(lw),
        ast_expr_constant_create(1 as libc::c_int),
    );
    // Note: Original leaks the expression to avoid double-freeing subexpressions.
    let expr = ast_expr_eq_create(lw_succ, Box::from_raw(up));
    let result = state_eval(s, &expr);
    std::mem::forget(expr);
    result
}

pub unsafe fn object_upto(obj: *mut Object, excl_up: *mut AstExpr, s: *mut State) -> *mut Object {
    let lw: *mut AstExpr = (*obj).offset;
    let up: *mut AstExpr = object_upper(obj);
    let prop0 = ast_expr_le_create(ast_expr_copy(&*lw), ast_expr_copy(&*excl_up));
    let prop1 = ast_expr_eq_create(ast_expr_copy(&*lw), ast_expr_copy(&*excl_up));
    let prop2 = ast_expr_eq_create(ast_expr_copy(&*up), ast_expr_copy(&*excl_up));
    let e0: bool = state_eval(s, &*prop0);
    let e1: bool = state_eval(s, &*prop1);
    let e2: bool = state_eval(s, &*prop2);
    drop(prop2);
    drop(prop1);
    drop(prop0);
    ast_expr_destroy(up);
    if !e0 {
        panic!();
    }
    if e1 {
        return ptr::null_mut();
    }
    if e2 {
        if !((*obj).r#type as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
            panic!();
        }
        return object_value_create(
            Box::into_raw(ast_expr_copy(&*(*obj).offset)),
            value_copy(&*(*obj).c2rust_unnamed.value),
        );
    }
    return object_range_create(
        Box::into_raw(ast_expr_copy(&*(*obj).offset)),
        range_create(
            Box::into_raw(ast_expr_difference_create(
                Box::from_raw(excl_up),
                Box::from_raw(lw),
            )),
            value_as_location(&*state_alloc(s)),
        ),
    );
}

pub unsafe fn object_from(obj: *mut Object, incl_lw: &AstExpr, s: *mut State) -> *mut Object {
    let lw: *mut AstExpr = (*obj).offset;
    let up: *mut AstExpr = object_upper(obj);
    let prop0: *mut AstExpr = Box::into_raw(ast_expr_ge_create(
        ast_expr_copy(incl_lw),
        ast_expr_copy(&*up),
    ));
    let prop1: *mut AstExpr = Box::into_raw(ast_expr_eq_create(
        ast_expr_copy(incl_lw),
        ast_expr_copy(&*lw),
    ));
    let e0: bool = state_eval(s, &*prop0);
    let e1: bool = state_eval(s, &*prop1);
    ast_expr_destroy(prop1);
    ast_expr_destroy(prop0);
    if e0 {
        ast_expr_destroy(up);
        return ptr::null_mut();
    }
    if e1 {
        if !((*obj).r#type as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
            panic!();
        }
        ast_expr_destroy(up);
        return object_value_create(
            Box::into_raw(ast_expr_copy(incl_lw)),
            value_copy(&*(*obj).c2rust_unnamed.value),
        );
    }
    return object_range_create(
        Box::into_raw(ast_expr_copy(incl_lw)),
        range_create(
            Box::into_raw(ast_expr_difference_create(
                Box::from_raw(up),
                ast_expr_copy(incl_lw),
            )),
            value_as_location(&*state_alloc(s)),
        ),
    );
}

pub unsafe fn object_dealloc(obj: *mut Object, s: *mut State) -> Result<()> {
    match (*obj).r#type {
        0 => state_dealloc(s, (*obj).c2rust_unnamed.value),
        1 => range_dealloc((*obj).c2rust_unnamed.range, s),
        _ => panic!(),
    }
}

pub unsafe fn object_getmember(
    obj: *mut Object,
    t: *mut AstType,
    member: *mut libc::c_char,
    s: *mut State,
) -> *mut Object {
    return value_struct_member(getorcreatestruct(obj, t, s), member);
}

unsafe fn getorcreatestruct(obj: *mut Object, t: *mut AstType, s: *mut State) -> *mut Value {
    let mut v: *mut Value = object_as_value(obj);
    if !v.is_null() {
        return v;
    }
    let complete: *mut AstType = ast_type_struct_complete(t, state_getext(s));
    if complete.is_null() {
        panic!();
    }
    v = value_struct_create(complete);
    object_assign(obj, v);
    return v;
}

pub unsafe fn object_getmembertype(
    obj: *mut Object,
    t: *mut AstType,
    member: *mut libc::c_char,
    s: *mut State,
) -> *mut AstType {
    return value_struct_membertype(getorcreatestruct(obj, t, s), member);
}

pub unsafe fn range_create(size: *mut AstExpr, loc: *mut Location) -> *mut Range {
    let r: *mut Range = malloc(::core::mem::size_of::<Range>()) as *mut Range;
    (*r).size = size;
    (*r).loc = loc;
    return r;
}

pub unsafe fn range_copy(r: *mut Range) -> *mut Range {
    return range_create(
        Box::into_raw(ast_expr_copy(&*(*r).size)),
        location_copy(&*(*r).loc),
    );
}

pub unsafe fn range_destroy(r: *mut Range) {
    ast_expr_destroy((*r).size);
    location_destroy((*r).loc);
    free(r as *mut libc::c_void);
}

pub unsafe fn range_str(r: *mut Range) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let size: *mut libc::c_char = ast_expr_str(&*(*r).size);
    let loc: *mut libc::c_char = location_str(&*(*r).loc);
    strbuilder_write!(b, "virt:{}@{}", cstr!(size), cstr!(loc));
    free(loc as *mut libc::c_void);
    free(size as *mut libc::c_void);
    return strbuilder_build(b);
}

pub unsafe fn range_size(r: *mut Range) -> *mut AstExpr {
    return (*r).size;
}

pub unsafe fn range_dealloc(r: *mut Range, s: *mut State) -> Result<()> {
    state_dealloc(s, value_ptr_create((*r).loc))
}

pub unsafe fn range_isdeallocand(r: *mut Range, s: *mut State) -> bool {
    return state_isdeallocand(s, (*r).loc);
}

pub unsafe fn range_references(r: *mut Range, loc: *mut Location, s: *mut State) -> bool {
    return location_references((*r).loc, loc, s);
}

pub unsafe fn object_arr_create() -> *mut ObjectArr {
    let arr: *mut ObjectArr = calloc(1, ::core::mem::size_of::<ObjectArr>()) as *mut ObjectArr;
    if arr.is_null() {
        panic!();
    }
    return arr;
}

pub unsafe fn object_arr_destroy(arr: *mut ObjectArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        object_destroy(*((*arr).object).offset(i as isize));
        i += 1;
    }
    free((*arr).object as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}

pub unsafe fn object_arr_copy(arr: *mut ObjectArr) -> *mut ObjectArr {
    let copy: *mut ObjectArr = object_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        object_arr_append(copy, object_copy(*((*arr).object).offset(i as isize)));
        i += 1;
    }
    return copy;
}

pub unsafe fn object_arr_allocs(arr: *mut ObjectArr) -> *mut *mut Object {
    return (*arr).object;
}

pub unsafe fn object_arr_nallocs(arr: *mut ObjectArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn object_arr_index(
    arr: *mut ObjectArr,
    offset: &AstExpr,
    state: *mut State,
) -> libc::c_int {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if object_contains(*((*arr).object).offset(i as isize), offset, state) {
            return i;
        }
        i += 1;
    }
    return -(1 as libc::c_int);
}

pub unsafe fn object_arr_index_upperincl(
    arr: *mut ObjectArr,
    offset: &AstExpr,
    state: *mut State,
) -> libc::c_int {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if object_contains_upperincl(*((*arr).object).offset(i as isize), offset, state) {
            return i;
        }
        i += 1;
    }
    return -(1 as libc::c_int);
}

pub unsafe fn object_arr_insert(
    arr: *mut ObjectArr,
    index: libc::c_int,
    obj: *mut Object,
) -> libc::c_int {
    (*arr).n += 1;
    (*arr).object = realloc(
        (*arr).object as *mut libc::c_void,
        (::core::mem::size_of::<*mut Object>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut Object;
    if ((*arr).object).is_null() {
        panic!();
    }
    let mut i: libc::c_int = (*arr).n - 1 as libc::c_int;
    while i > index {
        let ref mut fresh0 = *((*arr).object).offset(i as isize);
        *fresh0 = *((*arr).object).offset((i - 1 as libc::c_int) as isize);
        i -= 1;
    }
    let ref mut fresh1 = *((*arr).object).offset(index as isize);
    *fresh1 = obj;
    return index;
}

pub unsafe fn object_arr_append(arr: *mut ObjectArr, obj: *mut Object) -> libc::c_int {
    return object_arr_insert(arr, (*arr).n, obj);
}

pub unsafe fn object_arr_remove(arr: *mut ObjectArr, index: libc::c_int) {
    let mut i: libc::c_int = index;
    while i < (*arr).n - 1 as libc::c_int {
        let ref mut fresh2 = *((*arr).object).offset(i as isize);
        *fresh2 = *((*arr).object).offset((i + 1 as libc::c_int) as isize);
        i += 1;
    }
    (*arr).n -= 1;
    (*arr).object = realloc(
        (*arr).object as *mut libc::c_void,
        (::core::mem::size_of::<*mut Object>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut Object;
    if !(!((*arr).object).is_null() || (*arr).n == 0) {
        panic!();
    }
}

pub unsafe fn object_arr_nobjects(arr: *mut ObjectArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn object_arr_objects(arr: *mut ObjectArr) -> *mut *mut Object {
    return (*arr).object;
}
