#![allow(
    dead_code,
    mutable_transmutes,
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
use crate::state::location::{location_copy, location_destroy, location_references, location_str};
use crate::state::state::{
    state_alloc, state_dealloc, state_eval, state_getext, state_isdeallocand,
};
use crate::util::{dynamic_str, error, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::value::{
    value_abstractcopy, value_as_location, value_copy, value_destroy, value_ptr_create,
    value_references, value_referencesheap, value_str, value_struct_create, value_struct_member,
    value_struct_membertype,
};
use crate::{AstExpr, AstType, Location, State, StrBuilder, Value};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Object {
    pub type_0: ObjectType,
    pub offset: *mut AstExpr,
    pub c2rust_unnamed: C2RustUnnamed,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub range: *mut Range,
    pub value: *mut Value,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Range {
    pub size: *mut AstExpr,
    pub loc: *mut Location,
}
pub type ObjectType = libc::c_uint;
pub const OBJECT_DEALLOCAND_RANGE: ObjectType = 1;
pub const OBJECT_VALUE: ObjectType = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ObjectArr {
    pub n: libc::c_int,
    pub object: *mut *mut Object,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ObjectResult {
    pub val: *mut Object,
    pub err: *mut error,
}

pub unsafe fn object_value_create(mut offset: *mut AstExpr, mut v: *mut Value) -> *mut Object {
    let mut obj: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    if obj.is_null() {
        panic!();
    }
    (*obj).offset = offset;
    (*obj).c2rust_unnamed.value = v;
    (*obj).type_0 = OBJECT_VALUE;
    return obj;
}

pub unsafe fn object_range_create(mut offset: *mut AstExpr, mut r: *mut Range) -> *mut Object {
    if r.is_null() {
        panic!();
    }
    let mut obj: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    if obj.is_null() {
        panic!();
    }
    (*obj).offset = offset;
    (*obj).c2rust_unnamed.range = r;
    (*obj).type_0 = OBJECT_DEALLOCAND_RANGE;
    return obj;
}

pub unsafe fn object_destroy(mut obj: *mut Object) {
    match (*obj).type_0 {
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

pub unsafe fn object_copy(mut old: *mut Object) -> *mut Object {
    let mut new: *mut Object = malloc(::core::mem::size_of::<Object>()) as *mut Object;
    (*new).offset = ast_expr_copy((*old).offset);
    (*new).type_0 = (*old).type_0;
    match (*old).type_0 {
        0 => {
            (*new).c2rust_unnamed.value = if !((*old).c2rust_unnamed.value).is_null() {
                value_copy((*old).c2rust_unnamed.value)
            } else {
                0 as *mut Value
            };
        }
        1 => {
            (*new).c2rust_unnamed.range = range_copy((*old).c2rust_unnamed.range);
        }
        _ => panic!(),
    }
    return new;
}

pub unsafe fn object_abstractcopy(mut old: *mut Object, mut s: *mut State) -> *mut Object {
    match (*old).type_0 {
        1 => object_copy(old),
        0 => object_value_create(
            ast_expr_copy((*old).offset),
            if !((*old).c2rust_unnamed.value).is_null() {
                value_abstractcopy((*old).c2rust_unnamed.value, s)
            } else {
                0 as *mut Value
            },
        ),
        _ => panic!(),
    }
}

pub unsafe fn object_str(mut obj: *mut Object) -> *mut libc::c_char {
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

unsafe fn inner_str(mut obj: *mut Object) -> *mut libc::c_char {
    match (*obj).type_0 {
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

pub unsafe fn object_referencesheap(mut obj: *mut Object, mut s: *mut State) -> bool {
    if !object_isvalue(obj) {
        return true;
    }
    return !((*obj).c2rust_unnamed.value).is_null()
        && value_referencesheap((*obj).c2rust_unnamed.value, s);
}

pub unsafe fn object_hasvalue(mut obj: *mut Object) -> bool {
    if object_isvalue(obj) {
        return !((*obj).c2rust_unnamed.value).is_null();
    }
    false
}

pub unsafe fn object_isvalue(mut obj: *mut Object) -> bool {
    return (*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint;
}

pub unsafe fn object_as_value(mut obj: *mut Object) -> *mut Value {
    if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*obj).c2rust_unnamed.value;
}

pub unsafe fn object_isdeallocand(mut obj: *mut Object, mut s: *mut State) -> bool {
    match (*obj).type_0 {
        0 => {
            !((*obj).c2rust_unnamed.value).is_null()
                && state_isdeallocand(s, value_as_location((*obj).c2rust_unnamed.value))
                    as libc::c_int
                    != 0
        }
        1 => range_isdeallocand((*obj).c2rust_unnamed.range, s),
        _ => panic!(),
    }
}

pub unsafe fn object_references(
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
        panic!();
    }
    let mut v: *mut Value = object_as_value(obj);
    return if !v.is_null() {
        value_references(v, loc, s) as libc::c_int
    } else {
        0 as libc::c_int
    } != 0;
}

pub unsafe fn object_assign(mut obj: *mut Object, mut val: *mut Value) -> *mut error {
    if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    (*obj).c2rust_unnamed.value = val;
    return 0 as *mut error;
}
unsafe fn object_size(mut obj: *mut Object) -> *mut AstExpr {
    match (*obj).type_0 {
        0 => ast_expr_constant_create(1 as libc::c_int),
        1 => ast_expr_copy(range_size((*obj).c2rust_unnamed.range)),
        _ => panic!(),
    }
}

pub unsafe fn object_lower(mut obj: *mut Object) -> *mut AstExpr {
    return (*obj).offset;
}

pub unsafe fn object_upper(mut obj: *mut Object) -> *mut AstExpr {
    return ast_expr_sum_create(ast_expr_copy((*obj).offset), object_size(obj));
}

pub unsafe fn object_contains(
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

pub unsafe fn object_contains_upperincl(
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

pub unsafe fn object_isempty(mut obj: *mut Object, mut s: *mut State) -> bool {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    return state_eval(s, ast_expr_eq_create(lw, up));
}

pub unsafe fn object_contig_precedes(
    mut before: *mut Object,
    mut after: *mut Object,
    mut s: *mut State,
) -> bool {
    let mut lw: *mut AstExpr = object_upper(before);
    let mut up: *mut AstExpr = (*after).offset;
    return state_eval(s, ast_expr_eq_create(lw, up));
}

pub unsafe fn object_issingular(mut obj: *mut Object, mut s: *mut State) -> bool {
    let mut lw: *mut AstExpr = (*obj).offset;
    let mut up: *mut AstExpr = object_upper(obj);
    let mut lw_succ: *mut AstExpr =
        ast_expr_sum_create(lw, ast_expr_constant_create(1 as libc::c_int));
    return state_eval(s, ast_expr_eq_create(lw_succ, up));
}

pub unsafe fn object_upto(
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
    if !e0 {
        panic!();
    }
    if e1 {
        return 0 as *mut Object;
    }
    if e2 {
        if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint)
            as libc::c_int as libc::c_long
            != 0
        {
            panic!();
        } else {
        };
        return object_value_create(
            ast_expr_copy((*obj).offset),
            value_copy((*obj).c2rust_unnamed.value),
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

pub unsafe fn object_from(
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
        if !((*obj).type_0 as libc::c_uint == OBJECT_VALUE as libc::c_int as libc::c_uint) {
            panic!();
        }
        ast_expr_destroy(up);
        return object_value_create(
            ast_expr_copy(incl_lw),
            value_copy((*obj).c2rust_unnamed.value),
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

pub unsafe fn object_dealloc(mut obj: *mut Object, mut s: *mut State) -> *mut error {
    match (*obj).type_0 {
        0 => state_dealloc(s, (*obj).c2rust_unnamed.value),
        1 => range_dealloc((*obj).c2rust_unnamed.range, s),
        _ => panic!(),
    }
}

pub unsafe fn object_getmember(
    mut obj: *mut Object,
    mut t: *mut AstType,
    mut member: *mut libc::c_char,
    mut s: *mut State,
) -> *mut Object {
    return value_struct_member(getorcreatestruct(obj, t, s), member);
}
unsafe fn getorcreatestruct(
    mut obj: *mut Object,
    mut t: *mut AstType,
    mut s: *mut State,
) -> *mut Value {
    let mut v: *mut Value = object_as_value(obj);
    if !v.is_null() {
        return v;
    }
    let mut complete: *mut AstType = ast_type_struct_complete(t, state_getext(s));
    if complete.is_null() {
        panic!();
    }
    v = value_struct_create(complete);
    object_assign(obj, v);
    return v;
}

pub unsafe fn object_getmembertype(
    mut obj: *mut Object,
    mut t: *mut AstType,
    mut member: *mut libc::c_char,
    mut s: *mut State,
) -> *mut AstType {
    return value_struct_membertype(getorcreatestruct(obj, t, s), member);
}

pub unsafe fn object_result_error_create(mut err: *mut error) -> *mut ObjectResult {
    if err.is_null() {
        panic!();
    }
    let mut r: *mut ObjectResult =
        malloc(::core::mem::size_of::<ObjectResult>()) as *mut ObjectResult;
    (*r).val = 0 as *mut Object;
    (*r).err = err;
    return r;
}

pub unsafe fn object_result_value_create(mut val: *mut Object) -> *mut ObjectResult {
    let mut r: *mut ObjectResult =
        malloc(::core::mem::size_of::<ObjectResult>()) as *mut ObjectResult;
    (*r).val = val;
    (*r).err = 0 as *mut error;
    return r;
}

pub unsafe fn object_result_destroy(mut res: *mut ObjectResult) {
    if !((*res).err).is_null() {
        panic!();
    }
    if !((*res).val).is_null() {
        object_destroy((*res).val);
    }
    free(res as *mut libc::c_void);
}

pub unsafe fn object_result_iserror(mut res: *mut ObjectResult) -> bool {
    return !((*res).err).is_null();
}

pub unsafe fn object_result_as_error(mut res: *mut ObjectResult) -> *mut error {
    if ((*res).err).is_null() {
        panic!();
    }
    return (*res).err;
}

pub unsafe fn object_result_as_value(mut res: *mut ObjectResult) -> *mut Object {
    if !((*res).err).is_null() {
        panic!();
    }
    return (*res).val;
}

pub unsafe fn object_result_hasvalue(mut res: *mut ObjectResult) -> bool {
    if object_result_iserror(res) {
        panic!();
    }
    return !((*res).val).is_null();
}

pub unsafe fn range_create(mut size: *mut AstExpr, mut loc: *mut Location) -> *mut Range {
    let mut r: *mut Range = malloc(::core::mem::size_of::<Range>()) as *mut Range;
    (*r).size = size;
    (*r).loc = loc;
    return r;
}

pub unsafe fn range_copy(mut r: *mut Range) -> *mut Range {
    return range_create(ast_expr_copy((*r).size), location_copy((*r).loc));
}

pub unsafe fn range_destroy(mut r: *mut Range) {
    ast_expr_destroy((*r).size);
    location_destroy((*r).loc);
    free(r as *mut libc::c_void);
}

pub unsafe fn range_str(mut r: *mut Range) -> *mut libc::c_char {
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

pub unsafe fn range_size(mut r: *mut Range) -> *mut AstExpr {
    return (*r).size;
}

pub unsafe fn range_dealloc(mut r: *mut Range, mut s: *mut State) -> *mut error {
    return state_dealloc(s, value_ptr_create((*r).loc));
}

pub unsafe fn range_isdeallocand(mut r: *mut Range, mut s: *mut State) -> bool {
    return state_isdeallocand(s, (*r).loc);
}

pub unsafe fn range_references(
    mut r: *mut Range,
    mut loc: *mut Location,
    mut s: *mut State,
) -> bool {
    return location_references((*r).loc, loc, s);
}

pub unsafe fn object_arr_create() -> *mut ObjectArr {
    let mut arr: *mut ObjectArr = calloc(1, ::core::mem::size_of::<ObjectArr>()) as *mut ObjectArr;
    if arr.is_null() {
        panic!();
    }
    return arr;
}

pub unsafe fn object_arr_destroy(mut arr: *mut ObjectArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        object_destroy(*((*arr).object).offset(i as isize));
        i += 1;
    }
    free((*arr).object as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}

pub unsafe fn object_arr_copy(mut arr: *mut ObjectArr) -> *mut ObjectArr {
    let mut copy: *mut ObjectArr = object_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        object_arr_append(copy, object_copy(*((*arr).object).offset(i as isize)));
        i += 1;
    }
    return copy;
}

pub unsafe fn object_arr_allocs(mut arr: *mut ObjectArr) -> *mut *mut Object {
    return (*arr).object;
}

pub unsafe fn object_arr_nallocs(mut arr: *mut ObjectArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn object_arr_index(
    mut arr: *mut ObjectArr,
    mut offset: *mut AstExpr,
    mut state: *mut State,
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
    mut arr: *mut ObjectArr,
    mut offset: *mut AstExpr,
    mut state: *mut State,
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
    mut arr: *mut ObjectArr,
    mut index: libc::c_int,
    mut obj: *mut Object,
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

pub unsafe fn object_arr_append(mut arr: *mut ObjectArr, mut obj: *mut Object) -> libc::c_int {
    return object_arr_insert(arr, (*arr).n, obj);
}

pub unsafe fn object_arr_remove(mut arr: *mut ObjectArr, mut index: libc::c_int) {
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

pub unsafe fn object_arr_nobjects(mut arr: *mut ObjectArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn object_arr_objects(mut arr: *mut ObjectArr) -> *mut *mut Object {
    return (*arr).object;
}
