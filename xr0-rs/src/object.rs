use std::ptr;

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_difference_create,
    ast_expr_eq_create, ast_expr_ge_create, ast_expr_le_create, ast_expr_lt_create,
    ast_expr_sum_create, ast_type_struct_complete,
};
use crate::state::location::{location_references, location_str};
use crate::state::state::{
    state_alloc, state_dealloc, state_eval, state_getext, state_isdeallocand,
};
use crate::util::{strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::value::{
    value_abstractcopy, value_as_location, value_copy, value_destroy, value_into_location,
    value_ptr_create, value_references, value_referencesheap, value_str, value_struct_create,
    value_struct_member, value_struct_membertype,
};
use crate::{strbuilder_write, AstExpr, AstType, Location, State, Value};

pub struct Object {
    pub kind: ObjectKind,
    pub offset: Box<AstExpr>,
}

pub enum ObjectKind {
    DeallocandRange(Box<Range>),
    Value(*mut Value),
}

#[derive(Clone)]
pub struct Range {
    size: Box<AstExpr>,
    loc: Box<Location>,
}

pub unsafe fn object_value_create(offset: Box<AstExpr>, v: *mut Value) -> Box<Object> {
    Box::new(Object {
        kind: ObjectKind::Value(v),
        offset,
    })
}

pub unsafe fn object_range_create(offset: Box<AstExpr>, r: Box<Range>) -> Box<Object> {
    Box::new(Object {
        kind: ObjectKind::DeallocandRange(r),
        offset,
    })
}

pub unsafe fn object_destroy(obj: *mut Object) {
    drop(Box::from_raw(obj));
}

impl Drop for Object {
    fn drop(&mut self) {
        unsafe {
            match &self.kind {
                ObjectKind::Value(v) => {
                    if !(*v).is_null() {
                        value_destroy(*v);
                    }
                }
                ObjectKind::DeallocandRange(_) => {}
            }
        }
    }
}

pub unsafe fn object_copy(old: *mut Object) -> Box<Object> {
    Box::new(Object {
        kind: match &(*old).kind {
            ObjectKind::Value(v) => ObjectKind::Value(if !(*v).is_null() {
                value_copy(&**v)
            } else {
                ptr::null_mut()
            }),
            ObjectKind::DeallocandRange(range) => ObjectKind::DeallocandRange(range.clone()),
        },
        offset: ast_expr_copy(&*(*old).offset),
    })
}

pub unsafe fn object_abstractcopy(old: *mut Object, s: *mut State) -> Box<Object> {
    match &(*old).kind {
        ObjectKind::DeallocandRange(_) => object_copy(old),
        ObjectKind::Value(v) => object_value_create(
            ast_expr_copy(&*(*old).offset),
            if !(*v).is_null() {
                value_abstractcopy(&**v, s)
            } else {
                ptr::null_mut()
            },
        ),
    }
}

pub unsafe fn object_str(obj: *mut Object) -> OwningCStr {
    let mut b = strbuilder_create();
    strbuilder_write!(b, "{{");
    strbuilder_write!(b, "{}:", &*(*obj).offset);
    strbuilder_write!(b, "<{}>", inner_str(obj));
    strbuilder_write!(b, "}}");
    strbuilder_build(b)
}

unsafe fn inner_str(obj: *mut Object) -> OwningCStr {
    match &(*obj).kind {
        ObjectKind::Value(v) => {
            if !(*v).is_null() {
                value_str(&**v)
            } else {
                OwningCStr::empty()
            }
        }
        ObjectKind::DeallocandRange(range) => range_str(range),
    }
}

pub unsafe fn object_referencesheap(obj: *mut Object, s: *mut State) -> bool {
    match &(*obj).kind {
        ObjectKind::Value(v) => !(*v).is_null() && value_referencesheap(&**v, s),
        _ => true,
    }
}

pub unsafe fn object_hasvalue(obj: *mut Object) -> bool {
    match &(*obj).kind {
        ObjectKind::Value(v) => !(*v).is_null(),
        _ => false,
    }
}

pub unsafe fn object_isvalue(obj: *mut Object) -> bool {
    matches!((*obj).kind, ObjectKind::Value(_))
}

pub unsafe fn object_as_value(obj: *mut Object) -> *mut Value {
    let ObjectKind::Value(v) = &(*obj).kind else {
        panic!();
    };
    *v
}

pub unsafe fn object_isdeallocand(obj: &Object, s: *mut State) -> bool {
    match &obj.kind {
        ObjectKind::Value(v) => !(*v).is_null() && state_isdeallocand(s, value_as_location(&**v)),
        ObjectKind::DeallocandRange(range) => range_isdeallocand(range, s),
    }
}

pub unsafe fn object_references(obj: &Object, loc: &Location, s: *mut State) -> bool {
    match &obj.kind {
        ObjectKind::DeallocandRange(range) => range_references(range, loc, s),
        ObjectKind::Value(v) => !(*v).is_null() && value_references(*v, loc, s),
    }
}

pub unsafe fn object_assign(obj: &mut Object, val: *mut Value) -> *mut Error {
    if let ObjectKind::Value(v) = &mut (*obj).kind {
        *v = val;
    } else {
        panic!();
    }
    ptr::null_mut()
}

unsafe fn object_size(obj: &Object) -> *mut AstExpr {
    match &obj.kind {
        ObjectKind::Value(_) => Box::into_raw(ast_expr_constant_create(1)),
        ObjectKind::DeallocandRange(range) => Box::into_raw(ast_expr_copy(range_size(range))),
    }
}

pub unsafe fn object_lower(obj: &mut Object) -> *mut AstExpr {
    &mut *obj.offset
}

pub unsafe fn object_upper(obj: &Object) -> *mut AstExpr {
    Box::into_raw(ast_expr_sum_create(
        ast_expr_copy(&*(*obj).offset),
        Box::from_raw(object_size(obj)),
    ))
}

pub unsafe fn object_contains(obj: &Object, offset: &AstExpr, s: &State) -> bool {
    let lw = &*obj.offset;
    let up: *mut AstExpr = object_upper(obj);
    let of = offset;
    // Note: Original leaks the expressions to avoid double-freeing subexpressions.
    let e1 = ast_expr_le_create(ast_expr_copy(lw), ast_expr_copy(of));
    let e2 = ast_expr_lt_create(ast_expr_copy(of), ast_expr_copy(&*up));
    ast_expr_destroy(up);
    let result = state_eval(s, &e1) && state_eval(s, &e2);
    std::mem::forget(e1);
    std::mem::forget(e2);
    result
}

pub unsafe fn object_contains_upperincl(obj: *mut Object, offset: &AstExpr, s: &State) -> bool {
    let lw: *mut AstExpr = &mut *(*obj).offset;
    let up: *mut AstExpr = object_upper(&*obj);
    let of: *mut AstExpr = offset as *const AstExpr as *mut AstExpr;
    // Note: Original leaks the expressions to avoid double-freeing subexpressions.
    let lower_bound_expr = ast_expr_le_create(Box::from_raw(lw), Box::from_raw(of));
    let upper_bound_expr = ast_expr_le_create(Box::from_raw(of), Box::from_raw(up));
    let result = state_eval(s, &lower_bound_expr) && state_eval(s, &upper_bound_expr);
    std::mem::forget(lower_bound_expr);
    std::mem::forget(upper_bound_expr);
    result
}

#[allow(dead_code)]
pub unsafe fn object_isempty(obj: *mut Object, s: &State) -> bool {
    let lw: *mut AstExpr = &mut *(*obj).offset;
    let up: *mut AstExpr = object_upper(&*obj);
    // Note: Original leaks the expression to avoid double-freeing subexpressions.
    let expr = ast_expr_eq_create(Box::from_raw(lw), Box::from_raw(up));
    let result = state_eval(s, &expr);
    std::mem::forget(expr);
    result
}

pub unsafe fn object_contig_precedes(before: *mut Object, after: *mut Object, s: &State) -> bool {
    let lw: *mut AstExpr = object_upper(&*before);
    let up: *mut AstExpr = &mut *(*after).offset;
    // Note: Original leaks the expression to avoid double-freeing subexpressions.
    let expr = ast_expr_eq_create(Box::from_raw(lw), Box::from_raw(up));
    let result = state_eval(s, &expr);
    std::mem::forget(expr);
    result
}

#[allow(dead_code)]
pub unsafe fn object_issingular(obj: *mut Object, s: &State) -> bool {
    let lw: *mut AstExpr = &mut *(*obj).offset;
    let up: *mut AstExpr = object_upper(&*obj);
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

pub unsafe fn object_upto(
    obj: *mut Object,
    excl_up: *mut AstExpr,
    s: *mut State,
) -> Option<Box<Object>> {
    let lw: *mut AstExpr = &mut *(*obj).offset;
    let up: *mut AstExpr = object_upper(&*obj);
    let prop0 = ast_expr_le_create(ast_expr_copy(&*lw), ast_expr_copy(&*excl_up));
    let prop1 = ast_expr_eq_create(ast_expr_copy(&*lw), ast_expr_copy(&*excl_up));
    let prop2 = ast_expr_eq_create(ast_expr_copy(&*up), ast_expr_copy(&*excl_up));
    let e0: bool = state_eval(&*s, &prop0);
    let e1: bool = state_eval(&*s, &prop1);
    let e2: bool = state_eval(&*s, &prop2);
    drop(prop2);
    drop(prop1);
    drop(prop0);
    ast_expr_destroy(up);
    if !e0 {
        panic!();
    }
    if e1 {
        return None;
    }
    if e2 {
        let ObjectKind::Value(v) = &(*obj).kind else {
            panic!();
        };
        return Some(object_value_create(
            ast_expr_copy(&*(*obj).offset),
            value_copy(&**v),
        ));
    }

    // Note: This makes one copy of `obj->offset` but also just does `Box::from_raw(lw)` which ...
    // seems like it's take ownership of `obj->offset`, and maybe we leak `obj` to avoid a double
    // free?
    Some(object_range_create(
        ast_expr_copy(&*(*obj).offset),
        range_create(
            ast_expr_difference_create(Box::from_raw(excl_up), Box::from_raw(lw)),
            value_into_location(state_alloc(s)),
        ),
    ))
}

pub unsafe fn object_from(
    obj: *mut Object,
    incl_lw: &AstExpr,
    s: *mut State,
) -> Option<Box<Object>> {
    let lw = &(*obj).offset;
    let up: *mut AstExpr = object_upper(&*obj);
    let prop0 = ast_expr_ge_create(ast_expr_copy(incl_lw), ast_expr_copy(&*up));
    let prop1 = ast_expr_eq_create(ast_expr_copy(incl_lw), ast_expr_copy(lw));
    let e0: bool = state_eval(&*s, &prop0);
    let e1: bool = state_eval(&*s, &prop1);
    drop(prop1);
    drop(prop0);
    if e0 {
        ast_expr_destroy(up);
        return None;
    }
    if e1 {
        let ObjectKind::Value(v) = &(*obj).kind else {
            panic!();
        };
        ast_expr_destroy(up);
        return Some(object_value_create(
            ast_expr_copy(incl_lw),
            value_copy(&**v),
        ));
    }
    Some(object_range_create(
        ast_expr_copy(incl_lw),
        range_create(
            ast_expr_difference_create(Box::from_raw(up), ast_expr_copy(incl_lw)),
            value_into_location(state_alloc(s)),
        ),
    ))
}

pub unsafe fn object_dealloc(obj: *mut Object, s: *mut State) -> Result<()> {
    match &(*obj).kind {
        ObjectKind::Value(v) => state_dealloc(s, *v),
        ObjectKind::DeallocandRange(range) => range_dealloc(range, s),
    }
}

pub unsafe fn object_getmember(
    obj: *mut Object,
    t: &AstType,
    member: *mut libc::c_char,
    s: *mut State,
) -> *mut Object {
    value_struct_member(getorcreatestruct(obj, t, s), member)
}

unsafe fn getorcreatestruct(obj: *mut Object, t: &AstType, s: *mut State) -> *mut Value {
    let mut v: *mut Value = object_as_value(obj);
    if !v.is_null() {
        return v;
    }
    let complete = ast_type_struct_complete(t, &*state_getext(s)).unwrap();
    v = value_struct_create(complete);
    object_assign(&mut *obj, v);
    v
}

pub unsafe fn object_getmembertype(
    obj: *mut Object,
    t: &AstType,
    member: *mut libc::c_char,
    s: *mut State,
) -> Option<&AstType> {
    value_struct_membertype(&*getorcreatestruct(obj, t, s), member)
}

pub fn range_create(size: Box<AstExpr>, loc: Box<Location>) -> Box<Range> {
    Box::new(Range { size, loc })
}

pub fn range_str(r: &Range) -> OwningCStr {
    let mut b = strbuilder_create();
    strbuilder_write!(b, "virt:{}@{}", r.size, location_str(&r.loc));
    strbuilder_build(b)
}

pub fn range_size(r: &Range) -> &AstExpr {
    &r.size
}

pub unsafe fn range_dealloc(r: &Range, s: *mut State) -> Result<()> {
    // FIXME - this is clearly insane, take Range by value
    state_dealloc(
        s,
        value_ptr_create(Box::from_raw(&*r.loc as *const Location as *mut Location)),
    )
}

pub unsafe fn range_isdeallocand(r: &Range, s: *mut State) -> bool {
    state_isdeallocand(s, &r.loc)
}

pub unsafe fn range_references(r: &Range, loc: &Location, s: *mut State) -> bool {
    location_references(&r.loc, loc, s)
}

pub unsafe fn object_arr_index(
    arr: &[*mut Object],
    offset: &AstExpr,
    state: &State,
) -> Option<usize> {
    for (i, &obj) in arr.iter().enumerate() {
        if object_contains(&*obj, offset, state) {
            return Some(i);
        }
    }
    None
}

pub unsafe fn object_arr_index_upperincl(
    arr: &[*mut Object],
    offset: &AstExpr,
    state: &State,
) -> Option<usize> {
    for (i, &obj) in arr.iter().enumerate() {
        if object_contains_upperincl(obj, offset, state) {
            return Some(i);
        }
    }
    None
}
