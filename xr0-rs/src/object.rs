use std::fmt::{self, Display, Formatter};

use crate::ast::{
    ast_expr_copy, ast_type_struct_complete, ast_variable_name, ast_variable_type, LValue,
};
use crate::state::location::{location_dealloc, location_references};
use crate::state::state::state_eval;
use crate::state::State;
use crate::util::Result;
use crate::value::{
    value_abstractcopy, value_as_location, value_copy, value_references, value_referencesheap,
    value_struct_create, ValueKind,
};
use crate::{AstExpr, AstType, Location, Value};

/// A span of memory within a block. This could be the whole block, a field of a struct, an element
/// of an array, etc.
///
/// An object is either "value" or "range". If it's "value" then it either has a Value, or it's
/// uninitialized.
#[derive(Clone)]
pub struct Object {
    pub kind: ObjectKind,
    /// Expression for the offset of this object within the enclosing block.
    pub offset: Box<AstExpr>,
}

#[derive(Clone)]
pub enum ObjectKind {
    DeallocandRange(Box<Range>),
    Value(Option<Box<Value>>),
}

#[derive(Clone)]
pub struct Range {
    size: Box<AstExpr>,
    loc: Box<Location>,
}

pub fn object_value_create(offset: Box<AstExpr>, v: Option<Box<Value>>) -> Box<Object> {
    Box::new(Object {
        kind: ObjectKind::Value(v),
        offset,
    })
}

pub fn object_range_create(offset: Box<AstExpr>, r: Box<Range>) -> Box<Object> {
    Box::new(Object {
        kind: ObjectKind::DeallocandRange(r),
        offset,
    })
}

pub fn object_copy(old: &Object) -> Box<Object> {
    Box::new(old.clone())
}

pub fn object_abstractcopy(old: &Object, s: &mut State) -> Box<Object> {
    match &old.kind {
        ObjectKind::DeallocandRange(_) => object_copy(old),
        ObjectKind::Value(v) => object_value_create(
            old.offset.clone(),
            v.as_ref().and_then(|v| value_abstractcopy(v, s)),
        ),
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Object { kind, offset } = self;
        write!(f, "{{{offset}:<{kind}>}}")
    }
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ObjectKind::Value(Some(v)) => write!(f, "{v}"),
            ObjectKind::Value(None) => Ok(()),
            ObjectKind::DeallocandRange(range) => write!(f, "{range}"),
        }
    }
}

pub fn object_referencesheap(obj: &Object, s: &mut State) -> bool {
    match &obj.kind {
        ObjectKind::Value(Some(v)) => value_referencesheap(v, s),
        ObjectKind::Value(None) => false,
        ObjectKind::DeallocandRange(_) => true,
    }
}

pub fn object_hasvalue(obj: &Object) -> bool {
    matches!(obj.kind, ObjectKind::Value(Some(_)))
}

pub fn object_isvalue(obj: &Object) -> bool {
    matches!(obj.kind, ObjectKind::Value(_))
}

pub fn object_as_value(obj: &Object) -> Option<&Value> {
    let ObjectKind::Value(v) = &obj.kind else {
        panic!();
    };
    v.as_deref()
}

pub fn object_as_value_mut(obj: &mut Object) -> Option<&mut Value> {
    let ObjectKind::Value(v) = &mut obj.kind else {
        panic!();
    };
    v.as_deref_mut()
}

pub fn object_isdeallocand(obj: &Object, s: &mut State) -> bool {
    match &obj.kind {
        ObjectKind::Value(None) => false,
        ObjectKind::Value(Some(v)) => s.loc_is_deallocand(value_as_location(v)),
        ObjectKind::DeallocandRange(range) => range_isdeallocand(range, s),
    }
}

pub fn object_references(obj: &Object, loc: &Location, s: &mut State) -> bool {
    match &obj.kind {
        ObjectKind::DeallocandRange(range) => range_references(range, loc, s),
        ObjectKind::Value(None) => false,
        ObjectKind::Value(Some(v)) => value_references(v, loc, s),
    }
}

impl Object {
    pub fn assign(&mut self, val: Option<Box<Value>>) {
        let ObjectKind::Value(v) = &mut self.kind else {
            panic!();
        };
        *v = val;
    }
}

fn object_size(obj: &Object) -> Box<AstExpr> {
    match &obj.kind {
        ObjectKind::Value(_) => AstExpr::new_constant(1),
        ObjectKind::DeallocandRange(range) => ast_expr_copy(range_size(range)),
    }
}

#[allow(dead_code)]
pub fn object_lower(obj: &Object) -> &AstExpr {
    &obj.offset
}

pub fn object_upper(obj: &Object) -> Box<AstExpr> {
    AstExpr::new_sum(ast_expr_copy(&obj.offset), object_size(obj))
}

pub fn object_contains(obj: &Object, offset: &AstExpr, s: &State) -> bool {
    let lw = &obj.offset;
    let up = object_upper(obj);
    let of = offset;
    let e1 = AstExpr::new_le(ast_expr_copy(lw), ast_expr_copy(of));
    let e2 = AstExpr::new_lt(ast_expr_copy(of), ast_expr_copy(&up));
    state_eval(s, &e1) && state_eval(s, &e2)
}

pub fn object_contains_upperincl(obj: &Object, offset: &AstExpr, s: &State) -> bool {
    let lw = &obj.offset;
    let up = object_upper(obj);
    let of = offset;
    // Note: These copies are not in the original. Original leaks the expressions to avoid
    // double-freeing subexpressions.
    let lower_bound_expr = AstExpr::new_le(ast_expr_copy(lw), ast_expr_copy(of));
    let upper_bound_expr = AstExpr::new_le(ast_expr_copy(of), up);
    state_eval(s, &lower_bound_expr) && state_eval(s, &upper_bound_expr)
}

#[allow(dead_code)]
pub fn object_isempty(obj: &Object, s: &State) -> bool {
    let lw = &obj.offset;
    let up = object_upper(obj);
    // Note: Original does not make a copy of `lw`; instead it leaks the expression to avoid
    // double-freeing subexpressions.
    state_eval(s, &AstExpr::new_eq(ast_expr_copy(lw), up))
}

pub fn object_contig_precedes(before: &Object, after: &Object, s: &State) -> bool {
    let lw = object_upper(before);
    let up = &after.offset;
    // Note: Original does not make a copy of `up`; instead it leaks the expression to avoid
    // double-freeing subexpressions.
    state_eval(s, &AstExpr::new_eq(lw, ast_expr_copy(up)))
}

#[allow(dead_code)]
pub fn object_issingular(obj: &Object, s: &State) -> bool {
    let lw = &obj.offset;
    let up = object_upper(obj);
    // Note: Original does not make a copy of `lw`; instead it leaks the expression to avoid
    // double-freeing subexpressions.
    let lw_succ = AstExpr::new_sum(ast_expr_copy(lw), AstExpr::new_constant(1));
    state_eval(s, &AstExpr::new_eq(lw_succ, up))
}

/// Returns an `Object` covering the slice of `obj` up to the offset (within the enclosing Block)
/// given by `excl_up`; or `None` if `the slice would be empty.
///
/// # Panics
///
/// If `excl_up` can't be proved to be `>=` the start offset of `obj`.
pub fn object_upto(obj: &Object, excl_up: &AstExpr, s: &mut State) -> Option<Box<Object>> {
    let lw = &obj.offset;
    let up = object_upper(obj);
    let prop0 = AstExpr::new_le(ast_expr_copy(lw), ast_expr_copy(excl_up));
    let prop1 = AstExpr::new_eq(ast_expr_copy(lw), ast_expr_copy(excl_up));
    let prop2 = AstExpr::new_eq(up, ast_expr_copy(excl_up));
    let e0: bool = state_eval(s, &prop0);
    let e1: bool = state_eval(s, &prop1);
    let e2: bool = state_eval(s, &prop2);
    drop(prop2);
    drop(prop1);
    drop(prop0);
    assert!(e0, "excl_up must be decidably >= the start offset of obj");
    if e1 {
        // `excl_up` is equal to this object's lower bound. Nothing to return.
        return None;
    }
    if e2 {
        // `excl_up` is equal to this object's upper bound. Return everything.
        // I'm not sure why this doesn't return `Some(obj.clone())`.
        // Note: Original doesn't null-check the value here; objects can be VALUE with null value.
        let ObjectKind::Value(Some(v)) = &obj.kind else {
            panic!();
        };
        return Some(object_value_create(
            ast_expr_copy(&obj.offset),
            Some(value_copy(v)),
        ));
    }

    // `excl_up` is not decidably equal to the upper or lower bound of `obj`. In fact, we did not
    // insist on `excl_up <= up`, so `excl_up` could be past that end.

    // Note: I think there's a double free in the original, where it creates an expression using
    // `lw` without copying it, but `obj` owns that expr.
    Some(object_range_create(
        ast_expr_copy(&obj.offset),
        range_create(
            AstExpr::new_difference(ast_expr_copy(excl_up), ast_expr_copy(lw)),
            s.alloc().into_location(),
        ),
    ))
}

pub fn object_from(obj: &Object, incl_lw: &AstExpr, s: &mut State) -> Option<Box<Object>> {
    let lw = &obj.offset;
    let up = object_upper(obj);
    let prop0 = AstExpr::new_ge(ast_expr_copy(incl_lw), ast_expr_copy(&up));
    let prop1 = AstExpr::new_eq(ast_expr_copy(incl_lw), ast_expr_copy(lw));
    let e0: bool = state_eval(s, &prop0);
    let e1: bool = state_eval(s, &prop1);
    drop(prop1);
    drop(prop0);
    if e0 {
        return None;
    }
    if e1 {
        // Note: Original doesn't null-check the value here; objects can be VALUE with null value.
        let ObjectKind::Value(Some(v)) = &obj.kind else {
            panic!();
        };
        return Some(object_value_create(
            ast_expr_copy(incl_lw),
            Some(value_copy(v)),
        ));
    }
    Some(object_range_create(
        ast_expr_copy(incl_lw),
        range_create(
            AstExpr::new_difference(up, ast_expr_copy(incl_lw)),
            s.alloc().into_location(),
        ),
    ))
}

pub fn object_dealloc(obj: &Object, s: &mut State) -> Result<()> {
    // Note: Original doesn't handle the possibility of Value(None) here.
    match &obj.kind {
        ObjectKind::Value(Some(v)) => (*s).dealloc(v),
        ObjectKind::Value(None) => panic!(),
        ObjectKind::DeallocandRange(range) => range_dealloc(range, s),
    }
}

pub fn object_member_lvalue<'s>(
    obj: &'s mut Object,
    t: &AstType,
    member: &str,
    s: &'s mut State,
) -> LValue<'s> {
    let val = getorcreatestruct(obj, t, s);
    let ValueKind::Struct(sv) = &mut val.kind else {
        panic!();
    };

    let obj = sv.m.get_mut(member).map(|boxed| &mut **boxed);
    let t = sv
        .members
        .iter()
        .find(|var| member == ast_variable_name(var))
        .map(|var| ast_variable_type(var));
    LValue { t, obj }
}

fn getorcreatestruct<'obj>(obj: &'obj mut Object, t: &AstType, s: &mut State) -> &'obj mut Value {
    // XXX FIXME: very silly rust construction because of borrow checker limitation
    if object_as_value_mut(obj).is_some() {
        object_as_value_mut(obj).unwrap()
    } else {
        let complete = ast_type_struct_complete(t, s.ext()).unwrap();
        obj.assign(Some(value_struct_create(complete)));
        object_as_value_mut(obj).unwrap()
    }
}

pub fn range_create(size: Box<AstExpr>, loc: Box<Location>) -> Box<Range> {
    Box::new(Range { size, loc })
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Range { size, loc } = self;
        write!(f, "virt:{size}@{loc}")
    }
}

pub fn range_size(r: &Range) -> &AstExpr {
    &r.size
}

pub fn range_dealloc(r: &Range, s: &mut State) -> Result<()> {
    // Note: The original creates a value that borrows the location from `r`, then leaks the value
    // to avoid double-freeing the location.
    location_dealloc(&r.loc, &mut s.heap)
}

pub fn range_isdeallocand(r: &Range, s: &mut State) -> bool {
    s.loc_is_deallocand(&r.loc)
}

pub fn range_references(r: &Range, loc: &Location, s: &mut State) -> bool {
    location_references(&r.loc, loc, s)
}

pub fn object_arr_index(arr: &[Box<Object>], offset: &AstExpr, state: &State) -> Option<usize> {
    for (i, obj) in arr.iter().enumerate() {
        if object_contains(obj, offset, state) {
            return Some(i);
        }
    }
    None
}

pub fn object_arr_index_upperincl(
    arr: &[Box<Object>],
    offset: &AstExpr,
    state: &State,
) -> Option<usize> {
    for (i, obj) in arr.iter().enumerate() {
        if object_contains_upperincl(obj, offset, state) {
            return Some(i);
        }
    }
    None
}
