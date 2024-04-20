#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use libc::{free, strcmp};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_equal,
    ast_expr_identifier_create, ast_expr_literal_create, ast_expr_member_create, ast_expr_str,
    ast_type_create_voidptr, ast_type_struct_complete, ast_type_struct_members,
    ast_variable_arr_copy, ast_variable_arr_destroy, ast_variable_arr_from_slice,
    ast_variable_arr_n, ast_variable_arr_v, ast_variable_name, ast_variable_type,
};
use crate::object::{
    object_abstractcopy, object_as_value, object_assign, object_copy, object_destroy,
    object_value_create,
};
use crate::state::location::{
    location_copy, location_destroy, location_references, location_referencesheap, location_str,
    location_transfigure,
};
use crate::state::state::{state_getext, state_vconst};
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, strbuilder_putc, Map};
use crate::{
    cstr, strbuilder_write, AstExpr, AstType, AstVariable, AstVariableArr, Location, Object, State,
    StrBuilder,
};

pub struct Value {
    pub kind: ValueKind,
}

pub enum ValueKind {
    Sync(*mut Number),
    IndefinitePtr(*mut Number),
    DefinitePtr(*mut Location),
    Int(*mut Number),
    Literal(*mut libc::c_char),
    Struct(StructValue),
}

#[derive(Copy, Clone)]
pub struct StructValue {
    pub members: *mut AstVariableArr,
    pub m: *mut Map,
}

#[derive(Clone)]
pub struct Number {
    pub kind: NumberKind,
}

#[derive(Clone)]
pub enum NumberKind {
    Ranges(Vec<NumberRange>),
    Computed(*mut AstExpr),
}

#[derive(Clone)]
pub struct NumberRange {
    pub lower: *mut NumberValue,
    pub upper: *mut NumberValue,
}

#[derive(Clone)]
pub enum NumberValue {
    Constant(libc::c_int),
    Limit(bool),
}

fn value_create(kind: ValueKind) -> *mut Value {
    Box::into_raw(Box::new(Value { kind }))
}

pub unsafe fn value_ptr_create(loc: *mut Location) -> *mut Value {
    value_create(ValueKind::DefinitePtr(loc))
}

pub unsafe fn value_ptr_indefinite_create() -> *mut Value {
    value_create(ValueKind::IndefinitePtr(number_indefinite_create()))
}

pub unsafe fn value_int_create(val: libc::c_int) -> *mut Value {
    value_create(ValueKind::Int(number_single_create(val)))
}

pub unsafe fn value_literal_create(lit: *mut libc::c_char) -> *mut Value {
    value_create(ValueKind::Literal(dynamic_str(lit)))
}

pub unsafe fn value_int_ne_create(not_val: libc::c_int) -> *mut Value {
    value_create(ValueKind::Int(number_ne_create(not_val)))
}

pub unsafe fn value_int_range_create(lw: libc::c_int, excl_up: libc::c_int) -> *mut Value {
    value_create(ValueKind::Int(number_with_range_create(lw, excl_up)))
}

pub unsafe fn value_int_indefinite_create() -> *mut Value {
    value_create(ValueKind::Int(number_indefinite_create()))
}

pub unsafe fn value_sync_create(e: *mut AstExpr) -> *mut Value {
    value_create(ValueKind::Sync(number_computed_create(e)))
}

pub unsafe fn value_struct_create(t: *mut AstType) -> *mut Value {
    let members = ast_variable_arr_from_slice(
        ast_type_struct_members(&*t).expect("can't create value of incomplete type"),
    );
    value_create(ValueKind::Struct(StructValue {
        members,
        m: Box::into_raw(frommembers(members)),
    }))
}

pub unsafe fn value_struct_indefinite_create(
    mut t: *mut AstType,
    s: *mut State,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut Value {
    t = ast_type_struct_complete(t, state_getext(s));
    if (ast_type_struct_members(&*t)).is_none() {
        panic!();
    }
    let v: *mut Value = value_struct_create(t);
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };
    let n: libc::c_int = ast_variable_arr_n(sv.members);
    let var: *mut *mut AstVariable = ast_variable_arr_v(sv.members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let field: *mut libc::c_char = ast_variable_name(*var.offset(i as isize));
        let obj: *mut Object = (*sv.m).get(field) as *mut Object;
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_write!(b, "{}.{}", cstr!(comment), cstr!(field));
        object_assign(
            obj,
            state_vconst(
                s,
                ast_variable_type(*var.offset(i as isize)),
                strbuilder_build(b),
                persist,
            ),
        );
        i += 1;
    }
    return v;
}

unsafe fn ptr_referencesheap(v: &Value, s: *mut State) -> bool {
    match &v.kind {
        ValueKind::DefinitePtr(loc) => location_referencesheap(*loc, s),
        ValueKind::IndefinitePtr(_) => false,
        _ => panic!(),
    }
}

pub unsafe fn value_transfigure(v: *mut Value, compare: *mut State, islval: bool) -> *mut Value {
    match &(*v).kind {
        ValueKind::Sync(_) | ValueKind::Literal(_) => {
            if islval {
                ptr::null_mut()
            } else {
                v
            }
        }
        ValueKind::Struct(_) => {
            panic!();
        }
        ValueKind::Int(_) => {
            if islval {
                ptr::null_mut()
            } else {
                state_vconst(compare, ast_type_create_voidptr(), ptr::null_mut(), false)
            }
        }
        ValueKind::DefinitePtr(loc) => location_transfigure(*loc, compare),
        ValueKind::IndefinitePtr(_) => panic!(),
    }
}

pub unsafe fn value_pf_augment(old: *mut Value, root: *mut AstExpr) -> *mut Value {
    if !value_isstruct(&*old) {
        panic!();
    }
    let v: *mut Value = value_copy(&*old);
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };
    let n: libc::c_int = ast_variable_arr_n(sv.members);
    let var: *mut *mut AstVariable = ast_variable_arr_v(sv.members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let field: *mut libc::c_char = ast_variable_name(*var.offset(i as isize));
        let obj: *mut Object = (*sv.m).get(field) as *mut Object;
        let obj_value: *mut Value = object_as_value(obj);
        if !obj_value.is_null() && value_issync(&*obj_value) {
            object_assign(
                obj,
                value_sync_create(Box::into_raw(ast_expr_member_create(
                    ast_expr_copy(&*root),
                    dynamic_str(field),
                ))),
            );
        }
        i += 1;
    }
    return v;
}

pub fn value_isstruct(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Struct(_))
}

unsafe fn frommembers(members: *mut AstVariableArr) -> Box<Map> {
    let mut m = Map::new();
    let n: libc::c_int = ast_variable_arr_n(members);
    let v: *mut *mut AstVariable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        m.set(
            dynamic_str(ast_variable_name(*v.offset(i as isize))),
            object_value_create(
                Box::into_raw(ast_expr_constant_create(0 as libc::c_int)),
                ptr::null_mut(),
            ) as *const libc::c_void,
        );
        i += 1;
    }
    return m;
}

unsafe fn destroymembers(m: &Map) {
    for p in m.values() {
        object_destroy(p as *mut Object);
    }
}

unsafe fn copymembers(old: &Map) -> Box<Map> {
    let mut new = Map::new();
    for (k, v) in old.pairs() {
        new.set(
            dynamic_str(k),
            object_copy(v as *mut Object) as *const libc::c_void,
        );
    }
    return new;
}

pub unsafe fn value_struct_abstractcopy(old: &StructValue, s: *mut State) -> *mut Value {
    value_create(ValueKind::Struct(StructValue {
        members: ast_variable_arr_copy(old.members),
        m: Box::into_raw(abstractcopymembers(&*old.m, s)),
    }))
}

unsafe fn abstractcopymembers(old: &Map, s: *mut State) -> Box<Map> {
    let mut new = Map::new();
    for (k, v) in old.pairs() {
        new.set(
            dynamic_str(k),
            object_abstractcopy(v as *mut Object, s) as *const libc::c_void,
        );
    }
    return new;
}

pub unsafe fn value_struct_membertype(v: *mut Value, member: *mut libc::c_char) -> *mut AstType {
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };
    let members: *mut AstVariableArr = sv.members;
    let n: libc::c_int = ast_variable_arr_n(members);
    let var: *mut *mut AstVariable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if strcmp(member, ast_variable_name(*var.offset(i as isize))) == 0 as libc::c_int {
            return ast_variable_type(*var.offset(i as isize));
        }
        i += 1;
    }
    ptr::null_mut()
}

pub unsafe fn value_struct_member(v: *mut Value, member: *mut libc::c_char) -> *mut Object {
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };
    return (*sv.m).get(member) as *mut Object;
}

unsafe fn struct_referencesheap(sv: &StructValue, s: *mut State) -> bool {
    let m: &Map = &*sv.m;
    for p in m.values() {
        let val: *mut Value = object_as_value(p as *mut Object);
        if !val.is_null() && value_referencesheap(&*val, s) {
            return true;
        }
    }
    false
}

pub unsafe fn value_copy(v: &Value) -> *mut Value {
    value_create(match &v.kind {
        ValueKind::Sync(n) => ValueKind::Sync(number_copy(*n)),
        ValueKind::DefinitePtr(loc) => ValueKind::DefinitePtr(location_copy(*loc)),
        ValueKind::IndefinitePtr(n) => ValueKind::IndefinitePtr(number_copy(*n)),
        ValueKind::Int(n) => ValueKind::Int(number_copy(*n)),
        ValueKind::Literal(s) => ValueKind::Literal(dynamic_str(*s)),
        ValueKind::Struct(struct_) => ValueKind::Struct(StructValue {
            members: ast_variable_arr_copy(struct_.members),
            m: Box::into_raw(copymembers(&*struct_.m)),
        }),
    })
}

pub unsafe fn value_abstractcopy(v: &Value, s: *mut State) -> *mut Value {
    if !value_referencesheap(v, s) {
        return ptr::null_mut();
    }
    match &v.kind {
        ValueKind::IndefinitePtr(_) | ValueKind::DefinitePtr(_) => value_copy(v),
        ValueKind::Struct(sv) => value_struct_abstractcopy(sv, s),
        _ => panic!(),
    }
}

impl Drop for ValueKind {
    fn drop(&mut self) {
        unsafe {
            match self {
                ValueKind::Sync(n) | ValueKind::IndefinitePtr(n) | ValueKind::Int(n) => {
                    number_destroy(*n)
                }
                ValueKind::DefinitePtr(loc) => location_destroy(*loc),
                ValueKind::Literal(s) => free(*s as *mut libc::c_void),
                ValueKind::Struct(struct_) => {
                    ast_variable_arr_destroy(struct_.members);
                    destroymembers(&*struct_.m);
                    Box::from_raw(struct_.m).destroy();
                }
            }
        }
    }
}

pub unsafe fn value_destroy(v: *mut Value) {
    drop(Box::from_raw(v));
}

pub unsafe fn value_str(v: *mut Value) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match &(*v).kind {
        ValueKind::Sync(n) => {
            value_sync_sprint(*n, b);
        }
        ValueKind::DefinitePtr(loc) => {
            value_definite_ptr_sprint(*loc, b);
        }
        ValueKind::IndefinitePtr(n) => {
            value_indefinite_ptr_sprint(*n, b);
        }
        ValueKind::Int(n) => {
            value_int_sprint(*n, b);
        }
        ValueKind::Literal(s) => {
            strbuilder_write!(b, "\"{}\"", cstr!(*s));
        }
        ValueKind::Struct(sv) => {
            value_struct_sprint(sv, b);
        }
    }
    return strbuilder_build(b);
}

pub unsafe fn value_sync_sprint(n: *mut Number, b: *mut StrBuilder) {
    strbuilder_write!(b, "comp:{}", cstr!(number_str(n)));
}

pub unsafe fn value_definite_ptr_sprint(loc: *mut Location, b: *mut StrBuilder) {
    let s = location_str(loc);
    strbuilder_write!(b, "ptr:{}", cstr!(s));
    free(s as *mut libc::c_void);
}

pub unsafe fn value_indefinite_ptr_sprint(n: *mut Number, b: *mut StrBuilder) {
    let s = number_str(n);
    strbuilder_write!(b, "ptr:{}", cstr!(s));
    free(s as *mut libc::c_void);
}

pub unsafe fn value_int_sprint(n: *mut Number, b: *mut StrBuilder) {
    strbuilder_write!(b, "int:{}", cstr!(number_str(n)));
}

pub unsafe fn value_struct_sprint(sv: &StructValue, b: *mut StrBuilder) {
    strbuilder_write!(b, "struct:{{");
    let members: *mut AstVariableArr = sv.members;
    let n: libc::c_int = ast_variable_arr_n(members);
    let var: *mut *mut AstVariable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let f: *mut libc::c_char = ast_variable_name(*var.offset(i as isize));
        let val: *mut Value = object_as_value((*sv.m).get(f) as *mut Object);
        let val_str: *mut libc::c_char = if !val.is_null() {
            value_str(val)
        } else {
            dynamic_str(b"\0" as *const u8 as *const libc::c_char)
        };
        strbuilder_write!(
            b,
            ".{} = <{}>{}",
            cstr!(f),
            cstr!(val_str),
            if i + 1 < n { ", " } else { "" },
        );
        free(val_str as *mut libc::c_void);
        i += 1;
    }
    strbuilder_write!(b, "}}");
}

pub unsafe fn value_islocation(v: &Value) -> bool {
    matches!(v.kind, ValueKind::DefinitePtr(_))
}

pub unsafe fn value_as_location(v: &Value) -> *mut Location {
    let ValueKind::DefinitePtr(loc) = &v.kind else {
        panic!();
    };
    *loc
}

pub unsafe fn value_referencesheap(v: &Value, s: *mut State) -> bool {
    match &v.kind {
        ValueKind::DefinitePtr(_) | ValueKind::IndefinitePtr(_) => ptr_referencesheap(v, s),
        ValueKind::Struct(sv) => struct_referencesheap(sv, s),
        _ => false,
    }
}

pub unsafe fn value_as_constant(v: &Value) -> libc::c_int {
    let ValueKind::Int(n) = &v.kind else {
        panic!();
    };
    number_as_constant(*n)
}

pub unsafe fn value_isconstant(v: &Value) -> bool {
    match &v.kind {
        ValueKind::Int(n) => number_isconstant(*n),
        _ => false,
    }
}

pub unsafe fn value_issync(v: &Value) -> bool {
    match &v.kind {
        ValueKind::Sync(n) => number_issync(*n),
        _ => false,
    }
}

pub unsafe fn value_as_sync(v: *mut Value) -> *mut AstExpr {
    let ValueKind::Sync(n) = &(*v).kind else {
        panic!();
    };
    number_as_sync(*n)
}

pub unsafe fn value_isint(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Int(_))
}

pub unsafe fn value_to_expr(v: *mut Value) -> *mut AstExpr {
    match &(*v).kind {
        ValueKind::DefinitePtr(_) => Box::into_raw(ast_expr_identifier_create(value_str(v))),
        ValueKind::IndefinitePtr(_) => Box::into_raw(ast_expr_identifier_create(value_str(v))),
        ValueKind::Literal(_) => Box::into_raw(ast_expr_copy(&*value_as_literal(&*v))),
        ValueKind::Sync(n) => Box::into_raw(ast_expr_copy(&*number_as_sync(*n))),
        ValueKind::Int(n) => number_to_expr(*n),
        _ => panic!(),
    }
}

pub unsafe fn value_isliteral(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Literal(_))
}

pub unsafe fn value_as_literal(v: &Value) -> *mut AstExpr {
    let ValueKind::Literal(s) = &v.kind else {
        panic!();
    };
    Box::into_raw(ast_expr_literal_create(*s))
}

pub unsafe fn value_references(v: *mut Value, loc: *mut Location, s: *mut State) -> bool {
    match &(*v).kind {
        ValueKind::DefinitePtr(vloc) => location_references(*vloc, loc, s),
        ValueKind::Struct(sv) => struct_references(sv, loc, s),
        _ => false,
    }
}

unsafe fn struct_references(sv: &StructValue, loc: *mut Location, s: *mut State) -> bool {
    let m: &Map = &*sv.m;
    for p in m.values() {
        let val: *mut Value = object_as_value(p as *mut Object);
        if !val.is_null() && value_references(val, loc, s) {
            return true;
        }
    }
    false
}

pub unsafe fn value_equal(v1: &Value, v2: &Value) -> bool {
    match (&v1.kind, &v2.kind) {
        (ValueKind::Literal(s1), ValueKind::Literal(s2)) => strcmp(*s1, *s2) == 0,
        (ValueKind::Int(n1), ValueKind::Int(n2)) | (ValueKind::Sync(n1), ValueKind::Sync(n2)) => {
            number_equal(*n1, *n2)
        }
        _ => panic!(),
    }
}

pub unsafe fn value_assume(v: *mut Value, value: bool) -> bool {
    match &(*v).kind {
        ValueKind::Int(n) => number_assume(*n, value),
        ValueKind::IndefinitePtr(n) => number_assume(*n, value),
        _ => panic!(),
    }
}

unsafe fn number_create(kind: NumberKind) -> *mut Number {
    Box::into_raw(Box::new(Number { kind }))
}

unsafe fn number_ranges_create(ranges: Vec<NumberRange>) -> *mut Number {
    number_create(NumberKind::Ranges(ranges))
}

unsafe fn number_single_create(val: libc::c_int) -> *mut Number {
    number_create(NumberKind::Ranges(number_range_arr_single_create(val)))
}

unsafe fn number_range_arr_single_create(val: libc::c_int) -> Vec<NumberRange> {
    vec![number_range_create(
        number_value_constant_create(val),
        number_value_constant_create(val + 1 as libc::c_int),
    )]
}

unsafe fn number_computed_create(e: *mut AstExpr) -> *mut Number {
    number_create(NumberKind::Computed(e))
}

unsafe fn number_range_arr_ne_create(val: libc::c_int) -> Vec<NumberRange> {
    vec![
        number_range_create(number_value_min_create(), number_value_constant_create(val)),
        number_range_create(
            number_value_constant_create(val + 1 as libc::c_int),
            number_value_max_create(),
        ),
    ]
}

unsafe fn number_ne_create(val: libc::c_int) -> *mut Number {
    number_ranges_create(number_range_arr_ne_create(val))
}

unsafe fn number_with_range_create(lw: libc::c_int, excl_up: libc::c_int) -> *mut Number {
    number_ranges_create(vec![number_range_create(
        number_value_constant_create(lw),
        number_value_constant_create(excl_up),
    )])
}

unsafe fn number_indefinite_create() -> *mut Number {
    number_ranges_create(vec![number_range_create(
        number_value_min_create(),
        number_value_max_create(),
    )])
}

unsafe fn number_range_lw(n: *mut Number) -> libc::c_int {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    assert_eq!(ranges.len(), 1);
    number_value_as_constant(ranges[0].lower)
}

unsafe fn number_range_up(n: *mut Number) -> libc::c_int {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    assert_eq!(ranges.len(), 1);
    number_value_as_constant(ranges[0].upper)
}

unsafe fn number_destroy(n: *mut Number) {
    match &(*n).kind {
        NumberKind::Ranges(ranges) => {
            number_range_arr_destroy(ranges);
        }
        NumberKind::Computed(computation) => {
            ast_expr_destroy(*computation);
        }
    }
}

unsafe fn number_ranges_sprint(ranges: &[NumberRange]) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_putc(b, '{' as i32 as libc::c_char);
    let n = ranges.len();
    for (i, range) in ranges.iter().enumerate() {
        let r = number_range_str(range);
        strbuilder_write!(b, "{}{}", cstr!(r), if i + 1 < n { ", " } else { "" });
        free(r as *mut libc::c_void);
    }
    strbuilder_putc(b, '}' as i32 as libc::c_char);
    return strbuilder_build(b);
}

unsafe fn number_str(num: *mut Number) -> *mut libc::c_char {
    match &(*num).kind {
        NumberKind::Ranges(ranges) => number_ranges_sprint(ranges),
        NumberKind::Computed(computation) => ast_expr_str(&**computation),
    }
}

unsafe fn number_equal(n1: *mut Number, n2: *mut Number) -> bool {
    match (&(*n1).kind, &(*n2).kind) {
        (NumberKind::Ranges(ranges1), NumberKind::Ranges(ranges2)) => {
            number_ranges_equal(ranges1, ranges2)
        }
        (NumberKind::Computed(c1), NumberKind::Computed(c2)) => ast_expr_equal(&**c1, &**c2),
        _ => panic!(),
    }
}

unsafe fn number_ranges_equal(n1: &[NumberRange], n2: &[NumberRange]) -> bool {
    let len = n1.len();
    if len != n2.len() {
        return false;
    }
    n1.iter()
        .zip(n2)
        .all(|(nr1, nr2)| number_range_equal(nr1, nr2))
}

unsafe fn number_assume(n: *mut Number, value: bool) -> bool {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    if !number_range_arr_canbe(ranges, value) {
        return false;
    }
    *ranges = number_range_assumed_value(value);
    return true;
}

unsafe fn number_range_assumed_value(value: bool) -> Vec<NumberRange> {
    if value {
        number_range_arr_ne_create(0)
    } else {
        number_range_arr_single_create(0)
    }
}

unsafe fn number_isconstant(n: *mut Number) -> bool {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    ranges.len() == 1 && number_range_issingle(&ranges[0])
}

unsafe fn number_as_constant(n: *mut Number) -> libc::c_int {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    assert_eq!(ranges.len(), 1);
    number_range_as_constant(&ranges[0])
}

unsafe fn number_issync(n: *mut Number) -> bool {
    matches!((*n).kind, NumberKind::Computed(_))
}

unsafe fn number_as_sync(n: *mut Number) -> *mut AstExpr {
    let NumberKind::Computed(computation) = &mut (*n).kind else {
        panic!();
    };
    *computation
}

unsafe fn number_to_expr(n: *mut Number) -> *mut AstExpr {
    match &(*n).kind {
        NumberKind::Ranges(ranges) => number_ranges_to_expr(ranges),
        NumberKind::Computed(computation) => Box::into_raw(ast_expr_copy(&**computation)),
    }
}

unsafe fn number_copy(num: *mut Number) -> *mut Number {
    match &(*num).kind {
        NumberKind::Ranges(ranges) => number_ranges_create(number_range_arr_copy(ranges)),
        NumberKind::Computed(computation) => {
            number_computed_create(Box::into_raw(ast_expr_copy(&**computation)))
        }
    }
}

unsafe fn number_range_arr_destroy(arr: &[NumberRange]) {
    for range in arr {
        number_range_destroy(range);
    }
}

unsafe fn number_range_arr_copy(old: &[NumberRange]) -> Vec<NumberRange> {
    old.iter().map(|range| number_range_copy(range)).collect()
}

unsafe fn number_ranges_to_expr(arr: &[NumberRange]) -> *mut AstExpr {
    assert_eq!(arr.len(), 1);
    Box::into_raw(ast_expr_constant_create(number_range_as_constant(&arr[0])))
}

unsafe fn number_range_arr_canbe(arr: &[NumberRange], value: bool) -> bool {
    arr.iter().any(|range| number_range_canbe(range, value))
}

unsafe fn number_range_create(lw: *mut NumberValue, up: *mut NumberValue) -> NumberRange {
    NumberRange {
        lower: lw,
        upper: up,
    }
}

unsafe fn number_range_destroy(r: &NumberRange) {
    number_value_destroy(r.lower);
    number_value_destroy(r.upper);
}

unsafe fn number_range_lower(r: *mut NumberRange) -> *mut NumberValue {
    return (*r).lower;
}

unsafe fn number_range_upper(r: *mut NumberRange) -> *mut NumberValue {
    return (*r).upper;
}

unsafe fn number_range_str(r: &NumberRange) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    if number_range_issingle(r) {
        strbuilder_write!(b, "{}", cstr!(number_value_str(r.lower)));
    } else {
        strbuilder_write!(
            b,
            "{}:{}",
            cstr!(number_value_str(r.lower)),
            cstr!(number_value_str(r.upper)),
        );
    }
    return strbuilder_build(b);
}

unsafe fn number_range_copy(r: &NumberRange) -> NumberRange {
    return number_range_create(number_value_copy(r.lower), number_value_copy(r.upper));
}

unsafe fn number_range_canbe(r: &NumberRange, value: bool) -> bool {
    if value {
        if number_value_equal(r.lower, r.upper) {
            return false;
        }
        number_value_le_constant(r.lower, -1) || constant_le_number_value(1, r.lower)
    } else {
        number_value_le_constant(r.lower, 0) && constant_le_number_value(1, r.upper)
    }
}

unsafe fn number_range_issingle(r: &NumberRange) -> bool {
    return number_values_aresingle(r.lower, r.upper);
}

unsafe fn number_range_equal(r1: &NumberRange, r2: &NumberRange) -> bool {
    return number_value_equal(r1.lower, r2.lower) as libc::c_int != 0
        && number_value_equal(r1.upper, r2.upper) as libc::c_int != 0;
}

unsafe fn number_range_as_constant(r: &NumberRange) -> libc::c_int {
    if !number_range_issingle(r) {
        panic!();
    }
    number_value_as_constant(r.lower)
}

unsafe fn number_value_constant_create(constant: libc::c_int) -> *mut NumberValue {
    Box::into_raw(Box::new(NumberValue::Constant(constant)))
}

unsafe fn number_value_limit_create(max: bool) -> *mut NumberValue {
    Box::into_raw(Box::new(NumberValue::Limit(max)))
}

unsafe fn number_value_min_create() -> *mut NumberValue {
    number_value_limit_create(false)
}

unsafe fn number_value_max_create() -> *mut NumberValue {
    number_value_limit_create(true)
}

unsafe fn number_value_destroy(v: *mut NumberValue) {
    drop(Box::from_raw(v));
}

unsafe fn number_value_str(v: *mut NumberValue) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match &*v {
        NumberValue::Constant(k) => {
            strbuilder_write!(b, "{k}");
        }
        NumberValue::Limit(true) => {
            strbuilder_write!(b, "MAX");
        }
        NumberValue::Limit(false) => {
            strbuilder_write!(b, "MIN");
        }
    }
    return strbuilder_build(b);
}

unsafe fn number_value_copy(v: *mut NumberValue) -> *mut NumberValue {
    match &*v {
        NumberValue::Constant(k) => number_value_constant_create(*k),
        NumberValue::Limit(max) => number_value_limit_create(*max),
    }
}

unsafe fn number_values_aresingle(v1: *mut NumberValue, v2: *mut NumberValue) -> bool {
    match (&*v1, &*v2) {
        (NumberValue::Constant(k1), NumberValue::Constant(k2)) => *k1 == *k2 - 1,
        (NumberValue::Limit(max1), NumberValue::Limit(max2)) => *max1 == *max2,
        _ => panic!(),
    }
}

unsafe fn number_value_difference(v1: *mut NumberValue, v2: *mut NumberValue) -> libc::c_int {
    match (&*v1, &*v2) {
        (NumberValue::Constant(v1), NumberValue::Constant(v2)) => *v1 - *v2,
        _ => panic!(),
    }
}

unsafe fn number_value_equal(v1: *mut NumberValue, v2: *mut NumberValue) -> bool {
    match (&*v1, &*v2) {
        (NumberValue::Constant(k1), NumberValue::Constant(k2)) => *k1 == *k2,
        (NumberValue::Limit(max1), NumberValue::Limit(max2)) => *max1 == *max2,
        _ => panic!(),
    }
}

unsafe fn number_value_as_constant(v: *mut NumberValue) -> libc::c_int {
    match &*v {
        NumberValue::Constant(k) => *k,
        _ => panic!(),
    }
}

unsafe fn number_value_le_constant(v: *mut NumberValue, constant: libc::c_int) -> bool {
    match &*v {
        NumberValue::Constant(k) => *k <= constant,
        NumberValue::Limit(max) => !*max,
    }
}

unsafe fn constant_le_number_value(constant: libc::c_int, v: *mut NumberValue) -> bool {
    match &*v {
        NumberValue::Constant(k) => constant <= *k,
        NumberValue::Limit(max) => *max,
    }
}
