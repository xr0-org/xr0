use std::collections::HashMap;
use std::ffi::CStr;
use std::ptr;

use libc::strcmp;

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_equal,
    ast_expr_identifier_create, ast_expr_literal_create, ast_expr_member_create, ast_expr_str,
    ast_type_create_voidptr, ast_type_struct_complete, ast_type_struct_members,
    ast_variable_arr_copy, ast_variable_name, ast_variable_type,
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
use crate::util::{strbuilder_build, strbuilder_create, strbuilder_putc, OwningCStr};
use crate::{
    cstr, strbuilder_write, AstExpr, AstType, AstVariable, Location, Object, State, StrBuilder,
};

pub struct Value {
    pub kind: ValueKind,
}

pub enum ValueKind {
    Sync(Box<Number>),
    IndefinitePtr(*mut Number),
    DefinitePtr(*mut Location),
    Int(*mut Number),
    Literal(OwningCStr),
    Struct(Box<StructValue>),
}

pub struct StructValue {
    pub members: Vec<Box<AstVariable>>,
    pub m: HashMap<String, *mut Object>,
}

#[derive(Clone)]
pub struct Number {
    pub kind: NumberKind,
}

pub enum NumberKind {
    Ranges(Vec<NumberRange>),
    Computed(*mut AstExpr),
}

#[derive(Copy, Clone)]
pub struct NumberRange {
    pub lower: NumberValue,
    pub upper: NumberValue,
}

#[derive(Copy, Clone)]
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
    value_create(ValueKind::Literal(OwningCStr::copy_char_ptr(lit)))
}

#[allow(dead_code)]
pub unsafe fn value_int_ne_create(not_val: libc::c_int) -> *mut Value {
    value_create(ValueKind::Int(number_ne_create(not_val)))
}

#[allow(dead_code)]
pub unsafe fn value_int_range_create(lw: libc::c_int, excl_up: libc::c_int) -> *mut Value {
    value_create(ValueKind::Int(number_with_range_create(lw, excl_up)))
}

pub unsafe fn value_int_indefinite_create() -> *mut Value {
    value_create(ValueKind::Int(number_indefinite_create()))
}

pub unsafe fn value_sync_create(e: *mut AstExpr) -> *mut Value {
    value_create(ValueKind::Sync(Box::from_raw(number_computed_create(e))))
}

pub unsafe fn value_struct_create(t: &AstType) -> *mut Value {
    let members = ast_type_struct_members(t).expect("can't create value of incomplete type");
    value_create(ValueKind::Struct(Box::new(StructValue {
        members: ast_variable_arr_copy(members),
        m: from_members(members),
    })))
}

pub unsafe fn value_struct_indefinite_create(
    mut t: &AstType,
    s: *mut State,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut Value {
    // Note: The original doesn't null-check here. I wonder how it would handle `typedef struct foo
    // foo;`.
    t = ast_type_struct_complete(t, &*state_getext(s)).unwrap();
    if (ast_type_struct_members(t)).is_none() {
        panic!();
    }
    let v: *mut Value = value_struct_create(t);
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };
    for var in &sv.members {
        let field: *mut libc::c_char = ast_variable_name(var);
        let field = CStr::from_ptr(field).to_str().unwrap();

        let obj: *mut Object = sv.m.get(field).copied().unwrap();
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_write!(b, "{}.{field}", cstr!(comment));
        object_assign(
            obj,
            state_vconst(
                s,
                ast_variable_type(var),
                strbuilder_build(b).into_ptr(),
                persist,
            ),
        );
    }
    v
}

unsafe fn ptr_referencesheap(v: &Value, s: *mut State) -> bool {
    match &v.kind {
        ValueKind::DefinitePtr(loc) => location_referencesheap(&**loc, s),
        ValueKind::IndefinitePtr(_) => false,
        _ => panic!(),
    }
}

#[allow(dead_code)]
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
                // Note: Original leaked this type.
                state_vconst(compare, &ast_type_create_voidptr(), ptr::null_mut(), false)
            }
        }
        ValueKind::DefinitePtr(loc) => location_transfigure(&**loc, compare),
        ValueKind::IndefinitePtr(_) => panic!(),
    }
}

pub unsafe fn value_pf_augment(old: *mut Value, root: &AstExpr) -> *mut Value {
    if !value_isstruct(&*old) {
        panic!();
    }
    let v: *mut Value = value_copy(&*old);
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };

    for var in &sv.members {
        let field: *mut libc::c_char = ast_variable_name(var);
        let field_str = CStr::from_ptr(field).to_str().unwrap();
        let obj = *sv.m.get(field_str).unwrap();
        let obj_value: *mut Value = object_as_value(obj);
        if !obj_value.is_null() && value_issync(&*obj_value) {
            object_assign(
                obj,
                value_sync_create(Box::into_raw(ast_expr_member_create(
                    ast_expr_copy(root),
                    OwningCStr::copy(CStr::from_ptr(field)),
                ))),
            );
        }
    }
    v
}

pub fn value_isstruct(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Struct(_))
}

unsafe fn from_members(members: &[Box<AstVariable>]) -> HashMap<String, *mut Object> {
    let mut m = HashMap::new();
    for var in members {
        let id = ast_variable_name(var);
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        m.insert(
            id,
            object_value_create(
                Box::into_raw(ast_expr_constant_create(0 as libc::c_int)),
                ptr::null_mut(),
            ),
        );
    }
    m
}

unsafe fn copy_members(old: &HashMap<String, *mut Object>) -> HashMap<String, *mut Object> {
    old.iter()
        .map(|(k, &v)| (k.clone(), object_copy(v)))
        .collect()
}

pub unsafe fn value_struct_abstractcopy(old: &StructValue, s: *mut State) -> *mut Value {
    value_create(ValueKind::Struct(Box::new(StructValue {
        members: ast_variable_arr_copy(&old.members),
        m: abstract_copy_members(&old.m, s),
    })))
}

unsafe fn abstract_copy_members(
    old: &HashMap<String, *mut Object>,
    s: *mut State,
) -> HashMap<String, *mut Object> {
    old.iter()
        .map(|(k, &v)| (k.clone(), object_abstractcopy(v, s)))
        .collect()
}

pub unsafe fn value_struct_membertype(v: &Value, member: *mut libc::c_char) -> Option<&AstType> {
    let ValueKind::Struct(sv) = &v.kind else {
        panic!();
    };
    for var in &sv.members {
        if strcmp(member, ast_variable_name(var)) == 0 as libc::c_int {
            return Some(ast_variable_type(var));
        }
    }
    None
}

pub unsafe fn value_struct_member(v: *mut Value, member: *mut libc::c_char) -> *mut Object {
    let ValueKind::Struct(sv) = &(*v).kind else {
        panic!();
    };
    let member = CStr::from_ptr(member).to_str().unwrap();
    sv.m.get(member).copied().unwrap_or(ptr::null_mut())
}

unsafe fn struct_referencesheap(sv: &StructValue, s: *mut State) -> bool {
    for &p in sv.m.values() {
        let val: *mut Value = object_as_value(p);
        if !val.is_null() && value_referencesheap(&*val, s) {
            return true;
        }
    }
    false
}

pub unsafe fn value_copy(v: &Value) -> *mut Value {
    value_create(match &v.kind {
        ValueKind::Sync(n) => ValueKind::Sync(n.clone()),
        ValueKind::DefinitePtr(loc) => ValueKind::DefinitePtr(Box::into_raw(location_copy(&**loc))),
        ValueKind::IndefinitePtr(n) => ValueKind::IndefinitePtr(number_copy(*n)),
        ValueKind::Int(n) => ValueKind::Int(number_copy(*n)),
        ValueKind::Literal(s) => ValueKind::Literal(s.clone()),
        ValueKind::Struct(struct_) => ValueKind::Struct(Box::new(StructValue {
            members: ast_variable_arr_copy(&struct_.members),
            m: copy_members(&struct_.m),
        })),
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
                ValueKind::Sync(_) => {}
                ValueKind::IndefinitePtr(n) | ValueKind::Int(n) => number_destroy(*n),
                ValueKind::DefinitePtr(loc) => location_destroy(*loc),
                ValueKind::Literal(_) => {}
                ValueKind::Struct(_) => {}
            }
        }
    }
}

impl Drop for StructValue {
    fn drop(&mut self) {
        unsafe {
            for &p in self.m.values() {
                object_destroy(p);
            }
        }
    }
}

pub unsafe fn value_destroy(v: *mut Value) {
    drop(Box::from_raw(v));
}

pub unsafe fn value_str(v: *mut Value) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    match &(*v).kind {
        ValueKind::Sync(n) => {
            value_sync_sprint(n, b);
        }
        ValueKind::DefinitePtr(loc) => {
            value_definite_ptr_sprint(*loc, b);
        }
        ValueKind::IndefinitePtr(n) => {
            value_indefinite_ptr_sprint(&**n, b);
        }
        ValueKind::Int(n) => {
            value_int_sprint(&**n, b);
        }
        ValueKind::Literal(s) => {
            strbuilder_write!(b, "\"{s}\"");
        }
        ValueKind::Struct(sv) => {
            value_struct_sprint(sv, b);
        }
    }
    strbuilder_build(b)
}

unsafe fn value_sync_sprint(n: &Number, b: *mut StrBuilder) {
    strbuilder_write!(b, "comp:{}", number_str(n));
}

unsafe fn value_definite_ptr_sprint(loc: *mut Location, b: *mut StrBuilder) {
    let s = location_str(&*loc);
    strbuilder_write!(b, "ptr:{s}");
}

unsafe fn value_indefinite_ptr_sprint(n: &Number, b: *mut StrBuilder) {
    strbuilder_write!(b, "ptr:{}", number_str(n));
}

unsafe fn value_int_sprint(n: &Number, b: *mut StrBuilder) {
    strbuilder_write!(b, "int:{}", number_str(n));
}

unsafe fn value_struct_sprint(sv: &StructValue, b: *mut StrBuilder) {
    strbuilder_write!(b, "struct:{{");
    let n = sv.members.len();
    for (i, var) in sv.members.iter().enumerate() {
        let f: *mut libc::c_char = ast_variable_name(var);
        let f_str = CStr::from_ptr(f).to_str().unwrap();
        let val: *mut Value = object_as_value(sv.m.get(f_str).copied().unwrap());
        let val_str = if !val.is_null() {
            value_str(val)
        } else {
            OwningCStr::empty()
        };
        strbuilder_write!(
            b,
            ".{} = <{val_str}>{}",
            cstr!(f),
            if i + 1 < n { ", " } else { "" },
        );
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
        ValueKind::Int(n) => number_isconstant(&**n),
        _ => false,
    }
}

pub unsafe fn value_issync(v: &Value) -> bool {
    match &v.kind {
        ValueKind::Sync(n) => number_issync(n),
        _ => false,
    }
}

pub unsafe fn value_as_sync(v: &Value) -> &AstExpr {
    let ValueKind::Sync(n) = &v.kind else {
        panic!();
    };
    number_as_sync(n)
}

pub unsafe fn value_into_sync(v: *mut Value) -> *mut AstExpr {
    let ValueKind::Sync(n) = &mut (*v).kind else {
        panic!();
    };
    number_into_sync(&mut **n)
}

pub unsafe fn value_isint(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Int(_))
}

pub unsafe fn value_to_expr(v: *mut Value) -> *mut AstExpr {
    match &(*v).kind {
        ValueKind::DefinitePtr(_) => Box::into_raw(ast_expr_identifier_create(value_str(v))),
        ValueKind::IndefinitePtr(_) => Box::into_raw(ast_expr_identifier_create(value_str(v))),
        ValueKind::Literal(_) => Box::into_raw(ast_expr_copy(&*value_as_literal(&*v))),
        ValueKind::Sync(n) => Box::into_raw(ast_expr_copy(number_as_sync(n))),
        ValueKind::Int(n) => number_to_expr(*n),
        _ => panic!(),
    }
}

#[allow(dead_code)]
pub unsafe fn value_isliteral(v: &Value) -> bool {
    matches!(v.kind, ValueKind::Literal(_))
}

pub unsafe fn value_as_literal(v: &Value) -> *mut AstExpr {
    let ValueKind::Literal(s) = &v.kind else {
        panic!();
    };
    Box::into_raw(ast_expr_literal_create(s.clone()))
}

pub unsafe fn value_references(v: *mut Value, loc: &Location, s: *mut State) -> bool {
    match &(*v).kind {
        ValueKind::DefinitePtr(vloc) => location_references(&**vloc, loc, s),
        ValueKind::Struct(sv) => struct_references(sv, loc, s),
        _ => false,
    }
}

unsafe fn struct_references(sv: &StructValue, loc: &Location, s: *mut State) -> bool {
    sv.m.values().any(|&obj| {
        let val: *mut Value = object_as_value(obj);
        !val.is_null() && value_references(val, loc, s)
    })
}

pub unsafe fn value_equal(v1: &Value, v2: &Value) -> bool {
    match (&v1.kind, &v2.kind) {
        (ValueKind::Literal(s1), ValueKind::Literal(s2)) => s1 == s2,
        (ValueKind::Sync(n1), ValueKind::Sync(n2)) => number_equal(n1, n2),
        (ValueKind::Int(n1), ValueKind::Int(n2)) => number_equal(&**n1, &**n2),
        _ => panic!(),
    }
}

#[allow(dead_code)]
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

#[allow(dead_code)]
unsafe fn number_range_lw(n: *mut Number) -> libc::c_int {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    assert_eq!(ranges.len(), 1);
    number_value_as_constant(ranges[0].lower)
}

#[allow(dead_code)]
unsafe fn number_range_up(n: *mut Number) -> libc::c_int {
    let NumberKind::Ranges(ranges) = &mut (*n).kind else {
        panic!();
    };
    assert_eq!(ranges.len(), 1);
    number_value_as_constant(ranges[0].upper)
}

unsafe fn number_destroy(n: *mut Number) {
    match &(*n).kind {
        NumberKind::Ranges(_) => {}
        NumberKind::Computed(computation) => {
            ast_expr_destroy(*computation);
        }
    }
}

unsafe fn number_ranges_sprint(ranges: &[NumberRange]) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_putc(b, '{' as i32 as libc::c_char);
    let n = ranges.len();
    for (i, range) in ranges.iter().enumerate() {
        let r = number_range_str(range);
        strbuilder_write!(b, "{r}{}", if i + 1 < n { ", " } else { "" });
    }
    strbuilder_putc(b, '}' as i32 as libc::c_char);
    strbuilder_build(b)
}

unsafe fn number_str(num: &Number) -> OwningCStr {
    match &num.kind {
        NumberKind::Ranges(ranges) => number_ranges_sprint(ranges),
        NumberKind::Computed(computation) => ast_expr_str(&**computation),
    }
}

unsafe fn number_equal(n1: &Number, n2: &Number) -> bool {
    match (&n1.kind, &n2.kind) {
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
    true
}

unsafe fn number_range_assumed_value(value: bool) -> Vec<NumberRange> {
    if value {
        number_range_arr_ne_create(0)
    } else {
        number_range_arr_single_create(0)
    }
}

unsafe fn number_isconstant(n: &Number) -> bool {
    let NumberKind::Ranges(ranges) = &n.kind else {
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

unsafe fn number_issync(n: &Number) -> bool {
    matches!(n.kind, NumberKind::Computed(_))
}

unsafe fn number_as_sync(n: &Number) -> &AstExpr {
    let NumberKind::Computed(computation) = &n.kind else {
        panic!();
    };
    &**computation
}

unsafe fn number_into_sync(n: *mut Number) -> *mut AstExpr {
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
    Box::into_raw(Box::new((*num).clone()))
}

impl Clone for NumberKind {
    fn clone(&self) -> Self {
        unsafe {
            match &self {
                NumberKind::Ranges(ranges) => NumberKind::Ranges(ranges.clone()),
                NumberKind::Computed(computation) => {
                    NumberKind::Computed(Box::into_raw(ast_expr_copy(&**computation)))
                }
            }
        }
    }
}

unsafe fn number_ranges_to_expr(arr: &[NumberRange]) -> *mut AstExpr {
    assert_eq!(arr.len(), 1);
    Box::into_raw(ast_expr_constant_create(number_range_as_constant(&arr[0])))
}

unsafe fn number_range_arr_canbe(arr: &[NumberRange], value: bool) -> bool {
    arr.iter().any(|range| number_range_canbe(range, value))
}

unsafe fn number_range_create(lw: NumberValue, up: NumberValue) -> NumberRange {
    NumberRange {
        lower: lw,
        upper: up,
    }
}

unsafe fn number_range_str(r: &NumberRange) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    if number_range_issingle(r) {
        strbuilder_write!(b, "{}", number_value_str(&r.lower));
    } else {
        strbuilder_write!(
            b,
            "{}:{}",
            number_value_str(&r.lower),
            number_value_str(&r.upper),
        );
    }
    strbuilder_build(b)
}

unsafe fn number_range_canbe(r: &NumberRange, value: bool) -> bool {
    if value {
        if number_value_equal(&r.lower, &r.upper) {
            return false;
        }
        number_value_le_constant(r.lower, -1) || constant_le_number_value(1, r.lower)
    } else {
        number_value_le_constant(r.lower, 0) && constant_le_number_value(1, r.upper)
    }
}

unsafe fn number_range_issingle(r: &NumberRange) -> bool {
    number_values_aresingle(&r.lower, &r.upper)
}

unsafe fn number_range_equal(r1: &NumberRange, r2: &NumberRange) -> bool {
    number_value_equal(&r1.lower, &r2.lower) && number_value_equal(&r1.upper, &r2.upper)
}

unsafe fn number_range_as_constant(r: &NumberRange) -> libc::c_int {
    if !number_range_issingle(r) {
        panic!();
    }
    number_value_as_constant(r.lower)
}

unsafe fn number_value_constant_create(constant: libc::c_int) -> NumberValue {
    NumberValue::Constant(constant)
}

unsafe fn number_value_limit_create(max: bool) -> NumberValue {
    NumberValue::Limit(max)
}

unsafe fn number_value_min_create() -> NumberValue {
    number_value_limit_create(false)
}

unsafe fn number_value_max_create() -> NumberValue {
    number_value_limit_create(true)
}

unsafe fn number_value_str(v: &NumberValue) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    match v {
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
    strbuilder_build(b)
}

unsafe fn number_values_aresingle(v1: &NumberValue, v2: &NumberValue) -> bool {
    match (*v1, *v2) {
        (NumberValue::Constant(k1), NumberValue::Constant(k2)) => k1 == k2 - 1,
        (NumberValue::Limit(max1), NumberValue::Limit(max2)) => max1 == max2,
        _ => panic!(),
    }
}

#[allow(dead_code)]
unsafe fn number_value_difference(v1: &NumberValue, v2: &NumberValue) -> libc::c_int {
    match (*v1, *v2) {
        (NumberValue::Constant(v1), NumberValue::Constant(v2)) => v1 - v2,
        _ => panic!(),
    }
}

unsafe fn number_value_equal(v1: &NumberValue, v2: &NumberValue) -> bool {
    match (*v1, *v2) {
        (NumberValue::Constant(k1), NumberValue::Constant(k2)) => k1 == k2,
        (NumberValue::Limit(max1), NumberValue::Limit(max2)) => max1 == max2,
        _ => panic!(),
    }
}

unsafe fn number_value_as_constant(v: NumberValue) -> libc::c_int {
    match v {
        NumberValue::Constant(k) => k,
        _ => panic!(),
    }
}

unsafe fn number_value_le_constant(v: NumberValue, constant: libc::c_int) -> bool {
    match v {
        NumberValue::Constant(k) => k <= constant,
        NumberValue::Limit(max) => !max,
    }
}

unsafe fn constant_le_number_value(constant: libc::c_int, v: NumberValue) -> bool {
    match v {
        NumberValue::Constant(k) => constant <= k,
        NumberValue::Limit(max) => max,
    }
}
