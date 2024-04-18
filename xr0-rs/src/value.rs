#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use libc::{calloc, free, malloc, realloc, strcmp};

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
use crate::util::{
    dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf, strbuilder_putc, Map,
};
use crate::{AstExpr, AstType, AstVariable, AstVariableArr, Location, Object, State, StrBuilder};

#[derive(Clone)]
#[repr(C)]
pub struct Value {
    pub r#type: ValueType,
    pub kind: ValueKind,
}

pub type ValueType = libc::c_uint;
pub const VALUE_STRUCT: ValueType = 4;
pub const VALUE_LITERAL: ValueType = 3;
pub const VALUE_INT: ValueType = 2;
pub const VALUE_PTR: ValueType = 1;
pub const VALUE_SYNC: ValueType = 0;

#[derive(Copy, Clone)]
#[repr(C)]
pub union ValueKind {
    pub ptr: PtrValue,
    pub n: *mut Number,
    pub s: *mut libc::c_char,
    pub _struct: StructValue,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct StructValue {
    pub members: *mut AstVariableArr,
    pub m: *mut Map,
}

#[derive(Clone)]
#[repr(C)]
pub struct Number {
    pub r#type: NumberType,
    pub kind: NumberKind,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union NumberKind {
    pub ranges: *mut NumberRangeArr,
    pub computation: *mut AstExpr,
}

#[derive(Clone)]
#[repr(C)]
pub struct NumberRangeArr {
    pub n: libc::c_int,
    pub range: *mut *mut NumberRange,
}

#[derive(Clone)]
#[repr(C)]
pub struct NumberRange {
    pub lower: *mut NumberValue,
    pub upper: *mut NumberValue,
}

#[derive(Clone)]
#[repr(C)]
pub struct NumberValue {
    pub r#type: NumberValueType,
    pub kind: NumberValueKind,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union NumberValueKind {
    pub constant: libc::c_int,
    pub max: bool,
}

pub type NumberValueType = libc::c_uint;
pub const NUMBER_VALUE_LIMIT: NumberValueType = 1;
pub const NUMBER_VALUE_CONSTANT: NumberValueType = 0;
pub type NumberType = libc::c_uint;
pub const NUMBER_COMPUTED: NumberType = 1;
pub const NUMBER_RANGES: NumberType = 0;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct PtrValue {
    pub isindefinite: bool,
    pub kind: PtrValueKind,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union PtrValueKind {
    pub loc: *mut Location,
    pub n: *mut Number,
}

pub unsafe fn value_ptr_create(loc: *mut Location) -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_PTR;
    (*v).kind.ptr.isindefinite = false;
    (*v).kind.ptr.kind.loc = loc;
    return v;
}

pub unsafe fn value_ptr_indefinite_create() -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_PTR;
    (*v).kind.ptr.isindefinite = 1 as libc::c_int != 0;
    (*v).kind.ptr.kind.n = number_indefinite_create();
    return v;
}
unsafe fn ptr_referencesheap(v: *mut Value, s: *mut State) -> bool {
    return !(*v).kind.ptr.isindefinite
        && location_referencesheap((*v).kind.ptr.kind.loc, s) as libc::c_int != 0;
}

pub unsafe fn value_ptr_copy(old: *mut Value) -> *mut Value {
    let new: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if new.is_null() {
        panic!();
    }
    (*new).r#type = VALUE_PTR;
    (*new).kind.ptr.isindefinite = (*old).kind.ptr.isindefinite;
    if (*old).kind.ptr.isindefinite {
        (*new).kind.ptr.kind.n = number_copy((*old).kind.ptr.kind.n);
    } else {
        (*new).kind.ptr.kind.loc = location_copy((*old).kind.ptr.kind.loc);
    }
    return new;
}

pub unsafe fn value_ptr_sprint(v: *mut Value, b: *mut StrBuilder) {
    let s: *mut libc::c_char = if (*v).kind.ptr.isindefinite as libc::c_int != 0 {
        number_str((*v).kind.ptr.kind.n)
    } else {
        location_str((*v).kind.ptr.kind.loc)
    };
    strbuilder_printf(b, b"ptr:%s\0" as *const u8 as *const libc::c_char, s);
    free(s as *mut libc::c_void);
}

pub unsafe fn value_int_create(val: libc::c_int) -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_INT;
    (*v).kind.n = number_single_create(val);
    return v;
}

pub unsafe fn value_literal_create(lit: *mut libc::c_char) -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_LITERAL;
    (*v).kind.s = dynamic_str(lit);
    return v;
}

pub unsafe fn value_transfigure(v: *mut Value, compare: *mut State, islval: bool) -> *mut Value {
    match (*v).r#type {
        0 | 3 => {
            return if islval as libc::c_int != 0 {
                0 as *mut Value
            } else {
                v
            }
        }
        4 => {
            panic!();
        }
        2 => {}
        1 => return location_transfigure(value_as_location(v), compare),
        _ => {
            panic!();
        }
    }
    return if islval as libc::c_int != 0 {
        0 as *mut Value
    } else {
        state_vconst(
            compare,
            ast_type_create_voidptr(),
            0 as *mut libc::c_char,
            false,
        )
    };
}

pub unsafe fn value_int_ne_create(not_val: libc::c_int) -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_INT;
    (*v).kind.n = number_ne_create(not_val);
    return v;
}

pub unsafe fn value_int_range_create(lw: libc::c_int, excl_up: libc::c_int) -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_INT;
    (*v).kind.n = number_with_range_create(lw, excl_up);
    return v;
}

pub unsafe fn value_int_indefinite_create() -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_INT;
    (*v).kind.n = number_indefinite_create();
    return v;
}

pub unsafe fn value_int_lw(v: *mut Value) -> libc::c_int {
    return number_range_lw((*v).kind.n);
}

pub unsafe fn value_int_up(v: *mut Value) -> libc::c_int {
    return number_range_up((*v).kind.n);
}

pub unsafe fn value_sync_create(e: *mut AstExpr) -> *mut Value {
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_SYNC;
    (*v).kind.n = number_computed_create(e);
    return v;
}

pub unsafe fn value_sync_copy(old: *mut Value) -> *mut Value {
    if !((*old).r#type as libc::c_uint == VALUE_SYNC as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let new: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if new.is_null() {
        panic!();
    }
    (*new).r#type = VALUE_SYNC;
    (*new).kind.n = number_copy((*old).kind.n);
    return new;
}

pub unsafe fn value_int_copy(old: *mut Value) -> *mut Value {
    if !((*old).r#type as libc::c_uint == VALUE_INT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let new: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if new.is_null() {
        panic!();
    }
    (*new).r#type = VALUE_INT;
    (*new).kind.n = number_copy((*old).kind.n);
    return new;
}

pub unsafe fn value_struct_create(t: *mut AstType) -> *mut Value {
    let members = ast_variable_arr_from_slice(
        ast_type_struct_members(&*t).expect("can't create value of incomplete type"),
    );
    let v: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = VALUE_STRUCT;
    (*v).kind._struct.members = members;
    (*v).kind._struct.m = Box::into_raw(frommembers(members));
    return v;
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
    let n: libc::c_int = ast_variable_arr_n((*v).kind._struct.members);
    let var: *mut *mut AstVariable = ast_variable_arr_v((*v).kind._struct.members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let field: *mut libc::c_char = ast_variable_name(*var.offset(i as isize));
        let obj: *mut Object = (*(*v).kind._struct.m).get(field) as *mut Object;
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"%s.%s\0" as *const u8 as *const libc::c_char,
            comment,
            field,
        );
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

pub unsafe fn value_pf_augment(old: *mut Value, root: *mut AstExpr) -> *mut Value {
    if !value_isstruct(old) {
        panic!();
    }
    let v: *mut Value = value_copy(old);
    let n: libc::c_int = ast_variable_arr_n((*v).kind._struct.members);
    let var: *mut *mut AstVariable = ast_variable_arr_v((*v).kind._struct.members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let field: *mut libc::c_char = ast_variable_name(*var.offset(i as isize));
        let obj: *mut Object = (*(*v).kind._struct.m).get(field) as *mut Object;
        let obj_value: *mut Value = object_as_value(obj);
        if !obj_value.is_null() {
            if value_issync(obj_value) {
                object_assign(
                    obj,
                    value_sync_create(Box::into_raw(ast_expr_member_create(
                        ast_expr_copy(&*root),
                        dynamic_str(field),
                    ))),
                );
            }
        }
        i += 1;
    }
    return v;
}

pub unsafe fn value_isstruct(v: *mut Value) -> bool {
    return (*v).r#type as libc::c_uint == VALUE_STRUCT as libc::c_int as libc::c_uint;
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
                0 as *mut Value,
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

pub unsafe fn value_struct_copy(old: *mut Value) -> *mut Value {
    let new: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if new.is_null() {
        panic!();
    }
    (*new).r#type = VALUE_STRUCT;
    (*new).kind._struct.members = ast_variable_arr_copy((*old).kind._struct.members);
    (*new).kind._struct.m = Box::into_raw(copymembers(&*(*old).kind._struct.m));
    return new;
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

pub unsafe fn value_struct_abstractcopy(old: *mut Value, s: *mut State) -> *mut Value {
    let new: *mut Value = malloc(::core::mem::size_of::<Value>()) as *mut Value;
    if new.is_null() {
        panic!();
    }
    (*new).r#type = VALUE_STRUCT;
    (*new).kind._struct.members = ast_variable_arr_copy((*old).kind._struct.members);
    (*new).kind._struct.m = Box::into_raw(abstractcopymembers(&*(*old).kind._struct.m, s));
    return new;
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
    let members: *mut AstVariableArr = (*v).kind._struct.members;
    let n: libc::c_int = ast_variable_arr_n(members);
    let var: *mut *mut AstVariable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if strcmp(member, ast_variable_name(*var.offset(i as isize))) == 0 as libc::c_int {
            return ast_variable_type(*var.offset(i as isize));
        }
        i += 1;
    }
    0 as *mut AstType
}

pub unsafe fn value_struct_member(v: *mut Value, member: *mut libc::c_char) -> *mut Object {
    return (*(*v).kind._struct.m).get(member) as *mut Object;
}
unsafe fn struct_referencesheap(v: *mut Value, s: *mut State) -> bool {
    let m: &Map = &*(*v).kind._struct.m;
    for p in m.values() {
        let val: *mut Value = object_as_value(p as *mut Object);
        if !val.is_null() && value_referencesheap(val, s) as libc::c_int != 0 {
            return 1 as libc::c_int != 0;
        }
    }
    false
}

pub unsafe fn value_struct_sprint(v: *mut Value, b: *mut StrBuilder) {
    strbuilder_printf(b, b"struct:{\0" as *const u8 as *const libc::c_char);
    let members: *mut AstVariableArr = (*v).kind._struct.members;
    let n: libc::c_int = ast_variable_arr_n(members);
    let var: *mut *mut AstVariable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let f: *mut libc::c_char = ast_variable_name(*var.offset(i as isize));
        let val: *mut Value = object_as_value((*(*v).kind._struct.m).get(f) as *mut Object);
        let val_str: *mut libc::c_char = if !val.is_null() {
            value_str(val)
        } else {
            dynamic_str(b"\0" as *const u8 as *const libc::c_char)
        };
        strbuilder_printf(
            b,
            b".%s = <%s>%s\0" as *const u8 as *const libc::c_char,
            f,
            val_str,
            if (i + 1 as libc::c_int) < n {
                b", \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            },
        );
        free(val_str as *mut libc::c_void);
        i += 1;
    }
    strbuilder_printf(b, b"}\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn value_int_sprint(v: *mut Value, b: *mut StrBuilder) {
    strbuilder_printf(
        b,
        b"int:%s\0" as *const u8 as *const libc::c_char,
        number_str((*v).kind.n),
    );
}

pub unsafe fn value_sync_sprint(v: *mut Value, b: *mut StrBuilder) {
    strbuilder_printf(
        b,
        b"comp:%s\0" as *const u8 as *const libc::c_char,
        number_str((*v).kind.n),
    );
}

pub unsafe fn value_copy(v: *mut Value) -> *mut Value {
    match (*v).r#type {
        0 => value_sync_copy(v),
        1 => value_ptr_copy(v),
        2 => value_int_copy(v),
        3 => value_literal_create((*v).kind.s),
        4 => value_struct_copy(v),
        _ => panic!(),
    }
}

pub unsafe fn value_abstractcopy(v: *mut Value, s: *mut State) -> *mut Value {
    if !value_referencesheap(v, s) {
        return 0 as *mut Value;
    }
    match (*v).r#type {
        1 => value_copy(v),
        4 => value_struct_abstractcopy(v, s),
        _ => panic!(),
    }
}

pub unsafe fn value_destroy(v: *mut Value) {
    match (*v).r#type {
        0 => {
            number_destroy((*v).kind.n);
        }
        1 => {
            if (*v).kind.ptr.isindefinite {
                number_destroy((*v).kind.ptr.kind.n);
            } else if !((*v).kind.ptr.kind.loc).is_null() {
                location_destroy((*v).kind.ptr.kind.loc);
            }
        }
        2 => {
            number_destroy((*v).kind.n);
        }
        3 => {
            free((*v).kind.s as *mut libc::c_void);
        }
        4 => {
            ast_variable_arr_destroy((*v).kind._struct.members);
            destroymembers(&*(*v).kind._struct.m);
            Box::from_raw((*v).kind._struct.m).destroy();
        }
        _ => panic!(),
    }
    free(v as *mut libc::c_void);
}

pub unsafe fn value_str(v: *mut Value) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match (*v).r#type {
        0 => {
            value_sync_sprint(v, b);
        }
        1 => {
            value_ptr_sprint(v, b);
        }
        2 => {
            value_int_sprint(v, b);
        }
        3 => {
            strbuilder_printf(
                b,
                b"\"%s\"\0" as *const u8 as *const libc::c_char,
                (*v).kind.s,
            );
        }
        4 => {
            value_struct_sprint(v, b);
        }
        _ => panic!(),
    }
    return strbuilder_build(b);
}

pub unsafe fn value_islocation(v: *mut Value) -> bool {
    if v.is_null() {
        panic!();
    }
    return (*v).r#type as libc::c_uint == VALUE_PTR as libc::c_int as libc::c_uint
        && !(*v).kind.ptr.isindefinite;
}

pub unsafe fn value_as_location(v: *mut Value) -> *mut Location {
    if !value_islocation(v) {
        panic!();
    }
    return (*v).kind.ptr.kind.loc;
}

pub unsafe fn value_referencesheap(v: *mut Value, s: *mut State) -> bool {
    match (*v).r#type {
        1 => return ptr_referencesheap(v, s),
        4 => return struct_referencesheap(v, s),
        _ => return false,
    };
}

pub unsafe fn value_as_constant(v: *mut Value) -> libc::c_int {
    if !((*v).r#type as libc::c_uint == VALUE_INT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return number_as_constant((*v).kind.n);
}

pub unsafe fn value_isconstant(v: *mut Value) -> bool {
    if (*v).r#type as libc::c_uint != VALUE_INT as libc::c_int as libc::c_uint {
        return false;
    }
    return number_isconstant((*v).kind.n);
}

pub unsafe fn value_issync(v: *mut Value) -> bool {
    if (*v).r#type as libc::c_uint != VALUE_SYNC as libc::c_int as libc::c_uint {
        return false;
    }
    return number_issync((*v).kind.n);
}

pub unsafe fn value_as_sync(v: *mut Value) -> *mut AstExpr {
    assert_eq!((*v).r#type, VALUE_SYNC);
    return number_as_sync((*v).kind.n);
}

pub unsafe fn value_to_expr(v: *mut Value) -> *mut AstExpr {
    match (*v).r#type {
        1 => Box::into_raw(ast_expr_identifier_create(value_str(v))),
        3 => Box::into_raw(ast_expr_copy(&*value_as_literal(v))),
        0 => Box::into_raw(ast_expr_copy(&*value_as_sync(v))),
        2 => number_to_expr((*v).kind.n),
        _ => panic!(),
    }
}

pub unsafe fn value_isliteral(v: *mut Value) -> bool {
    if (*v).r#type as libc::c_uint != VALUE_LITERAL as libc::c_int as libc::c_uint {
        return false;
    }
    true
}

pub unsafe fn value_as_literal(v: *mut Value) -> *mut AstExpr {
    if !((*v).r#type as libc::c_uint == VALUE_LITERAL as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    Box::into_raw(ast_expr_literal_create((*v).kind.s))
}

pub unsafe fn value_type(v: *mut Value) -> ValueType {
    return (*v).r#type;
}

pub unsafe fn value_references(v: *mut Value, loc: *mut Location, s: *mut State) -> bool {
    match (*v).r#type {
        1 => {
            return !(*v).kind.ptr.isindefinite
                && location_references((*v).kind.ptr.kind.loc, loc, s) as libc::c_int != 0;
        }
        4 => return struct_references(v, loc, s),
        _ => return false,
    };
}

unsafe fn struct_references(v: *mut Value, loc: *mut Location, s: *mut State) -> bool {
    let m: &Map = &*(*v).kind._struct.m;
    for p in m.values() {
        let val: *mut Value = object_as_value(p as *mut Object);
        if !val.is_null() && value_references(val, loc, s) as libc::c_int != 0 {
            return true;
        }
    }
    false
}

pub unsafe fn values_comparable(v1: *mut Value, v2: *mut Value) -> bool {
    (*v1).r#type as libc::c_uint == (*v2).r#type as libc::c_uint
}

pub unsafe fn value_equal(v1: *mut Value, v2: *mut Value) -> bool {
    if !((*v1).r#type as libc::c_uint == (*v2).r#type as libc::c_uint) {
        panic!();
    }
    match (*v1).r#type {
        3 => strcmp((*v1).kind.s, (*v2).kind.s) == 0,
        2 | 0 => number_equal((*v1).kind.n, (*v2).kind.n),
        _ => panic!(),
    }
}

pub unsafe fn value_assume(v: *mut Value, value: bool) -> bool {
    match (*v).r#type {
        2 => number_assume((*v).kind.n, value),
        1 => {
            if !(*v).kind.ptr.isindefinite {
                panic!();
            }
            number_assume((*v).kind.ptr.kind.n, value)
        }
        _ => panic!(),
    }
}

pub unsafe fn number_ranges_create(ranges: *mut NumberRangeArr) -> *mut Number {
    let num: *mut Number = calloc(1, ::core::mem::size_of::<Number>()) as *mut Number;
    (*num).r#type = NUMBER_RANGES;
    (*num).kind.ranges = ranges;
    return num;
}

pub unsafe fn number_single_create(val: libc::c_int) -> *mut Number {
    return number_ranges_create(number_range_arr_single_create(val));
}

pub unsafe fn number_range_arr_single_create(val: libc::c_int) -> *mut NumberRangeArr {
    let arr: *mut NumberRangeArr = number_range_arr_create();
    number_range_arr_append(
        arr,
        number_range_create(
            number_value_constant_create(val),
            number_value_constant_create(val + 1 as libc::c_int),
        ),
    );
    return arr;
}

pub unsafe fn number_computed_create(e: *mut AstExpr) -> *mut Number {
    let num: *mut Number = calloc(1, ::core::mem::size_of::<Number>()) as *mut Number;
    (*num).r#type = NUMBER_COMPUTED;
    (*num).kind.computation = e;
    return num;
}

pub unsafe fn number_range_arr_ne_create(val: libc::c_int) -> *mut NumberRangeArr {
    let arr: *mut NumberRangeArr = number_range_arr_create();
    number_range_arr_append(
        arr,
        number_range_create(number_value_min_create(), number_value_constant_create(val)),
    );
    number_range_arr_append(
        arr,
        number_range_create(
            number_value_constant_create(val + 1 as libc::c_int),
            number_value_max_create(),
        ),
    );
    return arr;
}

pub unsafe fn number_ne_create(val: libc::c_int) -> *mut Number {
    return number_ranges_create(number_range_arr_ne_create(val));
}

pub unsafe fn number_with_range_create(lw: libc::c_int, excl_up: libc::c_int) -> *mut Number {
    let arr: *mut NumberRangeArr = number_range_arr_create();
    number_range_arr_append(
        arr,
        number_range_create(
            number_value_constant_create(lw),
            number_value_constant_create(excl_up),
        ),
    );
    return number_ranges_create(arr);
}

pub unsafe fn number_indefinite_create() -> *mut Number {
    let arr: *mut NumberRangeArr = number_range_arr_create();
    number_range_arr_append(
        arr,
        number_range_create(number_value_min_create(), number_value_max_create()),
    );
    return number_ranges_create(arr);
}

pub unsafe fn number_range_lw(n: *mut Number) -> libc::c_int {
    if !(number_range_arr_n((*n).kind.ranges) == 1 as libc::c_int) as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let r: *mut NumberRange =
        *(number_range_arr_range((*n).kind.ranges)).offset(0 as libc::c_int as isize);
    return number_value_as_constant(number_range_lower(r));
}

pub unsafe fn number_range_up(n: *mut Number) -> libc::c_int {
    if !(number_range_arr_n((*n).kind.ranges) == 1 as libc::c_int) as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let r: *mut NumberRange =
        *(number_range_arr_range((*n).kind.ranges)).offset(0 as libc::c_int as isize);
    return number_value_as_constant(number_range_upper(r));
}

pub unsafe fn number_destroy(n: *mut Number) {
    match (*n).r#type {
        0 => {
            number_range_arr_destroy((*n).kind.ranges);
        }
        1 => {
            ast_expr_destroy((*n).kind.computation);
        }
        _ => panic!(),
    };
}

pub unsafe fn number_ranges_sprint(num: *mut Number) -> *mut libc::c_char {
    if !((*num).r#type as libc::c_uint == NUMBER_RANGES as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let b: *mut StrBuilder = strbuilder_create();
    let n: libc::c_int = number_range_arr_n((*num).kind.ranges);
    let range: *mut *mut NumberRange = number_range_arr_range((*num).kind.ranges);
    strbuilder_putc(b, '{' as i32 as libc::c_char);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let r: *mut libc::c_char = number_range_str(*range.offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%s\0" as *const u8 as *const libc::c_char,
            r,
            if (i + 1 as libc::c_int) < n {
                b", \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            },
        );
        free(r as *mut libc::c_void);
        i += 1;
    }
    strbuilder_putc(b, '}' as i32 as libc::c_char);
    return strbuilder_build(b);
}

pub unsafe fn number_str(num: *mut Number) -> *mut libc::c_char {
    match (*num).r#type {
        0 => number_ranges_sprint(num),
        1 => ast_expr_str(&*(*num).kind.computation),
        _ => panic!(),
    }
}

pub unsafe fn number_equal(n1: *mut Number, n2: *mut Number) -> bool {
    if !((*n1).r#type as libc::c_uint == (*n2).r#type as libc::c_uint) {
        panic!();
    }
    match (*n1).r#type {
        0 => number_ranges_equal(n1, n2),
        1 => ast_expr_equal(&*(*n1).kind.computation, &*(*n2).kind.computation),

        _ => panic!(),
    }
}

pub unsafe fn number_ranges_equal(n1: *mut Number, n2: *mut Number) -> bool {
    if !((*n1).r#type as libc::c_uint == (*n2).r#type as libc::c_uint
        && (*n1).r#type as libc::c_uint == NUMBER_RANGES as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let len: libc::c_int = number_range_arr_n((*n1).kind.ranges);
    if len != number_range_arr_n((*n2).kind.ranges) {
        return false;
    }
    let n1_r: *mut *mut NumberRange = number_range_arr_range((*n1).kind.ranges);
    let n2_r: *mut *mut NumberRange = number_range_arr_range((*n2).kind.ranges);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < len {
        if !number_range_equal(*n1_r.offset(i as isize), *n2_r.offset(i as isize)) {
            return false;
        }
        i += 1;
    }
    return 1 as libc::c_int != 0;
}
unsafe fn number_assume(n: *mut Number, value: bool) -> bool {
    if !((*n).r#type as libc::c_uint == NUMBER_RANGES as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    if !number_range_arr_canbe((*n).kind.ranges, value) {
        return false;
    }
    (*n).kind.ranges = number_range_assumed_value(value);
    return 1 as libc::c_int != 0;
}
unsafe fn number_range_assumed_value(value: bool) -> *mut NumberRangeArr {
    if value {
        return number_range_arr_ne_create(0 as libc::c_int);
    } else {
        return number_range_arr_single_create(0 as libc::c_int);
    };
}

pub unsafe fn number_isconstant(n: *mut Number) -> bool {
    if !((*n).r#type as libc::c_uint == NUMBER_RANGES as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return number_range_arr_n((*n).kind.ranges) == 1 as libc::c_int
        && number_range_issingle(
            *(number_range_arr_range((*n).kind.ranges)).offset(0 as libc::c_int as isize),
        ) as libc::c_int
            != 0;
}

pub unsafe fn number_as_constant(n: *mut Number) -> libc::c_int {
    if !((*n).r#type as libc::c_uint == NUMBER_RANGES as libc::c_int as libc::c_uint
        && number_range_arr_n((*n).kind.ranges) == 1 as libc::c_int) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return number_range_as_constant(
        *(number_range_arr_range((*n).kind.ranges)).offset(0 as libc::c_int as isize),
    );
}

pub unsafe fn number_issync(n: *mut Number) -> bool {
    return (*n).r#type as libc::c_uint == NUMBER_COMPUTED as libc::c_int as libc::c_uint;
}

pub unsafe fn number_as_sync(n: *mut Number) -> *mut AstExpr {
    if !((*n).r#type as libc::c_uint == NUMBER_COMPUTED as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    return (*n).kind.computation;
}

pub unsafe fn number_to_expr(n: *mut Number) -> *mut AstExpr {
    match (*n).r#type {
        0 => number_ranges_to_expr((*n).kind.ranges),
        1 => Box::into_raw(ast_expr_copy(&*number_as_sync(n))),
        _ => panic!(),
    }
}

pub unsafe fn number_copy(num: *mut Number) -> *mut Number {
    match (*num).r#type {
        0 => number_ranges_create(number_range_arr_copy((*num).kind.ranges)),
        1 => number_computed_create(Box::into_raw(ast_expr_copy(&*(*num).kind.computation))),
        _ => panic!(),
    }
}

pub unsafe fn number_range_arr_create() -> *mut NumberRangeArr {
    let arr: *mut NumberRangeArr =
        calloc(1, ::core::mem::size_of::<NumberRangeArr>()) as *mut NumberRangeArr;
    if arr.is_null() {
        panic!();
    }
    return arr;
}

pub unsafe fn number_range_arr_destroy(arr: *mut NumberRangeArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        number_range_destroy(*((*arr).range).offset(i as isize));
        i += 1;
    }
    free((*arr).range as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}

pub unsafe fn number_range_arr_n(arr: *mut NumberRangeArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn number_range_arr_range(arr: *mut NumberRangeArr) -> *mut *mut NumberRange {
    return (*arr).range;
}

pub unsafe fn number_range_arr_append(
    arr: *mut NumberRangeArr,
    r: *mut NumberRange,
) -> libc::c_int {
    (*arr).n += 1;
    (*arr).range = realloc(
        (*arr).range as *mut libc::c_void,
        (::core::mem::size_of::<*mut NumberRange>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut NumberRange;
    if ((*arr).range).is_null() {
        panic!();
    }
    let loc: libc::c_int = (*arr).n - 1 as libc::c_int;
    let ref mut fresh0 = *((*arr).range).offset(loc as isize);
    *fresh0 = r;
    return loc;
}

pub unsafe fn number_range_arr_copy(old: *mut NumberRangeArr) -> *mut NumberRangeArr {
    let new: *mut NumberRangeArr = number_range_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        number_range_arr_append(new, number_range_copy(*((*old).range).offset(i as isize)));
        i += 1;
    }
    new
}

pub unsafe fn number_ranges_to_expr(arr: *mut NumberRangeArr) -> *mut AstExpr {
    if !(number_range_arr_n(arr) == 1 as libc::c_int) {
        panic!();
    }
    Box::into_raw(ast_expr_constant_create(number_range_as_constant(
        *((*arr).range).offset(0 as libc::c_int as isize),
    )))
}

unsafe fn number_range_arr_canbe(arr: *mut NumberRangeArr, value: bool) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        if number_range_canbe(*((*arr).range).offset(i as isize), value) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return false;
}

pub unsafe fn number_range_create(lw: *mut NumberValue, up: *mut NumberValue) -> *mut NumberRange {
    let r: *mut NumberRange = malloc(::core::mem::size_of::<NumberRange>()) as *mut NumberRange;
    (*r).lower = lw;
    (*r).upper = up;
    return r;
}

pub unsafe fn number_range_destroy(r: *mut NumberRange) {
    number_value_destroy((*r).lower);
    number_value_destroy((*r).upper);
    free(r as *mut libc::c_void);
}

pub unsafe fn number_range_lower(r: *mut NumberRange) -> *mut NumberValue {
    return (*r).lower;
}

pub unsafe fn number_range_upper(r: *mut NumberRange) -> *mut NumberValue {
    return (*r).upper;
}

pub unsafe fn number_range_str(r: *mut NumberRange) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    if number_range_issingle(r) {
        strbuilder_printf(
            b,
            b"%s\0" as *const u8 as *const libc::c_char,
            number_value_str((*r).lower),
        );
    } else {
        strbuilder_printf(
            b,
            b"%s:%s\0" as *const u8 as *const libc::c_char,
            number_value_str((*r).lower),
            number_value_str((*r).upper),
        );
    }
    return strbuilder_build(b);
}

pub unsafe fn number_range_copy(r: *mut NumberRange) -> *mut NumberRange {
    return number_range_create(number_value_copy((*r).lower), number_value_copy((*r).upper));
}
unsafe fn number_range_canbe(r: *mut NumberRange, value: bool) -> bool {
    if value {
        if number_value_equal((*r).lower, (*r).upper) {
            return false;
        }
        return number_value_le_constant((*r).lower, -(1 as libc::c_int)) as libc::c_int != 0
            || constant_le_number_value(1 as libc::c_int, (*r).lower) as libc::c_int != 0;
    } else {
        return number_value_le_constant((*r).lower, 0 as libc::c_int) as libc::c_int != 0
            && constant_le_number_value(1 as libc::c_int, (*r).upper) as libc::c_int != 0;
    };
}

pub unsafe fn number_range_issingle(r: *mut NumberRange) -> bool {
    return number_values_aresingle((*r).lower, (*r).upper);
}

pub unsafe fn number_range_equal(r1: *mut NumberRange, r2: *mut NumberRange) -> bool {
    return number_value_equal((*r1).lower, (*r2).lower) as libc::c_int != 0
        && number_value_equal((*r1).upper, (*r2).upper) as libc::c_int != 0;
}

pub unsafe fn number_range_as_constant(r: *mut NumberRange) -> libc::c_int {
    if !number_range_issingle(r) {
        panic!();
    }
    return number_value_as_constant((*r).lower);
}

pub unsafe fn number_value_constant_create(constant: libc::c_int) -> *mut NumberValue {
    let v: *mut NumberValue = malloc(::core::mem::size_of::<NumberValue>()) as *mut NumberValue;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = NUMBER_VALUE_CONSTANT;
    (*v).kind.constant = constant;
    return v;
}

pub unsafe fn number_value_limit_create(max: bool) -> *mut NumberValue {
    let v: *mut NumberValue = malloc(::core::mem::size_of::<NumberValue>()) as *mut NumberValue;
    if v.is_null() {
        panic!();
    }
    (*v).r#type = NUMBER_VALUE_LIMIT;
    (*v).kind.max = max;
    return v;
}

pub unsafe fn number_value_min_create() -> *mut NumberValue {
    return number_value_limit_create(false);
}

pub unsafe fn number_value_max_create() -> *mut NumberValue {
    return number_value_limit_create(1 as libc::c_int != 0);
}

pub unsafe fn number_value_destroy(v: *mut NumberValue) {
    free(v as *mut libc::c_void);
}

pub unsafe fn number_value_str(v: *mut NumberValue) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match (*v).r#type {
        0 => {
            strbuilder_printf(
                b,
                b"%d\0" as *const u8 as *const libc::c_char,
                (*v).kind.constant,
            );
        }
        1 => {
            strbuilder_printf(
                b,
                b"%s\0" as *const u8 as *const libc::c_char,
                if (*v).kind.max as libc::c_int != 0 {
                    b"MAX\0" as *const u8 as *const libc::c_char
                } else {
                    b"MIN\0" as *const u8 as *const libc::c_char
                },
            );
        }
        _ => panic!(),
    }
    return strbuilder_build(b);
}

pub unsafe fn number_value_copy(v: *mut NumberValue) -> *mut NumberValue {
    match (*v).r#type {
        0 => number_value_constant_create((*v).kind.constant),
        1 => number_value_limit_create((*v).kind.max),
        _ => panic!(),
    }
}

pub unsafe fn number_values_aresingle(v1: *mut NumberValue, v2: *mut NumberValue) -> bool {
    if (*v1).r#type != (*v2).r#type {
        return false;
    }
    match (*v1).r#type {
        0 => (*v1).kind.constant == (*v2).kind.constant - 1 as libc::c_int,
        1 => (*v1).kind.max as libc::c_int == (*v2).kind.max as libc::c_int,
        _ => panic!(),
    }
}

pub unsafe fn number_value_difference(v1: *mut NumberValue, v2: *mut NumberValue) -> libc::c_int {
    if !((*v1).r#type as libc::c_uint == (*v2).r#type as libc::c_uint) {
        panic!();
    }
    match (*v1).r#type {
        0 => (*v1).kind.constant - (*v2).kind.constant,
        _ => panic!(),
    }
}

pub unsafe fn number_value_equal(v1: *mut NumberValue, v2: *mut NumberValue) -> bool {
    if (*v1).r#type as libc::c_uint != (*v2).r#type as libc::c_uint {
        return false;
    }
    match (*v1).r#type {
        0 => number_value_difference(v1, v2) == 0 as libc::c_int,
        1 => (*v1).kind.max as libc::c_int == (*v2).kind.max as libc::c_int,
        _ => panic!(),
    }
}

pub unsafe fn number_value_as_constant(v: *mut NumberValue) -> libc::c_int {
    if !((*v).r#type as libc::c_uint == NUMBER_VALUE_CONSTANT as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*v).kind.constant;
}
unsafe fn number_value_le_constant(v: *mut NumberValue, constant: libc::c_int) -> bool {
    match (*v).r#type {
        0 => (*v).kind.constant <= constant,
        1 => !(*v).kind.max,
        _ => panic!(),
    }
}
unsafe fn constant_le_number_value(constant: libc::c_int, v: *mut NumberValue) -> bool {
    match (*v).r#type {
        0 => constant <= (*v).kind.constant,
        1 => (*v).kind.max,
        _ => panic!(),
    }
}
