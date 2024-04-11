#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_variables
)]

use libc::{calloc, exit, fprintf, free, malloc, realloc, strcmp, strlen, strncmp};

use crate::c_util::__stderrp;
use crate::ext::{
    externals_declarefunc, externals_declarestruct, externals_declaretypedef, externals_declarevar,
    externals_getfunc, externals_getstruct, externals_gettypedef,
};
use crate::math::{
    math_atom_nat_create, math_atom_variable_create, math_eq, math_expr_atom_create,
    math_expr_destroy, math_expr_neg_create, math_expr_sum_create, math_ge, math_gt, math_le,
    math_lt,
};
use crate::object::{
    object_as_value, object_assign, object_destroy, object_getmember, object_getmembertype,
    object_hasvalue,
};
use crate::parser::{lexememarker, lexememarker_copy, lexememarker_destroy, lexememarker_str};
use crate::props::{props_contradicts, props_get, props_install};
use crate::state::state::{
    object_res, state_addresses_deallocand, state_alloc, state_clump, state_copy,
    state_copywithname, state_create, state_create_withprops, state_dealloc, state_declare,
    state_deref, state_destroy, state_equal, state_get, state_getext, state_getloc,
    state_getobject, state_getobjecttype, state_getprops, state_getresult, state_getvconst,
    state_hasgarbage, state_isalloc, state_islval, state_popframe, state_pushframe,
    state_range_alloc, state_range_aredeallocands, state_range_dealloc, state_static_init,
    state_str, state_vconst,
};
use crate::util::{
    dynamic_str, error, error_create, map, strbuilder_build, strbuilder_create, strbuilder_printf,
    strbuilder_putc, string_arr, string_arr_append, string_arr_concat, string_arr_contains,
    string_arr_create, string_arr_deque, string_arr_n, string_arr_s, v_printf,
};
use crate::value::{
    value_as_constant, value_as_location, value_as_sync, value_copy, value_destroy, value_equal,
    value_int_create, value_int_indefinite_create, value_isconstant, value_islocation,
    value_isstruct, value_issync, value_literal_create, value_pf_augment,
    value_ptr_indefinite_create, value_str, value_struct_indefinite_create, value_struct_member,
    value_sync_create, value_to_expr, values_comparable,
};
use crate::{
    math_expr, Externals as externals, Object as object, Props as props, State as state,
    StrBuilder as strbuilder, Value as value, Variable as variable,
};

pub type ast_alloc_kind = libc::c_uint;
pub const CLUMP: ast_alloc_kind = 4;
pub const DEALLOC: ast_alloc_kind = 2;
pub const ALLOC: ast_alloc_kind = 1;
pub type ast_unary_operator = libc::c_uint;
pub const UNARY_OP_BANG: ast_unary_operator = 32;
pub const UNARY_OP_ONES_COMPLEMENT: ast_unary_operator = 16;
pub const UNARY_OP_NEGATIVE: ast_unary_operator = 8;
pub const UNARY_OP_POSITIVE: ast_unary_operator = 4;
pub const UNARY_OP_DEREFERENCE: ast_unary_operator = 2;
pub const UNARY_OP_ADDRESS: ast_unary_operator = 1;
pub type ast_binary_operator = libc::c_uint;
pub const BINARY_OP_SUBTRACTION: ast_binary_operator = 128;
pub const BINARY_OP_ADDITION: ast_binary_operator = 64;
pub const BINARY_OP_GE: ast_binary_operator = 32;
pub const BINARY_OP_LE: ast_binary_operator = 16;
pub const BINARY_OP_GT: ast_binary_operator = 8;
pub const BINARY_OP_LT: ast_binary_operator = 4;
pub const BINARY_OP_NE: ast_binary_operator = 2;
pub const BINARY_OP_EQ: ast_binary_operator = 1;

pub struct ast_expr {
    kind: ast_expr_kind,
    root: *mut ast_expr,
    u: C2RustUnnamed,
}

pub union C2RustUnnamed {
    string: *mut libc::c_char,
    constant: ConstantExpr,
    call: CallExpr,
    incdec: IncDecExpr,
    unary_op: ast_unary_operator,
    binary: BinaryExpr,
    assignment_value: *mut ast_expr,
    alloc: AllocExpr,
}

#[derive(Copy, Clone)]
pub struct AllocExpr {
    kind: ast_alloc_kind,
    arg: *mut ast_expr,
}

#[derive(Copy, Clone)]
pub struct BinaryExpr {
    op: ast_binary_operator,
    e1: *mut ast_expr,
    e2: *mut ast_expr,
}

#[derive(Copy, Clone)]
pub struct IncDecExpr {
    inc: libc::c_int,
    pre: libc::c_int,
}

#[derive(Copy, Clone)]
pub struct CallExpr {
    n: libc::c_int,
    arg: *mut *mut ast_expr,
}

#[derive(Copy, Clone)]
pub struct ConstantExpr {
    constant: libc::c_int,
    ischar: bool,
}

pub type ast_expr_kind = libc::c_uint;
pub const EXPR_ALLOCATION: ast_expr_kind = 16384;
pub const EXPR_ARBARG: ast_expr_kind = 8192;
pub const EXPR_ISDEREFERENCABLE: ast_expr_kind = 4096;
pub const EXPR_ISDEALLOCAND: ast_expr_kind = 2048;
pub const EXPR_ASSIGNMENT: ast_expr_kind = 1024;
pub const EXPR_BINARY: ast_expr_kind = 512;
pub const EXPR_UNARY: ast_expr_kind = 256;
pub const EXPR_STRUCTMEMBER: ast_expr_kind = 128;
pub const EXPR_INCDEC: ast_expr_kind = 64;
pub const EXPR_CALL: ast_expr_kind = 32;
pub const EXPR_ITERATION: ast_expr_kind = 16;
pub const EXPR_BRACKETED: ast_expr_kind = 8;
pub const EXPR_STRING_LITERAL: ast_expr_kind = 4;
pub const EXPR_CONSTANT: ast_expr_kind = 2;
pub const EXPR_IDENTIFIER: ast_expr_kind = 1;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct result {
    pub val: *mut value,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_type {
    pub mod_0: libc::c_int,
    pub base: ast_type_base,
    pub c2rust_unnamed: C2RustUnnamed_5,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_5 {
    pub ptr_type: *mut ast_type,
    pub arr: C2RustUnnamed_7,
    pub structunion: C2RustUnnamed_6,
    pub userdef: *mut libc::c_char,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_6 {
    pub tag: *mut libc::c_char,
    pub members: *mut ast_variable_arr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_variable_arr {
    pub n: libc::c_int,
    pub v: *mut *mut ast_variable,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_variable {
    pub name: *mut libc::c_char,
    pub type_0: *mut ast_type,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_7 {
    pub type_0: *mut ast_type,
    pub length: libc::c_int,
}
pub type ast_type_base = libc::c_uint;
pub const TYPE_USERDEF: ast_type_base = 14;
pub const TYPE_ENUM: ast_type_base = 13;
pub const TYPE_UNION: ast_type_base = 12;
pub const TYPE_STRUCT: ast_type_base = 11;
pub const TYPE_ARRAY: ast_type_base = 10;
pub const TYPE_POINTER: ast_type_base = 9;
pub const TYPE_UNSIGNED: ast_type_base = 8;
pub const TYPE_SIGNED: ast_type_base = 7;
pub const TYPE_DOUBLE: ast_type_base = 6;
pub const TYPE_FLOAT: ast_type_base = 5;
pub const TYPE_LONG: ast_type_base = 4;
pub const TYPE_INT: ast_type_base = 3;
pub const TYPE_SHORT: ast_type_base = 2;
pub const TYPE_CHAR: ast_type_base = 1;
pub const TYPE_VOID: ast_type_base = 0;
pub type ast_type_modifier = libc::c_uint;
pub const MOD_VOLATILE: ast_type_modifier = 64;
pub const MOD_CONST: ast_type_modifier = 32;
pub const MOD_REGISTER: ast_type_modifier = 16;
pub const MOD_AUTO: ast_type_modifier = 8;
pub const MOD_STATIC: ast_type_modifier = 4;
pub const MOD_EXTERN: ast_type_modifier = 2;
pub const MOD_TYPEDEF: ast_type_modifier = 1;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct lvalue {
    pub t: *mut ast_type,
    pub obj: *mut object,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct lvalue_res {
    pub lval: *mut lvalue,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_function {
    pub isaxiom: bool,
    pub ret: *mut ast_type,
    pub name: *mut libc::c_char,
    pub nparam: libc::c_int,
    pub param: *mut *mut ast_variable,
    pub abstract_0: *mut ast_block,
    pub body: *mut ast_block,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_block {
    pub ndecl: libc::c_int,
    pub nstmt: libc::c_int,
    pub decl: *mut *mut ast_variable,
    pub stmt: *mut *mut ast_stmt,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_stmt {
    pub kind: ast_stmt_kind,
    pub u: C2RustUnnamed_8,
    pub loc: *mut lexememarker,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_8 {
    pub labelled: C2RustUnnamed_13,
    pub compound: *mut ast_block,
    pub selection: C2RustUnnamed_12,
    pub iteration: C2RustUnnamed_11,
    pub expr: *mut ast_expr,
    pub jump: C2RustUnnamed_10,
    pub alloc: C2RustUnnamed_9,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_9 {
    pub kind: ast_alloc_kind,
    pub arg: *mut ast_expr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_10 {
    pub kind: ast_jump_kind,
    pub rv: *mut ast_expr,
}
pub type ast_jump_kind = libc::c_uint;
pub const JUMP_RETURN: ast_jump_kind = 1;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_11 {
    pub init: *mut ast_stmt,
    pub cond: *mut ast_stmt,
    pub body: *mut ast_stmt,
    pub iter: *mut ast_expr,
    pub abstract_0: *mut ast_block,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_12 {
    pub isswitch: bool,
    pub cond: *mut ast_expr,
    pub body: *mut ast_stmt,
    pub nest: *mut ast_stmt,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_13 {
    pub label: *mut libc::c_char,
    pub stmt: *mut ast_stmt,
}
pub type ast_stmt_kind = libc::c_uint;
pub const STMT_ALLOCATION: ast_stmt_kind = 512;
pub const STMT_JUMP: ast_stmt_kind = 256;
pub const STMT_ITERATION_E: ast_stmt_kind = 128;
pub const STMT_ITERATION: ast_stmt_kind = 64;
pub const STMT_SELECTION: ast_stmt_kind = 32;
pub const STMT_EXPR: ast_stmt_kind = 16;
pub const STMT_COMPOUND_V: ast_stmt_kind = 8;
pub const STMT_COMPOUND: ast_stmt_kind = 4;
pub const STMT_LABELLED: ast_stmt_kind = 2;
pub const STMT_NOP: ast_stmt_kind = 1;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct decision {
    pub decision: bool,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct preconds_result {
    pub stmt: *mut ast_stmt,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct preresult {
    pub iscontradiction: bool,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_stmt_splits {
    pub n: libc::c_int,
    pub cond: *mut *mut ast_expr,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_function_arr {
    pub n: libc::c_int,
    pub f: *mut *mut ast_function,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast_externdecl {
    pub kind: ast_externdecl_kind,
    pub c2rust_unnamed: C2RustUnnamed_14,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_14 {
    pub function: *mut ast_function,
    pub variable: *mut ast_variable,
    pub _typedef: C2RustUnnamed_15,
    pub _struct: *mut ast_type,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_15 {
    pub name: *mut libc::c_char,
    pub type_0: *mut ast_type,
}
pub type ast_externdecl_kind = libc::c_uint;
pub const EXTERN_STRUCT: ast_externdecl_kind = 3;
pub const EXTERN_TYPEDEF: ast_externdecl_kind = 2;
pub const EXTERN_VARIABLE: ast_externdecl_kind = 1;
pub const EXTERN_FUNCTION: ast_externdecl_kind = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ast {
    pub n: libc::c_int,
    pub decl: *mut *mut ast_externdecl,
}

unsafe fn expr_literal_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut res: *mut result = result_value_create(state_static_init(state, expr));
    return res;
}
unsafe fn expr_constant_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    return result_value_create(value_int_create(ast_expr_as_constant(expr)));
}
unsafe fn rangeprocess_dealloc(
    mut dealloc: *mut ast_expr,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> *mut error {
    let mut obj: *mut object = hack_base_object_from_alloc(ast_expr_alloc_arg(dealloc), state);
    return state_range_dealloc(state, obj, lw, up);
}
unsafe fn hack_base_object_from_alloc(
    mut expr: *mut ast_expr,
    mut state: *mut state,
) -> *mut object {
    let mut inner: *mut ast_expr = ast_expr_unary_operand(expr);
    let mut i: *mut ast_expr =
        ast_expr_identifier_create(dynamic_str(b"i\0" as *const u8 as *const libc::c_char));
    if !ast_expr_equal(ast_expr_binary_e2(inner), i) {
        panic!();
    }
    ast_expr_destroy(i);
    let mut res: lvalue_res = ast_expr_lvalue(ast_expr_binary_e1(inner), state);
    if !(res.err).is_null() {
        panic!();
    }
    let mut obj: *mut object = lvalue_object(res.lval);
    if obj.is_null() {
        panic!();
    }
    return obj;
}

pub unsafe fn ast_expr_equal(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> bool {
    if e1.is_null() || e2.is_null() {
        return 0 as libc::c_int != 0;
    }
    if (*e1).kind as libc::c_uint != (*e2).kind as libc::c_uint {
        return 0 as libc::c_int != 0;
    }
    match (*e1).kind as libc::c_uint {
        2 => return (*e1).u.constant.constant == (*e2).u.constant.constant,
        1 => {
            return strcmp(ast_expr_as_identifier(e1), ast_expr_as_identifier(e2))
                == 0 as libc::c_int;
        }
        4 => {
            return strcmp(ast_expr_as_literal(e1), ast_expr_as_literal(e2)) == 0 as libc::c_int;
        }
        1024 => {
            return ast_expr_equal((*e1).root, (*e2).root) as libc::c_int != 0
                && ast_expr_equal((*e1).u.assignment_value, (*e2).u.assignment_value)
                    as libc::c_int
                    != 0;
        }
        256 => {
            return (*e1).u.unary_op as libc::c_uint == (*e2).u.unary_op as libc::c_uint
                && ast_expr_equal((*e1).root, (*e2).root) as libc::c_int != 0;
        }
        512 => {
            return ast_expr_binary_op(e1) as libc::c_uint
                == ast_expr_binary_op(e2) as libc::c_uint
                && ast_expr_equal(ast_expr_binary_e1(e1), ast_expr_binary_e1(e2)) as libc::c_int
                    != 0
                && ast_expr_equal(ast_expr_binary_e2(e1), ast_expr_binary_e2(e2)) as libc::c_int
                    != 0;
        }
        32 => {
            if (*e1).u.call.n != (*e2).u.call.n {
                return 0 as libc::c_int != 0;
            }
            let mut i: libc::c_int = 0 as libc::c_int;
            while i < (*e1).u.call.n {
                if !ast_expr_equal(
                    *((*e1).u.call.arg).offset(i as isize),
                    *((*e2).u.call.arg).offset(i as isize),
                ) {
                    return 0 as libc::c_int != 0;
                }
                i += 1;
            }
            return ast_expr_equal((*e1).root, (*e2).root);
        }
        128 => {
            return ast_expr_equal(ast_expr_member_root(e1), ast_expr_member_root(e2))
                as libc::c_int
                != 0
                && strcmp(ast_expr_member_field(e1), ast_expr_member_field(e2))
                    == 0 as libc::c_int;
        }
        _ => {
            panic!();
        }
    }
}
unsafe fn rangeprocess_alloc(
    mut expr: *mut ast_expr,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> *mut error {
    let mut lval: *mut ast_expr = ast_expr_assignment_lval(expr);
    let mut rval: *mut ast_expr = ast_expr_assignment_rval(expr);
    if !(ast_expr_kind(rval) as libc::c_uint == EXPR_ALLOCATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    if !(ast_expr_alloc_kind(rval) as libc::c_uint != DEALLOC as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut obj: *mut object = hack_base_object_from_alloc(lval, state);
    return state_range_alloc(state, obj, lw, up);
}

pub unsafe fn ast_expr_matheval(mut e: *mut ast_expr) -> bool {
    if !((*e).kind as libc::c_uint == EXPR_BINARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let mut e1: *mut math_expr = math_expr((*e).u.binary.e1);
    let mut e2: *mut math_expr = math_expr((*e).u.binary.e2);
    let mut val: bool = eval_prop(e1, (*e).u.binary.op, e2);
    math_expr_destroy(e2);
    math_expr_destroy(e1);
    return val;
}
unsafe fn math_expr(mut e: *mut ast_expr) -> *mut math_expr {
    match (*e).kind as libc::c_uint {
        1 => {
            return math_expr_atom_create(math_atom_variable_create(dynamic_str((*e).u.string)));
        }
        2 => {
            if (*e).u.constant.constant < 0 as libc::c_int {
                return math_expr_neg_create(math_expr_atom_create(math_atom_nat_create(
                    -(*e).u.constant.constant as libc::c_uint,
                )));
            }
            return math_expr_atom_create(math_atom_nat_create(
                (*e).u.constant.constant as libc::c_uint,
            ));
        }
        512 => {
            return math_expr_sum_create(
                math_expr((*e).u.binary.e1),
                binary_e2((*e).u.binary.e2, (*e).u.binary.op),
            );
        }
        _ => {
            panic!();
        }
    }
}
unsafe fn binary_e2(mut e2: *mut ast_expr, mut op: ast_binary_operator) -> *mut math_expr {
    match op as libc::c_uint {
        64 => return math_expr(e2),
        128 => return math_expr_neg_create(math_expr(e2)),
        _ => {
            panic!();
        }
    }
}
unsafe fn eval_prop(
    mut e1: *mut math_expr,
    mut op: ast_binary_operator,
    mut e2: *mut math_expr,
) -> bool {
    match op as libc::c_uint {
        1 => return math_eq(e1, e2),
        2 => return !math_eq(e1, e2),
        4 => return math_lt(e1, e2),
        8 => return math_gt(e1, e2),
        16 => return math_le(e1, e2),
        32 => return math_ge(e1, e2),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_expr_decide(mut expr: *mut ast_expr, mut state: *mut state) -> bool {
    match ast_expr_kind(expr) as libc::c_uint {
        2 => return ast_expr_as_constant(expr) != 0,
        256 => return expr_unary_decide(expr, state),
        2048 => return expr_isdeallocand_decide(expr, state),
        512 => return expr_binary_decide(expr, state),
        _ => {
            panic!();
        }
    }
}

unsafe fn expr_binary_decide(mut expr: *mut ast_expr, mut state: *mut state) -> bool {
    let mut root: *mut result = ast_expr_eval(ast_expr_binary_e1(expr), state);
    let mut last: *mut result = ast_expr_eval(ast_expr_binary_e2(expr), state);
    if !(!result_iserror(root) && !result_iserror(last)) {
        panic!();
    }
    return value_compare(
        result_as_value(root),
        ast_expr_binary_op(expr),
        result_as_value(last),
    );
}

unsafe fn value_compare(
    mut v1: *mut value,
    mut op: ast_binary_operator,
    mut v2: *mut value,
) -> bool {
    match op as libc::c_uint {
        1 => return value_equal(v1, v2),
        2 => return !value_compare(v1, BINARY_OP_EQ, v2),
        _ => {
            panic!();
        }
    }
}

unsafe fn expr_isdeallocand_decide(mut expr: *mut ast_expr, mut state: *mut state) -> bool {
    let mut obj: *mut object = hack_object_from_assertion(expr, state);
    let mut isdeallocand: bool = state_addresses_deallocand(state, obj);
    return isdeallocand;
}

unsafe fn hack_object_from_assertion(
    mut expr: *mut ast_expr,
    mut state: *mut state,
) -> *mut object {
    let mut assertand: *mut ast_expr = ast_expr_isdeallocand_assertand(expr);
    let mut res: lvalue_res = ast_expr_lvalue(assertand, state);
    if !(res.err).is_null() {
        panic!();
    }
    let mut obj: *mut object = lvalue_object(res.lval);
    if obj.is_null() {
        panic!();
    }
    return obj;
}

unsafe fn expr_unary_decide(mut expr: *mut ast_expr, mut state: *mut state) -> bool {
    let mut operand: *mut ast_expr = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) as libc::c_uint {
        32 => return !ast_expr_decide(operand, state),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_expr_rangedecide(
    mut expr: *mut ast_expr,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> bool {
    match ast_expr_kind(expr) as libc::c_uint {
        256 => return unary_rangedecide(expr, lw, up, state),
        2048 => return expr_isdeallocand_rangedecide(expr, lw, up, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn expr_isdeallocand_rangedecide(
    mut expr: *mut ast_expr,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> bool {
    let mut acc: *mut ast_expr = ast_expr_isdeallocand_assertand(expr);
    if !(ast_expr_unary_op(acc) as libc::c_uint
        == UNARY_OP_DEREFERENCE as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let mut inner: *mut ast_expr = ast_expr_unary_operand(acc);
    let mut i: *mut ast_expr =
        ast_expr_identifier_create(dynamic_str(b"i\0" as *const u8 as *const libc::c_char));
    let mut j: *mut ast_expr =
        ast_expr_identifier_create(dynamic_str(b"j\0" as *const u8 as *const libc::c_char));
    if !(ast_expr_equal(ast_expr_binary_e2(inner), i) as libc::c_int != 0
        || ast_expr_equal(ast_expr_binary_e2(inner), j) as libc::c_int != 0) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    ast_expr_destroy(j);
    ast_expr_destroy(i);
    let mut res: lvalue_res = ast_expr_lvalue(ast_expr_binary_e1(acc), state);
    if !(res.err).is_null() {
        panic!();
    }
    let mut obj: *mut object = lvalue_object(res.lval);
    if obj.is_null() {
        panic!();
    }
    return state_range_aredeallocands(state, obj, lw, up);
}
unsafe fn unary_rangedecide(
    mut expr: *mut ast_expr,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> bool {
    let mut operand: *mut ast_expr = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) as libc::c_uint {
        32 => return !ast_expr_rangedecide(operand, lw, up, state),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_expr_exec(mut expr: *mut ast_expr, mut state: *mut state) -> *mut error {
    let mut res: *mut result = ast_expr_eval(expr, state);
    if result_iserror(res) {
        return result_as_error(res);
    }
    return 0 as *mut error;
}

pub unsafe fn ast_expr_arbarg_create() -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_ARBARG;
    return expr;
}

pub unsafe fn ast_expr_assume(mut expr: *mut ast_expr, mut state: *mut state) -> *mut preresult {
    return reduce_assume(expr, 1 as libc::c_int != 0, state);
}
unsafe fn reduce_assume(
    mut expr: *mut ast_expr,
    mut value: bool,
    mut s: *mut state,
) -> *mut preresult {
    match (*expr).kind as libc::c_uint {
        1 => return identifier_assume(expr, value, s),
        256 => {
            if !(ast_expr_unary_op(expr) as libc::c_uint
                == UNARY_OP_BANG as libc::c_int as libc::c_uint)
            {
                panic!();
            }
            return reduce_assume(ast_expr_unary_operand(expr), !value, s);
        }
        8 => return reduce_assume((*expr).root, value, s),
        32 | 128 => return ast_expr_pf_reduce_assume(expr, value, s),
        512 => return binary_assume(expr, value, s),
        _ => {
            panic!();
        }
    }
}
unsafe fn binary_assume(
    mut expr: *mut ast_expr,
    mut value: bool,
    mut s: *mut state,
) -> *mut preresult {
    let mut r1: *mut result = ast_expr_pf_reduce((*expr).u.binary.e1, s);
    let mut r2: *mut result = ast_expr_pf_reduce((*expr).u.binary.e2, s);
    let mut v1: *mut value = result_as_value(r1);
    let mut v2: *mut value = result_as_value(r2);
    return irreducible_assume(
        ast_expr_binary_create(value_to_expr(v1), (*expr).u.binary.op, value_to_expr(v2)),
        value,
        s,
    );
}
unsafe fn irreducible_assume(
    mut e: *mut ast_expr,
    mut value: bool,
    mut s: *mut state,
) -> *mut preresult {
    let mut prop: *mut ast_expr = ast_expr_inverted_copy(e, !value);
    let mut r: *mut preresult = irreducible_assume_actual(prop, s);
    ast_expr_destroy(prop);
    return r;
}
unsafe fn irreducible_assume_actual(mut e: *mut ast_expr, mut s: *mut state) -> *mut preresult {
    let mut p: *mut props = state_getprops(s);
    if props_contradicts(p, e) {
        return preresult_contradiction_create();
    }
    props_install(state_getprops(s), ast_expr_copy(e));
    return preresult_empty_create();
}

pub unsafe fn ast_expr_isdereferencable_create(mut assertand: *mut ast_expr) -> *mut ast_expr {
    let mut new: *mut ast_expr = ast_expr_create();
    (*new).kind = EXPR_ISDEREFERENCABLE;
    (*new).root = assertand;
    return new;
}
unsafe fn ast_expr_pf_reduce_assume(
    mut expr: *mut ast_expr,
    mut value: bool,
    mut s: *mut state,
) -> *mut preresult {
    let mut res: *mut result = ast_expr_pf_reduce(expr, s);
    if !(!result_iserror(res) && result_hasvalue(res) as libc::c_int != 0) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return irreducible_assume(value_as_sync(result_as_value(res)), value, s);
}
unsafe fn identifier_assume(
    mut expr: *mut ast_expr,
    mut value: bool,
    mut s: *mut state,
) -> *mut preresult {
    let mut s_copy: *mut state = state_copy(s);
    let mut res: *mut result = ast_expr_eval(expr, s_copy);
    if !(!result_iserror(res) && result_hasvalue(res) as libc::c_int != 0) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    state_destroy(s_copy);
    return irreducible_assume(value_as_sync(result_as_value(res)), value, s);
}
unsafe fn binary_deref_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut res: *mut result = ast_expr_eval(ast_expr_binary_e1(expr), state);
    if result_iserror(res) {
        return res;
    }
    let mut arr: *mut value = result_as_value(res);
    if arr.is_null() {
        panic!();
    }
    let mut deref_res: object_res = state_deref(state, arr, ast_expr_binary_e2(expr));
    if !(deref_res.err).is_null() {
        return result_error_create(deref_res.err);
    }
    if (deref_res.obj).is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        let mut s: *mut libc::c_char = ast_expr_str(expr);
        strbuilder_printf(
            b,
            b"undefined indirection: *(%s) has no value\0" as *const u8 as *const libc::c_char,
            s,
        );
        free(s as *mut libc::c_void);
        return result_error_create(error_create(strbuilder_build(b)));
    }
    result_destroy(res);
    let mut v: *mut value = object_as_value(deref_res.obj);
    if v.is_null() {
        let mut b_0: *mut strbuilder = strbuilder_create();
        let mut s_0: *mut libc::c_char = ast_expr_str(expr);
        strbuilder_printf(
            b_0,
            b"undefined indirection: *(%s) has no value\0" as *const u8 as *const libc::c_char,
            s_0,
        );
        free(s_0 as *mut libc::c_void);
        return result_error_create(error_create(strbuilder_build(b_0)));
    }
    return result_value_create(value_copy(v));
}
unsafe fn hack_identifier_builtin_eval(
    mut id: *mut libc::c_char,
    mut state: *mut state,
) -> *mut result {
    if !(state_getvconst(state, id)).is_null()
        || strncmp(id, b"ptr:\0" as *const u8 as *const libc::c_char, 4) == 0 as libc::c_int
    {
        return result_value_create(value_sync_create(ast_expr_identifier_create(dynamic_str(
            id,
        ))));
    }
    return result_error_create(error_create(
        b"not built-in\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    ));
}

pub unsafe fn ast_expr_isdeallocand_create(mut assertand: *mut ast_expr) -> *mut ast_expr {
    let mut new: *mut ast_expr = ast_expr_create();
    (*new).kind = EXPR_ISDEALLOCAND;
    (*new).root = assertand;
    return new;
}

pub unsafe fn ast_expr_assignment_create(
    mut root: *mut ast_expr,
    mut value: *mut ast_expr,
) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_ASSIGNMENT;
    (*expr).root = root;
    (*expr).u.assignment_value = value;
    return expr;
}
unsafe fn ast_expr_bracketed_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"(%s)\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}
unsafe fn expr_to_binary(mut expr: *mut ast_expr) -> *mut ast_expr {
    match ast_expr_kind(expr) as libc::c_uint {
        512 => return ast_expr_copy(expr),
        _ => {
            return ast_expr_binary_create(
                ast_expr_copy(expr),
                BINARY_OP_ADDITION,
                ast_expr_constant_create(0 as libc::c_int),
            );
        }
    };
}
unsafe fn expr_identifier_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut res: *mut result = hack_identifier_builtin_eval(ast_expr_as_identifier(expr), state);
    if !result_iserror(res) && result_hasvalue(res) as libc::c_int != 0 {
        return res;
    }
    let mut id: *mut libc::c_char = ast_expr_as_identifier(expr);
    if *id.offset(0 as libc::c_int as isize) as libc::c_int == '#' as i32 {
        return result_value_create(value_literal_create(id));
    }
    let mut obj: *mut object = state_getobject(state, id);
    if obj.is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"unknown idenitfier %s\0" as *const u8 as *const libc::c_char,
            id,
        );
        return result_error_create(error_create(strbuilder_build(b)));
    }
    let mut val: *mut value = object_as_value(obj);
    if val.is_null() {
        v_printf(
            b"state: %s\n\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
            state_str(state),
        );
        let mut b_0: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b_0,
            b"undefined memory access: %s has no value\0" as *const u8 as *const libc::c_char,
            id,
        );
        return result_error_create(error_create(strbuilder_build(b_0)));
    }
    return result_value_create(value_copy(val));
}
unsafe fn expr_structmember_eval(mut expr: *mut ast_expr, mut s: *mut state) -> *mut result {
    let mut root: *mut ast_expr = ast_expr_member_root(expr);
    let mut res: *mut result = ast_expr_eval(root, s);
    if result_iserror(res) {
        return res;
    }
    let mut field: *mut libc::c_char = ast_expr_member_field(expr);
    let mut member: *mut object = value_struct_member(result_as_value(res), field);
    if member.is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        let mut root_str: *mut libc::c_char = ast_expr_str(root);
        strbuilder_printf(
            b,
            b"`%s' has no field `%s'\0" as *const u8 as *const libc::c_char,
            root_str,
            field,
        );
        free(root_str as *mut libc::c_void);
        return result_error_create(error_create(strbuilder_build(b)));
    }
    let mut obj_value: *mut value = object_as_value(member);
    let mut v: *mut value = if !obj_value.is_null() {
        value_copy(obj_value)
    } else {
        0 as *mut value
    };
    result_destroy(res);
    return result_value_create(v);
}
unsafe fn address_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut operand: *mut ast_expr = ast_expr_unary_operand(expr);
    let mut id: *mut libc::c_char = ast_expr_as_identifier(operand);
    let mut v: *mut value = state_getloc(state, id);
    return result_value_create(v);
}

pub unsafe fn ast_expr_alloc_rangeprocess(
    mut alloc: *mut ast_expr,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut result_lw: *mut result = ast_expr_eval(lw, state);
    let mut result_up: *mut result = ast_expr_eval(up, state);
    if result_iserror(result_lw) {
        return result_as_error(result_lw);
    }
    if result_iserror(result_up) {
        return result_as_error(result_up);
    }
    let mut res_lw: *mut ast_expr = value_to_expr(result_as_value(result_lw));
    let mut res_up: *mut ast_expr = value_to_expr(result_as_value(result_up));
    result_destroy(result_up);
    result_destroy(result_lw);
    match (*alloc).kind {
        1024 => {
            err = rangeprocess_alloc(alloc, res_lw, res_up, state);
        }
        16384 => {
            err = rangeprocess_dealloc(alloc, res_lw, res_up, state);
        }
        _ => {
            panic!();
        }
    }
    ast_expr_destroy(res_up);
    ast_expr_destroy(res_lw);
    if !err.is_null() {
        return err;
    }
    return 0 as *mut error;
}

pub unsafe fn ast_expr_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    match ast_expr_kind(expr) {
        2 => return expr_constant_eval(expr, state),
        4 => return expr_literal_eval(expr, state),
        1 => return expr_identifier_eval(expr, state),
        256 => return expr_unary_eval(expr, state),
        128 => return expr_structmember_eval(expr, state),
        32 => return expr_call_eval(expr, state),
        1024 => return expr_assign_eval(expr, state),
        64 => return expr_incdec_eval(expr, state),
        512 => return expr_binary_eval(expr, state),
        8192 => return arbarg_eval(expr, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn arbarg_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    return result_value_create(state_vconst(
        state,
        ast_type_create_ptr(ast_type_create(TYPE_VOID, 0)),
        0 as *mut libc::c_char,
        0 as libc::c_int != 0,
    ));
}
unsafe fn expr_binary_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut e1: *mut ast_expr = ast_expr_binary_e1(expr);
    let mut e2: *mut ast_expr = ast_expr_binary_e2(expr);
    let mut res1: *mut result = ast_expr_eval(e1, state);
    let mut res2: *mut result = ast_expr_eval(e2, state);
    if result_iserror(res1) {
        return res1;
    }
    if result_iserror(res2) {
        return res2;
    }
    return result_value_create(value_sync_create(ast_expr_binary_create(
        value_to_expr(result_as_value(res1)),
        ast_expr_binary_op(expr),
        value_to_expr(result_as_value(res2)),
    )));
}
unsafe fn expr_incdec_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut assign: *mut ast_expr = ast_expr_incdec_to_assignment(expr);
    let mut res: *mut result = 0 as *mut result;
    if ast_expr_incdec_pre(expr) {
        res = expr_assign_eval(assign, state);
    } else {
        res = ast_expr_eval(ast_expr_incdec_root(expr), state);
        result_destroy(expr_assign_eval(assign, state));
    }
    ast_expr_destroy(assign);
    return res;
}
unsafe fn ast_expr_destroy_literal(mut expr: *mut ast_expr) {
    if !((*expr).kind == EXPR_STRING_LITERAL) {
        panic!();
    }
    free((*expr).u.string as *mut libc::c_void);
}
unsafe fn ast_expr_destroy_identifier(mut expr: *mut ast_expr) {
    if !((*expr).kind == EXPR_IDENTIFIER) {
        panic!();
    }
    free((*expr).u.string as *mut libc::c_void);
}
unsafe fn expr_assign_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut lval: *mut ast_expr = ast_expr_assignment_lval(expr);
    let mut rval: *mut ast_expr = ast_expr_assignment_rval(expr);
    let mut res: *mut result = ast_expr_eval(rval, state);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        debug_assert!(false);
        return result_error_create(error_create(
            b"undefined indirection (rvalue)\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    let mut lval_res: lvalue_res = ast_expr_lvalue(lval, state);
    if !(lval_res.err).is_null() {
        return result_error_create(lval_res.err);
    }
    let mut obj: *mut object = lvalue_object(lval_res.lval);
    if obj.is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        let mut s: *mut libc::c_char = ast_expr_str(lval);
        strbuilder_printf(
            b,
            b"undefined indirection: %s is not an lvalue\0" as *const u8 as *const libc::c_char,
            s,
        );
        free(s as *mut libc::c_void);
        return result_error_create(error_create(strbuilder_build(b)));
    }
    object_assign(obj, value_copy(result_as_value(res)));
    return res;
}

pub unsafe fn ast_expr_lvalue(mut expr: *mut ast_expr, mut state: *mut state) -> lvalue_res {
    match ast_expr_kind(expr) {
        1 => return expr_identifier_lvalue(expr, state),
        256 => return expr_unary_lvalue(expr, state),
        128 => return expr_structmember_lvalue(expr, state),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn expr_structmember_lvalue(
    mut expr: *mut ast_expr,
    mut state: *mut state,
) -> lvalue_res {
    let mut root: *mut ast_expr = ast_expr_member_root(expr);
    let mut root_res: lvalue_res = ast_expr_lvalue(root, state);
    let mut root_obj: *mut object = lvalue_object(root_res.lval);
    if root_obj.is_null() {
        panic!();
    }
    let mut field: *mut libc::c_char = ast_expr_member_field(expr);
    let mut member: *mut object =
        object_getmember(root_obj, lvalue_type(root_res.lval), field, state);
    if member.is_null() {
        return {
            let mut init = lvalue_res {
                lval: 0 as *mut lvalue,
                err: error_create(
                    b"lvalue error\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
                ),
            };
            init
        };
    }
    let mut t: *mut ast_type =
        object_getmembertype(root_obj, lvalue_type(root_res.lval), field, state);
    if t.is_null() {
        panic!();
    }
    return {
        let mut init = lvalue_res {
            lval: lvalue_create(t, member),
            err: 0 as *mut error,
        };
        init
    };
}

pub unsafe fn expr_unary_lvalue(mut expr: *mut ast_expr, mut state: *mut state) -> lvalue_res {
    if !(ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE) {
        panic!();
    }
    let mut inner: *mut ast_expr = ast_expr_unary_operand(expr);
    if ast_expr_kind(inner) == EXPR_IDENTIFIER {
        let mut root_res: lvalue_res = ast_expr_lvalue(inner, state);
        if !(root_res.err).is_null() {
            return root_res;
        }
        let mut root_obj: *mut object = lvalue_object(root_res.lval);
        if root_obj.is_null() {
            return {
                let mut init = lvalue_res {
                    lval: 0 as *mut lvalue,
                    err: 0 as *mut error,
                };
                init
            };
        }
        let mut t: *mut ast_type = ast_type_ptr_type(lvalue_type(root_res.lval));
        let mut root_val: *mut value = object_as_value(root_obj);
        if root_val.is_null() {
            panic!();
        }
        let mut res: object_res =
            state_deref(state, root_val, ast_expr_constant_create(0 as libc::c_int));
        if !(res.err).is_null() {
            return {
                let mut init = lvalue_res {
                    lval: 0 as *mut lvalue,
                    err: res.err,
                };
                init
            };
        }
        return {
            let mut init = lvalue_res {
                lval: lvalue_create(t, res.obj),
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut root_res_0: lvalue_res = ast_expr_lvalue(ast_expr_binary_e1(inner), state);
    if !(root_res_0.err).is_null() {
        return root_res_0;
    }
    let mut root_obj_0: *mut object = lvalue_object(root_res_0.lval);
    if root_obj_0.is_null() {
        return {
            let mut init = lvalue_res {
                lval: 0 as *mut lvalue,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut t_0: *mut ast_type = ast_type_ptr_type(lvalue_type(root_res_0.lval));
    let mut root_val_0: *mut value = object_as_value(root_obj_0);
    if root_val_0.is_null() {
        panic!();
    }
    let mut res_0: object_res = state_deref(state, root_val_0, ast_expr_binary_e2(inner));
    if !(res_0.err).is_null() {
        return {
            let mut init = lvalue_res {
                lval: 0 as *mut lvalue,
                err: 0 as *mut error,
            };
            init
        };
    }
    return {
        let mut init = lvalue_res {
            lval: lvalue_create(t_0, res_0.obj),
            err: 0 as *mut error,
        };
        init
    };
}

pub unsafe fn expr_identifier_lvalue(mut expr: *mut ast_expr, mut state: *mut state) -> lvalue_res {
    let mut id: *mut libc::c_char = ast_expr_as_identifier(expr);
    return {
        let mut init = lvalue_res {
            lval: lvalue_create(state_getobjecttype(state, id), state_getobject(state, id)),
            err: 0 as *mut error,
        };
        init
    };
}
unsafe fn dereference_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut binary: *mut ast_expr = expr_to_binary(ast_expr_unary_operand(expr));
    let mut res: *mut result = binary_deref_eval(binary, state);
    ast_expr_destroy(binary);
    return res;
}
unsafe fn ast_expr_constant_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut constant: libc::c_int = (*expr).u.constant.constant;
    if !(*expr).u.constant.ischar {
        strbuilder_printf(b, b"%d\0" as *const u8 as *const libc::c_char, constant);
        return;
    }
    match constant {
        10 | 9 | 11 | 8 | 12 | 7 | 92 | 63 | 39 | 34 | 0 => {
            strbuilder_printf(
                b,
                b"'%s'\0" as *const u8 as *const libc::c_char,
                escape_str(constant as libc::c_char),
            );
        }
        _ => {
            strbuilder_printf(b, b"'%c'\0" as *const u8 as *const libc::c_char, constant);
        }
    };
}
unsafe fn escape_str(mut c: libc::c_char) -> *mut libc::c_char {
    match c as libc::c_int {
        10 => return b"\\n\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        9 => return b"\\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        11 => return b"\\v\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        8 => return b"\\b\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        12 => return b"\\f\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        7 => return b"\\a\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        92 => return b"\\\\\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        63 => return b"\\?\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        39 => return b"\\'\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        34 => return b"\\\"\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        0 => return b"\\0\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        _ => {
            panic!();
        }
    }
}
unsafe fn expr_call_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut err: *mut error = 0 as *mut error;
    let mut root: *mut ast_expr = ast_expr_call_root(expr);
    let mut name: *mut libc::c_char = ast_expr_as_identifier(root);
    let mut f: *mut ast_function = externals_getfunc(state_getext(state), name);
    if f.is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        return result_error_create(error_create(strbuilder_build(b)));
    }
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut params: *mut *mut ast_variable = ast_function_params(f);
    let mut rtype: *mut ast_type = ast_function_type(f);
    let mut args = prepare_arguments(
        ast_expr_call_nargs(expr),
        ast_expr_call_args(expr),
        nparams,
        params,
        state,
    );
    state_pushframe(state, dynamic_str(name), rtype);
    err = prepare_parameters(nparams, params, &args, name, state);
    if !err.is_null() {
        return result_error_create(err);
    }
    err = call_setupverify(f, state_copy(state));
    if !err.is_null() {
        return result_error_create(err);
    }
    let mut res: *mut result = call_absexec(expr, state);
    if result_iserror(res) {
        return res;
    }
    let mut v: *mut value = 0 as *mut value;
    if result_hasvalue(res) {
        v = value_copy(result_as_value(res));
    }
    state_popframe(state);
    if !v.is_null() {
        return pf_augment(v, expr, state);
    }
    return res;
}
unsafe fn call_absexec(mut expr: *mut ast_expr, mut s: *mut state) -> *mut result {
    let mut root: *mut ast_expr = ast_expr_call_root(expr);
    let mut name: *mut libc::c_char = ast_expr_as_identifier(root);
    let mut f: *mut ast_function = externals_getfunc(state_getext(s), name);
    if f.is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        return result_error_create(error_create(strbuilder_build(b)));
    }
    let mut res: *mut result = ast_function_absexec(f, s);
    if result_iserror(res) as libc::c_int != 0 || result_hasvalue(res) as libc::c_int != 0 {
        return res;
    }
    return call_arbitraryresult(expr, f, s);
}
unsafe fn call_arbitraryresult(
    mut expr: *mut ast_expr,
    mut f: *mut ast_function,
    mut state: *mut state,
) -> *mut result {
    let mut res: *mut result = call_to_computed_value(f, state);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        panic!();
    }
    return res;
}
unsafe fn call_to_computed_value(mut f: *mut ast_function, mut s: *mut state) -> *mut result {
    let mut root: *mut libc::c_char = ast_function_name(f);
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut uncomputed_param: *mut *mut ast_variable = ast_function_params(f);
    let mut computed_param: *mut *mut ast_expr =
        malloc((::core::mem::size_of::<*mut ast_expr>()).wrapping_mul(nparams as usize))
            as *mut *mut ast_expr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nparams {
        let mut param: *mut ast_expr = ast_expr_identifier_create(dynamic_str(ast_variable_name(
            *uncomputed_param.offset(i as isize),
        )));
        let mut res: *mut result = ast_expr_eval(param, s);
        ast_expr_destroy(param);
        if result_iserror(res) {
            return res;
        }
        if !result_hasvalue(res) {
            panic!();
        }
        let mut v: *mut value = result_as_value(res);
        if value_islocation(v) {
            let ref mut fresh0 = *computed_param.offset(i as isize);
            *fresh0 = ast_expr_identifier_create(value_str(v));
        } else {
            let ref mut fresh1 = *computed_param.offset(i as isize);
            *fresh1 = value_to_expr(v);
        }
        i += 1;
    }
    return result_value_create(value_sync_create(ast_expr_call_create(
        ast_expr_identifier_create(dynamic_str(root)),
        nparams,
        computed_param,
    )));
}

pub unsafe fn ast_expr_absexec(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    match ast_expr_kind(expr) {
        1024 => return assign_absexec(expr, state),
        4096 => return isdereferencable_absexec(expr, state),
        16384 => return alloc_absexec(expr, state),
        1 | 2 | 256 | 32 | 128 | 8192 => return ast_expr_eval(expr, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn expr_unary_eval(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    match ast_expr_unary_op(expr) {
        2 => return dereference_eval(expr, state),
        1 => return address_eval(expr, state),
        32 => {
            return result_value_create(value_literal_create(
                b"hack\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
            ));
        }
        _ => {
            panic!();
        }
    }
}
unsafe fn alloc_absexec(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    match ast_expr_alloc_kind(expr) {
        1 => return result_value_create(state_alloc(state)),
        2 => return dealloc_process(expr, state),
        4 => return result_value_create(state_clump(state)),
        _ => {
            panic!();
        }
    }
}
unsafe fn dealloc_process(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut arg: *mut ast_expr = ast_expr_alloc_arg(expr);
    let mut res: *mut result = ast_expr_eval(arg, state);
    if result_iserror(res) {
        return res;
    }
    let mut val: *mut value = result_as_value(res);
    if val.is_null() {
        panic!();
    }
    let mut err: *mut error = state_dealloc(state, val);
    if !err.is_null() {
        return result_error_create(err);
    }
    value_destroy(val);
    return result_value_create(0 as *mut value);
}

pub unsafe fn prepare_parameters(
    mut nparams: libc::c_int,
    mut param: *mut *mut ast_variable,
    mut args: &[*mut result],
    mut fname: *mut libc::c_char,
    mut state: *mut state,
) -> *mut error {
    assert_eq!(nparams as usize, args.len());
    let mut i: libc::c_int = 0 as libc::c_int;
    while (i as usize) < args.len() {
        state_declare(state, *param.offset(i as isize), 1 as libc::c_int != 0);
        let mut res: *mut result = args[i as usize];
        if result_iserror(res) {
            return result_as_error(res);
        }
        if !result_hasvalue(res) {
            let mut b: *mut strbuilder = strbuilder_create();
            strbuilder_printf(
                b,
                b"parameter `%s' of function `%s' has no value\0" as *const u8
                    as *const libc::c_char,
                ast_variable_name(*param.offset(i as isize)),
                fname,
            );
            return error_create(strbuilder_build(b));
        }
        let mut name: *mut ast_expr =
            ast_expr_identifier_create(dynamic_str(ast_variable_name(*param.offset(i as isize))));
        let mut lval_res: lvalue_res = ast_expr_lvalue(name, state);
        if !(lval_res.err).is_null() {
            return lval_res.err;
        }
        let mut obj: *mut object = lvalue_object(lval_res.lval);
        ast_expr_destroy(name);
        object_assign(obj, value_copy(result_as_value(res)));
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn isdereferencable_absexec(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut p: *mut props = state_getprops(state);
    props_install(p, expr);
    return result_value_create(0 as *mut value);
}

pub unsafe fn prepare_arguments(
    mut nargs: libc::c_int,
    mut arg: *mut *mut ast_expr,
    mut nparams: libc::c_int,
    mut param: *mut *mut ast_variable,
    mut state: *mut state,
) -> Vec<*mut result> {
    assert_eq!(nargs, nparams);
    let mut args = vec![];
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nargs {
        args.push(ast_expr_eval(*arg.offset(i as isize), state));
        i += 1;
    }
    args
}

unsafe fn assign_absexec(mut expr: *mut ast_expr, mut state: *mut state) -> *mut result {
    let mut lval: *mut ast_expr = ast_expr_assignment_lval(expr);
    let mut rval: *mut ast_expr = ast_expr_assignment_rval(expr);
    let mut res: *mut result = ast_expr_absexec(rval, state);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        debug_assert!(false);
        return result_error_create(error_create(
            b"undefined indirection (rvalue)\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    let mut lval_res: lvalue_res = ast_expr_lvalue(lval, state);
    if !(lval_res.err).is_null() {
        return result_error_create(lval_res.err);
    }
    let mut obj: *mut object = lvalue_object(lval_res.lval);
    if obj.is_null() {
        return result_error_create(error_create(
            b"undefined indirection (lvalue)\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    object_assign(obj, value_copy(result_as_value(res)));
    return res;
}

unsafe fn verify_paramspec(
    mut param: *mut value,
    mut arg: *mut value,
    mut param_state: *mut state,
    mut arg_state: *mut state,
) -> *mut error {
    if !state_islval(param_state, param) {
        return 0 as *mut error;
    }
    if !state_islval(arg_state, arg) {
        return error_create(
            b"must be lvalue\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    if state_isalloc(param_state, param) as libc::c_int != 0 && !state_isalloc(arg_state, arg) {
        return error_create(
            b"must be heap allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut param_res: object_res =
        state_get(param_state, value_as_location(param), 0 as libc::c_int != 0);
    if !(param_res.err).is_null() {
        return param_res.err;
    }
    let mut arg_res: object_res =
        state_get(arg_state, value_as_location(arg), 0 as libc::c_int != 0);
    if !(arg_res.err).is_null() {
        return arg_res.err;
    }
    if (param_res.obj).is_null() {
        panic!();
    }
    if (arg_res.obj).is_null() {
        panic!();
    }
    if !object_hasvalue(param_res.obj) {
        return 0 as *mut error;
    }
    if !object_hasvalue(arg_res.obj) {
        return error_create(
            b"must be rvalue\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return verify_paramspec(
        object_as_value(param_res.obj),
        object_as_value(arg_res.obj),
        param_state,
        arg_state,
    );
}
unsafe fn call_setupverify(mut f: *mut ast_function, mut arg_state: *mut state) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut fname: *mut libc::c_char = ast_function_name(f);
    let mut param_state: *mut state = state_create(
        dynamic_str(fname),
        state_getext(arg_state),
        ast_function_type(f),
    );
    err = ast_function_initparams(f, param_state);
    if !err.is_null() {
        return err;
    }
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut param: *mut *mut ast_variable = ast_function_params(f);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nparams {
        let mut id: *mut libc::c_char = ast_variable_name(*param.offset(i as isize));
        let mut param_0: *mut value = state_getloc(param_state, id);
        let mut arg: *mut value = state_getloc(arg_state, id);
        err = verify_paramspec(param_0, arg, param_state, arg_state);
        if !err.is_null() {
            let mut b: *mut strbuilder = strbuilder_create();
            strbuilder_printf(
                b,
                b"parameter %s of %s %s\0" as *const u8 as *const libc::c_char,
                id,
                fname,
                (*err).msg,
            );
            return error_create(strbuilder_build(b));
        }
        i += 1;
    }
    return 0 as *mut error;
}

pub unsafe fn ast_expr_bracketed_root(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_BRACKETED) {
        panic!();
    }
    return (*expr).root;
}

pub unsafe fn ast_expr_bracketed_create(mut root: *mut ast_expr) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_BRACKETED;
    (*expr).root = root;
    return expr;
}

pub unsafe fn ast_expr_literal_create(mut s: *mut libc::c_char) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_STRING_LITERAL;
    (*expr).u.string = s;
    return expr;
}

pub unsafe fn ast_expr_as_literal(mut expr: *mut ast_expr) -> *mut libc::c_char {
    if !((*expr).kind == EXPR_STRING_LITERAL) {
        panic!();
    }
    return (*expr).u.string;
}

pub unsafe fn ast_expr_as_constant(mut expr: *mut ast_expr) -> libc::c_int {
    if !((*expr).kind == EXPR_CONSTANT) {
        panic!();
    }
    return (*expr).u.constant.constant;
}
unsafe fn pf_augment(
    mut v: *mut value,
    mut call: *mut ast_expr,
    mut state: *mut state,
) -> *mut result {
    if !value_isstruct(v) {
        return result_value_create(value_copy(v));
    }
    let mut res: *mut result = ast_expr_pf_reduce(call, state);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        panic!();
    }
    return result_value_create(value_pf_augment(v, value_as_sync(result_as_value(res))));
}

pub unsafe fn ast_expr_constant_create_char(mut c: libc::c_char) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_CONSTANT;
    (*expr).u.constant.ischar = 1 as libc::c_int != 0;
    (*expr).u.constant.constant = c as libc::c_int;
    return expr;
}

pub unsafe fn ast_expr_constant_create(mut k: libc::c_int) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_CONSTANT;
    (*expr).u.constant.ischar = 0 as libc::c_int != 0;
    (*expr).u.constant.constant = k;
    return expr;
}

pub unsafe fn ast_expr_as_identifier(mut expr: *mut ast_expr) -> *mut libc::c_char {
    if !((*expr).kind == EXPR_IDENTIFIER) {
        panic!();
    }
    return (*expr).u.string;
}
unsafe fn ast_expr_create() -> *mut ast_expr {
    return malloc(::core::mem::size_of::<ast_expr>()) as *mut ast_expr;
}

pub unsafe fn ast_expr_identifier_create(mut s: *mut libc::c_char) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_IDENTIFIER;
    (*expr).u.string = s;
    return expr;
}
unsafe fn unary_pf_reduce(mut e: *mut ast_expr, mut s: *mut state) -> *mut result {
    let mut res: *mut result = ast_expr_pf_reduce(ast_expr_unary_operand(e), s);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        panic!();
    }
    return result_value_create(value_sync_create(ast_expr_unary_create(
        value_as_sync(result_as_value(res)),
        ast_expr_unary_op(e),
    )));
}
unsafe fn binary_pf_reduce(
    mut e1: *mut ast_expr,
    mut op: ast_binary_operator,
    mut e2: *mut ast_expr,
    mut s: *mut state,
) -> *mut result {
    let mut res1: *mut result = ast_expr_pf_reduce(e1, s);
    if result_iserror(res1) {
        return res1;
    }
    if !result_hasvalue(res1) {
        panic!();
    }
    let mut res2: *mut result = ast_expr_pf_reduce(e2, s);
    if result_iserror(res2) {
        return res2;
    }
    if !result_hasvalue(res2) {
        panic!();
    }
    return result_value_create(value_sync_create(ast_expr_binary_create(
        value_to_expr(result_as_value(res1)),
        op,
        value_to_expr(result_as_value(res2)),
    )));
}
unsafe fn call_pf_reduce(mut e: *mut ast_expr, mut s: *mut state) -> *mut result {
    let mut root: *mut libc::c_char = ast_expr_as_identifier(ast_expr_call_root(e));
    let mut nargs: libc::c_int = ast_expr_call_nargs(e);
    let mut unreduced_arg: *mut *mut ast_expr = ast_expr_call_args(e);
    let mut reduced_arg: *mut *mut ast_expr =
        malloc((::core::mem::size_of::<*mut ast_expr>()).wrapping_mul(nargs as usize))
            as *mut *mut ast_expr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nargs {
        let mut res: *mut result = ast_expr_pf_reduce(*unreduced_arg.offset(i as isize), s);
        if result_iserror(res) {
            return res;
        }
        if !result_hasvalue(res) {
            panic!();
        }
        let ref mut fresh3 = *reduced_arg.offset(i as isize);
        *fresh3 = ast_expr_copy(value_to_expr(result_as_value(res)));
        i += 1;
    }
    return result_value_create(value_sync_create(ast_expr_call_create(
        ast_expr_identifier_create(dynamic_str(root)),
        nargs,
        reduced_arg,
    )));
}
unsafe fn structmember_pf_reduce(mut expr: *mut ast_expr, mut s: *mut state) -> *mut result {
    let mut res: *mut result = ast_expr_pf_reduce(ast_expr_member_root(expr), s);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        panic!();
    }
    let mut field: *mut libc::c_char = ast_expr_member_field(expr);
    let mut v: *mut value = result_as_value(res);
    if value_isstruct(v) {
        let mut obj: *mut object = value_struct_member(v, field);
        let mut obj_value: *mut value = object_as_value(obj);
        if obj_value.is_null() {
            panic!();
        }
        return result_value_create(value_copy(obj_value));
    }
    if !value_issync(v) {
        panic!();
    }
    return result_value_create(value_sync_create(ast_expr_member_create(
        value_as_sync(v),
        dynamic_str(field),
    )));
}

pub unsafe fn ast_expr_pf_reduce(mut e: *mut ast_expr, mut s: *mut state) -> *mut result {
    match ast_expr_kind(e) {
        2 | 4 | 1 => return ast_expr_eval(e, s),
        256 => return unary_pf_reduce(e, s),
        512 => {
            return binary_pf_reduce(
                ast_expr_binary_e1(e),
                ast_expr_binary_op(e),
                ast_expr_binary_e2(e),
                s,
            );
        }
        32 => return call_pf_reduce(e, s),
        128 => return structmember_pf_reduce(e, s),
        8 => return ast_expr_pf_reduce(ast_expr_bracketed_root(e), s),
        _ => {
            panic!();
        }
    }
}
unsafe fn ast_expr_destroy_unary(mut expr: *mut ast_expr) {
    if !((*expr).kind == EXPR_UNARY) {
        panic!();
    }
    ast_expr_destroy((*expr).root);
}
unsafe fn ast_expr_member_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut ast_expr = (*expr).root;
    if (*root).kind == EXPR_UNARY {
        return ast_expr_member_deref_str_build(root, (*expr).u.string, b);
    }
    let mut r: *mut libc::c_char = ast_expr_str(root);
    strbuilder_printf(
        b,
        b"%s.%s\0" as *const u8 as *const libc::c_char,
        r,
        (*expr).u.string,
    );
    free(r as *mut libc::c_void);
}
unsafe fn ast_expr_member_deref_str_build(
    mut root: *mut ast_expr,
    mut member: *mut libc::c_char,
    mut b: *mut strbuilder,
) {
    if !(ast_expr_unary_op(root) == UNARY_OP_DEREFERENCE) {
        panic!();
    }
    let mut inner: *mut ast_expr = ast_expr_unary_operand(root);
    let mut e1: *mut ast_expr = ast_expr_binary_e1(inner);
    let mut e2: *mut ast_expr = ast_expr_binary_e2(inner);
    let mut left: *mut libc::c_char = ast_expr_str(e1);
    if (*e2).kind == EXPR_CONSTANT && ast_expr_as_constant(e2) == 0 as libc::c_int {
        strbuilder_printf(
            b,
            b"%s->%s\0" as *const u8 as *const libc::c_char,
            left,
            member,
        );
    } else {
        let mut index: *mut libc::c_char = ast_expr_str(e2);
        strbuilder_printf(
            b,
            b"%s[%s].%s\0" as *const u8 as *const libc::c_char,
            left,
            index,
            member,
        );
        free(index as *mut libc::c_void);
    }
    free(left as *mut libc::c_void);
}
unsafe fn ast_expr_incdec_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    let mut op: *mut libc::c_char = (if (*expr).u.incdec.inc != 0 {
        b"++\0" as *const u8 as *const libc::c_char
    } else {
        b"--\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    if (*expr).u.incdec.pre != 0 {
        strbuilder_printf(b, b"%s%s\0" as *const u8 as *const libc::c_char, op, root);
    } else {
        strbuilder_printf(b, b"%s%s\0" as *const u8 as *const libc::c_char, root, op);
    }
    free(root as *mut libc::c_void);
}
unsafe fn ast_expr_call_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"%s(\0" as *const u8 as *const libc::c_char, root);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*expr).u.call.n {
        let mut arg: *mut libc::c_char = ast_expr_str(*((*expr).u.call.arg).offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%s\0" as *const u8 as *const libc::c_char,
            arg,
            if (i + 1 as libc::c_int) < (*expr).u.call.n {
                b", \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            },
        );
        free(arg as *mut libc::c_void);
        i += 1;
    }
    strbuilder_printf(b, b")\0" as *const u8 as *const libc::c_char);
    free(root as *mut libc::c_void);
}
unsafe fn ast_expr_destroy_call(mut expr: *mut ast_expr) {
    if !((*expr).kind == EXPR_CALL) {
        panic!();
    }
    ast_expr_destroy((*expr).root);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*expr).u.call.n {
        ast_expr_destroy(*((*expr).u.call.arg).offset(i as isize));
        i += 1;
    }
    free((*expr).u.call.arg as *mut libc::c_void);
}
unsafe fn ast_expr_destroy_incdec(mut expr: *mut ast_expr) {
    if !((*expr).kind == EXPR_INCDEC) {
        panic!();
    }
    ast_expr_destroy((*expr).root);
}

pub unsafe fn ast_expr_inverted_copy(mut expr: *mut ast_expr, mut invert: bool) -> *mut ast_expr {
    let mut copy: *mut ast_expr = ast_expr_copy(expr);
    return if invert as libc::c_int != 0 {
        ast_expr_unary_create(copy, UNARY_OP_BANG)
    } else {
        copy
    };
}

pub unsafe fn ast_expr_member_field(mut expr: *mut ast_expr) -> *mut libc::c_char {
    if !((*expr).kind == EXPR_STRUCTMEMBER) {
        panic!();
    }
    return (*expr).u.string;
}

pub unsafe fn ast_expr_member_root(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_STRUCTMEMBER) {
        panic!();
    }
    return (*expr).root;
}

pub unsafe fn ast_expr_incdec_pre(mut expr: *mut ast_expr) -> bool {
    if !((*expr).kind == EXPR_INCDEC) {
        panic!();
    }
    return (*expr).u.incdec.pre != 0;
}

pub unsafe fn ast_expr_incdec_root(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_INCDEC) {
        panic!();
    }
    return (*expr).root;
}
unsafe fn ast_expr_copy_call(mut expr: *mut ast_expr) -> *mut ast_expr {
    let mut arg: *mut *mut ast_expr =
        malloc((::core::mem::size_of::<*mut ast_expr>()).wrapping_mul((*expr).u.call.n as usize))
            as *mut *mut ast_expr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*expr).u.call.n {
        let ref mut fresh4 = *arg.offset(i as isize);
        *fresh4 = ast_expr_copy(*((*expr).u.call.arg).offset(i as isize));
        i += 1;
    }
    return ast_expr_call_create(ast_expr_copy((*expr).root), (*expr).u.call.n, arg);
}

pub unsafe fn ast_expr_member_create(
    mut _struct: *mut ast_expr,
    mut field: *mut libc::c_char,
) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_STRUCTMEMBER;
    (*expr).root = _struct;
    (*expr).u.string = field;
    return expr;
}

pub unsafe fn ast_expr_binary_create(
    mut e1: *mut ast_expr,
    mut op: ast_binary_operator,
    mut e2: *mut ast_expr,
) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_BINARY;
    (*expr).u.binary.e1 = e1;
    (*expr).u.binary.op = op;
    (*expr).u.binary.e2 = e2;
    return expr;
}

pub unsafe fn ast_expr_unary_create(
    mut root: *mut ast_expr,
    mut op: ast_unary_operator,
) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_UNARY;
    (*expr).root = root;
    (*expr).u.unary_op = op;
    return expr;
}

pub unsafe fn ast_expr_incdec_to_assignment(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_INCDEC) {
        panic!();
    }
    return ast_expr_assignment_create(
        ast_expr_copy((*expr).root),
        ast_expr_binary_create(
            ast_expr_copy((*expr).root),
            if (*expr).u.incdec.inc != 0 {
                BINARY_OP_ADDITION
            } else {
                BINARY_OP_SUBTRACTION
            },
            ast_expr_constant_create(1 as libc::c_int),
        ),
    );
}

pub unsafe fn ast_expr_incdec_create(
    mut root: *mut ast_expr,
    mut inc: bool,
    mut pre: bool,
) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_INCDEC;
    (*expr).root = root;
    (*expr).u.incdec.inc = inc as libc::c_int;
    (*expr).u.incdec.pre = pre as libc::c_int;
    return expr;
}

pub unsafe fn ast_expr_call_args(mut expr: *mut ast_expr) -> *mut *mut ast_expr {
    return (*expr).u.call.arg;
}

pub unsafe fn ast_expr_call_nargs(mut expr: *mut ast_expr) -> libc::c_int {
    return (*expr).u.call.n;
}

pub unsafe fn ast_expr_call_root(mut expr: *mut ast_expr) -> *mut ast_expr {
    return (*expr).root;
}

pub unsafe fn ast_expr_call_create(
    mut root: *mut ast_expr,
    mut narg: libc::c_int,
    mut arg: *mut *mut ast_expr,
) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_CALL;
    (*expr).root = root;
    (*expr).u.call.n = narg;
    (*expr).u.call.arg = arg;
    return expr;
}

pub unsafe fn ast_expr_iteration_create() -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_ITERATION;
    return expr;
}

pub unsafe fn ast_expr_alloc_kind(mut expr: *mut ast_expr) -> ast_alloc_kind {
    if !((*expr).kind == EXPR_ALLOCATION) {
        panic!();
    }
    return (*expr).u.alloc.kind;
}

pub unsafe fn ast_expr_alloc_arg(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_ALLOCATION) {
        panic!();
    }
    return (*expr).u.alloc.arg;
}

pub unsafe fn ast_expr_isisdereferencable(mut expr: *mut ast_expr) -> bool {
    return (*expr).kind == EXPR_ISDEREFERENCABLE;
}

pub unsafe fn ast_expr_dealloc_create(mut arg: *mut ast_expr) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_ALLOCATION;
    (*expr).u.alloc.kind = DEALLOC;
    (*expr).u.alloc.arg = arg;
    return expr;
}

pub unsafe fn ast_expr_str(mut expr: *mut ast_expr) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    match (*expr).kind {
        1 => {
            strbuilder_printf(b, (*expr).u.string);
        }
        2 => {
            ast_expr_constant_str_build(expr, b);
        }
        4 => {
            strbuilder_printf(
                b,
                b"\"%s\"\0" as *const u8 as *const libc::c_char,
                (*expr).u.string,
            );
        }
        8 => {
            ast_expr_bracketed_str_build(expr, b);
        }
        32 => {
            ast_expr_call_str_build(expr, b);
        }
        64 => {
            ast_expr_incdec_str_build(expr, b);
        }
        128 => {
            ast_expr_member_str_build(expr, b);
        }
        256 => {
            ast_expr_unary_str_build(expr, b);
        }
        512 => {
            ast_expr_binary_str_build(expr, b);
        }
        1024 => {
            ast_expr_assignment_str_build(expr, b);
        }
        2048 => {
            ast_expr_isdeallocand_str_build(expr, b);
        }
        4096 => {
            ast_expr_isdereferencable_str_build(expr, b);
        }
        8192 => {
            strbuilder_putc(b, '$' as i32 as libc::c_char);
        }
        16384 => {
            ast_expr_alloc_str_build(expr, b);
        }
        _ => {
            panic!();
        }
    }
    return strbuilder_build(b);
}
unsafe fn ast_expr_alloc_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    if !((*expr).kind == EXPR_ALLOCATION) {
        panic!();
    }
    let mut arg: *mut libc::c_char = ast_expr_str((*expr).u.alloc.arg);
    match (*expr).u.alloc.kind {
        1 => {
            strbuilder_printf(
                b,
                b".%s %s;\0" as *const u8 as *const libc::c_char,
                b"malloc\0" as *const u8 as *const libc::c_char,
                arg,
            );
        }
        2 => {
            strbuilder_printf(
                b,
                b".%s %s;\0" as *const u8 as *const libc::c_char,
                b"free\0" as *const u8 as *const libc::c_char,
                arg,
            );
        }
        4 => {
            strbuilder_printf(
                b,
                b".%s %s;\0" as *const u8 as *const libc::c_char,
                b"clump\0" as *const u8 as *const libc::c_char,
                arg,
            );
        }
        _ => {
            panic!();
        }
    }
    free(arg as *mut libc::c_void);
}

pub unsafe fn ast_expr_alloc_create(mut arg: *mut ast_expr) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_ALLOCATION;
    (*expr).u.alloc.kind = ALLOC;
    (*expr).u.alloc.arg = arg;
    return expr;
}

pub unsafe fn ast_expr_isdereferencable_assertand(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_ISDEREFERENCABLE) {
        panic!();
    }
    return (*expr).root;
}

pub unsafe fn ast_expr_isdeallocand_assertand(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind == EXPR_ISDEALLOCAND) {
        panic!();
    }
    return (*expr).root;
}
unsafe fn ast_expr_alloc_copy(mut expr: *mut ast_expr) -> *mut ast_expr {
    let mut arg: *mut ast_expr = ast_expr_copy((*expr).u.alloc.arg);
    match (*expr).u.alloc.kind {
        ALLOC => return ast_expr_alloc_create(arg),
        DEALLOC => return ast_expr_dealloc_create(arg),
        CLUMP => return ast_expr_clump_create(arg),
        _ => panic!(),
    }
}
unsafe fn ast_expr_isdereferencable_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"$%s\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

pub unsafe fn ast_expr_assignment_rval(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind as libc::c_uint == EXPR_ASSIGNMENT as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*expr).u.assignment_value;
}

pub unsafe fn ast_expr_copy(mut expr: *mut ast_expr) -> *mut ast_expr {
    if expr.is_null() {
        panic!();
    }
    match (*expr).kind as libc::c_uint {
        1 => return ast_expr_identifier_create(dynamic_str((*expr).u.string)),
        2 => {
            return if (*expr).u.constant.ischar as libc::c_int != 0 {
                ast_expr_constant_create_char((*expr).u.constant.constant as libc::c_char)
            } else {
                ast_expr_constant_create((*expr).u.constant.constant)
            };
        }
        4 => return ast_expr_literal_create(dynamic_str((*expr).u.string)),
        8 => return ast_expr_bracketed_create(ast_expr_copy((*expr).root)),
        32 => return ast_expr_copy_call(expr),
        64 => {
            return ast_expr_incdec_create(
                ast_expr_copy((*expr).root),
                (*expr).u.incdec.inc != 0,
                (*expr).u.incdec.pre != 0,
            );
        }
        128 => {
            return ast_expr_member_create(
                ast_expr_copy((*expr).root),
                dynamic_str((*expr).u.string),
            );
        }
        256 => {
            return ast_expr_unary_create(ast_expr_copy((*expr).root), (*expr).u.unary_op);
        }
        512 => {
            return ast_expr_binary_create(
                ast_expr_copy((*expr).u.binary.e1),
                (*expr).u.binary.op,
                ast_expr_copy((*expr).u.binary.e2),
            );
        }
        1024 => {
            return ast_expr_assignment_create(
                ast_expr_copy((*expr).root),
                ast_expr_copy((*expr).u.assignment_value),
            );
        }
        2048 => return ast_expr_isdeallocand_create(ast_expr_copy((*expr).root)),
        4096 => return ast_expr_isdereferencable_create(ast_expr_copy((*expr).root)),
        8192 => return ast_expr_arbarg_create(),
        16384 => return ast_expr_alloc_copy(expr),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_expr_assignment_lval(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind as libc::c_uint == EXPR_ASSIGNMENT as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*expr).root;
}

pub unsafe fn ast_expr_binary_e2(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind as libc::c_uint == EXPR_BINARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*expr).u.binary.e2;
}
unsafe fn ast_expr_isdeallocand_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"@%s\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}
unsafe fn ast_expr_assignment_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    let mut value: *mut libc::c_char = ast_expr_str((*expr).u.assignment_value);
    strbuilder_printf(
        b,
        b"%s = %s\0" as *const u8 as *const libc::c_char,
        root,
        value,
    );
    free(value as *mut libc::c_void);
    free(root as *mut libc::c_void);
}
unsafe fn ast_expr_binary_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut opstr: [*const libc::c_char; 129] = [
        0 as *const libc::c_char,
        b"==\0" as *const u8 as *const libc::c_char,
        b"!=\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        b"<\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b">\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"<=\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b">=\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"+\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"-\0" as *const u8 as *const libc::c_char,
    ];
    let mut e1: *mut libc::c_char = ast_expr_str((*expr).u.binary.e1);
    let mut e2: *mut libc::c_char = ast_expr_str((*expr).u.binary.e2);
    strbuilder_printf(
        b,
        b"%s%s%s\0" as *const u8 as *const libc::c_char,
        e1,
        opstr[(*expr).u.binary.op as usize],
        e2,
    );
    free(e1 as *mut libc::c_void);
    free(e2 as *mut libc::c_void);
}

pub unsafe fn ast_expr_binary_e1(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind as libc::c_uint == EXPR_BINARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*expr).u.binary.e1;
}

pub unsafe fn ast_expr_difference_create(
    mut e1: *mut ast_expr,
    mut e2: *mut ast_expr,
) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_SUBTRACTION, e2);
}

pub unsafe fn ast_expr_sum_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_ADDITION, e2);
}
unsafe fn ast_expr_unary_str_build(mut expr: *mut ast_expr, mut b: *mut strbuilder) {
    let mut op: ast_unary_operator = (*expr).u.unary_op;
    assert!(UNARY_OP_ADDRESS <= op && op <= UNARY_OP_BANG);
    let opchar: [libc::c_char; 33] = [
        0,
        '&' as i32 as libc::c_char,
        '*' as i32 as libc::c_char,
        0,
        '+' as i32 as libc::c_char,
        0,
        0,
        0,
        '-' as i32 as libc::c_char,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        '~' as i32 as libc::c_char,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        '!' as i32 as libc::c_char,
    ];
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(
        b,
        b"%c(%s)\0" as *const u8 as *const libc::c_char,
        opchar[op as usize] as libc::c_int,
        root,
    );
    free(root as *mut libc::c_void);
}

pub unsafe fn ast_expr_ge_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_GE, e2);
}

pub unsafe fn ast_expr_binary_op(mut expr: *mut ast_expr) -> ast_binary_operator {
    if !((*expr).kind as libc::c_uint == EXPR_BINARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*expr).u.binary.op;
}

pub unsafe fn ast_expr_destroy(mut expr: *mut ast_expr) {
    match (*expr).kind as libc::c_uint {
        1 => {
            ast_expr_destroy_identifier(expr);
        }
        4 => {
            ast_expr_destroy_literal(expr);
        }
        8 => {
            ast_expr_destroy((*expr).root);
        }
        32 => {
            ast_expr_destroy_call(expr);
        }
        64 => {
            ast_expr_destroy_incdec(expr);
        }
        128 => {
            ast_expr_destroy((*expr).root);
            free((*expr).u.string as *mut libc::c_void);
        }
        256 => {
            ast_expr_destroy_unary(expr);
        }
        512 => {
            ast_expr_destroy_binary(expr);
        }
        1024 => {
            ast_expr_destroy_assignment(expr);
        }
        2048 => {
            ast_expr_destroy((*expr).root);
        }
        4096 => {
            ast_expr_destroy((*expr).root);
        }
        2 | 8192 => {}
        16384 => {
            ast_expr_destroy((*expr).u.alloc.arg);
        }
        _ => {
            panic!();
        }
    }
    free(expr as *mut libc::c_void);
}

pub unsafe fn ast_expr_le_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_LE, e2);
}

pub unsafe fn ast_expr_clump_create(mut arg: *mut ast_expr) -> *mut ast_expr {
    let mut expr: *mut ast_expr = ast_expr_create();
    (*expr).kind = EXPR_ALLOCATION;
    (*expr).u.alloc.kind = CLUMP;
    (*expr).u.alloc.arg = arg;
    return expr;
}

pub unsafe fn ast_expr_gt_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_GT, e2);
}

pub unsafe fn ast_expr_lt_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_LT, e2);
}

pub unsafe fn ast_expr_ne_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_NE, e2);
}
unsafe fn ast_expr_destroy_binary(mut expr: *mut ast_expr) {
    if !((*expr).kind as libc::c_uint == EXPR_BINARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    ast_expr_destroy((*expr).u.binary.e1);
    ast_expr_destroy((*expr).u.binary.e2);
}

pub unsafe fn ast_expr_eq_create(mut e1: *mut ast_expr, mut e2: *mut ast_expr) -> *mut ast_expr {
    return ast_expr_binary_create(e1, BINARY_OP_EQ, e2);
}
unsafe fn ast_expr_destroy_assignment(mut expr: *mut ast_expr) {
    if !((*expr).kind as libc::c_uint == EXPR_ASSIGNMENT as libc::c_int as libc::c_uint) {
        panic!();
    }
    ast_expr_destroy((*expr).root);
    ast_expr_destroy((*expr).u.assignment_value);
}

pub unsafe fn ast_expr_unary_operand(mut expr: *mut ast_expr) -> *mut ast_expr {
    if !((*expr).kind as libc::c_uint == EXPR_UNARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*expr).root;
}

pub unsafe fn ast_expr_kind(mut expr: *mut ast_expr) -> ast_expr_kind {
    return (*expr).kind;
}

pub unsafe fn ast_expr_unary_isdereference(mut expr: *mut ast_expr) -> bool {
    if ast_expr_kind(expr) as libc::c_uint != EXPR_UNARY as libc::c_int as libc::c_uint {
        return 0 as libc::c_int != 0;
    }
    return ast_expr_unary_op(expr) == UNARY_OP_DEREFERENCE;
}

pub unsafe fn ast_expr_unary_op(mut expr: *mut ast_expr) -> ast_unary_operator {
    if !((*expr).kind as libc::c_uint == EXPR_UNARY as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*expr).u.unary_op;
}

pub unsafe fn ast_expr_getfuncs(mut expr: *mut ast_expr) -> Box<string_arr> {
    match (*expr).kind as libc::c_uint {
        1 | 2 | 4 | 128 | 2048 | 4096 | 8192 => return string_arr_create(),
        32 => ast_expr_call_getfuncs(expr),
        8 | 256 | 64 => ast_expr_getfuncs((*expr).root),
        1024 => string_arr_concat(
            &ast_expr_getfuncs((*expr).root),
            &ast_expr_getfuncs((*expr).u.assignment_value),
        ),
        512 => string_arr_concat(
            &ast_expr_getfuncs((*expr).u.binary.e1),
            &ast_expr_getfuncs((*expr).u.binary.e2),
        ),
        _ => panic!("invalid expr kind"),
    }
}

pub unsafe fn ast_expr_splits(mut e: *mut ast_expr, mut s: *mut state) -> ast_stmt_splits {
    match ast_expr_kind(e) as libc::c_uint {
        32 => return call_splits(e, s),
        1024 => return ast_expr_splits(ast_expr_assignment_rval(e), s),
        256 => return ast_expr_splits(ast_expr_unary_operand(e), s),
        512 => return binary_splits(e, s),
        64 => return ast_expr_splits(ast_expr_incdec_root(e), s),
        128 => return ast_expr_splits(ast_expr_member_root(e), s),
        2 | 1 | 4 | 8192 | 4096 | 16384 => {
            return {
                let mut init = ast_stmt_splits {
                    n: 0 as libc::c_int,
                    cond: 0 as *mut *mut ast_expr,
                    err: 0 as *mut error,
                };
                init
            };
        }
        _ => {
            panic!();
        }
    }
}
unsafe fn call_splits(mut expr: *mut ast_expr, mut state: *mut state) -> ast_stmt_splits {
    let mut err: *mut error = 0 as *mut error;
    let mut root: *mut ast_expr = ast_expr_call_root(expr);
    let mut name: *mut libc::c_char = ast_expr_as_identifier(root);
    let mut f: *mut ast_function = externals_getfunc(state_getext(state), name);
    if f.is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function: `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        err = error_create(strbuilder_build(b));
        return {
            let mut init = ast_stmt_splits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut ast_expr,
                err: err,
            };
            init
        };
    }
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut params: *mut *mut ast_variable = ast_function_params(f);
    let mut s_copy: *mut state = state_copy(state);
    let mut args = prepare_arguments(
        ast_expr_call_nargs(expr),
        ast_expr_call_args(expr),
        nparams,
        params,
        s_copy,
    );
    let mut ret_type: *mut ast_type = ast_function_type(f);
    state_pushframe(s_copy, dynamic_str(name), ret_type);
    err = prepare_parameters(nparams, params, &args, name, s_copy);
    if !err.is_null() {
        return {
            let mut init = ast_stmt_splits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut ast_expr,
                err: err,
            };
            init
        };
    }
    let mut n: libc::c_int = 0 as libc::c_int;
    let mut cond: *mut *mut ast_expr = 0 as *mut *mut ast_expr;
    let mut abs: *mut ast_block = ast_function_abstract(f);
    let mut ndecls: libc::c_int = ast_block_ndecls(abs);
    if ndecls != 0 {
        let mut var: *mut *mut ast_variable = ast_block_decls(abs);
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < ndecls {
            state_declare(s_copy, *var.offset(i as isize), 0 as libc::c_int != 0);
            i += 1;
        }
    }
    let mut nstmts: libc::c_int = ast_block_nstmts(abs);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts(abs);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nstmts {
        let mut splits: ast_stmt_splits = ast_stmt_splits(*stmt.offset(i_0 as isize), s_copy);
        let mut j: libc::c_int = 0 as libc::c_int;
        while j < splits.n {
            n += 1;
            cond = realloc(
                cond as *mut libc::c_void,
                (::core::mem::size_of::<*mut ast_expr>()).wrapping_mul(n as usize),
            ) as *mut *mut ast_expr;
            let ref mut fresh5 = *cond.offset((n - 1 as libc::c_int) as isize);
            *fresh5 = *(splits.cond).offset(j as isize);
            j += 1;
        }
        i_0 += 1;
    }
    state_popframe(s_copy);
    ast_stmt_splits {
        n,
        cond,
        err: 0 as *mut error,
    }
}
unsafe fn binary_splits(mut e: *mut ast_expr, mut s: *mut state) -> ast_stmt_splits {
    let mut s1: ast_stmt_splits = ast_expr_splits(ast_expr_binary_e1(e), s);
    let mut s2: ast_stmt_splits = ast_expr_splits(ast_expr_binary_e2(e), s);
    let mut n: libc::c_int = s1.n + s2.n;
    let mut cond: *mut *mut ast_expr =
        malloc((::core::mem::size_of::<*mut ast_expr>()).wrapping_mul(n as usize))
            as *mut *mut ast_expr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < s1.n {
        let ref mut fresh6 = *cond.offset(i as isize);
        *fresh6 = *(s1.cond).offset(i as isize);
        i += 1;
    }
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < s2.n {
        let ref mut fresh7 = *cond.offset((i_0 + s1.n) as isize);
        *fresh7 = *(s2.cond).offset(i_0 as isize);
        i_0 += 1;
    }
    return {
        let mut init = ast_stmt_splits {
            n: n,
            cond: cond,
            err: 0 as *mut error,
        };
        init
    };
}

unsafe fn ast_expr_call_getfuncs(mut expr: *mut ast_expr) -> Box<string_arr> {
    let mut res = string_arr_create();
    let mut root: *mut ast_expr = (*expr).root;
    if !((*root).kind as libc::c_uint == EXPR_IDENTIFIER as libc::c_int as libc::c_uint) {
        panic!();
    }
    string_arr_append(&mut res, dynamic_str((*root).u.string));
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*expr).u.call.n {
        res = string_arr_concat(
            &res,
            &ast_expr_getfuncs(*((*expr).u.call.arg).offset(i as isize)),
        );
        i += 1;
    }
    res
}

unsafe fn calculate_indegrees(mut g: &map) -> Box<map> {
    let mut indegrees = map::new();
    for key in g.keys() {
        let mut deps: *mut string_arr = g.get(key) as *mut string_arr;
        if (indegrees.get(key)).is_null() {
            indegrees.set(
                dynamic_str(key),
                dynamic_int(0 as libc::c_int) as *const libc::c_void,
            );
            let mut j: libc::c_int = 0 as libc::c_int;
            while j < (*deps).n {
                let mut dep_key: *mut libc::c_char = *((*deps).s).offset(j as isize);
                if (indegrees.get(dep_key)).is_null() {
                    indegrees.set(
                        dynamic_str(dep_key),
                        dynamic_int(0 as libc::c_int) as *const libc::c_void,
                    );
                }
                j += 1;
            }
        }
    }
    for key in indegrees.keys() {
        let mut n_arr: *mut string_arr = g.get(key) as *mut string_arr;
        if !n_arr.is_null() {
            let mut j_0: libc::c_int = 0 as libc::c_int;
            while j_0 < (*n_arr).n {
                let mut count: *mut libc::c_int = indegrees.get(key) as *mut libc::c_int;
                *count = *count + 1 as libc::c_int;
                j_0 += 1;
            }
        }
    }
    return indegrees;
}

unsafe fn dynamic_int(mut i: libc::c_int) -> *mut libc::c_int {
    let mut val: *mut libc::c_int =
        malloc(::core::mem::size_of::<libc::c_int>()) as *mut libc::c_int;
    *val = i;
    return val;
}

unsafe fn build_indegree_zero(mut indegrees: &map) -> Box<string_arr> {
    let mut indegree_zero = string_arr_create();
    for key in indegrees.keys() {
        let mut val: *mut libc::c_int = indegrees.get(key) as *mut libc::c_int;
        if *val == 0 as libc::c_int {
            string_arr_append(&mut indegree_zero, dynamic_str(key));
        }
    }
    indegree_zero
}

pub unsafe fn topological_order(
    mut fname: *mut libc::c_char,
    mut ext: *mut externals,
) -> Box<string_arr> {
    let mut order = string_arr_create();
    let mut g = ast_function_buildgraph(fname, ext);
    let mut indegrees = calculate_indegrees(&g);
    let mut indegree_zero = build_indegree_zero(&indegrees);
    while (*indegree_zero).n > 0 as libc::c_int {
        let mut curr: *mut libc::c_char = string_arr_deque(&mut indegree_zero);
        string_arr_append(&mut order, curr);
        for key in (*g).keys() {
            let mut v: *mut string_arr = g.get(key) as *mut string_arr;
            if string_arr_contains(&*v, curr) {
                let mut count: *mut libc::c_int = indegrees.get(key) as *mut libc::c_int;
                *count = *count - 1 as libc::c_int;
                if *count == 0 as libc::c_int {
                    string_arr_append(&mut indegree_zero, dynamic_str(key));
                }
            }
        }
    }
    if order.n != indegrees.len() {
        fprintf(
            __stderrp,
            b"cycle detected in graph\0" as *const u8 as *const libc::c_char,
        );
        exit(1 as libc::c_int);
    }
    order
}

pub unsafe fn ast_block_create(
    mut decl: *mut *mut ast_variable,
    mut ndecl: libc::c_int,
    mut stmt: *mut *mut ast_stmt,
    mut nstmt: libc::c_int,
) -> *mut ast_block {
    if !(nstmt > 0 as libc::c_int || stmt.is_null()) {
        panic!();
    }
    let mut b: *mut ast_block = malloc(::core::mem::size_of::<ast_block>()) as *mut ast_block;
    (*b).decl = decl;
    (*b).ndecl = ndecl;
    (*b).stmt = stmt;
    (*b).nstmt = nstmt;
    return b;
}

pub unsafe fn ast_block_destroy(mut b: *mut ast_block) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*b).ndecl {
        ast_variable_destroy(*((*b).decl).offset(i as isize));
        i += 1;
    }
    free((*b).decl as *mut libc::c_void);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < (*b).nstmt {
        ast_stmt_destroy(*((*b).stmt).offset(i_0 as isize));
        i_0 += 1;
    }
    free((*b).stmt as *mut libc::c_void);
    free(b as *mut libc::c_void);
}

pub unsafe fn ast_block_copy(mut b: *mut ast_block) -> *mut ast_block {
    if b.is_null() {
        panic!();
    }
    return ast_block_create(
        copy_var_arr((*b).ndecl, (*b).decl),
        (*b).ndecl,
        copy_stmt_arr((*b).nstmt, (*b).stmt),
        (*b).nstmt,
    );
}
unsafe fn copy_var_arr(
    mut len: libc::c_int,
    mut var: *mut *mut ast_variable,
) -> *mut *mut ast_variable {
    if !(len == 0 as libc::c_int || !var.is_null()) {
        panic!();
    }
    if len == 0 as libc::c_int {
        return 0 as *mut *mut ast_variable;
    }
    let mut new: *mut *mut ast_variable =
        malloc((::core::mem::size_of::<*mut ast_variable>()).wrapping_mul(len as usize))
            as *mut *mut ast_variable;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < len {
        let ref mut fresh8 = *new.offset(i as isize);
        *fresh8 = ast_variable_copy(*var.offset(i as isize));
        i += 1;
    }
    return new;
}
unsafe fn copy_stmt_arr(mut len: libc::c_int, mut stmt: *mut *mut ast_stmt) -> *mut *mut ast_stmt {
    if !(len == 0 as libc::c_int || !stmt.is_null()) {
        panic!();
    }
    if len == 0 as libc::c_int {
        return 0 as *mut *mut ast_stmt;
    }
    let mut new: *mut *mut ast_stmt =
        malloc((::core::mem::size_of::<*mut ast_stmt>()).wrapping_mul(len as usize))
            as *mut *mut ast_stmt;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < len {
        let ref mut fresh9 = *new.offset(i as isize);
        *fresh9 = ast_stmt_copy(*stmt.offset(i as isize));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_block_str(
    mut b: *mut ast_block,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut sb: *mut strbuilder = strbuilder_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*b).ndecl {
        let mut s: *mut libc::c_char = ast_variable_str(*((*b).decl).offset(i as isize));
        strbuilder_printf(
            sb,
            b"%s%s;\n\0" as *const u8 as *const libc::c_char,
            indent,
            s,
        );
        free(s as *mut libc::c_void);
        i += 1;
    }
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < (*b).nstmt {
        let mut s_0: *mut libc::c_char = ast_stmt_str(*((*b).stmt).offset(i_0 as isize));
        strbuilder_printf(
            sb,
            b"%s%s\n\0" as *const u8 as *const libc::c_char,
            indent,
            s_0,
        );
        free(s_0 as *mut libc::c_void);
        i_0 += 1;
    }
    return strbuilder_build(sb);
}

pub unsafe fn ast_block_ndecls(mut b: *mut ast_block) -> libc::c_int {
    return (*b).ndecl;
}

pub unsafe fn ast_block_decls(mut b: *mut ast_block) -> *mut *mut ast_variable {
    return (*b).decl;
}

pub unsafe fn ast_block_nstmts(mut b: *mut ast_block) -> libc::c_int {
    return (*b).nstmt;
}

pub unsafe fn ast_block_stmts(mut b: *mut ast_block) -> *mut *mut ast_stmt {
    if !((*b).nstmt > 0 as libc::c_int || ((*b).stmt).is_null()) as libc::c_int as libc::c_long != 0
    {
        panic!();
    }
    return (*b).stmt;
}

pub unsafe fn ast_block_isterminal(mut b: *mut ast_block, mut s: *mut state) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*b).nstmt {
        if ast_stmt_isterminal(*((*b).stmt).offset(i as isize), s) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}

pub unsafe fn ast_block_preconds(mut b: *mut ast_block) -> preconds_result {
    let mut n: libc::c_int = ast_block_nstmts(b);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if ast_stmt_ispre(*stmt.offset(i as isize)) {
            let mut preconds: *mut ast_stmt = ast_stmt_labelled_stmt(*stmt.offset(i as isize));
            let mut err: *mut error = ast_stmt_preconds_validate(preconds);
            if !err.is_null() {
                return {
                    let mut init = preconds_result {
                        stmt: 0 as *mut ast_stmt,
                        err: err,
                    };
                    init
                };
            }
            return {
                let mut init = preconds_result {
                    stmt: preconds,
                    err: 0 as *mut error,
                };
                init
            };
        }
        i += 1;
    }
    return {
        let mut init = preconds_result {
            stmt: 0 as *mut ast_stmt,
            err: 0 as *mut error,
        };
        init
    };
}

pub unsafe fn ast_stmt_process(
    mut stmt: *mut ast_stmt,
    mut fname: *mut libc::c_char,
    mut state: *mut state,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    if ast_stmt_kind(stmt) as libc::c_uint == STMT_COMPOUND_V as libc::c_int as libc::c_uint {
        err = ast_stmt_verify(stmt, state);
        if !err.is_null() {
            let mut b: *mut strbuilder = strbuilder_create();
            let mut loc: *mut lexememarker = ast_stmt_lexememarker(stmt);
            if loc.is_null() {
                panic!();
            }
            let mut m: *mut libc::c_char = lexememarker_str(loc);
            strbuilder_printf(
                b,
                b"%s: %s\0" as *const u8 as *const libc::c_char,
                m,
                (*err).msg,
            );
            free(m as *mut libc::c_void);
            return error_create(strbuilder_build(b));
        }
    }
    err = ast_stmt_exec(stmt, state);
    if !err.is_null() {
        let mut b_0: *mut strbuilder = strbuilder_create();
        let mut loc_0: *mut lexememarker = ast_stmt_lexememarker(stmt);
        if loc_0.is_null() {
            panic!();
        }
        let mut m_0: *mut libc::c_char = lexememarker_str(loc_0);
        strbuilder_printf(
            b_0,
            b"%s:%s: cannot exec statement: %s\0" as *const u8 as *const libc::c_char,
            m_0,
            fname,
            (*err).msg,
        );
        free(m_0 as *mut libc::c_void);
        return error_create(strbuilder_build(b_0));
    }
    return 0 as *mut error;
}

pub unsafe fn ast_stmt_preprocess(
    mut stmt: *mut ast_stmt,
    mut state: *mut state,
) -> *mut preresult {
    if ast_stmt_isassume(stmt) {
        return stmt_installprop(stmt, state);
    }
    return preresult_empty_create();
}

pub unsafe fn ast_stmt_equal(mut s1: *mut ast_stmt, mut s2: *mut ast_stmt) -> bool {
    if s1.is_null() || s2.is_null() {
        return 0 as libc::c_int != 0;
    }
    if ast_stmt_kind(s1) as libc::c_uint != ast_stmt_kind(s2) as libc::c_uint {
        return 0 as libc::c_int != 0;
    }
    match ast_stmt_kind(s1) as libc::c_uint {
        16 => return ast_expr_equal(ast_stmt_as_expr(s1), ast_stmt_as_expr(s2)),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_stmt_labelled_stmt(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_LABELLED as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.labelled.stmt;
}
unsafe fn labelled_absexec(
    mut stmt: *mut ast_stmt,
    mut state: *mut state,
    mut should_setup: bool,
) -> *mut result {
    if !ast_stmt_ispre(stmt) {
        let mut s: *mut libc::c_char = ast_stmt_str(stmt);
        let cstr = std::ffi::CStr::from_ptr(s);
        panic!("expected precondition, got: {cstr:?}");
    }
    let mut setup: *mut ast_stmt = ast_stmt_labelled_stmt(stmt);
    if setup.is_null() {
        panic!();
    }
    if !should_setup {
        return result_value_create(0 as *mut value);
    }
    return ast_stmt_absexec(setup, state, should_setup);
}

pub unsafe fn ast_stmt_copy(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    let mut loc: *mut lexememarker = if !((*stmt).loc).is_null() {
        lexememarker_copy((*stmt).loc)
    } else {
        0 as *mut lexememarker
    };
    match (*stmt).kind as libc::c_uint {
        2 => {
            return ast_stmt_create_labelled(
                loc,
                dynamic_str((*stmt).u.labelled.label),
                ast_stmt_copy((*stmt).u.labelled.stmt),
            );
        }
        1 => return ast_stmt_create_nop(loc),
        16 => return ast_stmt_create_expr(loc, ast_expr_copy((*stmt).u.expr)),
        4 => return ast_stmt_create_compound(loc, ast_block_copy((*stmt).u.compound)),
        8 => return ast_stmt_create_compound_v(loc, ast_block_copy((*stmt).u.compound)),
        32 => {
            return ast_stmt_create_sel(
                loc,
                (*stmt).u.selection.isswitch,
                ast_expr_copy((*stmt).u.selection.cond),
                ast_stmt_copy((*stmt).u.selection.body),
                if !((*stmt).u.selection.nest).is_null() {
                    ast_stmt_copy((*stmt).u.selection.nest)
                } else {
                    0 as *mut ast_stmt
                },
            );
        }
        64 => {
            return ast_stmt_create_iter(
                loc,
                ast_stmt_copy((*stmt).u.iteration.init),
                ast_stmt_copy((*stmt).u.iteration.cond),
                ast_expr_copy((*stmt).u.iteration.iter),
                ast_block_copy((*stmt).u.iteration.abstract_0),
                ast_stmt_copy((*stmt).u.iteration.body),
            );
        }
        128 => return ast_stmt_copy_iter(stmt),
        256 => {
            return ast_stmt_create_jump(
                loc,
                (*stmt).u.jump.kind,
                ast_expr_copy_ifnotnull((*stmt).u.jump.rv),
            );
        }
        _ => {
            panic!();
        }
    }
}
unsafe fn labelled_setupabsexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut res: *mut result = ast_stmt_absexec(stmt, state, 1 as libc::c_int != 0);
    if result_iserror(res) {
        return result_as_error(res);
    }
    return 0 as *mut error;
}
unsafe fn sel_setupabsexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut dec: decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if !(dec.err).is_null() {
        return dec.err;
    }
    if dec.decision {
        return stmt_setupabsexec(ast_stmt_sel_body(stmt), state);
    }
    if !(ast_stmt_sel_nest(stmt)).is_null() {
        panic!();
    }
    return 0 as *mut error;
}
unsafe fn comp_setupabsexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut b: *mut ast_block = ast_stmt_as_block(stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let mut nstmt: libc::c_int = ast_block_nstmts(b);
    let mut stmts: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmt {
        if ast_stmt_ispre(*stmts.offset(i as isize)) {
            err = stmt_setupabsexec(*stmts.offset(i as isize), state);
            if !err.is_null() {
                return err;
            }
            if ast_stmt_isterminal(*stmts.offset(i as isize), state) {
                break;
            }
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn stmt_setupabsexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    match ast_stmt_kind(stmt) as libc::c_uint {
        16 | 512 | 256 => return 0 as *mut error,
        2 => return labelled_setupabsexec(stmt, state),
        32 => return sel_setupabsexec(stmt, state),
        4 => return comp_setupabsexec(stmt, state),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_stmt_setupabsexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    if ast_stmt_kind(stmt) as libc::c_uint != STMT_SELECTION as libc::c_int as libc::c_uint {
        return 0 as *mut error;
    }
    return stmt_setupabsexec(stmt, state);
}

pub unsafe fn ast_stmt_isselection(mut stmt: *mut ast_stmt) -> bool {
    return (*stmt).kind as libc::c_uint == STMT_SELECTION as libc::c_int as libc::c_uint;
}

pub unsafe fn ast_stmt_isassume(mut stmt: *mut ast_stmt) -> bool {
    return (*stmt).kind as libc::c_uint == STMT_LABELLED as libc::c_int as libc::c_uint
        && strcmp(
            (*stmt).u.labelled.label,
            b"assume\0" as *const u8 as *const libc::c_char,
        ) == 0 as libc::c_int;
}
unsafe fn stmt_installprop(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut preresult {
    return ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state);
}

pub unsafe fn ast_stmt_ispre(mut stmt: *mut ast_stmt) -> bool {
    return (*stmt).kind as libc::c_uint == STMT_LABELLED as libc::c_int as libc::c_uint
        && strcmp(
            (*stmt).u.labelled.label,
            b"setup\0" as *const u8 as *const libc::c_char,
        ) == 0 as libc::c_int;
}
unsafe fn stmt_v_block_verify(
    mut v_block_stmt: *mut ast_stmt,
    mut state: *mut state,
) -> *mut error {
    let mut b: *mut ast_block = ast_stmt_as_v_block(v_block_stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let mut nstmts: libc::c_int = ast_block_nstmts(b);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        let mut err: *mut error = ast_stmt_verify(*stmt.offset(i as isize), state);
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn stmt_expr_verify(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut expr: *mut ast_expr = ast_stmt_as_expr(stmt);
    if ast_expr_decide(expr, state) {
        return 0 as *mut error;
    }
    return error_create(
        b"cannot verify statement\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
}
unsafe fn iter_empty(mut stmt: *mut ast_stmt, mut state: *mut state) -> bool {
    let mut err: *mut error = ast_stmt_exec(ast_stmt_iter_init(stmt), state);
    if !err.is_null() {
        panic!();
    }
    return !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state);
}
unsafe fn stmt_iter_verify(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    if iter_empty(stmt, state) {
        return 0 as *mut error;
    }
    let mut body: *mut ast_stmt = ast_stmt_iter_body(stmt);
    if !(ast_stmt_kind(body) as libc::c_uint == STMT_COMPOUND as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut Block: *mut ast_block = ast_stmt_as_block(body);
    if !(ast_block_ndecls(Block) == 0 as libc::c_int && ast_block_nstmts(Block) == 1 as libc::c_int)
    {
        panic!();
    }
    let mut assertion: *mut ast_expr =
        ast_stmt_as_expr(*(ast_block_stmts(Block)).offset(0 as libc::c_int as isize));
    let mut lw: *mut ast_expr = ast_stmt_iter_lower_bound(stmt);
    let mut up: *mut ast_expr = ast_stmt_iter_upper_bound(stmt);
    if !ast_expr_rangedecide(assertion, lw, up, state) {
        return error_create(
            b"could not verify\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return 0 as *mut error;
}

pub unsafe fn ast_stmt_verify(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    match ast_stmt_kind(stmt) as libc::c_uint {
        1 => return 0 as *mut error,
        8 => return stmt_v_block_verify(stmt, state),
        16 => return stmt_expr_verify(stmt, state),
        64 => return stmt_iter_verify(stmt, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn stmt_compound_exec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut b: *mut ast_block = ast_stmt_as_block(stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let mut nstmt: libc::c_int = ast_block_nstmts(b);
    let mut stmts: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmt {
        let mut err: *mut error = ast_stmt_exec(*stmts.offset(i as isize), state);
        if !err.is_null() {
            return err;
        }
        if ast_stmt_isterminal(*stmts.offset(i as isize), state) {
            break;
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn stmt_sel_exec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut dec: decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if !(dec.err).is_null() {
        return dec.err;
    }
    if dec.decision {
        return ast_stmt_exec(ast_stmt_sel_body(stmt), state);
    }
    if !(ast_stmt_sel_nest(stmt)).is_null() {
        panic!();
    }
    return 0 as *mut error;
}
unsafe fn iter_neteffect(mut iter: *mut ast_stmt) -> *mut ast_stmt {
    let mut abs: *mut ast_block = ast_stmt_iter_abstract(iter);
    if abs.is_null() {
        panic!();
    }
    let mut nstmts: libc::c_int = ast_block_nstmts(abs);
    if nstmts == 0 {
        return 0 as *mut ast_stmt;
    }
    if !(ast_block_ndecls(abs) == 0 as libc::c_int && nstmts == 1 as libc::c_int) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return ast_stmt_create_iter(
        0 as *mut lexememarker,
        ast_stmt_copy(ast_stmt_iter_init(iter)),
        ast_stmt_copy(ast_stmt_iter_cond(iter)),
        ast_expr_copy(ast_stmt_iter_iter(iter)),
        ast_block_create(
            0 as *mut *mut ast_variable,
            0 as libc::c_int,
            0 as *mut *mut ast_stmt,
            0 as libc::c_int,
        ),
        ast_stmt_create_compound(
            0 as *mut lexememarker,
            ast_block_copy(ast_stmt_iter_abstract(iter)),
        ),
    );
}
unsafe fn stmt_iter_exec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut neteffect: *mut ast_stmt = iter_neteffect(stmt);
    if neteffect.is_null() {
        return 0 as *mut error;
    }
    let mut res: *mut result = ast_stmt_absexec(neteffect, state, 1 as libc::c_int != 0);
    if result_iserror(res) {
        return result_as_error(res);
    }
    ast_stmt_destroy(neteffect);
    return 0 as *mut error;
}
unsafe fn stmt_jump_exec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    let mut res: *mut result = ast_expr_eval(ast_stmt_jump_rv(stmt), state);
    if result_iserror(res) {
        return result_as_error(res);
    }
    if result_hasvalue(res) {
        let mut obj: *mut object = state_getresult(state);
        if obj.is_null() {
            panic!();
        }
        object_assign(obj, value_copy(result_as_value(res)));
    }
    return 0 as *mut error;
}

pub unsafe fn ast_stmt_exec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut error {
    match ast_stmt_kind(stmt) as libc::c_uint {
        1 => return 0 as *mut error,
        2 => return ast_stmt_exec(ast_stmt_labelled_stmt(stmt), state),
        4 => return stmt_compound_exec(stmt, state),
        8 => return 0 as *mut error,
        16 => return ast_expr_exec(ast_stmt_as_expr(stmt), state),
        32 => return stmt_sel_exec(stmt, state),
        64 => return stmt_iter_exec(stmt, state),
        256 => return stmt_jump_exec(stmt, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn ast_stmt_jump_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    if !((*stmt).kind as libc::c_uint == STMT_JUMP as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let mut rv: *mut libc::c_char = ast_expr_str((*stmt).u.jump.rv);
    strbuilder_printf(b, b"return %s;\n\0" as *const u8 as *const libc::c_char, rv);
    free(rv as *mut libc::c_void);
}

pub unsafe fn ast_stmt_iter_abstract(mut stmt: *mut ast_stmt) -> *mut ast_block {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.abstract_0;
}

pub unsafe fn ast_stmt_iter_iter(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.iter;
}

pub unsafe fn ast_stmt_lexememarker(mut stmt: *mut ast_stmt) -> *mut lexememarker {
    return (*stmt).loc;
}
unsafe fn ast_stmt_iter_e_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION_E as libc::c_int as libc::c_uint) {
        panic!();
    }
    (*stmt).kind = STMT_ITERATION;
    let mut s: *mut libc::c_char = ast_stmt_str(stmt);
    (*stmt).kind = STMT_ITERATION_E;
    strbuilder_printf(b, b".%s\0" as *const u8 as *const libc::c_char, s);
    free(s as *mut libc::c_void);
}
unsafe fn ast_stmt_iter_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut init: *mut libc::c_char = ast_stmt_str((*stmt).u.iteration.init);
    let mut cond: *mut libc::c_char = ast_stmt_str((*stmt).u.iteration.cond);
    let mut body: *mut libc::c_char = ast_stmt_str((*stmt).u.iteration.body);
    let mut iter: *mut libc::c_char = ast_expr_str((*stmt).u.iteration.iter);
    let mut abs: *mut libc::c_char = (if !((*stmt).u.iteration.abstract_0).is_null() {
        ast_block_str(
            (*stmt).u.iteration.abstract_0,
            b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ) as *const libc::c_char
    } else {
        b"\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    strbuilder_printf(
        b,
        b"for (%s %s %s) [%s] { %s }\0" as *const u8 as *const libc::c_char,
        init,
        cond,
        iter,
        abs,
        body,
    );
    free(init as *mut libc::c_void);
    free(cond as *mut libc::c_void);
    free(body as *mut libc::c_void);
    free(iter as *mut libc::c_void);
}

pub unsafe fn ast_stmt_str(mut stmt: *mut ast_stmt) -> *mut libc::c_char {
    if stmt.is_null() {
        panic!();
    }
    let mut b: *mut strbuilder = strbuilder_create();
    match (*stmt).kind as libc::c_uint {
        2 => {
            ast_stmt_labelled_sprint(stmt, b);
        }
        1 => {
            ast_stmt_nop_sprint(stmt, b);
        }
        16 => {
            ast_stmt_expr_sprint(stmt, b);
        }
        4 => {
            ast_stmt_compound_sprint(stmt, b);
        }
        8 => {
            ast_stmt_compound_sprint(stmt, b);
        }
        32 => {
            ast_stmt_sel_sprint(stmt, b);
        }
        64 => {
            ast_stmt_iter_sprint(stmt, b);
        }
        128 => {
            ast_stmt_iter_e_sprint(stmt, b);
        }
        256 => {
            ast_stmt_jump_sprint(stmt, b);
        }
        _ => {
            panic!();
        }
    }
    return strbuilder_build(b);
}
unsafe fn ast_expr_copy_ifnotnull(mut expr: *mut ast_expr) -> *mut ast_expr {
    return if !expr.is_null() {
        ast_expr_copy(expr)
    } else {
        0 as *mut ast_expr
    };
}

pub unsafe fn ast_stmt_iter_cond(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.cond;
}

pub unsafe fn ast_stmt_iter_init(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.init;
}

pub unsafe fn ast_stmt_labelled_label(mut stmt: *mut ast_stmt) -> *mut libc::c_char {
    if !((*stmt).kind as libc::c_uint == STMT_LABELLED as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.labelled.label;
}
unsafe fn sel_isterminal(mut stmt: *mut ast_stmt, mut s: *mut state) -> bool {
    let mut dec: decision = sel_decide(ast_stmt_sel_cond(stmt), s);
    if !(dec.err).is_null() {
        panic!();
    }
    if dec.decision {
        return ast_stmt_isterminal(ast_stmt_sel_body(stmt), s);
    }
    return 0 as libc::c_int != 0;
}
unsafe fn comp_absexec(
    mut stmt: *mut ast_stmt,
    mut state: *mut state,
    mut should_setup: bool,
) -> *mut result {
    let mut b: *mut ast_block = ast_stmt_as_block(stmt);
    let mut stmts: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        let mut res: *mut result = ast_stmt_absexec(*stmts.offset(i as isize), state, should_setup);
        if result_iserror(res) {
            return res;
        }
        i += 1;
    }
    return result_value_create(0 as *mut value);
}

pub unsafe fn ast_stmt_as_block(mut stmt: *mut ast_stmt) -> *mut ast_block {
    if !((*stmt).kind as libc::c_uint == STMT_COMPOUND as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.compound;
}

pub unsafe fn ast_stmt_jump_rv(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    return (*stmt).u.jump.rv;
}
unsafe fn jump_absexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut result {
    return ast_expr_absexec(
        ast_expr_assignment_create(
            ast_expr_identifier_create(
                b"return\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
            ),
            ast_stmt_jump_rv(stmt),
        ),
        state,
    );
}

pub unsafe fn ast_stmt_absexec(
    mut stmt: *mut ast_stmt,
    mut state: *mut state,
    mut should_setup: bool,
) -> *mut result {
    match ast_stmt_kind(stmt) as libc::c_uint {
        1 => return result_value_create(0 as *mut value),
        2 => return labelled_absexec(stmt, state, should_setup),
        16 => return ast_expr_absexec(ast_stmt_as_expr(stmt), state),
        32 => return sel_absexec(stmt, state, should_setup),
        64 => return iter_absexec(stmt, state),
        4 => return comp_absexec(stmt, state, should_setup),
        256 => return jump_absexec(stmt, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn ast_stmt_sel_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    if !((*stmt).kind as libc::c_uint == STMT_SELECTION as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut cond: *mut libc::c_char = ast_expr_str((*stmt).u.selection.cond);
    let mut body: *mut libc::c_char = ast_stmt_str((*stmt).u.selection.body);
    strbuilder_printf(
        b,
        b"if (%s) { %s }\0" as *const u8 as *const libc::c_char,
        cond,
        body,
    );
    let mut nest_stmt: *mut ast_stmt = (*stmt).u.selection.nest;
    if !nest_stmt.is_null() {
        let mut nest: *mut libc::c_char = ast_stmt_str(nest_stmt);
        strbuilder_printf(b, b" else %s\0" as *const u8 as *const libc::c_char, nest);
        free(nest as *mut libc::c_void);
    }
    free(cond as *mut libc::c_void);
    free(body as *mut libc::c_void);
}

pub unsafe fn ast_stmt_isterminal(mut stmt: *mut ast_stmt, mut s: *mut state) -> bool {
    match (*stmt).kind as libc::c_uint {
        256 => {
            return (*stmt).u.jump.kind as libc::c_uint
                == JUMP_RETURN as libc::c_int as libc::c_uint;
        }
        4 => return ast_block_isterminal((*stmt).u.compound, s),
        32 => return sel_isterminal(stmt, s),
        _ => return 0 as libc::c_int != 0,
    };
}

pub unsafe fn ast_stmt_create_jump(
    mut loc: *mut lexememarker,
    mut kind: ast_jump_kind,
    mut rv: *mut ast_expr,
) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_JUMP;
    (*stmt).u.jump.kind = JUMP_RETURN;
    (*stmt).u.jump.rv = rv;
    return stmt;
}

pub unsafe fn sel_decide(mut control: *mut ast_expr, mut state: *mut state) -> decision {
    let mut res: *mut result = ast_expr_pf_reduce(control, state);
    if result_iserror(res) {
        return {
            let mut init = decision {
                decision: false,
                err: result_as_error(res),
            };
            init
        };
    }
    if !result_hasvalue(res) {
        panic!();
    }
    let mut v: *mut value = result_as_value(res);
    if value_issync(v) {
        let mut sync: *mut ast_expr = value_as_sync(v);
        let mut p: *mut props = state_getprops(state);
        if props_get(p, sync) {
            return {
                let mut init = decision {
                    decision: 1 as libc::c_int != 0,
                    err: 0 as *mut error,
                };
                init
            };
        } else if props_contradicts(p, sync) {
            return {
                let mut init = decision {
                    decision: 0 as libc::c_int != 0,
                    err: 0 as *mut error,
                };
                init
            };
        }
    }
    if value_isconstant(v) {
        if value_as_constant(v) != 0 {
            return {
                let mut init = decision {
                    decision: 1 as libc::c_int != 0,
                    err: 0 as *mut error,
                };
                init
            };
        }
        return {
            let mut init = decision {
                decision: 0 as libc::c_int != 0,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut zero: *mut value = value_int_create(0 as libc::c_int);
    if !values_comparable(zero, v) {
        let mut b: *mut strbuilder = strbuilder_create();
        let mut c_str: *mut libc::c_char = ast_expr_str(control);
        let mut v_str: *mut libc::c_char = value_str(v);
        strbuilder_printf(
            b,
            b"`%s' with value `%s' is undecidable\0" as *const u8 as *const libc::c_char,
            c_str,
            v_str,
        );
        free(v_str as *mut libc::c_void);
        free(c_str as *mut libc::c_void);
        return {
            let mut init = decision {
                decision: 0 as libc::c_int != 0,
                err: error_create(strbuilder_build(b)),
            };
            init
        };
    }
    let mut nonzero: bool = !value_equal(zero, v);
    value_destroy(zero);
    return {
        let mut init = decision {
            decision: nonzero,
            err: 0 as *mut error,
        };
        init
    };
}
unsafe fn ast_stmt_compound_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    if !((*stmt).kind as libc::c_uint == STMT_COMPOUND as libc::c_int as libc::c_uint
        || (*stmt).kind as libc::c_uint == STMT_COMPOUND_V as libc::c_int as libc::c_uint)
    {
        panic!();
    }
    let mut s: *mut libc::c_char = ast_block_str(
        (*stmt).u.compound,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    strbuilder_printf(b, s);
    free(s as *mut libc::c_void);
}
unsafe fn ast_stmt_expr_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    if !((*stmt).kind as libc::c_uint == STMT_EXPR as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let mut s: *mut libc::c_char = ast_expr_str((*stmt).u.expr);
    strbuilder_printf(b, b"%s;\0" as *const u8 as *const libc::c_char, s);
    free(s as *mut libc::c_void);
}
unsafe fn ast_stmt_create(mut loc: *mut lexememarker) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = calloc(1, ::core::mem::size_of::<ast_stmt>()) as *mut ast_stmt;
    (*stmt).loc = loc;
    return stmt;
}
unsafe fn ast_stmt_copy_iter(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    (*stmt).kind = STMT_ITERATION;
    let mut copy: *mut ast_stmt = ast_stmt_copy(stmt);
    (*stmt).kind = STMT_ITERATION_E;
    return ast_stmt_create_iter_e(copy);
}

pub unsafe fn ast_stmt_create_iter_e(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    (*stmt).kind = STMT_ITERATION_E;
    return stmt;
}

pub unsafe fn ast_stmt_create_iter(
    mut loc: *mut lexememarker,
    mut init: *mut ast_stmt,
    mut cond: *mut ast_stmt,
    mut iter: *mut ast_expr,
    mut abstract_0: *mut ast_block,
    mut body: *mut ast_stmt,
) -> *mut ast_stmt {
    if !(!init.is_null()
        && !cond.is_null()
        && !iter.is_null()
        && !abstract_0.is_null()
        && !body.is_null()) as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_ITERATION;
    (*stmt).u.iteration.init = init;
    (*stmt).u.iteration.cond = cond;
    (*stmt).u.iteration.iter = iter;
    (*stmt).u.iteration.body = body;
    (*stmt).u.iteration.abstract_0 = abstract_0;
    return stmt;
}
unsafe fn ast_stmt_nop_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    strbuilder_printf(b, b";\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn ast_stmt_iter_body(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.body;
}

pub unsafe fn ast_stmt_create_sel(
    mut loc: *mut lexememarker,
    mut isswitch: bool,
    mut cond: *mut ast_expr,
    mut body: *mut ast_stmt,
    mut nest: *mut ast_stmt,
) -> *mut ast_stmt {
    if isswitch {
        panic!();
    }
    if cond.is_null() {
        panic!();
    }
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_SELECTION;
    (*stmt).u.selection.isswitch = isswitch;
    (*stmt).u.selection.cond = cond;
    (*stmt).u.selection.body = body;
    (*stmt).u.selection.nest = nest;
    return stmt;
}

pub unsafe fn ast_stmt_create_compound_v(
    mut loc: *mut lexememarker,
    mut b: *mut ast_block,
) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_COMPOUND_V;
    (*stmt).u.compound = b;
    return stmt;
}
unsafe fn hack_alloc_from_neteffect(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    let mut body: *mut ast_stmt = ast_stmt_iter_body(stmt);
    if !(ast_stmt_kind(body) as libc::c_uint == STMT_COMPOUND as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut Block: *mut ast_block = ast_stmt_as_block(body);
    if !(ast_block_ndecls(Block) == 0 as libc::c_int && ast_block_nstmts(Block) == 1 as libc::c_int)
    {
        panic!();
    }
    return ast_stmt_as_expr(*(ast_block_stmts(Block)).offset(0 as libc::c_int as isize));
}

pub unsafe fn ast_stmt_iter_lower_bound(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut init: *mut ast_stmt = (*stmt).u.iteration.init;
    if !((*init).kind as libc::c_uint == STMT_EXPR as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return ast_expr_assignment_rval((*init).u.expr);
}
unsafe fn ast_stmt_labelled_sprint(mut stmt: *mut ast_stmt, mut b: *mut strbuilder) {
    let mut s: *mut libc::c_char = ast_stmt_str((*stmt).u.labelled.stmt);
    strbuilder_printf(
        b,
        b"%s: %s\0" as *const u8 as *const libc::c_char,
        (*stmt).u.labelled.label,
        s,
    );
    free(s as *mut libc::c_void);
}
unsafe fn sel_absexec(
    mut stmt: *mut ast_stmt,
    mut state: *mut state,
    mut should_setup: bool,
) -> *mut result {
    let mut dec: decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if !(dec.err).is_null() {
        return result_error_create(dec.err);
    }
    if dec.decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    if !(ast_stmt_sel_nest(stmt)).is_null() {
        panic!();
    }
    return result_value_create(0 as *mut value);
}

pub unsafe fn ast_stmt_sel_nest(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_SELECTION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.selection.nest;
}

pub unsafe fn ast_stmt_create_labelled(
    mut loc: *mut lexememarker,
    mut label: *mut libc::c_char,
    mut substmt: *mut ast_stmt,
) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_LABELLED;
    (*stmt).u.labelled.label = label;
    (*stmt).u.labelled.stmt = substmt;
    return stmt;
}

pub unsafe fn ast_stmt_create_nop(mut loc: *mut lexememarker) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_NOP;
    return stmt;
}

pub unsafe fn ast_stmt_create_expr(
    mut loc: *mut lexememarker,
    mut expr: *mut ast_expr,
) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_EXPR;
    (*stmt).u.expr = expr;
    return stmt;
}

pub unsafe fn ast_stmt_create_compound(
    mut loc: *mut lexememarker,
    mut b: *mut ast_block,
) -> *mut ast_stmt {
    let mut stmt: *mut ast_stmt = ast_stmt_create(loc);
    (*stmt).kind = STMT_COMPOUND;
    (*stmt).u.compound = b;
    return stmt;
}

pub unsafe fn ast_stmt_sel_cond(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    if !((*stmt).kind as libc::c_uint == STMT_SELECTION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.selection.cond;
}

pub unsafe fn ast_stmt_sel_body(mut stmt: *mut ast_stmt) -> *mut ast_stmt {
    if !((*stmt).kind as libc::c_uint == STMT_SELECTION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.selection.body;
}

pub unsafe fn ast_stmt_iter_upper_bound(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    if !((*stmt).kind as libc::c_uint == STMT_ITERATION as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut cond: *mut ast_stmt = (*stmt).u.iteration.cond;
    if !((*cond).kind as libc::c_uint == STMT_EXPR as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return ast_expr_binary_e2((*cond).u.expr);
}
unsafe fn iter_absexec(mut stmt: *mut ast_stmt, mut state: *mut state) -> *mut result {
    let mut err: *mut error = 0 as *mut error;
    let mut alloc: *mut ast_expr = hack_alloc_from_neteffect(stmt);
    let mut lw: *mut ast_expr = ast_stmt_iter_lower_bound(stmt);
    let mut up: *mut ast_expr = ast_stmt_iter_upper_bound(stmt);
    err = ast_expr_alloc_rangeprocess(alloc, lw, up, state);
    if !err.is_null() {
        return result_error_create(err);
    }
    return result_value_create(0 as *mut value);
}

pub unsafe fn ast_stmt_destroy(mut stmt: *mut ast_stmt) {
    match (*stmt).kind as libc::c_uint {
        2 => {
            free((*stmt).u.labelled.label as *mut libc::c_void);
            ast_stmt_destroy((*stmt).u.labelled.stmt);
        }
        1 => {}
        4 | 8 => {
            ast_block_destroy((*stmt).u.compound);
        }
        32 => {
            ast_expr_destroy((*stmt).u.selection.cond);
            ast_stmt_destroy((*stmt).u.selection.body);
            if !((*stmt).u.selection.nest).is_null() {
                ast_stmt_destroy((*stmt).u.selection.nest);
            }
        }
        64 | 128 => {
            ast_stmt_destroy((*stmt).u.iteration.init);
            ast_stmt_destroy((*stmt).u.iteration.cond);
            ast_stmt_destroy((*stmt).u.iteration.body);
            ast_expr_destroy((*stmt).u.iteration.iter);
            ast_block_destroy((*stmt).u.iteration.abstract_0);
        }
        16 => {
            ast_expr_destroy((*stmt).u.expr);
        }
        256 => {
            ast_stmt_destroy_jump(stmt);
        }
        _ => {
            panic!();
        }
    }
    if !((*stmt).loc).is_null() {
        lexememarker_destroy((*stmt).loc);
    }
    free(stmt as *mut libc::c_void);
}
unsafe fn ast_stmt_destroy_jump(mut stmt: *mut ast_stmt) {
    let mut rv: *mut ast_expr = (*stmt).u.jump.rv;
    if rv.is_null() {
        return;
    }
    if !((*stmt).u.jump.kind as libc::c_uint == JUMP_RETURN as libc::c_int as libc::c_uint) {
        panic!();
    }
    ast_expr_destroy(rv);
}

pub unsafe fn ast_stmt_as_expr(mut stmt: *mut ast_stmt) -> *mut ast_expr {
    if !((*stmt).kind as libc::c_uint == STMT_EXPR as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*stmt).u.expr;
}

pub unsafe fn ast_stmt_as_v_block(mut stmt: *mut ast_stmt) -> *mut ast_block {
    if !((*stmt).kind as libc::c_uint == STMT_COMPOUND_V as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.compound;
}

pub unsafe fn ast_stmt_kind(mut stmt: *mut ast_stmt) -> ast_stmt_kind {
    return (*stmt).kind;
}

pub unsafe fn ast_stmt_getfuncs(mut stmt: *mut ast_stmt) -> Box<string_arr> {
    match (*stmt).kind as libc::c_uint {
        1 => string_arr_create(),
        2 => ast_stmt_getfuncs((*stmt).u.labelled.stmt),
        4 | 8 => ast_stmt_compound_getfuncs(stmt),
        16 => ast_stmt_expr_getfuncs(stmt),
        32 => ast_stmt_selection_getfuncs(stmt),
        64 | 128 => ast_stmt_iteration_getfuncs(stmt),
        256 => ast_expr_getfuncs((*stmt).u.jump.rv),
        _ => panic!("invalid stmt kind"),
    }
}

pub unsafe fn ast_stmt_splits(mut stmt: *mut ast_stmt, mut s: *mut state) -> ast_stmt_splits {
    match (*stmt).kind as libc::c_uint {
        1 => {
            return {
                let mut init = ast_stmt_splits {
                    n: 0 as libc::c_int,
                    cond: 0 as *mut *mut ast_expr,
                    err: 0 as *mut error,
                };
                init
            };
        }
        16 => return ast_expr_splits((*stmt).u.expr, s),
        32 => return stmt_sel_splits(stmt, s),
        256 => {
            if !((*stmt).u.jump.rv).is_null() {
                return ast_expr_splits((*stmt).u.jump.rv, s);
            }
            return {
                let mut init = ast_stmt_splits {
                    n: 0 as libc::c_int,
                    cond: 0 as *mut *mut ast_expr,
                    err: 0 as *mut error,
                };
                init
            };
        }
        2 => return ast_stmt_splits((*stmt).u.labelled.stmt, s),
        64 | 4 | 8 => {
            return {
                let mut init = ast_stmt_splits {
                    n: 0 as libc::c_int,
                    cond: 0 as *mut *mut ast_expr,
                    err: 0 as *mut error,
                };
                init
            };
        }
        _ => {
            panic!();
        }
    }
}
unsafe fn stmt_sel_splits(mut stmt: *mut ast_stmt, mut s: *mut state) -> ast_stmt_splits {
    let mut res: *mut result = ast_expr_pf_reduce((*stmt).u.selection.cond, s);
    let mut v: *mut value = result_as_value(res);
    let mut e: *mut ast_expr = value_to_expr(v);
    if condexists(e, s) as libc::c_int != 0 || value_isconstant(v) as libc::c_int != 0 {
        return {
            let mut init = ast_stmt_splits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut ast_expr,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut cond: *mut *mut ast_expr =
        malloc(::core::mem::size_of::<*mut ast_expr>()) as *mut *mut ast_expr;
    let ref mut fresh10 = *cond.offset(0 as libc::c_int as isize);
    *fresh10 = e;
    return {
        let mut init = ast_stmt_splits {
            n: 1 as libc::c_int,
            cond: cond,
            err: 0 as *mut error,
        };
        init
    };
}
unsafe fn condexists(mut cond: *mut ast_expr, mut s: *mut state) -> bool {
    let mut res: *mut result = ast_expr_pf_reduce(cond, s);
    if !(!result_iserror(res) && result_hasvalue(res) as libc::c_int != 0) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let mut reduced: *mut ast_expr = value_to_expr(result_as_value(res));
    let mut p: *mut props = state_getprops(s);
    return props_get(p, reduced) as libc::c_int != 0
        || props_contradicts(p, reduced) as libc::c_int != 0;
}
unsafe fn ast_stmt_expr_getfuncs(mut stmt: *mut ast_stmt) -> Box<string_arr> {
    ast_expr_getfuncs((*stmt).u.expr)
}
unsafe fn ast_stmt_selection_getfuncs(mut stmt: *mut ast_stmt) -> Box<string_arr> {
    let mut cond: *mut ast_expr = (*stmt).u.selection.cond;
    let mut body: *mut ast_stmt = (*stmt).u.selection.body;
    let mut nest: *mut ast_stmt = (*stmt).u.selection.nest;
    let mut cond_arr = ast_expr_getfuncs(cond);
    let mut body_arr = ast_stmt_getfuncs(body);
    let mut nest_arr = if !nest.is_null() {
        ast_stmt_getfuncs(nest)
    } else {
        string_arr_create()
    };
    string_arr_concat(
        &string_arr_create(),
        &string_arr_concat(&cond_arr, &string_arr_concat(&body_arr, &nest_arr)),
    )
}
unsafe fn ast_stmt_iteration_getfuncs(mut stmt: *mut ast_stmt) -> Box<string_arr> {
    let mut init: *mut ast_stmt = (*stmt).u.iteration.init;
    let mut cond: *mut ast_stmt = (*stmt).u.iteration.cond;
    let mut body: *mut ast_stmt = (*stmt).u.iteration.body;
    let mut iter: *mut ast_expr = (*stmt).u.iteration.iter;
    let mut init_arr = ast_stmt_getfuncs(init);
    let mut cond_arr = ast_stmt_getfuncs(cond);
    let mut body_arr = ast_stmt_getfuncs(body);
    let mut iter_arr = ast_expr_getfuncs(iter);
    string_arr_concat(
        &string_arr_create(),
        &string_arr_concat(
            &string_arr_concat(&init_arr, &cond_arr),
            &string_arr_concat(&body_arr, &iter_arr),
        ),
    )
}
unsafe fn ast_stmt_compound_getfuncs(mut stmt: *mut ast_stmt) -> Box<string_arr> {
    let mut res = string_arr_create();
    let mut b: *mut ast_block = (*stmt).u.compound;
    let mut stmts: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        res = string_arr_concat(&res, &ast_stmt_getfuncs(*stmts.offset(i as isize)));
        i += 1;
    }
    res
}

pub unsafe fn ast_stmt_preconds_validate(mut stmt: *mut ast_stmt) -> *mut error {
    match (*stmt).kind as libc::c_uint {
        16 | 512 | 64 => return 0 as *mut error,
        32 => return preconds_selection_verify(stmt),
        4 => return preconds_compound_verify(stmt),
        _ => {
            panic!();
        }
    }
}
unsafe fn preconds_selection_verify(mut stmt: *mut ast_stmt) -> *mut error {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut l: *mut lexememarker = ast_stmt_lexememarker(stmt);
    strbuilder_printf(
        b,
        b"%s setup preconditions must be decidable\0" as *const u8 as *const libc::c_char,
        lexememarker_str(l),
    );
    return error_create(strbuilder_build(b));
}
unsafe fn preconds_compound_verify(mut stmt: *mut ast_stmt) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut b: *mut ast_block = (*stmt).u.compound;
    let mut stmts: *mut *mut ast_stmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        err = ast_stmt_preconds_validate(*stmts.offset(i as isize));
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut error;
}

pub unsafe fn ast_type_isint(mut t: *mut ast_type) -> bool {
    return (*t).base as libc::c_uint == TYPE_INT as libc::c_int as libc::c_uint;
}

pub unsafe fn ast_type_ispointer(mut t: *mut ast_type) -> bool {
    return (*t).base as libc::c_uint == TYPE_POINTER as libc::c_int as libc::c_uint;
}

pub unsafe fn ast_type_create(
    mut base: ast_type_base,
    mut mod_0: ast_type_modifier,
) -> *mut ast_type {
    let mut t: *mut ast_type = malloc(::core::mem::size_of::<ast_type>()) as *mut ast_type;
    if t.is_null() {
        panic!();
    }
    (*t).base = base;
    (*t).mod_0 = mod_0 as libc::c_int;
    return t;
}

pub unsafe fn ast_type_create_ptr(mut ref_0: *mut ast_type) -> *mut ast_type {
    if ref_0.is_null() {
        panic!();
    }
    let mut t: *mut ast_type = ast_type_create(TYPE_POINTER, 0 as ast_type_modifier);
    (*t).c2rust_unnamed.ptr_type = ref_0;
    return t;
}

pub unsafe fn ast_type_create_voidptr() -> *mut ast_type {
    let mut t: *mut ast_type = ast_type_create(TYPE_POINTER, 0 as ast_type_modifier);
    (*t).c2rust_unnamed.ptr_type = 0 as *mut ast_type;
    return t;
}

pub unsafe fn ast_type_create_arr(
    mut base: *mut ast_type,
    mut length: libc::c_int,
) -> *mut ast_type {
    if base.is_null() {
        panic!();
    }
    let mut t: *mut ast_type = ast_type_create(TYPE_ARRAY, 0 as ast_type_modifier);
    (*t).c2rust_unnamed.arr.type_0 = base;
    (*t).c2rust_unnamed.arr.length = length;
    return t;
}

pub unsafe fn ast_type_create_struct(
    mut tag: *mut libc::c_char,
    mut members: *mut ast_variable_arr,
) -> *mut ast_type {
    let mut t: *mut ast_type = ast_type_create(TYPE_STRUCT, 0 as ast_type_modifier);
    (*t).c2rust_unnamed.structunion.tag = tag;
    (*t).c2rust_unnamed.structunion.members = members;
    return t;
}

pub unsafe fn ast_type_create_userdef(mut name: *mut libc::c_char) -> *mut ast_type {
    let mut t: *mut ast_type = ast_type_create(TYPE_USERDEF, 0 as ast_type_modifier);
    (*t).c2rust_unnamed.userdef = name;
    return t;
}

pub unsafe fn ast_type_vconst(
    mut t: *mut ast_type,
    mut s: *mut state,
    mut comment: *mut libc::c_char,
    mut persist: bool,
) -> *mut value {
    match (*t).base as libc::c_uint {
        3 => return value_int_indefinite_create(),
        9 => return value_ptr_indefinite_create(),
        14 => {
            return ast_type_vconst(
                externals_gettypedef(state_getext(s), (*t).c2rust_unnamed.userdef),
                s,
                comment,
                persist,
            );
        }
        11 => return value_struct_indefinite_create(t, s, comment, persist),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_type_isstruct(mut t: *mut ast_type) -> bool {
    return (*t).base as libc::c_uint == TYPE_STRUCT as libc::c_int as libc::c_uint;
}

pub unsafe fn ast_type_struct_complete(
    mut t: *mut ast_type,
    mut ext: *mut externals,
) -> *mut ast_type {
    if !(ast_type_struct_members(t)).is_null() {
        return t;
    }
    let mut tag: *mut libc::c_char = ast_type_struct_tag(t);
    if tag.is_null() {
        panic!();
    }
    return externals_getstruct(ext, tag);
}

pub unsafe fn ast_type_struct_members(mut t: *mut ast_type) -> *mut ast_variable_arr {
    if !((*t).base as libc::c_uint == TYPE_STRUCT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*t).c2rust_unnamed.structunion.members;
}

pub unsafe fn ast_type_struct_tag(mut t: *mut ast_type) -> *mut libc::c_char {
    if !((*t).base as libc::c_uint == TYPE_STRUCT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*t).c2rust_unnamed.structunion.tag;
}

pub unsafe fn ast_type_create_struct_anonym(mut members: *mut ast_variable_arr) -> *mut ast_type {
    return ast_type_create_struct(0 as *mut libc::c_char, members);
}

pub unsafe fn ast_type_create_struct_partial(mut tag: *mut libc::c_char) -> *mut ast_type {
    return ast_type_create_struct(tag, 0 as *mut ast_variable_arr);
}

pub unsafe fn ast_type_copy_struct(mut old: *mut ast_type) -> *mut ast_type {
    if !((*old).base as libc::c_uint == TYPE_STRUCT as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    let mut new: *mut ast_type = ast_type_create(
        TYPE_STRUCT,
        (*old).mod_0 as libc::c_uint as ast_type_modifier,
    );
    (*new).c2rust_unnamed.structunion.tag = if !((*old).c2rust_unnamed.structunion.tag).is_null() {
        dynamic_str((*old).c2rust_unnamed.structunion.tag)
    } else {
        0 as *mut libc::c_char
    };
    (*new).c2rust_unnamed.structunion.members =
        if !((*old).c2rust_unnamed.structunion.members).is_null() {
            ast_variable_arr_copy((*old).c2rust_unnamed.structunion.members)
        } else {
            0 as *mut ast_variable_arr
        };
    return new;
}

pub unsafe fn ast_type_mod_or(mut t: *mut ast_type, mut m: ast_type_modifier) {
    (*t).mod_0 = ((*t).mod_0 as libc::c_uint | m as libc::c_uint) as libc::c_int;
}

pub unsafe fn ast_type_istypedef(mut t: *mut ast_type) -> bool {
    ((*t).mod_0 as libc::c_uint as ast_type_modifier) & MOD_TYPEDEF != 0
}

pub unsafe fn ast_type_destroy(mut t: *mut ast_type) {
    match (*t).base as libc::c_uint {
        9 => {
            if ((*t).c2rust_unnamed.ptr_type).is_null() {
                panic!();
            }
            ast_type_destroy((*t).c2rust_unnamed.ptr_type);
        }
        10 => {
            if ((*t).c2rust_unnamed.arr.type_0).is_null() {
                panic!();
            }
            ast_type_destroy((*t).c2rust_unnamed.arr.type_0);
        }
        _ => {}
    }
    free(t as *mut libc::c_void);
}

pub unsafe fn ast_type_copy(mut t: *mut ast_type) -> *mut ast_type {
    if t.is_null() {
        panic!();
    }
    match (*t).base as libc::c_uint {
        9 => return ast_type_create_ptr(ast_type_copy((*t).c2rust_unnamed.ptr_type)),
        10 => {
            return ast_type_create_arr(
                ast_type_copy((*t).c2rust_unnamed.arr.type_0),
                (*t).c2rust_unnamed.arr.length,
            );
        }
        11 => return ast_type_copy_struct(t),
        14 => return ast_type_create_userdef(dynamic_str((*t).c2rust_unnamed.userdef)),
        0 | 3 | 1 => {
            return ast_type_create((*t).base, (*t).mod_0 as libc::c_uint as ast_type_modifier)
        }
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_type_str(mut t: *mut ast_type) -> *mut libc::c_char {
    if t.is_null() {
        panic!();
    }
    let mut basestr: [*const libc::c_char; 9] = [
        b"void\0" as *const u8 as *const libc::c_char,
        b"char\0" as *const u8 as *const libc::c_char,
        b"short\0" as *const u8 as *const libc::c_char,
        b"int\0" as *const u8 as *const libc::c_char,
        b"long\0" as *const u8 as *const libc::c_char,
        b"float\0" as *const u8 as *const libc::c_char,
        b"double\0" as *const u8 as *const libc::c_char,
        b"signed\0" as *const u8 as *const libc::c_char,
        b"unsigned\0" as *const u8 as *const libc::c_char,
    ];
    let mut b: *mut strbuilder = strbuilder_create();
    let mut mod_0: *mut libc::c_char = mod_str((*t).mod_0);
    strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, mod_0);
    free(mod_0 as *mut libc::c_void);
    match (*t).base as libc::c_uint {
        9 => {
            ast_type_str_build_ptr(b, t);
        }
        10 => {
            ast_type_str_build_arr(b, t);
        }
        11 => {
            ast_type_str_build_struct(b, t);
        }
        14 => {
            strbuilder_printf(
                b,
                b"%s\0" as *const u8 as *const libc::c_char,
                (*t).c2rust_unnamed.userdef,
            );
        }
        _ => {
            strbuilder_printf(b, basestr[(*t).base as usize]);
        }
    }
    return strbuilder_build(b);
}
unsafe fn mod_str(mut mod_0: libc::c_int) -> *mut libc::c_char {
    let mut modstr: [*const libc::c_char; 65] = [
        0 as *const libc::c_char,
        b"typedef\0" as *const u8 as *const libc::c_char,
        b"extern\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        b"static\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"auto\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"register\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"const\0" as *const u8 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        0 as *const libc::c_char,
        b"volatile\0" as *const u8 as *const libc::c_char,
    ];
    let modlen: libc::c_int = 7 as libc::c_int;
    let mut b: *mut strbuilder = strbuilder_create();
    let mut nmods: libc::c_int = 0 as libc::c_int;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < modlen {
        if (1 as libc::c_int) << i & mod_0 != 0 {
            nmods += 1;
        }
        i += 1;
    }
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < modlen {
        let mut m: libc::c_int = (1 as libc::c_int) << i_0;
        if m & mod_0 != 0 {
            let fresh11 = nmods;
            nmods = nmods - 1;
            let mut space: *mut libc::c_char = (if fresh11 != 0 {
                b" \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            }) as *mut libc::c_char;
            strbuilder_printf(
                b,
                b"%s%s\0" as *const u8 as *const libc::c_char,
                modstr[m as usize],
                space,
            );
        }
        i_0 += 1;
    }
    return strbuilder_build(b);
}
unsafe fn ast_type_str_build_ptr(mut b: *mut strbuilder, mut t: *mut ast_type) {
    let mut base: *mut libc::c_char = ast_type_str((*t).c2rust_unnamed.ptr_type);
    let mut space: bool = (*(*t).c2rust_unnamed.ptr_type).base as libc::c_uint
        != TYPE_POINTER as libc::c_int as libc::c_uint;
    strbuilder_printf(
        b,
        b"%s%s*\0" as *const u8 as *const libc::c_char,
        base,
        if space as libc::c_int != 0 {
            b" \0" as *const u8 as *const libc::c_char
        } else {
            b"\0" as *const u8 as *const libc::c_char
        },
    );
    free(base as *mut libc::c_void);
}
unsafe fn ast_type_str_build_arr(mut b: *mut strbuilder, mut t: *mut ast_type) {
    let mut base: *mut libc::c_char = ast_type_str((*t).c2rust_unnamed.arr.type_0);
    strbuilder_printf(
        b,
        b"%s[%d]\0" as *const u8 as *const libc::c_char,
        base,
        (*t).c2rust_unnamed.arr.length,
    );
    free(base as *mut libc::c_void);
}
unsafe fn ast_type_str_build_struct(mut b: *mut strbuilder, mut t: *mut ast_type) {
    let mut tag: *mut libc::c_char = (*t).c2rust_unnamed.structunion.tag;
    let mut members: *mut ast_variable_arr = (*t).c2rust_unnamed.structunion.members;
    if !(!tag.is_null() || !members.is_null()) {
        panic!();
    }
    strbuilder_printf(b, b"struct \0" as *const u8 as *const libc::c_char);
    if !tag.is_null() {
        strbuilder_printf(b, tag);
    }
    if members.is_null() {
        return;
    }
    strbuilder_printf(b, b" { \0" as *const u8 as *const libc::c_char);
    let mut n: libc::c_int = ast_variable_arr_n(members);
    let mut v: *mut *mut ast_variable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut s: *mut libc::c_char = ast_variable_str(*v.offset(i as isize));
        strbuilder_printf(b, b"%s; \0" as *const u8 as *const libc::c_char, s);
        free(s as *mut libc::c_void);
        i += 1;
    }
    strbuilder_printf(b, b"}\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn ast_type_base(mut t: *mut ast_type) -> ast_type_base {
    return (*t).base;
}

pub unsafe fn ast_type_ptr_type(mut t: *mut ast_type) -> *mut ast_type {
    if !((*t).base as libc::c_uint == TYPE_POINTER as libc::c_int as libc::c_uint) as libc::c_int
        as libc::c_long
        != 0
    {
        panic!();
    }
    return (*t).c2rust_unnamed.ptr_type;
}

pub unsafe fn ast_variable_create(
    mut name: *mut libc::c_char,
    mut type_0: *mut ast_type,
) -> *mut ast_variable {
    let mut v: *mut ast_variable =
        malloc(::core::mem::size_of::<ast_variable>()) as *mut ast_variable;
    (*v).name = name;
    (*v).type_0 = type_0;
    return v;
}

pub unsafe fn ast_variable_destroy(mut v: *mut ast_variable) {
    ast_type_destroy((*v).type_0);
    free((*v).name as *mut libc::c_void);
    free(v as *mut libc::c_void);
}

pub unsafe fn ast_variable_copy(mut v: *mut ast_variable) -> *mut ast_variable {
    if v.is_null() {
        panic!();
    }
    return ast_variable_create(dynamic_str((*v).name), ast_type_copy((*v).type_0));
}

pub unsafe fn ast_variables_copy(
    mut n: libc::c_int,
    mut v: *mut *mut ast_variable,
) -> *mut *mut ast_variable {
    if !(!v.is_null() || n == 0) {
        panic!();
    }
    let mut new: *mut *mut ast_variable =
        calloc(n as usize, ::core::mem::size_of::<*mut variable>()) as *mut *mut ast_variable;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let ref mut fresh12 = *new.offset(i as isize);
        *fresh12 = ast_variable_copy(*v.offset(i as isize));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_variable_str(mut v: *mut ast_variable) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut t: *mut libc::c_char = ast_type_str((*v).type_0);
    strbuilder_printf(
        b,
        b"%s %s\0" as *const u8 as *const libc::c_char,
        t,
        (*v).name,
    );
    free(t as *mut libc::c_void);
    return strbuilder_build(b);
}

pub unsafe fn ast_variable_name(mut v: *mut ast_variable) -> *mut libc::c_char {
    return (*v).name;
}

pub unsafe fn ast_variable_type(mut v: *mut ast_variable) -> *mut ast_type {
    return (*v).type_0;
}

pub unsafe fn ast_variable_arr_create() -> *mut ast_variable_arr {
    return calloc(1, ::core::mem::size_of::<ast_variable_arr>()) as *mut ast_variable_arr;
}

pub unsafe fn ast_variable_arr_append(mut arr: *mut ast_variable_arr, mut v: *mut ast_variable) {
    (*arr).n += 1;
    (*arr).v = realloc(
        (*arr).v as *mut libc::c_void,
        (::core::mem::size_of::<*mut ast_variable>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut ast_variable;
    let ref mut fresh13 = *((*arr).v).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh13 = v;
}

pub unsafe fn ast_variable_arr_destroy(mut arr: *mut ast_variable_arr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        ast_variable_destroy(*((*arr).v).offset(i as isize));
        i += 1;
    }
    free(arr as *mut libc::c_void);
}

pub unsafe fn ast_variable_arr_n(mut arr: *mut ast_variable_arr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn ast_variable_arr_v(mut arr: *mut ast_variable_arr) -> *mut *mut ast_variable {
    return (*arr).v;
}

pub unsafe fn ast_variable_arr_copy(mut old: *mut ast_variable_arr) -> *mut ast_variable_arr {
    let mut new: *mut ast_variable_arr = ast_variable_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        ast_variable_arr_append(new, ast_variable_copy(*((*old).v).offset(i as isize)));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_function_create(
    mut isaxiom: bool,
    mut ret: *mut ast_type,
    mut name: *mut libc::c_char,
    mut nparam: libc::c_int,
    mut param: *mut *mut ast_variable,
    mut abstract_0: *mut ast_block,
    mut body: *mut ast_block,
) -> *mut ast_function {
    let mut f: *mut ast_function =
        malloc(::core::mem::size_of::<ast_function>()) as *mut ast_function;
    (*f).isaxiom = isaxiom;
    (*f).ret = ret;
    (*f).name = name;
    (*f).nparam = nparam;
    (*f).param = param;
    if abstract_0.is_null() {
        panic!();
    }
    (*f).abstract_0 = abstract_0;
    (*f).body = body;
    return f;
}

pub unsafe fn ast_function_destroy(mut f: *mut ast_function) {
    ast_type_destroy((*f).ret);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*f).nparam {
        ast_variable_destroy(*((*f).param).offset(i as isize));
        i += 1;
    }
    ast_block_destroy((*f).abstract_0);
    if !((*f).body).is_null() {
        ast_block_destroy((*f).body);
    }
    free((*f).param as *mut libc::c_void);
    free((*f).name as *mut libc::c_void);
    free(f as *mut libc::c_void);
}

pub unsafe fn ast_function_str(mut f: *mut ast_function) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    if (*f).isaxiom {
        strbuilder_printf(b, b"axiom \0" as *const u8 as *const libc::c_char);
    }
    let mut ret: *mut libc::c_char = ast_type_str((*f).ret);
    strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, ret);
    free(ret as *mut libc::c_void);
    strbuilder_printf(b, b"%s(\0" as *const u8 as *const libc::c_char, (*f).name);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*f).nparam {
        let mut v: *mut libc::c_char = ast_variable_str(*((*f).param).offset(i as isize));
        let mut space: *mut libc::c_char = (if (i + 1 as libc::c_int) < (*f).nparam {
            b", \0" as *const u8 as *const libc::c_char
        } else {
            b"\0" as *const u8 as *const libc::c_char
        }) as *mut libc::c_char;
        strbuilder_printf(b, b"%s%s\0" as *const u8 as *const libc::c_char, v, space);
        free(v as *mut libc::c_void);
        i += 1;
    }
    let mut abs: *mut libc::c_char = ast_block_str(
        (*f).abstract_0,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    strbuilder_printf(b, b") ~ [\n%s]\0" as *const u8 as *const libc::c_char, abs);
    free(abs as *mut libc::c_void);
    if !((*f).body).is_null() {
        let mut body: *mut libc::c_char = ast_block_str(
            (*f).body,
            b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
        strbuilder_printf(b, b"{\n%s}\0" as *const u8 as *const libc::c_char, body);
        free(body as *mut libc::c_void);
    } else {
        strbuilder_printf(b, b";\0" as *const u8 as *const libc::c_char);
    }
    strbuilder_printf(b, b"\n\0" as *const u8 as *const libc::c_char);
    return strbuilder_build(b);
}

pub unsafe fn ast_function_name(mut f: *mut ast_function) -> *mut libc::c_char {
    return (*f).name;
}

pub unsafe fn ast_function_copy(mut f: *mut ast_function) -> *mut ast_function {
    if f.is_null() {
        panic!();
    }
    let mut param: *mut *mut ast_variable =
        malloc((::core::mem::size_of::<*mut ast_variable>()).wrapping_mul((*f).nparam as usize))
            as *mut *mut ast_variable;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*f).nparam {
        let ref mut fresh14 = *param.offset(i as isize);
        *fresh14 = ast_variable_copy(*((*f).param).offset(i as isize));
        i += 1;
    }
    return ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        dynamic_str((*f).name),
        (*f).nparam,
        param,
        ast_block_copy((*f).abstract_0),
        if !((*f).body).is_null() {
            ast_block_copy((*f).body)
        } else {
            0 as *mut ast_block
        },
    );
}

pub unsafe fn ast_function_isaxiom(mut f: *mut ast_function) -> bool {
    return (*f).isaxiom;
}

pub unsafe fn ast_function_isproto(mut f: *mut ast_function) -> bool {
    return !((*f).abstract_0).is_null() && ((*f).body).is_null();
}

pub unsafe fn ast_function_absisempty(mut f: *mut ast_function) -> bool {
    return ast_block_ndecls((*f).abstract_0) == 0 as libc::c_int
        && ast_block_nstmts((*f).abstract_0) == 0 as libc::c_int;
}

pub unsafe fn ast_function_type(mut f: *mut ast_function) -> *mut ast_type {
    return (*f).ret;
}

pub unsafe fn ast_function_body(mut f: *mut ast_function) -> *mut ast_block {
    if ((*f).body).is_null() {
        fprintf(
            __stderrp,
            b"cannot find body for `%s'\n\0" as *const u8 as *const libc::c_char,
            (*f).name,
        );
    }
    if ((*f).body).is_null() {
        panic!();
    }
    return (*f).body;
}

pub unsafe fn ast_function_abstract(mut f: *mut ast_function) -> *mut ast_block {
    if ((*f).abstract_0).is_null() {
        panic!();
    }
    return (*f).abstract_0;
}

pub unsafe fn ast_function_nparams(mut f: *mut ast_function) -> libc::c_int {
    return (*f).nparam;
}

pub unsafe fn ast_function_params(mut f: *mut ast_function) -> *mut *mut ast_variable {
    return (*f).param;
}

pub unsafe fn ast_function_preconditions(mut f: *mut ast_function) -> preconds_result {
    return ast_block_preconds(ast_function_abstract(f));
}

pub unsafe fn ast_function_protostitch(
    mut f: *mut ast_function,
    mut ext: *mut externals,
) -> *mut ast_function {
    let mut proto: *mut ast_function = externals_getfunc(ext, (*f).name);
    if !proto.is_null() && !((*proto).abstract_0).is_null() {
        (*f).abstract_0 = ast_block_copy((*proto).abstract_0);
    }
    return f;
}

pub unsafe fn ast_function_verify(mut f: *mut ast_function, mut ext: *mut externals) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut state: *mut state =
        state_create(dynamic_str(ast_function_name(f)), ext, ast_function_type(f));
    err = ast_function_initparams(f, state);
    if !err.is_null() {
        return err;
    }
    err = path_absverify_withstate(f, state);
    if !err.is_null() {
        return err;
    }
    state_destroy(state);
    return 0 as *mut error;
}
unsafe fn path_absverify_withstate(mut f: *mut ast_function, mut state: *mut state) -> *mut error {
    let mut abs: *mut ast_block = ast_function_abstract(f);
    let mut ndecls: libc::c_int = ast_block_ndecls(abs);
    let mut var: *mut *mut ast_variable = ast_block_decls(abs);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ndecls {
        state_declare(state, *var.offset(i as isize), 0 as libc::c_int != 0);
        i += 1;
    }
    return path_absverify(f, state, 0 as libc::c_int);
}
unsafe fn path_absverify(
    mut f: *mut ast_function,
    mut state: *mut state,
    mut index: libc::c_int,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut abs: *mut ast_block = ast_function_abstract(f);
    let mut nstmts: libc::c_int = ast_block_nstmts(abs);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts(abs);
    let mut i: libc::c_int = index;
    while i < nstmts {
        let mut splits: ast_stmt_splits = ast_stmt_splits(*stmt.offset(i as isize), state);
        if !(splits.err).is_null() {
            return splits.err;
        }
        if splits.n != 0 {
            if (splits.cond).is_null() {
                panic!();
            }
            return split_paths_absverify(f, state, i, &mut splits);
        }
        if !ast_stmt_ispre(*stmt.offset(i as isize)) {
            let mut res: *mut result =
                ast_stmt_absexec(*stmt.offset(i as isize), state, 1 as libc::c_int != 0);
            if result_iserror(res) {
                return result_as_error(res);
            }
        }
        i += 1;
    }
    err = abstract_audit(f, state);
    if !err.is_null() {
        return err;
    }
    return 0 as *mut error;
}

pub unsafe fn ast_function_initparams(mut f: *mut ast_function, mut s: *mut state) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut params: *mut *mut ast_variable = ast_function_params(f);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nparams {
        state_declare(s, *params.offset(i as isize), 1 as libc::c_int != 0);
        i += 1;
    }
    err = ast_function_precondsinit(f, s);
    if !err.is_null() {
        return err;
    }
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nparams {
        err = inititalise_param(*params.offset(i_0 as isize), s);
        if !err.is_null() {
            return err;
        }
        i_0 += 1;
    }
    return 0 as *mut error;
}
unsafe fn ast_function_precondsinit(mut f: *mut ast_function, mut s: *mut state) -> *mut error {
    let mut pre: preconds_result = ast_function_preconditions(f);
    if !(pre.err).is_null() {
        return pre.err;
    }
    if (pre.stmt).is_null() {
        return 0 as *mut error;
    }
    let mut res: *mut result = ast_stmt_absexec(pre.stmt, s, 1 as libc::c_int != 0);
    if result_iserror(res) {
        return result_as_error(res);
    }
    return 0 as *mut error;
}
unsafe fn inititalise_param(mut param: *mut ast_variable, mut state: *mut state) -> *mut error {
    let mut name: *mut libc::c_char = ast_variable_name(param);
    let mut t: *mut ast_type = ast_variable_type(param);
    let mut obj: *mut object = state_getobject(state, name);
    if obj.is_null() {
        panic!();
    }
    if !object_hasvalue(obj) {
        let mut val: *mut value = state_vconst(state, t, dynamic_str(name), 1 as libc::c_int != 0);
        object_assign(obj, val);
    }
    return 0 as *mut error;
}
unsafe fn abstract_audit(mut f: *mut ast_function, mut abstract_state: *mut state) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut actual_state: *mut state = state_create_withprops(
        dynamic_str(ast_function_name(f)),
        state_getext(abstract_state),
        ast_function_type(f),
        state_getprops(abstract_state),
    );
    err = ast_function_initparams(f, actual_state);
    if !err.is_null() {
        panic!();
    }
    err = ast_function_setupabsexec(f, actual_state);
    if !err.is_null() {
        return err;
    }
    err = abstract_auditwithstate(f, actual_state, abstract_state);
    if !err.is_null() {
        return err;
    }
    state_destroy(actual_state);
    return 0 as *mut error;
}
unsafe fn ast_function_setupabsexec(mut f: *mut ast_function, mut state: *mut state) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut nstmts: libc::c_int = ast_block_nstmts((*f).abstract_0);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts((*f).abstract_0);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        err = ast_stmt_setupabsexec(*stmt.offset(i as isize), state);
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn abstract_auditwithstate(
    mut f: *mut ast_function,
    mut actual_state: *mut state,
    mut abstract_state: *mut state,
) -> *mut error {
    let mut ndecls: libc::c_int = ast_block_ndecls((*f).body);
    let mut var: *mut *mut ast_variable = ast_block_decls((*f).body);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ndecls {
        state_declare(actual_state, *var.offset(i as isize), 0 as libc::c_int != 0);
        i += 1;
    }
    return path_verify(f, actual_state, 0 as libc::c_int, abstract_state);
}
unsafe fn path_verify(
    mut f: *mut ast_function,
    mut actual_state: *mut state,
    mut index: libc::c_int,
    mut abstract_state: *mut state,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut fname: *mut libc::c_char = ast_function_name(f);
    let mut nstmts: libc::c_int = ast_block_nstmts((*f).body);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts((*f).body);
    let mut i: libc::c_int = index;
    while i < nstmts {
        let mut splits: ast_stmt_splits = ast_stmt_splits(*stmt.offset(i as isize), actual_state);
        if splits.n != 0 {
            return split_paths_verify(f, actual_state, i, &mut splits, abstract_state);
        }
        err = ast_stmt_process(*stmt.offset(i as isize), fname, actual_state);
        if !err.is_null() {
            return err;
        }
        if ast_stmt_isterminal(*stmt.offset(i as isize), actual_state) {
            break;
        }
        i += 1;
    }
    if state_hasgarbage(actual_state) {
        v_printf(
            b"actual: %s\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
            state_str(actual_state),
        );
        let mut b: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"%s: garbage on heap\0" as *const u8 as *const libc::c_char,
            ast_function_name(f),
        );
        return error_create(strbuilder_build(b));
    }
    let mut equiv: bool = state_equal(actual_state, abstract_state);
    if !equiv {
        let mut b_0: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b_0,
            b"%s: actual and abstract states differ\0" as *const u8 as *const libc::c_char,
            ast_function_name(f),
        );
        return error_create(strbuilder_build(b_0));
    }
    return 0 as *mut error;
}

pub unsafe fn ast_function_absexec(mut f: *mut ast_function, mut state: *mut state) -> *mut result {
    let mut ndecls: libc::c_int = ast_block_ndecls((*f).abstract_0);
    if ndecls != 0 {
        let mut var: *mut *mut ast_variable = ast_block_decls((*f).abstract_0);
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < ndecls {
            state_declare(state, *var.offset(i as isize), 0 as libc::c_int != 0);
            i += 1;
        }
    }
    let mut nstmts: libc::c_int = ast_block_nstmts((*f).abstract_0);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts((*f).abstract_0);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nstmts {
        let mut res: *mut result =
            ast_stmt_absexec(*stmt.offset(i_0 as isize), state, 0 as libc::c_int != 0);
        if result_iserror(res) {
            return res;
        }
        i_0 += 1;
    }
    let mut obj: *mut object = state_getresult(state);
    if obj.is_null() {
        panic!();
    }
    return result_value_create(object_as_value(obj));
}
unsafe fn split_path_verify(
    mut f: *mut ast_function,
    mut actual_state: *mut state,
    mut index: libc::c_int,
    mut cond: *mut ast_expr,
    mut abstract_state: *mut state,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut paths: *mut ast_function_arr = body_paths(f, index, cond);
    let mut n: libc::c_int = ast_function_arr_len(paths);
    if !(n == 2 as libc::c_int) {
        panic!();
    }
    let mut func: *mut *mut ast_function = ast_function_arr_func(paths);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut actual_copy: *mut state =
            state_copywithname(actual_state, ast_function_name(*func.offset(i as isize)));
        let mut abstract_copy: *mut state =
            state_copywithname(abstract_state, ast_function_name(*func.offset(i as isize)));
        let mut r: *mut preresult = ast_expr_assume(
            ast_expr_inverted_copy(cond, i == 1 as libc::c_int),
            actual_copy,
        );
        if preresult_iserror(r) {
            return preresult_as_error(r);
        }
        if !preresult_iscontradiction(r) {
            err = path_verify(*func.offset(i as isize), actual_copy, index, abstract_copy);
            if !err.is_null() {
                return err;
            }
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn recurse_buildgraph(
    mut g: &mut map,
    mut dedup: &mut map,
    mut fname: *mut libc::c_char,
    mut ext: *mut externals,
) {
    let mut local_dedup = map::new();
    if !(dedup.get(fname)).is_null() {
        return;
    }
    dedup.set(fname, 1 as libc::c_int as *mut libc::c_void);
    let mut f: *mut ast_function = externals_getfunc(ext, fname);
    if f.is_null() {
        fprintf(
            __stderrp,
            b"function `%s' is not declared\n\0" as *const u8 as *const libc::c_char,
            fname,
        );
        exit(1 as libc::c_int);
    }
    if f.is_null() {
        panic!();
    }
    if (*f).isaxiom {
        return;
    }
    if ((*f).body).is_null() {
        panic!();
    }
    let mut body: *mut ast_block = (*f).body;
    let mut nstmts: libc::c_int = ast_block_nstmts(body);
    let mut stmt: *mut *mut ast_stmt = ast_block_stmts(body);
    if stmt.is_null() {
        panic!();
    }
    let mut val = string_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        let mut farr = ast_stmt_getfuncs(*stmt.offset(i as isize));
        let mut func: *mut *mut libc::c_char = string_arr_s(&mut farr);
        let mut j: libc::c_int = 0 as libc::c_int;
        while j < string_arr_n(&farr) {
            if (local_dedup.get(*func.offset(j as isize))).is_null() {
                let mut f_0: *mut ast_function = externals_getfunc(ext, *func.offset(j as isize));
                if !(*f_0).isaxiom {
                    string_arr_append(&mut val, *func.offset(j as isize));
                }
                local_dedup.set(
                    *func.offset(j as isize),
                    1 as libc::c_int as *mut libc::c_void,
                );
                recurse_buildgraph(g, dedup, *func.offset(j as isize), ext);
            }
            j += 1;
        }
        i += 1;
    }
    g.set(
        dynamic_str(fname),
        Box::into_raw(val) as *const libc::c_void,
    );
}
unsafe fn abstract_paths(
    mut f: *mut ast_function,
    mut index: libc::c_int,
    mut cond: *mut ast_expr,
) -> *mut ast_function_arr {
    let mut res: *mut ast_function_arr = ast_function_arr_create();
    let mut f_true: *mut ast_function = ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        split_name((*f).name, cond),
        (*f).nparam,
        ast_variables_copy((*f).nparam, (*f).param),
        ast_block_copy((*f).abstract_0),
        ast_block_copy((*f).body),
    );
    let mut inv_assumption: *mut ast_expr = ast_expr_inverted_copy(cond, 1 as libc::c_int != 0);
    let mut f_false: *mut ast_function = ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        split_name((*f).name, inv_assumption),
        (*f).nparam,
        ast_variables_copy((*f).nparam, (*f).param),
        ast_block_copy((*f).abstract_0),
        ast_block_copy((*f).body),
    );
    ast_function_arr_append(res, f_true);
    ast_function_arr_append(res, f_false);
    return res;
}
unsafe fn split_path_absverify(
    mut f: *mut ast_function,
    mut state: *mut state,
    mut index: libc::c_int,
    mut cond: *mut ast_expr,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut paths: *mut ast_function_arr = abstract_paths(f, index, cond);
    let mut n: libc::c_int = ast_function_arr_len(paths);
    if !(n == 2 as libc::c_int) {
        panic!();
    }
    let mut func: *mut *mut ast_function = ast_function_arr_func(paths);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut s_copy: *mut state =
            state_copywithname(state, ast_function_name(*func.offset(i as isize)));
        let mut r: *mut preresult =
            ast_expr_assume(ast_expr_inverted_copy(cond, i == 1 as libc::c_int), s_copy);
        if preresult_iserror(r) {
            return preresult_as_error(r);
        }
        if !preresult_iscontradiction(r) {
            err = path_absverify(*func.offset(i as isize), s_copy, index);
            if !err.is_null() {
                return err;
            }
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn split_paths_absverify(
    mut f: *mut ast_function,
    mut state: *mut state,
    mut index: libc::c_int,
    mut splits: *mut ast_stmt_splits,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*splits).n {
        err = split_path_absverify(f, state, index, *((*splits).cond).offset(i as isize));
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut error;
}

pub unsafe fn ast_function_buildgraph(
    mut fname: *mut libc::c_char,
    mut ext: *mut externals,
) -> Box<map> {
    let mut dedup = map::new();
    let mut g = map::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    return g;
}
unsafe fn split_name(
    mut name: *mut libc::c_char,
    mut assumption: *mut ast_expr,
) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut assumption_str: *mut libc::c_char = ast_expr_str(assumption);
    strbuilder_printf(
        b,
        b"%s | %s\0" as *const u8 as *const libc::c_char,
        name,
        assumption_str,
    );
    free(assumption_str as *mut libc::c_void);
    return strbuilder_build(b);
}
unsafe fn split_paths_verify(
    mut f: *mut ast_function,
    mut actual_state: *mut state,
    mut index: libc::c_int,
    mut splits: *mut ast_stmt_splits,
    mut abstract_state: *mut state,
) -> *mut error {
    let mut err: *mut error = 0 as *mut error;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*splits).n {
        err = split_path_verify(
            f,
            actual_state,
            index,
            *((*splits).cond).offset(i as isize),
            abstract_state,
        );
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut error;
}
unsafe fn body_paths(
    mut f: *mut ast_function,
    mut index: libc::c_int,
    mut cond: *mut ast_expr,
) -> *mut ast_function_arr {
    let mut res: *mut ast_function_arr = ast_function_arr_create();
    let mut f_true: *mut ast_function = ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        split_name((*f).name, cond),
        (*f).nparam,
        ast_variables_copy((*f).nparam, (*f).param),
        ast_block_copy((*f).abstract_0),
        (*f).body,
    );
    let mut inv_assumption: *mut ast_expr = ast_expr_inverted_copy(cond, 1 as libc::c_int != 0);
    let mut f_false: *mut ast_function = ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        split_name((*f).name, inv_assumption),
        (*f).nparam,
        ast_variables_copy((*f).nparam, (*f).param),
        ast_block_copy((*f).abstract_0),
        (*f).body,
    );
    ast_function_arr_append(res, f_true);
    ast_function_arr_append(res, f_false);
    return res;
}

pub unsafe fn ast_function_arr_create() -> *mut ast_function_arr {
    return calloc(1, ::core::mem::size_of::<ast_function_arr>()) as *mut ast_function_arr;
}

pub unsafe fn ast_function_arr_copy(mut old: *mut ast_function_arr) -> *mut ast_function_arr {
    let mut new: *mut ast_function_arr = ast_function_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        ast_function_arr_append(new, ast_function_copy(*((*old).f).offset(i as isize)));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_function_arr_destroy(mut arr: *mut ast_function_arr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        ast_function_destroy(*((*arr).f).offset(i as isize));
        i += 1;
    }
    free(arr as *mut libc::c_void);
}

pub unsafe fn ast_function_arr_append(mut arr: *mut ast_function_arr, mut f: *mut ast_function) {
    (*arr).n += 1;
    (*arr).f = realloc(
        (*arr).f as *mut libc::c_void,
        (::core::mem::size_of::<*mut ast_function>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut ast_function;
    let ref mut fresh15 = *((*arr).f).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh15 = f;
}

pub unsafe fn ast_function_arr_appendrange(
    mut arr: *mut ast_function_arr,
    mut range: *mut ast_function_arr,
) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*range).n {
        ast_function_arr_append(arr, *((*range).f).offset(i as isize));
        i += 1;
    }
}

pub unsafe fn ast_function_arr_len(mut arr: *mut ast_function_arr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn ast_function_arr_func(mut arr: *mut ast_function_arr) -> *mut *mut ast_function {
    return (*arr).f;
}

pub unsafe fn ast_functiondecl_create(mut f: *mut ast_function) -> *mut ast_externdecl {
    let mut decl: *mut ast_externdecl =
        malloc(::core::mem::size_of::<ast_externdecl>()) as *mut ast_externdecl;
    (*decl).kind = EXTERN_FUNCTION;
    (*decl).c2rust_unnamed.function = f;
    return decl;
}

pub unsafe fn ast_externdecl_isfunction(mut decl: *mut ast_externdecl) -> bool {
    return (*decl).kind as libc::c_uint == EXTERN_FUNCTION as libc::c_int as libc::c_uint;
}

pub unsafe fn ast_externdecl_as_function(mut decl: *mut ast_externdecl) -> *mut ast_function {
    if !((*decl).kind as libc::c_uint == EXTERN_FUNCTION as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*decl).c2rust_unnamed.function;
}

pub unsafe fn ast_decl_create(
    mut name: *mut libc::c_char,
    mut t: *mut ast_type,
) -> *mut ast_externdecl {
    let mut decl: *mut ast_externdecl =
        malloc(::core::mem::size_of::<ast_externdecl>()) as *mut ast_externdecl;
    if ast_type_istypedef(t) {
        (*decl).kind = EXTERN_TYPEDEF;
        (*decl).c2rust_unnamed._typedef.name = name;
        (*decl).c2rust_unnamed._typedef.type_0 = t;
    } else if ast_type_isstruct(t) {
        if (ast_type_struct_tag(t)).is_null() {
            panic!();
        }
        (*decl).kind = EXTERN_STRUCT;
        (*decl).c2rust_unnamed._struct = t;
    } else {
        (*decl).kind = EXTERN_VARIABLE;
        (*decl).c2rust_unnamed.variable = ast_variable_create(name, t);
    }
    return decl;
}

pub unsafe fn ast_externdecl_install(mut decl: *mut ast_externdecl, mut ext: *mut externals) {
    let mut f: *mut ast_function = 0 as *mut ast_function;
    let mut v: *mut ast_variable = 0 as *mut ast_variable;
    match (*decl).kind as libc::c_uint {
        0 => {
            f = (*decl).c2rust_unnamed.function;
            externals_declarefunc(ext, ast_function_name(f), f);
        }
        1 => {
            v = (*decl).c2rust_unnamed.variable;
            externals_declarevar(ext, ast_variable_name(v), v);
        }
        2 => {
            externals_declaretypedef(
                ext,
                (*decl).c2rust_unnamed._typedef.name,
                (*decl).c2rust_unnamed._typedef.type_0,
            );
        }
        3 => {
            externals_declarestruct(ext, (*decl).c2rust_unnamed._struct);
        }
        _ => {
            panic!();
        }
    };
}

pub unsafe fn ast_externdecl_destroy(mut decl: *mut ast_externdecl) {
    match (*decl).kind as libc::c_uint {
        0 => {
            ast_function_destroy((*decl).c2rust_unnamed.function);
        }
        1 => {
            ast_variable_destroy((*decl).c2rust_unnamed.variable);
        }
        2 => {
            free((*decl).c2rust_unnamed._typedef.name as *mut libc::c_void);
            ast_type_destroy((*decl).c2rust_unnamed._typedef.type_0);
        }
        3 => {
            ast_type_destroy((*decl).c2rust_unnamed._struct);
        }
        _ => {
            panic!();
        }
    }
    free(decl as *mut libc::c_void);
}

pub unsafe fn parse_int(mut s: *mut libc::c_char) -> libc::c_int {
    let mut n: libc::c_int = 0 as libc::c_int;
    while *s != 0 {
        n = 10 as libc::c_int * n + (*s as libc::c_int - '0' as i32);
        s = s.offset(1);
    }
    return n;
}

pub unsafe fn parse_char(mut s: *mut libc::c_char) -> libc::c_char {
    if !(strlen(s) >= 3 && *s.offset(0 as libc::c_int as isize) as libc::c_int == '\'' as i32) {
        panic!();
    }
    match *s.offset(1 as libc::c_int as isize) as libc::c_int {
        92 => {
            if !(*s.offset(3 as libc::c_int as isize) as libc::c_int == '\'' as i32) {
                panic!();
            }
            return parse_escape(*s.offset(2 as libc::c_int as isize)) as libc::c_char;
        }
        _ => {
            if !(*s.offset(2 as libc::c_int as isize) as libc::c_int == '\'' as i32) {
                panic!();
            }
            return *s.offset(1 as libc::c_int as isize);
        }
    };
}

pub unsafe fn parse_escape(mut c: libc::c_char) -> libc::c_int {
    match c as libc::c_int {
        48 => return '\0' as i32,
        116 => return '\t' as i32,
        110 => return '\t' as i32,
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_create(mut decl: *mut ast_externdecl) -> *mut ast {
    let mut node: *mut ast = calloc(1, ::core::mem::size_of::<ast>()) as *mut ast;
    return ast_append(node, decl);
}

pub unsafe fn ast_destroy(mut node: *mut ast) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*node).n {
        ast_externdecl_destroy(*((*node).decl).offset(i as isize));
        i += 1;
    }
    free((*node).decl as *mut libc::c_void);
    free(node as *mut libc::c_void);
}

pub unsafe fn ast_append(mut node: *mut ast, mut decl: *mut ast_externdecl) -> *mut ast {
    (*node).n += 1;
    (*node).decl = realloc(
        (*node).decl as *mut libc::c_void,
        (::core::mem::size_of::<*mut ast_externdecl>()).wrapping_mul((*node).n as usize),
    ) as *mut *mut ast_externdecl;
    let ref mut fresh16 = *((*node).decl).offset(((*node).n - 1 as libc::c_int) as isize);
    *fresh16 = decl;
    return node;
}

pub unsafe fn result_error_create(mut err: *mut error) -> *mut result {
    if err.is_null() {
        panic!();
    }
    let mut r: *mut result = malloc(::core::mem::size_of::<result>()) as *mut result;
    (*r).val = 0 as *mut value;
    (*r).err = err;
    return r;
}

pub unsafe fn result_value_create(mut val: *mut value) -> *mut result {
    let mut r: *mut result = malloc(::core::mem::size_of::<result>()) as *mut result;
    (*r).val = val;
    (*r).err = 0 as *mut error;
    return r;
}

pub unsafe fn result_destroy(mut res: *mut result) {
    if !((*res).err).is_null() {
        panic!();
    }
    if !((*res).val).is_null() {
        value_destroy((*res).val);
    }
    free(res as *mut libc::c_void);
}

pub unsafe fn result_iserror(mut res: *mut result) -> bool {
    return !((*res).err).is_null();
}

pub unsafe fn result_as_error(mut res: *mut result) -> *mut error {
    if ((*res).err).is_null() {
        panic!();
    }
    return (*res).err;
}

pub unsafe fn result_as_value(mut res: *mut result) -> *mut value {
    if !((*res).err).is_null() {
        panic!();
    }
    return (*res).val;
}

pub unsafe fn result_hasvalue(mut res: *mut result) -> bool {
    if result_iserror(res) {
        panic!();
    }
    return !((*res).val).is_null();
}

pub unsafe fn lvalue_create(mut t: *mut ast_type, mut obj: *mut object) -> *mut lvalue {
    let mut l: *mut lvalue = malloc(::core::mem::size_of::<lvalue>()) as *mut lvalue;
    (*l).t = t;
    (*l).obj = obj;
    return l;
}

pub unsafe fn lvalue_destroy(mut l: *mut lvalue) {
    ast_type_destroy((*l).t);
    object_destroy((*l).obj);
    free(l as *mut libc::c_void);
}

pub unsafe fn lvalue_type(mut l: *mut lvalue) -> *mut ast_type {
    return (*l).t;
}

pub unsafe fn lvalue_object(mut l: *mut lvalue) -> *mut object {
    return (*l).obj;
}

pub unsafe fn preresult_empty_create() -> *mut preresult {
    return calloc(1, ::core::mem::size_of::<preresult>()) as *mut preresult;
}

pub unsafe fn preresult_error_create(mut err: *mut error) -> *mut preresult {
    if err.is_null() {
        panic!();
    }
    let mut r: *mut preresult = preresult_empty_create();
    (*r).err = err;
    return r;
}

pub unsafe fn preresult_contradiction_create() -> *mut preresult {
    let mut r: *mut preresult = preresult_empty_create();
    (*r).iscontradiction = 1 as libc::c_int != 0;
    return r;
}

pub unsafe fn preresult_destroy(mut r: *mut preresult) {
    if !((*r).err).is_null() {
        panic!();
    }
    free(r as *mut libc::c_void);
}

pub unsafe fn preresult_isempty(mut r: *mut preresult) -> bool {
    return !((*r).iscontradiction as libc::c_int != 0 || !((*r).err).is_null());
}

pub unsafe fn preresult_iserror(mut r: *mut preresult) -> bool {
    return !((*r).err).is_null();
}

pub unsafe fn preresult_as_error(mut r: *mut preresult) -> *mut error {
    if ((*r).err).is_null() {
        panic!();
    }
    return (*r).err;
}

pub unsafe fn preresult_iscontradiction(mut r: *mut preresult) -> bool {
    return (*r).iscontradiction;
}

pub unsafe fn ast_topological_order(
    mut fname: *mut libc::c_char,
    mut ext: *mut externals,
) -> Box<string_arr> {
    topological_order(fname, ext)
}

pub unsafe fn ast_protostitch(
    mut f: *mut ast_function,
    mut ext: *mut externals,
) -> *mut ast_function {
    return ast_function_protostitch(f, ext);
}
