#![allow(
    dead_code,
    mutable_transmutes,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_variables
)]

use std::ffi::CStr;

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
use crate::parser::{lexememarker_copy, lexememarker_destroy, lexememarker_str, LexemeMarker};
use crate::props::{props_contradicts, props_get, props_install};
use crate::state::state::{
    state_addresses_deallocand, state_alloc, state_clump, state_copy, state_copywithname,
    state_create, state_create_withprops, state_dealloc, state_declare, state_deref, state_destroy,
    state_equal, state_get, state_getext, state_getloc, state_getobject, state_getobjecttype,
    state_getprops, state_getresult, state_getvconst, state_hasgarbage, state_isalloc,
    state_islval, state_popframe, state_pushframe, state_range_alloc, state_range_aredeallocands,
    state_range_dealloc, state_static_init, state_str, state_vconst, ObjectRes,
};
use crate::util::{
    dynamic_str, error_create, strbuilder_build, strbuilder_create, strbuilder_printf,
    strbuilder_putc, string_arr_append, string_arr_concat, string_arr_contains, string_arr_create,
    string_arr_deque, string_arr_n, string_arr_s, Error, Map, StringArr,
};
use crate::value::{
    value_as_constant, value_as_location, value_as_sync, value_copy, value_destroy, value_equal,
    value_int_create, value_int_indefinite_create, value_isconstant, value_islocation,
    value_isstruct, value_issync, value_literal_create, value_pf_augment,
    value_ptr_indefinite_create, value_str, value_struct_indefinite_create, value_struct_member,
    value_sync_create, value_to_expr, values_comparable,
};
use crate::{
    vprintln, Externals, MathExpr, Object, Props, State, StrBuilder, Value, Variable as variable,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstAllocKind {
    Clump,
    Dealloc,
    Alloc,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstUnaryOp {
    Address,
    Dereference,
    Positive,
    Negative,
    OnesComplement,
    Bang,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstBinaryOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Addition,
    Subtraction,
}

pub struct AstExpr {
    kind: AstExprKind,
    root: *mut AstExpr,
}

#[derive(Copy, Clone)]
pub struct AllocExpr {
    kind: AstAllocKind,
    arg: *mut AstExpr,
}

#[derive(Copy, Clone)]
pub struct BinaryExpr {
    op: AstBinaryOp,
    e1: *mut AstExpr,
    e2: *mut AstExpr,
}

#[derive(Copy, Clone)]
pub struct IncDecExpr {
    inc: libc::c_int,
    pre: libc::c_int,
}

#[derive(Copy, Clone)]
pub struct CallExpr {
    n: libc::c_int,
    arg: *mut *mut AstExpr,
}

#[derive(Copy, Clone)]
pub struct ConstantExpr {
    constant: libc::c_int,
    ischar: bool,
}

#[derive(Copy, Clone)]
enum AstExprKind {
    Identifier(*mut libc::c_char),
    Constant(ConstantExpr),
    StringLiteral(*mut libc::c_char),
    Bracketed,
    Iteration,
    Call(CallExpr),
    IncDec(IncDecExpr),
    StructMember(*mut libc::c_char),
    Unary(AstUnaryOp),
    Binary(BinaryExpr),
    Assignment(*mut AstExpr),
    IsDeallocand,
    IsDereferencable,
    ArbArg,
    Allocation(AllocExpr),
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct result {
    pub val: *mut Value,
    pub err: *mut Error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstType {
    pub mod_0: libc::c_int,
    pub base: AstTypeBase,
    pub c2rust_unnamed: AstTypeUnion,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union AstTypeUnion {
    pub ptr_type: *mut AstType,
    pub arr: AstArrayType,
    pub structunion: AstStructType,
    pub userdef: *mut libc::c_char,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstStructType {
    pub tag: *mut libc::c_char,
    pub members: *mut AstVariableArr,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstVariableArr {
    pub n: libc::c_int,
    pub v: *mut *mut AstVariable,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstVariable {
    pub name: *mut libc::c_char,
    pub type_0: *mut AstType,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstArrayType {
    pub type_0: *mut AstType,
    pub length: libc::c_int,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstTypeBase {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Pointer,
    Array,
    Struct,
    Union,
    Enum,
    UserDefined,
}

pub type AstTypeModifier = libc::c_uint;
pub const MOD_TYPEDEF: AstTypeModifier = 1;
pub const MOD_EXTERN: AstTypeModifier = 2;
pub const MOD_STATIC: AstTypeModifier = 4;
pub const MOD_AUTO: AstTypeModifier = 8;
pub const MOD_REGISTER: AstTypeModifier = 16;
pub const MOD_CONST: AstTypeModifier = 32;
pub const MOD_VOLATILE: AstTypeModifier = 64;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct LValue {
    pub t: *mut AstType,
    pub obj: *mut Object,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct LValueRes {
    pub lval: *mut LValue,
    pub err: *mut Error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstFunction {
    pub isaxiom: bool,
    pub ret: *mut AstType,
    pub name: *mut libc::c_char,
    pub nparam: libc::c_int,
    pub param: *mut *mut AstVariable,
    pub abstract_0: *mut AstBlock,
    pub body: *mut AstBlock,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstBlock {
    pub ndecl: libc::c_int,
    pub nstmt: libc::c_int,
    pub decl: *mut *mut AstVariable,
    pub stmt: *mut *mut AstStmt,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstStmt {
    pub kind: AstStmtKind,
    pub u: C2RustUnnamed_8,
    pub loc: *mut LexemeMarker,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_8 {
    pub labelled: AstLabelledStmt,
    pub compound: *mut AstBlock,
    pub selection: AstSelectionStmt,
    pub iteration: AstIterationStmt,
    pub expr: *mut AstExpr,
    pub jump: AstJumpStmt,
    pub alloc: AstAllocStmt,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstAllocStmt {
    pub kind: AstAllocKind,
    pub arg: *mut AstExpr,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstJumpStmt {
    pub kind: AstJumpKind,
    pub rv: *mut AstExpr,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstJumpKind {
    Return,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstIterationStmt {
    pub init: *mut AstStmt,
    pub cond: *mut AstStmt,
    pub body: *mut AstStmt,
    pub iter: *mut AstExpr,
    pub abstract_0: *mut AstBlock,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstSelectionStmt {
    pub isswitch: bool,
    pub cond: *mut AstExpr,
    pub body: *mut AstStmt,
    pub nest: *mut AstStmt,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstLabelledStmt {
    pub label: *mut libc::c_char,
    pub stmt: *mut AstStmt,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstStmtKind {
    Nop,
    Labelled,
    Compound,
    CompoundV,
    Expr,
    Selection,
    Iteration,
    IterationE,
    Jump,
    Allocation,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Decision {
    pub decision: bool,
    pub err: *mut Error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct PrecondsResult {
    pub stmt: *mut AstStmt,
    pub err: *mut Error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Preresult {
    pub iscontradiction: bool,
    pub err: *mut Error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstStmtSplits {
    pub n: libc::c_int,
    pub cond: *mut *mut AstExpr,
    pub err: *mut Error,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstFunctionArr {
    pub n: libc::c_int,
    pub f: *mut *mut AstFunction,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstExternDecl {
    pub kind: AstExternDeclKind,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct AstTypedefDecl {
    pub name: *mut libc::c_char,
    pub type_0: *mut AstType,
}

#[derive(Copy, Clone)]
pub enum AstExternDeclKind {
    Function(*mut AstFunction),
    Variable(*mut AstVariable),
    Typedef(AstTypedefDecl),
    Struct(*mut AstType),
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Ast {
    pub n: libc::c_int,
    pub decl: *mut *mut AstExternDecl,
}

unsafe fn expr_literal_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut res: *mut result = result_value_create(state_static_init(state, expr));
    return res;
}
unsafe fn expr_constant_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    return result_value_create(value_int_create(ast_expr_as_constant(expr)));
}
unsafe fn rangeprocess_dealloc(
    mut dealloc: *mut AstExpr,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> *mut Error {
    let mut obj: *mut Object = hack_base_object_from_alloc(ast_expr_alloc_arg(dealloc), state);
    return state_range_dealloc(state, obj, lw, up);
}
unsafe fn hack_base_object_from_alloc(
    mut expr: *mut AstExpr,
    mut state: *mut State,
) -> *mut Object {
    let mut inner: *mut AstExpr = ast_expr_unary_operand(expr);
    let mut i: *mut AstExpr =
        ast_expr_identifier_create(dynamic_str(b"i\0" as *const u8 as *const libc::c_char));
    if !ast_expr_equal(ast_expr_binary_e2(inner), i) {
        panic!();
    }
    ast_expr_destroy(i);
    let mut res: LValueRes = ast_expr_lvalue(ast_expr_binary_e1(inner), state);
    if !(res.err).is_null() {
        panic!();
    }
    let mut obj: *mut Object = lvalue_object(res.lval);
    if obj.is_null() {
        panic!();
    }
    return obj;
}

pub unsafe fn ast_expr_equal(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> bool {
    if e1.is_null() || e2.is_null() {
        return false;
    }
    match ((*e1).kind, (*e2).kind) {
        (AstExprKind::Constant(c1), AstExprKind::Constant(c2)) => c1.constant == c2.constant,
        (AstExprKind::Identifier(id1), AstExprKind::Identifier(id2)) => strcmp(id1, id2) == 0,
        (AstExprKind::StringLiteral(s1), AstExprKind::StringLiteral(s2)) => strcmp(s1, s2) == 0,
        (AstExprKind::Assignment(a1), AstExprKind::Assignment(a2)) => {
            ast_expr_equal((*e1).root, (*e2).root) && ast_expr_equal(a1, a1)
        }
        (AstExprKind::Unary(op1), AstExprKind::Unary(op2)) => {
            op1 == op2 && ast_expr_equal((*e1).root, (*e2).root)
        }
        (AstExprKind::Binary(b1), AstExprKind::Binary(b2)) => {
            b1.op == b2.op && ast_expr_equal(b1.e1, b2.e1) && ast_expr_equal(b1.e2, b2.e2)
        }
        (AstExprKind::Call(c1), AstExprKind::Call(c2)) => {
            if c1.n != c2.n {
                return false;
            }
            let mut i: libc::c_int = 0 as libc::c_int;
            while i < c1.n {
                if !ast_expr_equal(*(c1.arg).offset(i as isize), *(c2.arg).offset(i as isize)) {
                    return false;
                }
                i += 1;
            }
            ast_expr_equal((*e1).root, (*e2).root)
        }
        (AstExprKind::StructMember(field1), AstExprKind::StructMember(field2)) => {
            ast_expr_equal(ast_expr_member_root(e1), ast_expr_member_root(e2))
                && strcmp(field1, field2) == 0
        }
        _ => false,
    }
}

unsafe fn rangeprocess_alloc(
    mut expr: *mut AstExpr,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> *mut Error {
    let mut lval: *mut AstExpr = ast_expr_assignment_lval(expr);
    let mut rval: *mut AstExpr = ast_expr_assignment_rval(expr);
    let AstExprKind::Allocation(alloc) = (*rval).kind else {
        panic!()
    };
    assert_ne!(alloc.kind, AstAllocKind::Dealloc);
    let mut obj: *mut Object = hack_base_object_from_alloc(lval, state);
    return state_range_alloc(state, obj, lw, up);
}

pub unsafe fn ast_expr_matheval(mut e: *mut AstExpr) -> bool {
    match (*e).kind {
        AstExprKind::Binary(b) => {
            let mut e1: *mut MathExpr = math_expr(b.e1);
            let mut e2: *mut MathExpr = math_expr(b.e2);
            let mut val: bool = eval_prop(e1, b.op, e2);
            math_expr_destroy(e2);
            math_expr_destroy(e1);
            return val;
        }
        _ => panic!(),
    }
}

unsafe fn math_expr(mut e: *mut AstExpr) -> *mut MathExpr {
    match (*e).kind {
        AstExprKind::Identifier(id) => {
            math_expr_atom_create(math_atom_variable_create(dynamic_str(id)))
        }
        AstExprKind::Constant(ConstantExpr { constant: c, .. }) => {
            if c < 0 {
                math_expr_neg_create(math_expr_atom_create(math_atom_nat_create(
                    -c as libc::c_uint,
                )))
            } else {
                math_expr_atom_create(math_atom_nat_create(c as libc::c_uint))
            }
        }
        AstExprKind::Binary(b) => math_expr_sum_create(math_expr(b.e1), binary_e2(b.e2, b.op)),
        _ => {
            panic!();
        }
    }
}

unsafe fn binary_e2(mut e2: *mut AstExpr, mut op: AstBinaryOp) -> *mut MathExpr {
    match op {
        AstBinaryOp::Addition => math_expr(e2),
        AstBinaryOp::Subtraction => math_expr_neg_create(math_expr(e2)),
        _ => panic!(),
    }
}

unsafe fn eval_prop(mut e1: *mut MathExpr, mut op: AstBinaryOp, mut e2: *mut MathExpr) -> bool {
    match op {
        AstBinaryOp::Eq => math_eq(e1, e2),
        AstBinaryOp::Ne => !math_eq(e1, e2),
        AstBinaryOp::Lt => math_lt(e1, e2),
        AstBinaryOp::Gt => math_gt(e1, e2),
        AstBinaryOp::Le => math_le(e1, e2),
        AstBinaryOp::Ge => math_ge(e1, e2),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_decide(mut expr: *mut AstExpr, mut state: *mut State) -> bool {
    match ast_expr_kind(expr) {
        AstExprKind::Constant(_) => ast_expr_as_constant(expr) != 0,
        AstExprKind::Unary(_) => expr_unary_decide(expr, state),
        AstExprKind::IsDeallocand => expr_isdeallocand_decide(expr, state),
        AstExprKind::Binary(_) => expr_binary_decide(expr, state),
        _ => panic!(),
    }
}

unsafe fn expr_binary_decide(mut expr: *mut AstExpr, mut state: *mut State) -> bool {
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

unsafe fn value_compare(mut v1: *mut Value, mut op: AstBinaryOp, mut v2: *mut Value) -> bool {
    match op {
        AstBinaryOp::Eq => value_equal(v1, v2),
        AstBinaryOp::Ne => !value_compare(v1, AstBinaryOp::Eq, v2),
        _ => panic!(),
    }
}

unsafe fn expr_isdeallocand_decide(mut expr: *mut AstExpr, mut state: *mut State) -> bool {
    let mut obj: *mut Object = hack_object_from_assertion(expr, state);
    let mut isdeallocand: bool = state_addresses_deallocand(state, obj);
    return isdeallocand;
}

unsafe fn hack_object_from_assertion(mut expr: *mut AstExpr, mut state: *mut State) -> *mut Object {
    let mut assertand: *mut AstExpr = ast_expr_isdeallocand_assertand(expr);
    let mut res: LValueRes = ast_expr_lvalue(assertand, state);
    if !(res.err).is_null() {
        panic!();
    }
    let mut obj: *mut Object = lvalue_object(res.lval);
    if obj.is_null() {
        panic!();
    }
    return obj;
}

unsafe fn expr_unary_decide(mut expr: *mut AstExpr, mut state: *mut State) -> bool {
    let mut operand: *mut AstExpr = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_decide(operand, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_rangedecide(
    mut expr: *mut AstExpr,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> bool {
    match ast_expr_kind(expr) {
        AstExprKind::Unary(_) => return unary_rangedecide(expr, lw, up, state),
        AstExprKind::IsDeallocand => return expr_isdeallocand_rangedecide(expr, lw, up, state),
        _ => panic!(),
    }
}

unsafe fn expr_isdeallocand_rangedecide(
    mut expr: *mut AstExpr,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> bool {
    let mut acc: *mut AstExpr = ast_expr_isdeallocand_assertand(expr);
    assert_eq!(ast_expr_unary_op(acc), AstUnaryOp::Dereference);
    let mut inner: *mut AstExpr = ast_expr_unary_operand(acc);
    let mut i: *mut AstExpr =
        ast_expr_identifier_create(dynamic_str(b"i\0" as *const u8 as *const libc::c_char));
    let mut j: *mut AstExpr =
        ast_expr_identifier_create(dynamic_str(b"j\0" as *const u8 as *const libc::c_char));
    if !(ast_expr_equal(ast_expr_binary_e2(inner), i) as libc::c_int != 0
        || ast_expr_equal(ast_expr_binary_e2(inner), j) as libc::c_int != 0)
    {
        panic!();
    }
    ast_expr_destroy(j);
    ast_expr_destroy(i);
    let mut res: LValueRes = ast_expr_lvalue(ast_expr_binary_e1(acc), state);
    if !(res.err).is_null() {
        panic!();
    }
    let mut obj: *mut Object = lvalue_object(res.lval);
    if obj.is_null() {
        panic!();
    }
    return state_range_aredeallocands(state, obj, lw, up);
}

unsafe fn unary_rangedecide(
    mut expr: *mut AstExpr,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> bool {
    let mut operand: *mut AstExpr = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => return !ast_expr_rangedecide(operand, lw, up, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_exec(mut expr: *mut AstExpr, mut state: *mut State) -> *mut Error {
    let mut res: *mut result = ast_expr_eval(expr, state);
    if result_iserror(res) {
        return result_as_error(res);
    }
    return 0 as *mut Error;
}

pub unsafe fn ast_expr_arbarg_create() -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::ArbArg;
    return expr;
}

pub unsafe fn ast_expr_assume(mut expr: *mut AstExpr, mut state: *mut State) -> *mut Preresult {
    return reduce_assume(expr, true, state);
}

unsafe fn reduce_assume(
    mut expr: *mut AstExpr,
    mut value: bool,
    mut s: *mut State,
) -> *mut Preresult {
    match (*expr).kind {
        AstExprKind::Identifier(_) => return identifier_assume(expr, value, s),
        AstExprKind::Unary(op) => {
            assert_eq!(op, AstUnaryOp::Bang);
            return reduce_assume(ast_expr_unary_operand(expr), !value, s);
        }
        AstExprKind::Bracketed => return reduce_assume((*expr).root, value, s),
        AstExprKind::Call(_) | AstExprKind::StructMember(_) => {
            return ast_expr_pf_reduce_assume(expr, value, s)
        }
        AstExprKind::Binary(b) => return binary_assume(&b, value, s),
        _ => {
            panic!();
        }
    }
}

unsafe fn binary_assume(b: &BinaryExpr, mut value: bool, mut s: *mut State) -> *mut Preresult {
    let mut r1: *mut result = ast_expr_pf_reduce(b.e1, s);
    let mut r2: *mut result = ast_expr_pf_reduce(b.e2, s);
    let mut v1: *mut Value = result_as_value(r1);
    let mut v2: *mut Value = result_as_value(r2);
    return irreducible_assume(
        ast_expr_binary_create(value_to_expr(v1), b.op, value_to_expr(v2)),
        value,
        s,
    );
}

unsafe fn irreducible_assume(
    mut e: *mut AstExpr,
    mut value: bool,
    mut s: *mut State,
) -> *mut Preresult {
    let mut prop: *mut AstExpr = ast_expr_inverted_copy(e, !value);
    let mut r: *mut Preresult = irreducible_assume_actual(prop, s);
    ast_expr_destroy(prop);
    return r;
}

unsafe fn irreducible_assume_actual(mut e: *mut AstExpr, mut s: *mut State) -> *mut Preresult {
    let mut p: *mut Props = state_getprops(s);
    if props_contradicts(p, e) {
        return preresult_contradiction_create();
    }
    props_install(state_getprops(s), ast_expr_copy(e));
    return preresult_empty_create();
}

pub unsafe fn ast_expr_isdereferencable_create(mut assertand: *mut AstExpr) -> *mut AstExpr {
    let mut new: *mut AstExpr = ast_expr_create();
    (*new).kind = AstExprKind::IsDereferencable;
    (*new).root = assertand;
    return new;
}

unsafe fn ast_expr_pf_reduce_assume(
    mut expr: *mut AstExpr,
    mut value: bool,
    mut s: *mut State,
) -> *mut Preresult {
    let mut res: *mut result = ast_expr_pf_reduce(expr, s);
    if !(!result_iserror(res) && result_hasvalue(res) as libc::c_int != 0) {
        panic!();
    }
    return irreducible_assume(value_as_sync(result_as_value(res)), value, s);
}
unsafe fn identifier_assume(
    mut expr: *mut AstExpr,
    mut value: bool,
    mut s: *mut State,
) -> *mut Preresult {
    let mut s_copy: *mut State = state_copy(s);
    let mut res: *mut result = ast_expr_eval(expr, s_copy);
    if !(!result_iserror(res) && result_hasvalue(res) as libc::c_int != 0) {
        panic!();
    }
    state_destroy(s_copy);
    return irreducible_assume(value_as_sync(result_as_value(res)), value, s);
}
unsafe fn binary_deref_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut res: *mut result = ast_expr_eval(ast_expr_binary_e1(expr), state);
    if result_iserror(res) {
        return res;
    }
    let mut arr: *mut Value = result_as_value(res);
    if arr.is_null() {
        panic!();
    }
    let mut deref_res: ObjectRes = state_deref(state, arr, ast_expr_binary_e2(expr));
    if !(deref_res.err).is_null() {
        return result_error_create(deref_res.err);
    }
    if (deref_res.obj).is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
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
    let mut v: *mut Value = object_as_value(deref_res.obj);
    if v.is_null() {
        let mut b_0: *mut StrBuilder = strbuilder_create();
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
    mut state: *mut State,
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

pub unsafe fn ast_expr_isdeallocand_create(mut assertand: *mut AstExpr) -> *mut AstExpr {
    let mut new: *mut AstExpr = ast_expr_create();
    (*new).kind = AstExprKind::IsDeallocand;
    (*new).root = assertand;
    return new;
}

pub unsafe fn ast_expr_assignment_create(
    mut root: *mut AstExpr,
    mut value: *mut AstExpr,
) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Assignment(value);
    (*expr).root = root;
    return expr;
}

unsafe fn ast_expr_bracketed_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"(%s)\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

unsafe fn expr_to_binary(mut expr: *mut AstExpr) -> *mut AstExpr {
    match ast_expr_kind(expr) {
        AstExprKind::Binary(_) => return ast_expr_copy(expr),
        _ => {
            return ast_expr_binary_create(
                ast_expr_copy(expr),
                AstBinaryOp::Addition,
                ast_expr_constant_create(0 as libc::c_int),
            );
        }
    };
}

unsafe fn expr_identifier_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut res: *mut result = hack_identifier_builtin_eval(ast_expr_as_identifier(expr), state);
    if !result_iserror(res) && result_hasvalue(res) as libc::c_int != 0 {
        return res;
    }
    let mut id: *mut libc::c_char = ast_expr_as_identifier(expr);
    if *id.offset(0 as libc::c_int as isize) as libc::c_int == '#' as i32 {
        return result_value_create(value_literal_create(id));
    }
    let mut obj: *mut Object = state_getobject(state, id);
    if obj.is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"unknown idenitfier %s\0" as *const u8 as *const libc::c_char,
            id,
        );
        return result_error_create(error_create(strbuilder_build(b)));
    }
    let mut val: *mut Value = object_as_value(obj);
    if val.is_null() {
        vprintln!(
            "state: {}",
            CStr::from_ptr(state_str(state)).to_string_lossy()
        );
        let mut b_0: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b_0,
            b"undefined memory access: %s has no value\0" as *const u8 as *const libc::c_char,
            id,
        );
        return result_error_create(error_create(strbuilder_build(b_0)));
    }
    return result_value_create(value_copy(val));
}
unsafe fn expr_structmember_eval(mut expr: *mut AstExpr, mut s: *mut State) -> *mut result {
    let mut root: *mut AstExpr = ast_expr_member_root(expr);
    let mut res: *mut result = ast_expr_eval(root, s);
    if result_iserror(res) {
        return res;
    }
    let mut field: *mut libc::c_char = ast_expr_member_field(expr);
    let mut member: *mut Object = value_struct_member(result_as_value(res), field);
    if member.is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
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
    let mut obj_value: *mut Value = object_as_value(member);
    let mut v: *mut Value = if !obj_value.is_null() {
        value_copy(obj_value)
    } else {
        0 as *mut Value
    };
    result_destroy(res);
    return result_value_create(v);
}

unsafe fn address_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut operand: *mut AstExpr = ast_expr_unary_operand(expr);
    let mut id: *mut libc::c_char = ast_expr_as_identifier(operand);
    let mut v: *mut Value = state_getloc(state, id);
    return result_value_create(v);
}

pub unsafe fn ast_expr_alloc_rangeprocess(
    mut alloc: *mut AstExpr,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut result_lw: *mut result = ast_expr_eval(lw, state);
    let mut result_up: *mut result = ast_expr_eval(up, state);
    if result_iserror(result_lw) {
        return result_as_error(result_lw);
    }
    if result_iserror(result_up) {
        return result_as_error(result_up);
    }
    let mut res_lw: *mut AstExpr = value_to_expr(result_as_value(result_lw));
    let mut res_up: *mut AstExpr = value_to_expr(result_as_value(result_up));
    result_destroy(result_up);
    result_destroy(result_lw);
    match (*alloc).kind {
        AstExprKind::Assignment(_) => {
            err = rangeprocess_alloc(alloc, res_lw, res_up, state);
        }
        AstExprKind::Allocation(_) => {
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
    return 0 as *mut Error;
}

pub unsafe fn ast_expr_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    match ast_expr_kind(expr) {
        AstExprKind::Constant(_) => expr_constant_eval(expr, state),
        AstExprKind::StringLiteral(_) => expr_literal_eval(expr, state),
        AstExprKind::Identifier(_) => expr_identifier_eval(expr, state),
        AstExprKind::Unary(_) => expr_unary_eval(expr, state),
        AstExprKind::StructMember(_) => expr_structmember_eval(expr, state),
        AstExprKind::Call(_) => expr_call_eval(expr, state),
        AstExprKind::Assignment(_) => expr_assign_eval(expr, state),
        AstExprKind::IncDec(_) => expr_incdec_eval(expr, state),
        AstExprKind::Binary(_) => expr_binary_eval(expr, state),
        AstExprKind::ArbArg => arbarg_eval(expr, state),
        _ => panic!(),
    }
}

unsafe fn arbarg_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    return result_value_create(state_vconst(
        state,
        ast_type_create_ptr(ast_type_create(AstTypeBase::Void, 0)),
        0 as *mut libc::c_char,
        false,
    ));
}
unsafe fn expr_binary_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut e1: *mut AstExpr = ast_expr_binary_e1(expr);
    let mut e2: *mut AstExpr = ast_expr_binary_e2(expr);
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
unsafe fn expr_incdec_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut assign: *mut AstExpr = ast_expr_incdec_to_assignment(expr);
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

unsafe fn ast_expr_destroy_literal(mut expr: *mut AstExpr) {
    let AstExprKind::StringLiteral(s) = (*expr).kind else {
        panic!()
    };
    free(s as *mut libc::c_void);
}

unsafe fn ast_expr_destroy_identifier(mut expr: *mut AstExpr) {
    let AstExprKind::Identifier(id) = (*expr).kind else {
        panic!()
    };
    free(id as *mut libc::c_void);
}

unsafe fn expr_assign_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut lval: *mut AstExpr = ast_expr_assignment_lval(expr);
    let mut rval: *mut AstExpr = ast_expr_assignment_rval(expr);
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
    let mut lval_res: LValueRes = ast_expr_lvalue(lval, state);
    if !(lval_res.err).is_null() {
        return result_error_create(lval_res.err);
    }
    let mut obj: *mut Object = lvalue_object(lval_res.lval);
    if obj.is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
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

pub unsafe fn ast_expr_lvalue(mut expr: *mut AstExpr, mut state: *mut State) -> LValueRes {
    match ast_expr_kind(expr) {
        AstExprKind::Identifier(_) => expr_identifier_lvalue(expr, state),
        AstExprKind::Unary(_) => expr_unary_lvalue(expr, state),
        AstExprKind::StructMember(_) => expr_structmember_lvalue(expr, state),
        _ => panic!(),
    }
}

pub unsafe fn expr_structmember_lvalue(mut expr: *mut AstExpr, mut state: *mut State) -> LValueRes {
    let mut root: *mut AstExpr = ast_expr_member_root(expr);
    let mut root_res: LValueRes = ast_expr_lvalue(root, state);
    let mut root_obj: *mut Object = lvalue_object(root_res.lval);
    if root_obj.is_null() {
        panic!();
    }
    let mut field: *mut libc::c_char = ast_expr_member_field(expr);
    let mut member: *mut Object =
        object_getmember(root_obj, lvalue_type(root_res.lval), field, state);
    if member.is_null() {
        return {
            let mut init = LValueRes {
                lval: 0 as *mut LValue,
                err: error_create(
                    b"lvalue error\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
                ),
            };
            init
        };
    }
    let mut t: *mut AstType =
        object_getmembertype(root_obj, lvalue_type(root_res.lval), field, state);
    if t.is_null() {
        panic!();
    }
    return {
        let mut init = LValueRes {
            lval: lvalue_create(t, member),
            err: 0 as *mut Error,
        };
        init
    };
}

pub unsafe fn expr_unary_lvalue(mut expr: *mut AstExpr, mut state: *mut State) -> LValueRes {
    if !(ast_expr_unary_op(expr) == AstUnaryOp::Dereference) {
        panic!();
    }
    let mut inner: *mut AstExpr = ast_expr_unary_operand(expr);
    if matches!(ast_expr_kind(inner), AstExprKind::Identifier(_)) {
        let mut root_res: LValueRes = ast_expr_lvalue(inner, state);
        if !(root_res.err).is_null() {
            return root_res;
        }
        let mut root_obj: *mut Object = lvalue_object(root_res.lval);
        if root_obj.is_null() {
            return {
                let mut init = LValueRes {
                    lval: 0 as *mut LValue,
                    err: 0 as *mut Error,
                };
                init
            };
        }
        let mut t: *mut AstType = ast_type_ptr_type(lvalue_type(root_res.lval));
        let mut root_val: *mut Value = object_as_value(root_obj);
        if root_val.is_null() {
            panic!();
        }
        let mut res: ObjectRes =
            state_deref(state, root_val, ast_expr_constant_create(0 as libc::c_int));
        if !(res.err).is_null() {
            return {
                let mut init = LValueRes {
                    lval: 0 as *mut LValue,
                    err: res.err,
                };
                init
            };
        }
        return {
            let mut init = LValueRes {
                lval: lvalue_create(t, res.obj),
                err: 0 as *mut Error,
            };
            init
        };
    }
    let mut root_res_0: LValueRes = ast_expr_lvalue(ast_expr_binary_e1(inner), state);
    if !(root_res_0.err).is_null() {
        return root_res_0;
    }
    let mut root_obj_0: *mut Object = lvalue_object(root_res_0.lval);
    if root_obj_0.is_null() {
        return {
            let mut init = LValueRes {
                lval: 0 as *mut LValue,
                err: 0 as *mut Error,
            };
            init
        };
    }
    let mut t_0: *mut AstType = ast_type_ptr_type(lvalue_type(root_res_0.lval));
    let mut root_val_0: *mut Value = object_as_value(root_obj_0);
    if root_val_0.is_null() {
        panic!();
    }
    let mut res_0: ObjectRes = state_deref(state, root_val_0, ast_expr_binary_e2(inner));
    if !(res_0.err).is_null() {
        return {
            let mut init = LValueRes {
                lval: 0 as *mut LValue,
                err: 0 as *mut Error,
            };
            init
        };
    }
    return {
        let mut init = LValueRes {
            lval: lvalue_create(t_0, res_0.obj),
            err: 0 as *mut Error,
        };
        init
    };
}

pub unsafe fn expr_identifier_lvalue(mut expr: *mut AstExpr, mut state: *mut State) -> LValueRes {
    let mut id: *mut libc::c_char = ast_expr_as_identifier(expr);
    return {
        let mut init = LValueRes {
            lval: lvalue_create(state_getobjecttype(state, id), state_getobject(state, id)),
            err: 0 as *mut Error,
        };
        init
    };
}
unsafe fn dereference_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut binary: *mut AstExpr = expr_to_binary(ast_expr_unary_operand(expr));
    let mut res: *mut result = binary_deref_eval(binary, state);
    ast_expr_destroy(binary);
    return res;
}
unsafe fn ast_expr_constant_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let c = match (*expr).kind {
        AstExprKind::Constant(c) => c,
        _ => panic!(),
    };
    let mut constant: libc::c_int = c.constant;
    if !c.ischar {
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

unsafe fn expr_call_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut err: *mut Error = 0 as *mut Error;
    let mut root: *mut AstExpr = ast_expr_call_root(expr);
    let mut name: *mut libc::c_char = ast_expr_as_identifier(root);
    let mut f: *mut AstFunction = externals_getfunc(state_getext(state), name);
    if f.is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        return result_error_create(error_create(strbuilder_build(b)));
    }
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut params: *mut *mut AstVariable = ast_function_params(f);
    let mut rtype: *mut AstType = ast_function_type(f);
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
    let mut v: *mut Value = 0 as *mut Value;
    if result_hasvalue(res) {
        v = value_copy(result_as_value(res));
    }
    state_popframe(state);
    if !v.is_null() {
        return pf_augment(v, expr, state);
    }
    return res;
}
unsafe fn call_absexec(mut expr: *mut AstExpr, mut s: *mut State) -> *mut result {
    let mut root: *mut AstExpr = ast_expr_call_root(expr);
    let mut name: *mut libc::c_char = ast_expr_as_identifier(root);
    let mut f: *mut AstFunction = externals_getfunc(state_getext(s), name);
    if f.is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
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
    mut expr: *mut AstExpr,
    mut f: *mut AstFunction,
    mut state: *mut State,
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
unsafe fn call_to_computed_value(mut f: *mut AstFunction, mut s: *mut State) -> *mut result {
    let mut root: *mut libc::c_char = ast_function_name(f);
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut uncomputed_param: *mut *mut AstVariable = ast_function_params(f);
    let mut computed_param: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(nparams as usize))
            as *mut *mut AstExpr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nparams {
        let mut param: *mut AstExpr = ast_expr_identifier_create(dynamic_str(ast_variable_name(
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
        let mut v: *mut Value = result_as_value(res);
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

pub unsafe fn ast_expr_absexec(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    match ast_expr_kind(expr) {
        AstExprKind::Assignment(_) => assign_absexec(expr, state),
        AstExprKind::IsDereferencable => isdereferencable_absexec(expr, state),
        AstExprKind::Allocation(_) => alloc_absexec(expr, state),
        AstExprKind::Identifier(_)
        | AstExprKind::Constant(_)
        | AstExprKind::Unary(_)
        | AstExprKind::Call(_)
        | AstExprKind::StructMember(_)
        | AstExprKind::ArbArg => ast_expr_eval(expr, state),
        _ => panic!(),
    }
}

unsafe fn expr_unary_eval(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Dereference => dereference_eval(expr, state),
        AstUnaryOp::Address => address_eval(expr, state),
        AstUnaryOp::Bang => result_value_create(value_literal_create(
            b"hack\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        )),

        _ => panic!(),
    }
}

unsafe fn alloc_absexec(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    match ast_expr_alloc_kind(expr) {
        AstAllocKind::Alloc => return result_value_create(state_alloc(state)),
        AstAllocKind::Dealloc => return dealloc_process(expr, state),
        AstAllocKind::Clump => return result_value_create(state_clump(state)),
    }
}

unsafe fn dealloc_process(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut arg: *mut AstExpr = ast_expr_alloc_arg(expr);
    let mut res: *mut result = ast_expr_eval(arg, state);
    if result_iserror(res) {
        return res;
    }
    let mut val: *mut Value = result_as_value(res);
    if val.is_null() {
        panic!();
    }
    let mut err: *mut Error = state_dealloc(state, val);
    if !err.is_null() {
        return result_error_create(err);
    }
    value_destroy(val);
    return result_value_create(0 as *mut Value);
}

pub unsafe fn prepare_parameters(
    mut nparams: libc::c_int,
    mut param: *mut *mut AstVariable,
    mut args: &[*mut result],
    mut fname: *mut libc::c_char,
    mut state: *mut State,
) -> *mut Error {
    assert_eq!(nparams as usize, args.len());
    let mut i: libc::c_int = 0 as libc::c_int;
    while (i as usize) < args.len() {
        state_declare(state, *param.offset(i as isize), true);
        let mut res: *mut result = args[i as usize];
        if result_iserror(res) {
            return result_as_error(res);
        }
        if !result_hasvalue(res) {
            let mut b: *mut StrBuilder = strbuilder_create();
            strbuilder_printf(
                b,
                b"parameter `%s' of function `%s' has no value\0" as *const u8
                    as *const libc::c_char,
                ast_variable_name(*param.offset(i as isize)),
                fname,
            );
            return error_create(strbuilder_build(b));
        }
        let mut name: *mut AstExpr =
            ast_expr_identifier_create(dynamic_str(ast_variable_name(*param.offset(i as isize))));
        let mut lval_res: LValueRes = ast_expr_lvalue(name, state);
        if !(lval_res.err).is_null() {
            return lval_res.err;
        }
        let mut obj: *mut Object = lvalue_object(lval_res.lval);
        ast_expr_destroy(name);
        object_assign(obj, value_copy(result_as_value(res)));
        i += 1;
    }
    return 0 as *mut Error;
}
unsafe fn isdereferencable_absexec(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut p: *mut Props = state_getprops(state);
    props_install(p, expr);
    return result_value_create(0 as *mut Value);
}

pub unsafe fn prepare_arguments(
    mut nargs: libc::c_int,
    mut arg: *mut *mut AstExpr,
    mut nparams: libc::c_int,
    mut param: *mut *mut AstVariable,
    mut state: *mut State,
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

unsafe fn assign_absexec(mut expr: *mut AstExpr, mut state: *mut State) -> *mut result {
    let mut lval: *mut AstExpr = ast_expr_assignment_lval(expr);
    let mut rval: *mut AstExpr = ast_expr_assignment_rval(expr);
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
    let mut lval_res: LValueRes = ast_expr_lvalue(lval, state);
    if !(lval_res.err).is_null() {
        return result_error_create(lval_res.err);
    }
    let mut obj: *mut Object = lvalue_object(lval_res.lval);
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
    mut param: *mut Value,
    mut arg: *mut Value,
    mut param_state: *mut State,
    mut arg_state: *mut State,
) -> *mut Error {
    if !state_islval(param_state, param) {
        return 0 as *mut Error;
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
    let mut param_res: ObjectRes = state_get(param_state, value_as_location(param), false);
    if !(param_res.err).is_null() {
        return param_res.err;
    }
    let mut arg_res: ObjectRes = state_get(arg_state, value_as_location(arg), false);
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
        return 0 as *mut Error;
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
unsafe fn call_setupverify(mut f: *mut AstFunction, mut arg_state: *mut State) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut fname: *mut libc::c_char = ast_function_name(f);
    let mut param_state: *mut State = state_create(
        dynamic_str(fname),
        state_getext(arg_state),
        ast_function_type(f),
    );
    err = ast_function_initparams(f, param_state);
    if !err.is_null() {
        return err;
    }
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut param: *mut *mut AstVariable = ast_function_params(f);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nparams {
        let mut id: *mut libc::c_char = ast_variable_name(*param.offset(i as isize));
        let mut param_0: *mut Value = state_getloc(param_state, id);
        let mut arg: *mut Value = state_getloc(arg_state, id);
        err = verify_paramspec(param_0, arg, param_state, arg_state);
        if !err.is_null() {
            let mut b: *mut StrBuilder = strbuilder_create();
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
    return 0 as *mut Error;
}

pub unsafe fn ast_expr_bracketed_root(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::Bracketed));
    return (*expr).root;
}

pub unsafe fn ast_expr_bracketed_create(mut root: *mut AstExpr) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Bracketed;
    (*expr).root = root;
    return expr;
}

pub unsafe fn ast_expr_literal_create(mut s: *mut libc::c_char) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::StringLiteral(s);
    return expr;
}

pub unsafe fn ast_expr_as_literal(mut expr: *mut AstExpr) -> *mut libc::c_char {
    let AstExprKind::StringLiteral(s) = (*expr).kind else {
        panic!()
    };
    s
}

pub unsafe fn ast_expr_as_constant(mut expr: *mut AstExpr) -> libc::c_int {
    match (*expr).kind {
        AstExprKind::Constant(c) => c.constant,
        _ => panic!(),
    }
}

unsafe fn pf_augment(
    mut v: *mut Value,
    mut call: *mut AstExpr,
    mut state: *mut State,
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

pub unsafe fn ast_expr_constant_create_char(mut c: libc::c_char) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Constant(ConstantExpr {
        ischar: true,
        constant: c as libc::c_int,
    });
    return expr;
}

pub unsafe fn ast_expr_constant_create(mut k: libc::c_int) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Constant(ConstantExpr {
        ischar: false,
        constant: k,
    });
    return expr;
}

pub unsafe fn ast_expr_as_identifier(mut expr: *mut AstExpr) -> *mut libc::c_char {
    let AstExprKind::Identifier(id) = (*expr).kind else {
        panic!()
    };
    id
}

unsafe fn ast_expr_create() -> *mut AstExpr {
    return malloc(::core::mem::size_of::<AstExpr>()) as *mut AstExpr;
}

pub unsafe fn ast_expr_identifier_create(mut s: *mut libc::c_char) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Identifier(s);
    return expr;
}
unsafe fn unary_pf_reduce(mut e: *mut AstExpr, mut s: *mut State) -> *mut result {
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
    mut e1: *mut AstExpr,
    mut op: AstBinaryOp,
    mut e2: *mut AstExpr,
    mut s: *mut State,
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
unsafe fn call_pf_reduce(mut e: *mut AstExpr, mut s: *mut State) -> *mut result {
    let mut root: *mut libc::c_char = ast_expr_as_identifier(ast_expr_call_root(e));
    let mut nargs: libc::c_int = ast_expr_call_nargs(e);
    let mut unreduced_arg: *mut *mut AstExpr = ast_expr_call_args(e);
    let mut reduced_arg: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(nargs as usize))
            as *mut *mut AstExpr;
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
unsafe fn structmember_pf_reduce(mut expr: *mut AstExpr, mut s: *mut State) -> *mut result {
    let mut res: *mut result = ast_expr_pf_reduce(ast_expr_member_root(expr), s);
    if result_iserror(res) {
        return res;
    }
    if !result_hasvalue(res) {
        panic!();
    }
    let mut field: *mut libc::c_char = ast_expr_member_field(expr);
    let mut v: *mut Value = result_as_value(res);
    if value_isstruct(v) {
        let mut obj: *mut Object = value_struct_member(v, field);
        let mut obj_value: *mut Value = object_as_value(obj);
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

pub unsafe fn ast_expr_pf_reduce(mut e: *mut AstExpr, mut s: *mut State) -> *mut result {
    match ast_expr_kind(e) {
        AstExprKind::Constant(_) | AstExprKind::StringLiteral(_) | AstExprKind::Identifier(_) => {
            ast_expr_eval(e, s)
        }
        AstExprKind::Unary(_) => unary_pf_reduce(e, s),
        AstExprKind::Binary(b) => binary_pf_reduce(b.e1, b.op, b.e2, s),
        AstExprKind::Call(_) => call_pf_reduce(e, s),
        AstExprKind::StructMember(_) => structmember_pf_reduce(e, s),
        AstExprKind::Bracketed => ast_expr_pf_reduce(ast_expr_bracketed_root(e), s),
        _ => panic!(),
    }
}

unsafe fn ast_expr_destroy_unary(mut expr: *mut AstExpr) {
    assert!(matches!((*expr).kind, AstExprKind::Unary(_)));
    ast_expr_destroy((*expr).root);
}

unsafe fn ast_expr_member_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::StructMember(field) = (*expr).kind else {
        panic!()
    };
    let mut root: *mut AstExpr = (*expr).root;
    if matches!((*root).kind, AstExprKind::Unary(_)) {
        return ast_expr_member_deref_str_build(root, field, b);
    }
    let mut r: *mut libc::c_char = ast_expr_str(root);
    strbuilder_printf(b, b"%s.%s\0" as *const u8 as *const libc::c_char, r, field);
    free(r as *mut libc::c_void);
}

unsafe fn ast_expr_member_deref_str_build(
    mut root: *mut AstExpr,
    mut member: *mut libc::c_char,
    mut b: *mut StrBuilder,
) {
    if !(ast_expr_unary_op(root) == AstUnaryOp::Dereference) {
        panic!();
    }
    let mut inner: *mut AstExpr = ast_expr_unary_operand(root);
    let mut e1: *mut AstExpr = ast_expr_binary_e1(inner);
    let mut e2: *mut AstExpr = ast_expr_binary_e2(inner);
    let mut left: *mut libc::c_char = ast_expr_str(e1);
    if matches!((*e2).kind, AstExprKind::Constant(_))
        && ast_expr_as_constant(e2) == 0 as libc::c_int
    {
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

unsafe fn ast_expr_incdec_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::IncDec(incdec) = (*expr).kind else {
        panic!()
    };
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    let mut op: *mut libc::c_char = (if incdec.inc != 0 {
        b"++\0" as *const u8 as *const libc::c_char
    } else {
        b"--\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    if incdec.pre != 0 {
        strbuilder_printf(b, b"%s%s\0" as *const u8 as *const libc::c_char, op, root);
    } else {
        strbuilder_printf(b, b"%s%s\0" as *const u8 as *const libc::c_char, root, op);
    }
    free(root as *mut libc::c_void);
}

unsafe fn ast_expr_call_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::Call(call) = (*expr).kind else {
        panic!()
    };
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"%s(\0" as *const u8 as *const libc::c_char, root);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        let mut arg: *mut libc::c_char = ast_expr_str(*call.arg.offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%s\0" as *const u8 as *const libc::c_char,
            arg,
            if (i + 1 as libc::c_int) < call.n {
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

unsafe fn ast_expr_destroy_call(mut expr: *mut AstExpr) {
    let AstExprKind::Call(call) = (*expr).kind else {
        panic!()
    };
    ast_expr_destroy((*expr).root);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        ast_expr_destroy(*call.arg.offset(i as isize));
        i += 1;
    }
    free(call.arg as *mut libc::c_void);
}

unsafe fn ast_expr_destroy_incdec(mut expr: *mut AstExpr) {
    assert!(matches!((*expr).kind, AstExprKind::IncDec(_)));
    ast_expr_destroy((*expr).root);
}

pub unsafe fn ast_expr_inverted_copy(mut expr: *mut AstExpr, mut invert: bool) -> *mut AstExpr {
    let mut copy: *mut AstExpr = ast_expr_copy(expr);
    if invert {
        ast_expr_unary_create(copy, AstUnaryOp::Bang)
    } else {
        copy
    }
}

pub unsafe fn ast_expr_member_field(mut expr: *mut AstExpr) -> *mut libc::c_char {
    let AstExprKind::StructMember(field) = (*expr).kind else {
        panic!()
    };
    field
}

pub unsafe fn ast_expr_member_root(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::StructMember(_)));
    return (*expr).root;
}

pub unsafe fn ast_expr_incdec_pre(mut expr: *mut AstExpr) -> bool {
    match (*expr).kind {
        AstExprKind::IncDec(incdec) => incdec.pre != 0,
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_incdec_root(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::IncDec(_)));
    return (*expr).root;
}

unsafe fn ast_expr_copy_call(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::Call(call) = (*expr).kind else {
        panic!()
    };
    let mut arg: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(call.n as usize))
            as *mut *mut AstExpr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        let ref mut fresh4 = *arg.offset(i as isize);
        *fresh4 = ast_expr_copy(*(call.arg).offset(i as isize));
        i += 1;
    }
    return ast_expr_call_create(ast_expr_copy((*expr).root), call.n, arg);
}

pub unsafe fn ast_expr_member_create(
    mut _struct: *mut AstExpr,
    mut field: *mut libc::c_char,
) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::StructMember(field);
    (*expr).root = _struct;
    return expr;
}

pub unsafe fn ast_expr_binary_create(
    mut e1: *mut AstExpr,
    mut op: AstBinaryOp,
    mut e2: *mut AstExpr,
) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Binary(BinaryExpr { e1, op, e2 });
    return expr;
}

pub unsafe fn ast_expr_unary_create(mut root: *mut AstExpr, mut op: AstUnaryOp) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Unary(op);
    (*expr).root = root;
    return expr;
}

pub unsafe fn ast_expr_incdec_to_assignment(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::IncDec(incdec) = (*expr).kind else {
        panic!()
    };

    ast_expr_assignment_create(
        ast_expr_copy((*expr).root),
        ast_expr_binary_create(
            ast_expr_copy((*expr).root),
            if incdec.inc != 0 {
                AstBinaryOp::Addition
            } else {
                AstBinaryOp::Subtraction
            },
            ast_expr_constant_create(1 as libc::c_int),
        ),
    )
}

pub unsafe fn ast_expr_incdec_create(
    mut root: *mut AstExpr,
    mut inc: bool,
    mut pre: bool,
) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::IncDec(IncDecExpr {
        inc: inc as libc::c_int,
        pre: pre as libc::c_int,
    });
    (*expr).root = root;
    expr
}

pub unsafe fn ast_expr_call_args(mut expr: *mut AstExpr) -> *mut *mut AstExpr {
    let AstExprKind::Call(call) = (*expr).kind else {
        panic!()
    };
    call.arg
}

pub unsafe fn ast_expr_call_nargs(mut expr: *mut AstExpr) -> libc::c_int {
    let AstExprKind::Call(call) = (*expr).kind else {
        panic!()
    };
    call.n
}

pub unsafe fn ast_expr_call_root(mut expr: *mut AstExpr) -> *mut AstExpr {
    return (*expr).root;
}

pub unsafe fn ast_expr_call_create(
    mut root: *mut AstExpr,
    mut narg: libc::c_int,
    mut arg: *mut *mut AstExpr,
) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Call(CallExpr { n: narg, arg });
    (*expr).root = root;
    return expr;
}

pub unsafe fn ast_expr_iteration_create() -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Iteration;
    return expr;
}

pub unsafe fn ast_expr_alloc_kind(mut expr: *mut AstExpr) -> AstAllocKind {
    let AstExprKind::Allocation(alloc) = (*expr).kind else {
        panic!()
    };
    alloc.kind
}

pub unsafe fn ast_expr_alloc_arg(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::Allocation(alloc) = (*expr).kind else {
        panic!()
    };
    alloc.arg
}

pub unsafe fn ast_expr_isisdereferencable(mut expr: *mut AstExpr) -> bool {
    return matches!((*expr).kind, AstExprKind::IsDereferencable);
}

pub unsafe fn ast_expr_dealloc_create(mut arg: *mut AstExpr) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Dealloc,
        arg,
    });
    expr
}

pub unsafe fn ast_expr_str(mut expr: *mut AstExpr) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    match (*expr).kind {
        AstExprKind::Identifier(id) => {
            strbuilder_printf(b, id);
        }
        AstExprKind::Constant(_) => {
            ast_expr_constant_str_build(expr, b);
        }
        AstExprKind::StringLiteral(s) => {
            strbuilder_printf(b, b"\"%s\"\0" as *const u8 as *const libc::c_char, s);
        }
        AstExprKind::Bracketed => {
            ast_expr_bracketed_str_build(expr, b);
        }
        AstExprKind::Call(_) => {
            ast_expr_call_str_build(expr, b);
        }
        AstExprKind::IncDec(_) => {
            ast_expr_incdec_str_build(expr, b);
        }
        AstExprKind::StructMember(_) => {
            ast_expr_member_str_build(expr, b);
        }
        AstExprKind::Unary(_) => {
            ast_expr_unary_str_build(expr, b);
        }
        AstExprKind::Binary(_) => {
            ast_expr_binary_str_build(expr, b);
        }
        AstExprKind::Assignment(_) => {
            ast_expr_assignment_str_build(expr, b);
        }
        AstExprKind::IsDeallocand => {
            ast_expr_isdeallocand_str_build(expr, b);
        }
        AstExprKind::IsDereferencable => {
            ast_expr_isdereferencable_str_build(expr, b);
        }
        AstExprKind::ArbArg => {
            strbuilder_putc(b, '$' as i32 as libc::c_char);
        }
        AstExprKind::Allocation(_) => {
            ast_expr_alloc_str_build(expr, b);
        }
        _ => {
            panic!();
        }
    }
    return strbuilder_build(b);
}
unsafe fn ast_expr_alloc_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::Allocation(alloc) = (*expr).kind else {
        panic!()
    };
    let mut arg: *mut libc::c_char = ast_expr_str(alloc.arg);
    match alloc.kind {
        AstAllocKind::Alloc => {
            strbuilder_printf(
                b,
                b".%s %s;\0" as *const u8 as *const libc::c_char,
                b"malloc\0" as *const u8 as *const libc::c_char,
                arg,
            );
        }
        AstAllocKind::Dealloc => {
            strbuilder_printf(
                b,
                b".%s %s;\0" as *const u8 as *const libc::c_char,
                b"free\0" as *const u8 as *const libc::c_char,
                arg,
            );
        }
        AstAllocKind::Clump => {
            strbuilder_printf(
                b,
                b".%s %s;\0" as *const u8 as *const libc::c_char,
                b"clump\0" as *const u8 as *const libc::c_char,
                arg,
            );
        }
    }
    free(arg as *mut libc::c_void);
}

pub unsafe fn ast_expr_alloc_create(mut arg: *mut AstExpr) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Alloc,
        arg,
    });
    expr
}

pub unsafe fn ast_expr_isdereferencable_assertand(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::IsDereferencable));
    return (*expr).root;
}

pub unsafe fn ast_expr_isdeallocand_assertand(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::IsDeallocand));
    return (*expr).root;
}

unsafe fn ast_expr_alloc_copy(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::Allocation(alloc) = (*expr).kind else {
        panic!()
    };
    let mut arg: *mut AstExpr = ast_expr_copy(alloc.arg);
    match alloc.kind {
        AstAllocKind::Alloc => return ast_expr_alloc_create(arg),
        AstAllocKind::Dealloc => return ast_expr_dealloc_create(arg),
        AstAllocKind::Clump => return ast_expr_clump_create(arg),
    }
}

unsafe fn ast_expr_isdereferencable_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"$%s\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

pub unsafe fn ast_expr_assignment_rval(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::Assignment(rval) = (*expr).kind else {
        panic!()
    };
    rval
}

pub unsafe fn ast_expr_copy(mut expr: *mut AstExpr) -> *mut AstExpr {
    if expr.is_null() {
        panic!();
    }
    match (*expr).kind {
        AstExprKind::Identifier(id) => ast_expr_identifier_create(dynamic_str(id)),
        AstExprKind::Constant(c) => {
            if c.ischar {
                ast_expr_constant_create_char(c.constant as libc::c_char)
            } else {
                ast_expr_constant_create(c.constant)
            }
        }
        AstExprKind::StringLiteral(s) => ast_expr_literal_create(dynamic_str(s)),
        AstExprKind::Bracketed => ast_expr_bracketed_create(ast_expr_copy((*expr).root)),
        AstExprKind::Call(_) => ast_expr_copy_call(expr),
        AstExprKind::IncDec(incdec) => ast_expr_incdec_create(
            ast_expr_copy((*expr).root),
            incdec.inc != 0,
            incdec.pre != 0,
        ),
        AstExprKind::StructMember(field) => {
            ast_expr_member_create(ast_expr_copy((*expr).root), dynamic_str(field))
        }
        AstExprKind::Unary(op) => ast_expr_unary_create(ast_expr_copy((*expr).root), op),
        AstExprKind::Binary(b) => {
            ast_expr_binary_create(ast_expr_copy(b.e1), b.op, ast_expr_copy(b.e2))
        }
        AstExprKind::Assignment(rval) => {
            ast_expr_assignment_create(ast_expr_copy((*expr).root), ast_expr_copy(rval))
        }
        AstExprKind::IsDeallocand => ast_expr_isdeallocand_create(ast_expr_copy((*expr).root)),
        AstExprKind::IsDereferencable => {
            ast_expr_isdereferencable_create(ast_expr_copy((*expr).root))
        }
        AstExprKind::ArbArg => ast_expr_arbarg_create(),
        AstExprKind::Allocation(_) => ast_expr_alloc_copy(expr),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_assignment_lval(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::Assignment(_)));
    (*expr).root
}

pub unsafe fn ast_expr_binary_e2(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::Binary(b) = (*expr).kind else {
        panic!()
    };
    b.e2
}

unsafe fn ast_expr_isdeallocand_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(b, b"@%s\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

unsafe fn ast_expr_assignment_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::Assignment(rval) = (*expr).kind else {
        panic!()
    };
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    let mut rval: *mut libc::c_char = ast_expr_str(rval);
    strbuilder_printf(
        b,
        b"%s = %s\0" as *const u8 as *const libc::c_char,
        root,
        rval,
    );
    free(rval as *mut libc::c_void);
    free(root as *mut libc::c_void);
}

unsafe fn ast_expr_binary_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::Binary(binary) = (*expr).kind else {
        panic!()
    };
    let opstr = match binary.op {
        AstBinaryOp::Eq => b"==\0" as &[u8],
        AstBinaryOp::Ne => b"!=\0",
        AstBinaryOp::Lt => b"<\0",
        AstBinaryOp::Gt => b">\0",
        AstBinaryOp::Le => b"<=\0",
        AstBinaryOp::Ge => b">=\0",
        AstBinaryOp::Addition => b"+\0",
        AstBinaryOp::Subtraction => b"-\0",
    };
    let mut e1: *mut libc::c_char = ast_expr_str(binary.e1);
    let mut e2: *mut libc::c_char = ast_expr_str(binary.e2);
    strbuilder_printf(
        b,
        b"%s%s%s\0" as *const u8 as *const libc::c_char,
        e1,
        opstr.as_ptr() as *const libc::c_char,
        e2,
    );
    free(e1 as *mut libc::c_void);
    free(e2 as *mut libc::c_void);
}

pub unsafe fn ast_expr_binary_e1(mut expr: *mut AstExpr) -> *mut AstExpr {
    let AstExprKind::Binary(b) = (*expr).kind else {
        panic!()
    };
    b.e1
}

pub unsafe fn ast_expr_difference_create(
    mut e1: *mut AstExpr,
    mut e2: *mut AstExpr,
) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Subtraction, e2);
}

pub unsafe fn ast_expr_sum_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Addition, e2);
}

unsafe fn ast_expr_unary_str_build(mut expr: *mut AstExpr, mut b: *mut StrBuilder) {
    let AstExprKind::Unary(op) = (*expr).kind else {
        panic!()
    };
    let c = match op {
        AstUnaryOp::Address => b'&',
        AstUnaryOp::Dereference => b'*',
        AstUnaryOp::Positive => b'+',
        AstUnaryOp::Negative => b'-',
        AstUnaryOp::OnesComplement => b'~',
        AstUnaryOp::Bang => b'!',
    } as libc::c_char;
    let mut root: *mut libc::c_char = ast_expr_str((*expr).root);
    strbuilder_printf(
        b,
        b"%c(%s)\0" as *const u8 as *const libc::c_char,
        c as libc::c_int,
        root,
    );
    free(root as *mut libc::c_void);
}

pub unsafe fn ast_expr_ge_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Ge, e2);
}

pub unsafe fn ast_expr_binary_op(mut expr: *mut AstExpr) -> AstBinaryOp {
    let AstExprKind::Binary(b) = (*expr).kind else {
        panic!()
    };
    b.op
}

pub unsafe fn ast_expr_destroy(mut expr: *mut AstExpr) {
    match (*expr).kind {
        AstExprKind::Identifier(_) => {
            ast_expr_destroy_identifier(expr);
        }
        AstExprKind::StringLiteral(_) => {
            ast_expr_destroy_literal(expr);
        }
        AstExprKind::Bracketed => {
            ast_expr_destroy((*expr).root);
        }
        AstExprKind::Call(_) => {
            ast_expr_destroy_call(expr);
        }
        AstExprKind::IncDec(_) => {
            ast_expr_destroy_incdec(expr);
        }
        AstExprKind::StructMember(field) => {
            ast_expr_destroy((*expr).root);
            free(field as *mut libc::c_void);
        }
        AstExprKind::Unary(_) => {
            ast_expr_destroy_unary(expr);
        }
        AstExprKind::Binary(_) => {
            ast_expr_destroy_binary(expr);
        }
        AstExprKind::Assignment(_) => {
            ast_expr_destroy_assignment(expr);
        }
        AstExprKind::IsDeallocand => {
            ast_expr_destroy((*expr).root);
        }
        AstExprKind::IsDereferencable => {
            ast_expr_destroy((*expr).root);
        }
        AstExprKind::Constant(_) | AstExprKind::ArbArg => {}
        AstExprKind::Allocation(alloc) => {
            ast_expr_destroy(alloc.arg);
        }
        _ => {
            panic!();
        }
    }
    free(expr as *mut libc::c_void);
}

pub unsafe fn ast_expr_le_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Le, e2);
}

pub unsafe fn ast_expr_clump_create(mut arg: *mut AstExpr) -> *mut AstExpr {
    let mut expr: *mut AstExpr = ast_expr_create();
    (*expr).kind = AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Clump,
        arg,
    });
    expr
}

pub unsafe fn ast_expr_gt_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Gt, e2);
}

pub unsafe fn ast_expr_lt_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Lt, e2);
}

pub unsafe fn ast_expr_ne_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Ne, e2);
}

unsafe fn ast_expr_destroy_binary(mut expr: *mut AstExpr) {
    let AstExprKind::Binary(b) = (*expr).kind else {
        panic!()
    };
    ast_expr_destroy(b.e1);
    ast_expr_destroy(b.e2);
}

pub unsafe fn ast_expr_eq_create(mut e1: *mut AstExpr, mut e2: *mut AstExpr) -> *mut AstExpr {
    return ast_expr_binary_create(e1, AstBinaryOp::Eq, e2);
}

unsafe fn ast_expr_destroy_assignment(mut expr: *mut AstExpr) {
    let AstExprKind::Assignment(rval) = (*expr).kind else {
        panic!()
    };
    ast_expr_destroy((*expr).root);
    ast_expr_destroy(rval);
}

pub unsafe fn ast_expr_unary_operand(mut expr: *mut AstExpr) -> *mut AstExpr {
    assert!(matches!((*expr).kind, AstExprKind::Unary(_)));
    (*expr).root
}

unsafe fn ast_expr_kind(mut expr: *mut AstExpr) -> AstExprKind {
    (*expr).kind
}

pub unsafe fn ast_expr_unary_isdereference(mut expr: *mut AstExpr) -> bool {
    assert!(matches!(ast_expr_kind(expr), AstExprKind::Unary(_)));
    return ast_expr_unary_op(expr) == AstUnaryOp::Dereference;
}

pub unsafe fn ast_expr_unary_op(mut expr: *mut AstExpr) -> AstUnaryOp {
    let AstExprKind::Unary(op) = (*expr).kind else {
        panic!()
    };
    op
}

pub unsafe fn ast_expr_getfuncs(mut expr: *mut AstExpr) -> Box<StringArr> {
    match (*expr).kind {
        AstExprKind::Identifier(_)
        | AstExprKind::Constant(_)
        | AstExprKind::StringLiteral(_)
        | AstExprKind::StructMember(_)
        | AstExprKind::IsDeallocand
        | AstExprKind::IsDereferencable
        | AstExprKind::ArbArg => return string_arr_create(),
        AstExprKind::Call(_) => ast_expr_call_getfuncs(expr),
        AstExprKind::Bracketed | AstExprKind::Unary(_) | AstExprKind::IncDec(_) => {
            ast_expr_getfuncs((*expr).root)
        }
        AstExprKind::Assignment(rval) => {
            string_arr_concat(&ast_expr_getfuncs((*expr).root), &ast_expr_getfuncs(rval))
        }
        AstExprKind::Binary(b) => {
            string_arr_concat(&ast_expr_getfuncs(b.e1), &ast_expr_getfuncs(b.e2))
        }
        _ => panic!("invalid expr kind"),
    }
}

pub unsafe fn ast_expr_splits(mut e: *mut AstExpr, mut s: *mut State) -> AstStmtSplits {
    match ast_expr_kind(e) {
        AstExprKind::Call(_) => call_splits(e, s),
        AstExprKind::Assignment(rval) => ast_expr_splits(rval, s),
        AstExprKind::Unary(_) => ast_expr_splits(ast_expr_unary_operand(e), s),
        AstExprKind::Binary(_) => binary_splits(e, s),
        AstExprKind::IncDec(_) => ast_expr_splits(ast_expr_incdec_root(e), s),
        AstExprKind::StructMember(_) => ast_expr_splits(ast_expr_member_root(e), s),
        AstExprKind::Constant(_)
        | AstExprKind::Identifier(_)
        | AstExprKind::StringLiteral(_)
        | AstExprKind::ArbArg
        | AstExprKind::IsDereferencable
        | AstExprKind::Allocation(_) => AstStmtSplits {
            n: 0 as libc::c_int,
            cond: 0 as *mut *mut AstExpr,
            err: 0 as *mut Error,
        },
        _ => panic!(),
    }
}

unsafe fn call_splits(mut expr: *mut AstExpr, mut state: *mut State) -> AstStmtSplits {
    let mut err: *mut Error = 0 as *mut Error;
    let mut root: *mut AstExpr = ast_expr_call_root(expr);
    let mut name: *mut libc::c_char = ast_expr_as_identifier(root);
    let mut f: *mut AstFunction = externals_getfunc(state_getext(state), name);
    if f.is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function: `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        err = error_create(strbuilder_build(b));
        return {
            let mut init = AstStmtSplits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut AstExpr,
                err: err,
            };
            init
        };
    }
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut params: *mut *mut AstVariable = ast_function_params(f);
    let mut s_copy: *mut State = state_copy(state);
    let mut args = prepare_arguments(
        ast_expr_call_nargs(expr),
        ast_expr_call_args(expr),
        nparams,
        params,
        s_copy,
    );
    let mut ret_type: *mut AstType = ast_function_type(f);
    state_pushframe(s_copy, dynamic_str(name), ret_type);
    err = prepare_parameters(nparams, params, &args, name, s_copy);
    if !err.is_null() {
        return {
            let mut init = AstStmtSplits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut AstExpr,
                err: err,
            };
            init
        };
    }
    let mut n: libc::c_int = 0 as libc::c_int;
    let mut cond: *mut *mut AstExpr = 0 as *mut *mut AstExpr;
    let mut abs: *mut AstBlock = ast_function_abstract(f);
    let mut ndecls: libc::c_int = ast_block_ndecls(abs);
    if ndecls != 0 {
        let mut var: *mut *mut AstVariable = ast_block_decls(abs);
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < ndecls {
            state_declare(s_copy, *var.offset(i as isize), false);
            i += 1;
        }
    }
    let mut nstmts: libc::c_int = ast_block_nstmts(abs);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts(abs);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nstmts {
        let mut splits: AstStmtSplits = ast_stmt_splits(*stmt.offset(i_0 as isize), s_copy);
        let mut j: libc::c_int = 0 as libc::c_int;
        while j < splits.n {
            n += 1;
            cond = realloc(
                cond as *mut libc::c_void,
                (::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(n as usize),
            ) as *mut *mut AstExpr;
            let ref mut fresh5 = *cond.offset((n - 1 as libc::c_int) as isize);
            *fresh5 = *(splits.cond).offset(j as isize);
            j += 1;
        }
        i_0 += 1;
    }
    state_popframe(s_copy);
    AstStmtSplits {
        n,
        cond,
        err: 0 as *mut Error,
    }
}
unsafe fn binary_splits(mut e: *mut AstExpr, mut s: *mut State) -> AstStmtSplits {
    let mut s1: AstStmtSplits = ast_expr_splits(ast_expr_binary_e1(e), s);
    let mut s2: AstStmtSplits = ast_expr_splits(ast_expr_binary_e2(e), s);
    let mut n: libc::c_int = s1.n + s2.n;
    let mut cond: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(n as usize))
            as *mut *mut AstExpr;
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
    AstStmtSplits {
        n,
        cond,
        err: 0 as *mut Error,
    }
}

unsafe fn ast_expr_call_getfuncs(mut expr: *mut AstExpr) -> Box<StringArr> {
    let AstExprKind::Call(call) = (*expr).kind else {
        panic!()
    };
    let mut res = string_arr_create();
    let mut root: *mut AstExpr = (*expr).root;
    let AstExprKind::Identifier(id) = (*root).kind else {
        panic!()
    };
    string_arr_append(&mut res, dynamic_str(id));
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        res = string_arr_concat(&res, &ast_expr_getfuncs(*(call.arg).offset(i as isize)));
        i += 1;
    }
    res
}

unsafe fn calculate_indegrees(mut g: &Map) -> Box<Map> {
    let mut indegrees = Map::new();
    for key in g.keys() {
        let mut deps: *mut StringArr = g.get(key) as *mut StringArr;
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
        let mut n_arr: *mut StringArr = g.get(key) as *mut StringArr;
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

unsafe fn build_indegree_zero(mut indegrees: &Map) -> Box<StringArr> {
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
    mut ext: *mut Externals,
) -> Box<StringArr> {
    let mut order = string_arr_create();
    let mut g = ast_function_buildgraph(fname, ext);
    let mut indegrees = calculate_indegrees(&g);
    let mut indegree_zero = build_indegree_zero(&indegrees);
    while (*indegree_zero).n > 0 as libc::c_int {
        let mut curr: *mut libc::c_char = string_arr_deque(&mut indegree_zero);
        string_arr_append(&mut order, curr);
        for key in (*g).keys() {
            let mut v: *mut StringArr = g.get(key) as *mut StringArr;
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
    mut decl: *mut *mut AstVariable,
    mut ndecl: libc::c_int,
    mut stmt: *mut *mut AstStmt,
    mut nstmt: libc::c_int,
) -> *mut AstBlock {
    if !(nstmt > 0 as libc::c_int || stmt.is_null()) {
        panic!();
    }
    let mut b: *mut AstBlock = malloc(::core::mem::size_of::<AstBlock>()) as *mut AstBlock;
    (*b).decl = decl;
    (*b).ndecl = ndecl;
    (*b).stmt = stmt;
    (*b).nstmt = nstmt;
    return b;
}

pub unsafe fn ast_block_destroy(mut b: *mut AstBlock) {
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

pub unsafe fn ast_block_copy(mut b: *mut AstBlock) -> *mut AstBlock {
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
    mut var: *mut *mut AstVariable,
) -> *mut *mut AstVariable {
    if !(len == 0 as libc::c_int || !var.is_null()) {
        panic!();
    }
    if len == 0 as libc::c_int {
        return 0 as *mut *mut AstVariable;
    }
    let mut new: *mut *mut AstVariable =
        malloc((::core::mem::size_of::<*mut AstVariable>()).wrapping_mul(len as usize))
            as *mut *mut AstVariable;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < len {
        let ref mut fresh8 = *new.offset(i as isize);
        *fresh8 = ast_variable_copy(*var.offset(i as isize));
        i += 1;
    }
    return new;
}
unsafe fn copy_stmt_arr(mut len: libc::c_int, mut stmt: *mut *mut AstStmt) -> *mut *mut AstStmt {
    if !(len == 0 as libc::c_int || !stmt.is_null()) {
        panic!();
    }
    if len == 0 as libc::c_int {
        return 0 as *mut *mut AstStmt;
    }
    let mut new: *mut *mut AstStmt =
        malloc((::core::mem::size_of::<*mut AstStmt>()).wrapping_mul(len as usize))
            as *mut *mut AstStmt;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < len {
        let ref mut fresh9 = *new.offset(i as isize);
        *fresh9 = ast_stmt_copy(*stmt.offset(i as isize));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_block_str(
    mut b: *mut AstBlock,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut sb: *mut StrBuilder = strbuilder_create();
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

pub unsafe fn ast_block_ndecls(mut b: *mut AstBlock) -> libc::c_int {
    return (*b).ndecl;
}

pub unsafe fn ast_block_decls(mut b: *mut AstBlock) -> *mut *mut AstVariable {
    return (*b).decl;
}

pub unsafe fn ast_block_nstmts(mut b: *mut AstBlock) -> libc::c_int {
    return (*b).nstmt;
}

pub unsafe fn ast_block_stmts(mut b: *mut AstBlock) -> *mut *mut AstStmt {
    if !((*b).nstmt > 0 as libc::c_int || ((*b).stmt).is_null()) {
        panic!();
    }
    return (*b).stmt;
}

pub unsafe fn ast_block_isterminal(mut b: *mut AstBlock, mut s: *mut State) -> bool {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*b).nstmt {
        if ast_stmt_isterminal(*((*b).stmt).offset(i as isize), s) {
            return true;
        }
        i += 1;
    }
    return false;
}

pub unsafe fn ast_block_preconds(mut b: *mut AstBlock) -> PrecondsResult {
    let mut n: libc::c_int = ast_block_nstmts(b);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if ast_stmt_ispre(*stmt.offset(i as isize)) {
            let mut preconds: *mut AstStmt = ast_stmt_labelled_stmt(*stmt.offset(i as isize));
            let mut err: *mut Error = ast_stmt_preconds_validate(preconds);
            if !err.is_null() {
                return {
                    let mut init = PrecondsResult {
                        stmt: 0 as *mut AstStmt,
                        err: err,
                    };
                    init
                };
            }
            return {
                let mut init = PrecondsResult {
                    stmt: preconds,
                    err: 0 as *mut Error,
                };
                init
            };
        }
        i += 1;
    }
    return {
        let mut init = PrecondsResult {
            stmt: 0 as *mut AstStmt,
            err: 0 as *mut Error,
        };
        init
    };
}

pub unsafe fn ast_stmt_process(
    mut stmt: *mut AstStmt,
    mut fname: *mut libc::c_char,
    mut state: *mut State,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    if ast_stmt_kind(stmt) as libc::c_uint == AstStmtKind::CompoundV as libc::c_int as libc::c_uint
    {
        err = ast_stmt_verify(stmt, state);
        if !err.is_null() {
            let mut b: *mut StrBuilder = strbuilder_create();
            let mut loc: *mut LexemeMarker = ast_stmt_lexememarker(stmt);
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
        let mut b_0: *mut StrBuilder = strbuilder_create();
        let mut loc_0: *mut LexemeMarker = ast_stmt_lexememarker(stmt);
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
    return 0 as *mut Error;
}

pub unsafe fn ast_stmt_preprocess(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Preresult {
    if ast_stmt_isassume(stmt) {
        return stmt_installprop(stmt, state);
    }
    return preresult_empty_create();
}

pub unsafe fn ast_stmt_equal(mut s1: *mut AstStmt, mut s2: *mut AstStmt) -> bool {
    if s1.is_null() || s2.is_null() {
        return false;
    }
    if ast_stmt_kind(s1) as libc::c_uint != ast_stmt_kind(s2) as libc::c_uint {
        return false;
    }
    match ast_stmt_kind(s1) {
        AstStmtKind::Expr => return ast_expr_equal(ast_stmt_as_expr(s1), ast_stmt_as_expr(s2)),
        _ => panic!(),
    }
}

pub unsafe fn ast_stmt_labelled_stmt(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Labelled as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.labelled.stmt;
}
unsafe fn labelled_absexec(
    mut stmt: *mut AstStmt,
    mut state: *mut State,
    mut should_setup: bool,
) -> *mut result {
    if !ast_stmt_ispre(stmt) {
        let mut s: *mut libc::c_char = ast_stmt_str(stmt);
        let cstr = std::ffi::CStr::from_ptr(s);
        panic!("expected precondition, got: {cstr:?}");
    }
    let mut setup: *mut AstStmt = ast_stmt_labelled_stmt(stmt);
    if setup.is_null() {
        panic!();
    }
    if !should_setup {
        return result_value_create(0 as *mut Value);
    }
    return ast_stmt_absexec(setup, state, should_setup);
}

pub unsafe fn ast_stmt_copy(mut stmt: *mut AstStmt) -> *mut AstStmt {
    let mut loc: *mut LexemeMarker = if !((*stmt).loc).is_null() {
        lexememarker_copy((*stmt).loc)
    } else {
        0 as *mut LexemeMarker
    };
    match (*stmt).kind {
        AstStmtKind::Labelled => ast_stmt_create_labelled(
            loc,
            dynamic_str((*stmt).u.labelled.label),
            ast_stmt_copy((*stmt).u.labelled.stmt),
        ),

        AstStmtKind::Nop => ast_stmt_create_nop(loc),
        AstStmtKind::Expr => ast_stmt_create_expr(loc, ast_expr_copy((*stmt).u.expr)),
        AstStmtKind::Compound => ast_stmt_create_compound(loc, ast_block_copy((*stmt).u.compound)),
        AstStmtKind::CompoundV => {
            ast_stmt_create_compound_v(loc, ast_block_copy((*stmt).u.compound))
        }
        AstStmtKind::Selection => ast_stmt_create_sel(
            loc,
            (*stmt).u.selection.isswitch,
            ast_expr_copy((*stmt).u.selection.cond),
            ast_stmt_copy((*stmt).u.selection.body),
            if !((*stmt).u.selection.nest).is_null() {
                ast_stmt_copy((*stmt).u.selection.nest)
            } else {
                0 as *mut AstStmt
            },
        ),

        AstStmtKind::Iteration => ast_stmt_create_iter(
            loc,
            ast_stmt_copy((*stmt).u.iteration.init),
            ast_stmt_copy((*stmt).u.iteration.cond),
            ast_expr_copy((*stmt).u.iteration.iter),
            ast_block_copy((*stmt).u.iteration.abstract_0),
            ast_stmt_copy((*stmt).u.iteration.body),
        ),

        AstStmtKind::IterationE => return ast_stmt_copy_iter(stmt),
        AstStmtKind::Jump => ast_stmt_create_jump(
            loc,
            (*stmt).u.jump.kind,
            ast_expr_copy_ifnotnull((*stmt).u.jump.rv),
        ),
        _ => panic!(),
    }
}
unsafe fn labelled_setupabsexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut res: *mut result = ast_stmt_absexec(stmt, state, true);
    if result_iserror(res) {
        return result_as_error(res);
    }
    return 0 as *mut Error;
}
unsafe fn sel_setupabsexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if !(dec.err).is_null() {
        return dec.err;
    }
    if dec.decision {
        return stmt_setupabsexec(ast_stmt_sel_body(stmt), state);
    }
    if !(ast_stmt_sel_nest(stmt)).is_null() {
        panic!();
    }
    return 0 as *mut Error;
}
unsafe fn comp_setupabsexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut b: *mut AstBlock = ast_stmt_as_block(stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let mut nstmt: libc::c_int = ast_block_nstmts(b);
    let mut stmts: *mut *mut AstStmt = ast_block_stmts(b);
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
    return 0 as *mut Error;
}
unsafe fn stmt_setupabsexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    match ast_stmt_kind(stmt) {
        AstStmtKind::Expr | AstStmtKind::Allocation | AstStmtKind::Jump => 0 as *mut Error,
        AstStmtKind::Labelled => labelled_setupabsexec(stmt, state),
        AstStmtKind::Selection => sel_setupabsexec(stmt, state),
        AstStmtKind::Compound => comp_setupabsexec(stmt, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_stmt_setupabsexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    if ast_stmt_kind(stmt) != AstStmtKind::Selection {
        return 0 as *mut Error;
    }
    return stmt_setupabsexec(stmt, state);
}

pub unsafe fn ast_stmt_isselection(mut stmt: *mut AstStmt) -> bool {
    (*stmt).kind == AstStmtKind::Selection
}

pub unsafe fn ast_stmt_isassume(mut stmt: *mut AstStmt) -> bool {
    return (*stmt).kind == AstStmtKind::Labelled
        && strcmp(
            (*stmt).u.labelled.label,
            b"assume\0" as *const u8 as *const libc::c_char,
        ) == 0 as libc::c_int;
}

unsafe fn stmt_installprop(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Preresult {
    return ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state);
}

pub unsafe fn ast_stmt_ispre(mut stmt: *mut AstStmt) -> bool {
    return (*stmt).kind == AstStmtKind::Labelled
        && strcmp(
            (*stmt).u.labelled.label,
            b"setup\0" as *const u8 as *const libc::c_char,
        ) == 0 as libc::c_int;
}

unsafe fn stmt_v_block_verify(mut v_block_stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut b: *mut AstBlock = ast_stmt_as_v_block(v_block_stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let mut nstmts: libc::c_int = ast_block_nstmts(b);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        let mut err: *mut Error = ast_stmt_verify(*stmt.offset(i as isize), state);
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut Error;
}
unsafe fn stmt_expr_verify(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut expr: *mut AstExpr = ast_stmt_as_expr(stmt);
    if ast_expr_decide(expr, state) {
        return 0 as *mut Error;
    }
    return error_create(
        b"cannot verify statement\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
}
unsafe fn iter_empty(mut stmt: *mut AstStmt, mut state: *mut State) -> bool {
    let mut err: *mut Error = ast_stmt_exec(ast_stmt_iter_init(stmt), state);
    if !err.is_null() {
        panic!();
    }
    return !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state);
}
unsafe fn stmt_iter_verify(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    if iter_empty(stmt, state) {
        return 0 as *mut Error;
    }
    let mut body: *mut AstStmt = ast_stmt_iter_body(stmt);
    assert_eq!(ast_stmt_kind(body), AstStmtKind::Compound);
    let mut block: *mut AstBlock = ast_stmt_as_block(body);
    assert_eq!(ast_block_ndecls(block), 0);
    assert_eq!(ast_block_nstmts(block), 1);
    let mut assertion: *mut AstExpr =
        ast_stmt_as_expr(*(ast_block_stmts(block)).offset(0 as libc::c_int as isize));
    let mut lw: *mut AstExpr = ast_stmt_iter_lower_bound(stmt);
    let mut up: *mut AstExpr = ast_stmt_iter_upper_bound(stmt);
    if !ast_expr_rangedecide(assertion, lw, up, state) {
        return error_create(
            b"could not verify\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return 0 as *mut Error;
}

pub unsafe fn ast_stmt_verify(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    match ast_stmt_kind(stmt) {
        AstStmtKind::Nop => 0 as *mut Error,
        AstStmtKind::CompoundV => stmt_v_block_verify(stmt, state),
        AstStmtKind::Expr => stmt_expr_verify(stmt, state),
        AstStmtKind::Iteration => stmt_iter_verify(stmt, state),
        _ => panic!(),
    }
}

unsafe fn stmt_compound_exec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut b: *mut AstBlock = ast_stmt_as_block(stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let mut nstmt: libc::c_int = ast_block_nstmts(b);
    let mut stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmt {
        let mut err: *mut Error = ast_stmt_exec(*stmts.offset(i as isize), state);
        if !err.is_null() {
            return err;
        }
        if ast_stmt_isterminal(*stmts.offset(i as isize), state) {
            break;
        }
        i += 1;
    }
    return 0 as *mut Error;
}
unsafe fn stmt_sel_exec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if !(dec.err).is_null() {
        return dec.err;
    }
    if dec.decision {
        return ast_stmt_exec(ast_stmt_sel_body(stmt), state);
    }
    if !(ast_stmt_sel_nest(stmt)).is_null() {
        panic!();
    }
    return 0 as *mut Error;
}
unsafe fn iter_neteffect(mut iter: *mut AstStmt) -> *mut AstStmt {
    let mut abs: *mut AstBlock = ast_stmt_iter_abstract(iter);
    if abs.is_null() {
        panic!();
    }
    let mut nstmts: libc::c_int = ast_block_nstmts(abs);
    if nstmts == 0 {
        return 0 as *mut AstStmt;
    }
    if !(ast_block_ndecls(abs) == 0 as libc::c_int && nstmts == 1 as libc::c_int) {
        panic!();
    }
    return ast_stmt_create_iter(
        0 as *mut LexemeMarker,
        ast_stmt_copy(ast_stmt_iter_init(iter)),
        ast_stmt_copy(ast_stmt_iter_cond(iter)),
        ast_expr_copy(ast_stmt_iter_iter(iter)),
        ast_block_create(
            0 as *mut *mut AstVariable,
            0 as libc::c_int,
            0 as *mut *mut AstStmt,
            0 as libc::c_int,
        ),
        ast_stmt_create_compound(
            0 as *mut LexemeMarker,
            ast_block_copy(ast_stmt_iter_abstract(iter)),
        ),
    );
}
unsafe fn stmt_iter_exec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut neteffect: *mut AstStmt = iter_neteffect(stmt);
    if neteffect.is_null() {
        return 0 as *mut Error;
    }
    let mut res: *mut result = ast_stmt_absexec(neteffect, state, true);
    if result_iserror(res) {
        return result_as_error(res);
    }
    ast_stmt_destroy(neteffect);
    return 0 as *mut Error;
}
unsafe fn stmt_jump_exec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    let mut res: *mut result = ast_expr_eval(ast_stmt_jump_rv(stmt), state);
    if result_iserror(res) {
        return result_as_error(res);
    }
    if result_hasvalue(res) {
        let mut obj: *mut Object = state_getresult(state);
        if obj.is_null() {
            panic!();
        }
        object_assign(obj, value_copy(result_as_value(res)));
    }
    return 0 as *mut Error;
}

pub unsafe fn ast_stmt_exec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut Error {
    match ast_stmt_kind(stmt) {
        AstStmtKind::Nop => return 0 as *mut Error,
        AstStmtKind::Labelled => return ast_stmt_exec(ast_stmt_labelled_stmt(stmt), state),
        AstStmtKind::Compound => return stmt_compound_exec(stmt, state),
        AstStmtKind::CompoundV => return 0 as *mut Error,
        AstStmtKind::Expr => return ast_expr_exec(ast_stmt_as_expr(stmt), state),
        AstStmtKind::Selection => return stmt_sel_exec(stmt, state),
        AstStmtKind::Iteration => return stmt_iter_exec(stmt, state),
        AstStmtKind::Jump => return stmt_jump_exec(stmt, state),
        _ => {
            panic!();
        }
    }
}
unsafe fn ast_stmt_jump_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Jump as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut rv: *mut libc::c_char = ast_expr_str((*stmt).u.jump.rv);
    strbuilder_printf(b, b"return %s;\n\0" as *const u8 as *const libc::c_char, rv);
    free(rv as *mut libc::c_void);
}

pub unsafe fn ast_stmt_iter_abstract(mut stmt: *mut AstStmt) -> *mut AstBlock {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.abstract_0;
}

pub unsafe fn ast_stmt_iter_iter(mut stmt: *mut AstStmt) -> *mut AstExpr {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.iter;
}

pub unsafe fn ast_stmt_lexememarker(mut stmt: *mut AstStmt) -> *mut LexemeMarker {
    return (*stmt).loc;
}
unsafe fn ast_stmt_iter_e_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::IterationE as libc::c_int as libc::c_uint) {
        panic!();
    }
    (*stmt).kind = AstStmtKind::Iteration;
    let mut s: *mut libc::c_char = ast_stmt_str(stmt);
    (*stmt).kind = AstStmtKind::IterationE;
    strbuilder_printf(b, b".%s\0" as *const u8 as *const libc::c_char, s);
    free(s as *mut libc::c_void);
}
unsafe fn ast_stmt_iter_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
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

pub unsafe fn ast_stmt_str(mut stmt: *mut AstStmt) -> *mut libc::c_char {
    if stmt.is_null() {
        panic!();
    }
    let mut b: *mut StrBuilder = strbuilder_create();
    match (*stmt).kind {
        AstStmtKind::Labelled => {
            ast_stmt_labelled_sprint(stmt, b);
        }
        AstStmtKind::Nop => {
            ast_stmt_nop_sprint(stmt, b);
        }
        AstStmtKind::Expr => {
            ast_stmt_expr_sprint(stmt, b);
        }
        AstStmtKind::Compound => {
            ast_stmt_compound_sprint(stmt, b);
        }
        AstStmtKind::CompoundV => {
            ast_stmt_compound_sprint(stmt, b);
        }
        AstStmtKind::Selection => {
            ast_stmt_sel_sprint(stmt, b);
        }
        AstStmtKind::Iteration => {
            ast_stmt_iter_sprint(stmt, b);
        }
        AstStmtKind::IterationE => {
            ast_stmt_iter_e_sprint(stmt, b);
        }
        AstStmtKind::Jump => {
            ast_stmt_jump_sprint(stmt, b);
        }
        _ => {
            panic!();
        }
    }
    return strbuilder_build(b);
}
unsafe fn ast_expr_copy_ifnotnull(mut expr: *mut AstExpr) -> *mut AstExpr {
    return if !expr.is_null() {
        ast_expr_copy(expr)
    } else {
        0 as *mut AstExpr
    };
}

pub unsafe fn ast_stmt_iter_cond(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.cond;
}

pub unsafe fn ast_stmt_iter_init(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.init;
}

pub unsafe fn ast_stmt_labelled_label(mut stmt: *mut AstStmt) -> *mut libc::c_char {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Labelled as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.labelled.label;
}
unsafe fn sel_isterminal(mut stmt: *mut AstStmt, mut s: *mut State) -> bool {
    let mut dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), s);
    if !(dec.err).is_null() {
        panic!();
    }
    if dec.decision {
        return ast_stmt_isterminal(ast_stmt_sel_body(stmt), s);
    }
    return false;
}
unsafe fn comp_absexec(
    mut stmt: *mut AstStmt,
    mut state: *mut State,
    mut should_setup: bool,
) -> *mut result {
    let mut b: *mut AstBlock = ast_stmt_as_block(stmt);
    let mut stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        let mut res: *mut result = ast_stmt_absexec(*stmts.offset(i as isize), state, should_setup);
        if result_iserror(res) {
            return res;
        }
        i += 1;
    }
    return result_value_create(0 as *mut Value);
}

pub unsafe fn ast_stmt_as_block(mut stmt: *mut AstStmt) -> *mut AstBlock {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Compound as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.compound;
}

pub unsafe fn ast_stmt_jump_rv(mut stmt: *mut AstStmt) -> *mut AstExpr {
    return (*stmt).u.jump.rv;
}
unsafe fn jump_absexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut result {
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
    mut stmt: *mut AstStmt,
    mut state: *mut State,
    mut should_setup: bool,
) -> *mut result {
    match ast_stmt_kind(stmt) {
        AstStmtKind::Nop => result_value_create(0 as *mut Value),
        AstStmtKind::Labelled => labelled_absexec(stmt, state, should_setup),
        AstStmtKind::Expr => ast_expr_absexec(ast_stmt_as_expr(stmt), state),
        AstStmtKind::Selection => sel_absexec(stmt, state, should_setup),
        AstStmtKind::Iteration => iter_absexec(stmt, state),
        AstStmtKind::Compound => comp_absexec(stmt, state, should_setup),
        AstStmtKind::Jump => jump_absexec(stmt, state),
        _ => panic!(),
    }
}
unsafe fn ast_stmt_sel_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Selection as libc::c_int as libc::c_uint) {
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
    let mut nest_stmt: *mut AstStmt = (*stmt).u.selection.nest;
    if !nest_stmt.is_null() {
        let mut nest: *mut libc::c_char = ast_stmt_str(nest_stmt);
        strbuilder_printf(b, b" else %s\0" as *const u8 as *const libc::c_char, nest);
        free(nest as *mut libc::c_void);
    }
    free(cond as *mut libc::c_void);
    free(body as *mut libc::c_void);
}

pub unsafe fn ast_stmt_isterminal(mut stmt: *mut AstStmt, mut s: *mut State) -> bool {
    match (*stmt).kind {
        AstStmtKind::Jump => (*stmt).u.jump.kind == AstJumpKind::Return,
        AstStmtKind::Compound => ast_block_isterminal((*stmt).u.compound, s),
        AstStmtKind::Selection => sel_isterminal(stmt, s),
        _ => false,
    }
}

pub unsafe fn ast_stmt_create_jump(
    mut loc: *mut LexemeMarker,
    mut kind: AstJumpKind,
    mut rv: *mut AstExpr,
) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Jump;
    (*stmt).u.jump.kind = AstJumpKind::Return;
    (*stmt).u.jump.rv = rv;
    return stmt;
}

pub unsafe fn sel_decide(mut control: *mut AstExpr, mut state: *mut State) -> Decision {
    let mut res: *mut result = ast_expr_pf_reduce(control, state);
    if result_iserror(res) {
        return {
            let mut init = Decision {
                decision: false,
                err: result_as_error(res),
            };
            init
        };
    }
    if !result_hasvalue(res) {
        panic!();
    }
    let mut v: *mut Value = result_as_value(res);
    if value_issync(v) {
        let mut sync: *mut AstExpr = value_as_sync(v);
        let mut p: *mut Props = state_getprops(state);
        if props_get(p, sync) {
            return {
                let mut init = Decision {
                    decision: true,
                    err: 0 as *mut Error,
                };
                init
            };
        } else if props_contradicts(p, sync) {
            return {
                let mut init = Decision {
                    decision: false,
                    err: 0 as *mut Error,
                };
                init
            };
        }
    }
    if value_isconstant(v) {
        if value_as_constant(v) != 0 {
            return {
                let mut init = Decision {
                    decision: true,
                    err: 0 as *mut Error,
                };
                init
            };
        }
        return {
            let mut init = Decision {
                decision: false,
                err: 0 as *mut Error,
            };
            init
        };
    }
    let mut zero: *mut Value = value_int_create(0 as libc::c_int);
    if !values_comparable(zero, v) {
        let mut b: *mut StrBuilder = strbuilder_create();
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
            let mut init = Decision {
                decision: false,
                err: error_create(strbuilder_build(b)),
            };
            init
        };
    }
    let mut nonzero: bool = !value_equal(zero, v);
    value_destroy(zero);
    return {
        let mut init = Decision {
            decision: nonzero,
            err: 0 as *mut Error,
        };
        init
    };
}
unsafe fn ast_stmt_compound_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Compound as libc::c_int as libc::c_uint
        || (*stmt).kind as libc::c_uint == AstStmtKind::CompoundV as libc::c_int as libc::c_uint)
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
unsafe fn ast_stmt_expr_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Expr as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut s: *mut libc::c_char = ast_expr_str((*stmt).u.expr);
    strbuilder_printf(b, b"%s;\0" as *const u8 as *const libc::c_char, s);
    free(s as *mut libc::c_void);
}
unsafe fn ast_stmt_create(mut loc: *mut LexemeMarker) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = calloc(1, ::core::mem::size_of::<AstStmt>()) as *mut AstStmt;
    (*stmt).loc = loc;
    return stmt;
}
unsafe fn ast_stmt_copy_iter(mut stmt: *mut AstStmt) -> *mut AstStmt {
    (*stmt).kind = AstStmtKind::Iteration;
    let mut copy: *mut AstStmt = ast_stmt_copy(stmt);
    (*stmt).kind = AstStmtKind::IterationE;
    return ast_stmt_create_iter_e(copy);
}

pub unsafe fn ast_stmt_create_iter_e(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    (*stmt).kind = AstStmtKind::IterationE;
    return stmt;
}

pub unsafe fn ast_stmt_create_iter(
    mut loc: *mut LexemeMarker,
    mut init: *mut AstStmt,
    mut cond: *mut AstStmt,
    mut iter: *mut AstExpr,
    mut abstract_0: *mut AstBlock,
    mut body: *mut AstStmt,
) -> *mut AstStmt {
    if !(!init.is_null()
        && !cond.is_null()
        && !iter.is_null()
        && !abstract_0.is_null()
        && !body.is_null()) as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Iteration;
    (*stmt).u.iteration.init = init;
    (*stmt).u.iteration.cond = cond;
    (*stmt).u.iteration.iter = iter;
    (*stmt).u.iteration.body = body;
    (*stmt).u.iteration.abstract_0 = abstract_0;
    return stmt;
}
unsafe fn ast_stmt_nop_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
    strbuilder_printf(b, b";\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn ast_stmt_iter_body(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.iteration.body;
}

pub unsafe fn ast_stmt_create_sel(
    mut loc: *mut LexemeMarker,
    mut isswitch: bool,
    mut cond: *mut AstExpr,
    mut body: *mut AstStmt,
    mut nest: *mut AstStmt,
) -> *mut AstStmt {
    if isswitch {
        panic!();
    }
    if cond.is_null() {
        panic!();
    }
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Selection;
    (*stmt).u.selection.isswitch = isswitch;
    (*stmt).u.selection.cond = cond;
    (*stmt).u.selection.body = body;
    (*stmt).u.selection.nest = nest;
    return stmt;
}

pub unsafe fn ast_stmt_create_compound_v(
    mut loc: *mut LexemeMarker,
    mut b: *mut AstBlock,
) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::CompoundV;
    (*stmt).u.compound = b;
    return stmt;
}
unsafe fn hack_alloc_from_neteffect(mut stmt: *mut AstStmt) -> *mut AstExpr {
    let mut body: *mut AstStmt = ast_stmt_iter_body(stmt);
    assert_eq!(ast_stmt_kind(body), AstStmtKind::Compound);
    let mut block: *mut AstBlock = ast_stmt_as_block(body);
    assert_eq!(ast_block_ndecls(block), 0);
    assert_eq!(ast_block_nstmts(block), 1);
    return ast_stmt_as_expr(*(ast_block_stmts(block)).offset(0 as libc::c_int as isize));
}

pub unsafe fn ast_stmt_iter_lower_bound(mut stmt: *mut AstStmt) -> *mut AstExpr {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut init: *mut AstStmt = (*stmt).u.iteration.init;
    if !((*init).kind as libc::c_uint == AstStmtKind::Expr as libc::c_int as libc::c_uint) {
        panic!();
    }
    return ast_expr_assignment_rval((*init).u.expr);
}
unsafe fn ast_stmt_labelled_sprint(mut stmt: *mut AstStmt, mut b: *mut StrBuilder) {
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
    mut stmt: *mut AstStmt,
    mut state: *mut State,
    mut should_setup: bool,
) -> *mut result {
    let mut dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if !(dec.err).is_null() {
        return result_error_create(dec.err);
    }
    if dec.decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    if !(ast_stmt_sel_nest(stmt)).is_null() {
        panic!();
    }
    return result_value_create(0 as *mut Value);
}

pub unsafe fn ast_stmt_sel_nest(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Selection as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.selection.nest;
}

pub unsafe fn ast_stmt_create_labelled(
    mut loc: *mut LexemeMarker,
    mut label: *mut libc::c_char,
    mut substmt: *mut AstStmt,
) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Labelled;
    (*stmt).u.labelled.label = label;
    (*stmt).u.labelled.stmt = substmt;
    return stmt;
}

pub unsafe fn ast_stmt_create_nop(mut loc: *mut LexemeMarker) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Nop;
    return stmt;
}

pub unsafe fn ast_stmt_create_expr(
    mut loc: *mut LexemeMarker,
    mut expr: *mut AstExpr,
) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Expr;
    (*stmt).u.expr = expr;
    return stmt;
}

pub unsafe fn ast_stmt_create_compound(
    mut loc: *mut LexemeMarker,
    mut b: *mut AstBlock,
) -> *mut AstStmt {
    let mut stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Compound;
    (*stmt).u.compound = b;
    return stmt;
}

pub unsafe fn ast_stmt_sel_cond(mut stmt: *mut AstStmt) -> *mut AstExpr {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Selection as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.selection.cond;
}

pub unsafe fn ast_stmt_sel_body(mut stmt: *mut AstStmt) -> *mut AstStmt {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Selection as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.selection.body;
}

pub unsafe fn ast_stmt_iter_upper_bound(mut stmt: *mut AstStmt) -> *mut AstExpr {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Iteration as libc::c_int as libc::c_uint) {
        panic!();
    }
    let mut cond: *mut AstStmt = (*stmt).u.iteration.cond;
    if !((*cond).kind as libc::c_uint == AstStmtKind::Expr as libc::c_int as libc::c_uint) {
        panic!();
    }
    return ast_expr_binary_e2((*cond).u.expr);
}
unsafe fn iter_absexec(mut stmt: *mut AstStmt, mut state: *mut State) -> *mut result {
    let mut err: *mut Error = 0 as *mut Error;
    let mut alloc: *mut AstExpr = hack_alloc_from_neteffect(stmt);
    let mut lw: *mut AstExpr = ast_stmt_iter_lower_bound(stmt);
    let mut up: *mut AstExpr = ast_stmt_iter_upper_bound(stmt);
    err = ast_expr_alloc_rangeprocess(alloc, lw, up, state);
    if !err.is_null() {
        return result_error_create(err);
    }
    return result_value_create(0 as *mut Value);
}

pub unsafe fn ast_stmt_destroy(mut stmt: *mut AstStmt) {
    match (*stmt).kind {
        AstStmtKind::Labelled => {
            free((*stmt).u.labelled.label as *mut libc::c_void);
            ast_stmt_destroy((*stmt).u.labelled.stmt);
        }
        AstStmtKind::Nop => {}
        AstStmtKind::Compound | AstStmtKind::CompoundV => {
            ast_block_destroy((*stmt).u.compound);
        }
        AstStmtKind::Selection => {
            ast_expr_destroy((*stmt).u.selection.cond);
            ast_stmt_destroy((*stmt).u.selection.body);
            if !((*stmt).u.selection.nest).is_null() {
                ast_stmt_destroy((*stmt).u.selection.nest);
            }
        }
        AstStmtKind::Iteration | AstStmtKind::IterationE => {
            ast_stmt_destroy((*stmt).u.iteration.init);
            ast_stmt_destroy((*stmt).u.iteration.cond);
            ast_stmt_destroy((*stmt).u.iteration.body);
            ast_expr_destroy((*stmt).u.iteration.iter);
            ast_block_destroy((*stmt).u.iteration.abstract_0);
        }
        AstStmtKind::Expr => {
            ast_expr_destroy((*stmt).u.expr);
        }
        AstStmtKind::Jump => {
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
unsafe fn ast_stmt_destroy_jump(mut stmt: *mut AstStmt) {
    let mut rv: *mut AstExpr = (*stmt).u.jump.rv;
    if rv.is_null() {
        return;
    }
    assert_eq!((*stmt).u.jump.kind, AstJumpKind::Return);
    ast_expr_destroy(rv);
}

pub unsafe fn ast_stmt_as_expr(mut stmt: *mut AstStmt) -> *mut AstExpr {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::Expr as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.expr;
}

pub unsafe fn ast_stmt_as_v_block(mut stmt: *mut AstStmt) -> *mut AstBlock {
    if !((*stmt).kind as libc::c_uint == AstStmtKind::CompoundV as libc::c_int as libc::c_uint) {
        panic!();
    }
    return (*stmt).u.compound;
}

pub unsafe fn ast_stmt_kind(mut stmt: *mut AstStmt) -> AstStmtKind {
    return (*stmt).kind;
}

pub unsafe fn ast_stmt_getfuncs(mut stmt: *mut AstStmt) -> Box<StringArr> {
    match (*stmt).kind {
        AstStmtKind::Nop => string_arr_create(),
        AstStmtKind::Labelled => ast_stmt_getfuncs((*stmt).u.labelled.stmt),
        AstStmtKind::Compound | AstStmtKind::CompoundV => ast_stmt_compound_getfuncs(stmt),
        AstStmtKind::Expr => ast_stmt_expr_getfuncs(stmt),
        AstStmtKind::Selection => ast_stmt_selection_getfuncs(stmt),
        AstStmtKind::Iteration | AstStmtKind::IterationE => ast_stmt_iteration_getfuncs(stmt),
        AstStmtKind::Jump => ast_expr_getfuncs((*stmt).u.jump.rv),
        _ => panic!("invalid stmt kind"),
    }
}

pub unsafe fn ast_stmt_splits(mut stmt: *mut AstStmt, mut s: *mut State) -> AstStmtSplits {
    match (*stmt).kind {
        AstStmtKind::Nop => {
            return {
                let mut init = AstStmtSplits {
                    n: 0 as libc::c_int,
                    cond: 0 as *mut *mut AstExpr,
                    err: 0 as *mut Error,
                };
                init
            };
        }
        AstStmtKind::Expr => return ast_expr_splits((*stmt).u.expr, s),
        AstStmtKind::Selection => return stmt_sel_splits(stmt, s),
        AstStmtKind::Jump => {
            if !((*stmt).u.jump.rv).is_null() {
                return ast_expr_splits((*stmt).u.jump.rv, s);
            }
            AstStmtSplits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut AstExpr,
                err: 0 as *mut Error,
            }
        }
        AstStmtKind::Labelled => return ast_stmt_splits((*stmt).u.labelled.stmt, s),
        AstStmtKind::Iteration | AstStmtKind::Compound | AstStmtKind::CompoundV => AstStmtSplits {
            n: 0 as libc::c_int,
            cond: 0 as *mut *mut AstExpr,
            err: 0 as *mut Error,
        },
        _ => panic!(),
    }
}
unsafe fn stmt_sel_splits(mut stmt: *mut AstStmt, mut s: *mut State) -> AstStmtSplits {
    let mut res: *mut result = ast_expr_pf_reduce((*stmt).u.selection.cond, s);
    let mut v: *mut Value = result_as_value(res);
    let mut e: *mut AstExpr = value_to_expr(v);
    if condexists(e, s) as libc::c_int != 0 || value_isconstant(v) as libc::c_int != 0 {
        return {
            let mut init = AstStmtSplits {
                n: 0 as libc::c_int,
                cond: 0 as *mut *mut AstExpr,
                err: 0 as *mut Error,
            };
            init
        };
    }
    let mut cond: *mut *mut AstExpr =
        malloc(::core::mem::size_of::<*mut AstExpr>()) as *mut *mut AstExpr;
    let ref mut fresh10 = *cond.offset(0 as libc::c_int as isize);
    *fresh10 = e;
    AstStmtSplits {
        n: 1 as libc::c_int,
        cond,
        err: 0 as *mut Error,
    }
}

unsafe fn condexists(mut cond: *mut AstExpr, mut s: *mut State) -> bool {
    let mut res: *mut result = ast_expr_pf_reduce(cond, s);
    if !(!result_iserror(res) && result_hasvalue(res) as libc::c_int != 0) {
        panic!();
    }
    let mut reduced: *mut AstExpr = value_to_expr(result_as_value(res));
    let mut p: *mut Props = state_getprops(s);
    return props_get(p, reduced) as libc::c_int != 0
        || props_contradicts(p, reduced) as libc::c_int != 0;
}

unsafe fn ast_stmt_expr_getfuncs(mut stmt: *mut AstStmt) -> Box<StringArr> {
    ast_expr_getfuncs((*stmt).u.expr)
}

unsafe fn ast_stmt_selection_getfuncs(mut stmt: *mut AstStmt) -> Box<StringArr> {
    let mut cond: *mut AstExpr = (*stmt).u.selection.cond;
    let mut body: *mut AstStmt = (*stmt).u.selection.body;
    let mut nest: *mut AstStmt = (*stmt).u.selection.nest;
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

unsafe fn ast_stmt_iteration_getfuncs(mut stmt: *mut AstStmt) -> Box<StringArr> {
    let mut init: *mut AstStmt = (*stmt).u.iteration.init;
    let mut cond: *mut AstStmt = (*stmt).u.iteration.cond;
    let mut body: *mut AstStmt = (*stmt).u.iteration.body;
    let mut iter: *mut AstExpr = (*stmt).u.iteration.iter;
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

unsafe fn ast_stmt_compound_getfuncs(mut stmt: *mut AstStmt) -> Box<StringArr> {
    let mut res = string_arr_create();
    let mut b: *mut AstBlock = (*stmt).u.compound;
    let mut stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        res = string_arr_concat(&res, &ast_stmt_getfuncs(*stmts.offset(i as isize)));
        i += 1;
    }
    res
}

pub unsafe fn ast_stmt_preconds_validate(mut stmt: *mut AstStmt) -> *mut Error {
    match (*stmt).kind {
        AstStmtKind::Expr | AstStmtKind::Allocation | AstStmtKind::Iteration => 0 as *mut Error,
        AstStmtKind::Selection => preconds_selection_verify(stmt),
        AstStmtKind::Compound => preconds_compound_verify(stmt),
        _ => panic!(),
    }
}
unsafe fn preconds_selection_verify(mut stmt: *mut AstStmt) -> *mut Error {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut l: *mut LexemeMarker = ast_stmt_lexememarker(stmt);
    strbuilder_printf(
        b,
        b"%s setup preconditions must be decidable\0" as *const u8 as *const libc::c_char,
        lexememarker_str(l),
    );
    return error_create(strbuilder_build(b));
}
unsafe fn preconds_compound_verify(mut stmt: *mut AstStmt) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut b: *mut AstBlock = (*stmt).u.compound;
    let mut stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        err = ast_stmt_preconds_validate(*stmts.offset(i as isize));
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut Error;
}

pub unsafe fn ast_type_isint(mut t: *mut AstType) -> bool {
    (*t).base == AstTypeBase::Int
}

pub unsafe fn ast_type_ispointer(mut t: *mut AstType) -> bool {
    (*t).base == AstTypeBase::Pointer
}

pub unsafe fn ast_type_create(mut base: AstTypeBase, mut mod_0: AstTypeModifier) -> *mut AstType {
    let mut t: *mut AstType = malloc(::core::mem::size_of::<AstType>()) as *mut AstType;
    if t.is_null() {
        panic!();
    }
    (*t).base = base;
    (*t).mod_0 = mod_0 as libc::c_int;
    return t;
}

pub unsafe fn ast_type_create_ptr(mut ref_0: *mut AstType) -> *mut AstType {
    if ref_0.is_null() {
        panic!();
    }
    let mut t: *mut AstType = ast_type_create(AstTypeBase::Pointer, 0 as AstTypeModifier);
    (*t).c2rust_unnamed.ptr_type = ref_0;
    return t;
}

pub unsafe fn ast_type_create_voidptr() -> *mut AstType {
    let mut t: *mut AstType = ast_type_create(AstTypeBase::Pointer, 0 as AstTypeModifier);
    (*t).c2rust_unnamed.ptr_type = 0 as *mut AstType;
    return t;
}

pub unsafe fn ast_type_create_arr(mut base: *mut AstType, mut length: libc::c_int) -> *mut AstType {
    if base.is_null() {
        panic!();
    }
    let mut t: *mut AstType = ast_type_create(AstTypeBase::Array, 0 as AstTypeModifier);
    (*t).c2rust_unnamed.arr.type_0 = base;
    (*t).c2rust_unnamed.arr.length = length;
    return t;
}

pub unsafe fn ast_type_create_struct(
    mut tag: *mut libc::c_char,
    mut members: *mut AstVariableArr,
) -> *mut AstType {
    let mut t: *mut AstType = ast_type_create(AstTypeBase::Struct, 0 as AstTypeModifier);
    (*t).c2rust_unnamed.structunion.tag = tag;
    (*t).c2rust_unnamed.structunion.members = members;
    return t;
}

pub unsafe fn ast_type_create_userdef(mut name: *mut libc::c_char) -> *mut AstType {
    let mut t: *mut AstType = ast_type_create(AstTypeBase::UserDefined, 0 as AstTypeModifier);
    (*t).c2rust_unnamed.userdef = name;
    return t;
}

pub unsafe fn ast_type_vconst(
    mut t: *mut AstType,
    mut s: *mut State,
    mut comment: *mut libc::c_char,
    mut persist: bool,
) -> *mut Value {
    match (*t).base {
        AstTypeBase::Int => return value_int_indefinite_create(),
        AstTypeBase::Pointer => return value_ptr_indefinite_create(),
        AstTypeBase::UserDefined => {
            return ast_type_vconst(
                externals_gettypedef(state_getext(s), (*t).c2rust_unnamed.userdef),
                s,
                comment,
                persist,
            );
        }
        AstTypeBase::Struct => return value_struct_indefinite_create(t, s, comment, persist),
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_type_isstruct(mut t: *mut AstType) -> bool {
    (*t).base == AstTypeBase::Struct
}

pub unsafe fn ast_type_struct_complete(
    mut t: *mut AstType,
    mut ext: *mut Externals,
) -> *mut AstType {
    if !(ast_type_struct_members(t)).is_null() {
        return t;
    }
    let mut tag: *mut libc::c_char = ast_type_struct_tag(t);
    if tag.is_null() {
        panic!();
    }
    return externals_getstruct(ext, tag);
}

pub unsafe fn ast_type_struct_members(mut t: *mut AstType) -> *mut AstVariableArr {
    assert_eq!((*t).base, AstTypeBase::Struct);
    (*t).c2rust_unnamed.structunion.members
}

pub unsafe fn ast_type_struct_tag(mut t: *mut AstType) -> *mut libc::c_char {
    assert_eq!((*t).base, AstTypeBase::Struct);
    (*t).c2rust_unnamed.structunion.tag
}

pub unsafe fn ast_type_create_struct_anonym(mut members: *mut AstVariableArr) -> *mut AstType {
    ast_type_create_struct(0 as *mut libc::c_char, members)
}

pub unsafe fn ast_type_create_struct_partial(mut tag: *mut libc::c_char) -> *mut AstType {
    ast_type_create_struct(tag, 0 as *mut AstVariableArr)
}

pub unsafe fn ast_type_copy_struct(mut old: *mut AstType) -> *mut AstType {
    assert_eq!((*old).base, AstTypeBase::Struct);
    let mut new: *mut AstType = ast_type_create(
        AstTypeBase::Struct,
        (*old).mod_0 as libc::c_uint as AstTypeModifier,
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
            0 as *mut AstVariableArr
        };
    return new;
}

pub unsafe fn ast_type_mod_or(mut t: *mut AstType, mut m: AstTypeModifier) {
    (*t).mod_0 = ((*t).mod_0 as libc::c_uint | m as libc::c_uint) as libc::c_int;
}

pub unsafe fn ast_type_istypedef(mut t: *mut AstType) -> bool {
    ((*t).mod_0 as libc::c_uint as AstTypeModifier) & MOD_TYPEDEF != 0
}

pub unsafe fn ast_type_destroy(mut t: *mut AstType) {
    match (*t).base {
        AstTypeBase::Pointer => {
            if ((*t).c2rust_unnamed.ptr_type).is_null() {
                panic!();
            }
            ast_type_destroy((*t).c2rust_unnamed.ptr_type);
        }
        AstTypeBase::Array => {
            if ((*t).c2rust_unnamed.arr.type_0).is_null() {
                panic!();
            }
            ast_type_destroy((*t).c2rust_unnamed.arr.type_0);
        }
        _ => {}
    }
    free(t as *mut libc::c_void);
}

pub unsafe fn ast_type_copy(mut t: *mut AstType) -> *mut AstType {
    if t.is_null() {
        panic!();
    }
    match (*t).base {
        AstTypeBase::Pointer => {
            return ast_type_create_ptr(ast_type_copy((*t).c2rust_unnamed.ptr_type))
        }
        AstTypeBase::Array => {
            return ast_type_create_arr(
                ast_type_copy((*t).c2rust_unnamed.arr.type_0),
                (*t).c2rust_unnamed.arr.length,
            );
        }
        AstTypeBase::Struct => return ast_type_copy_struct(t),
        AstTypeBase::UserDefined => {
            return ast_type_create_userdef(dynamic_str((*t).c2rust_unnamed.userdef))
        }
        AstTypeBase::Void | AstTypeBase::Int | AstTypeBase::Char => {
            return ast_type_create((*t).base, (*t).mod_0 as libc::c_uint as AstTypeModifier)
        }
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_type_str(mut t: *mut AstType) -> *mut libc::c_char {
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
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut mod_0: *mut libc::c_char = mod_str((*t).mod_0);
    strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, mod_0);
    free(mod_0 as *mut libc::c_void);
    match (*t).base {
        AstTypeBase::Pointer => {
            ast_type_str_build_ptr(b, t);
        }
        AstTypeBase::Array => {
            ast_type_str_build_arr(b, t);
        }
        AstTypeBase::Struct => {
            ast_type_str_build_struct(b, t);
        }
        AstTypeBase::UserDefined => {
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
    let mut b: *mut StrBuilder = strbuilder_create();
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
unsafe fn ast_type_str_build_ptr(mut b: *mut StrBuilder, mut t: *mut AstType) {
    let mut base: *mut libc::c_char = ast_type_str((*t).c2rust_unnamed.ptr_type);
    let mut space: bool = (*(*t).c2rust_unnamed.ptr_type).base as libc::c_uint
        != AstTypeBase::Pointer as libc::c_int as libc::c_uint;
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
unsafe fn ast_type_str_build_arr(mut b: *mut StrBuilder, mut t: *mut AstType) {
    let mut base: *mut libc::c_char = ast_type_str((*t).c2rust_unnamed.arr.type_0);
    strbuilder_printf(
        b,
        b"%s[%d]\0" as *const u8 as *const libc::c_char,
        base,
        (*t).c2rust_unnamed.arr.length,
    );
    free(base as *mut libc::c_void);
}
unsafe fn ast_type_str_build_struct(mut b: *mut StrBuilder, mut t: *mut AstType) {
    let mut tag: *mut libc::c_char = (*t).c2rust_unnamed.structunion.tag;
    let mut members: *mut AstVariableArr = (*t).c2rust_unnamed.structunion.members;
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
    let mut v: *mut *mut AstVariable = ast_variable_arr_v(members);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut s: *mut libc::c_char = ast_variable_str(*v.offset(i as isize));
        strbuilder_printf(b, b"%s; \0" as *const u8 as *const libc::c_char, s);
        free(s as *mut libc::c_void);
        i += 1;
    }
    strbuilder_printf(b, b"}\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn ast_type_base(mut t: *mut AstType) -> AstTypeBase {
    return (*t).base;
}

pub unsafe fn ast_type_ptr_type(mut t: *mut AstType) -> *mut AstType {
    assert_eq!((*t).base, AstTypeBase::Pointer);
    return (*t).c2rust_unnamed.ptr_type;
}

pub unsafe fn ast_variable_create(
    mut name: *mut libc::c_char,
    mut type_0: *mut AstType,
) -> *mut AstVariable {
    let mut v: *mut AstVariable = malloc(::core::mem::size_of::<AstVariable>()) as *mut AstVariable;
    (*v).name = name;
    (*v).type_0 = type_0;
    return v;
}

pub unsafe fn ast_variable_destroy(mut v: *mut AstVariable) {
    ast_type_destroy((*v).type_0);
    free((*v).name as *mut libc::c_void);
    free(v as *mut libc::c_void);
}

pub unsafe fn ast_variable_copy(mut v: *mut AstVariable) -> *mut AstVariable {
    if v.is_null() {
        panic!();
    }
    return ast_variable_create(dynamic_str((*v).name), ast_type_copy((*v).type_0));
}

pub unsafe fn ast_variables_copy(
    mut n: libc::c_int,
    mut v: *mut *mut AstVariable,
) -> *mut *mut AstVariable {
    if !(!v.is_null() || n == 0) {
        panic!();
    }
    let mut new: *mut *mut AstVariable =
        calloc(n as usize, ::core::mem::size_of::<*mut variable>()) as *mut *mut AstVariable;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let ref mut fresh12 = *new.offset(i as isize);
        *fresh12 = ast_variable_copy(*v.offset(i as isize));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_variable_str(mut v: *mut AstVariable) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
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

pub unsafe fn ast_variable_name(mut v: *mut AstVariable) -> *mut libc::c_char {
    return (*v).name;
}

pub unsafe fn ast_variable_type(mut v: *mut AstVariable) -> *mut AstType {
    return (*v).type_0;
}

pub unsafe fn ast_variable_arr_create() -> *mut AstVariableArr {
    return calloc(1, ::core::mem::size_of::<AstVariableArr>()) as *mut AstVariableArr;
}

pub unsafe fn ast_variable_arr_append(mut arr: *mut AstVariableArr, mut v: *mut AstVariable) {
    (*arr).n += 1;
    (*arr).v = realloc(
        (*arr).v as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstVariable>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut AstVariable;
    let ref mut fresh13 = *((*arr).v).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh13 = v;
}

pub unsafe fn ast_variable_arr_destroy(mut arr: *mut AstVariableArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        ast_variable_destroy(*((*arr).v).offset(i as isize));
        i += 1;
    }
    free(arr as *mut libc::c_void);
}

pub unsafe fn ast_variable_arr_n(mut arr: *mut AstVariableArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn ast_variable_arr_v(mut arr: *mut AstVariableArr) -> *mut *mut AstVariable {
    return (*arr).v;
}

pub unsafe fn ast_variable_arr_copy(mut old: *mut AstVariableArr) -> *mut AstVariableArr {
    let mut new: *mut AstVariableArr = ast_variable_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        ast_variable_arr_append(new, ast_variable_copy(*((*old).v).offset(i as isize)));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_function_create(
    mut isaxiom: bool,
    mut ret: *mut AstType,
    mut name: *mut libc::c_char,
    mut nparam: libc::c_int,
    mut param: *mut *mut AstVariable,
    mut abstract_0: *mut AstBlock,
    mut body: *mut AstBlock,
) -> *mut AstFunction {
    let mut f: *mut AstFunction = malloc(::core::mem::size_of::<AstFunction>()) as *mut AstFunction;
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

pub unsafe fn ast_function_destroy(mut f: *mut AstFunction) {
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

pub unsafe fn ast_function_str(mut f: *mut AstFunction) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
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

pub unsafe fn ast_function_name(mut f: *mut AstFunction) -> *mut libc::c_char {
    return (*f).name;
}

pub unsafe fn ast_function_copy(mut f: *mut AstFunction) -> *mut AstFunction {
    if f.is_null() {
        panic!();
    }
    let mut param: *mut *mut AstVariable =
        malloc((::core::mem::size_of::<*mut AstVariable>()).wrapping_mul((*f).nparam as usize))
            as *mut *mut AstVariable;
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
            0 as *mut AstBlock
        },
    );
}

pub unsafe fn ast_function_isaxiom(mut f: *mut AstFunction) -> bool {
    return (*f).isaxiom;
}

pub unsafe fn ast_function_isproto(mut f: *mut AstFunction) -> bool {
    return !((*f).abstract_0).is_null() && ((*f).body).is_null();
}

pub unsafe fn ast_function_absisempty(mut f: *mut AstFunction) -> bool {
    return ast_block_ndecls((*f).abstract_0) == 0 as libc::c_int
        && ast_block_nstmts((*f).abstract_0) == 0 as libc::c_int;
}

pub unsafe fn ast_function_type(mut f: *mut AstFunction) -> *mut AstType {
    return (*f).ret;
}

pub unsafe fn ast_function_body(mut f: *mut AstFunction) -> *mut AstBlock {
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

pub unsafe fn ast_function_abstract(mut f: *mut AstFunction) -> *mut AstBlock {
    if ((*f).abstract_0).is_null() {
        panic!();
    }
    return (*f).abstract_0;
}

pub unsafe fn ast_function_nparams(mut f: *mut AstFunction) -> libc::c_int {
    return (*f).nparam;
}

pub unsafe fn ast_function_params(mut f: *mut AstFunction) -> *mut *mut AstVariable {
    return (*f).param;
}

pub unsafe fn ast_function_preconditions(mut f: *mut AstFunction) -> PrecondsResult {
    return ast_block_preconds(ast_function_abstract(f));
}

pub unsafe fn ast_function_protostitch(
    mut f: *mut AstFunction,
    mut ext: *mut Externals,
) -> *mut AstFunction {
    let mut proto: *mut AstFunction = externals_getfunc(ext, (*f).name);
    if !proto.is_null() && !((*proto).abstract_0).is_null() {
        (*f).abstract_0 = ast_block_copy((*proto).abstract_0);
    }
    return f;
}

pub unsafe fn ast_function_verify(mut f: *mut AstFunction, mut ext: *mut Externals) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut state: *mut State =
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
    return 0 as *mut Error;
}
unsafe fn path_absverify_withstate(mut f: *mut AstFunction, mut state: *mut State) -> *mut Error {
    let mut abs: *mut AstBlock = ast_function_abstract(f);
    let mut ndecls: libc::c_int = ast_block_ndecls(abs);
    let mut var: *mut *mut AstVariable = ast_block_decls(abs);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ndecls {
        state_declare(state, *var.offset(i as isize), false);
        i += 1;
    }
    return path_absverify(f, state, 0 as libc::c_int);
}
unsafe fn path_absverify(
    mut f: *mut AstFunction,
    mut state: *mut State,
    mut index: libc::c_int,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut abs: *mut AstBlock = ast_function_abstract(f);
    let mut nstmts: libc::c_int = ast_block_nstmts(abs);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts(abs);
    let mut i: libc::c_int = index;
    while i < nstmts {
        let mut splits: AstStmtSplits = ast_stmt_splits(*stmt.offset(i as isize), state);
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
            let mut res: *mut result = ast_stmt_absexec(*stmt.offset(i as isize), state, true);
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
    return 0 as *mut Error;
}

pub unsafe fn ast_function_initparams(mut f: *mut AstFunction, mut s: *mut State) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut nparams: libc::c_int = ast_function_nparams(f);
    let mut params: *mut *mut AstVariable = ast_function_params(f);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nparams {
        state_declare(s, *params.offset(i as isize), true);
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
    return 0 as *mut Error;
}
unsafe fn ast_function_precondsinit(mut f: *mut AstFunction, mut s: *mut State) -> *mut Error {
    let mut pre: PrecondsResult = ast_function_preconditions(f);
    if !(pre.err).is_null() {
        return pre.err;
    }
    if (pre.stmt).is_null() {
        return 0 as *mut Error;
    }
    let mut res: *mut result = ast_stmt_absexec(pre.stmt, s, true);
    if result_iserror(res) {
        return result_as_error(res);
    }
    return 0 as *mut Error;
}
unsafe fn inititalise_param(mut param: *mut AstVariable, mut state: *mut State) -> *mut Error {
    let mut name: *mut libc::c_char = ast_variable_name(param);
    let mut t: *mut AstType = ast_variable_type(param);
    let mut obj: *mut Object = state_getobject(state, name);
    if obj.is_null() {
        panic!();
    }
    if !object_hasvalue(obj) {
        let mut val: *mut Value = state_vconst(state, t, dynamic_str(name), true);
        object_assign(obj, val);
    }
    return 0 as *mut Error;
}
unsafe fn abstract_audit(mut f: *mut AstFunction, mut abstract_state: *mut State) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut actual_state: *mut State = state_create_withprops(
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
    return 0 as *mut Error;
}
unsafe fn ast_function_setupabsexec(mut f: *mut AstFunction, mut state: *mut State) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut nstmts: libc::c_int = ast_block_nstmts((*f).abstract_0);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts((*f).abstract_0);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        err = ast_stmt_setupabsexec(*stmt.offset(i as isize), state);
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut Error;
}
unsafe fn abstract_auditwithstate(
    mut f: *mut AstFunction,
    mut actual_state: *mut State,
    mut abstract_state: *mut State,
) -> *mut Error {
    let mut ndecls: libc::c_int = ast_block_ndecls((*f).body);
    let mut var: *mut *mut AstVariable = ast_block_decls((*f).body);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ndecls {
        state_declare(actual_state, *var.offset(i as isize), false);
        i += 1;
    }
    return path_verify(f, actual_state, 0 as libc::c_int, abstract_state);
}
unsafe fn path_verify(
    mut f: *mut AstFunction,
    mut actual_state: *mut State,
    mut index: libc::c_int,
    mut abstract_state: *mut State,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut fname: *mut libc::c_char = ast_function_name(f);
    let mut nstmts: libc::c_int = ast_block_nstmts((*f).body);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts((*f).body);
    let mut i: libc::c_int = index;
    while i < nstmts {
        let mut splits: AstStmtSplits = ast_stmt_splits(*stmt.offset(i as isize), actual_state);
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
        vprintln!(
            "actual: {}",
            CStr::from_ptr(state_str(actual_state)).to_string_lossy()
        );
        let mut b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"%s: garbage on heap\0" as *const u8 as *const libc::c_char,
            ast_function_name(f),
        );
        return error_create(strbuilder_build(b));
    }
    let mut equiv: bool = state_equal(actual_state, abstract_state);
    if !equiv {
        let mut b_0: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b_0,
            b"%s: actual and abstract states differ\0" as *const u8 as *const libc::c_char,
            ast_function_name(f),
        );
        return error_create(strbuilder_build(b_0));
    }
    return 0 as *mut Error;
}

pub unsafe fn ast_function_absexec(mut f: *mut AstFunction, mut state: *mut State) -> *mut result {
    let mut ndecls: libc::c_int = ast_block_ndecls((*f).abstract_0);
    if ndecls != 0 {
        let mut var: *mut *mut AstVariable = ast_block_decls((*f).abstract_0);
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < ndecls {
            state_declare(state, *var.offset(i as isize), false);
            i += 1;
        }
    }
    let mut nstmts: libc::c_int = ast_block_nstmts((*f).abstract_0);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts((*f).abstract_0);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nstmts {
        let mut res: *mut result = ast_stmt_absexec(*stmt.offset(i_0 as isize), state, false);
        if result_iserror(res) {
            return res;
        }
        i_0 += 1;
    }
    let mut obj: *mut Object = state_getresult(state);
    if obj.is_null() {
        panic!();
    }
    return result_value_create(object_as_value(obj));
}
unsafe fn split_path_verify(
    mut f: *mut AstFunction,
    mut actual_state: *mut State,
    mut index: libc::c_int,
    mut cond: *mut AstExpr,
    mut abstract_state: *mut State,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut paths: *mut AstFunctionArr = body_paths(f, index, cond);
    let mut n: libc::c_int = ast_function_arr_len(paths);
    if !(n == 2 as libc::c_int) {
        panic!();
    }
    let mut func: *mut *mut AstFunction = ast_function_arr_func(paths);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut actual_copy: *mut State =
            state_copywithname(actual_state, ast_function_name(*func.offset(i as isize)));
        let mut abstract_copy: *mut State =
            state_copywithname(abstract_state, ast_function_name(*func.offset(i as isize)));
        let mut r: *mut Preresult = ast_expr_assume(
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
    return 0 as *mut Error;
}
unsafe fn recurse_buildgraph(
    mut g: &mut Map,
    mut dedup: &mut Map,
    mut fname: *mut libc::c_char,
    mut ext: *mut Externals,
) {
    let mut local_dedup = Map::new();
    if !(dedup.get(fname)).is_null() {
        return;
    }
    dedup.set(fname, 1 as libc::c_int as *mut libc::c_void);
    let mut f: *mut AstFunction = externals_getfunc(ext, fname);
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
    let mut body: *mut AstBlock = (*f).body;
    let mut nstmts: libc::c_int = ast_block_nstmts(body);
    let mut stmt: *mut *mut AstStmt = ast_block_stmts(body);
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
                let mut f_0: *mut AstFunction = externals_getfunc(ext, *func.offset(j as isize));
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
    mut f: *mut AstFunction,
    mut index: libc::c_int,
    mut cond: *mut AstExpr,
) -> *mut AstFunctionArr {
    let mut res: *mut AstFunctionArr = ast_function_arr_create();
    let mut f_true: *mut AstFunction = ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        split_name((*f).name, cond),
        (*f).nparam,
        ast_variables_copy((*f).nparam, (*f).param),
        ast_block_copy((*f).abstract_0),
        ast_block_copy((*f).body),
    );
    let mut inv_assumption: *mut AstExpr = ast_expr_inverted_copy(cond, true);
    let mut f_false: *mut AstFunction = ast_function_create(
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
    mut f: *mut AstFunction,
    mut state: *mut State,
    mut index: libc::c_int,
    mut cond: *mut AstExpr,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut paths: *mut AstFunctionArr = abstract_paths(f, index, cond);
    let mut n: libc::c_int = ast_function_arr_len(paths);
    if !(n == 2 as libc::c_int) {
        panic!();
    }
    let mut func: *mut *mut AstFunction = ast_function_arr_func(paths);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut s_copy: *mut State =
            state_copywithname(state, ast_function_name(*func.offset(i as isize)));
        let mut r: *mut Preresult =
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
    return 0 as *mut Error;
}
unsafe fn split_paths_absverify(
    mut f: *mut AstFunction,
    mut state: *mut State,
    mut index: libc::c_int,
    mut splits: *mut AstStmtSplits,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*splits).n {
        err = split_path_absverify(f, state, index, *((*splits).cond).offset(i as isize));
        if !err.is_null() {
            return err;
        }
        i += 1;
    }
    return 0 as *mut Error;
}

pub unsafe fn ast_function_buildgraph(
    mut fname: *mut libc::c_char,
    mut ext: *mut Externals,
) -> Box<Map> {
    let mut dedup = Map::new();
    let mut g = Map::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    return g;
}
unsafe fn split_name(
    mut name: *mut libc::c_char,
    mut assumption: *mut AstExpr,
) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
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
    mut f: *mut AstFunction,
    mut actual_state: *mut State,
    mut index: libc::c_int,
    mut splits: *mut AstStmtSplits,
    mut abstract_state: *mut State,
) -> *mut Error {
    let mut err: *mut Error = 0 as *mut Error;
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
    return 0 as *mut Error;
}
unsafe fn body_paths(
    mut f: *mut AstFunction,
    mut index: libc::c_int,
    mut cond: *mut AstExpr,
) -> *mut AstFunctionArr {
    let mut res: *mut AstFunctionArr = ast_function_arr_create();
    let mut f_true: *mut AstFunction = ast_function_create(
        (*f).isaxiom,
        ast_type_copy((*f).ret),
        split_name((*f).name, cond),
        (*f).nparam,
        ast_variables_copy((*f).nparam, (*f).param),
        ast_block_copy((*f).abstract_0),
        (*f).body,
    );
    let mut inv_assumption: *mut AstExpr = ast_expr_inverted_copy(cond, true);
    let mut f_false: *mut AstFunction = ast_function_create(
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

pub unsafe fn ast_function_arr_create() -> *mut AstFunctionArr {
    return calloc(1, ::core::mem::size_of::<AstFunctionArr>()) as *mut AstFunctionArr;
}

pub unsafe fn ast_function_arr_copy(mut old: *mut AstFunctionArr) -> *mut AstFunctionArr {
    let mut new: *mut AstFunctionArr = ast_function_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        ast_function_arr_append(new, ast_function_copy(*((*old).f).offset(i as isize)));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_function_arr_destroy(mut arr: *mut AstFunctionArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        ast_function_destroy(*((*arr).f).offset(i as isize));
        i += 1;
    }
    free(arr as *mut libc::c_void);
}

pub unsafe fn ast_function_arr_append(mut arr: *mut AstFunctionArr, mut f: *mut AstFunction) {
    (*arr).n += 1;
    (*arr).f = realloc(
        (*arr).f as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstFunction>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut AstFunction;
    let ref mut fresh15 = *((*arr).f).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh15 = f;
}

pub unsafe fn ast_function_arr_appendrange(
    mut arr: *mut AstFunctionArr,
    mut range: *mut AstFunctionArr,
) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*range).n {
        ast_function_arr_append(arr, *((*range).f).offset(i as isize));
        i += 1;
    }
}

pub unsafe fn ast_function_arr_len(mut arr: *mut AstFunctionArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn ast_function_arr_func(mut arr: *mut AstFunctionArr) -> *mut *mut AstFunction {
    return (*arr).f;
}

pub unsafe fn ast_functiondecl_create(mut f: *mut AstFunction) -> *mut AstExternDecl {
    let mut decl: *mut AstExternDecl =
        malloc(::core::mem::size_of::<AstExternDecl>()) as *mut AstExternDecl;
    (*decl).kind = AstExternDeclKind::Function(f);
    return decl;
}

pub unsafe fn ast_externdecl_isfunction(mut decl: *mut AstExternDecl) -> bool {
    matches!((*decl).kind, AstExternDeclKind::Function(_))
}

pub unsafe fn ast_externdecl_as_function(mut decl: *mut AstExternDecl) -> *mut AstFunction {
    match &(*decl).kind {
        AstExternDeclKind::Function(f) => *f,
        _ => panic!(),
    }
}

pub unsafe fn ast_decl_create(
    mut name: *mut libc::c_char,
    mut t: *mut AstType,
) -> *mut AstExternDecl {
    let mut decl: *mut AstExternDecl =
        malloc(::core::mem::size_of::<AstExternDecl>()) as *mut AstExternDecl;
    if ast_type_istypedef(t) {
        (*decl).kind = AstExternDeclKind::Typedef(AstTypedefDecl { name, type_0: t });
    } else if ast_type_isstruct(t) {
        if (ast_type_struct_tag(t)).is_null() {
            panic!();
        }
        (*decl).kind = AstExternDeclKind::Struct(t);
    } else {
        (*decl).kind = AstExternDeclKind::Variable(ast_variable_create(name, t));
    }
    return decl;
}

pub unsafe fn ast_externdecl_install(mut decl: *mut AstExternDecl, mut ext: *mut Externals) {
    match (*decl).kind {
        AstExternDeclKind::Function(f) => {
            externals_declarefunc(ext, ast_function_name(f), f);
        }
        AstExternDeclKind::Variable(v) => {
            externals_declarevar(ext, ast_variable_name(v), v);
        }
        AstExternDeclKind::Typedef(typedef) => {
            externals_declaretypedef(ext, typedef.name, typedef.type_0);
        }
        AstExternDeclKind::Struct(s) => {
            externals_declarestruct(ext, s);
        }
    }
}

pub unsafe fn ast_externdecl_destroy(mut decl: *mut AstExternDecl) {
    match (*decl).kind {
        AstExternDeclKind::Function(f) => {
            ast_function_destroy(f);
        }
        AstExternDeclKind::Variable(v) => {
            ast_variable_destroy(v);
        }
        AstExternDeclKind::Typedef(td) => {
            free(td.name as *mut libc::c_void);
            ast_type_destroy(td.type_0);
        }
        AstExternDeclKind::Struct(s) => {
            ast_type_destroy(s);
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

pub unsafe fn ast_create(mut decl: *mut AstExternDecl) -> *mut Ast {
    let mut node: *mut Ast = calloc(1, ::core::mem::size_of::<Ast>()) as *mut Ast;
    return ast_append(node, decl);
}

pub unsafe fn ast_destroy(mut node: *mut Ast) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*node).n {
        ast_externdecl_destroy(*((*node).decl).offset(i as isize));
        i += 1;
    }
    free((*node).decl as *mut libc::c_void);
    free(node as *mut libc::c_void);
}

pub unsafe fn ast_append(mut node: *mut Ast, mut decl: *mut AstExternDecl) -> *mut Ast {
    (*node).n += 1;
    (*node).decl = realloc(
        (*node).decl as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstExternDecl>()).wrapping_mul((*node).n as usize),
    ) as *mut *mut AstExternDecl;
    let ref mut fresh16 = *((*node).decl).offset(((*node).n - 1 as libc::c_int) as isize);
    *fresh16 = decl;
    return node;
}

pub unsafe fn result_error_create(mut err: *mut Error) -> *mut result {
    if err.is_null() {
        panic!();
    }
    let mut r: *mut result = malloc(::core::mem::size_of::<result>()) as *mut result;
    (*r).val = 0 as *mut Value;
    (*r).err = err;
    return r;
}

pub unsafe fn result_value_create(mut val: *mut Value) -> *mut result {
    let mut r: *mut result = malloc(::core::mem::size_of::<result>()) as *mut result;
    (*r).val = val;
    (*r).err = 0 as *mut Error;
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

pub unsafe fn result_as_error(mut res: *mut result) -> *mut Error {
    if ((*res).err).is_null() {
        panic!();
    }
    return (*res).err;
}

pub unsafe fn result_as_value(mut res: *mut result) -> *mut Value {
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

pub unsafe fn lvalue_create(mut t: *mut AstType, mut obj: *mut Object) -> *mut LValue {
    let mut l: *mut LValue = malloc(::core::mem::size_of::<LValue>()) as *mut LValue;
    (*l).t = t;
    (*l).obj = obj;
    return l;
}

pub unsafe fn lvalue_destroy(mut l: *mut LValue) {
    ast_type_destroy((*l).t);
    object_destroy((*l).obj);
    free(l as *mut libc::c_void);
}

pub unsafe fn lvalue_type(mut l: *mut LValue) -> *mut AstType {
    return (*l).t;
}

pub unsafe fn lvalue_object(mut l: *mut LValue) -> *mut Object {
    return (*l).obj;
}

pub unsafe fn preresult_empty_create() -> *mut Preresult {
    return calloc(1, ::core::mem::size_of::<Preresult>()) as *mut Preresult;
}

pub unsafe fn preresult_error_create(mut err: *mut Error) -> *mut Preresult {
    if err.is_null() {
        panic!();
    }
    let mut r: *mut Preresult = preresult_empty_create();
    (*r).err = err;
    return r;
}

pub unsafe fn preresult_contradiction_create() -> *mut Preresult {
    let mut r: *mut Preresult = preresult_empty_create();
    (*r).iscontradiction = true;
    return r;
}

pub unsafe fn preresult_destroy(mut r: *mut Preresult) {
    if !((*r).err).is_null() {
        panic!();
    }
    free(r as *mut libc::c_void);
}

pub unsafe fn preresult_isempty(mut r: *mut Preresult) -> bool {
    return !((*r).iscontradiction as libc::c_int != 0 || !((*r).err).is_null());
}

pub unsafe fn preresult_iserror(mut r: *mut Preresult) -> bool {
    return !((*r).err).is_null();
}

pub unsafe fn preresult_as_error(mut r: *mut Preresult) -> *mut Error {
    if ((*r).err).is_null() {
        panic!();
    }
    return (*r).err;
}

pub unsafe fn preresult_iscontradiction(mut r: *mut Preresult) -> bool {
    return (*r).iscontradiction;
}

pub unsafe fn ast_topological_order(
    mut fname: *mut libc::c_char,
    mut ext: *mut Externals,
) -> Box<StringArr> {
    topological_order(fname, ext)
}

pub unsafe fn ast_protostitch(
    mut f: *mut AstFunction,
    mut ext: *mut Externals,
) -> *mut AstFunction {
    return ast_function_protostitch(f, ext);
}
