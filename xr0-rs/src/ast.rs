#![allow(dead_code, unused_assignments, unused_variables)]

use std::ffi::CStr;
use std::fmt::{self, Display, Formatter};
use std::process;
use std::ptr;

use libc::{free, malloc, realloc, strcmp, strncmp};

use crate::ext::{
    externals_declarefunc, externals_declarestruct, externals_declaretypedef, externals_declarevar,
    externals_getfunc, externals_getstruct, externals_gettypedef,
};
use crate::math::{math_eq, math_ge, math_gt, math_le, math_lt, MathAtom, MathExpr};
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
    state_range_dealloc, state_static_init, state_str, state_vconst,
};
use crate::util::{
    dynamic_str, error_create, strbuilder_build, strbuilder_create, strbuilder_printf,
    strbuilder_putc, string_arr_append, string_arr_concat, string_arr_contains, string_arr_create,
    string_arr_deque, string_arr_n, string_arr_s, Error, Map, Result, StringArr,
};
use crate::value::{
    value_as_constant, value_as_location, value_as_sync, value_copy, value_destroy, value_equal,
    value_int_create, value_int_indefinite_create, value_isconstant, value_isint, value_islocation,
    value_isstruct, value_issync, value_literal_create, value_pf_augment,
    value_ptr_indefinite_create, value_str, value_struct_indefinite_create, value_struct_member,
    value_sync_create, value_to_expr,
};
use crate::{vprintln, Externals, Object, Props, State, StrBuilder, Value};

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
}

pub struct AllocExpr {
    kind: AstAllocKind,
    arg: Box<AstExpr>,
}

pub struct UnaryExpr {
    op: AstUnaryOp,
    arg: Box<AstExpr>,
}

pub struct BinaryExpr {
    op: AstBinaryOp,
    e1: Box<AstExpr>,
    e2: Box<AstExpr>,
}

pub struct AssignmentExpr {
    lval: Box<AstExpr>,
    rval: Box<AstExpr>,
}

pub struct IncDecExpr {
    operand: Box<AstExpr>,
    inc: libc::c_int,
    pre: libc::c_int,
}

pub struct CallExpr {
    fun: Box<AstExpr>,
    n: libc::c_int,
    arg: *mut *mut AstExpr,
}

pub struct ConstantExpr {
    constant: libc::c_int,
    ischar: bool,
}

pub struct StructMemberExpr {
    root: Box<AstExpr>,
    field: *mut libc::c_char,
}

enum AstExprKind {
    Identifier(*mut libc::c_char),
    Constant(ConstantExpr),
    StringLiteral(*mut libc::c_char),
    Bracketed(Box<AstExpr>),
    Iteration,
    Call(CallExpr),
    IncDec(IncDecExpr),
    StructMember(StructMemberExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Assignment(AssignmentExpr),
    IsDeallocand(Box<AstExpr>),
    IsDereferencable(Box<AstExpr>),
    ArbArg,
    Allocation(AllocExpr),
}

pub struct AstType {
    pub modifiers: libc::c_int,
    pub base: AstTypeBase,
}

pub struct AstStructType {
    pub tag: *mut libc::c_char,
    pub members: Option<Box<Vec<*mut AstVariable>>>,
}

pub struct AstVariableArr {
    pub n: libc::c_int,
    pub v: *mut *mut AstVariable,
}

pub struct AstVariable {
    pub name: *mut libc::c_char,
    pub r#type: *mut AstType,
}

pub struct AstArrayType {
    pub r#type: *mut AstType,
    pub length: libc::c_int,
}

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
    Pointer(*mut AstType),
    Array(AstArrayType),
    Struct(AstStructType),
    Union(AstStructType),
    Enum,
    UserDefined(*mut libc::c_char),
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
pub struct LValue {
    pub t: *mut AstType,
    pub obj: *mut Object,
}

pub struct AstFunction {
    pub isaxiom: bool,
    pub ret: *mut AstType,
    pub name: *mut libc::c_char,
    pub params: Vec<*mut AstVariable>,
    pub r#abstract: *mut AstBlock,
    pub body: *mut AstBlock, // can be null
}

pub struct AstBlock {
    pub decls: Vec<*mut AstVariable>,
    pub stmts: Vec<*mut AstStmt>,
}

pub struct AstStmt {
    pub kind: AstStmtKind,
    pub loc: *mut LexemeMarker,
}

pub struct AstAllocStmt {
    pub kind: AstAllocKind,
    pub arg: Box<AstExpr>,
}

pub struct AstJumpStmt {
    pub kind: AstJumpKind,
    pub rv: Option<Box<AstExpr>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstJumpKind {
    Return,
}

pub struct AstIterationStmt {
    pub init: *mut AstStmt,
    pub cond: *mut AstStmt,
    pub body: *mut AstStmt,
    pub iter: Box<AstExpr>,
    pub r#abstract: *mut AstBlock,
}

pub struct AstSelectionStmt {
    pub isswitch: bool,
    pub cond: Box<AstExpr>,
    pub body: *mut AstStmt,
    pub nest: *mut AstStmt,
}

pub struct AstLabelledStmt {
    pub label: *mut libc::c_char,
    pub stmt: *mut AstStmt,
}

pub enum AstStmtKind {
    Nop,
    Labelled(AstLabelledStmt),
    Compound(*mut AstBlock),
    CompoundV(*mut AstBlock),
    Expr(Box<AstExpr>),
    Selection(AstSelectionStmt),
    Iteration(AstIterationStmt),
    IterationE(AstIterationStmt),
    Jump(AstJumpStmt),
    Allocation(AstAllocStmt),
}

// TODO: kill
#[derive(Clone)]
pub struct Decision {
    pub decision: bool,
    pub err: Option<Box<Error>>,
}

// TODO: kill
#[derive(Clone)]
pub struct PrecondsResult {
    pub stmt: *mut AstStmt,
    pub err: Option<Box<Error>>,
}

#[derive(Clone)]
pub struct Preresult {
    pub is_contradiction: bool,
}

// TODO: kill
#[derive(Clone)]
pub struct AstStmtSplits {
    pub n: libc::c_int,
    pub cond: *mut *mut AstExpr,
    pub err: Option<Box<Error>>,
}

pub struct AstExternDecl {
    pub kind: AstExternDeclKind,
}

#[derive(Copy, Clone)]
pub struct AstTypedefDecl {
    pub name: *mut libc::c_char,
    pub type_0: *mut AstType,
}

pub enum AstExternDeclKind {
    Function(*mut AstFunction),
    Variable(*mut AstVariable),
    Typedef(AstTypedefDecl),
    Struct(*mut AstType),
}

pub struct Ast {
    pub decls: Vec<Box<AstExternDecl>>,
}

impl Display for AstExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        unsafe {
            let s = ast_expr_str(self);
            write!(f, "{:?}", std::ffi::CStr::from_ptr(s))?;
            free(s as *mut libc::c_void);
        }
        Ok(())
    }
}

unsafe fn expr_literal_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    Ok(state_static_init(state, expr))
}

unsafe fn expr_constant_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    Ok(value_int_create(ast_expr_as_constant(expr)))
}

unsafe fn rangeprocess_dealloc(
    dealloc: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    let obj: *mut Object = hack_base_object_from_alloc(ast_expr_alloc_arg(dealloc), state);
    return state_range_dealloc(state, obj, lw, up);
}

unsafe fn hack_base_object_from_alloc(expr: &AstExpr, state: *mut State) -> *mut Object {
    let inner = ast_expr_unary_operand(expr);
    let i = ast_expr_identifier_create(dynamic_str(b"i\0" as *const u8 as *const libc::c_char));
    if !ast_expr_equal(ast_expr_binary_e2(inner), &i) {
        panic!();
    }
    drop(i);
    let lval = ast_expr_lvalue(ast_expr_binary_e1(inner), state).unwrap();
    let obj: *mut Object = lvalue_object(lval);
    if obj.is_null() {
        panic!();
    }
    return obj;
}

pub unsafe fn ast_expr_equal(e1: &AstExpr, e2: &AstExpr) -> bool {
    match (&e1.kind, &e2.kind) {
        (AstExprKind::Constant(c1), AstExprKind::Constant(c2)) => c1.constant == c2.constant,
        (AstExprKind::Identifier(id1), AstExprKind::Identifier(id2)) => strcmp(*id1, *id2) == 0,
        (AstExprKind::StringLiteral(s1), AstExprKind::StringLiteral(s2)) => strcmp(*s1, *s2) == 0,
        (AstExprKind::Assignment(a1), AstExprKind::Assignment(a2)) => {
            ast_expr_equal(&a1.lval, &a2.lval) && ast_expr_equal(&a1.rval, &a2.rval)
        }
        (AstExprKind::Unary(u1), AstExprKind::Unary(u2)) => {
            u1.op == u2.op && ast_expr_equal(&u1.arg, &u2.arg)
        }
        (AstExprKind::Binary(b1), AstExprKind::Binary(b2)) => {
            b1.op == b2.op && ast_expr_equal(&b1.e1, &b2.e1) && ast_expr_equal(&b1.e2, &b2.e2)
        }
        (AstExprKind::Call(c1), AstExprKind::Call(c2)) => {
            if c1.n != c2.n {
                return false;
            }
            let mut i: libc::c_int = 0 as libc::c_int;
            while i < c1.n {
                if !ast_expr_equal(&**c1.arg.offset(i as isize), &**c2.arg.offset(i as isize)) {
                    return false;
                }
                i += 1;
            }
            ast_expr_equal(&c1.fun, &c2.fun)
        }
        (AstExprKind::StructMember(m1), AstExprKind::StructMember(m2)) => {
            ast_expr_equal(&m1.root, &m2.root) && strcmp(m1.field, m2.field) == 0
        }
        _ => false,
    }
}

unsafe fn rangeprocess_alloc(
    expr: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    let lval = ast_expr_assignment_lval(expr);
    let rval = ast_expr_assignment_rval(expr);
    let AstExprKind::Allocation(alloc) = &rval.kind else {
        panic!()
    };
    assert_ne!(alloc.kind, AstAllocKind::Dealloc);
    let obj: *mut Object = hack_base_object_from_alloc(lval, state);
    return state_range_alloc(state, obj, lw, up);
}

pub unsafe fn ast_expr_matheval(e: &AstExpr) -> bool {
    match &e.kind {
        AstExprKind::Binary(binary) => {
            let e1 = math_expr(&binary.e1);
            let e2 = math_expr(&binary.e2);
            eval_prop(&e1, binary.op, &e2)
        }
        _ => panic!(),
    }
}

unsafe fn math_expr(e: &AstExpr) -> Box<MathExpr> {
    Box::new(match &e.kind {
        AstExprKind::Identifier(id) => MathExpr::Atom(MathAtom::Variable(dynamic_str(*id))),
        AstExprKind::Constant(c) => {
            if c.constant < 0 {
                MathExpr::Neg(Box::new(MathExpr::Atom(MathAtom::Nat(
                    -c.constant as libc::c_uint,
                ))))
            } else {
                MathExpr::Atom(MathAtom::Nat(c.constant as libc::c_uint))
            }
        }
        AstExprKind::Binary(binary) => {
            MathExpr::Sum(math_expr(&binary.e1), binary_e2(&binary.e2, binary.op))
        }
        _ => {
            panic!();
        }
    })
}

unsafe fn binary_e2(e2: &AstExpr, op: AstBinaryOp) -> Box<MathExpr> {
    match op {
        AstBinaryOp::Addition => math_expr(e2),
        AstBinaryOp::Subtraction => Box::new(MathExpr::Neg(math_expr(e2))),
        _ => panic!(),
    }
}

unsafe fn eval_prop(e1: &MathExpr, op: AstBinaryOp, e2: &MathExpr) -> bool {
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

pub unsafe fn ast_expr_decide(expr: &AstExpr, state: *mut State) -> bool {
    match &expr.kind {
        AstExprKind::Constant(c) => c.constant != 0,
        AstExprKind::Unary(_) => expr_unary_decide(expr, state),
        AstExprKind::IsDeallocand(_) => expr_isdeallocand_decide(expr, state),
        AstExprKind::Binary(_) => expr_binary_decide(expr, state),
        _ => panic!(),
    }
}

unsafe fn expr_binary_decide(expr: &AstExpr, state: *mut State) -> bool {
    let root = ast_expr_eval(ast_expr_binary_e1(expr), state).unwrap();
    let last = ast_expr_eval(ast_expr_binary_e2(expr), state).unwrap();
    return value_compare(root, ast_expr_binary_op(expr), last);
}

unsafe fn value_compare(v1: *mut Value, op: AstBinaryOp, v2: *mut Value) -> bool {
    match op {
        AstBinaryOp::Eq => value_equal(v1, v2),
        AstBinaryOp::Ne => !value_compare(v1, AstBinaryOp::Eq, v2),
        _ => panic!(),
    }
}

unsafe fn expr_isdeallocand_decide(expr: &AstExpr, state: *mut State) -> bool {
    let obj: *mut Object = hack_object_from_assertion(expr, state);
    state_addresses_deallocand(state, obj)
}

unsafe fn hack_object_from_assertion(expr: &AstExpr, state: *mut State) -> *mut Object {
    let assertand = ast_expr_isdeallocand_assertand(expr);
    let res_lval = ast_expr_lvalue(assertand, state).unwrap();
    let obj: *mut Object = lvalue_object(res_lval);
    if obj.is_null() {
        panic!();
    }
    return obj;
}

unsafe fn expr_unary_decide(expr: &AstExpr, state: *mut State) -> bool {
    let operand = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_decide(operand, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_rangedecide(
    expr: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> bool {
    match &expr.kind {
        AstExprKind::Unary(_) => return unary_rangedecide(expr, lw, up, state),
        AstExprKind::IsDeallocand(_) => expr_isdeallocand_rangedecide(expr, lw, up, state),
        _ => panic!(),
    }
}

unsafe fn expr_isdeallocand_rangedecide(
    expr: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> bool {
    let acc = ast_expr_isdeallocand_assertand(expr);
    assert_eq!(ast_expr_unary_op(acc), AstUnaryOp::Dereference);
    let inner = ast_expr_unary_operand(acc);
    let i = ast_expr_identifier_create(dynamic_str(b"i\0" as *const u8 as *const libc::c_char));
    let j = ast_expr_identifier_create(dynamic_str(b"j\0" as *const u8 as *const libc::c_char));
    if !(ast_expr_equal(ast_expr_binary_e2(inner), &i)
        || ast_expr_equal(ast_expr_binary_e2(inner), &j))
    {
        panic!();
    }
    drop(j);
    drop(i);
    let res_lval = ast_expr_lvalue(ast_expr_binary_e1(acc), state).unwrap();
    let obj: *mut Object = lvalue_object(res_lval);
    if obj.is_null() {
        panic!();
    }
    return state_range_aredeallocands(state, obj, lw, up);
}

unsafe fn unary_rangedecide(expr: &AstExpr, lw: &AstExpr, up: &AstExpr, state: *mut State) -> bool {
    let operand = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_rangedecide(operand, lw, up, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_exec(expr: &AstExpr, state: *mut State) -> Result<()> {
    ast_expr_eval(expr, state)?;
    Ok(())
}

pub unsafe fn ast_expr_arbarg_create() -> Box<AstExpr> {
    ast_expr_create(AstExprKind::ArbArg)
}

pub unsafe fn ast_expr_assume(expr: &AstExpr, state: *mut State) -> Result<Preresult> {
    return reduce_assume(expr, true, state);
}

unsafe fn reduce_assume(expr: &AstExpr, value: bool, s: *mut State) -> Result<Preresult> {
    match &expr.kind {
        AstExprKind::Identifier(_) => identifier_assume(expr, value, s),
        AstExprKind::Unary(unary) => {
            assert_eq!(unary.op, AstUnaryOp::Bang);
            return reduce_assume(&unary.arg, !value, s);
        }
        AstExprKind::Bracketed(inner) => reduce_assume(inner, value, s),
        AstExprKind::Call(_) | AstExprKind::StructMember(_) => {
            ast_expr_pf_reduce_assume(expr, value, s)
        }
        AstExprKind::Binary(binary) => binary_assume(binary, value, s),
        _ => {
            panic!();
        }
    }
}

unsafe fn binary_assume(b: &BinaryExpr, value: bool, s: *mut State) -> Result<Preresult> {
    let v1 = ast_expr_pf_reduce(&*b.e1, s).unwrap();
    let v2 = ast_expr_pf_reduce(&*b.e2, s).unwrap();
    // Note: original leaks the expression.
    let expr = ast_expr_binary_create(
        Box::from_raw(value_to_expr(v1)),
        b.op,
        Box::from_raw(value_to_expr(v2)),
    );
    let result = irreducible_assume(&expr, value, s);
    std::mem::forget(expr);
    result
}

unsafe fn irreducible_assume(e: &AstExpr, value: bool, s: *mut State) -> Result<Preresult> {
    let prop = ast_expr_inverted_copy(e, !value);
    irreducible_assume_actual(&prop, s)
}

unsafe fn irreducible_assume_actual(e: &AstExpr, s: *mut State) -> Result<Preresult> {
    let p: *mut Props = state_getprops(s);
    if props_contradicts(p, e) {
        return Ok(Preresult {
            is_contradiction: true,
        });
    }
    props_install(state_getprops(s), Box::into_raw(ast_expr_copy(e)));
    Ok(Preresult {
        is_contradiction: false,
    })
}

pub unsafe fn ast_expr_isdereferencable_create(assertand: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::IsDereferencable(assertand))
}

unsafe fn ast_expr_pf_reduce_assume(
    expr: &AstExpr,
    value: bool,
    s: *mut State,
) -> Result<Preresult> {
    let res_val = ast_expr_pf_reduce(expr, s).unwrap();
    assert!(!res_val.is_null());
    return irreducible_assume(&*value_as_sync(res_val), value, s);
}

unsafe fn identifier_assume(expr: &AstExpr, value: bool, s: *mut State) -> Result<Preresult> {
    let s_copy: *mut State = state_copy(s);
    let res_val = ast_expr_eval(expr, s_copy).unwrap();
    assert!(!res_val.is_null());
    state_destroy(s_copy);
    return irreducible_assume(&*value_as_sync(res_val), value, s);
}

unsafe fn binary_deref_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let arr = ast_expr_eval(ast_expr_binary_e1(expr), state)?;
    if arr.is_null() {
        panic!();
    }
    let deref_obj = state_deref(state, arr, ast_expr_binary_e2(expr))?;
    if deref_obj.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        let s: *mut libc::c_char = ast_expr_str(expr);
        strbuilder_printf(
            b,
            b"undefined indirection: *(%s) has no value\0" as *const u8 as *const libc::c_char,
            s,
        );
        free(s as *mut libc::c_void);
        return Err(error_create(strbuilder_build(b)));
    }
    let v: *mut Value = object_as_value(deref_obj);
    if v.is_null() {
        let b_0: *mut StrBuilder = strbuilder_create();
        let s_0: *mut libc::c_char = ast_expr_str(expr);
        strbuilder_printf(
            b_0,
            b"undefined indirection: *(%s) has no value\0" as *const u8 as *const libc::c_char,
            s_0,
        );
        free(s_0 as *mut libc::c_void);
        return Err(error_create(strbuilder_build(b_0)));
    }
    return Ok(value_copy(&*v));
}

unsafe fn hack_identifier_builtin_eval(
    id: *mut libc::c_char,
    state: *mut State,
) -> Result<*mut Value> {
    if !(state_getvconst(state, id)).is_null()
        || strncmp(id, b"ptr:\0" as *const u8 as *const libc::c_char, 4) == 0 as libc::c_int
    {
        return Ok(value_sync_create(Box::into_raw(
            ast_expr_identifier_create(dynamic_str(id)),
        )));
    }
    return Err(error_create(
        b"not built-in\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    ));
}

pub unsafe fn ast_expr_isdeallocand_create(assertand: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::IsDeallocand(assertand))
}

pub unsafe fn ast_expr_assignment_create(root: Box<AstExpr>, value: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Assignment(AssignmentExpr {
        lval: root,
        rval: value,
    }))
}

unsafe fn ast_expr_bracketed_str_build(inner: &AstExpr, b: *mut StrBuilder) {
    let root: *mut libc::c_char = ast_expr_str(inner);
    strbuilder_printf(b, b"(%s)\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

unsafe fn expr_to_binary(expr: &AstExpr) -> Box<AstExpr> {
    match &expr.kind {
        AstExprKind::Binary(_) => ast_expr_copy(expr),
        _ => ast_expr_binary_create(
            ast_expr_copy(expr),
            AstBinaryOp::Addition,
            ast_expr_constant_create(0 as libc::c_int),
        ),
    }
}

unsafe fn expr_identifier_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    if let Ok(res) = hack_identifier_builtin_eval(ast_expr_as_identifier(expr), state) {
        if !res.is_null() {
            return Ok(res);
        }
    }
    let id: *mut libc::c_char = ast_expr_as_identifier(expr);
    if *id.offset(0 as libc::c_int as isize) as libc::c_int == '#' as i32 {
        return Ok(value_literal_create(id));
    }
    let obj: *mut Object = state_getobject(state, id);
    if obj.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"unknown idenitfier %s\0" as *const u8 as *const libc::c_char,
            id,
        );
        return Err(error_create(strbuilder_build(b)));
    }
    let val: *mut Value = object_as_value(obj);
    if val.is_null() {
        vprintln!(
            "state: {}",
            CStr::from_ptr(state_str(state)).to_string_lossy()
        );
        let b_0: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b_0,
            b"undefined memory access: %s has no value\0" as *const u8 as *const libc::c_char,
            id,
        );
        return Err(error_create(strbuilder_build(b_0)));
    }
    return Ok(value_copy(&*val));
}

unsafe fn expr_structmember_eval(expr: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let root = ast_expr_member_root(expr);
    let res_val = ast_expr_eval(root, s)?;
    let field: *mut libc::c_char = ast_expr_member_field(expr);
    let member: *mut Object = value_struct_member(res_val, field);
    if member.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        let root_str: *mut libc::c_char = ast_expr_str(root);
        strbuilder_printf(
            b,
            b"`%s' has no field `%s'\0" as *const u8 as *const libc::c_char,
            root_str,
            field,
        );
        free(root_str as *mut libc::c_void);
        return Err(error_create(strbuilder_build(b)));
    }
    let obj_value: *mut Value = object_as_value(member);
    let v: *mut Value = if !obj_value.is_null() {
        value_copy(&*obj_value)
    } else {
        ptr::null_mut()
    };
    Ok(v)
}

unsafe fn address_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let operand = ast_expr_unary_operand(expr);
    let id = ast_expr_as_identifier(operand);
    let v: *mut Value = state_getloc(state, id);
    return Ok(v);
}

pub unsafe fn ast_expr_alloc_rangeprocess(
    alloc: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    let lw_val = ast_expr_eval(lw, state)?;
    let up_val = ast_expr_eval(up, state)?;
    let res_lw = Box::from_raw(value_to_expr(lw_val));
    let res_up = Box::from_raw(value_to_expr(up_val));
    match &alloc.kind {
        AstExprKind::Assignment(_) => rangeprocess_alloc(alloc, &*res_lw, &*res_up, state),
        AstExprKind::Allocation(_) => rangeprocess_dealloc(alloc, &*res_lw, &*res_up, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    match &expr.kind {
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

unsafe fn arbarg_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    return Ok(state_vconst(
        state,
        ast_type_create_ptr(ast_type_create(AstTypeBase::Void, 0)),
        0 as *mut libc::c_char,
        false,
    ));
}

unsafe fn expr_binary_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let e1 = ast_expr_binary_e1(expr);
    let e2 = ast_expr_binary_e2(expr);
    let v1 = ast_expr_eval(e1, state)?;
    let v2 = ast_expr_eval(e2, state)?;
    Ok(value_sync_create(Box::into_raw(ast_expr_binary_create(
        Box::from_raw(value_to_expr(v1)),
        ast_expr_binary_op(expr),
        Box::from_raw(value_to_expr(v2)),
    ))))
}

unsafe fn expr_incdec_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let assign = ast_expr_incdec_to_assignment(expr);
    if ast_expr_incdec_pre(expr) {
        expr_assign_eval(&assign, state)
    } else {
        let res = ast_expr_eval(ast_expr_incdec_root(expr), state);
        let _ = expr_assign_eval(&assign, state);
        res
    }
}

unsafe fn expr_assign_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let lval = ast_expr_assignment_lval(expr);
    let rval = ast_expr_assignment_rval(expr);
    let rval_val = ast_expr_eval(rval, state)?;
    if rval_val.is_null() {
        debug_assert!(false);
        return Err(error_create(
            b"undefined indirection (rvalue)\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    let lval_lval = ast_expr_lvalue(lval, state)?;
    let obj: *mut Object = lvalue_object(lval_lval);
    if obj.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        let s: *mut libc::c_char = ast_expr_str(lval);
        strbuilder_printf(
            b,
            b"undefined indirection: %s is not an lvalue\0" as *const u8 as *const libc::c_char,
            s,
        );
        free(s as *mut libc::c_void);
        return Err(error_create(strbuilder_build(b)));
    }
    object_assign(obj, value_copy(&*rval_val));
    Ok(rval_val)
}

pub unsafe fn ast_expr_lvalue(expr: &AstExpr, state: *mut State) -> Result<*mut LValue> {
    match &expr.kind {
        AstExprKind::Identifier(_) => expr_identifier_lvalue(expr, state),
        AstExprKind::Unary(_) => expr_unary_lvalue(expr, state),
        AstExprKind::StructMember(_) => expr_structmember_lvalue(expr, state),
        _ => panic!(),
    }
}

pub unsafe fn expr_structmember_lvalue(expr: &AstExpr, state: *mut State) -> Result<*mut LValue> {
    let root = ast_expr_member_root(&*expr);
    // Note: Original fails to check for errors.
    let root_lval = ast_expr_lvalue(root, state).unwrap();
    let root_obj: *mut Object = lvalue_object(root_lval);
    if root_obj.is_null() {
        panic!();
    }
    let field: *mut libc::c_char = ast_expr_member_field(expr);
    let member: *mut Object = object_getmember(root_obj, lvalue_type(root_lval), field, state);
    if member.is_null() {
        return Err(error_create(
            b"lvalue error\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    let t: *mut AstType = object_getmembertype(root_obj, lvalue_type(root_lval), field, state);
    if t.is_null() {
        panic!();
    }
    Ok(lvalue_create(t, member))
}

pub unsafe fn expr_unary_lvalue(expr: &AstExpr, state: *mut State) -> Result<*mut LValue> {
    if !(ast_expr_unary_op(expr) == AstUnaryOp::Dereference) {
        panic!();
    }
    let inner = ast_expr_unary_operand(expr);
    if matches!(inner.kind, AstExprKind::Identifier(_)) {
        let root_lval = ast_expr_lvalue(inner, state)?;
        let root_obj: *mut Object = lvalue_object(root_lval);
        if root_obj.is_null() {
            return Ok(ptr::null_mut());
        }
        let t: *mut AstType = ast_type_ptr_type(lvalue_type(root_lval));
        let root_val: *mut Value = object_as_value(root_obj);
        if root_val.is_null() {
            panic!();
        }
        let obj = state_deref(
            state,
            root_val,
            &*ast_expr_constant_create(0 as libc::c_int),
        )?;
        return Ok(lvalue_create(t, obj));
    }
    let root_lval = ast_expr_lvalue(ast_expr_binary_e1(inner), state)?;
    let root_obj_0: *mut Object = lvalue_object(root_lval);
    if root_obj_0.is_null() {
        return Ok(ptr::null_mut());
    }
    let t_0: *mut AstType = ast_type_ptr_type(lvalue_type(root_lval));
    let root_val_0: *mut Value = object_as_value(root_obj_0);
    if root_val_0.is_null() {
        panic!();
    }
    let Ok(res_obj) = state_deref(state, root_val_0, ast_expr_binary_e2(inner)) else {
        // Note: error suppressed in original
        return Ok(ptr::null_mut());
    };
    Ok(lvalue_create(t_0, res_obj))
}

pub unsafe fn expr_identifier_lvalue(expr: &AstExpr, state: *mut State) -> Result<*mut LValue> {
    let id: *mut libc::c_char = ast_expr_as_identifier(expr);
    Ok(lvalue_create(
        state_getobjecttype(state, id),
        state_getobject(state, id),
    ))
}

unsafe fn dereference_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let binary = expr_to_binary(ast_expr_unary_operand(expr));
    binary_deref_eval(&binary, state)
}

unsafe fn ast_expr_constant_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::Constant(c) = &expr.kind else {
        panic!()
    };
    let constant: libc::c_int = c.constant;
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

unsafe fn escape_str(c: libc::c_char) -> *mut libc::c_char {
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

unsafe fn expr_call_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let root = ast_expr_call_root(expr);
    let name: *mut libc::c_char = ast_expr_as_identifier(&*root);
    let f: *mut AstFunction = externals_getfunc(state_getext(state), name);
    if f.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        return Err(error_create(strbuilder_build(b)));
    }
    let params = ast_function_params(&*f);
    let rtype = ast_function_type(&*f);
    let args = prepare_arguments(
        ast_expr_call_nargs(expr),
        ast_expr_call_args(expr),
        params,
        state,
    );
    state_pushframe(state, dynamic_str(name), rtype);
    prepare_parameters(params, args, name, state)?;
    call_setupverify(f, state_copy(state))?;
    let mut v = call_absexec(expr, state)?;
    if !v.is_null() {
        v = value_copy(&*v);
    }
    state_popframe(state);
    if !v.is_null() {
        return pf_augment(v, expr, state);
    }
    Ok(ptr::null_mut())
}

unsafe fn call_absexec(expr: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let root = ast_expr_call_root(expr);
    let name: *mut libc::c_char = ast_expr_as_identifier(&*root);
    let f: *mut AstFunction = externals_getfunc(state_getext(s), name);
    if f.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        return Err(error_create(strbuilder_build(b)));
    }
    let v = ast_function_absexec(&*f, s)?;
    if !v.is_null() {
        return Ok(v);
    }
    call_arbitraryresult(expr, &*f, s)
}

unsafe fn call_arbitraryresult(
    expr: &AstExpr,
    f: &AstFunction,
    state: *mut State,
) -> Result<*mut Value> {
    let res = call_to_computed_value(f, state)?;
    assert!(!res.is_null());
    Ok(res)
}

unsafe fn call_to_computed_value(f: &AstFunction, s: *mut State) -> Result<*mut Value> {
    let root: *mut libc::c_char = ast_function_name(f);
    let uncomputed_params = ast_function_params(f);
    let nparams = uncomputed_params.len();
    let computed_param: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(nparams)) as *mut *mut AstExpr;
    for (i, &p) in uncomputed_params.iter().enumerate() {
        let param = ast_expr_identifier_create(dynamic_str(ast_variable_name(p)));
        let v = ast_expr_eval(&param, s)?;
        drop(param);
        assert!(!v.is_null());
        if value_islocation(&*v) {
            let ref mut fresh0 = *computed_param.offset(i as isize);
            *fresh0 = Box::into_raw(ast_expr_identifier_create(value_str(v)));
        } else {
            let ref mut fresh1 = *computed_param.offset(i as isize);
            *fresh1 = value_to_expr(v);
        }
    }
    Ok(value_sync_create(Box::into_raw(ast_expr_call_create(
        ast_expr_identifier_create(dynamic_str(root)),
        uncomputed_params.len() as libc::c_int,
        computed_param,
    ))))
}

pub unsafe fn ast_expr_absexec(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    match &expr.kind {
        AstExprKind::Assignment(_) => assign_absexec(expr, state),
        AstExprKind::IsDereferencable(_) => isdereferencable_absexec(expr, state),
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

unsafe fn expr_unary_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Dereference => dereference_eval(expr, state),
        AstUnaryOp::Address => address_eval(expr, state),
        AstUnaryOp::Bang => Ok(value_literal_create(
            b"hack\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        )),

        _ => panic!(),
    }
}

unsafe fn alloc_absexec(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    match ast_expr_alloc_kind(expr) {
        AstAllocKind::Alloc => Ok(state_alloc(state)),
        AstAllocKind::Dealloc => dealloc_process(expr, state),
        AstAllocKind::Clump => Ok(state_clump(state)),
    }
}

unsafe fn dealloc_process(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let arg = ast_expr_alloc_arg(expr);
    let val = ast_expr_eval(arg, state)?;
    if val.is_null() {
        panic!();
    }
    state_dealloc(state, val)?;
    value_destroy(val);
    Ok(ptr::null_mut())
}

pub unsafe fn prepare_parameters(
    params: &[*mut AstVariable],
    args: Vec<Result<*mut Value>>,
    fname: *mut libc::c_char,
    state: *mut State,
) -> Result<()> {
    assert_eq!(params.len(), args.len());
    for (&param, res) in params.iter().zip(args) {
        state_declare(state, param, true);

        let arg = res?;
        if arg.is_null() {
            let b: *mut StrBuilder = strbuilder_create();
            strbuilder_printf(
                b,
                b"parameter `%s' of function `%s' has no value\0" as *const u8
                    as *const libc::c_char,
                ast_variable_name(param),
                fname,
            );
            return Err(error_create(strbuilder_build(b)));
        }
        let name = ast_expr_identifier_create(dynamic_str(ast_variable_name(param)));
        let lval_lval = ast_expr_lvalue(&name, state)?;
        let obj: *mut Object = lvalue_object(lval_lval);
        drop(name);
        object_assign(obj, value_copy(&*arg));
    }
    Ok(())
}

unsafe fn isdereferencable_absexec(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let p: *mut Props = state_getprops(state);
    props_install(p, expr as *const AstExpr as *mut AstExpr);
    Ok(ptr::null_mut())
}

pub unsafe fn prepare_arguments(
    nargs: libc::c_int,
    arg: *mut *mut AstExpr,
    params: &[*mut AstVariable],
    state: *mut State,
) -> Vec<Result<*mut Value>> {
    assert_eq!(nargs as isize, params.len() as isize);
    let mut args = vec![];
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nargs {
        args.push(ast_expr_eval(&**arg.offset(i as isize), state));
        i += 1;
    }
    args
}

unsafe fn assign_absexec(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let lval = ast_expr_assignment_lval(expr);
    let rval = ast_expr_assignment_rval(expr);
    let val = ast_expr_absexec(rval, state)?;
    if val.is_null() {
        debug_assert!(false);
        return Err(error_create(
            b"undefined indirection (rvalue)\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    let lval_res = ast_expr_lvalue(lval, state)?;
    let obj: *mut Object = lvalue_object(lval_res);
    if obj.is_null() {
        return Err(error_create(
            b"undefined indirection (lvalue)\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    object_assign(obj, value_copy(&*val));
    Ok(val)
}

unsafe fn verify_paramspec(
    param: *mut Value,
    arg: *mut Value,
    param_state: *mut State,
    arg_state: *mut State,
) -> Result<()> {
    if !state_islval(param_state, param) {
        return Ok(());
    }
    if !state_islval(arg_state, arg) {
        return Err(error_create(
            b"must be lvalue\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    if state_isalloc(param_state, param) as libc::c_int != 0 && !state_isalloc(arg_state, arg) {
        return Err(error_create(
            b"must be heap allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    let param_obj = state_get(param_state, value_as_location(&*param), false)?;
    let arg_obj = state_get(arg_state, value_as_location(&*arg), false)?;
    if param_obj.is_null() {
        panic!();
    }
    if arg_obj.is_null() {
        panic!();
    }
    if !object_hasvalue(param_obj) {
        return Ok(());
    }
    if !object_hasvalue(arg_obj) {
        return Err(error_create(
            b"must be rvalue\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    return verify_paramspec(
        object_as_value(param_obj),
        object_as_value(arg_obj),
        param_state,
        arg_state,
    );
}

unsafe fn call_setupverify(f: *mut AstFunction, arg_state: *mut State) -> Result<()> {
    let fname: *mut libc::c_char = ast_function_name(&*f);
    let param_state: *mut State = state_create(
        dynamic_str(fname),
        state_getext(arg_state),
        ast_function_type(&*f),
    );
    ast_function_initparams(&*f, param_state)?;
    let params = ast_function_params(&*f);
    for &p in params {
        let id: *mut libc::c_char = ast_variable_name(p);
        let param_0: *mut Value = state_getloc(param_state, id);
        let arg: *mut Value = state_getloc(arg_state, id);
        if let Err(err) = verify_paramspec(param_0, arg, param_state, arg_state) {
            let b: *mut StrBuilder = strbuilder_create();
            strbuilder_printf(
                b,
                b"parameter %s of %s %s\0" as *const u8 as *const libc::c_char,
                id,
                fname,
                (*err).msg,
            );
            return Err(error_create(strbuilder_build(b)));
        }
    }
    Ok(())
}

pub unsafe fn ast_expr_bracketed_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Bracketed(inner) = &expr.kind else {
        panic!()
    };
    inner
}

pub unsafe fn ast_expr_bracketed_create(root: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Bracketed(root))
}

pub unsafe fn ast_expr_literal_create(s: *mut libc::c_char) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::StringLiteral(s))
}

pub unsafe fn ast_expr_as_literal(expr: &AstExpr) -> *mut libc::c_char {
    let AstExprKind::StringLiteral(s) = &expr.kind else {
        panic!()
    };
    *s
}

pub unsafe fn ast_expr_as_constant(expr: &AstExpr) -> libc::c_int {
    match &expr.kind {
        AstExprKind::Constant(c) => c.constant,
        _ => panic!(),
    }
}

unsafe fn pf_augment(v: *mut Value, call: &AstExpr, state: *mut State) -> Result<*mut Value> {
    if !value_isstruct(&*v) {
        return Ok(value_copy(&*v));
    }
    let res_val = ast_expr_pf_reduce(call, state)?;
    assert!(!res_val.is_null());
    Ok(value_pf_augment(v, value_as_sync(res_val)))
}

pub unsafe fn ast_expr_constant_create_char(c: libc::c_char) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Constant(ConstantExpr {
        ischar: true,
        constant: c as libc::c_int,
    }))
}

pub unsafe fn ast_expr_constant_create(k: libc::c_int) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Constant(ConstantExpr {
        ischar: false,
        constant: k,
    }))
}

pub unsafe fn ast_expr_as_identifier(expr: &AstExpr) -> *mut libc::c_char {
    let AstExprKind::Identifier(id) = &expr.kind else {
        panic!()
    };
    *id
}

unsafe fn ast_expr_create(kind: AstExprKind) -> Box<AstExpr> {
    Box::new(AstExpr { kind })
}

pub unsafe fn ast_expr_identifier_create(s: *mut libc::c_char) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Identifier(s))
}

unsafe fn unary_pf_reduce(e: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let res_val = ast_expr_pf_reduce(ast_expr_unary_operand(e), s)?;
    assert!(!res_val.is_null());
    Ok(value_sync_create(Box::into_raw(ast_expr_unary_create(
        Box::from_raw(value_as_sync(res_val)),
        ast_expr_unary_op(e),
    ))))
}

unsafe fn binary_pf_reduce(
    e1: &AstExpr,
    op: AstBinaryOp,
    e2: &AstExpr,
    s: *mut State,
) -> Result<*mut Value> {
    let v1 = ast_expr_pf_reduce(e1, s)?;
    assert!(!v1.is_null());
    let v2 = ast_expr_pf_reduce(e2, s)?;
    assert!(!v2.is_null());
    return Ok(value_sync_create(Box::into_raw(ast_expr_binary_create(
        Box::from_raw(value_to_expr(v1)),
        op,
        Box::from_raw(value_to_expr(v2)),
    ))));
}

unsafe fn call_pf_reduce(e: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let root: *mut libc::c_char = ast_expr_as_identifier(ast_expr_call_root(&*e));
    let nargs: libc::c_int = ast_expr_call_nargs(e);
    let unreduced_arg: *mut *mut AstExpr = ast_expr_call_args(e);
    let reduced_arg: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(nargs as usize))
            as *mut *mut AstExpr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nargs {
        let arg = ast_expr_pf_reduce(&**unreduced_arg.offset(i as isize), s)?;
        assert!(!arg.is_null());
        let ref mut fresh3 = *reduced_arg.offset(i as isize);
        *fresh3 = value_to_expr(arg);
        i += 1;
    }
    Ok(value_sync_create(Box::into_raw(ast_expr_call_create(
        ast_expr_identifier_create(dynamic_str(root)),
        nargs,
        reduced_arg,
    ))))
}

unsafe fn structmember_pf_reduce(expr: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let v = ast_expr_pf_reduce(ast_expr_member_root(expr), s)?;
    assert!(!v.is_null());
    let field: *mut libc::c_char = ast_expr_member_field(expr);
    if value_isstruct(&*v) {
        let obj: *mut Object = value_struct_member(v, field);
        let obj_value: *mut Value = object_as_value(obj);
        if obj_value.is_null() {
            panic!();
        }
        return Ok(value_copy(&*obj_value));
    }
    if !value_issync(&*v) {
        panic!();
    }
    return Ok(value_sync_create(Box::into_raw(ast_expr_member_create(
        Box::from_raw(value_as_sync(v)),
        dynamic_str(field),
    ))));
}

pub unsafe fn ast_expr_pf_reduce(e: &AstExpr, s: *mut State) -> Result<*mut Value> {
    match &e.kind {
        AstExprKind::Constant(_) | AstExprKind::StringLiteral(_) | AstExprKind::Identifier(_) => {
            ast_expr_eval(e, s)
        }
        AstExprKind::Unary(_) => unary_pf_reduce(e, s),
        AstExprKind::Binary(binary) => binary_pf_reduce(&binary.e1, binary.op, &binary.e2, s),
        AstExprKind::Call(_) => call_pf_reduce(e, s),
        AstExprKind::StructMember(_) => structmember_pf_reduce(e, s),
        AstExprKind::Bracketed(inner) => ast_expr_pf_reduce(inner, s),
        _ => panic!(),
    }
}

unsafe fn ast_expr_member_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::StructMember(member) = &expr.kind else {
        panic!()
    };
    let root = &member.root;
    if matches!(root.kind, AstExprKind::Unary(_)) {
        return ast_expr_member_deref_str_build(root, member.field, b);
    }
    let r: *mut libc::c_char = ast_expr_str(root);
    strbuilder_printf(
        b,
        b"%s.%s\0" as *const u8 as *const libc::c_char,
        r,
        member.field,
    );
    free(r as *mut libc::c_void);
}

unsafe fn ast_expr_member_deref_str_build(
    root: &AstExpr,
    member: *mut libc::c_char,
    b: *mut StrBuilder,
) {
    if !(ast_expr_unary_op(root) == AstUnaryOp::Dereference) {
        panic!();
    }
    let inner = ast_expr_unary_operand(root);
    let e1 = ast_expr_binary_e1(inner);
    let e2 = ast_expr_binary_e2(inner);
    let left: *mut libc::c_char = ast_expr_str(e1);
    if matches!((*e2).kind, AstExprKind::Constant(_))
        && ast_expr_as_constant(&*e2) == 0 as libc::c_int
    {
        strbuilder_printf(
            b,
            b"%s->%s\0" as *const u8 as *const libc::c_char,
            left,
            member,
        );
    } else {
        let index: *mut libc::c_char = ast_expr_str(e2);
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

unsafe fn ast_expr_incdec_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::IncDec(incdec) = &expr.kind else {
        panic!()
    };
    let root: *mut libc::c_char = ast_expr_str(&incdec.operand);
    let op: *mut libc::c_char = (if incdec.inc != 0 {
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

unsafe fn ast_expr_call_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    let fun: *mut libc::c_char = ast_expr_str(&call.fun);
    strbuilder_printf(b, b"%s(\0" as *const u8 as *const libc::c_char, fun);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        let arg: *mut libc::c_char = ast_expr_str(&**call.arg.offset(i as isize));
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
    free(fun as *mut libc::c_void);
}

impl Drop for CallExpr {
    fn drop(&mut self) {
        unsafe {
            let mut i: libc::c_int = 0 as libc::c_int;
            while i < self.n {
                ast_expr_destroy(*self.arg.offset(i as isize));
                i += 1;
            }
            free(self.arg as *mut libc::c_void);
        }
    }
}

pub unsafe fn ast_expr_inverted_copy(expr: &AstExpr, invert: bool) -> Box<AstExpr> {
    let copy = ast_expr_copy(expr);
    if invert {
        ast_expr_unary_create(copy, AstUnaryOp::Bang)
    } else {
        copy
    }
}

pub unsafe fn ast_expr_member_field(expr: &AstExpr) -> *mut libc::c_char {
    let AstExprKind::StructMember(member) = &expr.kind else {
        panic!()
    };
    member.field
}

pub unsafe fn ast_expr_member_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::StructMember(member) = &expr.kind else {
        panic!()
    };
    &member.root
}

pub unsafe fn ast_expr_incdec_pre(expr: &AstExpr) -> bool {
    match &expr.kind {
        AstExprKind::IncDec(incdec) => incdec.pre != 0,
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_incdec_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IncDec(incdec) = &expr.kind else {
        panic!()
    };
    &incdec.operand
}

unsafe fn ast_expr_copy_call(expr: &AstExpr) -> Box<AstExpr> {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    let arg: *mut *mut AstExpr =
        malloc((::core::mem::size_of::<*mut AstExpr>()).wrapping_mul(call.n as usize))
            as *mut *mut AstExpr;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        let ref mut fresh4 = *arg.offset(i as isize);
        *fresh4 = Box::into_raw(ast_expr_copy(&**(call.arg).offset(i as isize)));
        i += 1;
    }
    ast_expr_call_create(ast_expr_copy(&call.fun), call.n, arg)
}

pub unsafe fn ast_expr_member_create(
    struct_: Box<AstExpr>,
    field: *mut libc::c_char,
) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::StructMember(StructMemberExpr {
        root: struct_,
        field,
    }))
}

pub unsafe fn ast_expr_binary_create(
    e1: Box<AstExpr>,
    op: AstBinaryOp,
    e2: Box<AstExpr>,
) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Binary(BinaryExpr { e1, op, e2 }))
}

pub unsafe fn ast_expr_unary_create(root: Box<AstExpr>, op: AstUnaryOp) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Unary(UnaryExpr { op, arg: root }))
}

pub unsafe fn ast_expr_incdec_to_assignment(expr: &AstExpr) -> Box<AstExpr> {
    let AstExprKind::IncDec(incdec) = &expr.kind else {
        panic!()
    };

    ast_expr_assignment_create(
        ast_expr_copy(&incdec.operand),
        ast_expr_binary_create(
            ast_expr_copy(&incdec.operand),
            if incdec.inc != 0 {
                AstBinaryOp::Addition
            } else {
                AstBinaryOp::Subtraction
            },
            ast_expr_constant_create(1 as libc::c_int),
        ),
    )
}

pub unsafe fn ast_expr_incdec_create(root: Box<AstExpr>, inc: bool, pre: bool) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::IncDec(IncDecExpr {
        operand: root,
        inc: inc as libc::c_int,
        pre: pre as libc::c_int,
    }))
}

pub unsafe fn ast_expr_call_args(expr: &AstExpr) -> *mut *mut AstExpr {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    call.arg
}

pub unsafe fn ast_expr_call_nargs(expr: &AstExpr) -> libc::c_int {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    call.n
}

pub unsafe fn ast_expr_call_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    &call.fun
}

pub unsafe fn ast_expr_call_create(
    root: Box<AstExpr>,
    narg: libc::c_int,
    arg: *mut *mut AstExpr,
) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Call(CallExpr {
        fun: root,
        n: narg,
        arg,
    }))
}

pub unsafe fn ast_expr_iteration_create() -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Iteration)
}

pub unsafe fn ast_expr_alloc_kind(expr: &AstExpr) -> AstAllocKind {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    alloc.kind
}

pub unsafe fn ast_expr_alloc_arg(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    &alloc.arg
}

pub unsafe fn ast_expr_isisdereferencable(expr: &AstExpr) -> bool {
    return matches!(expr.kind, AstExprKind::IsDereferencable(_));
}

pub unsafe fn ast_expr_dealloc_create(arg: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Dealloc,
        arg,
    }))
}

pub unsafe fn ast_expr_str(expr: &AstExpr) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match &expr.kind {
        AstExprKind::Identifier(id) => {
            strbuilder_printf(b, *id);
        }
        AstExprKind::Constant(_) => {
            ast_expr_constant_str_build(expr, b);
        }
        AstExprKind::StringLiteral(s) => {
            strbuilder_printf(b, b"\"%s\"\0" as *const u8 as *const libc::c_char, *s);
        }
        AstExprKind::Bracketed(inner) => {
            ast_expr_bracketed_str_build(inner, b);
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
        AstExprKind::IsDeallocand(assertand) => {
            ast_expr_isdeallocand_str_build(assertand, b);
        }
        AstExprKind::IsDereferencable(assertand) => {
            ast_expr_isdereferencable_str_build(assertand, b);
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

unsafe fn ast_expr_alloc_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    let arg: *mut libc::c_char = ast_expr_str(&*alloc.arg);
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

pub unsafe fn ast_expr_alloc_create(arg: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Alloc,
        arg,
    }))
}

pub unsafe fn ast_expr_isdereferencable_assertand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IsDereferencable(assertand) = &expr.kind else {
        panic!()
    };
    assertand
}

pub unsafe fn ast_expr_isdeallocand_assertand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IsDeallocand(assertand) = &expr.kind else {
        panic!()
    };
    assertand
}

unsafe fn ast_expr_alloc_copy(expr: &AstExpr) -> Box<AstExpr> {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    let arg = ast_expr_copy(&*alloc.arg);
    match alloc.kind {
        AstAllocKind::Alloc => ast_expr_alloc_create(arg),
        AstAllocKind::Dealloc => ast_expr_dealloc_create(arg),
        AstAllocKind::Clump => ast_expr_clump_create(arg),
    }
}

unsafe fn ast_expr_isdereferencable_str_build(assertand: &AstExpr, b: *mut StrBuilder) {
    let root: *mut libc::c_char = ast_expr_str(assertand);
    strbuilder_printf(b, b"$%s\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

pub unsafe fn ast_expr_assignment_rval(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!()
    };
    &assignment.rval
}

pub unsafe fn ast_expr_copy(expr: &AstExpr) -> Box<AstExpr> {
    match &expr.kind {
        AstExprKind::Identifier(id) => ast_expr_identifier_create(dynamic_str(*id)),
        AstExprKind::Constant(c) => {
            if c.ischar {
                ast_expr_constant_create_char(c.constant as libc::c_char)
            } else {
                ast_expr_constant_create(c.constant)
            }
        }
        AstExprKind::StringLiteral(s) => ast_expr_literal_create(dynamic_str(*s)),
        AstExprKind::Bracketed(inner) => ast_expr_bracketed_create(ast_expr_copy(inner)),
        AstExprKind::Call(_) => ast_expr_copy_call(expr),
        AstExprKind::IncDec(incdec) => ast_expr_incdec_create(
            ast_expr_copy(&incdec.operand),
            incdec.inc != 0,
            incdec.pre != 0,
        ),
        AstExprKind::StructMember(member) => {
            ast_expr_member_create(ast_expr_copy(&*member.root), dynamic_str(member.field))
        }
        AstExprKind::Unary(unary) => ast_expr_unary_create(ast_expr_copy(&unary.arg), unary.op),
        AstExprKind::Binary(binary) => ast_expr_binary_create(
            ast_expr_copy(&binary.e1),
            binary.op,
            ast_expr_copy(&binary.e2),
        ),
        AstExprKind::Assignment(assignment) => ast_expr_assignment_create(
            ast_expr_copy(&assignment.lval),
            ast_expr_copy(&assignment.rval),
        ),
        AstExprKind::IsDeallocand(assertand) => {
            ast_expr_isdeallocand_create(ast_expr_copy(assertand))
        }
        AstExprKind::IsDereferencable(assertand) => {
            ast_expr_isdereferencable_create(ast_expr_copy(assertand))
        }
        AstExprKind::ArbArg => ast_expr_arbarg_create(),
        AstExprKind::Allocation(_) => ast_expr_alloc_copy(expr),
        _ => panic!(),
    }
}

pub unsafe fn ast_expr_assignment_lval(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!()
    };
    &assignment.lval
}

pub unsafe fn ast_expr_binary_e2(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    &binary.e2
}

unsafe fn ast_expr_isdeallocand_str_build(assertand: &AstExpr, b: *mut StrBuilder) {
    let root: *mut libc::c_char = ast_expr_str(assertand);
    strbuilder_printf(b, b"@%s\0" as *const u8 as *const libc::c_char, root);
    free(root as *mut libc::c_void);
}

unsafe fn ast_expr_assignment_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!()
    };
    let root: *mut libc::c_char = ast_expr_str(&assignment.lval);
    let rval: *mut libc::c_char = ast_expr_str(&assignment.rval);
    strbuilder_printf(
        b,
        b"%s = %s\0" as *const u8 as *const libc::c_char,
        root,
        rval,
    );
    free(rval as *mut libc::c_void);
    free(root as *mut libc::c_void);
}

unsafe fn ast_expr_binary_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::Binary(binary) = &expr.kind else {
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
    let e1: *mut libc::c_char = ast_expr_str(&binary.e1);
    let e2: *mut libc::c_char = ast_expr_str(&binary.e2);
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

pub unsafe fn ast_expr_binary_e1(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    &binary.e1
}

pub unsafe fn ast_expr_difference_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Subtraction, e2);
}

pub unsafe fn ast_expr_sum_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Addition, e2);
}

unsafe fn ast_expr_unary_str_build(expr: &AstExpr, b: *mut StrBuilder) {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!()
    };
    let c = match unary.op {
        AstUnaryOp::Address => b'&',
        AstUnaryOp::Dereference => b'*',
        AstUnaryOp::Positive => b'+',
        AstUnaryOp::Negative => b'-',
        AstUnaryOp::OnesComplement => b'~',
        AstUnaryOp::Bang => b'!',
    } as libc::c_char;
    let root: *mut libc::c_char = ast_expr_str(&unary.arg);
    strbuilder_printf(
        b,
        b"%c(%s)\0" as *const u8 as *const libc::c_char,
        c as libc::c_int,
        root,
    );
    free(root as *mut libc::c_void);
}

pub unsafe fn ast_expr_ge_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Ge, e2);
}

pub unsafe fn ast_expr_binary_op(expr: &AstExpr) -> AstBinaryOp {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    binary.op
}

impl Drop for AstExprKind {
    fn drop(&mut self) {
        unsafe {
            match self {
                AstExprKind::Identifier(id) => {
                    free(*id as *mut libc::c_void);
                }
                AstExprKind::StringLiteral(s) => {
                    free(*s as *mut libc::c_void);
                }
                AstExprKind::Bracketed(_) => {}
                AstExprKind::Call(_) => {}
                AstExprKind::IncDec(_) => {}
                AstExprKind::StructMember(_) => {}
                AstExprKind::Unary(_) => {}
                AstExprKind::Binary(_) => {}
                AstExprKind::Assignment(_) => {}
                AstExprKind::IsDeallocand(_) => {}
                AstExprKind::IsDereferencable(assertand) => {}
                AstExprKind::Constant(_) => {}
                AstExprKind::ArbArg => {}
                AstExprKind::Allocation(_) => {}
                _ => {
                    panic!();
                }
            }
        }
    }
}

pub unsafe fn ast_expr_destroy(expr: *mut AstExpr) {
    drop(Box::from_raw(expr));
}

pub unsafe fn ast_expr_le_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Le, e2);
}

pub unsafe fn ast_expr_clump_create(arg: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Clump,
        arg,
    }))
}

pub unsafe fn ast_expr_gt_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Gt, e2);
}

pub unsafe fn ast_expr_lt_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Lt, e2);
}

pub unsafe fn ast_expr_ne_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Ne, e2);
}

pub unsafe fn ast_expr_eq_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    return ast_expr_binary_create(e1, AstBinaryOp::Eq, e2);
}

pub unsafe fn ast_expr_unary_operand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!()
    };
    &unary.arg
}

pub unsafe fn ast_expr_unary_isdereference(expr: &AstExpr) -> bool {
    assert!(matches!(&expr.kind, AstExprKind::Unary(_)));
    return ast_expr_unary_op(expr) == AstUnaryOp::Dereference;
}

pub unsafe fn ast_expr_unary_op(expr: &AstExpr) -> AstUnaryOp {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!()
    };
    unary.op
}

pub unsafe fn ast_expr_getfuncs(expr: &AstExpr) -> Box<StringArr> {
    match &expr.kind {
        AstExprKind::Identifier(_)
        | AstExprKind::Constant(_)
        | AstExprKind::StringLiteral(_)
        | AstExprKind::StructMember(_)
        | AstExprKind::IsDeallocand(_)
        | AstExprKind::IsDereferencable(_)
        | AstExprKind::ArbArg => return string_arr_create(),
        AstExprKind::Call(_) => ast_expr_call_getfuncs(expr),
        AstExprKind::Bracketed(inner) => ast_expr_getfuncs(inner),
        AstExprKind::IncDec(incdec) => ast_expr_getfuncs(&incdec.operand),
        AstExprKind::Unary(unary) => ast_expr_getfuncs(&unary.arg),
        AstExprKind::Assignment(assignment) => string_arr_concat(
            &ast_expr_getfuncs(&assignment.lval),
            &ast_expr_getfuncs(&assignment.rval),
        ),
        AstExprKind::Binary(binary) => string_arr_concat(
            &ast_expr_getfuncs(&binary.e1),
            &ast_expr_getfuncs(&binary.e2),
        ),
        _ => panic!("invalid expr kind"),
    }
}

pub unsafe fn ast_expr_splits(e: &AstExpr, s: *mut State) -> AstStmtSplits {
    match &(*e).kind {
        AstExprKind::Call(_) => call_splits(e, s),
        AstExprKind::Assignment(assignment) => ast_expr_splits(&assignment.rval, s),
        AstExprKind::Unary(_) => ast_expr_splits(ast_expr_unary_operand(e), s),
        AstExprKind::Binary(_) => binary_splits(e, s),
        AstExprKind::IncDec(_) => ast_expr_splits(ast_expr_incdec_root(e), s),
        AstExprKind::StructMember(_) => ast_expr_splits(ast_expr_member_root(e), s),
        AstExprKind::Constant(_)
        | AstExprKind::Identifier(_)
        | AstExprKind::StringLiteral(_)
        | AstExprKind::ArbArg
        | AstExprKind::IsDereferencable(_)
        | AstExprKind::Allocation(_) => AstStmtSplits {
            n: 0,
            cond: ptr::null_mut(),
            err: None,
        },
        _ => panic!(),
    }
}

unsafe fn call_splits(expr: &AstExpr, state: *mut State) -> AstStmtSplits {
    let root = ast_expr_call_root(expr);
    let name: *mut libc::c_char = ast_expr_as_identifier(root);
    let f: *mut AstFunction = externals_getfunc(state_getext(state), name);
    if f.is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"function: `%s' not found\0" as *const u8 as *const libc::c_char,
            name,
        );
        return AstStmtSplits {
            n: 0,
            cond: ptr::null_mut(),
            err: Some(error_create(strbuilder_build(b))),
        };
    }
    let params = ast_function_params(&*f);
    let s_copy: *mut State = state_copy(state);
    let args = prepare_arguments(
        ast_expr_call_nargs(expr),
        ast_expr_call_args(expr),
        params,
        s_copy,
    );
    let ret_type: *mut AstType = ast_function_type(&*f);
    state_pushframe(s_copy, dynamic_str(name), ret_type);
    if let Err(err) = prepare_parameters(params, args, name, s_copy) {
        return AstStmtSplits {
            n: 0,
            cond: ptr::null_mut(),
            err: Some(err),
        };
    }
    let mut n: libc::c_int = 0 as libc::c_int;
    let mut cond: *mut *mut AstExpr = ptr::null_mut();
    let abs = ast_function_abstract(&*f);
    let ndecls: libc::c_int = ast_block_ndecls(abs);
    if ndecls != 0 {
        let var: *mut *mut AstVariable = ast_block_decls(abs);
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < ndecls {
            state_declare(s_copy, *var.offset(i as isize), false);
            i += 1;
        }
    }
    let nstmts: libc::c_int = ast_block_nstmts(abs);
    let stmt: *mut *mut AstStmt = ast_block_stmts(abs);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nstmts {
        let splits: AstStmtSplits = ast_stmt_splits(&**stmt.offset(i_0 as isize), s_copy);
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
    AstStmtSplits { n, cond, err: None }
}

unsafe fn binary_splits(e: &AstExpr, s: *mut State) -> AstStmtSplits {
    let s1: AstStmtSplits = ast_expr_splits(ast_expr_binary_e1(e), s);
    let s2: AstStmtSplits = ast_expr_splits(ast_expr_binary_e2(e), s);
    let n: libc::c_int = s1.n + s2.n;
    let cond: *mut *mut AstExpr =
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
    AstStmtSplits { n, cond, err: None }
}

unsafe fn ast_expr_call_getfuncs(expr: &AstExpr) -> Box<StringArr> {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    let mut res = string_arr_create();
    let AstExprKind::Identifier(id) = &call.fun.kind else {
        panic!()
    };
    string_arr_append(&mut res, dynamic_str(*id));
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < call.n {
        res = string_arr_concat(&res, &ast_expr_getfuncs(&**(call.arg).offset(i as isize)));
        i += 1;
    }
    res
}

unsafe fn calculate_indegrees(g: &Map) -> Box<Map> {
    let mut indegrees = Map::new();
    for key in g.keys() {
        let deps: *mut StringArr = g.get(key) as *mut StringArr;
        if (indegrees.get(key)).is_null() {
            indegrees.set(
                dynamic_str(key),
                dynamic_int(0 as libc::c_int) as *const libc::c_void,
            );
            let mut j: libc::c_int = 0 as libc::c_int;
            while j < (*deps).n {
                let dep_key: *mut libc::c_char = *((*deps).s).offset(j as isize);
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
        let n_arr: *mut StringArr = g.get(key) as *mut StringArr;
        if !n_arr.is_null() {
            let mut j_0: libc::c_int = 0 as libc::c_int;
            while j_0 < (*n_arr).n {
                let count: *mut libc::c_int = indegrees.get(key) as *mut libc::c_int;
                *count = *count + 1 as libc::c_int;
                j_0 += 1;
            }
        }
    }
    return indegrees;
}

unsafe fn dynamic_int(i: libc::c_int) -> *mut libc::c_int {
    let val: *mut libc::c_int = malloc(::core::mem::size_of::<libc::c_int>()) as *mut libc::c_int;
    *val = i;
    return val;
}

unsafe fn build_indegree_zero(indegrees: &Map) -> Box<StringArr> {
    let mut indegree_zero = string_arr_create();
    for key in indegrees.keys() {
        let val: *mut libc::c_int = indegrees.get(key) as *mut libc::c_int;
        if *val == 0 as libc::c_int {
            string_arr_append(&mut indegree_zero, dynamic_str(key));
        }
    }
    indegree_zero
}

pub unsafe fn topological_order(fname: *mut libc::c_char, ext: *mut Externals) -> Box<StringArr> {
    let mut order = string_arr_create();
    let g = ast_function_buildgraph(fname, ext);
    let indegrees = calculate_indegrees(&g);
    let mut indegree_zero = build_indegree_zero(&indegrees);
    while (*indegree_zero).n > 0 as libc::c_int {
        let curr: *mut libc::c_char = string_arr_deque(&mut indegree_zero);
        string_arr_append(&mut order, curr);
        for key in (*g).keys() {
            let v: *mut StringArr = g.get(key) as *mut StringArr;
            if string_arr_contains(&*v, curr) {
                let count: *mut libc::c_int = indegrees.get(key) as *mut libc::c_int;
                *count = *count - 1 as libc::c_int;
                if *count == 0 as libc::c_int {
                    string_arr_append(&mut indegree_zero, dynamic_str(key));
                }
            }
        }
    }
    if order.n != indegrees.len() {
        eprintln!("cycle detected in graph");
        process::exit(1);
    }
    order
}

pub unsafe fn ast_block_create(
    decls: Vec<*mut AstVariable>,
    stmts: Vec<*mut AstStmt>,
) -> *mut AstBlock {
    Box::into_raw(Box::new(AstBlock { decls, stmts }))
}

impl Drop for AstBlock {
    fn drop(&mut self) {
        unsafe {
            for &decl in &self.decls {
                ast_variable_destroy(decl);
            }
            for &stmt in &self.stmts {
                ast_stmt_destroy(stmt);
            }
        }
    }
}

pub unsafe fn ast_block_destroy(b: *mut AstBlock) {
    drop(Box::from_raw(b));
}

pub unsafe fn ast_block_copy(b: &AstBlock) -> *mut AstBlock {
    return ast_block_create(
        b.decls
            .iter()
            .copied()
            .map(|var_ptr| unsafe { ast_variable_copy(var_ptr) })
            .collect(),
        b.stmts
            .iter()
            .copied()
            .map(|stmt| ast_stmt_copy(&*stmt))
            .collect(),
    );
}

pub unsafe fn ast_block_str(b: &AstBlock, indent: *mut libc::c_char) -> *mut libc::c_char {
    let sb: *mut StrBuilder = strbuilder_create();
    for &decl in &b.decls {
        let s: *mut libc::c_char = ast_variable_str(decl);
        strbuilder_printf(
            sb,
            b"%s%s;\n\0" as *const u8 as *const libc::c_char,
            indent,
            s,
        );
        free(s as *mut libc::c_void);
    }
    for &stmt in &b.stmts {
        let s: *mut libc::c_char = ast_stmt_str(&*stmt);
        strbuilder_printf(
            sb,
            b"%s%s\n\0" as *const u8 as *const libc::c_char,
            indent,
            s,
        );
        free(s as *mut libc::c_void);
    }
    return strbuilder_build(sb);
}

pub unsafe fn ast_block_ndecls(b: &AstBlock) -> libc::c_int {
    b.decls.len() as libc::c_int
}

pub unsafe fn ast_block_decls(b: &AstBlock) -> *mut *mut AstVariable {
    b.decls.as_slice().as_ptr() as *mut *mut AstVariable
}

pub unsafe fn ast_block_nstmts(b: &AstBlock) -> libc::c_int {
    b.stmts.len() as libc::c_int
}

pub unsafe fn ast_block_stmts(b: &AstBlock) -> *mut *mut AstStmt {
    b.stmts.as_slice().as_ptr() as *mut *mut AstStmt
}

pub unsafe fn ast_block_isterminal(b: &AstBlock, s: *mut State) -> bool {
    b.stmts.iter().any(|stmt| ast_stmt_isterminal(&**stmt, s))
}

pub unsafe fn ast_block_preconds(b: &AstBlock) -> PrecondsResult {
    let n: libc::c_int = ast_block_nstmts(b);
    let stmt: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if ast_stmt_ispre(&**stmt.offset(i as isize)) {
            let preconds = ast_stmt_labelled_stmt(&**stmt.offset(i as isize));
            if let Err(err) = ast_stmt_preconds_validate(preconds) {
                return PrecondsResult {
                    stmt: ptr::null_mut(),
                    err: Some(err),
                };
            }
            return PrecondsResult {
                stmt: preconds as *const AstStmt as *mut AstStmt, // TODO - figure out if this is safe
                err: None,
            };
        }
        i += 1;
    }
    PrecondsResult {
        stmt: 0 as *mut AstStmt,
        err: None,
    }
}

pub unsafe fn ast_stmt_process(
    stmt: &AstStmt,
    fname: *mut libc::c_char,
    state: *mut State,
) -> Result<()> {
    if matches!((*stmt).kind, AstStmtKind::CompoundV(_)) {
        if let Err(err) = ast_stmt_verify(stmt, state) {
            let b: *mut StrBuilder = strbuilder_create();
            let loc = ast_stmt_lexememarker(stmt);
            let m: *mut libc::c_char = lexememarker_str(loc);
            strbuilder_printf(
                b,
                b"%s: %s\0" as *const u8 as *const libc::c_char,
                m,
                err.msg,
            );
            free(m as *mut libc::c_void);
            return Err(error_create(strbuilder_build(b)));
        }
    }
    if let Err(err) = ast_stmt_exec(stmt, state) {
        let b: *mut StrBuilder = strbuilder_create();
        let loc = ast_stmt_lexememarker(stmt);
        let m: *mut libc::c_char = lexememarker_str(loc);
        strbuilder_printf(
            b,
            b"%s:%s: cannot exec statement: %s\0" as *const u8 as *const libc::c_char,
            m,
            fname,
            err.msg,
        );
        free(m as *mut libc::c_void);
        return Err(error_create(strbuilder_build(b)));
    }
    Ok(())
}

pub unsafe fn ast_stmt_preprocess(stmt: &AstStmt, state: *mut State) -> Result<Preresult> {
    if ast_stmt_isassume(stmt) {
        return stmt_installprop(stmt, state);
    }
    Ok(Preresult {
        is_contradiction: false,
    })
}

pub unsafe fn ast_stmt_labelled_stmt(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!()
    };
    &*labelled.stmt
}

unsafe fn labelled_absexec(
    stmt: &AstStmt,
    state: *mut State,
    should_setup: bool,
) -> Result<*mut Value> {
    if !ast_stmt_ispre(stmt) {
        let s: *mut libc::c_char = ast_stmt_str(stmt);
        let cstr = std::ffi::CStr::from_ptr(s);
        panic!("expected precondition, got: {cstr:?}");
    }
    let setup = ast_stmt_labelled_stmt(stmt);
    if !should_setup {
        return Ok(ptr::null_mut());
    }
    return ast_stmt_absexec(setup, state, should_setup);
}

pub unsafe fn ast_stmt_copy(stmt: &AstStmt) -> *mut AstStmt {
    let loc: *mut LexemeMarker = if !((*stmt).loc).is_null() {
        lexememarker_copy((*stmt).loc)
    } else {
        0 as *mut LexemeMarker
    };
    match &(*stmt).kind {
        AstStmtKind::Labelled(labelled) => ast_stmt_create_labelled(
            loc,
            dynamic_str(labelled.label),
            ast_stmt_copy(&*labelled.stmt),
        ),

        AstStmtKind::Nop => ast_stmt_create_nop(loc),
        AstStmtKind::Expr(expr) => ast_stmt_create_expr(loc, ast_expr_copy(expr)),
        AstStmtKind::Compound(compound) => {
            ast_stmt_create_compound(loc, ast_block_copy(&**compound))
        }
        AstStmtKind::CompoundV(compound) => {
            ast_stmt_create_compound_v(loc, ast_block_copy(&**compound))
        }
        AstStmtKind::Selection(selection) => ast_stmt_create_sel(
            loc,
            selection.isswitch,
            ast_expr_copy(&selection.cond),
            ast_stmt_copy(&*selection.body),
            if !(selection.nest).is_null() {
                ast_stmt_copy(&*selection.nest)
            } else {
                0 as *mut AstStmt
            },
        ),

        AstStmtKind::Iteration(iteration) => ast_stmt_copy_iter(loc, iteration, false),
        AstStmtKind::IterationE(iteration) => ast_stmt_copy_iter(loc, iteration, true),
        AstStmtKind::Jump(jump) => {
            ast_stmt_create_jump(loc, jump.kind, ast_expr_copy_ifnotnull(&jump.rv))
        }
        _ => panic!(),
    }
}

unsafe fn labelled_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let val = ast_stmt_absexec(stmt, state, true)?;
    // Note: original leaks this value
    // std::mem::forget(val);
    Ok(())
}

unsafe fn sel_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if let Some(err) = dec.err {
        return Err(err);
    }
    if dec.decision {
        return stmt_setupabsexec(ast_stmt_sel_body(stmt), state);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

unsafe fn comp_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let nstmt: libc::c_int = ast_block_nstmts(b);
    let stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmt {
        if ast_stmt_ispre(&**stmts.offset(i as isize)) {
            stmt_setupabsexec(&**stmts.offset(i as isize), state)?;
            if ast_stmt_isterminal(&**stmts.offset(i as isize), state) {
                break;
            }
        }
        i += 1;
    }
    Ok(())
}

unsafe fn stmt_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    match &(*stmt).kind {
        AstStmtKind::Expr(_) | AstStmtKind::Allocation(_) | AstStmtKind::Jump(_) => Ok(()),
        AstStmtKind::Labelled(_) => labelled_setupabsexec(stmt, state),
        AstStmtKind::Selection(_) => sel_setupabsexec(stmt, state),
        AstStmtKind::Compound(_) => comp_setupabsexec(stmt, state),
        _ => panic!(),
    }
}

pub unsafe fn ast_stmt_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    match stmt.kind {
        AstStmtKind::Selection(_) => stmt_setupabsexec(stmt, state),
        _ => Ok(()),
    }
}

pub unsafe fn ast_stmt_isselection(stmt: &AstStmt) -> bool {
    matches!(stmt.kind, AstStmtKind::Selection(_))
}

pub unsafe fn ast_stmt_isassume(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    strcmp(
        labelled.label,
        b"assume\0" as *const u8 as *const libc::c_char,
    ) == 0
}

unsafe fn stmt_installprop(stmt: &AstStmt, state: *mut State) -> Result<Preresult> {
    ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state)
}

pub unsafe fn ast_stmt_ispre(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    strcmp(
        labelled.label,
        b"setup\0" as *const u8 as *const libc::c_char,
    ) == 0
}

unsafe fn stmt_v_block_verify(v_block_stmt: &AstStmt, state: *mut State) -> Result<()> {
    let b = ast_stmt_as_v_block(v_block_stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let nstmts: libc::c_int = ast_block_nstmts(b);
    let stmt: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        ast_stmt_verify(&**stmt.offset(i as isize), state)?;
        i += 1;
    }
    Ok(())
}

unsafe fn stmt_expr_verify(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let expr = ast_stmt_as_expr(stmt);
    if ast_expr_decide(expr, state) {
        Ok(())
    } else {
        Err(error_create(
            b"cannot verify statement\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ))
    }
}

unsafe fn iter_empty(stmt: &AstStmt, state: *mut State) -> bool {
    if let Err(err) = ast_stmt_exec(ast_stmt_iter_init(stmt), state) {
        panic!();
    }
    return !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state);
}

unsafe fn stmt_iter_verify(stmt: &AstStmt, state: *mut State) -> Result<()> {
    if iter_empty(stmt, state) {
        return Ok(());
    }
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!((*body).kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_ndecls(block), 0);
    assert_eq!(ast_block_nstmts(block), 1);
    let assertion = ast_stmt_as_expr(&**(ast_block_stmts(block)).offset(0 as libc::c_int as isize));
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    if !ast_expr_rangedecide(assertion, lw, up, state) {
        return Err(error_create(
            b"could not verify\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    Ok(())
}

pub unsafe fn ast_stmt_verify(stmt: &AstStmt, state: *mut State) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(()),
        AstStmtKind::CompoundV(_) => stmt_v_block_verify(stmt, state),
        AstStmtKind::Expr(_) => stmt_expr_verify(stmt, state),
        AstStmtKind::Iteration(_) => stmt_iter_verify(stmt, state),
        _ => panic!(),
    }
}

unsafe fn stmt_compound_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    if !(ast_block_ndecls(b) == 0 as libc::c_int) {
        panic!();
    }
    let nstmt: libc::c_int = ast_block_nstmts(b);
    let stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmt {
        ast_stmt_exec(&**stmts.offset(i as isize), state)?;
        if ast_stmt_isterminal(&**stmts.offset(i as isize), state) {
            break;
        }
        i += 1;
    }
    Ok(())
}

unsafe fn stmt_sel_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if let Some(err) = dec.err {
        return Err(err);
    }
    if dec.decision {
        return ast_stmt_exec(ast_stmt_sel_body(stmt), state);
    }
    if !(ast_stmt_sel_nest(stmt)).is_none() {
        panic!();
    }
    Ok(())
}

unsafe fn iter_neteffect(iter: &AstStmt) -> *mut AstStmt {
    let abs = ast_stmt_iter_abstract(iter);
    let nstmts: libc::c_int = ast_block_nstmts(abs);
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
        ast_block_create(vec![], vec![]),
        ast_stmt_create_compound(
            0 as *mut LexemeMarker,
            ast_block_copy(ast_stmt_iter_abstract(iter)),
        ),
        false,
    );
}

unsafe fn stmt_iter_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let neteffect: *mut AstStmt = iter_neteffect(stmt);
    if neteffect.is_null() {
        return Ok(());
    }
    ast_stmt_absexec(&*neteffect, state, true)?;
    ast_stmt_destroy(neteffect);
    Ok(())
}

unsafe fn stmt_jump_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    // Note: jump_rv may be null. Error in original.
    let rv_val = ast_expr_eval(
        ast_stmt_jump_rv(stmt).expect("unsupported: return without value"),
        state,
    )?;
    if !rv_val.is_null() {
        let obj: *mut Object = state_getresult(state);
        if obj.is_null() {
            panic!();
        }
        object_assign(obj, value_copy(&*rv_val));
    }
    Ok(())
}

pub unsafe fn ast_stmt_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(()),
        AstStmtKind::Labelled(labelled) => ast_stmt_exec(&*labelled.stmt, state),
        AstStmtKind::Compound(_) => stmt_compound_exec(stmt, state),
        AstStmtKind::CompoundV(_) => Ok(()),
        AstStmtKind::Expr(expr) => ast_expr_exec(expr, state),
        AstStmtKind::Selection(_) => stmt_sel_exec(stmt, state),
        AstStmtKind::Iteration(_) => stmt_iter_exec(stmt, state),
        AstStmtKind::Jump(_) => stmt_jump_exec(stmt, state),
        _ => {
            panic!();
        }
    }
}

unsafe fn ast_stmt_jump_sprint(jump: &AstJumpStmt, b: *mut StrBuilder) {
    // Note: jump.rv can be null. Error in the original.
    let rv: *mut libc::c_char = ast_expr_str(jump.rv.as_ref().unwrap());
    strbuilder_printf(b, b"return %s;\n\0" as *const u8 as *const libc::c_char, rv);
    free(rv as *mut libc::c_void);
}

pub unsafe fn ast_stmt_iter_abstract(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &*iteration.r#abstract
}

pub unsafe fn ast_stmt_iter_iter(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &iteration.iter
}

pub unsafe fn ast_stmt_lexememarker(stmt: &AstStmt) -> &LexemeMarker {
    &*stmt.loc
}

unsafe fn ast_stmt_iter_sprint(iteration: &AstIterationStmt, b: *mut StrBuilder) {
    let init: *mut libc::c_char = ast_stmt_str(&*iteration.init);
    let cond: *mut libc::c_char = ast_stmt_str(&*iteration.cond);
    let body: *mut libc::c_char = ast_stmt_str(&*iteration.body);
    let iter: *mut libc::c_char = ast_expr_str(&iteration.iter);
    let abs: *mut libc::c_char = (if !(iteration.r#abstract).is_null() {
        ast_block_str(
            &*iteration.r#abstract,
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

pub unsafe fn ast_stmt_str(stmt: &AstStmt) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match &stmt.kind {
        AstStmtKind::Labelled(_) => {
            ast_stmt_labelled_sprint(stmt, b);
        }
        AstStmtKind::Nop => {
            ast_stmt_nop_sprint(stmt, b);
        }
        AstStmtKind::Expr(expr) => {
            ast_stmt_expr_sprint(expr, b);
        }
        AstStmtKind::Compound(compound) => {
            ast_stmt_compound_sprint(&**compound, b);
        }
        AstStmtKind::CompoundV(compound) => {
            ast_stmt_compound_sprint(&**compound, b);
        }
        AstStmtKind::Selection(_) => {
            ast_stmt_sel_sprint(stmt, b);
        }
        AstStmtKind::Iteration(iteration) => {
            ast_stmt_iter_sprint(iteration, b);
        }
        AstStmtKind::IterationE(iteration) => {
            ast_stmt_iter_sprint(iteration, b);
        }
        AstStmtKind::Jump(jump) => {
            ast_stmt_jump_sprint(jump, b);
        }
        _ => {
            panic!();
        }
    }
    return strbuilder_build(b);
}

unsafe fn ast_expr_copy_ifnotnull(expr: &Option<Box<AstExpr>>) -> Option<Box<AstExpr>> {
    expr.as_ref().map(|expr| ast_expr_copy(expr))
}

pub unsafe fn ast_stmt_iter_cond(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &(*stmt).kind else {
        panic!()
    };
    &*iteration.cond
}

pub unsafe fn ast_stmt_iter_init(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &*iteration.init
}

pub unsafe fn ast_stmt_labelled_label(stmt: &AstStmt) -> *mut libc::c_char {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!()
    };
    labelled.label
}

unsafe fn sel_isterminal(stmt: &AstStmt, s: *mut State) -> bool {
    let dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), s);
    if dec.err.is_some() {
        panic!();
    }
    if dec.decision {
        return ast_stmt_isterminal(ast_stmt_sel_body(stmt), s);
    }
    false
}

unsafe fn comp_absexec(
    stmt: &AstStmt,
    state: *mut State,
    should_setup: bool,
) -> Result<*mut Value> {
    let b = ast_stmt_as_block(stmt);
    let stmts: *mut *mut AstStmt = ast_block_stmts(b);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(b) {
        // Note: original leaks these values.
        let _ = ast_stmt_absexec(&**stmts.offset(i as isize), state, should_setup)?;
        i += 1;
    }
    return Ok(ptr::null_mut());
}

pub unsafe fn ast_stmt_as_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Compound(block) = &(*stmt).kind else {
        panic!();
    };
    &**block
}

pub unsafe fn ast_stmt_jump_rv(stmt: &AstStmt) -> Option<&AstExpr> {
    let AstStmtKind::Jump(jump) = &stmt.kind else {
        panic!()
    };
    jump.rv.as_ref().map(|rv| &**rv)
}

unsafe fn jump_absexec(stmt: &AstStmt, state: *mut State) -> Result<*mut Value> {
    // Note: Original leaks the expression to avoid a double free.
    let expr = ast_expr_assignment_create(
        ast_expr_identifier_create(
            b"return\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ),
        // Note: jump_rv can be null. Error in original.
        ast_expr_copy(ast_stmt_jump_rv(stmt).unwrap()),
    );
    let result = ast_expr_absexec(&expr, state);
    std::mem::forget(expr);
    result
}

pub unsafe fn ast_stmt_absexec(
    stmt: &AstStmt,
    state: *mut State,
    should_setup: bool,
) -> Result<*mut Value> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(ptr::null_mut()),
        AstStmtKind::Labelled(_) => labelled_absexec(stmt, state, should_setup),
        AstStmtKind::Expr(_) => ast_expr_absexec(ast_stmt_as_expr(stmt), state),
        AstStmtKind::Selection(_) => sel_absexec(stmt, state, should_setup),
        AstStmtKind::Iteration(_) => iter_absexec(stmt, state),
        AstStmtKind::Compound(_) => comp_absexec(stmt, state, should_setup),
        AstStmtKind::Jump(_) => jump_absexec(stmt, state),
        _ => panic!(),
    }
}

unsafe fn ast_stmt_sel_sprint(stmt: &AstStmt, b: *mut StrBuilder) {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    let cond: *mut libc::c_char = ast_expr_str(&selection.cond);
    let body: *mut libc::c_char = ast_stmt_str(&*selection.body);
    strbuilder_printf(
        b,
        b"if (%s) { %s }\0" as *const u8 as *const libc::c_char,
        cond,
        body,
    );
    let nest_stmt: *mut AstStmt = selection.nest;
    if !nest_stmt.is_null() {
        let nest: *mut libc::c_char = ast_stmt_str(&*nest_stmt);
        strbuilder_printf(b, b" else %s\0" as *const u8 as *const libc::c_char, nest);
        free(nest as *mut libc::c_void);
    }
    free(cond as *mut libc::c_void);
    free(body as *mut libc::c_void);
}

pub unsafe fn ast_stmt_isterminal(stmt: &AstStmt, s: *mut State) -> bool {
    match &(*stmt).kind {
        AstStmtKind::Jump(jump) => jump.kind == AstJumpKind::Return,
        AstStmtKind::Compound(block) => ast_block_isterminal(&**block, s),
        AstStmtKind::Selection(_) => sel_isterminal(stmt, s),
        _ => false,
    }
}

pub unsafe fn ast_stmt_create_jump(
    loc: *mut LexemeMarker,
    kind: AstJumpKind,
    rv: Option<Box<AstExpr>>,
) -> *mut AstStmt {
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Jump(AstJumpStmt {
        kind: AstJumpKind::Return,
        rv,
    });
    return stmt;
}

pub unsafe fn sel_decide(control: &AstExpr, state: *mut State) -> Decision {
    let v = match ast_expr_pf_reduce(control, state) {
        Ok(v) => v,
        Err(err) => {
            return Decision {
                decision: false,
                err: Some(err),
            }
        }
    };
    assert!(!v.is_null());
    if value_issync(&*v) {
        let sync: *mut AstExpr = value_as_sync(v);
        let p: *mut Props = state_getprops(state);
        if props_get(p, sync) {
            return {
                let init = Decision {
                    decision: true,
                    err: None,
                };
                init
            };
        } else if props_contradicts(p, &*sync) {
            return {
                let init = Decision {
                    decision: false,
                    err: None,
                };
                init
            };
        }
    }
    if value_isconstant(&*v) {
        if value_as_constant(&*v) != 0 {
            return {
                let init = Decision {
                    decision: true,
                    err: None,
                };
                init
            };
        }
        return {
            let init = Decision {
                decision: false,
                err: None,
            };
            init
        };
    }
    let zero: *mut Value = value_int_create(0 as libc::c_int);
    if !value_isint(&*v) {
        let b: *mut StrBuilder = strbuilder_create();
        let c_str: *mut libc::c_char = ast_expr_str(control);
        let v_str: *mut libc::c_char = value_str(v);
        strbuilder_printf(
            b,
            b"`%s' with value `%s' is undecidable\0" as *const u8 as *const libc::c_char,
            c_str,
            v_str,
        );
        free(v_str as *mut libc::c_void);
        free(c_str as *mut libc::c_void);
        return {
            let init = Decision {
                decision: false,
                err: Some(error_create(strbuilder_build(b))),
            };
            init
        };
    }
    let nonzero: bool = !value_equal(zero, v);
    value_destroy(zero);
    return {
        let init = Decision {
            decision: nonzero,
            err: None,
        };
        init
    };
}
unsafe fn ast_stmt_compound_sprint(compound: &AstBlock, b: *mut StrBuilder) {
    let s: *mut libc::c_char = ast_block_str(
        compound,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    strbuilder_printf(b, s);
    free(s as *mut libc::c_void);
}

unsafe fn ast_stmt_expr_sprint(expr: &AstExpr, b: *mut StrBuilder) {
    let s: *mut libc::c_char = ast_expr_str(expr);
    strbuilder_printf(b, b"%s;\0" as *const u8 as *const libc::c_char, s);
    free(s as *mut libc::c_void);
}

unsafe fn ast_stmt_create(loc: *mut LexemeMarker) -> *mut AstStmt {
    Box::into_raw(Box::new(AstStmt {
        kind: AstStmtKind::Nop,
        loc,
    }))
}

unsafe fn ast_stmt_copy_iter(
    loc: *mut LexemeMarker,
    iteration: &AstIterationStmt,
    as_iteration_e: bool,
) -> *mut AstStmt {
    let init = ast_stmt_copy(&*iteration.init);
    let cond = ast_stmt_copy(&*iteration.cond);
    let iter = ast_expr_copy(&iteration.iter);
    let abstract_0 = ast_block_copy(&*iteration.r#abstract);
    let body = ast_stmt_copy(&*iteration.body);

    ast_stmt_create_iter(loc, init, cond, iter, abstract_0, body, as_iteration_e)
}

pub unsafe fn ast_stmt_create_iter(
    loc: *mut LexemeMarker,
    init: *mut AstStmt,
    cond: *mut AstStmt,
    iter: Box<AstExpr>,
    abstract_0: *mut AstBlock,
    body: *mut AstStmt,
    as_iteration_e: bool,
) -> *mut AstStmt {
    assert!(!init.is_null() && !cond.is_null() && !abstract_0.is_null() && !body.is_null());
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    let iter = AstIterationStmt {
        init,
        cond,
        iter,
        body,
        r#abstract: abstract_0,
    };
    (*stmt).kind = if as_iteration_e {
        AstStmtKind::IterationE(iter)
    } else {
        AstStmtKind::Iteration(iter)
    };
    return stmt;
}

unsafe fn ast_stmt_nop_sprint(stmt: &AstStmt, b: *mut StrBuilder) {
    strbuilder_printf(b, b";\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn ast_stmt_iter_body(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &*iteration.body
}

pub unsafe fn ast_stmt_create_sel(
    loc: *mut LexemeMarker,
    isswitch: bool,
    cond: Box<AstExpr>,
    body: *mut AstStmt,
    nest: *mut AstStmt,
) -> *mut AstStmt {
    if isswitch {
        panic!();
    }
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Selection(AstSelectionStmt {
        isswitch,
        cond,
        body,
        nest,
    });
    return stmt;
}

pub unsafe fn ast_stmt_create_compound_v(loc: *mut LexemeMarker, b: *mut AstBlock) -> *mut AstStmt {
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::CompoundV(b);
    return stmt;
}
unsafe fn hack_alloc_from_neteffect(stmt: &AstStmt) -> &AstExpr {
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!((*body).kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_ndecls(block), 0);
    assert_eq!(ast_block_nstmts(block), 1);
    return ast_stmt_as_expr(&**(ast_block_stmts(block)).offset(0 as libc::c_int as isize));
}

pub unsafe fn ast_stmt_iter_lower_bound(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    let AstStmtKind::Expr(expr) = &(*iteration.init).kind else {
        panic!();
    };
    return ast_expr_assignment_rval(&**expr);
}

unsafe fn ast_stmt_labelled_sprint(stmt: &AstStmt, b: *mut StrBuilder) {
    let AstStmtKind::Labelled(labelled) = &(*stmt).kind else {
        panic!();
    };
    let s: *mut libc::c_char = ast_stmt_str(&*labelled.stmt);
    strbuilder_printf(
        b,
        b"%s: %s\0" as *const u8 as *const libc::c_char,
        labelled.label,
        s,
    );
    free(s as *mut libc::c_void);
}

unsafe fn sel_absexec(stmt: &AstStmt, state: *mut State, should_setup: bool) -> Result<*mut Value> {
    let dec: Decision = sel_decide(ast_stmt_sel_cond(stmt), state);
    if let Some(err) = dec.err {
        return Err(err);
    }
    if dec.decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    return Ok(0 as *mut Value);
}

pub unsafe fn ast_stmt_sel_nest(stmt: &AstStmt) -> Option<&AstStmt> {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    if selection.nest.is_null() {
        None
    } else {
        Some(&*selection.nest)
    }
}

pub unsafe fn ast_stmt_create_labelled(
    loc: *mut LexemeMarker,
    label: *mut libc::c_char,
    substmt: *mut AstStmt,
) -> *mut AstStmt {
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Labelled(AstLabelledStmt {
        label,
        stmt: substmt,
    });
    return stmt;
}

pub unsafe fn ast_stmt_create_nop(loc: *mut LexemeMarker) -> *mut AstStmt {
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Nop;
    return stmt;
}

pub unsafe fn ast_stmt_create_expr(loc: *mut LexemeMarker, expr: Box<AstExpr>) -> *mut AstStmt {
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Expr(expr);
    return stmt;
}

pub unsafe fn ast_stmt_create_compound(loc: *mut LexemeMarker, b: *mut AstBlock) -> *mut AstStmt {
    let stmt: *mut AstStmt = ast_stmt_create(loc);
    (*stmt).kind = AstStmtKind::Compound(b);
    return stmt;
}

pub unsafe fn ast_stmt_sel_cond(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    &selection.cond
}

pub unsafe fn ast_stmt_sel_body(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    &*selection.body
}

pub unsafe fn ast_stmt_iter_upper_bound(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    let AstStmtKind::Expr(expr) = &(*iteration.cond).kind else {
        panic!();
    };
    ast_expr_binary_e2(expr)
}

unsafe fn iter_absexec(stmt: &AstStmt, state: *mut State) -> Result<*mut Value> {
    let alloc = hack_alloc_from_neteffect(stmt);
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    ast_expr_alloc_rangeprocess(alloc, lw, up, state)?;
    Ok(ptr::null_mut())
}

pub unsafe fn ast_stmt_destroy(stmt: *mut AstStmt) {
    drop(Box::from_raw(stmt));
}

impl Drop for AstStmt {
    fn drop(&mut self) {
        unsafe {
            if !self.loc.is_null() {
                lexememarker_destroy(self.loc);
            }
        }
    }
}

impl Drop for AstStmtKind {
    fn drop(&mut self) {
        unsafe {
            match self {
                AstStmtKind::Labelled(labelled) => {
                    free(labelled.label as *mut libc::c_void);
                    ast_stmt_destroy(labelled.stmt);
                }
                AstStmtKind::Nop => {}
                AstStmtKind::Compound(block) | AstStmtKind::CompoundV(block) => {
                    ast_block_destroy(*block);
                }
                AstStmtKind::Selection(selection) => {
                    ast_stmt_destroy(selection.body);
                    if !(selection.nest).is_null() {
                        ast_stmt_destroy(selection.nest);
                    }
                }
                AstStmtKind::Iteration(iteration) | AstStmtKind::IterationE(iteration) => {
                    ast_stmt_destroy(iteration.init);
                    ast_stmt_destroy(iteration.cond);
                    ast_stmt_destroy(iteration.body);
                    ast_block_destroy(iteration.r#abstract);
                }
                AstStmtKind::Expr(_) => {}
                AstStmtKind::Jump(_) => {}
                _ => {
                    panic!();
                }
            }
        }
    }
}

pub unsafe fn ast_stmt_as_expr(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Expr(expr) = &stmt.kind else {
        panic!();
    };
    expr
}

pub unsafe fn ast_stmt_as_v_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::CompoundV(block) = &stmt.kind else {
        panic!();
    };
    &**block
}

pub unsafe fn ast_stmt_getfuncs(stmt: &AstStmt) -> Box<StringArr> {
    match &stmt.kind {
        AstStmtKind::Nop => string_arr_create(),
        AstStmtKind::Labelled(labelled) => ast_stmt_getfuncs(&*labelled.stmt),
        AstStmtKind::Compound(block) | AstStmtKind::CompoundV(block) => {
            ast_stmt_compound_getfuncs(&**block)
        }
        AstStmtKind::Expr(expr) => ast_expr_getfuncs(expr),
        AstStmtKind::Selection(selection) => ast_stmt_selection_getfuncs(selection),
        AstStmtKind::Iteration(iteration) | AstStmtKind::IterationE(iteration) => {
            ast_stmt_iteration_getfuncs(iteration)
        }
        // Note: jump.rv can be null. Error in original.
        AstStmtKind::Jump(jump) => ast_expr_getfuncs(jump.rv.as_ref().unwrap()),
        _ => panic!("invalid stmt kind"),
    }
}

pub unsafe fn ast_stmt_splits(stmt: &AstStmt, s: *mut State) -> AstStmtSplits {
    match &stmt.kind {
        AstStmtKind::Nop => {
            return {
                let init = AstStmtSplits {
                    n: 0,
                    cond: ptr::null_mut(),
                    err: None,
                };
                init
            };
        }
        AstStmtKind::Expr(expr) => return ast_expr_splits(expr, s),
        AstStmtKind::Selection(selection) => return stmt_sel_splits(selection, s),
        AstStmtKind::Jump(jump) => {
            if let Some(rv) = &jump.rv {
                return ast_expr_splits(rv, s);
            }
            AstStmtSplits {
                n: 0,
                cond: ptr::null_mut(),
                err: None,
            }
        }
        AstStmtKind::Labelled(labelled) => return ast_stmt_splits(&*labelled.stmt, s),
        AstStmtKind::Iteration(_) | AstStmtKind::Compound(_) | AstStmtKind::CompoundV(_) => {
            AstStmtSplits {
                n: 0,
                cond: ptr::null_mut(),
                err: None,
            }
        }
        _ => panic!(),
    }
}

unsafe fn stmt_sel_splits(selection: &AstSelectionStmt, s: *mut State) -> AstStmtSplits {
    let v = ast_expr_pf_reduce(&selection.cond, s).unwrap();
    let e: *mut AstExpr = value_to_expr(v);
    if condexists(&*e, s) as libc::c_int != 0 || value_isconstant(&*v) as libc::c_int != 0 {
        return {
            let init = AstStmtSplits {
                n: 0,
                cond: ptr::null_mut(),
                err: None,
            };
            init
        };
    }
    let cond: *mut *mut AstExpr =
        malloc(::core::mem::size_of::<*mut AstExpr>()) as *mut *mut AstExpr;
    let ref mut fresh10 = *cond.offset(0 as libc::c_int as isize);
    *fresh10 = e;
    AstStmtSplits {
        n: 1 as libc::c_int,
        cond,
        err: None,
    }
}

unsafe fn condexists(cond: &AstExpr, s: *mut State) -> bool {
    let val = ast_expr_pf_reduce(cond, s).unwrap();
    assert!(!val.is_null());
    // Note: original doesn't free this.
    let reduced: *mut AstExpr = value_to_expr(val);
    let p: *mut Props = state_getprops(s);
    return props_get(p, reduced) as libc::c_int != 0
        || props_contradicts(p, &*reduced) as libc::c_int != 0;
}

unsafe fn ast_stmt_selection_getfuncs(selection: &AstSelectionStmt) -> Box<StringArr> {
    let nest: *mut AstStmt = selection.nest;
    let cond_arr = ast_expr_getfuncs(&selection.cond);
    let body_arr = ast_stmt_getfuncs(&*selection.body);
    let nest_arr = if !nest.is_null() {
        ast_stmt_getfuncs(&*nest)
    } else {
        string_arr_create()
    };
    string_arr_concat(
        &string_arr_create(),
        &string_arr_concat(&cond_arr, &string_arr_concat(&body_arr, &nest_arr)),
    )
}

unsafe fn ast_stmt_iteration_getfuncs(iteration: &AstIterationStmt) -> Box<StringArr> {
    let init_arr = ast_stmt_getfuncs(&*iteration.init);
    let cond_arr = ast_stmt_getfuncs(&*iteration.cond);
    let body_arr = ast_stmt_getfuncs(&*iteration.body);
    let iter_arr = ast_expr_getfuncs(&iteration.iter);
    string_arr_concat(
        &string_arr_create(),
        &string_arr_concat(
            &string_arr_concat(&init_arr, &cond_arr),
            &string_arr_concat(&body_arr, &iter_arr),
        ),
    )
}

unsafe fn ast_stmt_compound_getfuncs(block: &AstBlock) -> Box<StringArr> {
    let mut res = string_arr_create();
    let stmts: *mut *mut AstStmt = ast_block_stmts(block);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(block) {
        res = string_arr_concat(&res, &ast_stmt_getfuncs(&**stmts.offset(i as isize)));
        i += 1;
    }
    res
}

pub unsafe fn ast_stmt_preconds_validate(stmt: &AstStmt) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Expr(_) | AstStmtKind::Allocation(_) | AstStmtKind::Iteration(_) => Ok(()),
        AstStmtKind::Selection(_) => preconds_selection_verify(stmt),
        AstStmtKind::Compound(block) => preconds_compound_verify(&**block),
        _ => panic!(),
    }
}
unsafe fn preconds_selection_verify(stmt: &AstStmt) -> Result<()> {
    let b: *mut StrBuilder = strbuilder_create();
    let l = ast_stmt_lexememarker(stmt);
    strbuilder_printf(
        b,
        b"%s setup preconditions must be decidable\0" as *const u8 as *const libc::c_char,
        lexememarker_str(l),
    );
    Err(error_create(strbuilder_build(b)))
}

unsafe fn preconds_compound_verify(block: &AstBlock) -> Result<()> {
    let stmts: *mut *mut AstStmt = ast_block_stmts(block);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ast_block_nstmts(block) {
        ast_stmt_preconds_validate(&**stmts.offset(i as isize))?;
        i += 1;
    }
    Ok(())
}

pub unsafe fn ast_type_isint(t: *mut AstType) -> bool {
    matches!((*t).base, AstTypeBase::Int)
}

pub unsafe fn ast_type_ispointer(t: *mut AstType) -> bool {
    matches!((*t).base, AstTypeBase::Pointer(_))
}

pub unsafe fn ast_type_create(base: AstTypeBase, modifiers: AstTypeModifier) -> *mut AstType {
    Box::into_raw(Box::new(AstType {
        base,
        modifiers: modifiers as libc::c_int,
    }))
}

pub unsafe fn ast_type_create_ptr(referent: *mut AstType) -> *mut AstType {
    assert!(!referent.is_null());
    ast_type_create(AstTypeBase::Pointer(referent), 0 as AstTypeModifier)
}

pub unsafe fn ast_type_create_voidptr() -> *mut AstType {
    ast_type_create(
        AstTypeBase::Pointer(0 as *mut AstType),
        0 as AstTypeModifier,
    )
}

pub unsafe fn ast_type_create_arr(base: *mut AstType, length: libc::c_int) -> *mut AstType {
    if base.is_null() {
        panic!();
    }
    ast_type_create(
        AstTypeBase::Array(AstArrayType {
            r#type: base,
            length,
        }),
        0 as AstTypeModifier,
    )
}

pub unsafe fn ast_type_create_struct(
    tag: *mut libc::c_char,
    members: Option<Box<Vec<*mut AstVariable>>>,
) -> *mut AstType {
    ast_type_create(
        AstTypeBase::Struct(AstStructType { tag, members }),
        0 as AstTypeModifier,
    )
}

pub unsafe fn ast_type_create_userdef(name: *mut libc::c_char) -> *mut AstType {
    ast_type_create(AstTypeBase::UserDefined(name), 0 as AstTypeModifier)
}

pub unsafe fn ast_type_vconst(
    t: *mut AstType,
    s: *mut State,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut Value {
    match &(*t).base {
        AstTypeBase::Int => value_int_indefinite_create(),
        AstTypeBase::Pointer(_) => value_ptr_indefinite_create(),
        AstTypeBase::UserDefined(name) => ast_type_vconst(
            externals_gettypedef(state_getext(s), *name),
            s,
            comment,
            persist,
        ),
        AstTypeBase::Struct(_) => value_struct_indefinite_create(t, s, comment, persist),
        _ => panic!(),
    }
}

pub unsafe fn ast_type_isstruct(t: &AstType) -> bool {
    matches!((*t).base, AstTypeBase::Struct(_))
}

pub unsafe fn ast_type_struct_complete(t: *mut AstType, ext: *mut Externals) -> *mut AstType {
    if !(ast_type_struct_members(&*t)).is_none() {
        return t;
    }
    let tag: *mut libc::c_char = ast_type_struct_tag(&*t);
    if tag.is_null() {
        panic!();
    }
    return externals_getstruct(ext, tag);
}

pub unsafe fn ast_type_struct_members(t: &AstType) -> Option<&[*mut AstVariable]> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!()
    };
    s.members.as_ref().map(|v| v.as_slice())
}

pub unsafe fn ast_type_struct_tag(t: &AstType) -> *mut libc::c_char {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!()
    };
    s.tag
}

pub unsafe fn ast_type_create_struct_anonym(members: Vec<*mut AstVariable>) -> *mut AstType {
    ast_type_create_struct(0 as *mut libc::c_char, Some(Box::new(members)))
}

pub unsafe fn ast_type_create_struct_partial(tag: *mut libc::c_char) -> *mut AstType {
    ast_type_create_struct(tag, None)
}

pub unsafe fn ast_type_copy_struct(old: *mut AstType) -> *mut AstType {
    let AstTypeBase::Struct(s) = &(*old).base else {
        panic!();
    };
    ast_type_create(
        AstTypeBase::Struct(AstStructType {
            tag: if !s.tag.is_null() {
                dynamic_str(s.tag)
            } else {
                0 as *mut libc::c_char
            },
            members: s.members.as_ref().map(|v| {
                Box::new(
                    v.iter()
                        .copied()
                        .map(|var_ptr| ast_variable_copy(var_ptr))
                        .collect(),
                )
            }),
        }),
        (*old).modifiers as libc::c_uint as AstTypeModifier,
    )
}

pub unsafe fn ast_type_mod_or(t: *mut AstType, m: AstTypeModifier) {
    (*t).modifiers = ((*t).modifiers as libc::c_uint | m as libc::c_uint) as libc::c_int;
}

pub unsafe fn ast_type_istypedef(t: *mut AstType) -> bool {
    ((*t).modifiers as libc::c_uint as AstTypeModifier) & MOD_TYPEDEF != 0
}

pub unsafe fn ast_type_destroy(t: *mut AstType) {
    match &(*t).base {
        AstTypeBase::Pointer(ptr_type) => {
            assert!(!ptr_type.is_null());
            ast_type_destroy(*ptr_type);
        }
        AstTypeBase::Array(arr) => {
            assert!(!arr.r#type.is_null());
            ast_type_destroy(arr.r#type);
        }
        _ => {}
    }
    drop(Box::from_raw(t));
}

pub unsafe fn ast_type_copy(t: *mut AstType) -> *mut AstType {
    if t.is_null() {
        panic!();
    }
    match &(*t).base {
        AstTypeBase::Pointer(ptr_type) => return ast_type_create_ptr(ast_type_copy(*ptr_type)),
        AstTypeBase::Array(arr) => {
            return ast_type_create_arr(ast_type_copy(arr.r#type), arr.length);
        }
        AstTypeBase::Struct(_) => return ast_type_copy_struct(t),
        AstTypeBase::UserDefined(name) => return ast_type_create_userdef(dynamic_str(*name)),
        AstTypeBase::Void => {
            return ast_type_create(AstTypeBase::Void, (*t).modifiers as AstTypeModifier)
        }
        AstTypeBase::Int => {
            return ast_type_create(AstTypeBase::Int, (*t).modifiers as AstTypeModifier)
        }
        AstTypeBase::Char => {
            return ast_type_create(AstTypeBase::Char, (*t).modifiers as AstTypeModifier)
        }
        _ => {
            panic!();
        }
    }
}

pub unsafe fn ast_type_str(t: *mut AstType) -> *mut libc::c_char {
    if t.is_null() {
        panic!();
    }
    let b: *mut StrBuilder = strbuilder_create();
    let mod_0: *mut libc::c_char = mod_str((*t).modifiers);
    strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, mod_0);
    free(mod_0 as *mut libc::c_void);
    match &(*t).base {
        AstTypeBase::Pointer(ptr_type) => {
            ast_type_str_build_ptr(b, *ptr_type);
        }
        AstTypeBase::Array(arr) => {
            ast_type_str_build_arr(b, arr);
        }
        AstTypeBase::Struct(s) => {
            ast_type_str_build_struct(b, s);
        }
        AstTypeBase::UserDefined(name) => {
            strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, *name);
        }
        AstTypeBase::Void => {
            strbuilder_printf(b, b"void\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Char => {
            strbuilder_printf(b, b"char\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Short => {
            strbuilder_printf(b, b"short\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Int => {
            strbuilder_printf(b, b"int\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Long => {
            strbuilder_printf(b, b"long\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Float => {
            strbuilder_printf(b, b"float\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Double => {
            strbuilder_printf(b, b"double\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Signed => {
            strbuilder_printf(b, b"signed\0" as *const u8 as *const libc::c_char);
        }
        AstTypeBase::Unsigned => {
            strbuilder_printf(b, b"unsigned\0" as *const u8 as *const libc::c_char);
        }
        _ => panic!(),
    }
    return strbuilder_build(b);
}

unsafe fn mod_str(mod_0: libc::c_int) -> *mut libc::c_char {
    let modstr: [*const libc::c_char; 7] = [
        b"typedef\0" as *const u8 as *const libc::c_char,
        b"extern\0" as *const u8 as *const libc::c_char,
        b"static\0" as *const u8 as *const libc::c_char,
        b"auto\0" as *const u8 as *const libc::c_char,
        b"register\0" as *const u8 as *const libc::c_char,
        b"const\0" as *const u8 as *const libc::c_char,
        b"volatile\0" as *const u8 as *const libc::c_char,
    ];
    let modlen: libc::c_int = 7 as libc::c_int;
    let b: *mut StrBuilder = strbuilder_create();
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
        let m: libc::c_int = (1 as libc::c_int) << i_0;
        if m & mod_0 != 0 {
            let fresh11 = nmods;
            nmods = nmods - 1;
            let space: *mut libc::c_char = (if fresh11 != 0 {
                b" \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            }) as *mut libc::c_char;
            strbuilder_printf(
                b,
                b"%s%s\0" as *const u8 as *const libc::c_char,
                modstr[i_0 as usize],
                space,
            );
        }
        i_0 += 1;
    }
    return strbuilder_build(b);
}

unsafe fn ast_type_str_build_ptr(b: *mut StrBuilder, ptr_type: *mut AstType) {
    let base: *mut libc::c_char = ast_type_str(ptr_type);
    let space: bool = !matches!((*ptr_type).base, AstTypeBase::Pointer(_));
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

unsafe fn ast_type_str_build_arr(b: *mut StrBuilder, arr: &AstArrayType) {
    let base: *mut libc::c_char = ast_type_str(arr.r#type);
    strbuilder_printf(
        b,
        b"%s[%d]\0" as *const u8 as *const libc::c_char,
        base,
        arr.length,
    );
    free(base as *mut libc::c_void);
}

unsafe fn ast_type_str_build_struct(b: *mut StrBuilder, s: &AstStructType) {
    let tag: *mut libc::c_char = s.tag;
    assert!(!tag.is_null() || !s.members.is_none());
    strbuilder_printf(b, b"struct \0" as *const u8 as *const libc::c_char);
    if !tag.is_null() {
        strbuilder_printf(b, tag);
    }
    let Some(members) = s.members.as_ref() else {
        return;
    };
    strbuilder_printf(b, b" { \0" as *const u8 as *const libc::c_char);
    for &field in members.iter() {
        let s: *mut libc::c_char = ast_variable_str(field);
        strbuilder_printf(b, b"%s; \0" as *const u8 as *const libc::c_char, s);
        free(s as *mut libc::c_void);
    }
    strbuilder_printf(b, b"}\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn ast_type_ptr_type(t: *mut AstType) -> *mut AstType {
    let AstTypeBase::Pointer(ptr_type) = &(*t).base else {
        panic!()
    };
    *ptr_type
}

pub unsafe fn ast_variable_create(
    name: *mut libc::c_char,
    type_0: *mut AstType,
) -> *mut AstVariable {
    Box::into_raw(Box::new(AstVariable {
        name,
        r#type: type_0,
    }))
}

pub unsafe fn ast_variable_destroy(v: *mut AstVariable) {
    ast_type_destroy((*v).r#type);
    free((*v).name as *mut libc::c_void);
    drop(Box::from_raw(v));
}

pub unsafe fn ast_variable_copy(v: *mut AstVariable) -> *mut AstVariable {
    if v.is_null() {
        panic!();
    }
    return ast_variable_create(dynamic_str((*v).name), ast_type_copy((*v).r#type));
}

pub unsafe fn ast_variables_copy(v: &[*mut AstVariable]) -> Vec<*mut AstVariable> {
    v.iter()
        .map(|&var_ptr| ast_variable_copy(var_ptr))
        .collect()
}

pub unsafe fn ast_variable_str(v: *mut AstVariable) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let t: *mut libc::c_char = ast_type_str((*v).r#type);
    strbuilder_printf(
        b,
        b"%s %s\0" as *const u8 as *const libc::c_char,
        t,
        (*v).name,
    );
    free(t as *mut libc::c_void);
    return strbuilder_build(b);
}

pub unsafe fn ast_variable_name(v: *mut AstVariable) -> *mut libc::c_char {
    return (*v).name;
}

pub unsafe fn ast_variable_type(v: *mut AstVariable) -> *mut AstType {
    return (*v).r#type;
}

pub unsafe fn ast_variable_arr_create() -> *mut AstVariableArr {
    Box::into_raw(Box::new(AstVariableArr {
        n: 0,
        v: ptr::null_mut(),
    }))
}

pub unsafe fn ast_variable_arr_append(arr: *mut AstVariableArr, v: *mut AstVariable) {
    (*arr).n += 1;
    (*arr).v = realloc(
        (*arr).v as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstVariable>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut AstVariable;
    let ref mut fresh13 = *((*arr).v).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh13 = v;
}

pub unsafe fn ast_variable_arr_destroy(arr: *mut AstVariableArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        ast_variable_destroy(*((*arr).v).offset(i as isize));
        i += 1;
    }
    drop(Box::from_raw(arr));
}

pub unsafe fn ast_variable_arr_n(arr: *mut AstVariableArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn ast_variable_arr_v(arr: *mut AstVariableArr) -> *mut *mut AstVariable {
    return (*arr).v;
}

pub unsafe fn ast_variable_arr_copy(old: *mut AstVariableArr) -> *mut AstVariableArr {
    let new: *mut AstVariableArr = ast_variable_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        ast_variable_arr_append(new, ast_variable_copy(*((*old).v).offset(i as isize)));
        i += 1;
    }
    return new;
}

pub unsafe fn ast_variable_arr_from_slice(old: &[*mut AstVariable]) -> *mut AstVariableArr {
    let new: *mut AstVariableArr = ast_variable_arr_create();
    for &var_ptr in old {
        ast_variable_arr_append(new, ast_variable_copy(var_ptr));
    }
    new
}

pub unsafe fn ast_function_create(
    isaxiom: bool,
    ret: *mut AstType,
    name: *mut libc::c_char,
    params: Vec<*mut AstVariable>,
    abstract_0: *mut AstBlock,
    body: *mut AstBlock,
) -> *mut AstFunction {
    if abstract_0.is_null() {
        panic!();
    }
    Box::into_raw(Box::new(AstFunction {
        isaxiom,
        ret,
        name,
        params,
        r#abstract: abstract_0,
        body,
    }))
}

pub unsafe fn ast_function_destroy(f: *mut AstFunction) {
    drop(Box::from_raw(f));
}

impl Drop for AstFunction {
    fn drop(&mut self) {
        unsafe {
            ast_type_destroy(self.ret);
            for &param in &self.params {
                ast_variable_destroy(param);
            }
            ast_block_destroy(self.r#abstract);
            if !self.body.is_null() {
                ast_block_destroy(self.body);
            }
            free(self.name as *mut libc::c_void);
        }
    }
}

pub unsafe fn ast_function_str(f: &AstFunction) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    if f.isaxiom {
        strbuilder_printf(b, b"axiom \0" as *const u8 as *const libc::c_char);
    }
    let ret: *mut libc::c_char = ast_type_str(f.ret);
    strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, ret);
    free(ret as *mut libc::c_void);
    strbuilder_printf(b, b"%s(\0" as *const u8 as *const libc::c_char, f.name);
    for (i, &param) in f.params.iter().enumerate() {
        let v: *mut libc::c_char = ast_variable_str(param);
        let space: *mut libc::c_char = (if i + 1 < f.params.len() {
            b", \0" as *const u8 as *const libc::c_char
        } else {
            b"\0" as *const u8 as *const libc::c_char
        }) as *mut libc::c_char;
        strbuilder_printf(b, b"%s%s\0" as *const u8 as *const libc::c_char, v, space);
        free(v as *mut libc::c_void);
    }
    let abs: *mut libc::c_char = ast_block_str(
        &*f.r#abstract,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    strbuilder_printf(b, b") ~ [\n%s]\0" as *const u8 as *const libc::c_char, abs);
    free(abs as *mut libc::c_void);
    if !(f.body).is_null() {
        let body: *mut libc::c_char = ast_block_str(
            &*f.body,
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

pub unsafe fn ast_function_name(f: &AstFunction) -> *mut libc::c_char {
    return f.name;
}

pub unsafe fn ast_function_copy(f: &AstFunction) -> *mut AstFunction {
    let params = f
        .params
        .iter()
        .map(|&param| ast_variable_copy(param))
        .collect();
    return ast_function_create(
        f.isaxiom,
        ast_type_copy(f.ret),
        dynamic_str(f.name),
        params,
        ast_block_copy(&*f.r#abstract),
        if !f.body.is_null() {
            ast_block_copy(&*f.body)
        } else {
            0 as *mut AstBlock
        },
    );
}

pub unsafe fn ast_function_isaxiom(f: &AstFunction) -> bool {
    return f.isaxiom;
}

pub unsafe fn ast_function_isproto(f: &AstFunction) -> bool {
    return !(f.r#abstract).is_null() && (f.body).is_null();
}

pub unsafe fn ast_function_absisempty(f: &AstFunction) -> bool {
    return ast_block_ndecls(&*f.r#abstract) == 0 as libc::c_int
        && ast_block_nstmts(&*f.r#abstract) == 0 as libc::c_int;
}

pub unsafe fn ast_function_type(f: &AstFunction) -> *mut AstType {
    f.ret
}

pub unsafe fn ast_function_body(f: &AstFunction) -> &AstBlock {
    assert!(
        !f.body.is_null(),
        "cannot find body for {:?}",
        CStr::from_ptr(f.name)
    );
    &*f.body
}

pub unsafe fn ast_function_abstract(f: &AstFunction) -> &AstBlock {
    assert!(!f.r#abstract.is_null());
    &*f.r#abstract
}

pub unsafe fn ast_function_params(f: &AstFunction) -> &[*mut AstVariable] {
    f.params.as_slice()
}

pub unsafe fn ast_function_preconditions(f: &AstFunction) -> PrecondsResult {
    ast_block_preconds(ast_function_abstract(f))
}

pub unsafe fn ast_function_protostitch(
    f: *mut AstFunction,
    ext: *mut Externals,
) -> *mut AstFunction {
    let proto: *mut AstFunction = externals_getfunc(ext, (*f).name);
    if !proto.is_null() && !((*proto).r#abstract).is_null() {
        (*f).r#abstract = ast_block_copy(&*(*proto).r#abstract);
    }
    return f;
}

pub unsafe fn ast_function_verify(f: *mut AstFunction, ext: *mut Externals) -> Result<()> {
    let state: *mut State = state_create(
        dynamic_str(ast_function_name(&*f)),
        ext,
        ast_function_type(&*f),
    );
    ast_function_initparams(&*f, state)?;
    path_absverify_withstate(f, state)?;
    state_destroy(state);
    Ok(())
}

unsafe fn path_absverify_withstate(f: *mut AstFunction, state: *mut State) -> Result<()> {
    let abs = ast_function_abstract(&*f);
    let ndecls: libc::c_int = ast_block_ndecls(abs);
    let var: *mut *mut AstVariable = ast_block_decls(abs);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ndecls {
        state_declare(state, *var.offset(i as isize), false);
        i += 1;
    }
    return path_absverify(f, state, 0 as libc::c_int);
}

unsafe fn path_absverify(f: *mut AstFunction, state: *mut State, index: libc::c_int) -> Result<()> {
    let abs = ast_function_abstract(&*f);
    let nstmts: libc::c_int = ast_block_nstmts(abs);
    let stmt: *mut *mut AstStmt = ast_block_stmts(abs);
    let mut i: libc::c_int = index;
    while i < nstmts {
        let mut splits: AstStmtSplits = ast_stmt_splits(&**stmt.offset(i as isize), state);
        if let Some(err) = splits.err {
            return Err(err);
        }
        if splits.n != 0 {
            if (splits.cond).is_null() {
                panic!();
            }
            return split_paths_absverify(f, state, i, &mut splits);
        }
        if !ast_stmt_ispre(&**stmt.offset(i as isize)) {
            ast_stmt_absexec(&**stmt.offset(i as isize), state, true)?;
        }
        i += 1;
    }
    abstract_audit(f, state)?;
    Ok(())
}

pub unsafe fn ast_function_initparams(f: &AstFunction, s: *mut State) -> Result<()> {
    let params = ast_function_params(f);
    for &param in params {
        state_declare(s, param, true);
    }
    ast_function_precondsinit(f, s)?;
    for &param in params {
        inititalise_param(param, s)?;
    }
    Ok(())
}

unsafe fn ast_function_precondsinit(f: &AstFunction, s: *mut State) -> Result<()> {
    let pre: PrecondsResult = ast_function_preconditions(f);
    if let Some(err) = pre.err {
        return Err(err);
    }
    if pre.stmt.is_null() {
        return Ok(());
    }
    ast_stmt_absexec(&*pre.stmt, s, true)?;
    Ok(())
}

unsafe fn inititalise_param(param: *mut AstVariable, state: *mut State) -> Result<()> {
    let name: *mut libc::c_char = ast_variable_name(param);
    let t: *mut AstType = ast_variable_type(param);
    let obj: *mut Object = state_getobject(state, name);
    if obj.is_null() {
        panic!();
    }
    if !object_hasvalue(obj) {
        let val: *mut Value = state_vconst(state, t, dynamic_str(name), true);
        object_assign(obj, val);
    }
    Ok(())
}

unsafe fn abstract_audit(f: *mut AstFunction, abstract_state: *mut State) -> Result<()> {
    let actual_state: *mut State = state_create_withprops(
        dynamic_str(ast_function_name(&*f)),
        state_getext(abstract_state),
        ast_function_type(&*f),
        state_getprops(abstract_state),
    );
    ast_function_initparams(&*f, actual_state).unwrap();
    ast_function_setupabsexec(&*f, actual_state)?;
    abstract_auditwithstate(f, actual_state, abstract_state)?;
    state_destroy(actual_state);
    Ok(())
}

unsafe fn ast_function_setupabsexec(f: &AstFunction, state: *mut State) -> Result<()> {
    let nstmts: libc::c_int = ast_block_nstmts(&*f.r#abstract);
    let stmt: *mut *mut AstStmt = ast_block_stmts(&*f.r#abstract);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        ast_stmt_setupabsexec(&**stmt.offset(i as isize), state)?;
        i += 1;
    }
    Ok(())
}

unsafe fn abstract_auditwithstate(
    f: *mut AstFunction,
    actual_state: *mut State,
    abstract_state: *mut State,
) -> Result<()> {
    let ndecls: libc::c_int = ast_block_ndecls(&*(*f).body);
    let var: *mut *mut AstVariable = ast_block_decls(&*(*f).body);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < ndecls {
        state_declare(actual_state, *var.offset(i as isize), false);
        i += 1;
    }
    return path_verify(f, actual_state, 0 as libc::c_int, abstract_state);
}

unsafe fn path_verify(
    f: *mut AstFunction,
    actual_state: *mut State,
    index: libc::c_int,
    abstract_state: *mut State,
) -> Result<()> {
    let fname: *mut libc::c_char = ast_function_name(&*f);
    let nstmts: libc::c_int = ast_block_nstmts(&*(*f).body);
    let stmt: *mut *mut AstStmt = ast_block_stmts(&*(*f).body);
    let mut i: libc::c_int = index;
    while i < nstmts {
        let mut splits: AstStmtSplits = ast_stmt_splits(&**stmt.offset(i as isize), actual_state);
        if splits.n != 0 {
            return split_paths_verify(f, actual_state, i, &mut splits, abstract_state);
        }
        ast_stmt_process(&**stmt.offset(i as isize), fname, actual_state)?;
        if ast_stmt_isterminal(&**stmt.offset(i as isize), actual_state) {
            break;
        }
        i += 1;
    }
    if state_hasgarbage(actual_state) {
        vprintln!(
            "actual: {}",
            CStr::from_ptr(state_str(actual_state)).to_string_lossy()
        );
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"%s: garbage on heap\0" as *const u8 as *const libc::c_char,
            ast_function_name(&*f),
        );
        return Err(error_create(strbuilder_build(b)));
    }
    let equiv: bool = state_equal(actual_state, abstract_state);
    if !equiv {
        let b_0: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b_0,
            b"%s: actual and abstract states differ\0" as *const u8 as *const libc::c_char,
            ast_function_name(&*f),
        );
        return Err(error_create(strbuilder_build(b_0)));
    }
    Ok(())
}

pub unsafe fn ast_function_absexec(f: &AstFunction, state: *mut State) -> Result<*mut Value> {
    let ndecls: libc::c_int = ast_block_ndecls(&*f.r#abstract);
    if ndecls != 0 {
        let var: *mut *mut AstVariable = ast_block_decls(&*f.r#abstract);
        let mut i: libc::c_int = 0 as libc::c_int;
        while i < ndecls {
            state_declare(state, *var.offset(i as isize), false);
            i += 1;
        }
    }
    let nstmts: libc::c_int = ast_block_nstmts(&*f.r#abstract);
    let stmt: *mut *mut AstStmt = ast_block_stmts(&*f.r#abstract);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < nstmts {
        ast_stmt_absexec(&**stmt.offset(i_0 as isize), state, false)?;
        i_0 += 1;
    }
    let obj: *mut Object = state_getresult(state);
    if obj.is_null() {
        panic!();
    }
    return Ok(object_as_value(obj));
}

unsafe fn split_path_verify(
    f: *mut AstFunction,
    actual_state: *mut State,
    index: libc::c_int,
    cond: &AstExpr,
    abstract_state: *mut State,
) -> Result<()> {
    let paths = body_paths(&*f, index, cond);
    assert_eq!(paths.len(), 2);
    // Note: Original leaks both functions.
    for (i, f) in paths.into_iter().enumerate() {
        let actual_copy: *mut State = state_copywithname(actual_state, ast_function_name(&*f));
        let abstract_copy: *mut State = state_copywithname(abstract_state, ast_function_name(&*f));
        // Note: Original leaks expression.
        let expr = ast_expr_inverted_copy(cond, i == 1);
        let r = ast_expr_assume(&expr, actual_copy);
        std::mem::forget(expr);
        let r = r?;
        if !r.is_contradiction {
            path_verify(f, actual_copy, index, abstract_copy)?;
        }
    }
    Ok(())
}

unsafe fn recurse_buildgraph(
    g: &mut Map,
    dedup: &mut Map,
    fname: *mut libc::c_char,
    ext: *mut Externals,
) {
    let mut local_dedup = Map::new();
    if !(dedup.get(fname)).is_null() {
        return;
    }
    dedup.set(fname, 1 as libc::c_int as *mut libc::c_void);
    let f: *mut AstFunction = externals_getfunc(ext, fname);
    if f.is_null() {
        eprintln!(
            "function `{}' is not declared",
            CStr::from_ptr(fname).to_string_lossy()
        );
        process::exit(1);
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
    let body: *mut AstBlock = (*f).body;
    let nstmts: libc::c_int = ast_block_nstmts(&*body);
    let stmt: *mut *mut AstStmt = ast_block_stmts(&*body);
    if stmt.is_null() {
        panic!();
    }
    let mut val = string_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < nstmts {
        let mut farr = ast_stmt_getfuncs(&**stmt.offset(i as isize));
        let func: *mut *mut libc::c_char = string_arr_s(&mut farr);
        let mut j: libc::c_int = 0 as libc::c_int;
        while j < string_arr_n(&farr) {
            if (local_dedup.get(*func.offset(j as isize))).is_null() {
                let f_0: *mut AstFunction = externals_getfunc(ext, *func.offset(j as isize));
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
    f: &AstFunction,
    index: libc::c_int,
    cond: &AstExpr,
) -> Vec<*mut AstFunction> {
    let f_true: *mut AstFunction = ast_function_create(
        f.isaxiom,
        ast_type_copy(f.ret),
        split_name(f.name, cond),
        ast_variables_copy(&f.params),
        ast_block_copy(&*f.r#abstract),
        ast_block_copy(&*f.body),
    );
    // Note: Original leaks inv_assumption, but I think unintentionally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false: *mut AstFunction = ast_function_create(
        f.isaxiom,
        ast_type_copy(f.ret),
        split_name(f.name, &inv_assumption),
        ast_variables_copy(&f.params),
        ast_block_copy(&*f.r#abstract),
        ast_block_copy(&*f.body),
    );
    vec![f_true, f_false]
}

unsafe fn split_path_absverify(
    f: *mut AstFunction,
    state: *mut State,
    index: libc::c_int,
    cond: &AstExpr,
) -> Result<()> {
    let paths = abstract_paths(&*f, index, cond);
    assert_eq!(paths.len(), 2);
    for (i, f) in paths.into_iter().enumerate() {
        let s_copy: *mut State = state_copywithname(state, ast_function_name(&*f));
        // Note: Original leaks `inv` but I think accidentally.
        let inv = ast_expr_inverted_copy(cond, i == 1);
        let r = ast_expr_assume(&inv, s_copy)?;
        if !r.is_contradiction {
            path_absverify(f, s_copy, index)?;
        }
    }
    Ok(())
}

unsafe fn split_paths_absverify(
    f: *mut AstFunction,
    state: *mut State,
    index: libc::c_int,
    splits: *mut AstStmtSplits,
) -> Result<()> {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*splits).n {
        split_path_absverify(f, state, index, &**(*splits).cond.offset(i as isize))?;
        i += 1;
    }
    Ok(())
}

pub unsafe fn ast_function_buildgraph(fname: *mut libc::c_char, ext: *mut Externals) -> Box<Map> {
    let mut dedup = Map::new();
    let mut g = Map::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    return g;
}

unsafe fn split_name(name: *mut libc::c_char, assumption: &AstExpr) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let assumption_str: *mut libc::c_char = ast_expr_str(assumption);
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
    f: *mut AstFunction,
    actual_state: *mut State,
    index: libc::c_int,
    splits: *mut AstStmtSplits,
    abstract_state: *mut State,
) -> Result<()> {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*splits).n {
        split_path_verify(
            f,
            actual_state,
            index,
            &**((*splits).cond).offset(i as isize),
            abstract_state,
        )?;
        i += 1;
    }
    Ok(())
}

unsafe fn body_paths(f: &AstFunction, index: libc::c_int, cond: &AstExpr) -> Vec<*mut AstFunction> {
    let f_true: *mut AstFunction = ast_function_create(
        f.isaxiom,
        ast_type_copy(f.ret),
        split_name(f.name, cond),
        ast_variables_copy(&f.params),
        ast_block_copy(&*f.r#abstract),
        f.body,
    );
    // Note: Original leaks `inv_assumption` but I think accidentally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false: *mut AstFunction = ast_function_create(
        f.isaxiom,
        ast_type_copy(f.ret),
        split_name(f.name, &*inv_assumption),
        ast_variables_copy(&f.params),
        ast_block_copy(&*f.r#abstract),
        f.body,
    );
    vec![f_true, f_false]
}

pub unsafe fn ast_functiondecl_create(f: *mut AstFunction) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: AstExternDeclKind::Function(f),
    })
}

pub unsafe fn ast_externdecl_as_function_ptr(decl: &AstExternDecl) -> Option<*mut AstFunction> {
    match &(*decl).kind {
        AstExternDeclKind::Function(f) => Some(*f),
        _ => None,
    }
}

pub unsafe fn ast_externdecl_as_function(decl: &AstExternDecl) -> Option<&AstFunction> {
    match &(*decl).kind {
        AstExternDeclKind::Function(f) => Some(&**f),
        _ => None,
    }
}

pub unsafe fn ast_decl_create(name: *mut libc::c_char, t: *mut AstType) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: if ast_type_istypedef(t) {
            AstExternDeclKind::Typedef(AstTypedefDecl { name, type_0: t })
        } else if ast_type_isstruct(&*t) {
            if (ast_type_struct_tag(&*t)).is_null() {
                panic!();
            }
            AstExternDeclKind::Struct(t)
        } else {
            AstExternDeclKind::Variable(ast_variable_create(name, t))
        },
    })
}

pub unsafe fn ast_externdecl_install(decl: *mut AstExternDecl, ext: *mut Externals) {
    match &(*decl).kind {
        AstExternDeclKind::Function(f) => {
            externals_declarefunc(ext, ast_function_name(&**f), *f);
        }
        AstExternDeclKind::Variable(v) => {
            externals_declarevar(ext, ast_variable_name(*v), *v);
        }
        AstExternDeclKind::Typedef(typedef) => {
            externals_declaretypedef(ext, typedef.name, typedef.type_0);
        }
        AstExternDeclKind::Struct(s) => {
            externals_declarestruct(ext, *s);
        }
    }
}

impl Drop for AstExternDeclKind {
    fn drop(&mut self) {
        unsafe {
            match self {
                AstExternDeclKind::Function(f) => {
                    ast_function_destroy(*f);
                }
                AstExternDeclKind::Variable(v) => {
                    ast_variable_destroy(*v);
                }
                AstExternDeclKind::Typedef(td) => {
                    free(td.name as *mut libc::c_void);
                    ast_type_destroy(td.type_0);
                }
                AstExternDeclKind::Struct(s) => {
                    ast_type_destroy(*s);
                }
            }
        }
    }
}

pub fn parse_int(s: &str) -> libc::c_int {
    s.parse().expect("parse error")
}

pub unsafe fn parse_char(s: &str) -> libc::c_char {
    assert!(s.starts_with('\'') && s.ends_with('\''));
    let s = &s[1..s.len() - 1];
    if s.starts_with('\\') {
        parse_escape(&s[1..]) as libc::c_char
    } else {
        s.chars().next().expect("invalid char literal") as u32 as libc::c_char
    }
}

pub unsafe fn parse_escape(c: &str) -> libc::c_int {
    match c {
        "0" => '\0' as u32 as libc::c_int,
        "t" => '\t' as u32 as libc::c_int,
        "n" => '\t' as u32 as libc::c_int, // Note: '\t' rather than '\n', a bug in the original.
        _ => {
            panic!("unrecognized char escape sequence: {:?}", c);
        }
    }
}

pub unsafe fn lvalue_create(t: *mut AstType, obj: *mut Object) -> *mut LValue {
    Box::into_raw(Box::new(LValue { t, obj }))
}

pub unsafe fn lvalue_destroy(l: *mut LValue) {
    ast_type_destroy((*l).t);
    object_destroy((*l).obj);
    drop(Box::from_raw(l));
}

pub unsafe fn lvalue_type(l: *mut LValue) -> *mut AstType {
    return (*l).t;
}

pub unsafe fn lvalue_object(l: *mut LValue) -> *mut Object {
    return (*l).obj;
}

pub unsafe fn ast_topological_order(
    fname: *mut libc::c_char,
    ext: *mut Externals,
) -> Box<StringArr> {
    topological_order(fname, ext)
}

pub unsafe fn ast_protostitch(f: *mut AstFunction, ext: *mut Externals) -> *mut AstFunction {
    ast_function_protostitch(f, ext)
}
