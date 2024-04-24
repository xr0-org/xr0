#![allow(dead_code)]

use std::collections::VecDeque;
use std::ffi::{CStr, CString};
use std::fmt::{self, Display, Formatter};
use std::process;
use std::ptr;

use libc::strncmp;

use crate::math::{math_eq, math_ge, math_gt, math_le, math_lt, MathAtom, MathExpr};
use crate::object::{
    object_as_value, object_assign, object_getmember, object_getmembertype, object_hasvalue,
};
use crate::parser::LexemeMarker;
use crate::state::state::{
    state_addresses_deallocand, state_alloc, state_clump, state_copy, state_copywithname,
    state_create, state_create_withprops, state_dealloc, state_declare, state_deref, state_equal,
    state_get, state_getext, state_getloc, state_getobject, state_getobjecttype, state_getprops,
    state_getresult, state_getvconst, state_hasgarbage, state_isalloc, state_islval,
    state_popframe, state_pushframe, state_range_alloc, state_range_aredeallocands,
    state_range_dealloc, state_static_init, state_str, state_vconst,
};
use crate::util::{
    dynamic_str, strbuilder_build, strbuilder_create, strbuilder_putc, string_arr_contains, Error,
    InsertionOrderMap, Map, OwningCStr, Result, SemiBox, StrBuilder,
};
use crate::value::{
    value_as_constant, value_as_location, value_as_sync, value_copy, value_destroy, value_equal,
    value_int_create, value_int_indefinite_create, value_into_sync, value_isconstant, value_isint,
    value_islocation, value_isstruct, value_issync, value_literal_create, value_pf_augment,
    value_ptr_indefinite_create, value_str, value_struct_indefinite_create, value_struct_member,
    value_sync_create, value_to_expr,
};
use crate::{cstr, strbuilder_write, vprintln, Externals, Object, State, Value};

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

#[derive(Clone)]
pub struct AstExpr {
    kind: AstExprKind,
}

#[derive(Clone)]
pub struct AllocExpr {
    kind: AstAllocKind,
    arg: Box<AstExpr>,
}

#[derive(Clone)]
pub struct UnaryExpr {
    op: AstUnaryOp,
    arg: Box<AstExpr>,
}

#[derive(Clone)]
pub struct BinaryExpr {
    op: AstBinaryOp,
    e1: Box<AstExpr>,
    e2: Box<AstExpr>,
}

#[derive(Clone)]
pub struct AssignmentExpr {
    lval: Box<AstExpr>,
    rval: Box<AstExpr>,
}

#[derive(Clone)]
pub struct IncDecExpr {
    operand: Box<AstExpr>,
    inc: libc::c_int,
    pre: libc::c_int,
}

#[derive(Clone)]
pub struct CallExpr {
    fun: Box<AstExpr>,
    args: Vec<Box<AstExpr>>,
}

#[derive(Clone)]
pub struct ConstantExpr {
    constant: libc::c_int,
    ischar: bool,
}

#[derive(Clone)]
pub struct StructMemberExpr {
    root: Box<AstExpr>,
    field: OwningCStr,
}

#[derive(Clone)]
enum AstExprKind {
    Identifier(OwningCStr),
    Constant(ConstantExpr),
    StringLiteral(OwningCStr),
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

// Note: In the original, `ast_type_copy(ast_type_create_voidptr())` would assert, and modifiers
// were not always copied.
#[derive(Clone)]
pub struct AstType {
    pub modifiers: libc::c_uint,
    pub base: AstTypeBase,
}

#[derive(Clone)]
pub struct AstStructType {
    pub tag: Option<OwningCStr>,
    pub members: Option<Box<Vec<Box<AstVariable>>>>,
}

#[derive(Clone)]
pub struct AstVariable {
    // Note: In the original, `name` could be null. However in most situations (almost all; an
    // exception is the parameter in `fclose(FILE *);` which has no name) this would be invalid, so
    // we end up banning it.
    pub name: OwningCStr,
    pub type_: Box<AstType>,
}

#[derive(Clone)]
pub struct AstArrayType {
    pub type_: Box<AstType>,
    pub length: libc::c_int,
}

#[derive(Clone)]
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
    Pointer(Option<Box<AstType>>),
    Array(AstArrayType),
    Struct(AstStructType),
    Union(AstStructType),
    Enum,
    UserDefined(OwningCStr),
}

pub type AstTypeModifier = libc::c_uint;
pub const MOD_TYPEDEF: AstTypeModifier = 1;
pub const MOD_EXTERN: AstTypeModifier = 2;
pub const MOD_STATIC: AstTypeModifier = 4;
pub const MOD_AUTO: AstTypeModifier = 8;
pub const MOD_REGISTER: AstTypeModifier = 16;
pub const MOD_CONST: AstTypeModifier = 32;
pub const MOD_VOLATILE: AstTypeModifier = 64;

// most likely this is a borrow of the type
pub struct LValue<'ast> {
    pub t: Option<&'ast AstType>,
    pub obj: *mut Object,
}

#[derive(Clone)]
pub struct AstFunction<'ast> {
    pub is_axiom: bool,
    pub ret: Box<AstType>,
    pub name: OwningCStr,
    pub params: Vec<Box<AstVariable>>,
    pub abstract_: Box<AstBlock>,
    pub body: Option<SemiBox<'ast, AstBlock>>,
}

#[derive(Clone)]
pub struct AstBlock {
    pub decls: Vec<Box<AstVariable>>,
    pub stmts: Vec<Box<AstStmt>>,
}

// Note: In the original, `ast_stmt_copy` did not handle allocation statements.
#[derive(Clone)]
pub struct AstStmt {
    pub kind: AstStmtKind,
    pub loc: Box<LexemeMarker>,
}

#[derive(Clone)]
pub struct AstAllocStmt {
    pub kind: AstAllocKind,
    pub arg: Box<AstExpr>,
}

#[derive(Clone)]
pub struct AstJumpStmt {
    pub kind: AstJumpKind,
    pub rv: Option<Box<AstExpr>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstJumpKind {
    Return,
}

#[derive(Clone)]
pub struct AstIterationStmt {
    pub init: Box<AstStmt>,
    pub cond: Box<AstStmt>,
    pub body: Box<AstStmt>,
    pub iter: Box<AstExpr>,
    pub abstract_: Box<AstBlock>,
}

#[derive(Clone)]
pub struct AstSelectionStmt {
    pub isswitch: bool,
    pub cond: Box<AstExpr>,
    pub body: Box<AstStmt>,
    pub nest: Option<Box<AstStmt>>,
}

#[derive(Clone)]
pub struct AstLabelledStmt {
    pub label: OwningCStr,
    pub stmt: Box<AstStmt>,
}

#[derive(Clone)]
pub enum AstStmtKind {
    Nop,
    Labelled(AstLabelledStmt),
    Compound(Box<AstBlock>),
    CompoundV(Box<AstBlock>),
    Expr(Box<AstExpr>),
    Selection(AstSelectionStmt),
    Iteration(AstIterationStmt),
    IterationE(AstIterationStmt),
    Jump(AstJumpStmt),
    Allocation(AstAllocStmt),
}

#[derive(Clone)]
pub struct Preresult {
    pub is_contradiction: bool,
}

#[derive(Clone, Default)]
pub struct AstStmtSplits {
    // Note: In the original this array is heap-allocated but never freed.
    pub conds: Vec<*mut AstExpr>,
}

pub struct AstExternDecl {
    pub kind: AstExternDeclKind,
}

pub struct AstTypedefDecl {
    pub name: OwningCStr,
    pub type_0: Box<AstType>,
}

pub enum AstExternDeclKind {
    Function(*mut AstFunction<'static>),
    Variable(Box<AstVariable>),
    Typedef(AstTypedefDecl),
    Struct(Box<AstType>),
}

pub struct Ast {
    pub decls: Vec<Box<AstExternDecl>>,
}

impl Display for AstExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", ast_expr_str(self))
    }
}

unsafe fn expr_literal_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    Ok(state_static_init(state, expr))
}

unsafe fn expr_constant_eval(expr: &AstExpr, _state: *mut State) -> Result<*mut Value> {
    Ok(value_int_create(ast_expr_as_constant(expr)))
}

unsafe fn rangeprocess_dealloc(
    dealloc: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    let obj: *mut Object = hack_base_object_from_alloc(ast_expr_alloc_arg(dealloc), state);
    state_range_dealloc(state, obj, lw, up)
}

unsafe fn hack_base_object_from_alloc(expr: &AstExpr, state: *mut State) -> *mut Object {
    let inner = ast_expr_unary_operand(expr);
    let i = ast_expr_identifier_create(OwningCStr::copy_str("i"));
    if !ast_expr_equal(ast_expr_binary_e2(inner), &i) {
        panic!();
    }
    drop(i);
    let lval = ast_expr_lvalue(ast_expr_binary_e1(inner), state).unwrap();
    let obj: *mut Object = lvalue_object(&lval);
    if obj.is_null() {
        panic!();
    }
    obj
}

pub fn ast_expr_equal(e1: &AstExpr, e2: &AstExpr) -> bool {
    match (&e1.kind, &e2.kind) {
        (AstExprKind::Constant(c1), AstExprKind::Constant(c2)) => c1.constant == c2.constant,
        (AstExprKind::Identifier(id1), AstExprKind::Identifier(id2)) => *id1 == *id2,
        (AstExprKind::StringLiteral(s1), AstExprKind::StringLiteral(s2)) => *s1 == *s2,
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
            c1.args.len() == c2.args.len()
                && c1
                    .args
                    .iter()
                    .zip(&c2.args)
                    .all(|(arg1, arg2)| ast_expr_equal(arg1, arg2))
                && ast_expr_equal(&c1.fun, &c2.fun)
        }
        (AstExprKind::StructMember(m1), AstExprKind::StructMember(m2)) => {
            ast_expr_equal(&m1.root, &m2.root) && m1.field == m2.field
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
    state_range_alloc(state, obj, lw, up)
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

fn math_expr(e: &AstExpr) -> Box<MathExpr> {
    Box::new(match &e.kind {
        AstExprKind::Identifier(id) => MathExpr::Atom(MathAtom::Variable(id.clone())),
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

fn binary_e2(e2: &AstExpr, op: AstBinaryOp) -> Box<MathExpr> {
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
    value_compare(&*root, ast_expr_binary_op(expr), &*last)
}

unsafe fn value_compare(v1: &Value, op: AstBinaryOp, v2: &Value) -> bool {
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
    let obj: *mut Object = lvalue_object(&res_lval);
    if obj.is_null() {
        panic!();
    }
    obj
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
        AstExprKind::Unary(_) => unary_rangedecide(expr, lw, up, state),
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
    let i = ast_expr_identifier_create(OwningCStr::copy_str("i"));
    let j = ast_expr_identifier_create(OwningCStr::copy_str("j"));
    if !(ast_expr_equal(ast_expr_binary_e2(inner), &i)
        || ast_expr_equal(ast_expr_binary_e2(inner), &j))
    {
        panic!();
    }
    drop(j);
    drop(i);
    let res_lval = ast_expr_lvalue(ast_expr_binary_e1(acc), state).unwrap();
    let obj: *mut Object = lvalue_object(&res_lval);
    if obj.is_null() {
        panic!();
    }
    state_range_aredeallocands(state, obj, lw, up)
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

pub fn ast_expr_arbarg_create() -> Box<AstExpr> {
    ast_expr_create(AstExprKind::ArbArg)
}

pub unsafe fn ast_expr_assume(expr: &AstExpr, state: *mut State) -> Result<Preresult> {
    reduce_assume(expr, true, state)
}

unsafe fn reduce_assume(expr: &AstExpr, value: bool, s: *mut State) -> Result<Preresult> {
    match &expr.kind {
        AstExprKind::Identifier(_) => identifier_assume(expr, value, s),
        AstExprKind::Unary(unary) => {
            assert_eq!(unary.op, AstUnaryOp::Bang);
            reduce_assume(&unary.arg, !value, s)
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
    let v1 = ast_expr_pf_reduce(&b.e1, s).unwrap();
    let v2 = ast_expr_pf_reduce(&b.e2, s).unwrap();
    // Note: original leaks the expression.
    let expr = ast_expr_binary_create(value_to_expr(&*v1), b.op, value_to_expr(&*v2));
    let result = irreducible_assume(&expr, value, s);
    std::mem::forget(expr);
    result
}

unsafe fn irreducible_assume(e: &AstExpr, value: bool, s: *mut State) -> Result<Preresult> {
    let prop = ast_expr_inverted_copy(e, !value);
    irreducible_assume_actual(&prop, s)
}

unsafe fn irreducible_assume_actual(e: &AstExpr, s: *mut State) -> Result<Preresult> {
    let p = state_getprops(&mut *s);
    if p.contradicts(e) {
        return Ok(Preresult {
            is_contradiction: true,
        });
    }
    p.install(ast_expr_copy(e));
    Ok(Preresult {
        is_contradiction: false,
    })
}

pub fn ast_expr_isdereferencable_create(assertand: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::IsDereferencable(assertand))
}

unsafe fn ast_expr_pf_reduce_assume(
    expr: &AstExpr,
    value: bool,
    s: *mut State,
) -> Result<Preresult> {
    let res_val = ast_expr_pf_reduce(expr, s).unwrap();
    assert!(!res_val.is_null());
    irreducible_assume(&value_into_sync(res_val), value, s)
}

unsafe fn identifier_assume(expr: &AstExpr, value: bool, s: *mut State) -> Result<Preresult> {
    let mut s_copy = state_copy(&*s);
    let res_val = ast_expr_eval(expr, &mut s_copy).unwrap();
    assert!(!res_val.is_null());
    drop(s_copy);
    irreducible_assume(&value_into_sync(res_val), value, s)
}

unsafe fn binary_deref_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let arr = ast_expr_eval(ast_expr_binary_e1(expr), state)?;
    if arr.is_null() {
        panic!();
    }
    let deref_obj = state_deref(state, arr, ast_expr_binary_e2(expr))?;
    if deref_obj.is_null() {
        return Err(Error::new(format!(
            "undefined indirection: *({expr}) has no value"
        )));
    }
    let v: *mut Value = object_as_value(deref_obj);
    if v.is_null() {
        return Err(Error::new(format!(
            "undefined indirection: *({expr}) has no value"
        )));
    }
    Ok(value_copy(&*v))
}

unsafe fn hack_identifier_builtin_eval(
    id: *mut libc::c_char,
    state: *mut State,
) -> Result<*mut Value> {
    if !(state_getvconst(state, id)).is_null()
        || strncmp(id, b"ptr:\0" as *const u8 as *const libc::c_char, 4) == 0 as libc::c_int
    {
        return Ok(value_sync_create(ast_expr_identifier_create(
            OwningCStr::copy_char_ptr(id),
        )));
    }
    Err(Error::new("not built-in".to_string()))
}

pub fn ast_expr_isdeallocand_create(assertand: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::IsDeallocand(assertand))
}

pub fn ast_expr_assignment_create(root: Box<AstExpr>, value: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Assignment(AssignmentExpr {
        lval: root,
        rval: value,
    }))
}

fn ast_expr_bracketed_str_build(inner: &AstExpr, b: &mut StrBuilder) {
    strbuilder_write!(*b, "({inner})");
}

fn expr_to_binary(expr: &AstExpr) -> Box<AstExpr> {
    match &expr.kind {
        AstExprKind::Binary(_) => ast_expr_copy(expr),
        _ => ast_expr_binary_create(
            ast_expr_copy(expr),
            AstBinaryOp::Addition,
            ast_expr_constant_create(0),
        ),
    }
}

unsafe fn expr_identifier_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let id = ast_expr_as_identifier(expr);
    if let Ok(res) = hack_identifier_builtin_eval(id.as_ptr(), state) {
        if !res.is_null() {
            return Ok(res);
        }
    }
    if id.as_str().starts_with('#') {
        return Ok(value_literal_create(id.as_ptr()));
    }
    let obj: *mut Object = state_getobject(state, id.as_ptr());
    if obj.is_null() {
        return Err(Error::new(format!("unknown idenitfier {id}")));
    }
    let val: *mut Value = object_as_value(obj);
    if val.is_null() {
        vprintln!("state: {}", state_str(state));
        return Err(Error::new(format!(
            "undefined memory access: {id} has no value",
        )));
    }
    Ok(value_copy(&*val))
}

unsafe fn expr_structmember_eval(expr: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let root = ast_expr_member_root(expr);
    let res_val = ast_expr_eval(root, s)?;
    let field = ast_expr_member_field(expr);
    let member: *mut Object = value_struct_member(res_val, field.as_ptr());
    if member.is_null() {
        return Err(Error::new(format!("`{root}' has no field `{field}'")));
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
    let v: *mut Value = state_getloc(state, id.as_ptr());
    Ok(v)
}

pub unsafe fn ast_expr_alloc_rangeprocess(
    alloc: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    let lw_val = ast_expr_eval(lw, state)?;
    let up_val = ast_expr_eval(up, state)?;
    let res_lw = value_to_expr(&*lw_val);
    let res_up = value_to_expr(&*up_val);
    match &alloc.kind {
        AstExprKind::Assignment(_) => rangeprocess_alloc(alloc, &res_lw, &res_up, state),
        AstExprKind::Allocation(_) => rangeprocess_dealloc(alloc, &res_lw, &res_up, state),
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

unsafe fn arbarg_eval(_expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    Ok(state_vconst(
        state,
        &ast_type_create_ptr(ast_type_create(AstTypeBase::Void, 0)),
        ptr::null_mut(),
        false,
    ))
}

unsafe fn expr_binary_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let e1 = ast_expr_binary_e1(expr);
    let e2 = ast_expr_binary_e2(expr);
    let v1 = ast_expr_eval(e1, state)?;
    let v2 = ast_expr_eval(e2, state)?;
    Ok(value_sync_create(ast_expr_binary_create(
        value_to_expr(&*v1),
        ast_expr_binary_op(expr),
        value_to_expr(&*v2),
    )))
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
        return Err(Error::new("undefined indirection (rvalue)".to_string()));
    }
    let lval_lval = ast_expr_lvalue(lval, state)?;
    let obj: *mut Object = lvalue_object(&lval_lval);
    if obj.is_null() {
        return Err(Error::new(format!(
            "undefined indirection: {lval} is not an lvalue"
        )));
    }
    object_assign(obj, value_copy(&*rval_val));
    Ok(rval_val)
}

pub unsafe fn ast_expr_lvalue(expr: &AstExpr, state: *mut State) -> Result<LValue> {
    match &expr.kind {
        AstExprKind::Identifier(_) => expr_identifier_lvalue(expr, state),
        AstExprKind::Unary(_) => expr_unary_lvalue(expr, state),
        AstExprKind::StructMember(_) => expr_structmember_lvalue(expr, state),
        _ => panic!(),
    }
}

pub unsafe fn expr_structmember_lvalue(expr: &AstExpr, state: *mut State) -> Result<LValue> {
    let root = ast_expr_member_root(expr);
    // Note: Original fails to check for errors.
    let root_lval = ast_expr_lvalue(root, state).unwrap();
    let root_obj: *mut Object = lvalue_object(&root_lval);
    if root_obj.is_null() {
        panic!();
    }
    let field = ast_expr_member_field(expr);
    let member: *mut Object =
        object_getmember(root_obj, lvalue_type(&root_lval), field.as_ptr(), state);
    if member.is_null() {
        return Err(Error::new("lvalue error".to_string()));
    }
    let t = object_getmembertype(root_obj, lvalue_type(&root_lval), field.as_ptr(), state);
    Ok(lvalue_create(t, member))
}

pub unsafe fn expr_unary_lvalue(expr: &AstExpr, state: *mut State) -> Result<LValue> {
    if !(ast_expr_unary_op(expr) == AstUnaryOp::Dereference) {
        panic!();
    }
    let inner = ast_expr_unary_operand(expr);
    if matches!(inner.kind, AstExprKind::Identifier(_)) {
        let root_lval = ast_expr_lvalue(inner, state)?;
        let root_obj: *mut Object = lvalue_object(&root_lval);
        if root_obj.is_null() {
            // `root` freed

            // Note: The original does `return (struct lvalue_res) { .lval = NULL, .err = NULL };`
            // here but I believe every single caller dereferences lval without checking it for
            // null, so it will crash.
            panic!();
        }
        let t = ast_type_ptr_type(lvalue_type(&root_lval));
        let root_val: *mut Value = object_as_value(root_obj);
        if root_val.is_null() {
            panic!();
        }
        let obj = state_deref(state, root_val, &ast_expr_constant_create(0))?;
        return Ok(lvalue_create(t, obj));
    }
    let root_lval = ast_expr_lvalue(ast_expr_binary_e1(inner), state)?;
    let root_obj: *mut Object = lvalue_object(&root_lval);
    if root_obj.is_null() {
        // `root` freed

        // Note: Original returns null. See note above.
        panic!();
    }
    let t = ast_type_ptr_type(lvalue_type(&root_lval));
    let root_val: *mut Value = object_as_value(root_obj);
    if root_val.is_null() {
        panic!();
    }
    let Ok(res_obj) = state_deref(state, root_val, ast_expr_binary_e2(inner)) else {
        // Note: Original returns null. See note above.
        panic!();
    };
    Ok(lvalue_create(t, res_obj))
}

pub unsafe fn expr_identifier_lvalue(expr: &AstExpr, state: *mut State) -> Result<LValue> {
    let id = ast_expr_as_identifier(expr);
    Ok(lvalue_create(
        Some(state_getobjecttype(&*state, id.as_ptr())),
        state_getobject(state, id.as_ptr()),
    ))
}

unsafe fn dereference_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let binary = expr_to_binary(ast_expr_unary_operand(expr));
    binary_deref_eval(&binary, state)
}

fn ast_expr_constant_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::Constant(c) = &expr.kind else {
        panic!()
    };
    let constant: libc::c_int = c.constant;
    if !c.ischar {
        strbuilder_write!(*b, "{constant}");
        return;
    }
    match char::try_from(constant as u32) {
        Ok(c) => {
            strbuilder_write!(*b, "{:?}", c);
        }
        _ => {
            strbuilder_write!(*b, "'\\x{{{:x}}}'", constant as libc::c_uint);
        }
    }
}

unsafe fn expr_call_eval(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let root = ast_expr_call_root(expr);
    let name = ast_expr_as_identifier(root);
    let f: *mut AstFunction<'static> = (*state_getext(state)).get_func(name.as_ptr());
    if f.is_null() {
        return Err(Error::new(format!("function `{name}' not found")));
    }
    let params = (*f).params();
    let rtype = (*f).rtype();
    let args = prepare_arguments(ast_expr_call_args(expr), params, state);
    state_pushframe(state, dynamic_str(name.as_ptr()), rtype);
    prepare_parameters(params, args, name.as_ptr(), state)?;
    call_setupverify(f, &mut state_copy(&*state))?;
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
    let name = ast_expr_as_identifier(root);
    let f: *mut AstFunction<'static> = (*state_getext(s)).get_func(name.as_ptr());
    if f.is_null() {
        return Err(Error::new(format!("function `{name}' not found")));
    }
    let v = ast_function_absexec(&*f, s)?;
    if !v.is_null() {
        return Ok(v);
    }
    call_arbitraryresult(expr, &*f, s)
}

unsafe fn call_arbitraryresult(
    _expr: &AstExpr,
    f: &AstFunction,
    state: *mut State,
) -> Result<*mut Value> {
    let res = call_to_computed_value(f, state)?;
    assert!(!res.is_null());
    Ok(res)
}

unsafe fn call_to_computed_value(f: &AstFunction, s: *mut State) -> Result<*mut Value> {
    let root = f.name();
    let uncomputed_params = f.params();
    let nparams = uncomputed_params.len();
    let mut computed_params = Vec::with_capacity(nparams);
    for p in uncomputed_params {
        let param = ast_expr_identifier_create(OwningCStr::copy_char_ptr(ast_variable_name(p)));
        let v = ast_expr_eval(&param, s)?;
        drop(param);
        assert!(!v.is_null());
        computed_params.push(if value_islocation(&*v) {
            ast_expr_identifier_create(value_str(&*v))
        } else {
            value_to_expr(&*v)
        });
    }
    Ok(value_sync_create(ast_expr_call_create(
        ast_expr_identifier_create(OwningCStr::copy_char_ptr(root)),
        computed_params,
    )))
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
    params: &[Box<AstVariable>],
    args: Vec<Result<*mut Value>>,
    fname: *mut libc::c_char,
    state: *mut State,
) -> Result<()> {
    assert_eq!(params.len(), args.len());
    for (param, res) in params.iter().zip(args) {
        state_declare(state, param, true);

        let arg = res?;
        if arg.is_null() {
            return Err(Error::new(format!(
                "parameter `{}' of function `{}' has no value",
                cstr!(ast_variable_name(param)),
                cstr!(fname),
            )));
        }
        let name = ast_expr_identifier_create(OwningCStr::copy_char_ptr(ast_variable_name(param)));
        let lval_lval = ast_expr_lvalue(&name, state)?;
        let obj: *mut Object = lvalue_object(&lval_lval);
        drop(name);
        object_assign(obj, value_copy(&*arg));
    }
    Ok(())
}

unsafe fn isdereferencable_absexec(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let p = state_getprops(&mut *state);
    // XXX FIXME: This definitely isn't OK. absexec has some weird stuff going on in the original.
    // Could clone here instead.
    p.install(Box::from_raw(expr as *const AstExpr as *mut AstExpr));
    Ok(ptr::null_mut())
}

pub unsafe fn prepare_arguments(
    args: &[Box<AstExpr>],
    params: &[Box<AstVariable>],
    state: *mut State,
) -> Vec<Result<*mut Value>> {
    assert_eq!(args.len(), params.len());
    args.iter().map(|arg| ast_expr_eval(arg, state)).collect()
}

unsafe fn assign_absexec(expr: &AstExpr, state: *mut State) -> Result<*mut Value> {
    let lval = ast_expr_assignment_lval(expr);
    let rval = ast_expr_assignment_rval(expr);
    let val = ast_expr_absexec(rval, state)?;
    if val.is_null() {
        debug_assert!(false);
        return Err(Error::new("undefined indirection (rvalue)".to_string()));
    }
    let lval_res = ast_expr_lvalue(lval, state)?;
    let obj: *mut Object = lvalue_object(&lval_res);
    if obj.is_null() {
        return Err(Error::new("undefined indirection (lvalue)".to_string()));
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
        return Err(Error::new("must be lvalue".to_string()));
    }
    if state_isalloc(param_state, param) as libc::c_int != 0 && !state_isalloc(arg_state, arg) {
        return Err(Error::new("must be heap allocated".to_string()));
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
        return Err(Error::new("must be rvalue".to_string()));
    }
    verify_paramspec(
        object_as_value(param_obj),
        object_as_value(arg_obj),
        param_state,
        arg_state,
    )
}

unsafe fn call_setupverify(f: *mut AstFunction, arg_state: *mut State) -> Result<()> {
    let fname = (*f).name();
    let mut param_state = state_create(dynamic_str(fname), state_getext(arg_state), (*f).rtype());
    ast_function_initparams(&*f, &mut param_state)?;
    let params = (*f).params();
    for p in params {
        let id = ast_variable_name(p);
        let param_0: *mut Value = state_getloc(&mut param_state, id);
        let arg: *mut Value = state_getloc(arg_state, id);
        if let Err(err) = verify_paramspec(param_0, arg, &mut param_state, arg_state) {
            return Err(Error::new(format!(
                "parameter {} of {} {}",
                cstr!(id),
                cstr!(fname),
                err.msg
            )));
        }
    }
    // Note: Original leaks the state.
    Ok(())
}

pub fn ast_expr_bracketed_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Bracketed(inner) = &expr.kind else {
        panic!()
    };
    inner
}

pub fn ast_expr_bracketed_create(root: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Bracketed(root))
}

pub fn ast_expr_literal_create(s: OwningCStr) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::StringLiteral(s))
}

pub fn ast_expr_as_literal(expr: &AstExpr) -> &OwningCStr {
    let AstExprKind::StringLiteral(s) = &expr.kind else {
        panic!()
    };
    s
}

pub fn ast_expr_as_constant(expr: &AstExpr) -> libc::c_int {
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
    Ok(value_pf_augment(v, value_as_sync(&*res_val)))
}

pub fn ast_expr_constant_create_char(c: libc::c_char) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Constant(ConstantExpr {
        ischar: true,
        constant: c as libc::c_int,
    }))
}

pub fn ast_expr_constant_create(k: libc::c_int) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Constant(ConstantExpr {
        ischar: false,
        constant: k,
    }))
}

pub fn ast_expr_as_identifier(expr: &AstExpr) -> &OwningCStr {
    let AstExprKind::Identifier(id) = &expr.kind else {
        panic!()
    };
    id
}

fn ast_expr_create(kind: AstExprKind) -> Box<AstExpr> {
    Box::new(AstExpr { kind })
}

pub unsafe fn ast_expr_destroy(expr: *mut AstExpr) {
    drop(Box::from_raw(expr));
}

pub fn ast_expr_identifier_create(s: OwningCStr) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Identifier(s))
}

unsafe fn unary_pf_reduce(e: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let res_val = ast_expr_pf_reduce(ast_expr_unary_operand(e), s)?;
    assert!(!res_val.is_null());
    Ok(value_sync_create(ast_expr_unary_create(
        value_into_sync(res_val),
        ast_expr_unary_op(e),
    )))
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
    Ok(value_sync_create(ast_expr_binary_create(
        value_to_expr(&*v1),
        op,
        value_to_expr(&*v2),
    )))
}

unsafe fn call_pf_reduce(e: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let root = ast_expr_as_identifier(ast_expr_call_root(e));
    let unreduced_args = ast_expr_call_args(e);
    let mut reduced_args = Vec::with_capacity(unreduced_args.len());
    for arg in unreduced_args {
        let val = ast_expr_pf_reduce(arg, s)?;
        assert!(!val.is_null());
        reduced_args.push(value_to_expr(&*val));
    }
    Ok(value_sync_create(ast_expr_call_create(
        ast_expr_identifier_create(root.clone()),
        reduced_args,
    )))
}

unsafe fn structmember_pf_reduce(expr: &AstExpr, s: *mut State) -> Result<*mut Value> {
    let v = ast_expr_pf_reduce(ast_expr_member_root(expr), s)?;
    assert!(!v.is_null());
    let field = ast_expr_member_field(expr);
    if value_isstruct(&*v) {
        let obj: *mut Object = value_struct_member(v, field.as_ptr());
        let obj_value: *mut Value = object_as_value(obj);
        if obj_value.is_null() {
            panic!();
        }
        return Ok(value_copy(&*obj_value));
    }
    if !value_issync(&*v) {
        panic!();
    }
    Ok(value_sync_create(ast_expr_member_create(
        value_into_sync(v),
        field.clone(),
    )))
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

fn ast_expr_member_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::StructMember(member) = &expr.kind else {
        panic!()
    };
    let root = &member.root;
    let field_name = member.field.as_str();
    if matches!(root.kind, AstExprKind::Unary(_)) {
        return ast_expr_member_deref_str_build(root, field_name, b);
    }
    strbuilder_write!(*b, "{root}.{field_name}");
}

fn ast_expr_member_deref_str_build(root: &AstExpr, member: &str, b: &mut StrBuilder) {
    if !(ast_expr_unary_op(root) == AstUnaryOp::Dereference) {
        panic!();
    }
    let inner = ast_expr_unary_operand(root);
    let e1 = ast_expr_binary_e1(inner);
    let e2 = ast_expr_binary_e2(inner);
    if matches!(e2.kind, AstExprKind::Constant(_)) && ast_expr_as_constant(e2) == 0 as libc::c_int {
        strbuilder_write!(*b, "{e1}->{member}");
    } else {
        strbuilder_write!(*b, "{e1}[{e2}].{member}");
    }
}

fn ast_expr_incdec_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::IncDec(incdec) = &expr.kind else {
        panic!()
    };
    let root = &incdec.operand;
    let op = if incdec.inc != 0 { "++" } else { "--" };
    if incdec.pre != 0 {
        strbuilder_write!(*b, "{op}{root}");
    } else {
        strbuilder_write!(*b, "{root}{op}");
    }
}

fn ast_expr_call_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    strbuilder_write!(*b, "{}(", call.fun);
    for (i, arg) in call.args.iter().enumerate() {
        strbuilder_write!(
            *b,
            "{arg}{}",
            if i + 1 < call.args.len() { ", " } else { "" },
        );
    }
    strbuilder_write!(*b, ")");
}

pub fn ast_expr_inverted_copy(expr: &AstExpr, invert: bool) -> Box<AstExpr> {
    let copy = ast_expr_copy(expr);
    if invert {
        ast_expr_unary_create(copy, AstUnaryOp::Bang)
    } else {
        copy
    }
}

pub fn ast_expr_member_field(expr: &AstExpr) -> &OwningCStr {
    let AstExprKind::StructMember(member) = &expr.kind else {
        panic!()
    };
    &member.field
}

pub fn ast_expr_member_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::StructMember(member) = &expr.kind else {
        panic!()
    };
    &member.root
}

pub fn ast_expr_incdec_pre(expr: &AstExpr) -> bool {
    match &expr.kind {
        AstExprKind::IncDec(incdec) => incdec.pre != 0,
        _ => panic!(),
    }
}

pub fn ast_expr_incdec_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IncDec(incdec) = &expr.kind else {
        panic!()
    };
    &incdec.operand
}

pub fn ast_expr_member_create(struct_: Box<AstExpr>, field: OwningCStr) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::StructMember(StructMemberExpr {
        root: struct_,
        field,
    }))
}

pub fn ast_expr_binary_create(e1: Box<AstExpr>, op: AstBinaryOp, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Binary(BinaryExpr { e1, op, e2 }))
}

pub fn ast_expr_unary_create(root: Box<AstExpr>, op: AstUnaryOp) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Unary(UnaryExpr { op, arg: root }))
}

pub fn ast_expr_incdec_to_assignment(expr: &AstExpr) -> Box<AstExpr> {
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

pub fn ast_expr_incdec_create(root: Box<AstExpr>, inc: bool, pre: bool) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::IncDec(IncDecExpr {
        operand: root,
        inc: inc as libc::c_int,
        pre: pre as libc::c_int,
    }))
}

pub fn ast_expr_call_args(expr: &AstExpr) -> &[Box<AstExpr>] {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    &call.args
}

pub fn ast_expr_call_root(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    &call.fun
}

pub fn ast_expr_call_create(fun: Box<AstExpr>, args: Vec<Box<AstExpr>>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Call(CallExpr { fun, args }))
}

pub fn ast_expr_iteration_create() -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Iteration)
}

pub fn ast_expr_alloc_kind(expr: &AstExpr) -> AstAllocKind {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    alloc.kind
}

pub fn ast_expr_alloc_arg(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    &alloc.arg
}

pub fn ast_expr_isisdereferencable(expr: &AstExpr) -> bool {
    matches!(expr.kind, AstExprKind::IsDereferencable(_))
}

pub fn ast_expr_dealloc_create(arg: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Dealloc,
        arg,
    }))
}

pub fn ast_expr_str(expr: &AstExpr) -> OwningCStr {
    let mut b = strbuilder_create();
    match &expr.kind {
        AstExprKind::Identifier(id) => {
            strbuilder_write!(b, "{id}");
        }
        AstExprKind::Constant(_) => {
            ast_expr_constant_str_build(expr, &mut b);
        }
        AstExprKind::StringLiteral(s) => {
            strbuilder_write!(b, "{s:?}");
        }
        AstExprKind::Bracketed(inner) => {
            ast_expr_bracketed_str_build(inner, &mut b);
        }
        AstExprKind::Call(_) => {
            ast_expr_call_str_build(expr, &mut b);
        }
        AstExprKind::IncDec(_) => {
            ast_expr_incdec_str_build(expr, &mut b);
        }
        AstExprKind::StructMember(_) => {
            ast_expr_member_str_build(expr, &mut b);
        }
        AstExprKind::Unary(_) => {
            ast_expr_unary_str_build(expr, &mut b);
        }
        AstExprKind::Binary(_) => {
            ast_expr_binary_str_build(expr, &mut b);
        }
        AstExprKind::Assignment(_) => {
            ast_expr_assignment_str_build(expr, &mut b);
        }
        AstExprKind::IsDeallocand(assertand) => {
            ast_expr_isdeallocand_str_build(assertand, &mut b);
        }
        AstExprKind::IsDereferencable(assertand) => {
            ast_expr_isdereferencable_str_build(assertand, &mut b);
        }
        AstExprKind::ArbArg => {
            strbuilder_putc(&mut b, '$' as i32 as libc::c_char);
        }
        AstExprKind::Allocation(_) => {
            ast_expr_alloc_str_build(expr, &mut b);
        }
        _ => {
            panic!();
        }
    }
    strbuilder_build(b)
}

fn ast_expr_alloc_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    let arg = &*alloc.arg;
    match alloc.kind {
        AstAllocKind::Alloc => {
            strbuilder_write!(*b, ".{} {arg};", "malloc");
        }
        AstAllocKind::Dealloc => {
            strbuilder_write!(*b, ".{} {arg};", "free");
        }
        AstAllocKind::Clump => {
            strbuilder_write!(*b, ".{} {arg};", "clump");
        }
    }
}

pub fn ast_expr_alloc_create(arg: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Alloc,
        arg,
    }))
}

pub fn ast_expr_isdereferencable_assertand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IsDereferencable(assertand) = &expr.kind else {
        panic!()
    };
    assertand
}

pub fn ast_expr_isdeallocand_assertand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IsDeallocand(assertand) = &expr.kind else {
        panic!()
    };
    assertand
}

fn ast_expr_alloc_copy(expr: &AstExpr) -> Box<AstExpr> {
    let AstExprKind::Allocation(alloc) = &expr.kind else {
        panic!()
    };
    let arg = ast_expr_copy(&alloc.arg);
    match alloc.kind {
        AstAllocKind::Alloc => ast_expr_alloc_create(arg),
        AstAllocKind::Dealloc => ast_expr_dealloc_create(arg),
        AstAllocKind::Clump => ast_expr_clump_create(arg),
    }
}

fn ast_expr_isdereferencable_str_build(assertand: &AstExpr, b: &mut StrBuilder) {
    strbuilder_write!(*b, "${assertand}");
}

pub fn ast_expr_assignment_rval(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!()
    };
    &assignment.rval
}

pub fn ast_expr_copy(expr: &AstExpr) -> Box<AstExpr> {
    Box::new(expr.clone())
}

pub fn ast_expr_assignment_lval(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!()
    };
    &assignment.lval
}

pub fn ast_expr_binary_e2(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    &binary.e2
}

fn ast_expr_isdeallocand_str_build(assertand: &AstExpr, b: &mut StrBuilder) {
    strbuilder_write!(*b, "@{assertand}");
}

fn ast_expr_assignment_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!()
    };
    strbuilder_write!(*b, "{} = {}", assignment.lval, assignment.rval);
}

fn ast_expr_binary_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    let opstr = match binary.op {
        AstBinaryOp::Eq => "==",
        AstBinaryOp::Ne => "!=",
        AstBinaryOp::Lt => "<",
        AstBinaryOp::Gt => ">",
        AstBinaryOp::Le => "<=",
        AstBinaryOp::Ge => ">=",
        AstBinaryOp::Addition => "+",
        AstBinaryOp::Subtraction => "-",
    };
    let e1 = &binary.e1;
    let e2 = &binary.e2;
    strbuilder_write!(*b, "{e1}{opstr}{e2}");
}

pub fn ast_expr_binary_e1(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    &binary.e1
}

pub fn ast_expr_difference_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Subtraction, e2)
}

pub fn ast_expr_sum_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Addition, e2)
}

fn ast_expr_unary_str_build(expr: &AstExpr, b: &mut StrBuilder) {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!()
    };
    let c = match unary.op {
        AstUnaryOp::Address => "&",
        AstUnaryOp::Dereference => "*",
        AstUnaryOp::Positive => "+",
        AstUnaryOp::Negative => "-",
        AstUnaryOp::OnesComplement => "~",
        AstUnaryOp::Bang => "!",
    };
    let root = &unary.arg;
    strbuilder_write!(*b, "{c}({root})");
}

pub fn ast_expr_ge_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Ge, e2)
}

pub fn ast_expr_binary_op(expr: &AstExpr) -> AstBinaryOp {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!()
    };
    binary.op
}

pub fn ast_expr_le_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Le, e2)
}

pub fn ast_expr_clump_create(arg: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_create(AstExprKind::Allocation(AllocExpr {
        kind: AstAllocKind::Clump,
        arg,
    }))
}

pub fn ast_expr_gt_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Gt, e2)
}

pub fn ast_expr_lt_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Lt, e2)
}

pub fn ast_expr_ne_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Ne, e2)
}

pub fn ast_expr_eq_create(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
    ast_expr_binary_create(e1, AstBinaryOp::Eq, e2)
}

pub fn ast_expr_unary_operand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!()
    };
    &unary.arg
}

pub fn ast_expr_unary_isdereference(expr: &AstExpr) -> bool {
    assert!(matches!(&expr.kind, AstExprKind::Unary(_)));
    ast_expr_unary_op(expr) == AstUnaryOp::Dereference
}

pub fn ast_expr_unary_op(expr: &AstExpr) -> AstUnaryOp {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!()
    };
    unary.op
}

pub fn ast_expr_getfuncs(expr: &AstExpr) -> Vec<OwningCStr> {
    match &expr.kind {
        AstExprKind::Identifier(_)
        | AstExprKind::Constant(_)
        | AstExprKind::StringLiteral(_)
        | AstExprKind::StructMember(_)
        | AstExprKind::IsDeallocand(_)
        | AstExprKind::IsDereferencable(_)
        | AstExprKind::ArbArg => vec![],
        AstExprKind::Call(_) => ast_expr_call_getfuncs(expr),
        AstExprKind::Bracketed(inner) => ast_expr_getfuncs(inner),
        AstExprKind::IncDec(incdec) => ast_expr_getfuncs(&incdec.operand),
        AstExprKind::Unary(unary) => ast_expr_getfuncs(&unary.arg),
        AstExprKind::Assignment(assignment) => [
            ast_expr_getfuncs(&assignment.lval),
            ast_expr_getfuncs(&assignment.rval),
        ]
        .concat(),
        AstExprKind::Binary(binary) => {
            [ast_expr_getfuncs(&binary.e1), ast_expr_getfuncs(&binary.e2)].concat()
        }
        _ => panic!("invalid expr kind"),
    }
}

pub unsafe fn ast_expr_splits(e: &AstExpr, s: *mut State) -> Result<AstStmtSplits> {
    match &e.kind {
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
        | AstExprKind::Allocation(_) => Ok(AstStmtSplits { conds: vec![] }),
        _ => panic!(),
    }
}

unsafe fn call_splits(expr: &AstExpr, state: *mut State) -> Result<AstStmtSplits> {
    let root = ast_expr_call_root(expr);
    let name = ast_expr_as_identifier(root);
    let f: *mut AstFunction = (*state_getext(state)).get_func(name.as_ptr());
    if f.is_null() {
        return Err(Error::new(format!("function: `{name}' not found")));
    }
    let params = (*f).params();
    let mut s_copy = state_copy(&*state);
    let args = prepare_arguments(ast_expr_call_args(expr), params, &mut s_copy);
    let ret_type = (*f).rtype();
    state_pushframe(&mut s_copy, dynamic_str(name.as_ptr()), ret_type);
    prepare_parameters(params, args, name.as_ptr(), &mut s_copy)?;
    let mut conds = vec![];
    let abs = (*f).abstract_block();
    for var in &abs.decls {
        state_declare(&mut s_copy, var, false);
    }
    for stmt in &abs.stmts {
        // Note: errors ignored in the original!
        let mut splits = ast_stmt_splits(stmt, &mut s_copy).unwrap_or_default();
        conds.append(&mut splits.conds);
    }
    state_popframe(&mut s_copy);
    // Note: Original leaks s_copy
    Ok(AstStmtSplits { conds })
}

unsafe fn binary_splits(e: &AstExpr, s: *mut State) -> Result<AstStmtSplits> {
    // Note: s1.err and s2.err are ignored in the original.
    let s1 = ast_expr_splits(ast_expr_binary_e1(e), s).unwrap_or_default();
    let s2 = ast_expr_splits(ast_expr_binary_e2(e), s).unwrap_or_default();
    Ok(AstStmtSplits {
        conds: [s1.conds, s2.conds].concat(),
    })
}

fn ast_expr_call_getfuncs(expr: &AstExpr) -> Vec<OwningCStr> {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!()
    };
    let mut res = vec![];
    let AstExprKind::Identifier(id) = &call.fun.kind else {
        panic!()
    };
    res.push(id.clone());
    for arg in &call.args {
        res.append(&mut ast_expr_getfuncs(arg));
    }
    res
}

unsafe fn calculate_indegrees(g: &FuncGraph) -> InsertionOrderMap<OwningCStr, libc::c_int> {
    let mut indegrees = InsertionOrderMap::new();
    for (key, deps) in g {
        if indegrees.get(CStr::from_ptr(key.as_ptr())).is_none() {
            indegrees.insert(OwningCStr::copy(key), 0);
            for &dep_key in deps {
                if indegrees.get(CStr::from_ptr(dep_key)).is_none() {
                    indegrees.insert(OwningCStr::new(dynamic_str(dep_key)), 0);
                }
            }
        }
    }
    for (key, count) in &mut indegrees {
        if let Some(n_arr) = g.get(CStr::from_ptr(key.as_ptr())) {
            *count += n_arr.len() as libc::c_int;
        }
    }
    indegrees
}

fn build_indegree_zero(
    indegrees: &InsertionOrderMap<OwningCStr, libc::c_int>,
) -> VecDeque<OwningCStr> {
    let mut indegree_zero = VecDeque::new();
    for (key, val) in indegrees {
        if *val == 0 {
            indegree_zero.push_back(key.clone());
        }
    }
    indegree_zero
}

pub unsafe fn topological_order(fname: &CStr, ext: &Externals) -> Vec<OwningCStr> {
    let mut order = vec![];
    let g = ast_function_buildgraph(fname, ext);
    // Note: Original leaks indegree_zero.
    let mut indegrees = calculate_indegrees(&g);
    let mut indegree_zero = build_indegree_zero(&indegrees);
    while let Some(curr) = indegree_zero.pop_front() {
        order.push(curr.clone());
        for (key, v) in &g {
            if string_arr_contains(v, curr.as_ptr()) {
                let count = indegrees.get_mut(key.as_c_str()).unwrap();
                *count -= 1;
                if *count == 0 {
                    indegree_zero.push_back(OwningCStr::copy(key.as_c_str()));
                }
            }
        }
    }
    if order.len() != indegrees.len() as usize {
        eprintln!("cycle detected in graph");
        process::exit(1);
    }
    order
}

pub fn ast_block_create(decls: Vec<Box<AstVariable>>, stmts: Vec<Box<AstStmt>>) -> Box<AstBlock> {
    Box::new(AstBlock { decls, stmts })
}

pub fn ast_block_str(b: &AstBlock, indent: &str) -> OwningCStr {
    let mut sb = strbuilder_create();
    for decl in &b.decls {
        let s = ast_variable_str(decl);
        strbuilder_write!(sb, "{indent}{s};\n");
    }
    for stmt in &b.stmts {
        let s = ast_stmt_str(stmt);
        strbuilder_write!(sb, "{indent}{s}\n");
    }
    strbuilder_build(sb)
}

pub fn ast_block_ndecls(b: &AstBlock) -> libc::c_int {
    b.decls.len() as libc::c_int
}

pub fn ast_block_decls(b: &AstBlock) -> &[Box<AstVariable>] {
    &b.decls
}

pub fn ast_block_nstmts(b: &AstBlock) -> libc::c_int {
    b.stmts.len() as libc::c_int
}

pub fn ast_block_stmts(b: &AstBlock) -> &[Box<AstStmt>] {
    &b.stmts
}

pub unsafe fn ast_block_isterminal(b: &AstBlock, s: *mut State) -> bool {
    b.stmts.iter().any(|stmt| ast_stmt_isterminal(stmt, s))
}

pub fn ast_block_preconds(b: &AstBlock) -> Result<Option<&AstStmt>> {
    for stmt in &b.stmts {
        if ast_stmt_ispre(stmt) {
            let preconds = ast_stmt_labelled_stmt(stmt);
            ast_stmt_preconds_validate(preconds)?;
            return Ok(Some(preconds));
        }
    }
    Ok(None)
}

pub unsafe fn ast_stmt_process(
    stmt: &AstStmt,
    fname: *mut libc::c_char,
    state: *mut State,
) -> Result<()> {
    if matches!(stmt.kind, AstStmtKind::CompoundV(_)) {
        if let Err(err) = ast_stmt_verify(stmt, state) {
            let loc = ast_stmt_lexememarker(stmt);
            return Err(Error::new(format!("{loc}: {}", err.msg)));
        }
    }
    if let Err(err) = ast_stmt_exec(stmt, state) {
        let loc = ast_stmt_lexememarker(stmt);
        let err_msg = format!("{loc}:{}: cannot exec statement: {}", cstr!(fname), err.msg,);
        return Err(Error::new(err_msg));
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

pub fn ast_stmt_labelled_stmt(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!()
    };
    &labelled.stmt
}

unsafe fn labelled_absexec(
    stmt: &AstStmt,
    state: *mut State,
    should_setup: bool,
) -> Result<*mut Value> {
    if !ast_stmt_ispre(stmt) {
        let s = ast_stmt_str(stmt);
        panic!("expected precondition, got: {s}");
    }
    let setup = ast_stmt_labelled_stmt(stmt);
    if !should_setup {
        return Ok(ptr::null_mut());
    }
    ast_stmt_absexec(setup, state, should_setup)
}

pub fn ast_stmt_copy(stmt: &AstStmt) -> Box<AstStmt> {
    Box::new(stmt.clone())
}

unsafe fn labelled_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    // Note: original leaks this value; we drop it instead.
    let _ = ast_stmt_absexec(stmt, state, true)?;
    Ok(())
}

unsafe fn sel_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return stmt_setupabsexec(ast_stmt_sel_body(stmt), state);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

unsafe fn comp_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    if ast_block_ndecls(b) != 0 {
        panic!();
    }
    for stmt in &b.stmts {
        if ast_stmt_ispre(stmt) {
            stmt_setupabsexec(stmt, state)?;
            if ast_stmt_isterminal(stmt, state) {
                break;
            }
        }
    }
    Ok(())
}

unsafe fn stmt_setupabsexec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    match &stmt.kind {
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

pub fn ast_stmt_isselection(stmt: &AstStmt) -> bool {
    matches!(stmt.kind, AstStmtKind::Selection(_))
}

pub fn ast_stmt_isassume(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    labelled.label.as_str() == "assume"
}

unsafe fn stmt_installprop(stmt: &AstStmt, state: *mut State) -> Result<Preresult> {
    ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state)
}

pub fn ast_stmt_ispre(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    labelled.label.as_str() == "setup"
}

unsafe fn stmt_v_block_verify(v_block_stmt: &AstStmt, state: *mut State) -> Result<()> {
    let b = ast_stmt_as_v_block(v_block_stmt);
    assert_eq!(ast_block_ndecls(b), 0);
    for stmt in &b.stmts {
        ast_stmt_verify(stmt, state)?;
    }
    Ok(())
}

unsafe fn stmt_expr_verify(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let expr = ast_stmt_as_expr(stmt);
    if ast_expr_decide(expr, state) {
        Ok(())
    } else {
        Err(Error::new("cannot verify statement".to_string()))
    }
}

unsafe fn iter_empty(stmt: &AstStmt, state: *mut State) -> bool {
    if let Err(err) = ast_stmt_exec(ast_stmt_iter_init(stmt), state) {
        panic!("{err}");
    }
    !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state)
}

unsafe fn stmt_iter_verify(stmt: &AstStmt, state: *mut State) -> Result<()> {
    if iter_empty(stmt, state) {
        return Ok(());
    }
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!(body.kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_ndecls(block), 0);
    assert_eq!(block.stmts.len(), 1);
    let assertion = ast_stmt_as_expr(&block.stmts[0]);
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    if !ast_expr_rangedecide(assertion, lw, up, state) {
        return Err(Error::new("could not verify".to_string()));
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
    assert_eq!(ast_block_ndecls(b), 0);
    for stmt in &b.stmts {
        ast_stmt_exec(stmt, state)?;
        if ast_stmt_isterminal(stmt, state) {
            break;
        }
    }
    Ok(())
}

unsafe fn stmt_sel_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return ast_stmt_exec(ast_stmt_sel_body(stmt), state);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

fn iter_neteffect(iter: &AstStmt) -> Option<Box<AstStmt>> {
    let abs = ast_stmt_iter_abstract(iter);
    let nstmts: libc::c_int = ast_block_nstmts(abs);
    if nstmts == 0 {
        return None;
    }
    if !(ast_block_ndecls(abs) == 0 as libc::c_int && nstmts == 1 as libc::c_int) {
        panic!();
    }
    // Note: Original passes NULL lexeme marker to these two constructors. In the Rust version, the
    // lexeme marker isn't nullable. It isn't worth warping the universe for this hack, so we dig
    // up with some phony locations.
    Some(ast_stmt_create_iter(
        Box::new(ast_stmt_lexememarker(iter).clone()),
        ast_stmt_copy(ast_stmt_iter_init(iter)),
        ast_stmt_copy(ast_stmt_iter_cond(iter)),
        ast_expr_copy(ast_stmt_iter_iter(iter)),
        ast_block_create(vec![], vec![]),
        ast_stmt_create_compound(
            Box::new(ast_stmt_lexememarker(ast_stmt_iter_body(iter)).clone()),
            Box::new(ast_stmt_iter_abstract(iter).clone()),
        ),
        false,
    ))
}

unsafe fn stmt_iter_exec(stmt: &AstStmt, state: *mut State) -> Result<()> {
    if let Some(neteffect) = iter_neteffect(stmt) {
        ast_stmt_absexec(&neteffect, state, true)?;
    }
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
        AstStmtKind::Labelled(labelled) => ast_stmt_exec(&labelled.stmt, state),
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

fn ast_stmt_jump_sprint(jump: &AstJumpStmt, b: &mut StrBuilder) {
    // Note: jump.rv can be null. Error in the original.
    let rv = jump.rv.as_ref().unwrap();
    strbuilder_write!(*b, "return {rv};\n");
}

pub fn ast_stmt_iter_abstract(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &iteration.abstract_
}

pub fn ast_stmt_iter_iter(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &iteration.iter
}

pub fn ast_stmt_lexememarker(stmt: &AstStmt) -> &LexemeMarker {
    &stmt.loc
}

fn ast_stmt_iter_sprint(iteration: &AstIterationStmt, b: &mut StrBuilder) {
    let init = ast_stmt_str(&iteration.init);
    let cond = ast_stmt_str(&iteration.cond);
    let body = ast_stmt_str(&iteration.body);
    let iter = &iteration.iter;
    let abs = ast_block_str(&iteration.abstract_, "\t");
    strbuilder_write!(*b, "for ({init} {cond} {iter}) [{abs}] {{ {body} }}");
}

pub fn ast_stmt_str(stmt: &AstStmt) -> OwningCStr {
    let mut b = strbuilder_create();
    match &stmt.kind {
        AstStmtKind::Labelled(_) => {
            ast_stmt_labelled_sprint(stmt, &mut b);
        }
        AstStmtKind::Nop => {
            ast_stmt_nop_sprint(&mut b);
        }
        AstStmtKind::Expr(expr) => {
            ast_stmt_expr_sprint(expr, &mut b);
        }
        AstStmtKind::Compound(compound) => {
            ast_stmt_compound_sprint(compound, &mut b);
        }
        AstStmtKind::CompoundV(compound) => {
            ast_stmt_compound_sprint(compound, &mut b);
        }
        AstStmtKind::Selection(_) => {
            ast_stmt_sel_sprint(stmt, &mut b);
        }
        AstStmtKind::Iteration(iteration) => {
            ast_stmt_iter_sprint(iteration, &mut b);
        }
        AstStmtKind::IterationE(iteration) => {
            ast_stmt_iter_sprint(iteration, &mut b);
        }
        AstStmtKind::Jump(jump) => {
            ast_stmt_jump_sprint(jump, &mut b);
        }
        _ => {
            panic!();
        }
    }
    strbuilder_build(b)
}

fn ast_expr_copy_ifnotnull(expr: &Option<Box<AstExpr>>) -> Option<Box<AstExpr>> {
    expr.as_ref().map(|expr| ast_expr_copy(expr))
}

pub fn ast_stmt_iter_cond(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &iteration.cond
}

pub fn ast_stmt_iter_init(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!()
    };
    &iteration.init
}

pub fn ast_stmt_labelled_label(stmt: &AstStmt) -> &OwningCStr {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!()
    };
    &labelled.label
}

unsafe fn sel_isterminal(stmt: &AstStmt, s: *mut State) -> bool {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), s).unwrap();
    if decision {
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
    for stmt in &b.stmts {
        // Note: original leaks these values.
        let _ = ast_stmt_absexec(stmt, state, should_setup)?;
    }
    Ok(ptr::null_mut())
}

pub fn ast_stmt_as_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Compound(block) = &stmt.kind else {
        panic!();
    };
    block
}

pub fn ast_stmt_jump_rv(stmt: &AstStmt) -> Option<&AstExpr> {
    let AstStmtKind::Jump(jump) = &stmt.kind else {
        panic!()
    };
    jump.rv.as_deref()
}

unsafe fn jump_absexec(stmt: &AstStmt, state: *mut State) -> Result<*mut Value> {
    // Note: Original leaks the expression to avoid a double free.
    let expr = ast_expr_assignment_create(
        ast_expr_identifier_create(OwningCStr::copy_str("return")),
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

fn ast_stmt_sel_sprint(stmt: &AstStmt, b: &mut StrBuilder) {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    let cond = &selection.cond;
    let body = ast_stmt_str(&selection.body);
    strbuilder_write!(*b, "if ({cond}) {{ {body} }}");
    if let Some(nest_stmt) = &selection.nest {
        let nest = ast_stmt_str(nest_stmt);
        strbuilder_write!(*b, " else {nest}");
    }
}

pub unsafe fn ast_stmt_isterminal(stmt: &AstStmt, s: *mut State) -> bool {
    match &stmt.kind {
        AstStmtKind::Jump(jump) => jump.kind == AstJumpKind::Return,
        AstStmtKind::Compound(block) => ast_block_isterminal(block, s),
        AstStmtKind::Selection(_) => sel_isterminal(stmt, s),
        _ => false,
    }
}

pub fn ast_stmt_create_jump(
    loc: Box<LexemeMarker>,
    kind: AstJumpKind,
    rv: Option<Box<AstExpr>>,
) -> Box<AstStmt> {
    ast_stmt_create(loc, AstStmtKind::Jump(AstJumpStmt { kind, rv }))
}

pub unsafe fn sel_decide(control: &AstExpr, state: *mut State) -> Result<bool> {
    let v = ast_expr_pf_reduce(control, state)?;
    assert!(!v.is_null());
    if value_issync(&*v) {
        let sync = value_as_sync(&*v);
        let p = state_getprops(&mut *state);
        if p.get(sync) {
            return Ok(true);
        } else if p.contradicts(sync) {
            return Ok(false);
        }
    }
    if value_isconstant(&*v) {
        if value_as_constant(&*v) != 0 {
            return Ok(true);
        }
        return Ok(false);
    }
    let zero: *mut Value = value_int_create(0 as libc::c_int);
    if !value_isint(&*v) {
        let v_str = value_str(&*v);
        return Err(Error::new(format!(
            "`{control}' with value `{v_str}' is undecidable"
        )));
    }
    let nonzero: bool = !value_equal(&*zero, &*v);
    value_destroy(zero);
    Ok(nonzero)
}

fn ast_stmt_compound_sprint(compound: &AstBlock, b: &mut StrBuilder) {
    let s = ast_block_str(compound, "\t");
    strbuilder_write!(*b, "{s}");
}

fn ast_stmt_expr_sprint(expr: &AstExpr, b: &mut StrBuilder) {
    strbuilder_write!(*b, "{expr};");
}

fn ast_stmt_create(loc: Box<LexemeMarker>, kind: AstStmtKind) -> Box<AstStmt> {
    Box::new(AstStmt { kind, loc })
}

pub fn ast_stmt_create_iter(
    loc: Box<LexemeMarker>,
    init: Box<AstStmt>,
    cond: Box<AstStmt>,
    iter: Box<AstExpr>,
    abstract_: Box<AstBlock>,
    body: Box<AstStmt>,
    as_iteration_e: bool,
) -> Box<AstStmt> {
    let iter = AstIterationStmt {
        init,
        cond,
        iter,
        body,
        abstract_,
    };
    ast_stmt_create(
        loc,
        if as_iteration_e {
            AstStmtKind::IterationE(iter)
        } else {
            AstStmtKind::Iteration(iter)
        },
    )
}

fn ast_stmt_nop_sprint(b: &mut StrBuilder) {
    strbuilder_write!(*b, ";");
}

pub fn ast_stmt_iter_body(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.body
}

pub fn ast_stmt_create_sel(
    loc: Box<LexemeMarker>,
    isswitch: bool,
    cond: Box<AstExpr>,
    body: Box<AstStmt>,
    nest: Option<Box<AstStmt>>,
) -> Box<AstStmt> {
    if isswitch {
        panic!();
    }
    ast_stmt_create(
        loc,
        AstStmtKind::Selection(AstSelectionStmt {
            isswitch,
            cond,
            body,
            nest,
        }),
    )
}

pub fn ast_stmt_create_compound_v(loc: Box<LexemeMarker>, b: Box<AstBlock>) -> Box<AstStmt> {
    ast_stmt_create(loc, AstStmtKind::CompoundV(b))
}

fn hack_alloc_from_neteffect(stmt: &AstStmt) -> &AstExpr {
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!(body.kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_ndecls(block), 0);
    assert_eq!(block.stmts.len(), 1);
    ast_stmt_as_expr(&block.stmts[0])
}

pub fn ast_stmt_iter_lower_bound(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    let AstStmtKind::Expr(expr) = &iteration.init.kind else {
        panic!();
    };
    ast_expr_assignment_rval(expr)
}

fn ast_stmt_labelled_sprint(stmt: &AstStmt, b: &mut StrBuilder) {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!();
    };
    let s = ast_stmt_str(&labelled.stmt);
    strbuilder_write!(*b, "{}: {s}", labelled.label);
}

unsafe fn sel_absexec(stmt: &AstStmt, state: *mut State, should_setup: bool) -> Result<*mut Value> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(ptr::null_mut())
}

pub fn ast_stmt_sel_nest(stmt: &AstStmt) -> Option<&AstStmt> {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    selection.nest.as_deref()
}

pub fn ast_stmt_create_labelled(
    loc: Box<LexemeMarker>,
    label: OwningCStr,
    substmt: Box<AstStmt>,
) -> Box<AstStmt> {
    ast_stmt_create(
        loc,
        AstStmtKind::Labelled(AstLabelledStmt {
            label,
            stmt: substmt,
        }),
    )
}

pub fn ast_stmt_create_nop(loc: Box<LexemeMarker>) -> Box<AstStmt> {
    ast_stmt_create(loc, AstStmtKind::Nop)
}

pub fn ast_stmt_create_expr(loc: Box<LexemeMarker>, expr: Box<AstExpr>) -> Box<AstStmt> {
    ast_stmt_create(loc, AstStmtKind::Expr(expr))
}

pub fn ast_stmt_create_compound(loc: Box<LexemeMarker>, b: Box<AstBlock>) -> Box<AstStmt> {
    ast_stmt_create(loc, AstStmtKind::Compound(b))
}

pub fn ast_stmt_sel_cond(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    &selection.cond
}

pub fn ast_stmt_sel_body(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    &selection.body
}

pub fn ast_stmt_iter_upper_bound(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    let AstStmtKind::Expr(expr) = &iteration.cond.kind else {
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

pub fn ast_stmt_as_expr(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Expr(expr) = &stmt.kind else {
        panic!();
    };
    expr
}

pub fn ast_stmt_as_v_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::CompoundV(block) = &stmt.kind else {
        panic!();
    };
    block
}

pub fn ast_stmt_getfuncs(stmt: &AstStmt) -> Vec<OwningCStr> {
    match &stmt.kind {
        AstStmtKind::Nop => vec![],
        AstStmtKind::Labelled(labelled) => ast_stmt_getfuncs(&labelled.stmt),
        AstStmtKind::Compound(block) | AstStmtKind::CompoundV(block) => {
            ast_stmt_compound_getfuncs(block)
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

pub unsafe fn ast_stmt_splits(stmt: &AstStmt, s: *mut State) -> Result<AstStmtSplits> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(AstStmtSplits { conds: vec![] }),
        AstStmtKind::Expr(expr) => ast_expr_splits(expr, s),
        AstStmtKind::Selection(selection) => stmt_sel_splits(selection, s),
        AstStmtKind::Jump(jump) => {
            if let Some(rv) = &jump.rv {
                return ast_expr_splits(rv, s);
            }
            Ok(AstStmtSplits { conds: vec![] })
        }
        AstStmtKind::Labelled(labelled) => ast_stmt_splits(&labelled.stmt, s),
        AstStmtKind::Iteration(_) | AstStmtKind::Compound(_) | AstStmtKind::CompoundV(_) => {
            Ok(AstStmtSplits { conds: vec![] })
        }
        _ => panic!(),
    }
}

unsafe fn stmt_sel_splits(selection: &AstSelectionStmt, s: *mut State) -> Result<AstStmtSplits> {
    let v = ast_expr_pf_reduce(&selection.cond, s).unwrap();
    let e = value_to_expr(&*v);
    if condexists(&e, s) || value_isconstant(&*v) {
        Ok(AstStmtSplits { conds: vec![] })
    } else {
        Ok(AstStmtSplits {
            conds: vec![Box::into_raw(e)],
        })
    }
}

unsafe fn condexists(cond: &AstExpr, s: *mut State) -> bool {
    let val = ast_expr_pf_reduce(cond, s).unwrap();
    assert!(!val.is_null());
    // Note: original doesn't free this.
    let reduced = value_to_expr(&*val);
    let p = state_getprops(&mut *s);
    p.get(&reduced) || p.contradicts(&reduced)
}

fn ast_stmt_selection_getfuncs(selection: &AstSelectionStmt) -> Vec<OwningCStr> {
    let cond_arr = ast_expr_getfuncs(&selection.cond);
    let body_arr = ast_stmt_getfuncs(&selection.body);
    let nest_arr = if let Some(nest) = &selection.nest {
        ast_stmt_getfuncs(nest)
    } else {
        vec![]
    };
    [cond_arr, body_arr, nest_arr].concat()
}

fn ast_stmt_iteration_getfuncs(iteration: &AstIterationStmt) -> Vec<OwningCStr> {
    [
        ast_stmt_getfuncs(&iteration.init),
        ast_stmt_getfuncs(&iteration.cond),
        ast_stmt_getfuncs(&iteration.body),
        ast_expr_getfuncs(&iteration.iter),
    ]
    .concat()
}

fn ast_stmt_compound_getfuncs(block: &AstBlock) -> Vec<OwningCStr> {
    let mut res = vec![];
    for stmt in &block.stmts {
        res.append(&mut ast_stmt_getfuncs(stmt));
    }
    res
}

pub fn ast_stmt_preconds_validate(stmt: &AstStmt) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Expr(_) | AstStmtKind::Allocation(_) | AstStmtKind::Iteration(_) => Ok(()),
        AstStmtKind::Selection(_) => preconds_selection_verify(stmt),
        AstStmtKind::Compound(block) => preconds_compound_verify(block),
        _ => panic!(),
    }
}
fn preconds_selection_verify(stmt: &AstStmt) -> Result<()> {
    let l = ast_stmt_lexememarker(stmt);
    Err(Error::new(format!(
        "{l} setup preconditions must be decidable",
    )))
}

fn preconds_compound_verify(block: &AstBlock) -> Result<()> {
    for stmt in &block.stmts {
        ast_stmt_preconds_validate(stmt)?;
    }
    Ok(())
}

pub fn ast_type_isint(t: &AstType) -> bool {
    matches!(t.base, AstTypeBase::Int)
}

pub fn ast_type_ispointer(t: &AstType) -> bool {
    matches!(t.base, AstTypeBase::Pointer(_))
}

pub fn ast_type_create(base: AstTypeBase, modifiers: AstTypeModifier) -> Box<AstType> {
    Box::new(AstType { base, modifiers })
}

pub fn ast_type_create_ptr(referent: Box<AstType>) -> Box<AstType> {
    ast_type_create(AstTypeBase::Pointer(Some(referent)), 0)
}

pub fn ast_type_create_voidptr() -> Box<AstType> {
    ast_type_create(AstTypeBase::Pointer(None), 0)
}

pub fn ast_type_create_arr(base: Box<AstType>, length: libc::c_int) -> Box<AstType> {
    ast_type_create(
        AstTypeBase::Array(AstArrayType {
            type_: base,
            length,
        }),
        0,
    )
}

pub fn ast_type_create_struct(
    tag: Option<OwningCStr>,
    members: Option<Box<Vec<Box<AstVariable>>>>,
) -> Box<AstType> {
    ast_type_create(AstTypeBase::Struct(AstStructType { tag, members }), 0)
}

pub fn ast_type_create_userdef(name: OwningCStr) -> Box<AstType> {
    ast_type_create(AstTypeBase::UserDefined(name), 0)
}

pub unsafe fn ast_type_vconst(
    t: &AstType,
    s: *mut State,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut Value {
    match &t.base {
        AstTypeBase::Int => value_int_indefinite_create(),
        AstTypeBase::Pointer(_) => value_ptr_indefinite_create(),
        AstTypeBase::UserDefined(name) => ast_type_vconst(
            // Note: Original does not null-check here.
            (*state_getext(s)).get_typedef(name.as_ptr()).unwrap(),
            s,
            comment,
            persist,
        ),
        AstTypeBase::Struct(_) => value_struct_indefinite_create(t, s, comment, persist),
        _ => panic!(),
    }
}

pub fn ast_type_isstruct(t: &AstType) -> bool {
    matches!(t.base, AstTypeBase::Struct(_))
}

pub unsafe fn ast_type_struct_complete<'a>(
    t: &'a AstType,
    ext: &'a Externals,
) -> Option<&'a AstType> {
    if ast_type_struct_members(t).is_some() {
        return Some(t);
    }
    let Some(tag) = ast_type_struct_tag(t) else {
        panic!();
    };
    ext.get_struct(tag.as_ptr())
}

pub fn ast_type_struct_members(t: &AstType) -> Option<&[Box<AstVariable>]> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!()
    };
    s.members.as_ref().map(|v| v.as_slice())
}

pub fn ast_type_struct_tag(t: &AstType) -> Option<&OwningCStr> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!()
    };
    s.tag.as_ref()
}

pub fn ast_type_create_struct_anonym(members: Vec<Box<AstVariable>>) -> Box<AstType> {
    ast_type_create_struct(None, Some(Box::new(members)))
}

pub fn ast_type_create_struct_partial(tag: OwningCStr) -> Box<AstType> {
    ast_type_create_struct(Some(tag), None)
}

pub fn ast_type_mod_or(t: &mut AstType, m: AstTypeModifier) {
    t.modifiers |= m;
}

pub fn ast_type_istypedef(t: &AstType) -> bool {
    t.modifiers & MOD_TYPEDEF != 0
}

pub fn ast_type_copy(t: &AstType) -> Box<AstType> {
    Box::new(t.clone())
}

pub fn ast_type_str(t: &AstType) -> OwningCStr {
    let mut b = strbuilder_create();
    strbuilder_write!(b, "{}", unsafe { mod_str(t.modifiers as libc::c_int) });
    match &t.base {
        AstTypeBase::Pointer(ptr_type) => {
            ast_type_str_build_ptr(&mut b, ptr_type.as_ref().unwrap());
        }
        AstTypeBase::Array(arr) => {
            ast_type_str_build_arr(&mut b, arr);
        }
        AstTypeBase::Struct(s) => {
            ast_type_str_build_struct(&mut b, s);
        }
        AstTypeBase::UserDefined(name) => {
            strbuilder_write!(b, "{name}");
        }
        AstTypeBase::Void => {
            strbuilder_write!(b, "void");
        }
        AstTypeBase::Char => {
            strbuilder_write!(b, "char");
        }
        AstTypeBase::Short => {
            strbuilder_write!(b, "short");
        }
        AstTypeBase::Int => {
            strbuilder_write!(b, "int");
        }
        AstTypeBase::Long => {
            strbuilder_write!(b, "long");
        }
        AstTypeBase::Float => {
            strbuilder_write!(b, "float");
        }
        AstTypeBase::Double => {
            strbuilder_write!(b, "double");
        }
        AstTypeBase::Signed => {
            strbuilder_write!(b, "signed");
        }
        AstTypeBase::Unsigned => {
            strbuilder_write!(b, "unsigned");
        }
        _ => panic!(),
    }
    strbuilder_build(b)
}

unsafe fn mod_str(mod_0: libc::c_int) -> OwningCStr {
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
    let mut b = strbuilder_create();
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
            nmods -= 1;
            let space = if fresh11 != 0 { " " } else { "" };
            strbuilder_write!(b, "{}{space}", cstr!(modstr[i_0 as usize]));
        }
        i_0 += 1;
    }
    strbuilder_build(b)
}

fn ast_type_str_build_ptr(b: &mut StrBuilder, ptr_type: &AstType) {
    let base = ast_type_str(ptr_type);
    let space: bool = !matches!(ptr_type.base, AstTypeBase::Pointer(_));
    strbuilder_write!(*b, "{base}{}*", if space { " " } else { "" },);
}

fn ast_type_str_build_arr(b: &mut StrBuilder, arr: &AstArrayType) {
    let base = ast_type_str(&arr.type_);
    strbuilder_write!(*b, "{base}[{}]", arr.length);
}

fn ast_type_str_build_struct(b: &mut StrBuilder, s: &AstStructType) {
    assert!(s.tag.is_some() || s.members.is_some());
    strbuilder_write!(*b, "struct ");
    if let Some(tag) = &s.tag {
        strbuilder_write!(*b, "{tag}");
    }
    let Some(members) = s.members.as_ref() else {
        return;
    };
    strbuilder_write!(*b, " {{ ");
    for field in members.iter() {
        let s = ast_variable_str(field);
        strbuilder_write!(*b, "{s}; ");
    }
    strbuilder_write!(*b, "}}");
}

pub fn ast_type_ptr_type(t: &AstType) -> Option<&AstType> {
    let AstTypeBase::Pointer(ptr_type) = &t.base else {
        panic!()
    };
    ptr_type.as_deref()
}

pub fn ast_variable_create(name: OwningCStr, ty: Box<AstType>) -> Box<AstVariable> {
    // Note: In the original, this function can take a null name and create a variable with null name.
    // This is actually done for the arguments in function declarations like `void fclose(FILE*);`.
    Box::new(AstVariable { name, type_: ty })
}

pub fn ast_variable_copy(v: &AstVariable) -> Box<AstVariable> {
    Box::new(v.clone())
}

pub fn ast_variable_arr_copy(v: &[Box<AstVariable>]) -> Vec<Box<AstVariable>> {
    v.to_vec()
}

pub fn ast_variable_str(v: &AstVariable) -> OwningCStr {
    let mut b = strbuilder_create();
    let t = ast_type_str(&v.type_);
    strbuilder_write!(b, "{t} {}", v.name);
    strbuilder_build(b)
}

pub fn ast_variable_name(v: &AstVariable) -> *mut libc::c_char {
    v.name.as_ptr()
}

pub fn ast_variable_type(v: &AstVariable) -> &AstType {
    &v.type_
}

pub fn ast_function_create(
    is_axiom: bool,
    ret: Box<AstType>,
    name: OwningCStr,
    params: Vec<Box<AstVariable>>,
    abstract_0: Box<AstBlock>,
    body: Option<SemiBox<AstBlock>>,
) -> *mut AstFunction {
    Box::into_raw(Box::new(AstFunction {
        is_axiom,
        ret,
        name,
        params,
        abstract_: abstract_0,
        body,
    }))
}

pub unsafe fn ast_function_destroy(f: *mut AstFunction) {
    drop(Box::from_raw(f));
}

impl<'ast> AstFunction<'ast> {
    pub fn str(&self) -> OwningCStr {
        let mut b = strbuilder_create();
        if self.is_axiom {
            strbuilder_write!(b, "axiom ");
        }
        strbuilder_write!(b, "{}\n", ast_type_str(&self.ret));
        strbuilder_write!(b, "{}(", self.name);
        for (i, param) in self.params.iter().enumerate() {
            let v = ast_variable_str(param);
            let space = if i + 1 < self.params.len() { ", " } else { "" };
            strbuilder_write!(b, "{v}{space}");
        }
        let abs = ast_block_str(&self.abstract_, "\t");
        strbuilder_write!(b, ") ~ [\n{abs}]");
        if let Some(body) = &self.body {
            let body = ast_block_str(body, "\t");
            strbuilder_write!(b, "{{\n{body}}}");
        } else {
            strbuilder_write!(b, ";");
        }
        strbuilder_write!(b, "\n");
        strbuilder_build(b)
    }

    pub fn name(&self) -> *mut libc::c_char {
        self.name.as_ptr()
    }

    pub fn copy(&self) -> *mut AstFunction<'ast> {
        Box::into_raw(Box::new(self.clone()))
    }

    pub fn is_axiom(&self) -> bool {
        self.is_axiom
    }

    pub fn is_proto(&self) -> bool {
        self.body.is_none()
    }

    pub fn abs_is_empty(&self) -> bool {
        ast_block_ndecls(&self.abstract_) == 0 && ast_block_nstmts(&self.abstract_) == 0
    }

    pub fn rtype(&self) -> &AstType {
        &self.ret
    }

    pub fn body(&self) -> &AstBlock {
        let Some(body) = self.body.as_ref() else {
            panic!("cannot find body for {:?}", self.name);
        };
        body
    }

    pub fn abstract_block(&self) -> &AstBlock {
        &self.abstract_
    }

    pub fn params(&self) -> &[Box<AstVariable>] {
        self.params.as_slice()
    }

    pub fn preconditions(&self) -> Result<Option<&AstStmt>> {
        ast_block_preconds(self.abstract_block())
    }
}

pub unsafe fn ast_function_protostitch(
    f: *mut AstFunction,
    ext: *mut Externals,
) -> *mut AstFunction {
    let proto: *mut AstFunction = (*ext).get_func((*f).name.as_ptr());
    if !proto.is_null() {
        (*f).abstract_ = (*proto).abstract_.clone();
    }
    f
}

pub unsafe fn ast_function_verify(f: *mut AstFunction, ext: *mut Externals) -> Result<()> {
    let mut state = state_create(dynamic_str((*f).name()), ext, (*f).rtype());
    ast_function_initparams(&*f, &mut state)?;
    path_absverify_withstate(f, &mut state)?;
    Ok(())
}

unsafe fn path_absverify_withstate(f: *mut AstFunction, state: *mut State) -> Result<()> {
    let abs = (*f).abstract_block();
    for var in &abs.decls {
        state_declare(state, var, false);
    }
    path_absverify(f, state, 0 as libc::c_int)
}

unsafe fn path_absverify(f: *mut AstFunction, state: *mut State, index: libc::c_int) -> Result<()> {
    let abs = (*f).abstract_block();
    for i in index as usize..abs.stmts.len() {
        let stmt = &abs.stmts[i];
        let mut splits = ast_stmt_splits(stmt, state)?;
        if !splits.conds.is_empty() {
            return split_paths_absverify(f, state, i as libc::c_int, &mut splits);
        }
        if !ast_stmt_ispre(stmt) {
            ast_stmt_absexec(stmt, state, true)?;
        }
    }
    abstract_audit(f, state)?;
    Ok(())
}

pub unsafe fn ast_function_initparams(f: &AstFunction, s: *mut State) -> Result<()> {
    let params = f.params();
    for param in params {
        state_declare(s, param, true);
    }
    ast_function_precondsinit(f, s)?;
    for param in params {
        inititalise_param(param, s)?;
    }
    Ok(())
}

// TODO: resultify
#[derive(Clone)]
pub struct PrecondsResult {
    pub stmt: *mut AstStmt,
    pub err: Option<Box<Error>>,
}

unsafe fn ast_function_precondsinit(f: &AstFunction, s: *mut State) -> Result<()> {
    let pre_stmt = f.preconditions()?;
    if let Some(stmt) = pre_stmt {
        ast_stmt_absexec(stmt, s, true)?;
    }
    Ok(())
}

unsafe fn inititalise_param(param: &AstVariable, state: *mut State) -> Result<()> {
    let name = ast_variable_name(param);
    let t = ast_variable_type(param);
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
    let mut actual_state = state_create_withprops(
        dynamic_str((*f).name()),
        state_getext(abstract_state),
        (*f).rtype(),
        (state_getprops(&mut *abstract_state)).clone(),
    );
    ast_function_initparams(&*f, &mut actual_state).unwrap();
    ast_function_setupabsexec(&*f, &mut actual_state)?;
    abstract_auditwithstate(f, &mut actual_state, abstract_state)?;
    Ok(())
}

unsafe fn ast_function_setupabsexec(f: &AstFunction, state: *mut State) -> Result<()> {
    for stmt in &f.abstract_.stmts {
        ast_stmt_setupabsexec(stmt, state)?;
    }
    Ok(())
}

unsafe fn abstract_auditwithstate(
    f: *mut AstFunction,
    actual_state: *mut State,
    abstract_state: *mut State,
) -> Result<()> {
    for decl in &(*f).body.as_ref().unwrap().decls {
        state_declare(actual_state, decl, false);
    }
    path_verify(f, actual_state, 0 as libc::c_int, abstract_state)
}

unsafe fn path_verify(
    f: *mut AstFunction,
    actual_state: *mut State,
    index: libc::c_int,
    abstract_state: *mut State,
) -> Result<()> {
    let fname = (*f).name();
    let stmts = &(*f).body.as_ref().unwrap().stmts;
    #[allow(clippy::needless_range_loop)]
    for i in index as usize..stmts.len() {
        let stmt = &stmts[i];
        // splits.err is ignored in the original
        let splits = ast_stmt_splits(stmt, actual_state);
        match splits {
            Ok(mut splits) if !splits.conds.is_empty() => {
                return split_paths_verify(
                    f,
                    actual_state,
                    i as libc::c_int,
                    &mut splits,
                    abstract_state,
                );
            }
            _ => {
                ast_stmt_process(stmt, fname, actual_state)?;
                if ast_stmt_isterminal(stmt, actual_state) {
                    break;
                }
            }
        }
    }
    if state_hasgarbage(actual_state) {
        vprintln!("actual: {}", state_str(actual_state));
        return Err(Error::new(format!(
            "{}: garbage on heap",
            cstr!((*f).name()),
        )));
    }
    let equiv: bool = state_equal(&*actual_state, &*abstract_state);
    if !equiv {
        return Err(Error::new(format!(
            "{}: actual and abstract states differ",
            cstr!((*f).name()),
        )));
    }
    Ok(())
}

pub unsafe fn ast_function_absexec(f: &AstFunction, state: *mut State) -> Result<*mut Value> {
    for decl in &f.abstract_.decls {
        state_declare(state, decl, false);
    }
    for stmt in &f.abstract_.stmts {
        ast_stmt_absexec(stmt, state, false)?;
    }
    let obj: *mut Object = state_getresult(state);
    if obj.is_null() {
        panic!();
    }
    Ok(object_as_value(obj))
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
    // Note: Original leaks both functions to avoid triple-freeing the body.
    // We borrow instead.
    for (i, f) in paths.into_iter().enumerate() {
        let mut actual_copy = state_copywithname(&*actual_state, (*f).name());
        let mut abstract_copy = state_copywithname(&*abstract_state, (*f).name());
        // Note: Original leaks expression.
        let expr = ast_expr_inverted_copy(cond, i == 1);
        let r = ast_expr_assume(&expr, &mut actual_copy);
        std::mem::forget(expr);
        let r = r?;
        if !r.is_contradiction {
            path_verify(f, &mut actual_copy, index, &mut abstract_copy)?;
        }
        // Note: Original leaks both state copies.
    }
    Ok(())
}

type FuncGraph = InsertionOrderMap<CString, Vec<*mut libc::c_char>>;

unsafe fn recurse_buildgraph(g: &mut FuncGraph, dedup: &mut Map, fname: &CStr, ext: &Externals) {
    let mut local_dedup = Map::new();
    if !(dedup.get(fname.as_ptr())).is_null() {
        return;
    }
    dedup.set(fname.as_ptr(), 1 as libc::c_int as *mut libc::c_void);
    let f: *mut AstFunction = ext.get_func(fname.as_ptr());
    if f.is_null() {
        eprintln!("function `{}' is not declared", fname.to_string_lossy());
        process::exit(1);
    }
    if f.is_null() {
        panic!();
    }
    if (*f).is_axiom {
        return;
    }
    let body = (*f).body.as_deref().unwrap();
    let mut val = vec![];
    for stmt in &body.stmts {
        let farr = ast_stmt_getfuncs(stmt);

        for func in farr {
            if (local_dedup.get(func.as_ptr())).is_null() {
                let func = func.into_ptr(); // transfer of ownership (it's going into val)
                let f_0: *mut AstFunction = ext.get_func(func);
                if !(*f_0).is_axiom {
                    val.push(func);
                }
                local_dedup.set(func, 1 as libc::c_int as *mut libc::c_void);
                recurse_buildgraph(g, dedup, CStr::from_ptr(func), ext);
            }
        }
    }
    g.insert(fname.into(), val);
}

unsafe fn abstract_paths<'origin>(
    f: &'origin AstFunction,
    _index: libc::c_int,
    cond: &AstExpr,
) -> Vec<*mut AstFunction<'origin>> {
    let f_true: *mut AstFunction = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_ptr(), cond),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    // Note: Original leaks inv_assumption, but I think unintentionally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false: *mut AstFunction = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_ptr(), &inv_assumption),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
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
        let mut s_copy = state_copywithname(&*state, (*f).name());
        // Note: Original leaks `inv` but I think accidentally.
        let inv = ast_expr_inverted_copy(cond, i == 1);
        let r = ast_expr_assume(&inv, &mut s_copy)?;
        if !r.is_contradiction {
            path_absverify(f, &mut s_copy, index)?;
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
    for &cond in &(*splits).conds {
        split_path_absverify(f, state, index, &*cond)?;
    }
    Ok(())
}

pub unsafe fn ast_function_buildgraph(fname: &CStr, ext: &Externals) -> FuncGraph {
    let mut dedup = Map::new();
    let mut g = InsertionOrderMap::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    g
}

unsafe fn split_name(name: *mut libc::c_char, assumption: &AstExpr) -> OwningCStr {
    let mut b = strbuilder_create();
    strbuilder_write!(b, "{} | {assumption}", cstr!(name));
    strbuilder_build(b)
}

unsafe fn split_paths_verify(
    f: *mut AstFunction,
    actual_state: *mut State,
    index: libc::c_int,
    splits: *mut AstStmtSplits,
    abstract_state: *mut State,
) -> Result<()> {
    for &cond in &(*splits).conds {
        split_path_verify(f, actual_state, index, &*cond, abstract_state)?;
    }
    Ok(())
}

unsafe fn body_paths<'origin>(
    f: &'origin AstFunction,
    _index: libc::c_int,
    cond: &AstExpr,
) -> Vec<*mut AstFunction<'origin>> {
    let f_true: *mut AstFunction = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_ptr(), cond),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    // Note: Original leaks `inv_assumption` but I think accidentally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false: *mut AstFunction = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_ptr(), &inv_assumption),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    vec![f_true, f_false]
}

pub fn ast_functiondecl_create(f: *mut AstFunction<'static>) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: AstExternDeclKind::Function(f),
    })
}

pub fn ast_externdecl_as_function_ptr(decl: &AstExternDecl) -> Option<*mut AstFunction<'static>> {
    match &decl.kind {
        AstExternDeclKind::Function(f) => Some(*f),
        _ => None,
    }
}

pub unsafe fn ast_externdecl_as_function(decl: &AstExternDecl) -> Option<&AstFunction> {
    match &decl.kind {
        AstExternDeclKind::Function(f) => Some(&**f),
        _ => None,
    }
}

pub fn ast_decl_create(name: OwningCStr, t: Box<AstType>) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: if ast_type_istypedef(&t) {
            AstExternDeclKind::Typedef(AstTypedefDecl { name, type_0: t })
        } else if ast_type_isstruct(&t) {
            assert!(ast_type_struct_tag(&t).is_some());
            AstExternDeclKind::Struct(t)
        } else {
            AstExternDeclKind::Variable(ast_variable_create(name, t))
        },
    })
}

pub unsafe fn ast_externdecl_install(decl: *mut AstExternDecl, ext: &mut Externals) {
    match &mut (*decl).kind {
        AstExternDeclKind::Function(f) => {
            ext.declare_func((**f).name(), *f);
        }
        AstExternDeclKind::Variable(v) => {
            ext.declare_var(ast_variable_name(v), &mut **v);
        }
        AstExternDeclKind::Typedef(typedef) => {
            ext.declare_typedef(typedef.name.as_ptr(), &*typedef.type_0);
        }
        AstExternDeclKind::Struct(s) => {
            ext.declare_struct(&**s);
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
                AstExternDeclKind::Variable(_) => {}
                AstExternDeclKind::Typedef(_) => {}
                AstExternDeclKind::Struct(_) => {}
            }
        }
    }
}

pub fn parse_int(s: &str) -> libc::c_int {
    s.parse().expect("parse error")
}

pub fn parse_char(s: &str) -> libc::c_char {
    assert!(s.starts_with('\'') && s.ends_with('\''));
    let s = &s[1..s.len() - 1];
    if let Some(stripped) = s.strip_prefix('\\') {
        parse_escape(stripped) as libc::c_char
    } else {
        s.chars().next().expect("invalid char literal") as u32 as libc::c_char
    }
}

pub fn parse_escape(c: &str) -> libc::c_int {
    match c {
        "0" => '\0' as u32 as libc::c_int,
        "t" => '\t' as u32 as libc::c_int,
        "n" => '\t' as u32 as libc::c_int, // Note: '\t' rather than '\n', a bug in the original.
        _ => {
            panic!("unrecognized char escape sequence: {:?}", c);
        }
    }
}

pub fn lvalue_create(t: Option<&AstType>, obj: *mut Object) -> LValue {
    LValue { t, obj }
}

pub fn lvalue_type<'ast>(l: &LValue<'ast>) -> &'ast AstType {
    l.t.unwrap()
}

pub fn lvalue_object(l: &LValue) -> *mut Object {
    l.obj
}

pub unsafe fn ast_topological_order(fname: &CStr, ext: &mut Externals) -> Vec<OwningCStr> {
    topological_order(fname, ext)
}

pub unsafe fn ast_protostitch(f: *mut AstFunction, ext: *mut Externals) -> *mut AstFunction {
    ast_function_protostitch(f, ext)
}
