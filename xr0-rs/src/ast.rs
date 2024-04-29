use std::collections::VecDeque;
use std::fmt::{self, Display, Formatter};
use std::process;
use std::sync::Arc;

use crate::math::{math_eq, math_ge, math_gt, math_le, math_lt, MathAtom, MathExpr};
use crate::object::object_member_lvalue;
use crate::parser::LexemeMarker;
use crate::state::state::{
    state_addresses_deallocand, state_copy, state_copywithname, state_create,
    state_create_withprops, state_declare, state_deref, state_equal, state_get, state_getloc,
    state_getobject, state_getobjecttype, state_getresult, state_getvconst, state_hasgarbage,
    state_isalloc, state_islval, state_popframe, state_pushframe, state_range_alloc,
    state_range_aredeallocands, state_range_dealloc, state_static_init, state_str, state_vconst,
};
use crate::state::State;
use crate::util::{Error, InsertionOrderMap, Result, SemiBox};
use crate::value::{
    value_as_constant, value_as_location, value_as_sync, value_copy, value_equal, value_int_create,
    value_int_indefinite_create, value_isconstant, value_isint, value_islocation, value_isstruct,
    value_issync, value_literal_create, value_pf_augment, value_ptr_indefinite_create, value_str,
    value_struct_indefinite_create, value_struct_member, value_sync_create, value_to_expr,
};
use crate::{str_write, vprintln, Externals, Object, Value};

/// Type of integer constants in C on the target platform. Currently XR0 can handle only 32-bit
/// constants. This is also used for the length of arrays. 2 billion should be enough for anyone.
#[allow(non_camel_case_types)]
pub type c_int = i32;

/// The unsigned type that corresponds to `c_int`.
#[allow(non_camel_case_types)]
pub type c_uint = u32;

/// Type of `char` values in C on the target platform.
#[allow(non_camel_case_types)]
pub type c_char = i8;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstAllocKind {
    Alloc,
    Dealloc,
    Clump,
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
    inc: bool,
    pre: bool,
}

#[derive(Clone)]
pub struct CallExpr {
    fun: Box<AstExpr>,
    args: Vec<Box<AstExpr>>,
}

#[derive(Clone)]
pub struct ConstantExpr {
    constant: c_int,
    is_char: bool,
}

#[derive(Clone)]
pub struct StructMemberExpr {
    root: Box<AstExpr>,
    member: String,
}

#[derive(Clone)]
enum AstExprKind {
    Identifier(String),
    Constant(ConstantExpr),
    StringLiteral(String),
    #[allow(dead_code)]
    Bracketed(Box<AstExpr>),
    #[allow(dead_code)]
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
    pub modifiers: AstTypeModifiers,
    pub base: AstTypeBase,
}

#[derive(Clone)]
pub struct AstStructType {
    pub tag: Option<String>,
    pub members: Option<Box<Vec<Box<AstVariable>>>>,
}

#[derive(Clone)]
pub struct AstVariable {
    // Note: In the original, `name` could be null. However in most situations (almost all; an
    // exception is the parameter in `fclose(FILE *);` which has no name) this would be invalid, so
    // we end up banning it.
    pub name: String,
    pub type_: Box<AstType>,
}

#[derive(Clone)]
pub struct AstArrayType {
    pub type_: Box<AstType>,
    pub length: c_int,
}

#[derive(Clone)]
pub enum AstTypeBase {
    Void,
    Char,
    #[allow(dead_code)]
    Short,
    Int,
    #[allow(dead_code)]
    Long,
    #[allow(dead_code)]
    Float,
    #[allow(dead_code)]
    Double,
    #[allow(dead_code)]
    Signed,
    #[allow(dead_code)]
    Unsigned,
    Pointer(Option<Box<AstType>>),
    #[allow(dead_code)]
    Array(AstArrayType),
    Struct(AstStructType),
    #[allow(dead_code)]
    Union(AstStructType),
    #[allow(dead_code)]
    Enum,
    UserDefined(String),
}

pub type AstTypeModifiers = u32;
pub const MOD_TYPEDEF: AstTypeModifiers = 1;
pub const MOD_EXTERN: AstTypeModifiers = 2;
pub const MOD_STATIC: AstTypeModifiers = 4;
pub const MOD_AUTO: AstTypeModifiers = 8;
pub const MOD_REGISTER: AstTypeModifiers = 16;
pub const MOD_CONST: AstTypeModifiers = 32;
pub const MOD_VOLATILE: AstTypeModifiers = 64;

pub struct LValue<'ast> {
    pub t: Option<&'ast AstType>,
    pub obj: Option<&'ast mut Object>,
}

#[derive(Clone)]
pub struct AstFunction<'ast> {
    pub is_axiom: bool,
    pub ret: Box<AstType>,
    pub name: String,
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
    pub label: String,
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
    #[allow(dead_code)]
    Allocation(AstAllocStmt),
}

#[derive(Clone)]
pub struct Preresult {
    pub is_contradiction: bool,
}

#[derive(Clone, Default)]
pub struct AstStmtSplits {
    // Note: In the original this array is heap-allocated but never freed.
    pub conds: Vec<Box<AstExpr>>,
}

#[derive(Clone)]
pub struct AstExternDecl {
    pub kind: AstExternDeclKind,
}

#[derive(Clone)]
pub struct AstTypedefDecl {
    pub name: String,
    pub type_0: Box<AstType>,
}

#[derive(Clone)]
pub enum AstExternDeclKind {
    Function(Box<AstFunction<'static>>),
    Variable(Box<AstVariable>),
    Typedef(AstTypedefDecl),
    Struct(Box<AstType>),
}

pub struct Ast {
    pub decls: Vec<Box<AstExternDecl>>,
}

impl Display for AstExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            AstExprKind::Identifier(id) => write!(f, "{id}"),
            AstExprKind::Constant(constant) => write!(f, "{constant}"),
            AstExprKind::StringLiteral(s) => write!(f, "{s:?}"),
            AstExprKind::Bracketed(inner) => write!(f, "({inner})"),
            AstExprKind::Call(call) => write!(f, "{call}"),
            AstExprKind::IncDec(incdec) => write!(f, "{incdec}"),
            AstExprKind::StructMember(sm) => write!(f, "{sm}"),
            AstExprKind::Unary(u) => write!(f, "{u}"),
            AstExprKind::Binary(b) => write!(f, "{b}"),
            AstExprKind::Assignment(a) => write!(f, "{a}"),
            AstExprKind::IsDeallocand(assertand) => write!(f, "@{assertand}"),
            AstExprKind::IsDereferencable(assertand) => write!(f, "${assertand}"),
            AstExprKind::ArbArg => write!(f, "$"),
            AstExprKind::Allocation(alloc) => write!(f, "{alloc}"),
            AstExprKind::Iteration => panic!(),
        }
    }
}

fn expr_literal_eval(literal: &str, state: &mut State) -> Result<Box<Value>> {
    Ok(state_static_init(state, literal))
}

fn expr_constant_eval(constant: &ConstantExpr, _state: &mut State) -> Result<Box<Value>> {
    Ok(value_int_create(constant.constant))
}

fn rangeprocess_dealloc(
    dealloc: &AllocExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> Result<()> {
    let state: *mut State = state;
    unsafe {
        let obj = hack_base_object_from_alloc(&dealloc.arg, &mut *state);
        state_range_dealloc(&mut *state, obj, lw, up)
    }
}

fn hack_base_object_from_alloc<'s>(expr: &'s AstExpr, state: &'s mut State) -> &'s Object {
    let inner = ast_expr_unary_operand(expr);
    let i = AstExpr::new_identifier("i".to_string());
    assert!(ast_expr_equal(ast_expr_binary_e2(inner), &i));
    drop(i);
    let lval = ast_expr_lvalue(ast_expr_binary_e1(inner), state).unwrap();
    lval.obj.unwrap()
}

impl AstExpr {
    fn new(kind: AstExprKind) -> Box<AstExpr> {
        Box::new(AstExpr { kind })
    }

    pub fn new_identifier(s: String) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Identifier(s))
    }

    pub fn new_constant(k: c_int) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Constant(ConstantExpr {
            is_char: false,
            constant: k,
        }))
    }

    pub fn new_constant_char(c: c_char) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Constant(ConstantExpr {
            is_char: true,
            constant: c as c_int,
        }))
    }

    pub fn new_literal(s: String) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::StringLiteral(s))
    }

    #[allow(dead_code)]
    pub fn new_bracketed(root: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Bracketed(root))
    }

    #[allow(dead_code)]
    pub fn new_iteration() -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Iteration)
    }

    pub fn new_call(fun: Box<AstExpr>, args: Vec<Box<AstExpr>>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Call(CallExpr { fun, args }))
    }

    pub fn new_incdec(root: Box<AstExpr>, inc: bool, pre: bool) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::IncDec(IncDecExpr {
            operand: root,
            inc,
            pre,
        }))
    }

    pub fn new_member(struct_: Box<AstExpr>, field: String) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::StructMember(StructMemberExpr {
            root: struct_,
            member: field,
        }))
    }

    pub fn new_unary(root: Box<AstExpr>, op: AstUnaryOp) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Unary(UnaryExpr { op, arg: root }))
    }

    pub fn new_binary(e1: Box<AstExpr>, op: AstBinaryOp, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Binary(BinaryExpr { e1, op, e2 }))
    }

    pub fn new_eq(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Eq, e2)
    }

    #[allow(dead_code)]
    pub fn new_ne(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Ne, e2)
    }

    pub fn new_lt(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Lt, e2)
    }

    #[allow(dead_code)]
    pub fn new_gt(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Gt, e2)
    }

    pub fn new_le(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Le, e2)
    }

    pub fn new_ge(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Ge, e2)
    }

    pub fn new_sum(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Addition, e2)
    }

    pub fn new_difference(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Subtraction, e2)
    }

    pub fn new_assignment(root: Box<AstExpr>, value: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Assignment(AssignmentExpr {
            lval: root,
            rval: value,
        }))
    }

    pub fn new_isdeallocand(assertand: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::IsDeallocand(assertand))
    }

    pub fn new_isdereferencable(assertand: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::IsDereferencable(assertand))
    }

    pub fn new_arbarg() -> Box<AstExpr> {
        AstExpr::new(AstExprKind::ArbArg)
    }

    pub fn new_alloc(arg: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Allocation(AllocExpr {
            kind: AstAllocKind::Alloc,
            arg,
        }))
    }

    pub fn new_dealloc(arg: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Allocation(AllocExpr {
            kind: AstAllocKind::Dealloc,
            arg,
        }))
    }

    pub fn new_clump(arg: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Allocation(AllocExpr {
            kind: AstAllocKind::Clump,
            arg,
        }))
    }
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
            ast_expr_equal(&m1.root, &m2.root) && m1.member == m2.member
        }
        _ => false,
    }
}

fn rangeprocess_alloc(
    assign: &AssignmentExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> Result<()> {
    let AssignmentExpr { lval, rval } = assign;
    let AstExprKind::Allocation(alloc) = &rval.kind else {
        panic!();
    };
    assert_ne!(alloc.kind, AstAllocKind::Dealloc);
    let state: *mut State = state;
    unsafe {
        let obj = hack_base_object_from_alloc(lval, &mut *state);
        state_range_alloc(&mut *state, obj, lw, up)
    }
}

pub fn ast_expr_matheval(e: &AstExpr) -> bool {
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
                    // This negation can overflow, which I think is undefined behavior in the
                    // original. Went ahead and fixed it in passing.
                    c.constant.wrapping_neg() as u32,
                ))))
            } else {
                MathExpr::Atom(MathAtom::Nat(c.constant as u32))
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

fn eval_prop(e1: &MathExpr, op: AstBinaryOp, e2: &MathExpr) -> bool {
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

pub fn ast_expr_decide(expr: &AstExpr, state: &mut State) -> bool {
    match &expr.kind {
        AstExprKind::Constant(c) => c.constant != 0,
        AstExprKind::Unary(_) => expr_unary_decide(expr, state),
        AstExprKind::IsDeallocand(_) => expr_isdeallocand_decide(expr, state),
        AstExprKind::Binary(_) => expr_binary_decide(expr, state),
        _ => panic!(),
    }
}

fn expr_binary_decide(expr: &AstExpr, state: &mut State) -> bool {
    let root = ast_expr_eval(ast_expr_binary_e1(expr), state).unwrap();
    let last = ast_expr_eval(ast_expr_binary_e2(expr), state).unwrap();
    // Note: `root` and `last` are leaked in the original.
    value_compare(&root, ast_expr_binary_op(expr), &last)
}

fn value_compare(v1: &Value, op: AstBinaryOp, v2: &Value) -> bool {
    match op {
        AstBinaryOp::Eq => value_equal(v1, v2),
        AstBinaryOp::Ne => !value_compare(v1, AstBinaryOp::Eq, v2),
        _ => panic!(),
    }
}

fn expr_isdeallocand_decide(expr: &AstExpr, state: &mut State) -> bool {
    let state: *mut State = state;
    unsafe {
        let obj = hack_object_from_assertion(expr, &mut *state);
        state_addresses_deallocand(&mut *state, obj)
    }
}

fn hack_object_from_assertion<'s>(expr: &'s AstExpr, state: &'s mut State) -> &'s Object {
    let assertand = ast_expr_isdeallocand_assertand(expr);
    ast_expr_lvalue(assertand, state).unwrap().obj.unwrap()
}

fn expr_unary_decide(expr: &AstExpr, state: &mut State) -> bool {
    let operand = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_decide(operand, state),
        _ => panic!(),
    }
}

pub fn ast_expr_rangedecide(expr: &AstExpr, lw: &AstExpr, up: &AstExpr, state: &mut State) -> bool {
    match &expr.kind {
        AstExprKind::Unary(_) => unary_rangedecide(expr, lw, up, state),
        AstExprKind::IsDeallocand(_) => expr_isdeallocand_rangedecide(expr, lw, up, state),
        _ => panic!(),
    }
}

fn expr_isdeallocand_rangedecide(
    expr: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> bool {
    let acc = ast_expr_isdeallocand_assertand(expr);
    assert_eq!(ast_expr_unary_op(acc), AstUnaryOp::Dereference);
    let inner = ast_expr_unary_operand(acc);
    let i = AstExpr::new_identifier("i".to_string());
    let j = AstExpr::new_identifier("j".to_string());
    assert!(
        !!(ast_expr_equal(ast_expr_binary_e2(inner), &i)
            || ast_expr_equal(ast_expr_binary_e2(inner), &j))
    );
    drop(j);
    drop(i);
    let state: *mut State = state;
    unsafe {
        let res_lval = ast_expr_lvalue(ast_expr_binary_e1(acc), &mut *state).unwrap();
        let obj = res_lval.obj.unwrap();
        state_range_aredeallocands(&mut *state, obj, lw, up)
    }
}

fn unary_rangedecide(expr: &AstExpr, lw: &AstExpr, up: &AstExpr, state: &mut State) -> bool {
    let operand = ast_expr_unary_operand(expr);
    match ast_expr_unary_op(expr) {
        AstUnaryOp::Bang => !ast_expr_rangedecide(operand, lw, up, state),
        _ => panic!(),
    }
}

pub fn ast_expr_exec(expr: &AstExpr, state: &mut State) -> Result<()> {
    // Note: In the original, the value returned by `ast_expr_eval` is leaked.
    ast_expr_eval(expr, state)?;
    Ok(())
}

pub fn ast_expr_assume(expr: &AstExpr, state: &mut State) -> Result<Preresult> {
    reduce_assume(expr, true, state)
}

fn reduce_assume(expr: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
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

fn binary_assume(b: &BinaryExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let v1 = ast_expr_pf_reduce(&b.e1, s).unwrap();
    let v2 = ast_expr_pf_reduce(&b.e2, s).unwrap();
    // Note: original leaks the expression.
    let expr = AstExpr::new_binary(value_to_expr(&v1), b.op, value_to_expr(&v2));
    irreducible_assume(&expr, value, s)
}

fn irreducible_assume(e: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let prop = ast_expr_inverted_copy(e, !value);
    irreducible_assume_actual(&prop, s)
}

fn irreducible_assume_actual(e: &AstExpr, s: &mut State) -> Result<Preresult> {
    let p = s.props();
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

fn ast_expr_pf_reduce_assume(expr: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let res_val = ast_expr_pf_reduce(expr, s).unwrap();
    irreducible_assume(&res_val.into_sync(), value, s)
}

fn identifier_assume(expr: &AstExpr, value: bool, s: &mut State) -> Result<Preresult> {
    let mut s_copy = state_copy(&*s);
    let res_val = ast_expr_eval(expr, &mut s_copy).unwrap();
    drop(s_copy);
    irreducible_assume(&res_val.into_sync(), value, s)
}

fn binary_deref_eval(expr: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    let arr = ast_expr_eval(ast_expr_binary_e1(expr), state)?;
    // Note: Original seems to leak `arr`.
    let Some(deref_obj) = state_deref(state, &arr, ast_expr_binary_e2(expr))? else {
        return Err(Error::new(format!(
            "undefined indirection: *({expr}) has no value"
        )));
    };
    let Some(v) = deref_obj.as_value() else {
        return Err(Error::new(format!(
            "undefined indirection: *({expr}) has no value"
        )));
    };
    Ok(value_copy(v))
}

fn hack_identifier_builtin_eval(id: &str, state: &State) -> Result<Box<Value>> {
    if state_getvconst(state, id).is_some() || id.starts_with("ptr:") {
        return Ok(value_sync_create(AstExpr::new_identifier(id.to_string())));
    }
    Err(Error::new("not built-in".to_string()))
}

fn expr_to_binary(expr: &AstExpr) -> Box<AstExpr> {
    match &expr.kind {
        AstExprKind::Binary(_) => ast_expr_copy(expr),
        _ => AstExpr::new_binary(
            ast_expr_copy(expr),
            AstBinaryOp::Addition,
            AstExpr::new_constant(0),
        ),
    }
}

fn expr_identifier_eval(id: &str, state: &mut State) -> Result<Box<Value>> {
    if let Ok(res) = hack_identifier_builtin_eval(id, state) {
        return Ok(res);
    }
    if id.starts_with('#') {
        return Ok(value_literal_create(id));
    }
    let Some(obj) = state_getobject(state, id) else {
        return Err(Error::new(format!("unknown idenitfier {id}")));
    };
    let Some(val) = obj.as_value() else {
        vprintln!("state: {}", state_str(state));
        return Err(Error::new(format!(
            "undefined memory access: {id} has no value",
        )));
    };
    Ok(value_copy(val))
}

fn expr_structmember_eval(expr: &StructMemberExpr, s: &mut State) -> Result<Box<Value>> {
    let StructMemberExpr { root, member } = expr;
    let res_val = ast_expr_eval(root, s)?;
    let Some(member) = value_struct_member(&res_val, member) else {
        return Err(Error::new(format!("`{root}' has no field `{member}'")));
    };
    let Some(obj_value) = member.as_value() else {
        // Note: Original would return null if obj_value is null, but almost nobody downstream handles it.
        panic!();
    };
    Ok(value_copy(obj_value))
}

fn address_eval(operand: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    let id = ast_expr_as_identifier(operand);
    Ok(state_getloc(state, id))
}

pub fn ast_expr_alloc_rangeprocess(
    alloc: &AstExpr,
    lw: &AstExpr,
    up: &AstExpr,
    state: &mut State,
) -> Result<()> {
    // Note: I think both values are leaked in the original.
    let lw_val = ast_expr_eval(lw, state)?;
    let up_val = ast_expr_eval(up, state)?;
    let res_lw = value_to_expr(&lw_val);
    let res_up = value_to_expr(&up_val);
    match &alloc.kind {
        AstExprKind::Assignment(assign) => rangeprocess_alloc(assign, &res_lw, &res_up, state),
        AstExprKind::Allocation(alloc) => rangeprocess_dealloc(alloc, &res_lw, &res_up, state),
        _ => panic!(),
    }
}

pub fn ast_expr_eval(expr: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    match &expr.kind {
        AstExprKind::Constant(constant) => expr_constant_eval(constant, state),
        AstExprKind::StringLiteral(literal) => expr_literal_eval(literal, state),
        AstExprKind::Identifier(identifier) => expr_identifier_eval(identifier, state),
        AstExprKind::Unary(unary) => expr_unary_eval(unary, state),
        AstExprKind::StructMember(structmember) => expr_structmember_eval(structmember, state),
        AstExprKind::Call(call) => expr_call_eval(call, state),
        AstExprKind::Assignment(assign) => expr_assign_eval(assign, state),
        AstExprKind::IncDec(incdec) => expr_incdec_eval(incdec, state),
        AstExprKind::Binary(binary) => expr_binary_eval(binary, state),
        AstExprKind::ArbArg => arbarg_eval(state),
        _ => panic!(),
    }
}

fn arbarg_eval(state: &mut State) -> Result<Box<Value>> {
    Ok(state_vconst(
        state,
        &ast_type_create_ptr(ast_type_create(AstTypeBase::Void, 0)),
        None,
        false,
    ))
}

fn expr_binary_eval(binary: &BinaryExpr, state: &mut State) -> Result<Box<Value>> {
    let BinaryExpr { op, e1, e2 } = binary;
    // Note: Both values are leaked in the original.
    let v1 = ast_expr_eval(e1, state)?;
    let v2 = ast_expr_eval(e2, state)?;
    let result = value_sync_create(AstExpr::new_binary(
        value_to_expr(&v1),
        *op,
        value_to_expr(&v2),
    ));
    Ok(result)
}

fn expr_incdec_eval(incdec: &IncDecExpr, state: &mut State) -> Result<Box<Value>> {
    let assign = ast_expr_incdec_to_assignment(incdec);
    if incdec.pre {
        expr_assign_eval(&assign, state)
    } else {
        let res = ast_expr_eval(&incdec.operand, state);
        // Note: The original also ignored errors here.
        let _ = expr_assign_eval(&assign, state);
        res
    }
}

fn expr_assign_eval(assign: &AssignmentExpr, state: &mut State) -> Result<Box<Value>> {
    let AssignmentExpr { lval, rval } = assign;
    let rval_val = ast_expr_eval(rval, state)?;
    let lval_lval = ast_expr_lvalue(lval, state)?;
    let Some(obj) = lval_lval.obj else {
        return Err(Error::new(format!(
            "undefined indirection: {lval} is not an lvalue"
        )));
    };
    obj.assign(Some(value_copy(&rval_val)));
    Ok(rval_val)
}

pub fn ast_expr_lvalue<'s>(expr: &'s AstExpr, state: &'s mut State) -> Result<LValue<'s>> {
    match &expr.kind {
        AstExprKind::Identifier(id) => expr_identifier_lvalue(id, state),
        AstExprKind::Unary(unary) => expr_unary_lvalue(unary, state),
        AstExprKind::StructMember(sm) => expr_structmember_lvalue(sm, state),
        _ => panic!(),
    }
}

pub fn expr_structmember_lvalue<'s>(
    sm: &'s StructMemberExpr,
    state: &'s mut State,
) -> Result<LValue<'s>> {
    let StructMemberExpr { root, member } = sm;
    let state: *mut State = state;
    unsafe {
        // Note: Original fails to check for errors.
        let root_lval = ast_expr_lvalue(root, &mut *state).unwrap();
        let root_obj = root_lval.obj.unwrap();
        let lvalue = object_member_lvalue(root_obj, root_lval.t.unwrap(), member, &mut *state);
        if lvalue.obj.is_none() {
            return Err(Error::new("lvalue error".to_string()));
        }
        Ok(lvalue)
    }
}

pub fn expr_unary_lvalue<'s>(unary: &'s UnaryExpr, state: &'s mut State) -> Result<LValue<'s>> {
    assert_eq!(unary.op, AstUnaryOp::Dereference);
    let inner = &unary.arg;
    match &inner.kind {
        AstExprKind::Identifier(_) => {
            let state: *mut State = state;
            unsafe {
                let root_lval = ast_expr_lvalue(inner, &mut *state)?;
                let Some(root_obj) = root_lval.obj else {
                    // `root` freed

                    // Note: The original does `return (struct lvalue_res) { .lval = NULL, .err = NULL };`
                    // here but I believe every single caller dereferences lval without checking it for
                    // null, so it will crash.
                    panic!();
                };
                let t = ast_type_ptr_type(root_lval.t.unwrap());
                let root_val = root_obj.as_value().unwrap();
                let obj = state_deref(&mut *state, root_val, &AstExpr::new_constant(0))?;
                Ok(LValue { t, obj })
            }
        }
        AstExprKind::Binary(BinaryExpr { op: _, e1, e2 }) => {
            let state: *mut State = state;
            unsafe {
                let root_lval = ast_expr_lvalue(e1, &mut *state)?;
                let Some(root_obj) = root_lval.obj else {
                    // `root` freed

                    // Note: Original returns null. See note above.
                    panic!();
                };
                let t = ast_type_ptr_type(root_lval.t.unwrap());
                let root_val = root_obj.as_value().unwrap();
                let Ok(res_obj) = state_deref(&mut *state, root_val, e2) else {
                    // Note: Original returns null. See note above.
                    panic!();
                };
                Ok(LValue { t, obj: res_obj })
            }
        }
        _ => panic!(),
    }
}

pub fn expr_identifier_lvalue<'s>(id: &str, state: &'s mut State) -> Result<LValue<'s>> {
    let state: *mut State = state;
    unsafe {
        Ok(LValue {
            t: Some(state_getobjecttype(&*state, id)),
            obj: state_getobject(&mut *state, id),
        })
    }
}

fn dereference_eval(operand: &AstExpr, state: &mut State) -> Result<Box<Value>> {
    let binary = expr_to_binary(operand);
    binary_deref_eval(&binary, state)
}

impl Display for ConstantExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let constant = self.constant;
        if !self.is_char {
            return write!(f, "{constant}");
        }
        // XXX FIXME: this generates Rust character literals, should generate C syntax
        match char::try_from(constant as u32) {
            Ok(c) => write!(f, "{:?}", c),
            _ => write!(f, "'\\x{:02x}'", constant as u32),
        }
    }
}

fn expr_call_eval(call: &CallExpr, state: &mut State) -> Result<Box<Value>> {
    let CallExpr { fun, args } = call;
    let name = ast_expr_as_identifier(fun);
    let state: *mut State = state;
    unsafe {
        let Some(f) = (*state).ext().get_func(name) else {
            return Err(Error::new(format!("function `{name}' not found")));
        };
        let params = f.params();
        let rtype = f.rtype();
        let args = prepare_arguments(args, params, &mut *state);
        state_pushframe(&mut *state, name.to_string(), rtype);
        prepare_parameters(params, args, name, &mut *state)?;
        call_setupverify(f, &mut state_copy(&*state))?;
        let v = call_absexec(call, &mut *state)?;
        state_popframe(&mut *state);
        pf_augment(&v, call, &mut *state)
    }
}

fn call_absexec(call: &CallExpr, s: &mut State) -> Result<Box<Value>> {
    let name = ast_expr_as_identifier(&call.fun);
    let s: *mut State = s;
    unsafe {
        let Some(f) = (*s).ext().get_func(name) else {
            return Err(Error::new(format!("function `{name}' not found")));
        };
        // Note: In the original, this checked for `ast_function_absexec` returning a result with
        // null value, and in that case called `call_arbitraryresult`. However, this can't happen.
        if let Some(v) = ast_function_absexec(f, &mut *s)? {
            return Ok(v);
        }
        call_arbitraryresult(call, f, &mut *s)
    }
}

fn call_arbitraryresult(
    _call: &CallExpr,
    f: &AstFunction,
    state: &mut State,
) -> Result<Box<Value>> {
    let res = call_to_computed_value(f, state)?;
    Ok(res)
}

fn call_to_computed_value(f: &AstFunction, s: &mut State) -> Result<Box<Value>> {
    let root = f.name();
    let uncomputed_params = f.params();
    let nparams = uncomputed_params.len();
    let mut computed_params = Vec::with_capacity(nparams);
    for p in uncomputed_params {
        let param = AstExpr::new_identifier(ast_variable_name(p).to_string());
        // Note: The original leaked a result here.
        let v = ast_expr_eval(&param, s)?;
        computed_params.push(if value_islocation(&v) {
            AstExpr::new_identifier(value_str(&v))
        } else {
            value_to_expr(&v)
        });
    }
    Ok(value_sync_create(AstExpr::new_call(
        AstExpr::new_identifier(root.to_string()),
        computed_params,
    )))
}

pub fn ast_expr_absexec(expr: &AstExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    match &expr.kind {
        AstExprKind::Assignment(assign) => assign_absexec(assign, state),
        AstExprKind::IsDereferencable(_) => isdereferencable_absexec(expr, state),
        AstExprKind::Allocation(alloc) => alloc_absexec(alloc, state),
        AstExprKind::Identifier(_)
        | AstExprKind::Constant(_)
        | AstExprKind::Unary(_)
        | AstExprKind::Call(_)
        | AstExprKind::StructMember(_)
        | AstExprKind::ArbArg => Ok(Some(ast_expr_eval(expr, state)?)),
        _ => panic!(),
    }
}

fn expr_unary_eval(unary: &UnaryExpr, state: &mut State) -> Result<Box<Value>> {
    match unary.op {
        AstUnaryOp::Dereference => dereference_eval(&unary.arg, state),
        AstUnaryOp::Address => address_eval(&unary.arg, state),
        AstUnaryOp::Bang => Ok(value_literal_create("hack")),
        _ => panic!(),
    }
}

fn alloc_absexec(alloc: &AllocExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    match alloc.kind {
        AstAllocKind::Alloc => Ok(Some(state.alloc())),
        AstAllocKind::Dealloc => dealloc_process(alloc, state),
        AstAllocKind::Clump => Ok(Some(state.clump())),
    }
}

fn dealloc_process(alloc: &AllocExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    let val = ast_expr_eval(&alloc.arg, state)?;
    state.dealloc(&val)?;
    Ok(None)
}

// Argument `fname` is unused after removing some dead (or ill-advised) code.
pub fn prepare_parameters(
    params: &[Box<AstVariable>],
    args: Vec<Result<Box<Value>>>,
    _fname: &str,
    state: &mut State,
) -> Result<()> {
    assert_eq!(params.len(), args.len());
    for (param, res) in params.iter().zip(args) {
        state_declare(state, param, true);

        let arg = res?;
        let name = AstExpr::new_identifier(ast_variable_name(param).to_string());
        let lval_lval = ast_expr_lvalue(&name, state)?;
        let obj = lval_lval.obj.unwrap();
        // Note: Original does not null-check `obj`.
        // Note: I think the arg is copied needlessly in the original, and one is leaked.
        obj.assign(Some(arg));
    }
    Ok(())
}

fn isdereferencable_absexec(expr: &AstExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    // Note: In the original it's unclear why it's safe for `state.props` to assume ownership of
    // `expr`. The way we solved the puzzle, copying is correct.
    let p = state.props();
    p.install(ast_expr_copy(expr));
    Ok(None)
}

pub fn prepare_arguments(
    args: &[Box<AstExpr>],
    params: &[Box<AstVariable>],
    state: &mut State,
) -> Vec<Result<Box<Value>>> {
    assert_eq!(args.len(), params.len());
    args.iter().map(|arg| ast_expr_eval(arg, state)).collect()
}

fn assign_absexec(assign: &AssignmentExpr, state: &mut State) -> Result<Option<Box<Value>>> {
    let AssignmentExpr { lval, rval } = assign;
    let Some(val) = ast_expr_absexec(rval, state)? else {
        debug_assert!(false);
        return Err(Error::new("undefined indirection (rvalue)".to_string()));
    };
    let lval_res = ast_expr_lvalue(lval, state)?;
    let Some(obj) = lval_res.obj else {
        return Err(Error::new("undefined indirection (lvalue)".to_string()));
    };
    obj.assign(Some(value_copy(&val)));
    Ok(Some(val))
}

fn verify_paramspec(
    param: &Value,
    arg: &Value,
    param_state: &mut State,
    arg_state: &mut State,
) -> Result<()> {
    if !state_islval(param_state, param) {
        return Ok(());
    }
    if !state_islval(arg_state, arg) {
        return Err(Error::new("must be lvalue".to_string()));
    }
    if state_isalloc(param_state, param) && !state_isalloc(arg_state, arg) {
        return Err(Error::new("must be heap allocated".to_string()));
    }
    let param_obj = state_get(param_state, value_as_location(param), false)?.unwrap();
    let arg_obj = state_get(arg_state, value_as_location(arg), false)?.unwrap();
    if !param_obj.has_value() {
        return Ok(());
    }
    if !arg_obj.has_value() {
        return Err(Error::new("must be rvalue".to_string()));
    }
    // XXX FIXME: Unlike the original, we copy the values to satisfy Rust alias analysis.
    let param_val = param_obj.as_value().unwrap().clone();
    let arg_val = arg_obj.as_value().unwrap().clone();
    verify_paramspec(&param_val, &arg_val, param_state, arg_state)
}

fn call_setupverify(f: &AstFunction, arg_state: &mut State) -> Result<()> {
    let fname = f.name();
    let mut param_state = state_create(fname.to_string(), arg_state.externals_arc(), f.rtype());
    ast_function_initparams(f, &mut param_state)?;
    let params = f.params();
    for p in params {
        let id = ast_variable_name(p);
        // Note: `param` and `arg` are deliberately leaked in the original to avoid double-freeing
        // the variable's location.
        let param = state_getloc(&mut param_state, id);
        let arg = state_getloc(arg_state, id);
        if let Err(err) = verify_paramspec(&param, &arg, &mut param_state, arg_state) {
            return Err(Error::new(format!("parameter {id} of {fname} {}", err.msg)));
        }
    }
    // Note: Original leaks the state.
    Ok(())
}

pub fn ast_expr_as_constant(expr: &AstExpr) -> c_int {
    match &expr.kind {
        AstExprKind::Constant(c) => c.constant,
        _ => panic!(),
    }
}

fn pf_augment(v: &Value, call: &CallExpr, state: &mut State) -> Result<Box<Value>> {
    if !value_isstruct(v) {
        return Ok(value_copy(v));
    }
    // Note: Original leaked a result and a value here.
    let res_val = call_pf_reduce(call, state)?;
    Ok(value_pf_augment(v, value_as_sync(&res_val)))
}

pub fn ast_expr_as_identifier(expr: &AstExpr) -> &str {
    let AstExprKind::Identifier(id) = &expr.kind else {
        panic!();
    };
    id
}

fn unary_pf_reduce(e: &AstExpr, s: &mut State) -> Result<Box<Value>> {
    let res_val = ast_expr_pf_reduce(ast_expr_unary_operand(e), s)?;
    Ok(value_sync_create(AstExpr::new_unary(
        res_val.into_sync(),
        ast_expr_unary_op(e),
    )))
}

fn binary_pf_reduce(binary: &BinaryExpr, s: &mut State) -> Result<Box<Value>> {
    let BinaryExpr { e1, op, e2 } = binary;
    let v1 = ast_expr_pf_reduce(e1, s)?;
    let v2 = ast_expr_pf_reduce(e2, s)?;
    // Note: Original leaked v1 and v2.
    Ok(value_sync_create(AstExpr::new_binary(
        value_to_expr(&v1),
        *op,
        value_to_expr(&v2),
    )))
}

fn call_pf_reduce(call: &CallExpr, s: &mut State) -> Result<Box<Value>> {
    let CallExpr {
        fun,
        args: unreduced_args,
    } = call;
    let root = ast_expr_as_identifier(fun);
    let mut reduced_args = Vec::with_capacity(unreduced_args.len());
    for arg in unreduced_args {
        // Note: Original leaked a result and a value here.
        let val = ast_expr_pf_reduce(arg, s)?;
        reduced_args.push(value_to_expr(&val));
    }
    Ok(value_sync_create(AstExpr::new_call(
        AstExpr::new_identifier(root.to_string()),
        reduced_args,
    )))
}

fn structmember_pf_reduce(sm: &StructMemberExpr, s: &mut State) -> Result<Box<Value>> {
    let StructMemberExpr { root, member } = sm;
    let v = ast_expr_pf_reduce(root, s)?;
    if value_isstruct(&v) {
        let obj = value_struct_member(&v, member).unwrap();
        let obj_value = obj.as_value().unwrap();
        return Ok(value_copy(obj_value));
    }
    assert!(value_issync(&v));
    Ok(value_sync_create(AstExpr::new_member(
        v.into_sync(),
        member.to_string(),
    )))
}

pub fn ast_expr_pf_reduce(e: &AstExpr, s: &mut State) -> Result<Box<Value>> {
    match &e.kind {
        AstExprKind::Constant(_) | AstExprKind::StringLiteral(_) | AstExprKind::Identifier(_) => {
            ast_expr_eval(e, s)
        }
        AstExprKind::Unary(_) => unary_pf_reduce(e, s),
        AstExprKind::Binary(binary) => binary_pf_reduce(binary, s),
        AstExprKind::Call(call) => call_pf_reduce(call, s),
        AstExprKind::StructMember(sm) => structmember_pf_reduce(sm, s),
        AstExprKind::Bracketed(inner) => ast_expr_pf_reduce(inner, s),
        _ => panic!(),
    }
}

impl Display for StructMemberExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let StructMemberExpr { root, member } = self;
        if let AstExprKind::Unary(unary) = &root.kind {
            let UnaryExpr { op, arg: inner } = unary;
            assert_eq!(*op, AstUnaryOp::Dereference);

            let e1 = ast_expr_binary_e1(inner);
            let e2 = ast_expr_binary_e2(inner);
            if matches!(e2.kind, AstExprKind::Constant(_)) && ast_expr_as_constant(e2) == 0 {
                write!(f, "{e1}->{member}")
            } else {
                write!(f, "{e1}[{e2}].{member}")
            }
        } else {
            write!(f, "{root}.{member}")
        }
    }
}

impl Display for IncDecExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let root = &self.operand;
        let op = if self.inc { "++" } else { "--" };
        if self.pre {
            write!(f, "{op}{root}")
        } else {
            write!(f, "{root}{op}")
        }
    }
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}(", self.fun)?;
        for (i, arg) in self.args.iter().enumerate() {
            write!(f, "{arg}")?;
            if i + 1 < self.args.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

pub fn ast_expr_inverted_copy(expr: &AstExpr, invert: bool) -> Box<AstExpr> {
    let copy = ast_expr_copy(expr);
    if invert {
        AstExpr::new_unary(copy, AstUnaryOp::Bang)
    } else {
        copy
    }
}

pub fn ast_expr_incdec_to_assignment(incdec: &IncDecExpr) -> AssignmentExpr {
    AssignmentExpr {
        lval: ast_expr_copy(&incdec.operand),
        rval: AstExpr::new_binary(
            ast_expr_copy(&incdec.operand),
            if incdec.inc {
                AstBinaryOp::Addition
            } else {
                AstBinaryOp::Subtraction
            },
            AstExpr::new_constant(1),
        ),
    }
}

impl Display for AllocExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let AllocExpr { kind, arg } = self;
        let kw = match kind {
            AstAllocKind::Alloc => "malloc",
            AstAllocKind::Dealloc => "free",
            AstAllocKind::Clump => "clump",
        };
        write!(f, ".{kw} {arg};")
    }
}

pub fn ast_expr_isdeallocand_assertand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IsDeallocand(assertand) = &expr.kind else {
        panic!();
    };
    assertand
}

pub fn ast_expr_assignment_rval(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!();
    };
    &assignment.rval
}

pub fn ast_expr_copy(expr: &AstExpr) -> Box<AstExpr> {
    Box::new(expr.clone())
}

pub fn ast_expr_binary_e2(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!();
    };
    &binary.e2
}

impl Display for AssignmentExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let AssignmentExpr { lval, rval } = self;
        write!(f, "{lval} = {rval}")
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let BinaryExpr { op, e1, e2 } = self;
        let opstr = match op {
            AstBinaryOp::Eq => "==",
            AstBinaryOp::Ne => "!=",
            AstBinaryOp::Lt => "<",
            AstBinaryOp::Gt => ">",
            AstBinaryOp::Le => "<=",
            AstBinaryOp::Ge => ">=",
            AstBinaryOp::Addition => "+",
            AstBinaryOp::Subtraction => "-",
        };
        write!(f, "{e1}{opstr}{e2}")
    }
}

pub fn ast_expr_binary_e1(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!();
    };
    &binary.e1
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let UnaryExpr { op, arg } = self;
        let c = match op {
            AstUnaryOp::Address => "&",
            AstUnaryOp::Dereference => "*",
            AstUnaryOp::Positive => "+",
            AstUnaryOp::Negative => "-",
            AstUnaryOp::OnesComplement => "~",
            AstUnaryOp::Bang => "!",
        };
        write!(f, "{c}({arg})")
    }
}

pub fn ast_expr_binary_op(expr: &AstExpr) -> AstBinaryOp {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!();
    };
    binary.op
}

pub fn ast_expr_unary_operand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!();
    };
    &unary.arg
}

pub fn ast_expr_unary_op(expr: &AstExpr) -> AstUnaryOp {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!();
    };
    unary.op
}

pub fn ast_expr_getfuncs(expr: &AstExpr) -> Vec<String> {
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

pub fn ast_expr_splits(e: &AstExpr, s: &mut State) -> Result<AstStmtSplits> {
    match &e.kind {
        AstExprKind::Call(call) => call_splits(call, s),
        AstExprKind::Assignment(assign) => ast_expr_splits(&assign.rval, s),
        AstExprKind::Unary(unary) => ast_expr_splits(&unary.arg, s),
        AstExprKind::Binary(binary) => binary_splits(binary, s),
        AstExprKind::IncDec(incdec) => ast_expr_splits(&incdec.operand, s),
        AstExprKind::StructMember(sm) => ast_expr_splits(&sm.root, s),
        AstExprKind::Constant(_)
        | AstExprKind::Identifier(_)
        | AstExprKind::StringLiteral(_)
        | AstExprKind::ArbArg
        | AstExprKind::IsDereferencable(_)
        | AstExprKind::Allocation(_) => Ok(AstStmtSplits { conds: vec![] }),
        _ => panic!(),
    }
}

fn call_splits(call: &CallExpr, state: &mut State) -> Result<AstStmtSplits> {
    let CallExpr { fun, args } = call;
    let name = ast_expr_as_identifier(fun);
    let Some(f) = state.ext().get_func(name) else {
        return Err(Error::new(format!("function: `{name}' not found")));
    };
    let params = f.params();
    let mut s_copy = state_copy(&*state);
    let args = prepare_arguments(args, params, &mut s_copy);
    let ret_type = f.rtype();
    state_pushframe(&mut s_copy, name.to_string(), ret_type);
    prepare_parameters(params, args, name, &mut s_copy)?;
    let mut conds = vec![];
    let abs = f.abstract_block();
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

fn binary_splits(binary: &BinaryExpr, s: &mut State) -> Result<AstStmtSplits> {
    // Note: s1.err and s2.err are ignored in the original.
    let s1 = ast_expr_splits(&binary.e1, s).unwrap_or_default();
    let s2 = ast_expr_splits(&binary.e2, s).unwrap_or_default();
    Ok(AstStmtSplits {
        conds: [s1.conds, s2.conds].concat(),
    })
}

fn ast_expr_call_getfuncs(expr: &AstExpr) -> Vec<String> {
    let AstExprKind::Call(call) = &expr.kind else {
        panic!();
    };
    let mut res = vec![];
    let AstExprKind::Identifier(id) = &call.fun.kind else {
        panic!();
    };
    res.push(id.clone());
    for arg in &call.args {
        res.append(&mut ast_expr_getfuncs(arg));
    }
    res
}

fn calculate_indegrees(g: &FuncGraph) -> InsertionOrderMap<String, usize> {
    let mut indegrees = InsertionOrderMap::new();
    for (key, deps) in g {
        if indegrees.get(key).is_none() {
            indegrees.insert(key.to_string(), 0);
            for dep_key in deps {
                if indegrees.get(dep_key).is_none() {
                    indegrees.insert(dep_key.to_string(), 0);
                }
            }
        }
    }
    for (key, count) in &mut indegrees {
        if let Some(n_arr) = g.get(key) {
            *count += n_arr.len();
        }
    }
    indegrees
}

fn build_indegree_zero(indegrees: &InsertionOrderMap<String, usize>) -> VecDeque<String> {
    let mut indegree_zero = VecDeque::new();
    for (key, val) in indegrees {
        if *val == 0 {
            indegree_zero.push_back(key.clone());
        }
    }
    indegree_zero
}

pub fn topological_order(fname: &str, ext: &Externals) -> Vec<String> {
    let mut order = vec![];
    let g = ast_function_buildgraph(fname, ext);
    // Note: Original leaks indegree_zero.
    let mut indegrees = calculate_indegrees(&g);
    let mut indegree_zero = build_indegree_zero(&indegrees);
    while let Some(curr) = indegree_zero.pop_front() {
        order.push(curr.clone());
        for (key, v) in &g {
            if v.contains(&curr) {
                let count = indegrees.get_mut(key).unwrap();
                *count -= 1;
                if *count == 0 {
                    indegree_zero.push_back(key.to_string());
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

pub fn ast_block_str(b: &AstBlock, indent: &str) -> String {
    let mut sb = String::new();
    for decl in &b.decls {
        str_write!(sb, "{indent}{decl};\n");
    }
    for stmt in &b.stmts {
        let s = ast_stmt_str(stmt);
        str_write!(sb, "{indent}{s}\n");
    }
    sb
}

pub fn ast_block_decls(b: &AstBlock) -> &[Box<AstVariable>] {
    &b.decls
}

pub fn ast_block_stmts(b: &AstBlock) -> &[Box<AstStmt>] {
    &b.stmts
}

pub fn ast_block_isterminal(b: &AstBlock, s: &mut State) -> bool {
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

pub fn ast_stmt_process(stmt: &AstStmt, fname: &str, state: &mut State) -> Result<()> {
    if matches!(stmt.kind, AstStmtKind::CompoundV(_)) {
        if let Err(err) = ast_stmt_verify(stmt, state) {
            let loc = ast_stmt_lexememarker(stmt);
            return Err(Error::new(format!("{loc}: {}", err.msg)));
        }
    }
    if let Err(err) = ast_stmt_exec(stmt, state) {
        let loc = ast_stmt_lexememarker(stmt);
        let err_msg = format!("{loc}:{fname}: cannot exec statement: {}", err.msg);
        return Err(Error::new(err_msg));
    }
    Ok(())
}

#[allow(dead_code)]
pub fn ast_stmt_preprocess(stmt: &AstStmt, state: &mut State) -> Result<Preresult> {
    if ast_stmt_isassume(stmt) {
        return stmt_installprop(stmt, state);
    }
    Ok(Preresult {
        is_contradiction: false,
    })
}

pub fn ast_stmt_labelled_stmt(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!();
    };
    &labelled.stmt
}

fn labelled_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    assert!(ast_stmt_ispre(stmt));
    let setup = ast_stmt_labelled_stmt(stmt);
    if should_setup {
        ast_stmt_absexec(setup, state, should_setup)?;
    }
    Ok(())
}

pub fn ast_stmt_copy(stmt: &AstStmt) -> Box<AstStmt> {
    Box::new(stmt.clone())
}

fn labelled_setupabsexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    ast_stmt_absexec(stmt, state, true)
}

fn sel_setupabsexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return stmt_setupabsexec(ast_stmt_sel_body(stmt), state);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

fn comp_setupabsexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    assert_eq!(ast_block_decls(b).len(), 0);
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

fn stmt_setupabsexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Expr(_) | AstStmtKind::Allocation(_) | AstStmtKind::Jump(_) => Ok(()),
        AstStmtKind::Labelled(_) => labelled_setupabsexec(stmt, state),
        AstStmtKind::Selection(_) => sel_setupabsexec(stmt, state),
        AstStmtKind::Compound(_) => comp_setupabsexec(stmt, state),
        _ => panic!(),
    }
}

pub fn ast_stmt_setupabsexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    match stmt.kind {
        AstStmtKind::Selection(_) => stmt_setupabsexec(stmt, state),
        _ => Ok(()),
    }
}

pub fn ast_stmt_isassume(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    labelled.label == "assume"
}

fn stmt_installprop(stmt: &AstStmt, state: &mut State) -> Result<Preresult> {
    ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state)
}

pub fn ast_stmt_ispre(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    labelled.label == "setup"
}

fn stmt_v_block_verify(v_block_stmt: &AstStmt, state: &mut State) -> Result<()> {
    let b = ast_stmt_as_v_block(v_block_stmt);
    assert_eq!(ast_block_decls(b).len(), 0);
    for stmt in &b.stmts {
        ast_stmt_verify(stmt, state)?;
    }
    Ok(())
}

fn stmt_expr_verify(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let expr = ast_stmt_as_expr(stmt);
    if ast_expr_decide(expr, state) {
        Ok(())
    } else {
        Err(Error::new("cannot verify statement".to_string()))
    }
}

fn iter_empty(stmt: &AstStmt, state: &mut State) -> bool {
    if let Err(err) = ast_stmt_exec(ast_stmt_iter_init(stmt), state) {
        panic!("{err}");
    }
    !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state)
}

fn stmt_iter_verify(stmt: &AstStmt, state: &mut State) -> Result<()> {
    if iter_empty(stmt, state) {
        return Ok(());
    }
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!(body.kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_decls(block).len(), 0);
    assert_eq!(block.stmts.len(), 1);
    let assertion = ast_stmt_as_expr(&block.stmts[0]);
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    if !ast_expr_rangedecide(assertion, lw, up, state) {
        return Err(Error::new("could not verify".to_string()));
    }
    Ok(())
}

pub fn ast_stmt_verify(stmt: &AstStmt, state: &mut State) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(()),
        AstStmtKind::CompoundV(_) => stmt_v_block_verify(stmt, state),
        AstStmtKind::Expr(_) => stmt_expr_verify(stmt, state),
        AstStmtKind::Iteration(_) => stmt_iter_verify(stmt, state),
        _ => panic!(),
    }
}

fn stmt_compound_exec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    assert_eq!(ast_block_decls(b).len(), 0);
    for stmt in &b.stmts {
        ast_stmt_exec(stmt, state)?;
        if ast_stmt_isterminal(stmt, state) {
            break;
        }
    }
    Ok(())
}

fn stmt_sel_exec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return ast_stmt_exec(ast_stmt_sel_body(stmt), state);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

fn iter_neteffect(iter: &AstStmt) -> Option<Box<AstStmt>> {
    let abs = ast_stmt_iter_abstract(iter);
    let nstmts = ast_block_stmts(abs).len();
    if nstmts == 0 {
        return None;
    }
    assert_eq!(ast_block_decls(abs).len(), 0);
    assert_eq!(nstmts, 1);
    // Note: Original passes NULL lexeme marker to these two constructors. In the Rust version, the
    // lexeme marker isn't nullable. It isn't worth warping the universe for this hack, so we dig
    // up with some phony locations.
    Some(AstStmt::new_iter(
        Box::new(ast_stmt_lexememarker(iter).clone()),
        ast_stmt_copy(ast_stmt_iter_init(iter)),
        ast_stmt_copy(ast_stmt_iter_cond(iter)),
        ast_expr_copy(ast_stmt_iter_iter(iter)),
        ast_block_create(vec![], vec![]),
        AstStmt::new_compound(
            Box::new(ast_stmt_lexememarker(ast_stmt_iter_body(iter)).clone()),
            Box::new(ast_stmt_iter_abstract(iter).clone()),
        ),
        false,
    ))
}

fn stmt_iter_exec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    if let Some(neteffect) = iter_neteffect(stmt) {
        ast_stmt_absexec(&neteffect, state, true)?;
    }
    Ok(())
}

fn stmt_jump_exec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    // Note: jump_rv may be null. Error in original.
    let rv_val = ast_expr_eval(
        ast_stmt_jump_rv(stmt).expect("unsupported: return without value"),
        state,
    )?;
    let obj = state_getresult(state);
    obj.assign(Some(value_copy(&rv_val)));
    Ok(())
}

pub fn ast_stmt_exec(stmt: &AstStmt, state: &mut State) -> Result<()> {
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

fn ast_stmt_jump_sprint(jump: &AstJumpStmt, b: &mut String) {
    // Note: jump.rv can be null. Error in the original.
    let rv = jump.rv.as_ref().unwrap();
    str_write!(*b, "return {rv};\n");
}

pub fn ast_stmt_iter_abstract(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.abstract_
}

pub fn ast_stmt_iter_iter(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.iter
}

pub fn ast_stmt_lexememarker(stmt: &AstStmt) -> &LexemeMarker {
    &stmt.loc
}

fn ast_stmt_iter_sprint(iteration: &AstIterationStmt, b: &mut String) {
    let init = ast_stmt_str(&iteration.init);
    let cond = ast_stmt_str(&iteration.cond);
    let body = ast_stmt_str(&iteration.body);
    let iter = &iteration.iter;
    let abs = ast_block_str(&iteration.abstract_, "\t");
    str_write!(*b, "for ({init} {cond} {iter}) [{abs}] {{ {body} }}");
}

pub fn ast_stmt_str(stmt: &AstStmt) -> String {
    let mut b = String::new();
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
    b
}

pub fn ast_stmt_iter_cond(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.cond
}

pub fn ast_stmt_iter_init(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.init
}

fn sel_isterminal(stmt: &AstStmt, s: &mut State) -> bool {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), s).unwrap();
    if decision {
        return ast_stmt_isterminal(ast_stmt_sel_body(stmt), s);
    }
    false
}

fn comp_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    for stmt in &b.stmts {
        ast_stmt_absexec(stmt, state, should_setup)?;
    }
    Ok(())
}

pub fn ast_stmt_as_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Compound(block) = &stmt.kind else {
        panic!();
    };
    block
}

pub fn ast_stmt_jump_rv(stmt: &AstStmt) -> Option<&AstExpr> {
    let AstStmtKind::Jump(jump) = &stmt.kind else {
        panic!();
    };
    jump.rv.as_deref()
}

fn jump_absexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    // Note: Original leaks the expression to avoid a double free.
    let expr = AstExpr::new_assignment(
        AstExpr::new_identifier("return".to_string()),
        // Note: jump_rv can be null. Error in original.
        ast_expr_copy(ast_stmt_jump_rv(stmt).unwrap()),
    );
    ast_expr_absexec(&expr, state)?;
    Ok(())
}

pub fn ast_stmt_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(()),
        AstStmtKind::Labelled(_) => labelled_absexec(stmt, state, should_setup),
        AstStmtKind::Expr(_) => {
            let _ = ast_expr_absexec(ast_stmt_as_expr(stmt), state)?;
            Ok(())
        }
        AstStmtKind::Selection(_) => sel_absexec(stmt, state, should_setup),
        AstStmtKind::Iteration(_) => iter_absexec(stmt, state),
        AstStmtKind::Compound(_) => comp_absexec(stmt, state, should_setup),
        AstStmtKind::Jump(_) => jump_absexec(stmt, state),
        _ => panic!(),
    }
}

fn ast_stmt_sel_sprint(stmt: &AstStmt, b: &mut String) {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    let cond = &selection.cond;
    let body = ast_stmt_str(&selection.body);
    str_write!(*b, "if ({cond}) {{ {body} }}");
    if let Some(nest_stmt) = &selection.nest {
        let nest = ast_stmt_str(nest_stmt);
        str_write!(*b, " else {nest}");
    }
}

pub fn ast_stmt_isterminal(stmt: &AstStmt, s: &mut State) -> bool {
    match &stmt.kind {
        AstStmtKind::Jump(jump) => jump.kind == AstJumpKind::Return,
        AstStmtKind::Compound(block) => ast_block_isterminal(block, s),
        AstStmtKind::Selection(_) => sel_isterminal(stmt, s),
        _ => false,
    }
}

impl AstStmt {
    fn new(loc: Box<LexemeMarker>, kind: AstStmtKind) -> Box<Self> {
        Box::new(AstStmt { kind, loc })
    }

    pub fn new_nop(loc: Box<LexemeMarker>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Nop)
    }

    pub fn new_labelled(loc: Box<LexemeMarker>, label: String, substmt: Box<AstStmt>) -> Box<Self> {
        AstStmt::new(
            loc,
            AstStmtKind::Labelled(AstLabelledStmt {
                label,
                stmt: substmt,
            }),
        )
    }

    pub fn new_compound(loc: Box<LexemeMarker>, b: Box<AstBlock>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Compound(b))
    }

    pub fn new_compound_v(loc: Box<LexemeMarker>, b: Box<AstBlock>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::CompoundV(b))
    }

    pub fn new_expr(loc: Box<LexemeMarker>, expr: Box<AstExpr>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Expr(expr))
    }

    pub fn new_sel(
        loc: Box<LexemeMarker>,
        isswitch: bool,
        cond: Box<AstExpr>,
        body: Box<AstStmt>,
        nest: Option<Box<AstStmt>>,
    ) -> Box<Self> {
        assert!(!isswitch);
        AstStmt::new(
            loc,
            AstStmtKind::Selection(AstSelectionStmt {
                isswitch,
                cond,
                body,
                nest,
            }),
        )
    }

    pub fn new_iter(
        loc: Box<LexemeMarker>,
        init: Box<AstStmt>,
        cond: Box<AstStmt>,
        iter: Box<AstExpr>,
        abstract_: Box<AstBlock>,
        body: Box<AstStmt>,
        as_iteration_e: bool,
    ) -> Box<Self> {
        let iter = AstIterationStmt {
            init,
            cond,
            iter,
            body,
            abstract_,
        };
        AstStmt::new(
            loc,
            if as_iteration_e {
                AstStmtKind::IterationE(iter)
            } else {
                AstStmtKind::Iteration(iter)
            },
        )
    }

    pub fn new_jump(
        loc: Box<LexemeMarker>,
        kind: AstJumpKind,
        rv: Option<Box<AstExpr>>,
    ) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Jump(AstJumpStmt { kind, rv }))
    }
}

pub fn sel_decide(control: &AstExpr, state: &mut State) -> Result<bool> {
    // This value is leaked in most paths through the original. In the sync case, part of it is
    // used and freed.
    let v = ast_expr_pf_reduce(control, state)?;
    if value_issync(&v) {
        // Note: Original leaks `sync`.
        let v_str = format!("{v}");
        let sync = v.into_sync();
        let p = state.props();
        if p.get(&sync) {
            Ok(true)
        } else if p.contradicts(&sync) {
            Ok(false)
        } else {
            Err(Error::new(format!(
                "`{control}' with value `{v_str}' is undecidable"
            )))
        }
    } else if value_isconstant(&v) {
        Ok(value_as_constant(&v) != 0)
    } else if value_isint(&v) {
        let zero = value_int_create(0);
        Ok(!value_equal(&zero, &v))
    } else {
        Err(Error::new(format!(
            "`{control}' with value `{v}' is undecidable"
        )))
    }
}

fn ast_stmt_compound_sprint(compound: &AstBlock, b: &mut String) {
    let s = ast_block_str(compound, "\t");
    str_write!(*b, "{s}");
}

fn ast_stmt_expr_sprint(expr: &AstExpr, b: &mut String) {
    str_write!(*b, "{expr};");
}

fn ast_stmt_nop_sprint(b: &mut String) {
    str_write!(*b, ";");
}

pub fn ast_stmt_iter_body(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.body
}

fn hack_alloc_from_neteffect(stmt: &AstStmt) -> &AstExpr {
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!(body.kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_decls(block).len(), 0);
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

fn ast_stmt_labelled_sprint(stmt: &AstStmt, b: &mut String) {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!();
    };
    let s = ast_stmt_str(&labelled.stmt);
    str_write!(*b, "{}: {s}", labelled.label);
}

fn sel_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

pub fn ast_stmt_sel_nest(stmt: &AstStmt) -> Option<&AstStmt> {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    selection.nest.as_deref()
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

fn iter_absexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let alloc = hack_alloc_from_neteffect(stmt);
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    ast_expr_alloc_rangeprocess(alloc, lw, up, state)?;
    Ok(())
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

pub fn ast_stmt_getfuncs(stmt: &AstStmt) -> Vec<String> {
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

pub fn ast_stmt_splits(stmt: &AstStmt, s: &mut State) -> Result<AstStmtSplits> {
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

fn stmt_sel_splits(selection: &AstSelectionStmt, s: &mut State) -> Result<AstStmtSplits> {
    // Note: The original asserts that no error occurred, rather than propagate.
    // Note: Original leaks `v`.
    let v = ast_expr_pf_reduce(&selection.cond, s).unwrap();
    let e = value_to_expr(&v);
    let conds = if condexists(&e, s) || value_isconstant(&v) {
        vec![]
    } else {
        vec![e]
    };
    Ok(AstStmtSplits { conds })
}

fn condexists(cond: &AstExpr, s: &mut State) -> bool {
    // Note: Original leaks `val`.
    let val = ast_expr_pf_reduce(cond, s).unwrap();
    let reduced = value_to_expr(&val);
    let p = s.props();
    p.get(&reduced) || p.contradicts(&reduced)
}

fn ast_stmt_selection_getfuncs(selection: &AstSelectionStmt) -> Vec<String> {
    let cond_arr = ast_expr_getfuncs(&selection.cond);
    let body_arr = ast_stmt_getfuncs(&selection.body);
    let nest_arr = if let Some(nest) = &selection.nest {
        ast_stmt_getfuncs(nest)
    } else {
        vec![]
    };
    [cond_arr, body_arr, nest_arr].concat()
}

fn ast_stmt_iteration_getfuncs(iteration: &AstIterationStmt) -> Vec<String> {
    [
        ast_stmt_getfuncs(&iteration.init),
        ast_stmt_getfuncs(&iteration.cond),
        ast_stmt_getfuncs(&iteration.body),
        ast_expr_getfuncs(&iteration.iter),
    ]
    .concat()
}

fn ast_stmt_compound_getfuncs(block: &AstBlock) -> Vec<String> {
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

pub fn ast_type_create(base: AstTypeBase, modifiers: AstTypeModifiers) -> Box<AstType> {
    Box::new(AstType { base, modifiers })
}

pub fn ast_type_create_ptr(referent: Box<AstType>) -> Box<AstType> {
    ast_type_create(AstTypeBase::Pointer(Some(referent)), 0)
}

pub fn ast_type_create_voidptr() -> Box<AstType> {
    ast_type_create(AstTypeBase::Pointer(None), 0)
}

#[allow(dead_code)]
pub fn ast_type_create_arr(base: Box<AstType>, length: c_int) -> Box<AstType> {
    ast_type_create(
        AstTypeBase::Array(AstArrayType {
            type_: base,
            length,
        }),
        0,
    )
}

pub fn ast_type_create_struct(
    tag: Option<String>,
    members: Option<Box<Vec<Box<AstVariable>>>>,
) -> Box<AstType> {
    ast_type_create(AstTypeBase::Struct(AstStructType { tag, members }), 0)
}

pub fn ast_type_create_userdef(name: String) -> Box<AstType> {
    ast_type_create(AstTypeBase::UserDefined(name), 0)
}

pub fn ast_type_vconst(t: &AstType, s: &mut State, comment: &str, persist: bool) -> Box<Value> {
    match &t.base {
        AstTypeBase::Int => value_int_indefinite_create(),
        AstTypeBase::Pointer(_) => value_ptr_indefinite_create(),
        AstTypeBase::UserDefined(name) => {
            // Note: Bumps a reference count as a lifetime hack.
            let ext = s.externals_arc();
            let type_ = ext.get_typedef(name).unwrap();
            ast_type_vconst(
                // Note: Original does not null-check here.
                type_, s, comment, persist,
            )
        }
        AstTypeBase::Struct(_) => value_struct_indefinite_create(t, s, comment, persist),
        _ => panic!(),
    }
}

pub fn ast_type_isstruct(t: &AstType) -> bool {
    matches!(t.base, AstTypeBase::Struct(_))
}

pub fn ast_type_struct_complete<'a>(t: &'a AstType, ext: &'a Externals) -> Option<&'a AstType> {
    if ast_type_struct_members(t).is_some() {
        return Some(t);
    }
    let Some(tag) = ast_type_struct_tag(t) else {
        panic!();
    };
    ext.get_struct(tag)
}

pub fn ast_type_struct_members(t: &AstType) -> Option<&[Box<AstVariable>]> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!();
    };
    s.members.as_ref().map(|v| v.as_slice())
}

pub fn ast_type_struct_tag(t: &AstType) -> Option<&str> {
    let AstTypeBase::Struct(s) = &t.base else {
        panic!();
    };
    s.tag.as_deref()
}

pub fn ast_type_create_struct_anonym(members: Vec<Box<AstVariable>>) -> Box<AstType> {
    ast_type_create_struct(None, Some(Box::new(members)))
}

pub fn ast_type_create_struct_partial(tag: String) -> Box<AstType> {
    ast_type_create_struct(Some(tag), None)
}

pub fn ast_type_mod_or(t: &mut AstType, m: AstTypeModifiers) {
    t.modifiers |= m;
}

pub fn ast_type_istypedef(t: &AstType) -> bool {
    t.modifiers & MOD_TYPEDEF != 0
}

pub fn ast_type_copy(t: &AstType) -> Box<AstType> {
    Box::new(t.clone())
}

impl Display for AstType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", mod_str(self.modifiers))?;
        match &self.base {
            AstTypeBase::Pointer(Some(ptr_type)) => {
                let space = !matches!(ptr_type.base, AstTypeBase::Pointer(_));
                write!(f, "{ptr_type}{}*", if space { " " } else { "" })
            }
            AstTypeBase::Array(AstArrayType { type_, length }) => write!(f, "{type_}[{length}]"),
            AstTypeBase::Struct(s) => write!(f, "{s}"),
            AstTypeBase::UserDefined(name) => write!(f, "{name}"),
            AstTypeBase::Void => write!(f, "void"),
            AstTypeBase::Char => write!(f, "char"),
            AstTypeBase::Short => write!(f, "short"),
            AstTypeBase::Int => write!(f, "int"),
            AstTypeBase::Long => write!(f, "long"),
            AstTypeBase::Float => write!(f, "float"),
            AstTypeBase::Double => write!(f, "double"),
            AstTypeBase::Signed => write!(f, "signed"),
            AstTypeBase::Unsigned => write!(f, "unsigned"),
            AstTypeBase::Pointer(None) | AstTypeBase::Union(_) | AstTypeBase::Enum => panic!(),
        }
    }
}

fn mod_str(modifiers: AstTypeModifiers) -> String {
    let modstr: [&'static str; 7] = [
        "typedef", "extern", "static", "auto", "register", "const", "volatile",
    ];
    let mut b = String::new();
    for (i, name) in modstr.iter().enumerate() {
        if 1 << i & modifiers != 0 {
            // Note: The original does some unnecessary work to decide whether to add a space, but
            // the answer is always yes, add the space. This is on purpose because of how this
            // function is used.
            str_write!(b, "{name} ");
        }
    }
    b
}

impl Display for AstStructType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        assert!(self.tag.is_some() || self.members.is_some());
        write!(f, "struct ")?;
        if let Some(tag) = &self.tag {
            write!(f, "{tag}")?;
        }
        let Some(members) = self.members.as_ref() else {
            return Ok(());
        };
        write!(f, " {{ ")?;
        for field in members.iter() {
            write!(f, "{field}; ")?;
        }
        write!(f, "}}")
    }
}

pub fn ast_type_ptr_type(t: &AstType) -> Option<&AstType> {
    let AstTypeBase::Pointer(ptr_type) = &t.base else {
        panic!();
    };
    ptr_type.as_deref()
}

pub fn ast_variable_create(name: String, ty: Box<AstType>) -> Box<AstVariable> {
    // Note: In the original, this function can take a null name and create a variable with null name.
    // This is actually done for the arguments in function declarations like `void fclose(FILE*);`.
    Box::new(AstVariable { name, type_: ty })
}

pub fn ast_variable_arr_copy(v: &[Box<AstVariable>]) -> Vec<Box<AstVariable>> {
    v.to_vec()
}

impl Display for AstVariable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let AstVariable { type_, name } = self;
        write!(f, "{type_} {name}")
    }
}

pub fn ast_variable_name(v: &AstVariable) -> &str {
    &v.name
}

pub fn ast_variable_type(v: &AstVariable) -> &AstType {
    &v.type_
}

pub fn ast_function_create(
    is_axiom: bool,
    ret: Box<AstType>,
    name: String,
    params: Vec<Box<AstVariable>>,
    abstract_0: Box<AstBlock>,
    body: Option<SemiBox<AstBlock>>,
) -> Box<AstFunction> {
    Box::new(AstFunction {
        is_axiom,
        ret,
        name,
        params,
        abstract_: abstract_0,
        body,
    })
}

impl<'ast> AstFunction<'ast> {
    #[allow(dead_code)]
    pub fn str(&self) -> String {
        let mut b = String::new();
        if self.is_axiom {
            str_write!(b, "axiom ");
        }
        str_write!(b, "{}\n", self.ret);
        str_write!(b, "{}(", self.name);
        for (i, param) in self.params.iter().enumerate() {
            let space = if i + 1 < self.params.len() { ", " } else { "" };
            str_write!(b, "{param}{space}");
        }
        let abs = ast_block_str(&self.abstract_, "\t");
        str_write!(b, ") ~ [\n{abs}]");
        if let Some(body) = &self.body {
            let body = ast_block_str(body, "\t");
            str_write!(b, "{{\n{body}}}");
        } else {
            str_write!(b, ";");
        }
        str_write!(b, "\n");
        b
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn copy(&self) -> Box<AstFunction<'ast>> {
        Box::new(self.clone())
    }

    pub fn is_axiom(&self) -> bool {
        self.is_axiom
    }

    pub fn is_proto(&self) -> bool {
        self.body.is_none()
    }

    pub fn abs_is_empty(&self) -> bool {
        ast_block_decls(&self.abstract_).is_empty() && ast_block_stmts(&self.abstract_).is_empty()
    }

    pub fn rtype(&self) -> &AstType {
        &self.ret
    }

    #[allow(dead_code)]
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

pub fn ast_function_protostitch(f: &mut AstFunction, ext: &Externals) {
    if let Some(proto) = ext.get_func(&f.name) {
        f.abstract_ = proto.abstract_.clone();
    }
}

pub fn ast_function_verify(f: &AstFunction, ext: Arc<Externals>) -> Result<()> {
    let mut state = state_create(f.name().to_string(), ext, f.rtype());
    ast_function_initparams(f, &mut state)?;
    path_absverify_withstate(f, &mut state)?;
    Ok(())
}

fn path_absverify_withstate(f: &AstFunction, state: &mut State) -> Result<()> {
    let abs = f.abstract_block();
    for var in &abs.decls {
        state_declare(state, var, false);
    }
    path_absverify(f, state, 0)
}

fn path_absverify(f: &AstFunction, state: &mut State, index: usize) -> Result<()> {
    let abs = f.abstract_block();
    for i in index..abs.stmts.len() {
        let stmt = &abs.stmts[i];
        let splits = ast_stmt_splits(stmt, state)?;
        if !splits.conds.is_empty() {
            return split_paths_absverify(f, state, i, &splits);
        }
        if !ast_stmt_ispre(stmt) {
            ast_stmt_absexec(stmt, state, true)?;
        }
    }
    abstract_audit(f, state)?;
    Ok(())
}

pub fn ast_function_initparams(f: &AstFunction, s: &mut State) -> Result<()> {
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

fn ast_function_precondsinit(f: &AstFunction, s: &mut State) -> Result<()> {
    let pre_stmt = f.preconditions()?;
    if let Some(stmt) = pre_stmt {
        ast_stmt_absexec(stmt, s, true)?;
    }
    Ok(())
}

fn inititalise_param(param: &AstVariable, state: &mut State) -> Result<()> {
    let state: *mut State = state;
    unsafe {
        let name = ast_variable_name(param);
        let t = ast_variable_type(param);
        let obj = state_getobject(&mut *state, name).unwrap();
        if !obj.has_value() {
            // XXX FIXME: dereferencing `state` again here definitely invalidates `obj`
            let val = state_vconst(&mut *state, t, Some(name), true);
            obj.assign(Some(val));
        }
    }
    Ok(())
}

fn abstract_audit(f: &AstFunction, abstract_state: &mut State) -> Result<()> {
    let mut actual_state = state_create_withprops(
        f.name().to_string(),
        (*abstract_state).externals_arc(),
        f.rtype(),
        abstract_state.props().clone(),
    );
    ast_function_initparams(f, &mut actual_state).unwrap();
    ast_function_setupabsexec(f, &mut actual_state)?;
    abstract_auditwithstate(f, &mut actual_state, abstract_state)?;
    Ok(())
}

fn ast_function_setupabsexec(f: &AstFunction, state: &mut State) -> Result<()> {
    for stmt in &f.abstract_.stmts {
        ast_stmt_setupabsexec(stmt, state)?;
    }
    Ok(())
}

fn abstract_auditwithstate(
    f: &AstFunction,
    actual_state: &mut State,
    abstract_state: &mut State,
) -> Result<()> {
    for decl in &f.body.as_ref().unwrap().decls {
        state_declare(actual_state, decl, false);
    }
    path_verify(f, actual_state, 0, abstract_state)
}

fn path_verify(
    f: &AstFunction,
    actual_state: &mut State,
    index: usize,
    abstract_state: &mut State,
) -> Result<()> {
    let fname = f.name();
    let stmts = &f.body.as_ref().unwrap().stmts;
    #[allow(clippy::needless_range_loop)]
    for i in index..stmts.len() {
        let stmt = &stmts[i];
        // splits.err is ignored in the original
        let splits = ast_stmt_splits(stmt, actual_state);
        match splits {
            Ok(splits) if !splits.conds.is_empty() => {
                return split_paths_verify(f, actual_state, i, &splits, abstract_state);
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
        vprintln!("actual: {}", state_str(&*actual_state));
        return Err(Error::new(format!("{fname}: garbage on heap")));
    }
    let equiv: bool = state_equal(&*actual_state, &*abstract_state);
    if !equiv {
        return Err(Error::new(format!(
            "{fname}: actual and abstract states differ",
        )));
    }
    Ok(())
}

pub fn ast_function_absexec(f: &AstFunction, state: &mut State) -> Result<Option<Box<Value>>> {
    for decl in &f.abstract_.decls {
        state_declare(state, decl, false);
    }
    for stmt in &f.abstract_.stmts {
        ast_stmt_absexec(stmt, state, false)?;
    }
    let obj = state_getresult(state);
    // Note: In the original, this function (unlike the other absexec functions) returned a
    // borrowed value which the caller cloned. Not a big difference.
    Ok(obj.as_value().map(value_copy))
}

fn split_path_verify(
    f: &AstFunction,
    actual_state: &mut State,
    index: usize,
    cond: &AstExpr,
    abstract_state: &mut State,
) -> Result<()> {
    let paths = body_paths(f, index, cond);
    assert_eq!(paths.len(), 2);
    // Note: Original leaks both functions to avoid triple-freeing the body.
    // We borrow instead.
    for (i, f) in paths.into_iter().enumerate() {
        let mut actual_copy = state_copywithname(&*actual_state, (*f).name().to_string());
        let mut abstract_copy = state_copywithname(&*abstract_state, (*f).name().to_string());
        // Note: Original leaks `expr`.
        let expr = ast_expr_inverted_copy(cond, i == 1);
        let r = ast_expr_assume(&expr, &mut actual_copy)?;
        if !r.is_contradiction {
            path_verify(&f, &mut actual_copy, index, &mut abstract_copy)?;
        }
        // Note: Original leaks both state copies.
    }
    Ok(())
}

type FuncGraph = InsertionOrderMap<String, Vec<String>>;

type DedupSet<'a> = InsertionOrderMap<String, ()>;

fn recurse_buildgraph(g: &mut FuncGraph, dedup: &mut DedupSet, fname: &str, ext: &Externals) {
    let mut local_dedup = vec![];
    if dedup.get(fname).is_some() {
        return;
    }
    dedup.insert(fname.to_string(), ());
    let Some(f) = ext.get_func(fname) else {
        eprintln!("function `{fname}' is not declared");
        process::exit(1);
    };
    if f.is_axiom {
        return;
    }
    let body = f.body.as_deref().unwrap();
    let mut val = vec![];
    for stmt in &body.stmts {
        let farr = ast_stmt_getfuncs(stmt);

        for func in farr {
            if !local_dedup.contains(&func) {
                // Note: The original avoids some of these string copies.
                local_dedup.push(func.clone());
                let f = ext.get_func(&func).unwrap();
                if !f.is_axiom {
                    val.push(func.to_string());
                }
                recurse_buildgraph(g, dedup, &func, ext);
            }
        }
    }
    g.insert(fname.to_string(), val);
}

fn abstract_paths<'origin>(
    f: &'origin AstFunction,
    _index: usize,
    cond: &AstExpr,
) -> Vec<Box<AstFunction<'origin>>> {
    let f_true = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_str(), cond),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    // Note: Original leaks inv_assumption, but I think unintentionally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_str(), &inv_assumption),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    vec![f_true, f_false]
}

fn split_path_absverify(
    f: &AstFunction,
    state: &mut State,
    index: usize,
    cond: &AstExpr,
) -> Result<()> {
    let paths = abstract_paths(f, index, cond);
    assert_eq!(paths.len(), 2);
    for (i, f) in paths.into_iter().enumerate() {
        // Note: Original does not copy f.name here -- which should be a double free, but s_copy is
        // leaked.
        let mut s_copy = state_copywithname(&*state, f.name().to_string());
        // Note: Original leaks `inv` but I think accidentally.
        let inv = ast_expr_inverted_copy(cond, i == 1);
        let r = ast_expr_assume(&inv, &mut s_copy)?;
        if !r.is_contradiction {
            path_absverify(&f, &mut s_copy, index)?;
        }
    }
    Ok(())
}

fn split_paths_absverify(
    f: &AstFunction,
    state: &mut State,
    index: usize,
    splits: &AstStmtSplits,
) -> Result<()> {
    for cond in &splits.conds {
        split_path_absverify(f, state, index, cond)?;
    }
    Ok(())
}

pub fn ast_function_buildgraph(fname: &str, ext: &Externals) -> FuncGraph {
    let mut dedup = InsertionOrderMap::new();
    let mut g = InsertionOrderMap::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    g
}

fn split_name(name: &str, assumption: &AstExpr) -> String {
    format!("{name} | {assumption}")
}

fn split_paths_verify(
    f: &AstFunction,
    actual_state: &mut State,
    index: usize,
    splits: &AstStmtSplits,
    abstract_state: &mut State,
) -> Result<()> {
    for cond in &splits.conds {
        split_path_verify(f, actual_state, index, cond, abstract_state)?;
    }
    Ok(())
}

fn body_paths<'origin>(
    f: &'origin AstFunction,
    _index: usize,
    cond: &AstExpr,
) -> Vec<Box<AstFunction<'origin>>> {
    let f_true = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_str(), cond),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    // Note: Original leaks `inv_assumption` but I think accidentally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_str(), &inv_assumption),
        ast_variable_arr_copy(&f.params),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    vec![f_true, f_false]
}

pub fn ast_functiondecl_create(f: Box<AstFunction<'static>>) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: AstExternDeclKind::Function(f),
    })
}

pub fn ast_externdecl_as_function_mut(
    decl: &mut AstExternDecl,
) -> Option<&mut AstFunction<'static>> {
    match &mut decl.kind {
        AstExternDeclKind::Function(f) => Some(f),
        _ => None,
    }
}

pub fn ast_externdecl_as_function(decl: &AstExternDecl) -> Option<&AstFunction> {
    match &decl.kind {
        AstExternDeclKind::Function(f) => Some(f),
        _ => None,
    }
}

pub fn ast_decl_create(name: String, t: Box<AstType>) -> Box<AstExternDecl> {
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

#[allow(clippy::boxed_local)]
pub fn ast_externdecl_install(decl: Box<AstExternDecl>, ext: &mut Externals) {
    match decl.kind {
        AstExternDeclKind::Function(f) => {
            ext.declare_func(f);
        }
        AstExternDeclKind::Variable(v) => {
            let name = ast_variable_name(&v).to_string();
            ext.declare_var(name, v);
        }
        AstExternDeclKind::Typedef(typedef) => {
            ext.declare_typedef(typedef.name.to_string(), typedef.type_0);
        }
        AstExternDeclKind::Struct(s) => {
            ext.declare_struct(s);
        }
    }
}

pub fn parse_int(s: &str) -> c_int {
    s.parse().expect("parse error")
}

pub fn parse_char(s: &str) -> c_char {
    // Note: Original also assumes these character literals have the same numeric values on the
    // target as in XR0, a decent bet for ASCII at least.
    assert!(s.starts_with('\'') && s.ends_with('\''));
    let s = &s[1..s.len() - 1];
    if let Some(stripped) = s.strip_prefix('\\') {
        parse_escape(stripped)
    } else {
        s.chars().next().expect("invalid char literal") as u32 as c_char
    }
}

pub fn parse_escape(c: &str) -> c_char {
    match c {
        "0" => 0,
        "t" => '\t' as u32 as c_char,
        "n" => '\t' as u32 as c_char, // Note: '\t' rather than '\n', a bug in the original.
        _ => {
            panic!("unrecognized char escape sequence: {:?}", c);
        }
    }
}

pub fn ast_topological_order(fname: &str, ext: &Externals) -> Vec<String> {
    topological_order(fname, ext)
}

pub fn ast_protostitch(f: &mut AstFunction<'static>, ext: &Externals) {
    ast_function_protostitch(f, ext)
}
