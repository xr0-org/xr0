use std::fmt::{self, Display, Formatter};

use crate::math::{math_eq, math_ge, math_gt, math_le, math_lt, MathAtom, MathExpr};

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

// Note: Original assigns these enumerators power-of-two values.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstAllocKind {
    Alloc,
    Dealloc,
    Clump,
}

// Note: Original assigns these enumerators power-of-two values.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstUnaryOp {
    Address,
    Dereference,
    Positive,
    Negative,
    OnesComplement,
    Bang,
}

// Note: Original assigns these enumerators power-of-two values.
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
    pub kind: AstExprKind,
}

#[derive(Clone)]
pub enum AstExprKind {
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

#[derive(Clone)]
pub struct ConstantExpr {
    pub constant: c_int,
    pub is_char: bool,
}

#[derive(Clone)]
pub struct CallExpr {
    pub fun: Box<AstExpr>,
    pub args: Vec<Box<AstExpr>>,
}

#[derive(Clone)]
pub struct IncDecExpr {
    pub operand: Box<AstExpr>,
    pub inc: bool,
    pub pre: bool,
}

#[derive(Clone)]
pub struct StructMemberExpr {
    pub root: Box<AstExpr>,
    pub member: String,
}

#[derive(Clone)]
pub struct UnaryExpr {
    pub op: AstUnaryOp,
    pub arg: Box<AstExpr>,
}

#[derive(Clone)]
pub struct BinaryExpr {
    pub op: AstBinaryOp,
    pub e1: Box<AstExpr>,
    pub e2: Box<AstExpr>,
}

#[derive(Clone)]
pub struct AssignmentExpr {
    pub lval: Box<AstExpr>,
    pub rval: Box<AstExpr>,
}

#[derive(Clone)]
pub struct AllocExpr {
    pub kind: AstAllocKind,
    pub arg: Box<AstExpr>,
}

//=ast_expr_str
impl Display for AstExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            AstExprKind::Identifier(id) => write!(f, "{id}"),
            AstExprKind::Constant(constant) => write!(f, "{constant}"),
            AstExprKind::StringLiteral(s) => write!(f, "{s:?}"), // XXX FIXME: this may depart from C syntax
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

//=ast_expr_constant_str_build
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

//=ast_expr_call_str_build
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

//=ast_expr_incdec_str_build
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

//=ast_expr_member_str_build
//=ast_expr_member_deref_str_build
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

//=ast_expr_unary_str_build
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

//=ast_expr_binary_str_build
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

//=ast_expr_assignment_str_build
impl Display for AssignmentExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let AssignmentExpr { lval, rval } = self;
        write!(f, "{lval} = {rval}")
    }
}

//=ast_expr_alloc_str_build
impl Display for AllocExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let AllocExpr { kind, arg } = self;
        let kw = match kind {
            AstAllocKind::Alloc => "malloc",
            AstAllocKind::Dealloc => "free",
            AstAllocKind::Clump => "clump",
        };
        write!(f, ".{kw}({arg})")
    }
}

impl AstExpr {
    //=ast_expr_create
    fn new(kind: AstExprKind) -> Box<AstExpr> {
        Box::new(AstExpr { kind })
    }

    //=ast_expr_identifier_create
    pub fn new_identifier(s: String) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Identifier(s))
    }

    //=ast_expr_constant_create
    pub fn new_constant(k: c_int) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Constant(ConstantExpr {
            is_char: false,
            constant: k,
        }))
    }

    //=ast_expr_constant_create_char
    pub fn new_constant_char(c: c_char) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Constant(ConstantExpr {
            is_char: true,
            constant: c as c_int,
        }))
    }

    //=ast_expr_literal_create
    pub fn new_literal(s: String) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::StringLiteral(s))
    }

    //=ast_expr_bracketed_create
    #[allow(dead_code)]
    pub fn new_bracketed(root: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Bracketed(root))
    }

    //=ast_expr_iteration_create
    #[allow(dead_code)]
    pub fn new_iteration() -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Iteration)
    }

    //=ast_expr_call_create
    pub fn new_call(fun: Box<AstExpr>, args: Vec<Box<AstExpr>>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Call(CallExpr { fun, args }))
    }

    //=ast_expr_incdec_create
    pub fn new_incdec(root: Box<AstExpr>, inc: bool, pre: bool) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::IncDec(IncDecExpr {
            operand: root,
            inc,
            pre,
        }))
    }

    //=ast_expr_member_create
    pub fn new_member(struct_: Box<AstExpr>, field: String) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::StructMember(StructMemberExpr {
            root: struct_,
            member: field,
        }))
    }

    //=ast_expr_unary_create
    pub fn new_unary(root: Box<AstExpr>, op: AstUnaryOp) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Unary(UnaryExpr { op, arg: root }))
    }

    //=ast_expr_binary_create
    pub fn new_binary(e1: Box<AstExpr>, op: AstBinaryOp, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Binary(BinaryExpr { e1, op, e2 }))
    }

    //=ast_expr_eq_create
    pub fn new_eq(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Eq, e2)
    }

    //=ast_expr_ne_create
    #[allow(dead_code)]
    pub fn new_ne(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Ne, e2)
    }

    //=ast_expr_lt_create
    pub fn new_lt(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Lt, e2)
    }

    //=ast_expr_gt_create
    #[allow(dead_code)]
    pub fn new_gt(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Gt, e2)
    }

    //=ast_expr_le_create
    pub fn new_le(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Le, e2)
    }

    //=ast_expr_ge_create
    pub fn new_ge(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Ge, e2)
    }

    //=ast_expr_sum_create
    pub fn new_sum(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Addition, e2)
    }

    //=ast_expr_difference_create
    pub fn new_difference(e1: Box<AstExpr>, e2: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new_binary(e1, AstBinaryOp::Subtraction, e2)
    }

    //=ast_expr_assignment_create
    pub fn new_assignment(root: Box<AstExpr>, value: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Assignment(AssignmentExpr {
            lval: root,
            rval: value,
        }))
    }

    //=ast_expr_isdeallocand_create
    pub fn new_isdeallocand(assertand: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::IsDeallocand(assertand))
    }

    //=ast_expr_isdereferencable_create
    pub fn new_isdereferencable(assertand: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::IsDereferencable(assertand))
    }

    //=ast_expr_arbarg_create
    pub fn new_arbarg() -> Box<AstExpr> {
        AstExpr::new(AstExprKind::ArbArg)
    }

    //=ast_expr_alloc_create
    pub fn new_alloc(arg: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Allocation(AllocExpr {
            kind: AstAllocKind::Alloc,
            arg,
        }))
    }

    //=ast_expr_dealloc_create
    pub fn new_dealloc(arg: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Allocation(AllocExpr {
            kind: AstAllocKind::Dealloc,
            arg,
        }))
    }

    //=ast_expr_clump_create
    pub fn new_clump(arg: Box<AstExpr>) -> Box<AstExpr> {
        AstExpr::new(AstExprKind::Allocation(AllocExpr {
            kind: AstAllocKind::Clump,
            arg,
        }))
    }
}

pub fn ast_expr_as_identifier(expr: &AstExpr) -> &str {
    let AstExprKind::Identifier(id) = &expr.kind else {
        panic!();
    };
    id
}

pub fn ast_expr_as_constant(expr: &AstExpr) -> c_int {
    match &expr.kind {
        AstExprKind::Constant(c) => c.constant,
        _ => panic!(),
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

pub fn ast_expr_inverted_copy(expr: &AstExpr, invert: bool) -> Box<AstExpr> {
    let copy = ast_expr_copy(expr);
    if invert {
        AstExpr::new_unary(copy, AstUnaryOp::Bang)
    } else {
        copy
    }
}

pub fn ast_expr_unary_op(expr: &AstExpr) -> AstUnaryOp {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!();
    };
    unary.op
}

pub fn ast_expr_unary_operand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Unary(unary) = &expr.kind else {
        panic!();
    };
    &unary.arg
}

pub fn ast_expr_binary_e1(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!();
    };
    &binary.e1
}

pub fn ast_expr_binary_e2(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!();
    };
    &binary.e2
}

pub fn ast_expr_binary_op(expr: &AstExpr) -> AstBinaryOp {
    let AstExprKind::Binary(binary) = &expr.kind else {
        panic!();
    };
    binary.op
}

pub fn ast_expr_assignment_rval(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::Assignment(assignment) = &expr.kind else {
        panic!();
    };
    &assignment.rval
}

pub fn ast_expr_isdeallocand_assertand(expr: &AstExpr) -> &AstExpr {
    let AstExprKind::IsDeallocand(assertand) = &expr.kind else {
        panic!();
    };
    assertand
}

pub fn ast_expr_copy(expr: &AstExpr) -> Box<AstExpr> {
    Box::new(expr.clone())
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
