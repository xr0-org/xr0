use crate::parser::LexemeMarker;
use crate::state::state::state_getresult;
use crate::state::State;
use crate::util::{Error, Result};
use crate::value::{
    value_copy, value_int_indefinite_create, value_ptr_indefinite_create,
    value_struct_indefinite_create,
};
use crate::{str_write, Externals, Object, Value};

mod block;
mod expr;
mod expr_verify;
mod stmt;
mod stmt_verify;
mod topological;
mod type_;
mod variable;
mod function;

pub use block::{
    ast_block_create, ast_block_decls, ast_block_isterminal, ast_block_preconds, ast_block_stmts,
    ast_block_str, AstBlock,
};
pub use expr::{
    ast_expr_as_identifier, ast_expr_assignment_rval, ast_expr_binary_e1, ast_expr_binary_e2,
    ast_expr_binary_op, ast_expr_copy, ast_expr_equal, ast_expr_getfuncs,
    ast_expr_incdec_to_assignment, ast_expr_inverted_copy, ast_expr_isdeallocand_assertand,
    ast_expr_matheval, ast_expr_unary_op, ast_expr_unary_operand, c_char, c_int, c_uint, AllocExpr,
    AssignmentExpr, AstAllocKind, AstBinaryOp, AstExpr, AstExprKind, AstUnaryOp, BinaryExpr,
    CallExpr, ConstantExpr, IncDecExpr, StructMemberExpr, UnaryExpr,
};
pub use expr_verify::{
    ast_expr_abseval, ast_expr_alloc_rangeprocess, ast_expr_assume, ast_expr_decide, ast_expr_eval,
    ast_expr_exec, ast_expr_pf_reduce, ast_expr_rangedecide,
};
pub use stmt::{
    ast_stmt_as_expr, ast_stmt_as_v_block, ast_stmt_copy, ast_stmt_getfuncs, ast_stmt_ispre,
    ast_stmt_isterminal, ast_stmt_iter_body, ast_stmt_iter_lower_bound, ast_stmt_iter_upper_bound,
    ast_stmt_jump_rv, ast_stmt_labelled_stmt, ast_stmt_lexememarker, ast_stmt_sel_body,
    ast_stmt_sel_cond, ast_stmt_sel_nest, ast_stmt_str, AstJumpKind, AstStmt, AstStmtKind,
};
pub use stmt_verify::{ast_stmt_absprocess, ast_stmt_process, ast_stmt_setupabsexec, sel_decide};
pub use topological::{topological_order, FuncGraph};
pub use type_::{
    ast_type_copy, ast_type_create, ast_type_create_arr, ast_type_create_ptr,
    ast_type_create_struct, ast_type_create_struct_anonym, ast_type_create_struct_partial,
    ast_type_create_userdef, ast_type_create_voidptr, ast_type_isstruct, ast_type_istypedef,
    ast_type_mod_or, ast_type_ptr_type, ast_type_struct_complete, ast_type_struct_members,
    ast_type_struct_tag, ast_type_vconst, AstArrayType, AstStructType, AstType, AstTypeBase,
    AstTypeModifiers, MOD_AUTO, MOD_CONST, MOD_EXTERN, MOD_REGISTER, MOD_STATIC, MOD_TYPEDEF,
    MOD_VOLATILE,
};
pub use variable::AstVariable;
pub use function::{AstFunction, ast_function_create};

pub struct LValue<'ast> {
    pub t: &'ast AstType,
    pub obj: Option<&'ast mut Object>,
}

#[derive(Clone)]
pub struct Preresult {
    pub is_contradiction: bool,
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

pub fn ast_stmt_as_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Compound(block) = &stmt.kind else {
        panic!();
    };
    block
}

pub const KEYWORD_RETURN: &str = "return";

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
            AstExternDeclKind::Variable(AstVariable::new(name, t))
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
            let name = v.name.clone();
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
