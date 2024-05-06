use crate::parser::LexemeMarker;
use crate::state::state::state_getresult;
use crate::state::State;
use crate::util::{Error, Result};
use crate::{str_write, Externals, Value};

mod block;
mod expr;
mod expr_verify;
mod extern_decl;
mod function;
mod literals;
mod stmt;
mod stmt_verify;
mod topological;
mod type_;
mod variable;

pub use block::{
    ast_block_create, ast_block_decls, ast_block_isterminal, ast_block_preconds, ast_block_stmts,
    ast_block_str, AstBlock,
};
pub use expr::{
    ast_expr_as_identifier, ast_expr_assignment_rval, ast_expr_binary_e1, ast_expr_binary_e2,
    ast_expr_binary_op, ast_expr_copy, ast_expr_equal, ast_expr_getfuncs,
    ast_expr_incdec_to_assignment, ast_expr_inverted_copy, ast_expr_is_not,
    ast_expr_isdeallocand_assertand, ast_expr_matheval, ast_expr_unary_op, ast_expr_unary_operand,
    c_char, c_int, c_uint, AllocExpr, AssignmentExpr, AstAllocKind, AstBinaryOp, AstExpr,
    AstExprKind, AstUnaryOp, BinaryExpr, CallExpr, ConstantExpr, IncDecExpr, StructMemberExpr,
    UnaryExpr,
};
pub use expr_verify::{
    ast_expr_abseval, ast_expr_alloc_rangeprocess, ast_expr_assume, ast_expr_decide, ast_expr_eval,
    ast_expr_exec, ast_expr_pf_reduce, ast_expr_rangedecide, LValue, Preresult,
};
pub use extern_decl::{AstExternDecl, AstExternDeclKind, AstTypedefDecl};
pub use function::{
    ast_function_absexec, ast_function_buildgraph, ast_function_create, ast_function_initparams,
    ast_function_protostitch, ast_function_setupabsexec, ast_function_verify, AstFunction,
};
pub use literals::{parse_char, parse_escape, parse_int};
pub use stmt::{
    ast_stmt_as_expr, ast_stmt_copy, ast_stmt_getfuncs, ast_stmt_isassume, ast_stmt_ispre,
    ast_stmt_isterminal, ast_stmt_iter_lower_bound, ast_stmt_iter_upper_bound, ast_stmt_jump_rv,
    ast_stmt_labelled_stmt, ast_stmt_lexememarker, ast_stmt_str, AstIterationStmt, AstJumpKind,
    AstSelectionStmt, AstStmt, AstStmtKind,
};
pub use stmt_verify::{ast_stmt_absprocess, ast_stmt_process, ast_stmt_setupabsexec, sel_decide};
pub use topological::{topological_order, FuncGraph};
pub use type_::{
    AstArrayType, AstStructType, AstType, AstTypeBase, AstTypeModifiers, MOD_AUTO, MOD_CONST,
    MOD_EXTERN, MOD_REGISTER, MOD_STATIC, MOD_TYPEDEF, MOD_VOLATILE,
};
pub use variable::AstVariable;

pub struct Ast {
    pub decls: Vec<Box<AstExternDecl>>,
}

pub fn ast_stmt_as_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Compound(block) = &stmt.kind else {
        panic!();
    };
    block
}

pub const KEYWORD_RETURN: &str = "return";

pub fn ast_topological_order(fname: &str, ext: &Externals) -> Vec<String> {
    topological_order(fname, ext)
}

pub fn ast_protostitch(f: &mut AstFunction<'static>, ext: &Externals) {
    ast_function_protostitch(f, ext)
}
