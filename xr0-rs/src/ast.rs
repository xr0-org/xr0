use std::process;
use std::sync::Arc;

use crate::parser::LexemeMarker;
use crate::state::state::{
    state_copy, state_copywithname, state_create, state_create_withprops, state_declare,
    state_equal, state_getobject, state_getresult, state_hasgarbage, state_str, state_vconst,
};
use crate::state::State;
use crate::util::{Error, InsertionOrderMap, Result, SemiBox};
use crate::value::{
    value_copy, value_int_indefinite_create, value_ptr_indefinite_create,
    value_struct_indefinite_create,
};
use crate::{str_write, vprintln, Externals, Object, Value};

mod block;
mod expr;
mod expr_verify;
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

pub struct LValue<'ast> {
    pub t: &'ast AstType,
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
    let fname = f.name();
    let abs = f.abstract_block();
    for i in index..abs.stmts.len() {
        let stmt = &abs.stmts[i];
        if ast_stmt_ispre(stmt) {
            continue;
        }

        let mut prestate = state_copy(state);
        if let Err(err) = ast_stmt_absprocess(stmt, fname, state, true) {
            let uc = err.try_into_undecideable_cond()?;
            return split_path_absverify(f, &mut prestate, i, &uc);
        }
    }

    // TODO: verify that `result' is of the same type as f->result
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
        ast_stmt_absprocess(stmt, f.name(), s, true)
            .map_err(|err| err.wrap(format!("{}:{} ", stmt.loc, f.name())))?;
    }
    Ok(())
}

fn inititalise_param(param: &AstVariable, state: &mut State) -> Result<()> {
    let state: *mut State = state;
    unsafe {
        let name = &param.name;
        let t = &param.type_;
        let obj = state_getobject(&mut *state, name).unwrap().unwrap();
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
        let mut prestate = state_copy(actual_state);
        if let Err(err) = ast_stmt_process(stmt, fname, actual_state) {
            let uc = err.try_into_undecideable_cond()?;
            return split_path_verify(f, &mut prestate, i, &uc, abstract_state);
        }
        if ast_stmt_isterminal(stmt, actual_state) {
            break;
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
    let fname = f.name();
    for stmt in &f.abstract_.stmts {
        ast_stmt_absprocess(stmt, fname, state, false)?;
    }
    let obj = state_getresult(state).unwrap().unwrap();
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
        f.params.clone(),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    // Note: Original leaks inv_assumption, but I think unintentionally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_str(), &inv_assumption),
        f.params.clone(),
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

pub fn ast_function_buildgraph(fname: &str, ext: &Externals) -> FuncGraph {
    let mut dedup = InsertionOrderMap::new();
    let mut g = InsertionOrderMap::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    g
}

fn split_name(name: &str, assumption: &AstExpr) -> String {
    format!("{name} | {assumption}")
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
        f.params.clone(),
        f.abstract_.clone(),
        f.body.as_ref().map(|body| body.reborrow()),
    );
    // Note: Original leaks `inv_assumption` but I think accidentally.
    let inv_assumption = ast_expr_inverted_copy(cond, true);
    let f_false = ast_function_create(
        f.is_axiom,
        ast_type_copy(&f.ret),
        split_name(f.name.as_str(), &inv_assumption),
        f.params.clone(),
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
