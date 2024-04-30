use std::fmt::{self, Display, Formatter};
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
    value_as_constant, value_as_sync, value_copy, value_equal, value_int_create,
    value_int_indefinite_create, value_isconstant, value_isint, value_issync,
    value_ptr_indefinite_create, value_struct_indefinite_create, value_to_expr,
};
use crate::{str_write, vprintln, Externals, Object, Value};

mod block;
mod expr;
mod expr_verify;
mod stmt;
mod topological;

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
pub use stmt::*;
pub use topological::{topological_order, FuncGraph};

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
    Pointer(Box<AstType>),
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

pub fn ast_stmt_process(stmt: &AstStmt, fname: &str, state: &mut State) -> Result<()> {
    if matches!(stmt.kind, AstStmtKind::CompoundV(_)) {
        ast_stmt_verify(stmt, state).map_err(|err| {
            let loc = ast_stmt_lexememarker(stmt);
            err.wrap(format!("{loc}: "))
        })?;
    }
    ast_stmt_exec(stmt, state).map_err(|err| {
        let loc = ast_stmt_lexememarker(stmt);
        err.wrap(format!("{loc}:{fname}: "))
    })?;
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

fn labelled_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    assert!(ast_stmt_ispre(stmt));
    let setup = ast_stmt_labelled_stmt(stmt);
    if should_setup {
        ast_stmt_absexec(setup, state, should_setup)?;
    }
    Ok(())
}

fn expr_absexec(expr: &AstExpr, state: &mut State) -> Result<()> {
    ast_expr_abseval(expr, state)?;
    Ok(())
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

fn stmt_installprop(stmt: &AstStmt, state: &mut State) -> Result<Preresult> {
    ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state)
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
    let obj = state_getresult(state).unwrap().unwrap();
    obj.assign(Some(value_copy(&rv_val)));
    Ok(())
}

fn ast_stmt_absprocess(
    stmt: &AstStmt,
    fname: &str,
    state: &mut State,
    should_setup: bool,
) -> Result<()> {
    ast_stmt_absexec(stmt, state, should_setup)
        .map_err(|err| err.wrap(format!("{}:{fname}: ", stmt.loc)))
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

pub const KEYWORD_RETURN: &str = "return";

fn jump_absexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    // Note: Original leaks the expression to avoid a double free. We copy instead.
    expr_absexec(
        &AstExpr::new_assignment(
            AstExpr::new_identifier(KEYWORD_RETURN.to_string()),
            // Note: jump_rv can be null. Error in original.
            ast_expr_copy(ast_stmt_jump_rv(stmt).unwrap()),
        ),
        state,
    )
}

pub fn ast_stmt_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    match &stmt.kind {
        AstStmtKind::Nop => Ok(()),
        AstStmtKind::Labelled(_) => labelled_absexec(stmt, state, should_setup),
        AstStmtKind::Expr(_) => {
            expr_absexec(ast_stmt_as_expr(stmt), state)?;
            Ok(())
        }
        AstStmtKind::Selection(_) => sel_absexec(stmt, state, should_setup),
        AstStmtKind::Iteration(_) => iter_absexec(stmt, state),
        AstStmtKind::Compound(_) => comp_absexec(stmt, state, should_setup),
        AstStmtKind::Jump(_) => jump_absexec(stmt, state),
        _ => panic!(),
    }
}

#[allow(unreachable_code)]
pub fn sel_decide(control: &AstExpr, state: &mut State) -> Result<bool> {
    // Note: This value is leaked in the original.
    let v = ast_expr_pf_reduce(control, state)?;
    if value_issync(&v) {
        let sync = value_as_sync(&v);
        let p = state.props();
        if p.get(sync) {
            return Ok(true);
        } else if p.contradicts(sync) {
            return Ok(false);
        }
    }
    if value_isconstant(&v) {
        return Ok(value_as_constant(&v) != 0);
    }
    let zero = value_int_create(0);
    if !value_isint(&v) {
        return Err(Error::undecideable_cond(value_to_expr(&v)));
    }
    Ok(!value_equal(&zero, &v))
}

fn hack_alloc_from_neteffect(stmt: &AstStmt) -> &AstExpr {
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!(body.kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_decls(block).len(), 0);
    assert_eq!(block.stmts.len(), 1);
    ast_stmt_as_expr(&block.stmts[0])
}

fn sel_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

fn iter_absexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let alloc = hack_alloc_from_neteffect(stmt);
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    ast_expr_alloc_rangeprocess(alloc, lw, up, state)?;
    Ok(())
}

pub fn ast_type_create(base: AstTypeBase, modifiers: AstTypeModifiers) -> Box<AstType> {
    Box::new(AstType { base, modifiers })
}

pub fn ast_type_create_ptr(referent: Box<AstType>) -> Box<AstType> {
    ast_type_create(AstTypeBase::Pointer(referent), 0)
}

pub fn ast_type_create_voidptr() -> Box<AstType> {
    ast_type_create(
        AstTypeBase::Pointer(ast_type_create(AstTypeBase::Void, 0)),
        0,
    )
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
            AstTypeBase::Pointer(ptr_type) => {
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
            AstTypeBase::Union(_) | AstTypeBase::Enum => panic!(),
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

pub fn ast_type_ptr_type(t: &AstType) -> &AstType {
    let AstTypeBase::Pointer(ptr_type) = &t.base else {
        panic!();
    };
    ptr_type
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
        let name = ast_variable_name(param);
        let t = ast_variable_type(param);
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
