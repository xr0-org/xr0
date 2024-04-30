use std::sync::Arc;
use std::process;

use super::{
    ast_block_preconds, ast_block_str, ast_stmt_absprocess, ast_stmt_ispre, ast_stmt_isterminal,
    ast_stmt_process, ast_stmt_setupabsexec, AstBlock, AstExpr, AstStmt, AstType, AstVariable,
    ast_expr_inverted_copy, ast_expr_assume, ast_stmt_getfuncs, FuncGraph, ast_type_copy,
};
use crate::state::state::{
    state_copy, state_create, state_create_withprops, state_declare, state_equal, state_getobject,
    state_hasgarbage, state_str, state_vconst, State, state_copywithname, state_getresult,
};
use crate::value::{Value, value_copy};
use crate::util::{Error, Result, SemiBox, InsertionOrderMap};
use crate::{str_write, vprintln, Externals};

#[derive(Clone)]
pub struct AstFunction<'ast> {
    pub is_axiom: bool,
    pub ret: Box<AstType>,
    pub name: String,
    pub params: Vec<Box<AstVariable>>,
    pub abstract_: Box<AstBlock>,
    pub body: Option<SemiBox<'ast, AstBlock>>,
}

impl<'ast> AstFunction<'ast> {
    //=ast_function_str
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

    //=ast_function_name
    pub fn name(&self) -> &str {
        &self.name
    }

    //=ast_function_copy
    pub fn copy(&self) -> Box<AstFunction<'ast>> {
        Box::new(self.clone())
    }

    //=ast_function_isaxiom
    pub fn is_axiom(&self) -> bool {
        self.is_axiom
    }

    //=ast_function_isproto
    pub fn is_proto(&self) -> bool {
        self.body.is_none()
    }

    //=ast_function_absisempty
    pub fn abs_is_empty(&self) -> bool {
        self.abstract_.decls.is_empty() && self.abstract_.stmts.is_empty()
    }

    //=ast_function_type
    pub fn rtype(&self) -> &AstType {
        &self.ret
    }

    //=ast_function_body
    #[allow(dead_code)]
    pub fn body(&self) -> &AstBlock {
        let Some(body) = self.body.as_ref() else {
            panic!("cannot find body for {:?}", self.name);
        };
        body
    }

    //=ast_function_params
    pub fn params(&self) -> &[Box<AstVariable>] {
        &self.params
    }

    //=ast_function_preconditions
    pub fn preconditions(&self) -> Result<Option<&AstStmt>> {
        ast_block_preconds(&self.abstract_)
    }
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
    for var in &f.abstract_.decls {
        state_declare(state, var, false);
    }
    path_absverify(f, state, 0)
}

fn path_absverify(f: &AstFunction, state: &mut State, index: usize) -> Result<()> {
    let fname = f.name();
    let abs = &f.abstract_;
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
        let mut actual_copy = state_copywithname(actual_state, f.name().to_string());
        let mut abstract_copy = state_copywithname(abstract_state, f.name().to_string());
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

pub fn ast_function_buildgraph(fname: &str, ext: &Externals) -> FuncGraph {
    let mut dedup = InsertionOrderMap::new();
    let mut g = InsertionOrderMap::new();
    recurse_buildgraph(&mut g, &mut dedup, fname, ext);
    g
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

fn split_name(name: &str, assumption: &AstExpr) -> String {
    format!("{name} | {assumption}")
}
