use std::process;

use super::{
    ast_block_preconds, ast_block_str, ast_stmt_absprocess, ast_stmt_getfuncs,
    ast_stmt_setupabsexec, AstBlock, AstStmt, AstType, AstVariable, FuncGraph,
};
use crate::path::Path;
use crate::state::state::{state_declare, state_getobject, state_getresult, state_vconst, State};
use crate::util::{InsertionOrderMap, Result, SemiBox};
use crate::value::{value_copy, Value};
use crate::{str_write, Externals};

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

pub fn ast_function_verify(f: &AstFunction, ext: &Externals) -> Result<()> {
    let mut path = Path::new(f, f.name.clone(), ext);
    while !path.at_end() {
        path.step()?;
    }
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
        ast_stmt_absprocess(stmt, s, true)
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

pub fn ast_function_setupabsexec(f: &AstFunction, state: &mut State) -> Result<()> {
    for stmt in &f.abstract_.stmts {
        ast_stmt_setupabsexec(stmt, state)?;
    }
    Ok(())
}

pub fn ast_function_absexec(f: &AstFunction, state: &mut State) -> Result<Option<Box<Value>>> {
    for decl in &f.abstract_.decls {
        state_declare(state, decl, false);
    }
    for stmt in &f.abstract_.stmts {
        ast_stmt_absprocess(stmt, state, false)?;
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
