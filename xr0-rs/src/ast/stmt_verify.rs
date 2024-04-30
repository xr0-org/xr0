use super::{
    ast_block_create, ast_block_decls, ast_block_stmts, ast_expr_abseval,
    ast_expr_alloc_rangeprocess, ast_expr_assume, ast_expr_copy, ast_expr_decide, ast_expr_eval,
    ast_expr_exec, ast_expr_pf_reduce, ast_expr_rangedecide, ast_stmt_as_block, ast_stmt_as_expr,
    ast_stmt_as_v_block, ast_stmt_copy, ast_stmt_ispre, ast_stmt_isterminal,
    ast_stmt_iter_abstract, ast_stmt_iter_body, ast_stmt_iter_cond, ast_stmt_iter_init,
    ast_stmt_iter_iter, ast_stmt_iter_lower_bound, ast_stmt_iter_upper_bound, ast_stmt_jump_rv,
    ast_stmt_labelled_stmt, ast_stmt_lexememarker, ast_stmt_sel_body, ast_stmt_sel_cond,
    ast_stmt_sel_nest, state_getresult, value_copy, AstExpr, AstStmt, AstStmtKind, Error,
    Preresult, Result, State, KEYWORD_RETURN,
};

use super::stmt::ast_stmt_isassume;
use crate::value::{
    value_as_constant, value_as_sync, value_equal, value_int_create, value_isconstant, value_isint,
    value_issync, value_to_expr,
};

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

fn stmt_installprop(stmt: &AstStmt, state: &mut State) -> Result<Preresult> {
    ast_expr_assume(ast_stmt_as_expr(ast_stmt_labelled_stmt(stmt)), state)
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

fn iter_empty(stmt: &AstStmt, state: &mut State) -> bool {
    if let Err(err) = ast_stmt_exec(ast_stmt_iter_init(stmt), state) {
        panic!("{err}");
    }
    !ast_expr_decide(ast_stmt_as_expr(ast_stmt_iter_cond(stmt)), state)
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

fn stmt_iter_exec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    if let Some(neteffect) = iter_neteffect(stmt) {
        ast_stmt_absexec(&neteffect, state, true)?;
    }
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

pub fn ast_stmt_absprocess(
    stmt: &AstStmt,
    fname: &str,
    state: &mut State,
    should_setup: bool,
) -> Result<()> {
    ast_stmt_absexec(stmt, state, should_setup)
        .map_err(|err| err.wrap(format!("{}:{fname}: ", stmt.loc)))
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

fn sel_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), state)?;
    if decision {
        return ast_stmt_absexec(ast_stmt_sel_body(stmt), state, should_setup);
    }
    assert!(ast_stmt_sel_nest(stmt).is_none());
    Ok(())
}

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

fn iter_absexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    let alloc = hack_alloc_from_neteffect(stmt);
    let lw = ast_stmt_iter_lower_bound(stmt);
    let up = ast_stmt_iter_upper_bound(stmt);
    ast_expr_alloc_rangeprocess(alloc, lw, up, state)?;
    Ok(())
}

fn hack_alloc_from_neteffect(stmt: &AstStmt) -> &AstExpr {
    let body = ast_stmt_iter_body(stmt);
    assert!(matches!(body.kind, AstStmtKind::Compound(_)));
    let block = ast_stmt_as_block(body);
    assert_eq!(ast_block_decls(block).len(), 0);
    assert_eq!(block.stmts.len(), 1);
    ast_stmt_as_expr(&block.stmts[0])
}

fn comp_absexec(stmt: &AstStmt, state: &mut State, should_setup: bool) -> Result<()> {
    let b = ast_stmt_as_block(stmt);
    for stmt in &b.stmts {
        ast_stmt_absexec(stmt, state, should_setup)?;
    }
    Ok(())
}

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

pub fn ast_stmt_setupabsexec(stmt: &AstStmt, state: &mut State) -> Result<()> {
    match stmt.kind {
        AstStmtKind::Selection(_) => stmt_setupabsexec(stmt, state),
        _ => Ok(()),
    }
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
