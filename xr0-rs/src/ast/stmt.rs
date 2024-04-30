use super::{
    ast_block_isterminal, ast_block_str, ast_expr_assignment_rval, ast_expr_binary_e2,
    ast_expr_getfuncs, sel_decide, str_write, AstAllocKind, AstBlock, AstExpr, Error, LexemeMarker,
    Result, State,
};

// Note: In the original, `ast_stmt_copy` did not handle allocation statements.
#[derive(Clone)]
pub struct AstStmt {
    pub kind: AstStmtKind,
    pub loc: Box<LexemeMarker>,
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
pub struct AstLabelledStmt {
    pub label: String,
    pub stmt: Box<AstStmt>,
}

#[derive(Clone)]
pub struct AstSelectionStmt {
    pub isswitch: bool,
    pub cond: Box<AstExpr>,
    pub body: Box<AstStmt>,
    pub nest: Option<Box<AstStmt>>,
}

#[derive(Clone)]
pub struct AstIterationStmt {
    pub init: Box<AstStmt>,
    pub cond: Box<AstStmt>,
    pub body: Box<AstStmt>,
    pub iter: Box<AstExpr>,
    pub abstract_: Box<AstBlock>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstJumpKind {
    Return,
}

#[derive(Clone)]
pub struct AstJumpStmt {
    pub kind: AstJumpKind,
    pub rv: Option<Box<AstExpr>>,
}

#[derive(Clone)]
pub struct AstAllocStmt {
    pub kind: AstAllocKind,
    pub arg: Box<AstExpr>,
}

impl AstStmt {
    //=ast_stmt_create
    fn new(loc: Box<LexemeMarker>, kind: AstStmtKind) -> Box<Self> {
        Box::new(AstStmt { kind, loc })
    }

    //=ast_stmt_create_nop
    pub fn new_nop(loc: Box<LexemeMarker>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Nop)
    }

    //=ast_stmt_create_labelled
    pub fn new_labelled(loc: Box<LexemeMarker>, label: String, substmt: Box<AstStmt>) -> Box<Self> {
        AstStmt::new(
            loc,
            AstStmtKind::Labelled(AstLabelledStmt {
                label,
                stmt: substmt,
            }),
        )
    }

    //=ast_stmt_create_compound
    pub fn new_compound(loc: Box<LexemeMarker>, b: Box<AstBlock>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Compound(b))
    }

    //=ast_stmt_create_compound_v
    pub fn new_compound_v(loc: Box<LexemeMarker>, b: Box<AstBlock>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::CompoundV(b))
    }

    //=ast_stmt_create_expr
    pub fn new_expr(loc: Box<LexemeMarker>, expr: Box<AstExpr>) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Expr(expr))
    }

    //=ast_stmt_create_sel
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

    //=ast_stmt_create_iter
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

    //=ast_stmt_create_jump
    pub fn new_jump(
        loc: Box<LexemeMarker>,
        kind: AstJumpKind,
        rv: Option<Box<AstExpr>>,
    ) -> Box<Self> {
        AstStmt::new(loc, AstStmtKind::Jump(AstJumpStmt { kind, rv }))
    }
}

pub fn ast_stmt_lexememarker(stmt: &AstStmt) -> &LexemeMarker {
    &stmt.loc
}

pub fn ast_stmt_labelled_stmt(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!();
    };
    &labelled.stmt
}

pub fn ast_stmt_ispre(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    labelled.label == "setup"
}

pub fn ast_stmt_isassume(stmt: &AstStmt) -> bool {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        return false;
    };
    labelled.label == "assume"
}

fn ast_stmt_labelled_sprint(stmt: &AstStmt, b: &mut String) {
    let AstStmtKind::Labelled(labelled) = &stmt.kind else {
        panic!();
    };
    let s = ast_stmt_str(&labelled.stmt);
    str_write!(*b, "{}: {s}", labelled.label);
}

fn ast_stmt_nop_sprint(b: &mut String) {
    str_write!(*b, ";");
}

fn ast_stmt_expr_sprint(expr: &AstExpr, b: &mut String) {
    str_write!(*b, "{expr};");
}

fn ast_stmt_compound_sprint(compound: &AstBlock, b: &mut String) {
    let s = ast_block_str(compound, "\t");
    str_write!(*b, "{s}");
}

pub fn ast_stmt_jump_rv(stmt: &AstStmt) -> Option<&AstExpr> {
    let AstStmtKind::Jump(jump) = &stmt.kind else {
        panic!();
    };
    jump.rv.as_deref()
}

pub fn ast_stmt_isterminal(stmt: &AstStmt, s: &mut State) -> bool {
    match &stmt.kind {
        AstStmtKind::Jump(jump) => jump.kind == AstJumpKind::Return,
        AstStmtKind::Compound(block) => ast_block_isterminal(block, s),
        AstStmtKind::Selection(_) => sel_isterminal(stmt, s),
        _ => false,
    }
}

fn sel_isterminal(stmt: &AstStmt, s: &mut State) -> bool {
    let decision = sel_decide(ast_stmt_sel_cond(stmt), s).unwrap();
    if decision {
        return ast_stmt_isterminal(ast_stmt_sel_body(stmt), s);
    }
    false
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

pub fn ast_stmt_sel_nest(stmt: &AstStmt) -> Option<&AstStmt> {
    let AstStmtKind::Selection(selection) = &stmt.kind else {
        panic!();
    };
    selection.nest.as_deref()
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

pub fn ast_stmt_iter_init(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.init
}

pub fn ast_stmt_iter_cond(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.cond
}

pub fn ast_stmt_iter_iter(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.iter
}

pub fn ast_stmt_iter_abstract(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.abstract_
}

pub fn ast_stmt_iter_body(stmt: &AstStmt) -> &AstStmt {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    &iteration.body
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

pub fn ast_stmt_iter_upper_bound(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Iteration(iteration) = &stmt.kind else {
        panic!();
    };
    let AstStmtKind::Expr(expr) = &iteration.cond.kind else {
        panic!();
    };
    ast_expr_binary_e2(expr)
}

fn ast_stmt_iter_sprint(iteration: &AstIterationStmt, b: &mut String) {
    let init = ast_stmt_str(&iteration.init);
    let cond = ast_stmt_str(&iteration.cond);
    let body = ast_stmt_str(&iteration.body);
    let iter = &iteration.iter;
    let abs = ast_block_str(&iteration.abstract_, "\t");
    str_write!(*b, "for ({init} {cond} {iter}) [{abs}] {{ {body} }}");
}

fn ast_stmt_jump_sprint(jump: &AstJumpStmt, b: &mut String) {
    // Note: jump.rv can be null. Error in the original.
    let rv = jump.rv.as_ref().unwrap();
    str_write!(*b, "return {rv};\n");
}

pub fn ast_stmt_copy(stmt: &AstStmt) -> Box<AstStmt> {
    Box::new(stmt.clone())
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

pub fn ast_stmt_as_v_block(stmt: &AstStmt) -> &AstBlock {
    let AstStmtKind::CompoundV(block) = &stmt.kind else {
        panic!();
    };
    block
}

pub fn ast_stmt_as_expr(stmt: &AstStmt) -> &AstExpr {
    let AstStmtKind::Expr(expr) = &stmt.kind else {
        panic!();
    };
    expr
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
