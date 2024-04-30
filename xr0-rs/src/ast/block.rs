use super::stmt::ast_stmt_preconds_validate;
use super::*;

#[derive(Clone)]
pub struct AstBlock {
    pub decls: Vec<Box<AstVariable>>,
    pub stmts: Vec<Box<AstStmt>>,
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
