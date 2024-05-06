use super::stmt::ast_stmt_preconds_validate;
use super::{
    ast_stmt_ispre, ast_stmt_isterminal, ast_stmt_labelled_stmt, ast_stmt_str, AstStmt,
    AstVariable, Result, State,
};
use crate::str_write;

#[derive(Clone)]
pub struct AstBlock {
    pub decls: Vec<Box<AstVariable>>,
    pub stmts: Vec<Box<AstStmt>>,
}

impl AstBlock {
    //=ast_block_create
    pub fn new(decls: Vec<Box<AstVariable>>, stmts: Vec<Box<AstStmt>>) -> Box<AstBlock> {
        Box::new(AstBlock { decls, stmts })
    }

    //=ast_block_str
    pub fn str(&self, indent: &str) -> String {
        let mut sb = String::new();
        for decl in &self.decls {
            str_write!(sb, "{indent}{decl};\n");
        }
        for stmt in &self.stmts {
            let s = ast_stmt_str(stmt);
            str_write!(sb, "{indent}{s}\n");
        }
        sb
    }

    //=ast_block_isterminal
    pub fn is_terminal(&self, s: &mut State) -> bool {
        self.stmts.iter().any(|stmt| ast_stmt_isterminal(stmt, s))
    }

    //=ast_block_preconds
    pub fn preconds(&self) -> Result<Option<&AstStmt>> {
        for stmt in &self.stmts {
            if ast_stmt_ispre(stmt) {
                let preconds = ast_stmt_labelled_stmt(stmt);
                ast_stmt_preconds_validate(preconds)?;
                return Ok(Some(preconds));
            }
        }
        Ok(None)
    }
}
