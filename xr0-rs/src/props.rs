use crate::ast::{ast_expr_equal, ast_expr_inverted_copy};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{strbuilder_write, AstExpr};

#[derive(Clone)]
pub struct Props {
    pub props: Vec<Box<AstExpr>>,
}

impl Props {
    pub fn new() -> Props {
        Props { props: vec![] }
    }

    pub unsafe fn str(&self, indent: &str) -> OwningCStr {
        if self.props.is_empty() {
            return OwningCStr::empty();
        }
        let mut b = strbuilder_create();
        strbuilder_write!(b, "{indent}\u{22a2} ");
        for (i, e) in self.props.iter().enumerate() {
            strbuilder_write!(b, "{e}{}", if i + 1 < self.props.len() { ", " } else { "" },);
        }
        strbuilder_write!(b, "\n");
        strbuilder_build(b)
    }

    pub fn install(&mut self, e: Box<AstExpr>) {
        if self.contradicts(&e) {
            panic!();
        }
        self.props.push(e);
    }

    pub fn get(&mut self, e: &AstExpr) -> bool {
        self.props.iter().any(|prop| ast_expr_equal(e, prop))
    }

    pub fn contradicts(&self, p1: &AstExpr) -> bool {
        let not_p1 = ast_expr_inverted_copy(p1, true);
        self.contradicts_actual(p1, &not_p1)
    }

    fn contradicts_actual(&self, p1: &AstExpr, not_p1: &AstExpr) -> bool {
        self.props.iter().any(|p2| {
            let not_p2 = ast_expr_inverted_copy(p2, true);
            ast_expr_equal(p1, &not_p2) || ast_expr_equal(not_p1, p2)
        })
    }
}
