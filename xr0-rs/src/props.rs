use crate::ast::{ast_expr_equal, ast_expr_inverted_copy};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{strbuilder_write, AstExpr, StrBuilder};

#[derive(Clone)]
pub struct Props {
    pub props: Vec<Box<AstExpr>>,
}

impl Props {
    pub fn new() -> Props {
        Props { props: vec![] }
    }
}

pub unsafe fn props_str(p: &Props, indent: &str) -> OwningCStr {
    if p.props.is_empty() {
        return OwningCStr::empty();
    }
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_write!(b, "{indent}\u{22a2} ");
    for (i, e) in p.props.iter().enumerate() {
        strbuilder_write!(b, "{e}{}", if i + 1 < p.props.len() { ", " } else { "" },);
    }
    strbuilder_write!(b, "\n");
    strbuilder_build(b)
}

pub unsafe fn props_install(p: &mut Props, e: Box<AstExpr>) {
    if props_contradicts(p, &*e) {
        panic!();
    }
    (*p).props.push(e);
}

pub unsafe fn props_get(p: &Props, e: &AstExpr) -> bool {
    (*p).props.iter().any(|prop| ast_expr_equal(&*e, prop))
}

pub unsafe fn props_contradicts(p: &Props, p1: &AstExpr) -> bool {
    let not_p1 = ast_expr_inverted_copy(p1, true);
    props_contradicts_actual(p, p1, &not_p1)
}

unsafe fn props_contradicts_actual(p: &Props, p1: &AstExpr, not_p1: &AstExpr) -> bool {
    p.props.iter().any(|p2| {
        let not_p2 = ast_expr_inverted_copy(p2, true);
        ast_expr_equal(p1, &not_p2) || ast_expr_equal(not_p1, p2)
    })
}
