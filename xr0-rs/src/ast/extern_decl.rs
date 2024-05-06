use super::{AstFunction, AstType, AstVariable};

use crate::Externals;

#[derive(Clone)]
pub struct AstExternDecl {
    pub kind: AstExternDeclKind,
}

#[derive(Clone)]
pub enum AstExternDeclKind {
    Function(Box<AstFunction<'static>>),
    Variable(Box<AstVariable>),
    Typedef(AstTypedefDecl),
    Struct(Box<AstType>),
}

#[derive(Clone)]
pub struct AstTypedefDecl {
    pub name: String,
    pub type_0: Box<AstType>,
}

pub fn ast_functiondecl_create(f: Box<AstFunction<'static>>) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: AstExternDeclKind::Function(f),
    })
}

pub fn ast_externdecl_as_function(decl: &AstExternDecl) -> Option<&AstFunction> {
    match &decl.kind {
        AstExternDeclKind::Function(f) => Some(f),
        _ => None,
    }
}

pub fn ast_externdecl_as_function_mut(
    decl: &mut AstExternDecl,
) -> Option<&mut AstFunction<'static>> {
    match &mut decl.kind {
        AstExternDeclKind::Function(f) => Some(f),
        _ => None,
    }
}

pub fn ast_decl_create(name: String, t: Box<AstType>) -> Box<AstExternDecl> {
    Box::new(AstExternDecl {
        kind: if t.is_typedef() {
            AstExternDeclKind::Typedef(AstTypedefDecl { name, type_0: t })
        } else if t.is_struct() {
            assert!(t.struct_tag().is_some());
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
