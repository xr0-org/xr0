use super::{AstFunction, AstType, AstVariable};

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
    pub type_: Box<AstType>,
}

impl AstExternDecl {
    //=ast_functiondecl_create
    pub fn new_function(f: Box<AstFunction<'static>>) -> Box<AstExternDecl> {
        Box::new(AstExternDecl {
            kind: AstExternDeclKind::Function(f),
        })
    }

    //=ast_externdecl_as_function
    pub fn as_function(&self) -> Option<&AstFunction> {
        match &self.kind {
            AstExternDeclKind::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_function_mut(&mut self) -> Option<&mut AstFunction<'static>> {
        match &mut self.kind {
            AstExternDeclKind::Function(f) => Some(f),
            _ => None,
        }
    }

    //=ast_decl_create
    pub fn new(name: String, t: Box<AstType>) -> Box<AstExternDecl> {
        Box::new(AstExternDecl {
            kind: if t.is_typedef() {
                AstExternDeclKind::Typedef(AstTypedefDecl { name, type_: t })
            } else if t.is_struct() {
                assert!(t.struct_tag().is_some());
                AstExternDeclKind::Struct(t)
            } else {
                AstExternDeclKind::Variable(AstVariable::new(name, t))
            },
        })
    }
}
