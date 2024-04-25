use std::collections::HashMap;

use crate::ast::{ast_type_str, ast_type_struct_tag};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{strbuilder_write, AstFunction, AstType, AstVariable};

#[derive(Default)]
pub struct Externals {
    func: HashMap<String, *mut AstFunction<'static>>,
    var: HashMap<String, *mut AstVariable>,
    typedef: HashMap<String, *const AstType>,
    _struct: HashMap<String, *const AstType>,
}

impl Externals {
    pub fn new() -> Self {
        Externals::default()
    }

    pub unsafe fn types_str(&self, indent: &str) -> OwningCStr {
        let mut b = strbuilder_create();
        for (k, v) in &self.typedef {
            strbuilder_write!(b, "{indent}{} {k}\n", ast_type_str(&**v));
        }
        for v in self._struct.values() {
            strbuilder_write!(b, "{indent}{}\n", ast_type_str(&**v));
        }
        strbuilder_build(b)
    }

    pub unsafe fn declare_func(&mut self, id: &str, f: *mut AstFunction<'static>) {
        self.func.insert(id.to_string(), f);
    }

    pub unsafe fn declare_var(&mut self, id: &str, v: *mut AstVariable) {
        self.var.insert(id.to_string(), v);
    }

    pub unsafe fn declare_typedef(&mut self, id: &str, t: *const AstType) {
        self.typedef.insert(id.to_string(), t);
    }

    pub unsafe fn declare_struct(&mut self, t: *const AstType) {
        let id = ast_type_struct_tag(&*t).unwrap().as_str().to_string();
        self._struct.insert(id, t);
    }

    pub unsafe fn get_func(&self, id: &str) -> Option<&AstFunction<'static>> {
        self.func.get(id).map(|ptr| &**ptr)
    }

    pub unsafe fn get_typedef(&self, id: &str) -> Option<&AstType> {
        self.typedef.get(id).map(|&p| &*p)
    }

    pub unsafe fn get_struct(&self, id: &str) -> Option<&AstType> {
        self._struct.get(id).map(|&p| &*p)
    }
}
