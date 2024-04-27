use std::collections::HashMap;

use crate::ast::{ast_type_str, ast_type_struct_tag};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{strbuilder_write, AstFunction, AstType, AstVariable};

#[derive(Default)]
pub struct Externals {
    func_insertion_order: Vec<OwningCStr>,
    func: HashMap<String, Box<AstFunction<'static>>>,
    var: HashMap<String, Box<AstVariable>>,
    typedef: HashMap<String, Box<AstType>>,
    struct_: HashMap<String, Box<AstType>>,
}

impl Externals {
    pub fn new() -> Self {
        Externals::default()
    }

    pub fn types_str(&self, indent: &str) -> OwningCStr {
        let mut b = strbuilder_create();
        for (k, v) in &self.typedef {
            strbuilder_write!(b, "{indent}{} {k}\n", ast_type_str(v));
        }
        for v in self.struct_.values() {
            strbuilder_write!(b, "{indent}{}\n", ast_type_str(v));
        }
        strbuilder_build(b)
    }

    pub fn declare_func(&mut self, f: Box<AstFunction<'static>>) {
        self.func_insertion_order.push(f.name().clone());
        let id = f.name().to_string();
        self.func.insert(id, f);
    }

    pub fn declare_var(&mut self, id: String, v: Box<AstVariable>) {
        self.var.insert(id, v);
    }

    pub fn declare_typedef(&mut self, id: String, t: Box<AstType>) {
        self.typedef.insert(id, t);
    }

    pub fn declare_struct(&mut self, t: Box<AstType>) {
        let id = ast_type_struct_tag(&t).unwrap().to_string();
        self.struct_.insert(id, t);
    }

    pub fn function_names(&self) -> &[OwningCStr] {
        &self.func_insertion_order
    }

    pub fn get_func(&self, id: &str) -> Option<&AstFunction<'static>> {
        self.func.get(id).map(|ptr| &**ptr)
    }

    pub fn get_typedef(&self, id: &str) -> Option<&AstType> {
        self.typedef.get(id).map(|p| &**p)
    }

    pub fn get_struct(&self, id: &str) -> Option<&AstType> {
        self.struct_.get(id).map(|p| &**p)
    }
}
