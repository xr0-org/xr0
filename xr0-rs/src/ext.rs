use std::collections::HashMap;
use std::ffi::CStr;
use std::ptr;

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

    pub unsafe fn declare_func(&mut self, id: *mut libc::c_char, f: *mut AstFunction<'static>) {
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self.func.insert(id, f);
    }

    pub unsafe fn declare_var(&mut self, id: *mut libc::c_char, v: *mut AstVariable) {
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self.var.insert(id, v);
    }

    pub unsafe fn declare_typedef(&mut self, id: *mut libc::c_char, t: *const AstType) {
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self.typedef.insert(id, t);
    }

    pub unsafe fn declare_struct(&mut self, t: *const AstType) {
        let id = ast_type_struct_tag(&*t).unwrap().as_str().to_string();
        self._struct.insert(id, t);
    }

    pub unsafe fn get_func(&self, id: *const libc::c_char) -> *mut AstFunction<'static> {
        let id = CStr::from_ptr(id).to_str().unwrap();
        self.func.get(id).copied().unwrap_or(ptr::null_mut())
    }

    pub unsafe fn get_typedef(&self, id: *const libc::c_char) -> Option<&AstType> {
        let id = CStr::from_ptr(id).to_str().unwrap();
        self.typedef.get(id).map(|&p| &*p)
    }

    pub unsafe fn get_struct(&self, id: *const libc::c_char) -> Option<&AstType> {
        let id = CStr::from_ptr(id).to_str().unwrap();
        self._struct.get(id).map(|&p| &*p)
    }
}
