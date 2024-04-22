use std::collections::HashMap;
use std::ffi::CStr;
use std::ptr;

use crate::ast::{ast_type_str, ast_type_struct_tag};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{cstr, strbuilder_write, AstFunction, AstType, AstVariable, StrBuilder};

#[derive(Default)]
pub struct Externals {
    func: HashMap<String, *mut AstFunction>,
    var: HashMap<String, *mut AstVariable>,
    typedef: HashMap<String, *mut AstType>,
    _struct: HashMap<String, *mut AstType>,
}

impl Externals {
    pub fn new() -> Self {
        Externals::default()
    }

    pub unsafe fn types_str(&self, indent: *mut libc::c_char) -> OwningCStr {
        let b: *mut StrBuilder = strbuilder_create();
        for (k, v) in &self.typedef {
            strbuilder_write!(b, "{}{} {k}\n", cstr!(indent), ast_type_str(*v));
        }
        for v in self._struct.values() {
            strbuilder_write!(b, "{}{}\n", cstr!(indent), ast_type_str(*v));
        }
        strbuilder_build(b)
    }

    pub unsafe fn declare_func(&mut self, id: *mut libc::c_char, f: *mut AstFunction) {
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self.func.insert(id, f);
    }

    pub unsafe fn declare_var(&mut self, id: *mut libc::c_char, v: *mut AstVariable) {
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self.var.insert(id, v);
    }

    pub unsafe fn declare_typedef(&mut self, id: *mut libc::c_char, t: *mut AstType) {
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self.typedef.insert(id, t);
    }

    pub unsafe fn declare_struct(&mut self, t: *mut AstType) {
        let id = ast_type_struct_tag(&*t);
        if id.is_null() {
            panic!();
        }
        let id = CStr::from_ptr(id).to_str().unwrap().to_string();
        self._struct.insert(id, t);
    }

    pub unsafe fn get_func(&self, id: *const libc::c_char) -> *mut AstFunction {
        let id = CStr::from_ptr(id).to_str().unwrap();
        self.func.get(id).copied().unwrap_or(ptr::null_mut())
    }

    pub unsafe fn get_typedef(&self, id: *const libc::c_char) -> *mut AstType {
        let id = CStr::from_ptr(id).to_str().unwrap();
        self.typedef.get(id).copied().unwrap_or(ptr::null_mut())
    }

    pub unsafe fn get_struct(&self, id: *const libc::c_char) -> *mut AstType {
        let id = CStr::from_ptr(id).to_str().unwrap();
        self._struct.get(id).copied().unwrap_or(ptr::null_mut())
    }
}
