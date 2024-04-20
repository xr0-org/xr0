use std::collections::HashMap;
use std::ffi::CStr;
use std::ptr;

use libc::{free, malloc};

use crate::ast::{ast_type_str, ast_type_struct_tag};
use crate::util::{strbuilder_build, strbuilder_create};
use crate::{cstr, strbuilder_write, AstFunction, AstType, AstVariable, StrBuilder};

pub struct Externals {
    func: HashMap<String, *mut AstFunction>,
    var: HashMap<String, *mut AstVariable>,
    typedef: HashMap<String, *mut AstType>,
    _struct: HashMap<String, *mut AstType>,
}

pub unsafe fn externals_create() -> *mut Externals {
    let ext: *mut Externals = malloc(::core::mem::size_of::<Externals>()) as *mut Externals;
    std::ptr::write(
        ext,
        Externals {
            func: HashMap::new(),
            var: HashMap::new(),
            typedef: HashMap::new(),
            _struct: HashMap::new(),
        },
    );
    return ext;
}

pub unsafe fn externals_destroy(ext: *mut Externals) {
    drop(Box::from_raw(ext));
}

pub unsafe fn externals_types_str(
    ext: *mut Externals,
    indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let mut m = &(*ext).typedef;
    for (k, v) in m {
        let ty = ast_type_str(*v);
        strbuilder_write!(b, "{}{} {k}\n", cstr!(indent), cstr!(ty));
        free(ty as *mut libc::c_void);
    }
    m = &(*ext)._struct;
    for v in m.values() {
        let ty = ast_type_str(*v);
        strbuilder_write!(b, "{}{}\n", cstr!(indent), cstr!(ty));
        free(ty as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

pub unsafe fn externals_declarefunc(
    ext: *mut Externals,
    id: *mut libc::c_char,
    f: *mut AstFunction,
) {
    let id = CStr::from_ptr(id).to_str().unwrap().to_string();
    (*ext).func.insert(id, f);
}

pub unsafe fn externals_declarevar(
    ext: *mut Externals,
    id: *mut libc::c_char,
    v: *mut AstVariable,
) {
    let id = CStr::from_ptr(id).to_str().unwrap().to_string();
    (*ext).var.insert(id, v);
}

pub unsafe fn externals_declaretypedef(
    ext: *mut Externals,
    id: *mut libc::c_char,
    t: *mut AstType,
) {
    let id = CStr::from_ptr(id).to_str().unwrap().to_string();
    (*ext).typedef.insert(id, t);
}

pub unsafe fn externals_declarestruct(ext: *mut Externals, t: *mut AstType) {
    let id = ast_type_struct_tag(&*t);
    if id.is_null() {
        panic!();
    }
    let id = CStr::from_ptr(id).to_str().unwrap().to_string();
    (*ext)._struct.insert(id, t);
}

pub unsafe fn externals_getfunc(ext: *mut Externals, id: *mut libc::c_char) -> *mut AstFunction {
    let id = CStr::from_ptr(id).to_str().unwrap();
    (*ext).func.get(id).copied().unwrap_or(ptr::null_mut())
}

pub unsafe fn externals_gettypedef(ext: *mut Externals, id: *mut libc::c_char) -> *mut AstType {
    let id = CStr::from_ptr(id).to_str().unwrap();
    (*ext).typedef.get(id).copied().unwrap_or(ptr::null_mut())
}

pub unsafe fn externals_getstruct(ext: *mut Externals, id: *mut libc::c_char) -> *mut AstType {
    let id = CStr::from_ptr(id).to_str().unwrap();
    (*ext)._struct.get(id).copied().unwrap_or(ptr::null_mut())
}
