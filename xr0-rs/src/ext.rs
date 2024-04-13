#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use libc::{free, malloc};

use crate::ast::{ast_type_str, ast_type_struct_tag};
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf, Map};
use crate::{AstFunction, AstType, AstVariable, StrBuilder};

pub struct Externals {
    pub func: Box<Map>,
    pub var: Box<Map>,
    pub _typedef: Box<Map>,
    pub _struct: Box<Map>,
}

pub unsafe fn externals_create() -> *mut Externals {
    let ext: *mut Externals = malloc(::core::mem::size_of::<Externals>()) as *mut Externals;
    std::ptr::write(
        ext,
        Externals {
            func: Map::new(),
            var: Map::new(),
            _typedef: Map::new(),
            _struct: Map::new(),
        },
    );
    return ext;
}

pub unsafe fn externals_destroy(ext: *mut Externals) {
    let Externals {
        func,
        var,
        _typedef,
        _struct,
    } = std::ptr::read(ext);
    func.destroy();
    var.destroy();
    _typedef.destroy();
    _struct.destroy();
    free(ext as *mut libc::c_void);
}

pub unsafe fn externals_types_str(
    ext: *mut Externals,
    indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let mut m = &(*ext)._typedef;
    for (k, v) in m.pairs() {
        let type_0: *mut libc::c_char = ast_type_str(v as *mut AstType);
        strbuilder_printf(
            b,
            b"%s%s %s\n\0" as *const u8 as *const libc::c_char,
            indent,
            type_0,
            k,
        );
        free(type_0 as *mut libc::c_void);
    }
    m = &(*ext)._struct;
    for v in m.values() {
        let type_1: *mut libc::c_char = ast_type_str(v as *mut AstType);
        strbuilder_printf(
            b,
            b"%s%s\n\0" as *const u8 as *const libc::c_char,
            indent,
            type_1,
        );
        free(type_1 as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

pub unsafe fn externals_declarefunc(
    ext: *mut Externals,
    id: *mut libc::c_char,
    f: *mut AstFunction,
) {
    (*ext).func.set(dynamic_str(id), f as *const libc::c_void);
}

pub unsafe fn externals_declarevar(
    ext: *mut Externals,
    id: *mut libc::c_char,
    v: *mut AstVariable,
) {
    (*ext).var.set(dynamic_str(id), v as *const libc::c_void);
}

pub unsafe fn externals_declaretypedef(
    ext: *mut Externals,
    id: *mut libc::c_char,
    t: *mut AstType,
) {
    (*ext)
        ._typedef
        .set(dynamic_str(id), t as *const libc::c_void);
}

pub unsafe fn externals_declarestruct(ext: *mut Externals, t: *mut AstType) {
    let id: *mut libc::c_char = ast_type_struct_tag(&*t);
    if id.is_null() {
        panic!();
    }
    (*ext)
        ._struct
        .set(dynamic_str(id), t as *const libc::c_void);
}

pub unsafe fn externals_getfunc(ext: *mut Externals, id: *mut libc::c_char) -> *mut AstFunction {
    return (*ext).func.get(id) as *mut AstFunction;
}

pub unsafe fn externals_gettypedef(ext: *mut Externals, id: *mut libc::c_char) -> *mut AstType {
    return (*ext)._typedef.get(id) as *mut AstType;
}

pub unsafe fn externals_getstruct(ext: *mut Externals, id: *mut libc::c_char) -> *mut AstType {
    return (*ext)._struct.get(id) as *mut AstType;
}
