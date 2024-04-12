#![allow(
    dead_code,
    mutable_transmutes,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

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
    let mut ext: *mut Externals = malloc(::core::mem::size_of::<Externals>()) as *mut Externals;
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

pub unsafe fn externals_destroy(mut ext: *mut Externals) {
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
    mut ext: *mut Externals,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut m = &(*ext)._typedef;
    for (k, v) in m.pairs() {
        let mut type_0: *mut libc::c_char = ast_type_str(v as *mut AstType);
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
        let mut type_1: *mut libc::c_char = ast_type_str(v as *mut AstType);
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
    mut ext: *mut Externals,
    mut id: *mut libc::c_char,
    mut f: *mut AstFunction,
) {
    (*ext).func.set(dynamic_str(id), f as *const libc::c_void);
}

pub unsafe fn externals_declarevar(
    mut ext: *mut Externals,
    mut id: *mut libc::c_char,
    mut v: *mut AstVariable,
) {
    (*ext).var.set(dynamic_str(id), v as *const libc::c_void);
}

pub unsafe fn externals_declaretypedef(
    mut ext: *mut Externals,
    mut id: *mut libc::c_char,
    mut t: *mut AstType,
) {
    (*ext)
        ._typedef
        .set(dynamic_str(id), t as *const libc::c_void);
}

pub unsafe fn externals_declarestruct(mut ext: *mut Externals, mut t: *mut AstType) {
    let mut id: *mut libc::c_char = ast_type_struct_tag(t);
    if id.is_null() {
        panic!();
    }
    (*ext)
        ._struct
        .set(dynamic_str(id), t as *const libc::c_void);
}

pub unsafe fn externals_getfunc(
    mut ext: *mut Externals,
    mut id: *mut libc::c_char,
) -> *mut AstFunction {
    return (*ext).func.get(id) as *mut AstFunction;
}

pub unsafe fn externals_gettypedef(
    mut ext: *mut Externals,
    mut id: *mut libc::c_char,
) -> *mut AstType {
    return (*ext)._typedef.get(id) as *mut AstType;
}

pub unsafe fn externals_getstruct(
    mut ext: *mut Externals,
    mut id: *mut libc::c_char,
) -> *mut AstType {
    return (*ext)._struct.get(id) as *mut AstType;
}
