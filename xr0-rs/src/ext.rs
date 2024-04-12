#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{free, malloc};

use crate::ast::{ast_type_str, ast_type_struct_tag};
use crate::util::{dynamic_str, map, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{ast_function, ast_type, ast_variable, StrBuilder as strbuilder};

pub struct externals {
    pub func: Box<map>,
    pub var: Box<map>,
    pub _typedef: Box<map>,
    pub _struct: Box<map>,
}

pub unsafe fn externals_create() -> *mut externals {
    let mut ext: *mut externals = malloc(::core::mem::size_of::<externals>()) as *mut externals;
    std::ptr::write(
        ext,
        externals {
            func: map::new(),
            var: map::new(),
            _typedef: map::new(),
            _struct: map::new(),
        },
    );
    return ext;
}

pub unsafe fn externals_destroy(mut ext: *mut externals) {
    let externals {
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
    mut ext: *mut externals,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut m = &(*ext)._typedef;
    for (k, v) in m.pairs() {
        let mut type_0: *mut libc::c_char = ast_type_str(v as *mut ast_type);
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
        let mut type_1: *mut libc::c_char = ast_type_str(v as *mut ast_type);
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
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
    mut f: *mut ast_function,
) {
    (*ext).func.set(dynamic_str(id), f as *const libc::c_void);
}

pub unsafe fn externals_declarevar(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
    mut v: *mut ast_variable,
) {
    (*ext).var.set(dynamic_str(id), v as *const libc::c_void);
}

pub unsafe fn externals_declaretypedef(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
    mut t: *mut ast_type,
) {
    (*ext)
        ._typedef
        .set(dynamic_str(id), t as *const libc::c_void);
}

pub unsafe fn externals_declarestruct(mut ext: *mut externals, mut t: *mut ast_type) {
    let mut id: *mut libc::c_char = ast_type_struct_tag(t);
    if id.is_null() {
        panic!();
    }
    (*ext)
        ._struct
        .set(dynamic_str(id), t as *const libc::c_void);
}

pub unsafe fn externals_getfunc(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
) -> *mut ast_function {
    return (*ext).func.get(id) as *mut ast_function;
}

pub unsafe fn externals_gettypedef(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
) -> *mut ast_type {
    return (*ext)._typedef.get(id) as *mut ast_type;
}

pub unsafe fn externals_getstruct(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
) -> *mut ast_type {
    return (*ext)._struct.get(id) as *mut ast_type;
}
