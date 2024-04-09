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
use crate::util::{
    dynamic_str, entry, map, map_create, map_destroy, map_get, map_set, strbuilder_build,
    strbuilder_create, strbuilder_printf,
};
use crate::{ast_function, ast_type, ast_variable, StrBuilder as strbuilder};

use crate::c_util::__assert_rtn;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct externals {
    pub func: *mut map,
    pub var: *mut map,
    pub _typedef: *mut map,
    pub _struct: *mut map,
}
#[no_mangle]
pub unsafe fn externals_create() -> *mut externals {
    let mut ext: *mut externals = malloc(::core::mem::size_of::<externals>()) as *mut externals;
    (*ext).func = map_create();
    (*ext).var = map_create();
    (*ext)._typedef = map_create();
    (*ext)._struct = map_create();
    return ext;
}
#[no_mangle]
pub unsafe fn externals_destroy(mut ext: *mut externals) {
    map_destroy((*ext).func);
    map_destroy((*ext).var);
    map_destroy((*ext)._typedef);
    map_destroy((*ext)._struct);
    free(ext as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn externals_types_str(
    mut ext: *mut externals,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut m: *mut map = (*ext)._typedef;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        let mut type_0: *mut libc::c_char = ast_type_str(e.value as *mut ast_type);
        strbuilder_printf(
            b,
            b"%s%s %s\n\0" as *const u8 as *const libc::c_char,
            indent,
            type_0,
            e.key,
        );
        free(type_0 as *mut libc::c_void);
        i += 1;
    }
    m = (*ext)._struct;
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < (*m).n {
        let mut type_1: *mut libc::c_char =
            ast_type_str((*((*m).entry).offset(i_0 as isize)).value as *mut ast_type);
        strbuilder_printf(
            b,
            b"%s%s\n\0" as *const u8 as *const libc::c_char,
            indent,
            type_1,
        );
        free(type_1 as *mut libc::c_void);
        i_0 += 1;
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe fn externals_declarefunc(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
    mut f: *mut ast_function,
) {
    map_set((*ext).func, dynamic_str(id), f as *const libc::c_void);
}
#[no_mangle]
pub unsafe fn externals_declarevar(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
    mut v: *mut ast_variable,
) {
    map_set((*ext).var, dynamic_str(id), v as *const libc::c_void);
}
#[no_mangle]
pub unsafe fn externals_declaretypedef(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
    mut t: *mut ast_type,
) {
    map_set((*ext)._typedef, dynamic_str(id), t as *const libc::c_void);
}
#[no_mangle]
pub unsafe fn externals_declarestruct(mut ext: *mut externals, mut t: *mut ast_type) {
    let mut id: *mut libc::c_char = ast_type_struct_tag(t);
    if id.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 24], &[libc::c_char; 24]>(
                b"externals_declarestruct\0",
            ))
            .as_ptr(),
            b"ext.c\0" as *const u8 as *const libc::c_char,
            77 as libc::c_int,
            b"id\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    map_set((*ext)._struct, dynamic_str(id), t as *const libc::c_void);
}
#[no_mangle]
pub unsafe fn externals_getfunc(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
) -> *mut ast_function {
    return map_get((*ext).func, id) as *mut ast_function;
}
#[no_mangle]
pub unsafe fn externals_gettypedef(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
) -> *mut ast_type {
    return map_get((*ext)._typedef, id) as *mut ast_type;
}
#[no_mangle]
pub unsafe fn externals_getstruct(
    mut ext: *mut externals,
    mut id: *mut libc::c_char,
) -> *mut ast_type {
    return map_get((*ext)._struct, id) as *mut ast_type;
}
