#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use std::ptr::addr_of_mut;

use libc::{exit, fflush, fprintf, free, malloc, realloc, snprintf, strlen};

use super::lexer::{installclass, lexememarker_str, lexloc, marker, yylex, yytext, IC_TYPE};
use crate::ast::*;
use crate::c_util::{__assert_rtn, __stderrp, __stdoutp};
use crate::root;
use crate::util::{dynamic_str, VERBOSE_MODE};

#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub string: *mut libc::c_char,
    pub constant: C2RustUnnamed_4,
    pub call: C2RustUnnamed_3,
    pub incdec: C2RustUnnamed_2,
    pub unary_op: ast_unary_operator,
    pub binary: C2RustUnnamed_1,
    pub assignment_value: *mut AstExpr,
    pub alloc: C2RustUnnamed_0,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub kind: ast_alloc_kind,
    pub arg: *mut AstExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_1 {
    pub op: ast_binary_operator,
    pub e1: *mut AstExpr,
    pub e2: *mut AstExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_2 {
    pub inc: libc::c_int,
    pub pre: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_3 {
    pub n: libc::c_int,
    pub arg: *mut *mut AstExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_4 {
    pub constant: libc::c_int,
    pub ischar: bool,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct expr_array {
    pub n: libc::c_int,
    pub expr: *mut *mut AstExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct stmt_array {
    pub n: libc::c_int,
    pub stmt: *mut *mut ast_stmt,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union YYSTYPE {
    pub string: *mut libc::c_char,
    pub ast: *mut Ast,
    pub block: *mut ast_block,
    pub expr: *mut AstExpr,
    pub externdecl: *mut ast_externdecl,
    pub function: *mut ast_function,
    pub statement: *mut ast_stmt,
    pub type_0: *mut ast_type,
    pub variable: *mut ast_variable,
    pub block_statement: block_statement,
    pub expr_array: expr_array,
    pub stmt_array: stmt_array,
    pub variable_array: *mut ast_variable_arr,
    pub direct_function_declarator: direct_function_declarator,
    pub function_declarator: function_declarator,
    pub declarator: declarator,
    pub declaration: declaration,
    pub binary_operator: libc::c_int,
    pub integer: libc::c_int,
    pub type_modifier: libc::c_int,
    pub unary_operator: libc::c_int,
    pub boolean: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct declaration {
    pub name: *mut libc::c_char,
    pub t: *mut ast_type,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct declarator {
    pub ptr_valence: libc::c_int,
    pub name: *mut libc::c_char,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct function_declarator {
    pub ptr_valence: libc::c_int,
    pub decl: direct_function_declarator,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct direct_function_declarator {
    pub name: *mut libc::c_char,
    pub n: libc::c_int,
    pub param: *mut *mut ast_variable,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct block_statement {
    pub abstract_0: *mut ast_block,
    pub body: *mut ast_block,
}
pub type yytype_uint8 = libc::c_uchar;
pub type yytype_uint16 = libc::c_ushort;
pub type yytype_int16 = libc::c_short;
#[derive(Copy, Clone)]
#[repr(C)]
pub union yyalloc {
    pub yyss: yytype_int16,
    pub yyvs: YYSTYPE,
}
#[no_mangle]
pub unsafe extern "C" fn strip_quotes(mut s: *mut libc::c_char) -> *mut libc::c_char {
    if !(*s.offset(0 as libc::c_int as isize) as libc::c_int == '"' as i32) as libc::c_int
        as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"strip_quotes\0")).as_ptr(),
            b"gram.y\0" as *const u8 as *const libc::c_char,
            27 as libc::c_int,
            b"s[0] == '\"'\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut len: libc::c_int = (strlen(s)).wrapping_sub(2).wrapping_add(1) as libc::c_int;
    let mut t: *mut libc::c_char =
        malloc((::core::mem::size_of::<libc::c_char>()).wrapping_mul(len as usize))
            as *mut libc::c_char;
    snprintf(
        t,
        len as usize,
        b"%s\0" as *const u8 as *const libc::c_char,
        s.offset(1 as libc::c_int as isize),
    );
    return t;
}
#[no_mangle]
pub unsafe extern "C" fn stmt_array_create(mut v: *mut ast_stmt) -> stmt_array {
    let mut arr: stmt_array = {
        let mut init = stmt_array {
            n: 1 as libc::c_int,
            stmt: malloc(::core::mem::size_of::<*mut ast_stmt>()) as *mut *mut ast_stmt,
        };
        init
    };
    let ref mut fresh0 = *(arr.stmt).offset(0 as libc::c_int as isize);
    *fresh0 = v;
    return arr;
}
#[no_mangle]
pub unsafe extern "C" fn stmt_array_append(
    mut arr: *mut stmt_array,
    mut v: *mut ast_stmt,
) -> stmt_array {
    (*arr).n += 1;
    (*arr).stmt = realloc(
        (*arr).stmt as *mut libc::c_void,
        (::core::mem::size_of::<*mut ast_stmt>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut ast_stmt;
    let ref mut fresh1 = *((*arr).stmt).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh1 = v;
    return *arr;
}
#[no_mangle]
pub unsafe extern "C" fn expr_array_create(mut v: *mut AstExpr) -> expr_array {
    let mut arr: expr_array = {
        let mut init = expr_array {
            n: 1 as libc::c_int,
            expr: malloc(::core::mem::size_of::<*mut AstExpr>()) as *mut *mut AstExpr,
        };
        init
    };
    let ref mut fresh2 = *(arr.expr).offset(0 as libc::c_int as isize);
    *fresh2 = v;
    return arr;
}
#[no_mangle]
pub unsafe extern "C" fn expr_array_append(
    mut arr: *mut expr_array,
    mut v: *mut AstExpr,
) -> expr_array {
    (*arr).n += 1;
    (*arr).expr = realloc(
        (*arr).expr as *mut libc::c_void,
        (::core::mem::size_of::<*mut AstExpr>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut AstExpr;
    let ref mut fresh3 = *((*arr).expr).offset(((*arr).n - 1 as libc::c_int) as isize);
    *fresh3 = v;
    return *arr;
}
#[no_mangle]
pub unsafe extern "C" fn variable_array_append(
    mut arr: *mut ast_variable_arr,
    mut v: *mut ast_variable,
) -> *mut ast_variable_arr {
    ast_variable_arr_append(arr, v);
    return arr;
}
#[no_mangle]
pub unsafe extern "C" fn variable_array_create(mut v: *mut ast_variable) -> *mut ast_variable_arr {
    return variable_array_append(ast_variable_arr_create(), v);
}
static mut yytranslate: [yytype_uint8; 326] = [
    0 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    82 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    77 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    78 as libc::c_int as yytype_uint8,
    79 as libc::c_int as yytype_uint8,
    76 as libc::c_int as yytype_uint8,
    80 as libc::c_int as yytype_uint8,
    75 as libc::c_int as yytype_uint8,
    83 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    90 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    84 as libc::c_int as yytype_uint8,
    88 as libc::c_int as yytype_uint8,
    85 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    73 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    74 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    86 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    87 as libc::c_int as yytype_uint8,
    81 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    5 as libc::c_int as yytype_uint8,
    6 as libc::c_int as yytype_uint8,
    7 as libc::c_int as yytype_uint8,
    8 as libc::c_int as yytype_uint8,
    9 as libc::c_int as yytype_uint8,
    10 as libc::c_int as yytype_uint8,
    11 as libc::c_int as yytype_uint8,
    12 as libc::c_int as yytype_uint8,
    13 as libc::c_int as yytype_uint8,
    14 as libc::c_int as yytype_uint8,
    15 as libc::c_int as yytype_uint8,
    16 as libc::c_int as yytype_uint8,
    17 as libc::c_int as yytype_uint8,
    18 as libc::c_int as yytype_uint8,
    19 as libc::c_int as yytype_uint8,
    20 as libc::c_int as yytype_uint8,
    21 as libc::c_int as yytype_uint8,
    22 as libc::c_int as yytype_uint8,
    23 as libc::c_int as yytype_uint8,
    24 as libc::c_int as yytype_uint8,
    25 as libc::c_int as yytype_uint8,
    26 as libc::c_int as yytype_uint8,
    27 as libc::c_int as yytype_uint8,
    28 as libc::c_int as yytype_uint8,
    29 as libc::c_int as yytype_uint8,
    30 as libc::c_int as yytype_uint8,
    31 as libc::c_int as yytype_uint8,
    32 as libc::c_int as yytype_uint8,
    33 as libc::c_int as yytype_uint8,
    34 as libc::c_int as yytype_uint8,
    35 as libc::c_int as yytype_uint8,
    36 as libc::c_int as yytype_uint8,
    37 as libc::c_int as yytype_uint8,
    38 as libc::c_int as yytype_uint8,
    39 as libc::c_int as yytype_uint8,
    40 as libc::c_int as yytype_uint8,
    41 as libc::c_int as yytype_uint8,
    42 as libc::c_int as yytype_uint8,
    43 as libc::c_int as yytype_uint8,
    44 as libc::c_int as yytype_uint8,
    45 as libc::c_int as yytype_uint8,
    46 as libc::c_int as yytype_uint8,
    47 as libc::c_int as yytype_uint8,
    48 as libc::c_int as yytype_uint8,
    49 as libc::c_int as yytype_uint8,
    50 as libc::c_int as yytype_uint8,
    51 as libc::c_int as yytype_uint8,
    52 as libc::c_int as yytype_uint8,
    53 as libc::c_int as yytype_uint8,
    54 as libc::c_int as yytype_uint8,
    55 as libc::c_int as yytype_uint8,
    56 as libc::c_int as yytype_uint8,
    57 as libc::c_int as yytype_uint8,
    58 as libc::c_int as yytype_uint8,
    59 as libc::c_int as yytype_uint8,
    60 as libc::c_int as yytype_uint8,
    61 as libc::c_int as yytype_uint8,
    62 as libc::c_int as yytype_uint8,
    63 as libc::c_int as yytype_uint8,
    64 as libc::c_int as yytype_uint8,
    65 as libc::c_int as yytype_uint8,
    66 as libc::c_int as yytype_uint8,
    67 as libc::c_int as yytype_uint8,
    68 as libc::c_int as yytype_uint8,
    69 as libc::c_int as yytype_uint8,
    70 as libc::c_int as yytype_uint8,
];
static mut yyr1: [yytype_uint8; 182] = [
    0 as libc::c_int as yytype_uint8,
    91 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    93 as libc::c_int as yytype_uint8,
    93 as libc::c_int as yytype_uint8,
    93 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    95 as libc::c_int as yytype_uint8,
    95 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    99 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    103 as libc::c_int as yytype_uint8,
    103 as libc::c_int as yytype_uint8,
    103 as libc::c_int as yytype_uint8,
    103 as libc::c_int as yytype_uint8,
    104 as libc::c_int as yytype_uint8,
    104 as libc::c_int as yytype_uint8,
    105 as libc::c_int as yytype_uint8,
    105 as libc::c_int as yytype_uint8,
    106 as libc::c_int as yytype_uint8,
    106 as libc::c_int as yytype_uint8,
    107 as libc::c_int as yytype_uint8,
    107 as libc::c_int as yytype_uint8,
    108 as libc::c_int as yytype_uint8,
    109 as libc::c_int as yytype_uint8,
    110 as libc::c_int as yytype_uint8,
    111 as libc::c_int as yytype_uint8,
    111 as libc::c_int as yytype_uint8,
    112 as libc::c_int as yytype_uint8,
    112 as libc::c_int as yytype_uint8,
    113 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    117 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    119 as libc::c_int as yytype_uint8,
    119 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    121 as libc::c_int as yytype_uint8,
    121 as libc::c_int as yytype_uint8,
    122 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    125 as libc::c_int as yytype_uint8,
    125 as libc::c_int as yytype_uint8,
    125 as libc::c_int as yytype_uint8,
    126 as libc::c_int as yytype_uint8,
    126 as libc::c_int as yytype_uint8,
    127 as libc::c_int as yytype_uint8,
    127 as libc::c_int as yytype_uint8,
    128 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    130 as libc::c_int as yytype_uint8,
    130 as libc::c_int as yytype_uint8,
    131 as libc::c_int as yytype_uint8,
    131 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    133 as libc::c_int as yytype_uint8,
    133 as libc::c_int as yytype_uint8,
    134 as libc::c_int as yytype_uint8,
    134 as libc::c_int as yytype_uint8,
    134 as libc::c_int as yytype_uint8,
    135 as libc::c_int as yytype_uint8,
    135 as libc::c_int as yytype_uint8,
    135 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    137 as libc::c_int as yytype_uint8,
    138 as libc::c_int as yytype_uint8,
    138 as libc::c_int as yytype_uint8,
    139 as libc::c_int as yytype_uint8,
    139 as libc::c_int as yytype_uint8,
    140 as libc::c_int as yytype_uint8,
    140 as libc::c_int as yytype_uint8,
    141 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    143 as libc::c_int as yytype_uint8,
    143 as libc::c_int as yytype_uint8,
    143 as libc::c_int as yytype_uint8,
    144 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    146 as libc::c_int as yytype_uint8,
    147 as libc::c_int as yytype_uint8,
    147 as libc::c_int as yytype_uint8,
    148 as libc::c_int as yytype_uint8,
    148 as libc::c_int as yytype_uint8,
    149 as libc::c_int as yytype_uint8,
    149 as libc::c_int as yytype_uint8,
    150 as libc::c_int as yytype_uint8,
    150 as libc::c_int as yytype_uint8,
    151 as libc::c_int as yytype_uint8,
    151 as libc::c_int as yytype_uint8,
    151 as libc::c_int as yytype_uint8,
    152 as libc::c_int as yytype_uint8,
    152 as libc::c_int as yytype_uint8,
    153 as libc::c_int as yytype_uint8,
    153 as libc::c_int as yytype_uint8,
    154 as libc::c_int as yytype_uint8,
    155 as libc::c_int as yytype_uint8,
    156 as libc::c_int as yytype_uint8,
    157 as libc::c_int as yytype_uint8,
    157 as libc::c_int as yytype_uint8,
    158 as libc::c_int as yytype_uint8,
    158 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    160 as libc::c_int as yytype_uint8,
    160 as libc::c_int as yytype_uint8,
    160 as libc::c_int as yytype_uint8,
];
static mut yyr2: [yytype_uint8; 182] = [
    0 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    5 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    5 as libc::c_int as yytype_uint8,
    7 as libc::c_int as yytype_uint8,
    5 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    8 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
];
static mut yydefact: [yytype_uint8; 281] = [
    0 as libc::c_int as yytype_uint8,
    117 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    91 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    93 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    95 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    113 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    104 as libc::c_int as yytype_uint8,
    105 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    126 as libc::c_int as yytype_uint8,
    174 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    83 as libc::c_int as yytype_uint8,
    85 as libc::c_int as yytype_uint8,
    99 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    84 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    171 as libc::c_int as yytype_uint8,
    173 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    127 as libc::c_int as yytype_uint8,
    87 as libc::c_int as yytype_uint8,
    81 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    88 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    90 as libc::c_int as yytype_uint8,
    121 as libc::c_int as yytype_uint8,
    122 as libc::c_int as yytype_uint8,
    86 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    103 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    148 as libc::c_int as yytype_uint8,
    175 as libc::c_int as yytype_uint8,
    176 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    181 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    1 as libc::c_int as yytype_uint8,
    172 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    82 as libc::c_int as yytype_uint8,
    180 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    108 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    106 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    5 as libc::c_int as yytype_uint8,
    6 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    164 as libc::c_int as yytype_uint8,
    165 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    30 as libc::c_int as yytype_uint8,
    31 as libc::c_int as yytype_uint8,
    32 as libc::c_int as yytype_uint8,
    33 as libc::c_int as yytype_uint8,
    34 as libc::c_int as yytype_uint8,
    35 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    11 as libc::c_int as yytype_uint8,
    19 as libc::c_int as yytype_uint8,
    22 as libc::c_int as yytype_uint8,
    25 as libc::c_int as yytype_uint8,
    36 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    37 as libc::c_int as yytype_uint8,
    40 as libc::c_int as yytype_uint8,
    43 as libc::c_int as yytype_uint8,
    52 as libc::c_int as yytype_uint8,
    56 as libc::c_int as yytype_uint8,
    58 as libc::c_int as yytype_uint8,
    59 as libc::c_int as yytype_uint8,
    60 as libc::c_int as yytype_uint8,
    61 as libc::c_int as yytype_uint8,
    63 as libc::c_int as yytype_uint8,
    65 as libc::c_int as yytype_uint8,
    66 as libc::c_int as yytype_uint8,
    79 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    155 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    157 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    137 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    143 as libc::c_int as yytype_uint8,
    150 as libc::c_int as yytype_uint8,
    149 as libc::c_int as yytype_uint8,
    138 as libc::c_int as yytype_uint8,
    139 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    169 as libc::c_int as yytype_uint8,
    140 as libc::c_int as yytype_uint8,
    141 as libc::c_int as yytype_uint8,
    177 as libc::c_int as yytype_uint8,
    178 as libc::c_int as yytype_uint8,
    119 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    128 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    179 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    122 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    34 as libc::c_int as yytype_uint8,
    36 as libc::c_int as yytype_uint8,
    80 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    2 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    107 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    153 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    26 as libc::c_int as yytype_uint8,
    27 as libc::c_int as yytype_uint8,
    23 as libc::c_int as yytype_uint8,
    24 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    152 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    17 as libc::c_int as yytype_uint8,
    18 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    69 as libc::c_int as yytype_uint8,
    70 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    73 as libc::c_int as yytype_uint8,
    74 as libc::c_int as yytype_uint8,
    75 as libc::c_int as yytype_uint8,
    76 as libc::c_int as yytype_uint8,
    77 as libc::c_int as yytype_uint8,
    78 as libc::c_int as yytype_uint8,
    68 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    28 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    48 as libc::c_int as yytype_uint8,
    49 as libc::c_int as yytype_uint8,
    46 as libc::c_int as yytype_uint8,
    47 as libc::c_int as yytype_uint8,
    51 as libc::c_int as yytype_uint8,
    54 as libc::c_int as yytype_uint8,
    55 as libc::c_int as yytype_uint8,
    51 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    160 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    147 as libc::c_int as yytype_uint8,
    156 as libc::c_int as yytype_uint8,
    151 as libc::c_int as yytype_uint8,
    158 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    131 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    125 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    154 as libc::c_int as yytype_uint8,
    110 as libc::c_int as yytype_uint8,
    133 as libc::c_int as yytype_uint8,
    112 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    146 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    170 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    7 as libc::c_int as yytype_uint8,
    16 as libc::c_int as yytype_uint8,
    13 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    20 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    15 as libc::c_int as yytype_uint8,
    67 as libc::c_int as yytype_uint8,
    38 as libc::c_int as yytype_uint8,
    39 as libc::c_int as yytype_uint8,
    41 as libc::c_int as yytype_uint8,
    42 as libc::c_int as yytype_uint8,
    44 as libc::c_int as yytype_uint8,
    45 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    62 as libc::c_int as yytype_uint8,
    64 as libc::c_int as yytype_uint8,
    144 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    130 as libc::c_int as yytype_uint8,
    109 as libc::c_int as yytype_uint8,
    135 as libc::c_int as yytype_uint8,
    134 as libc::c_int as yytype_uint8,
    111 as libc::c_int as yytype_uint8,
    29 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    8 as libc::c_int as yytype_uint8,
    9 as libc::c_int as yytype_uint8,
    10 as libc::c_int as yytype_uint8,
    14 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    12 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    53 as libc::c_int as yytype_uint8,
    57 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    161 as libc::c_int as yytype_uint8,
    163 as libc::c_int as yytype_uint8,
    21 as libc::c_int as yytype_uint8,
    50 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    166 as libc::c_int as yytype_uint8,
    162 as libc::c_int as yytype_uint8,
    167 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    168 as libc::c_int as yytype_uint8,
];
#[no_mangle]
pub unsafe extern "C" fn yyerror(s: *const libc::c_char) -> libc::c_int {
    fflush(__stdoutp);
    if (marker.filename).is_null() {
        fprintf(
            __stderrp,
            b"error: %s with no lexeme marker\n\0" as *const u8 as *const libc::c_char,
            s,
        );
        exit(1 as libc::c_int);
    }
    if VERBOSE_MODE != 0 {
        fprintf(
            __stderrp,
            b"\n%*s\n\0" as *const u8 as *const libc::c_char,
            marker.column,
            b"^\0" as *const u8 as *const libc::c_char,
        );
    }
    let mut mark: *mut libc::c_char = lexememarker_str(addr_of_mut!(marker));
    fprintf(
        __stderrp,
        b"%s: %s\n\0" as *const u8 as *const libc::c_char,
        mark,
        s,
    );
    free(mark as *mut libc::c_void);
    exit(1 as libc::c_int);
}
static mut yydefgoto: [yytype_int16; 70] = [
    -(1 as libc::c_int) as yytype_int16,
    96 as libc::c_int as yytype_int16,
    97 as libc::c_int as yytype_int16,
    98 as libc::c_int as yytype_int16,
    233 as libc::c_int as yytype_int16,
    99 as libc::c_int as yytype_int16,
    100 as libc::c_int as yytype_int16,
    101 as libc::c_int as yytype_int16,
    102 as libc::c_int as yytype_int16,
    103 as libc::c_int as yytype_int16,
    104 as libc::c_int as yytype_int16,
    105 as libc::c_int as yytype_int16,
    198 as libc::c_int as yytype_int16,
    245 as libc::c_int as yytype_int16,
    106 as libc::c_int as yytype_int16,
    201 as libc::c_int as yytype_int16,
    107 as libc::c_int as yytype_int16,
    108 as libc::c_int as yytype_int16,
    109 as libc::c_int as yytype_int16,
    110 as libc::c_int as yytype_int16,
    111 as libc::c_int as yytype_int16,
    112 as libc::c_int as yytype_int16,
    113 as libc::c_int as yytype_int16,
    114 as libc::c_int as yytype_int16,
    186 as libc::c_int as yytype_int16,
    115 as libc::c_int as yytype_int16,
    147 as libc::c_int as yytype_int16,
    63 as libc::c_int as yytype_int16,
    18 as libc::c_int as yytype_int16,
    64 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    37 as libc::c_int as yytype_int16,
    20 as libc::c_int as yytype_int16,
    21 as libc::c_int as yytype_int16,
    22 as libc::c_int as yytype_int16,
    23 as libc::c_int as yytype_int16,
    65 as libc::c_int as yytype_int16,
    66 as libc::c_int as yytype_int16,
    219 as libc::c_int as yytype_int16,
    24 as libc::c_int as yytype_int16,
    25 as libc::c_int as yytype_int16,
    148 as libc::c_int as yytype_int16,
    27 as libc::c_int as yytype_int16,
    40 as libc::c_int as yytype_int16,
    41 as libc::c_int as yytype_int16,
    28 as libc::c_int as yytype_int16,
    136 as libc::c_int as yytype_int16,
    137 as libc::c_int as yytype_int16,
    138 as libc::c_int as yytype_int16,
    221 as libc::c_int as yytype_int16,
    254 as libc::c_int as yytype_int16,
    118 as libc::c_int as yytype_int16,
    119 as libc::c_int as yytype_int16,
    120 as libc::c_int as yytype_int16,
    121 as libc::c_int as yytype_int16,
    122 as libc::c_int as yytype_int16,
    123 as libc::c_int as yytype_int16,
    124 as libc::c_int as yytype_int16,
    125 as libc::c_int as yytype_int16,
    126 as libc::c_int as yytype_int16,
    127 as libc::c_int as yytype_int16,
    128 as libc::c_int as yytype_int16,
    279 as libc::c_int as yytype_int16,
    129 as libc::c_int as yytype_int16,
    130 as libc::c_int as yytype_int16,
    131 as libc::c_int as yytype_int16,
    29 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    51 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
];
static mut yypact: [yytype_int16; 281] = [
    83 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    -(40 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    11 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    13 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    52 as libc::c_int as yytype_int16,
    -(18 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    536 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    14 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(22 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    52 as libc::c_int as yytype_int16,
    -(18 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(51 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    -(5 as libc::c_int) as yytype_int16,
    -(10 as libc::c_int) as yytype_int16,
    344 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(17 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    604 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    52 as libc::c_int as yytype_int16,
    14 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(51 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    11 as libc::c_int as yytype_int16,
    558 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    257 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    17 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    541 as libc::c_int as yytype_int16,
    541 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    472 as libc::c_int as yytype_int16,
    37 as libc::c_int as yytype_int16,
    55 as libc::c_int as yytype_int16,
    84 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    74 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(10 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    162 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    155 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(34 as libc::c_int) as yytype_int16,
    60 as libc::c_int as yytype_int16,
    132 as libc::c_int as yytype_int16,
    27 as libc::c_int as yytype_int16,
    133 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    72 as libc::c_int as yytype_int16,
    126 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    73 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    78 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    86 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    344 as libc::c_int as yytype_int16,
    431 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    117 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    14 as libc::c_int as yytype_int16,
    125 as libc::c_int as yytype_int16,
    122 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    127 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    128 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    585 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    135 as libc::c_int as yytype_int16,
    204 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    110 as libc::c_int as yytype_int16,
    431 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    121 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    134 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    453 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    67 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    137 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    137 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    431 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    431 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    147 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    623 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    204 as libc::c_int as yytype_int16,
    -(40 as libc::c_int) as yytype_int16,
    204 as libc::c_int as yytype_int16,
    139 as libc::c_int as yytype_int16,
    431 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    140 as libc::c_int as yytype_int16,
    141 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    142 as libc::c_int as yytype_int16,
    148 as libc::c_int as yytype_int16,
    149 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(11 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    156 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(34 as libc::c_int) as yytype_int16,
    -(34 as libc::c_int) as yytype_int16,
    60 as libc::c_int as yytype_int16,
    60 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    72 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    147 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    431 as libc::c_int as yytype_int16,
    431 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    472 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    19 as libc::c_int as yytype_int16,
    132 as libc::c_int as yytype_int16,
    27 as libc::c_int as yytype_int16,
    472 as libc::c_int as yytype_int16,
    173 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    160 as libc::c_int as yytype_int16,
    431 as libc::c_int as yytype_int16,
    157 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    431 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
];
static mut yypgoto: [yytype_int16; 70] = [
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    90 as libc::c_int as yytype_int16,
    1 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(91 as libc::c_int) as yytype_int16,
    -(32 as libc::c_int) as yytype_int16,
    -(7 as libc::c_int) as yytype_int16,
    -(49 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(2 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    39 as libc::c_int as yytype_int16,
    51 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(27 as libc::c_int) as yytype_int16,
    -(160 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(82 as libc::c_int) as yytype_int16,
    178 as libc::c_int as yytype_int16,
    8 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    6 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    199 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(111 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    190 as libc::c_int as yytype_int16,
    -(56 as libc::c_int) as yytype_int16,
    -(133 as libc::c_int) as yytype_int16,
    -(98 as libc::c_int) as yytype_int16,
    41 as libc::c_int as yytype_int16,
    0 as libc::c_int as yytype_int16,
    -(3 as libc::c_int) as yytype_int16,
    130 as libc::c_int as yytype_int16,
    -(39 as libc::c_int) as yytype_int16,
    -(12 as libc::c_int) as yytype_int16,
    208 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    56 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(112 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    21 as libc::c_int as yytype_int16,
    202 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(23 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    152 as libc::c_int as yytype_int16,
    -(199 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    183 as libc::c_int as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
    243 as libc::c_int as yytype_int16,
    -(20 as libc::c_int) as yytype_int16,
    -(200 as libc::c_int) as yytype_int16,
];
static mut yytable: [yytype_uint16; 677] = [
    26 as libc::c_int as yytype_uint16,
    163 as libc::c_int as yytype_uint16,
    50 as libc::c_int as yytype_uint16,
    62 as libc::c_int as yytype_uint16,
    33 as libc::c_int as yytype_uint16,
    167 as libc::c_int as yytype_uint16,
    19 as libc::c_int as yytype_uint16,
    42 as libc::c_int as yytype_uint16,
    17 as libc::c_int as yytype_uint16,
    150 as libc::c_int as yytype_uint16,
    187 as libc::c_int as yytype_uint16,
    250 as libc::c_int as yytype_uint16,
    234 as libc::c_int as yytype_uint16,
    209 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    50 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    59 as libc::c_int as yytype_uint16,
    39 as libc::c_int as yytype_uint16,
    60 as libc::c_int as yytype_uint16,
    32 as libc::c_int as yytype_uint16,
    61 as libc::c_int as yytype_uint16,
    45 as libc::c_int as yytype_uint16,
    34 as libc::c_int as yytype_uint16,
    53 as libc::c_int as yytype_uint16,
    237 as libc::c_int as yytype_uint16,
    43 as libc::c_int as yytype_uint16,
    26 as libc::c_int as yytype_uint16,
    26 as libc::c_int as yytype_uint16,
    192 as libc::c_int as yytype_uint16,
    193 as libc::c_int as yytype_uint16,
    26 as libc::c_int as yytype_uint16,
    50 as libc::c_int as yytype_uint16,
    146 as libc::c_int as yytype_uint16,
    19 as libc::c_int as yytype_uint16,
    139 as libc::c_int as yytype_uint16,
    17 as libc::c_int as yytype_uint16,
    16 as libc::c_int as yytype_uint16,
    53 as libc::c_int as yytype_uint16,
    194 as libc::c_int as yytype_uint16,
    195 as libc::c_int as yytype_uint16,
    39 as libc::c_int as yytype_uint16,
    218 as libc::c_int as yytype_uint16,
    188 as libc::c_int as yytype_uint16,
    142 as libc::c_int as yytype_uint16,
    49 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    223 as libc::c_int as yytype_uint16,
    189 as libc::c_int as yytype_uint16,
    146 as libc::c_int as yytype_uint16,
    269 as libc::c_int as yytype_uint16,
    142 as libc::c_int as yytype_uint16,
    52 as libc::c_int as yytype_uint16,
    57 as libc::c_int as yytype_uint16,
    116 as libc::c_int as yytype_uint16,
    220 as libc::c_int as yytype_uint16,
    141 as libc::c_int as yytype_uint16,
    135 as libc::c_int as yytype_uint16,
    49 as libc::c_int as yytype_uint16,
    38 as libc::c_int as yytype_uint16,
    263 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    68 as libc::c_int as yytype_uint16,
    141 as libc::c_int as yytype_uint16,
    264 as libc::c_int as yytype_uint16,
    135 as libc::c_int as yytype_uint16,
    58 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    47 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    133 as libc::c_int as yytype_uint16,
    132 as libc::c_int as yytype_uint16,
    56 as libc::c_int as yytype_uint16,
    155 as libc::c_int as yytype_uint16,
    156 as libc::c_int as yytype_uint16,
    116 as libc::c_int as yytype_uint16,
    49 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    224 as libc::c_int as yytype_uint16,
    225 as libc::c_int as yytype_uint16,
    67 as libc::c_int as yytype_uint16,
    227 as libc::c_int as yytype_uint16,
    228 as libc::c_int as yytype_uint16,
    229 as libc::c_int as yytype_uint16,
    252 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    255 as libc::c_int as yytype_uint16,
    154 as libc::c_int as yytype_uint16,
    16 as libc::c_int as yytype_uint16,
    202 as libc::c_int as yytype_uint16,
    235 as libc::c_int as yytype_uint16,
    16 as libc::c_int as yytype_uint16,
    249 as libc::c_int as yytype_uint16,
    160 as libc::c_int as yytype_uint16,
    150 as libc::c_int as yytype_uint16,
    209 as libc::c_int as yytype_uint16,
    238 as libc::c_int as yytype_uint16,
    239 as libc::c_int as yytype_uint16,
    44 as libc::c_int as yytype_uint16,
    35 as libc::c_int as yytype_uint16,
    161 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    62 as libc::c_int as yytype_uint16,
    272 as libc::c_int as yytype_uint16,
    162 as libc::c_int as yytype_uint16,
    273 as libc::c_int as yytype_uint16,
    218 as libc::c_int as yytype_uint16,
    164 as libc::c_int as yytype_uint16,
    218 as libc::c_int as yytype_uint16,
    257 as libc::c_int as yytype_uint16,
    196 as libc::c_int as yytype_uint16,
    197 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    220 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    220 as libc::c_int as yytype_uint16,
    142 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    165 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    207 as libc::c_int as yytype_uint16,
    46 as libc::c_int as yytype_uint16,
    15 as libc::c_int as yytype_uint16,
    141 as libc::c_int as yytype_uint16,
    81 as libc::c_int as yytype_uint16,
    82 as libc::c_int as yytype_uint16,
    47 as libc::c_int as yytype_uint16,
    190 as libc::c_int as yytype_uint16,
    191 as libc::c_int as yytype_uint16,
    48 as libc::c_int as yytype_uint16,
    141 as libc::c_int as yytype_uint16,
    192 as libc::c_int as yytype_uint16,
    193 as libc::c_int as yytype_uint16,
    203 as libc::c_int as yytype_uint16,
    270 as libc::c_int as yytype_uint16,
    271 as libc::c_int as yytype_uint16,
    199 as libc::c_int as yytype_uint16,
    200 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    72 as libc::c_int as yytype_uint16,
    166 as libc::c_int as yytype_uint16,
    73 as libc::c_int as yytype_uint16,
    74 as libc::c_int as yytype_uint16,
    240 as libc::c_int as yytype_uint16,
    241 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    16 as libc::c_int as yytype_uint16,
    204 as libc::c_int as yytype_uint16,
    277 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    157 as libc::c_int as yytype_uint16,
    158 as libc::c_int as yytype_uint16,
    280 as libc::c_int as yytype_uint16,
    205 as libc::c_int as yytype_uint16,
    231 as libc::c_int as yytype_uint16,
    169 as libc::c_int as yytype_uint16,
    170 as libc::c_int as yytype_uint16,
    171 as libc::c_int as yytype_uint16,
    206 as libc::c_int as yytype_uint16,
    236 as libc::c_int as yytype_uint16,
    175 as libc::c_int as yytype_uint16,
    176 as libc::c_int as yytype_uint16,
    177 as libc::c_int as yytype_uint16,
    178 as libc::c_int as yytype_uint16,
    179 as libc::c_int as yytype_uint16,
    180 as libc::c_int as yytype_uint16,
    181 as libc::c_int as yytype_uint16,
    182 as libc::c_int as yytype_uint16,
    183 as libc::c_int as yytype_uint16,
    184 as libc::c_int as yytype_uint16,
    242 as libc::c_int as yytype_uint16,
    243 as libc::c_int as yytype_uint16,
    274 as libc::c_int as yytype_uint16,
    210 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    266 as libc::c_int as yytype_uint16,
    267 as libc::c_int as yytype_uint16,
    212 as libc::c_int as yytype_uint16,
    213 as libc::c_int as yytype_uint16,
    214 as libc::c_int as yytype_uint16,
    222 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    215 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    230 as libc::c_int as yytype_uint16,
    253 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    217 as libc::c_int as yytype_uint16,
    226 as libc::c_int as yytype_uint16,
    256 as libc::c_int as yytype_uint16,
    258 as libc::c_int as yytype_uint16,
    259 as libc::c_int as yytype_uint16,
    260 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    135 as libc::c_int as yytype_uint16,
    261 as libc::c_int as yytype_uint16,
    262 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    244 as libc::c_int as yytype_uint16,
    89 as libc::c_int as yytype_uint16,
    90 as libc::c_int as yytype_uint16,
    91 as libc::c_int as yytype_uint16,
    92 as libc::c_int as yytype_uint16,
    144 as libc::c_int as yytype_uint16,
    94 as libc::c_int as yytype_uint16,
    265 as libc::c_int as yytype_uint16,
    275 as libc::c_int as yytype_uint16,
    276 as libc::c_int as yytype_uint16,
    172 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    173 as libc::c_int as yytype_uint16,
    95 as libc::c_int as yytype_uint16,
    174 as libc::c_int as yytype_uint16,
    46 as libc::c_int as yytype_uint16,
    246 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    247 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    185 as libc::c_int as yytype_uint16,
    268 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    145 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    278 as libc::c_int as yytype_uint16,
    248 as libc::c_int as yytype_uint16,
    159 as libc::c_int as yytype_uint16,
    140 as libc::c_int as yytype_uint16,
    151 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    72 as libc::c_int as yytype_uint16,
    211 as libc::c_int as yytype_uint16,
    73 as libc::c_int as yytype_uint16,
    74 as libc::c_int as yytype_uint16,
    143 as libc::c_int as yytype_uint16,
    251 as libc::c_int as yytype_uint16,
    153 as libc::c_int as yytype_uint16,
    168 as libc::c_int as yytype_uint16,
    55 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    208 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    117 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    77 as libc::c_int as yytype_uint16,
    78 as libc::c_int as yytype_uint16,
    79 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    80 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    81 as libc::c_int as yytype_uint16,
    82 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    83 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    152 as libc::c_int as yytype_uint16,
    88 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    89 as libc::c_int as yytype_uint16,
    90 as libc::c_int as yytype_uint16,
    91 as libc::c_int as yytype_uint16,
    92 as libc::c_int as yytype_uint16,
    93 as libc::c_int as yytype_uint16,
    94 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    47 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    95 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    72 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    73 as libc::c_int as yytype_uint16,
    74 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    77 as libc::c_int as yytype_uint16,
    78 as libc::c_int as yytype_uint16,
    79 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    80 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    81 as libc::c_int as yytype_uint16,
    82 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    83 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    88 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    89 as libc::c_int as yytype_uint16,
    90 as libc::c_int as yytype_uint16,
    91 as libc::c_int as yytype_uint16,
    92 as libc::c_int as yytype_uint16,
    93 as libc::c_int as yytype_uint16,
    94 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    47 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    95 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    72 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    73 as libc::c_int as yytype_uint16,
    74 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    72 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    73 as libc::c_int as yytype_uint16,
    74 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    72 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    73 as libc::c_int as yytype_uint16,
    74 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    77 as libc::c_int as yytype_uint16,
    78 as libc::c_int as yytype_uint16,
    79 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    80 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    81 as libc::c_int as yytype_uint16,
    82 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    83 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    88 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    89 as libc::c_int as yytype_uint16,
    90 as libc::c_int as yytype_uint16,
    91 as libc::c_int as yytype_uint16,
    92 as libc::c_int as yytype_uint16,
    93 as libc::c_int as yytype_uint16,
    94 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    47 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    95 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    232 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    89 as libc::c_int as yytype_uint16,
    90 as libc::c_int as yytype_uint16,
    91 as libc::c_int as yytype_uint16,
    92 as libc::c_int as yytype_uint16,
    144 as libc::c_int as yytype_uint16,
    94 as libc::c_int as yytype_uint16,
    54 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    1 as libc::c_int as yytype_uint16,
    69 as libc::c_int as yytype_uint16,
    70 as libc::c_int as yytype_uint16,
    71 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    89 as libc::c_int as yytype_uint16,
    90 as libc::c_int as yytype_uint16,
    91 as libc::c_int as yytype_uint16,
    92 as libc::c_int as yytype_uint16,
    144 as libc::c_int as yytype_uint16,
    94 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    75 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    15 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    76 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    84 as libc::c_int as yytype_uint16,
    85 as libc::c_int as yytype_uint16,
    86 as libc::c_int as yytype_uint16,
    87 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    16 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    149 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    2 as libc::c_int as yytype_uint16,
    3 as libc::c_int as yytype_uint16,
    4 as libc::c_int as yytype_uint16,
    5 as libc::c_int as yytype_uint16,
    6 as libc::c_int as yytype_uint16,
    7 as libc::c_int as yytype_uint16,
    8 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    9 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    10 as libc::c_int as yytype_uint16,
    11 as libc::c_int as yytype_uint16,
    12 as libc::c_int as yytype_uint16,
    13 as libc::c_int as yytype_uint16,
    14 as libc::c_int as yytype_uint16,
    216 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    0 as libc::c_int as yytype_uint16,
    134 as libc::c_int as yytype_uint16,
];
static mut yycheck: [yytype_int16; 677] = [
    0 as libc::c_int as yytype_int16,
    83 as libc::c_int as yytype_int16,
    25 as libc::c_int as yytype_int16,
    42 as libc::c_int as yytype_int16,
    16 as libc::c_int as yytype_int16,
    87 as libc::c_int as yytype_int16,
    0 as libc::c_int as yytype_int16,
    19 as libc::c_int as yytype_int16,
    0 as libc::c_int as yytype_int16,
    65 as libc::c_int as yytype_int16,
    101 as libc::c_int as yytype_int16,
    210 as libc::c_int as yytype_int16,
    172 as libc::c_int as yytype_int16,
    125 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    38 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    38 as libc::c_int as yytype_int16,
    19 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    15 as libc::c_int as yytype_int16,
    73 as libc::c_int as yytype_int16,
    23 as libc::c_int as yytype_int16,
    18 as libc::c_int as yytype_int16,
    28 as libc::c_int as yytype_int16,
    186 as libc::c_int as yytype_int16,
    21 as libc::c_int as yytype_int16,
    28 as libc::c_int as yytype_int16,
    29 as libc::c_int as yytype_int16,
    11 as libc::c_int as yytype_int16,
    12 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    56 as libc::c_int as yytype_int16,
    61 as libc::c_int as yytype_int16,
    29 as libc::c_int as yytype_int16,
    56 as libc::c_int as yytype_int16,
    29 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    42 as libc::c_int as yytype_int16,
    13 as libc::c_int as yytype_int16,
    14 as libc::c_int as yytype_int16,
    42 as libc::c_int as yytype_int16,
    154 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    57 as libc::c_int as yytype_int16,
    25 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    160 as libc::c_int as yytype_int16,
    83 as libc::c_int as yytype_int16,
    77 as libc::c_int as yytype_int16,
    250 as libc::c_int as yytype_int16,
    64 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    76 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    154 as libc::c_int as yytype_int16,
    57 as libc::c_int as yytype_int16,
    52 as libc::c_int as yytype_int16,
    38 as libc::c_int as yytype_int16,
    19 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    61 as libc::c_int as yytype_int16,
    73 as libc::c_int as yytype_int16,
    64 as libc::c_int as yytype_int16,
    76 as libc::c_int as yytype_int16,
    60 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    86 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    50 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    73 as libc::c_int as yytype_int16,
    74 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    56 as libc::c_int as yytype_int16,
    77 as libc::c_int as yytype_int16,
    161 as libc::c_int as yytype_int16,
    162 as libc::c_int as yytype_int16,
    86 as libc::c_int as yytype_int16,
    164 as libc::c_int as yytype_int16,
    165 as libc::c_int as yytype_int16,
    166 as libc::c_int as yytype_int16,
    218 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    220 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    18 as libc::c_int as yytype_int16,
    173 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    205 as libc::c_int as yytype_int16,
    90 as libc::c_int as yytype_int16,
    151 as libc::c_int as yytype_int16,
    208 as libc::c_int as yytype_int16,
    188 as libc::c_int as yytype_int16,
    189 as libc::c_int as yytype_int16,
    86 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    101 as libc::c_int as yytype_int16,
    142 as libc::c_int as yytype_int16,
    264 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    87 as libc::c_int as yytype_int16,
    218 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    220 as libc::c_int as yytype_int16,
    222 as libc::c_int as yytype_int16,
    84 as libc::c_int as yytype_int16,
    85 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    218 as libc::c_int as yytype_int16,
    38 as libc::c_int as yytype_int16,
    220 as libc::c_int as yytype_int16,
    135 as libc::c_int as yytype_int16,
    124 as libc::c_int as yytype_int16,
    125 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    124 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    51 as libc::c_int as yytype_int16,
    135 as libc::c_int as yytype_int16,
    62 as libc::c_int as yytype_int16,
    63 as libc::c_int as yytype_int16,
    86 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    142 as libc::c_int as yytype_int16,
    11 as libc::c_int as yytype_int16,
    12 as libc::c_int as yytype_int16,
    19 as libc::c_int as yytype_int16,
    258 as libc::c_int as yytype_int16,
    259 as libc::c_int as yytype_int16,
    15 as libc::c_int as yytype_int16,
    16 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    7 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    190 as libc::c_int as yytype_int16,
    191 as libc::c_int as yytype_int16,
    160 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    275 as libc::c_int as yytype_int16,
    17 as libc::c_int as yytype_int16,
    75 as libc::c_int as yytype_int16,
    76 as libc::c_int as yytype_int16,
    279 as libc::c_int as yytype_int16,
    90 as libc::c_int as yytype_int16,
    169 as libc::c_int as yytype_int16,
    8 as libc::c_int as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    87 as libc::c_int as yytype_int16,
    174 as libc::c_int as yytype_int16,
    20 as libc::c_int as yytype_int16,
    21 as libc::c_int as yytype_int16,
    22 as libc::c_int as yytype_int16,
    23 as libc::c_int as yytype_int16,
    24 as libc::c_int as yytype_int16,
    25 as libc::c_int as yytype_int16,
    26 as libc::c_int as yytype_int16,
    27 as libc::c_int as yytype_int16,
    28 as libc::c_int as yytype_int16,
    29 as libc::c_int as yytype_int16,
    192 as libc::c_int as yytype_int16,
    193 as libc::c_int as yytype_int16,
    269 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    188 as libc::c_int as yytype_int16,
    189 as libc::c_int as yytype_int16,
    190 as libc::c_int as yytype_int16,
    191 as libc::c_int as yytype_int16,
    192 as libc::c_int as yytype_int16,
    193 as libc::c_int as yytype_int16,
    244 as libc::c_int as yytype_int16,
    245 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    76 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    90 as libc::c_int as yytype_int16,
    54 as libc::c_int as yytype_int16,
    74 as libc::c_int as yytype_int16,
    202 as libc::c_int as yytype_int16,
    203 as libc::c_int as yytype_int16,
    205 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    219 as libc::c_int as yytype_int16,
    208 as libc::c_int as yytype_int16,
    74 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    213 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    222 as libc::c_int as yytype_int16,
    86 as libc::c_int as yytype_int16,
    77 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    82 as libc::c_int as yytype_int16,
    74 as libc::c_int as yytype_int16,
    58 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    73 as libc::c_int as yytype_int16,
    89 as libc::c_int as yytype_int16,
    75 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    201 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    202 as libc::c_int as yytype_int16,
    38 as libc::c_int as yytype_int16,
    88 as libc::c_int as yytype_int16,
    246 as libc::c_int as yytype_int16,
    244 as libc::c_int as yytype_int16,
    245 as libc::c_int as yytype_int16,
    246 as libc::c_int as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    276 as libc::c_int as yytype_int16,
    203 as libc::c_int as yytype_int16,
    77 as libc::c_int as yytype_int16,
    57 as libc::c_int as yytype_int16,
    67 as libc::c_int as yytype_int16,
    258 as libc::c_int as yytype_int16,
    259 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    7 as libc::c_int as yytype_int16,
    135 as libc::c_int as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    60 as libc::c_int as yytype_int16,
    213 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    88 as libc::c_int as yytype_int16,
    29 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    17 as libc::c_int as yytype_int16,
    275 as libc::c_int as yytype_int16,
    124 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    279 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    54 as libc::c_int as yytype_int16,
    55 as libc::c_int as yytype_int16,
    56 as libc::c_int as yytype_int16,
    57 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    59 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    62 as libc::c_int as yytype_int16,
    63 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    74 as libc::c_int as yytype_int16,
    75 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    77 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    82 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    86 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    89 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    7 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    17 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    54 as libc::c_int as yytype_int16,
    55 as libc::c_int as yytype_int16,
    56 as libc::c_int as yytype_int16,
    57 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    59 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    62 as libc::c_int as yytype_int16,
    63 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    75 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    77 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    82 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    86 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    89 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    7 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    17 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    7 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    17 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    7 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    9 as libc::c_int as yytype_int16,
    10 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    54 as libc::c_int as yytype_int16,
    55 as libc::c_int as yytype_int16,
    56 as libc::c_int as yytype_int16,
    57 as libc::c_int as yytype_int16,
    17 as libc::c_int as yytype_int16,
    59 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    62 as libc::c_int as yytype_int16,
    63 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    67 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    75 as libc::c_int as yytype_int16,
    54 as libc::c_int as yytype_int16,
    77 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    82 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    86 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    89 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    72 as libc::c_int as yytype_int16,
    54 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    77 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    82 as libc::c_int as yytype_int16,
    0 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    3 as libc::c_int as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    3 as libc::c_int as yytype_int16,
    4 as libc::c_int as yytype_int16,
    5 as libc::c_int as yytype_int16,
    6 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    77 as libc::c_int as yytype_int16,
    78 as libc::c_int as yytype_int16,
    79 as libc::c_int as yytype_int16,
    80 as libc::c_int as yytype_int16,
    81 as libc::c_int as yytype_int16,
    82 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    17 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    51 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    54 as libc::c_int as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    68 as libc::c_int as yytype_int16,
    69 as libc::c_int as yytype_int16,
    70 as libc::c_int as yytype_int16,
    71 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    78 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    87 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    30 as libc::c_int as yytype_int16,
    31 as libc::c_int as yytype_int16,
    32 as libc::c_int as yytype_int16,
    33 as libc::c_int as yytype_int16,
    34 as libc::c_int as yytype_int16,
    35 as libc::c_int as yytype_int16,
    36 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    38 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    44 as libc::c_int as yytype_int16,
    45 as libc::c_int as yytype_int16,
    46 as libc::c_int as yytype_int16,
    47 as libc::c_int as yytype_int16,
    48 as libc::c_int as yytype_int16,
    87 as libc::c_int as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    -(1 as libc::c_int) as yytype_int16,
    72 as libc::c_int as yytype_int16,
];
static mut yystos: [yytype_uint8; 281] = [
    0 as libc::c_int as yytype_uint8,
    3 as libc::c_int as yytype_uint8,
    30 as libc::c_int as yytype_uint8,
    31 as libc::c_int as yytype_uint8,
    32 as libc::c_int as yytype_uint8,
    33 as libc::c_int as yytype_uint8,
    34 as libc::c_int as yytype_uint8,
    35 as libc::c_int as yytype_uint8,
    36 as libc::c_int as yytype_uint8,
    38 as libc::c_int as yytype_uint8,
    44 as libc::c_int as yytype_uint8,
    45 as libc::c_int as yytype_uint8,
    46 as libc::c_int as yytype_uint8,
    47 as libc::c_int as yytype_uint8,
    48 as libc::c_int as yytype_uint8,
    51 as libc::c_int as yytype_uint8,
    78 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    119 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    123 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    125 as libc::c_int as yytype_uint8,
    126 as libc::c_int as yytype_uint8,
    130 as libc::c_int as yytype_uint8,
    131 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    133 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    157 as libc::c_int as yytype_uint8,
    158 as libc::c_int as yytype_uint8,
    160 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    121 as libc::c_int as yytype_uint8,
    122 as libc::c_int as yytype_uint8,
    131 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    134 as libc::c_int as yytype_uint8,
    135 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    86 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    81 as libc::c_int as yytype_uint8,
    86 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    144 as libc::c_int as yytype_uint8,
    147 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    133 as libc::c_int as yytype_uint8,
    0 as libc::c_int as yytype_uint8,
    158 as libc::c_int as yytype_uint8,
    131 as libc::c_int as yytype_uint8,
    76 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    73 as libc::c_int as yytype_uint8,
    135 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    127 as libc::c_int as yytype_uint8,
    128 as libc::c_int as yytype_uint8,
    86 as libc::c_int as yytype_uint8,
    73 as libc::c_int as yytype_uint8,
    4 as libc::c_int as yytype_uint8,
    5 as libc::c_int as yytype_uint8,
    6 as libc::c_int as yytype_uint8,
    7 as libc::c_int as yytype_uint8,
    9 as libc::c_int as yytype_uint8,
    10 as libc::c_int as yytype_uint8,
    17 as libc::c_int as yytype_uint8,
    54 as libc::c_int as yytype_uint8,
    55 as libc::c_int as yytype_uint8,
    56 as libc::c_int as yytype_uint8,
    57 as libc::c_int as yytype_uint8,
    59 as libc::c_int as yytype_uint8,
    62 as libc::c_int as yytype_uint8,
    63 as libc::c_int as yytype_uint8,
    67 as libc::c_int as yytype_uint8,
    68 as libc::c_int as yytype_uint8,
    69 as libc::c_int as yytype_uint8,
    70 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    75 as libc::c_int as yytype_uint8,
    77 as libc::c_int as yytype_uint8,
    78 as libc::c_int as yytype_uint8,
    79 as libc::c_int as yytype_uint8,
    80 as libc::c_int as yytype_uint8,
    81 as libc::c_int as yytype_uint8,
    82 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    92 as libc::c_int as yytype_uint8,
    93 as libc::c_int as yytype_uint8,
    94 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    98 as libc::c_int as yytype_uint8,
    99 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    105 as libc::c_int as yytype_uint8,
    107 as libc::c_int as yytype_uint8,
    108 as libc::c_int as yytype_uint8,
    109 as libc::c_int as yytype_uint8,
    110 as libc::c_int as yytype_uint8,
    111 as libc::c_int as yytype_uint8,
    112 as libc::c_int as yytype_uint8,
    113 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    143 as libc::c_int as yytype_uint8,
    144 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    146 as libc::c_int as yytype_uint8,
    147 as libc::c_int as yytype_uint8,
    148 as libc::c_int as yytype_uint8,
    149 as libc::c_int as yytype_uint8,
    150 as libc::c_int as yytype_uint8,
    151 as libc::c_int as yytype_uint8,
    152 as libc::c_int as yytype_uint8,
    154 as libc::c_int as yytype_uint8,
    155 as libc::c_int as yytype_uint8,
    156 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    144 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    120 as libc::c_int as yytype_uint8,
    137 as libc::c_int as yytype_uint8,
    138 as libc::c_int as yytype_uint8,
    139 as libc::c_int as yytype_uint8,
    159 as libc::c_int as yytype_uint8,
    122 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    137 as libc::c_int as yytype_uint8,
    81 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    113 as libc::c_int as yytype_uint8,
    117 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    87 as libc::c_int as yytype_uint8,
    128 as libc::c_int as yytype_uint8,
    127 as libc::c_int as yytype_uint8,
    74 as libc::c_int as yytype_uint8,
    145 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    97 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    96 as libc::c_int as yytype_uint8,
    117 as libc::c_int as yytype_uint8,
    90 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    154 as libc::c_int as yytype_uint8,
    8 as libc::c_int as yytype_uint8,
    9 as libc::c_int as yytype_uint8,
    10 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    73 as libc::c_int as yytype_uint8,
    75 as libc::c_int as yytype_uint8,
    20 as libc::c_int as yytype_uint8,
    21 as libc::c_int as yytype_uint8,
    22 as libc::c_int as yytype_uint8,
    23 as libc::c_int as yytype_uint8,
    24 as libc::c_int as yytype_uint8,
    25 as libc::c_int as yytype_uint8,
    26 as libc::c_int as yytype_uint8,
    27 as libc::c_int as yytype_uint8,
    28 as libc::c_int as yytype_uint8,
    29 as libc::c_int as yytype_uint8,
    88 as libc::c_int as yytype_uint8,
    115 as libc::c_int as yytype_uint8,
    99 as libc::c_int as yytype_uint8,
    78 as libc::c_int as yytype_uint8,
    83 as libc::c_int as yytype_uint8,
    79 as libc::c_int as yytype_uint8,
    80 as libc::c_int as yytype_uint8,
    11 as libc::c_int as yytype_uint8,
    12 as libc::c_int as yytype_uint8,
    13 as libc::c_int as yytype_uint8,
    14 as libc::c_int as yytype_uint8,
    84 as libc::c_int as yytype_uint8,
    85 as libc::c_int as yytype_uint8,
    103 as libc::c_int as yytype_uint8,
    15 as libc::c_int as yytype_uint8,
    16 as libc::c_int as yytype_uint8,
    106 as libc::c_int as yytype_uint8,
    18 as libc::c_int as yytype_uint8,
    19 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    90 as libc::c_int as yytype_uint8,
    87 as libc::c_int as yytype_uint8,
    118 as libc::c_int as yytype_uint8,
    149 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    71 as libc::c_int as yytype_uint8,
    134 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    76 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    74 as libc::c_int as yytype_uint8,
    87 as libc::c_int as yytype_uint8,
    74 as libc::c_int as yytype_uint8,
    124 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    130 as libc::c_int as yytype_uint8,
    140 as libc::c_int as yytype_uint8,
    90 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    89 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    95 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    132 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    99 as libc::c_int as yytype_uint8,
    99 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    100 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    101 as libc::c_int as yytype_uint8,
    86 as libc::c_int as yytype_uint8,
    104 as libc::c_int as yytype_uint8,
    104 as libc::c_int as yytype_uint8,
    110 as libc::c_int as yytype_uint8,
    111 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    150 as libc::c_int as yytype_uint8,
    139 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    136 as libc::c_int as yytype_uint8,
    141 as libc::c_int as yytype_uint8,
    129 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    76 as libc::c_int as yytype_uint8,
    74 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    102 as libc::c_int as yytype_uint8,
    105 as libc::c_int as yytype_uint8,
    150 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    114 as libc::c_int as yytype_uint8,
    87 as libc::c_int as yytype_uint8,
    116 as libc::c_int as yytype_uint8,
    58 as libc::c_int as yytype_uint8,
    72 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
    147 as libc::c_int as yytype_uint8,
    153 as libc::c_int as yytype_uint8,
    142 as libc::c_int as yytype_uint8,
];
unsafe extern "C" fn yydestruct(
    mut yymsg: *const libc::c_char,
    mut yytype: libc::c_int,
    mut yyvaluep: *mut YYSTYPE,
) {
    if yymsg.is_null() {
        yymsg = b"Deleting\0" as *const u8 as *const libc::c_char;
    }
    match yytype {
        _ => {}
    };
}
#[no_mangle]
pub static mut yychar: libc::c_int = 0;
#[no_mangle]
pub static mut yylval: YYSTYPE = YYSTYPE {
    string: 0 as *const libc::c_char as *mut libc::c_char,
};
#[no_mangle]
pub static mut yynerrs: libc::c_int = 0;
#[no_mangle]
pub unsafe extern "C" fn yyparse() -> libc::c_int {
    let mut current_block: u64;
    let mut yystate: libc::c_int = 0;
    let mut yyn: libc::c_int = 0;
    let mut yyresult: libc::c_int = 0;
    let mut yyerrstatus: libc::c_int = 0;
    let mut yytoken: libc::c_int = 0 as libc::c_int;
    let mut yyssa: [yytype_int16; 200] = [0; 200];
    let mut yyss: *mut yytype_int16 = yyssa.as_mut_ptr();
    let mut yyssp: *mut yytype_int16 = 0 as *mut yytype_int16;
    let mut yyvsa: [YYSTYPE; 200] = [YYSTYPE {
        string: 0 as *const libc::c_char as *mut libc::c_char,
    }; 200];
    let mut yyvs: *mut YYSTYPE = yyvsa.as_mut_ptr();
    let mut yyvsp: *mut YYSTYPE = 0 as *mut YYSTYPE;
    let mut yystacksize: libc::c_ulong = 200 as libc::c_int as libc::c_ulong;
    let mut yyval: YYSTYPE = YYSTYPE {
        string: 0 as *const libc::c_char as *mut libc::c_char,
    };
    let mut yylen: libc::c_int = 0 as libc::c_int;
    yystate = 0 as libc::c_int;
    yyerrstatus = 0 as libc::c_int;
    yynerrs = 0 as libc::c_int;
    yychar = -(2 as libc::c_int);
    yyssp = yyss;
    yyvsp = yyvs;
    '_yysetstate: loop {
        *yyssp = yystate as yytype_int16;
        if yyss
            .offset(yystacksize as isize)
            .offset(-(1 as libc::c_int as isize))
            <= yyssp
        {
            let mut yysize: libc::c_ulong = (yyssp.offset_from(yyss) as libc::c_long
                + 1 as libc::c_int as libc::c_long)
                as libc::c_ulong;
            if 10000 as libc::c_int as libc::c_ulong <= yystacksize {
                current_block = 17309628368869720559;
                break;
            }
            yystacksize = yystacksize.wrapping_mul(2 as libc::c_int as libc::c_ulong);
            if (10000 as libc::c_int as libc::c_ulong) < yystacksize {
                yystacksize = 10000 as libc::c_int as libc::c_ulong;
            }
            let mut yyss1: *mut yytype_int16 = yyss;
            let mut yyptr: *mut yyalloc = malloc(
                (yystacksize as usize)
                    .wrapping_mul(
                        (::core::mem::size_of::<yytype_int16>() as usize)
                            .wrapping_add(::core::mem::size_of::<YYSTYPE>() as usize),
                    )
                    .wrapping_add((::core::mem::size_of::<yyalloc>()).wrapping_sub(1)),
            ) as *mut yyalloc;
            if yyptr.is_null() {
                current_block = 17309628368869720559;
                break;
            }
            let mut yynewbytes: libc::c_ulong = 0;
            libc::memcpy(
                &mut (*yyptr).yyss as *mut yytype_int16 as *mut libc::c_void,
                yyss as *const libc::c_void,
                yysize.wrapping_mul(::core::mem::size_of::<yytype_int16>() as libc::c_ulong)
                    as libc::size_t,
            );
            yyss = &mut (*yyptr).yyss;
            yynewbytes = yystacksize
                .wrapping_mul(::core::mem::size_of::<yytype_int16>() as libc::c_ulong)
                .wrapping_add(
                    (::core::mem::size_of::<yyalloc>() as libc::c_ulong)
                        .wrapping_sub(1 as libc::c_int as libc::c_ulong),
                );
            yyptr = yyptr.offset(
                yynewbytes.wrapping_div(::core::mem::size_of::<yyalloc>() as libc::c_ulong)
                    as isize,
            );
            let mut yynewbytes_0: libc::c_ulong = 0;
            libc::memcpy(
                &mut (*yyptr).yyvs as *mut YYSTYPE as *mut libc::c_void,
                yyvs as *const libc::c_void,
                yysize.wrapping_mul(::core::mem::size_of::<YYSTYPE>() as libc::c_ulong)
                    as libc::size_t,
            );
            yyvs = &mut (*yyptr).yyvs;
            yynewbytes_0 = yystacksize
                .wrapping_mul(::core::mem::size_of::<YYSTYPE>() as libc::c_ulong)
                .wrapping_add(
                    (::core::mem::size_of::<yyalloc>() as libc::c_ulong)
                        .wrapping_sub(1 as libc::c_int as libc::c_ulong),
                );
            yyptr = yyptr.offset(
                yynewbytes_0.wrapping_div(::core::mem::size_of::<yyalloc>() as libc::c_ulong)
                    as isize,
            );
            if yyss1 != yyssa.as_mut_ptr() {
                free(yyss1 as *mut libc::c_void);
            }
            yyssp = yyss
                .offset(yysize as isize)
                .offset(-(1 as libc::c_int as isize));
            yyvsp = yyvs
                .offset(yysize as isize)
                .offset(-(1 as libc::c_int as isize));
            if yyss
                .offset(yystacksize as isize)
                .offset(-(1 as libc::c_int as isize))
                <= yyssp
            {
                current_block = 15222471318721994018;
                break;
            }
        }
        yyn = yypact[yystate as usize] as libc::c_int;
        if yyn == -(200 as libc::c_int) {
            current_block = 13941258588466156190;
        } else {
            if yychar == -(2 as libc::c_int) {
                yychar = yylex();
            }
            if yychar <= 0 as libc::c_int {
                yytoken = 0 as libc::c_int;
                yychar = yytoken;
            } else {
                yytoken = if yychar as libc::c_uint <= 325 as libc::c_int as libc::c_uint {
                    yytranslate[yychar as usize] as libc::c_int
                } else {
                    2 as libc::c_int
                };
            }
            yyn += yytoken;
            if yyn < 0 as libc::c_int
                || (676 as libc::c_int) < yyn
                || yycheck[yyn as usize] as libc::c_int != yytoken
            {
                current_block = 13941258588466156190;
            } else {
                yyn = yytable[yyn as usize] as libc::c_int;
                if yyn <= 0 as libc::c_int {
                    if yyn == 0 as libc::c_int || yyn == -(1 as libc::c_int) {
                        current_block = 15101389463757124688;
                    } else {
                        yyn = -yyn;
                        current_block = 1658610937227270576;
                    }
                } else {
                    if yyn == 54 as libc::c_int {
                        current_block = 13192644061987812392;
                        break;
                    }
                    if yyerrstatus != 0 {
                        yyerrstatus -= 1;
                    }
                    if yychar != 0 as libc::c_int {
                        yychar = -(2 as libc::c_int);
                    }
                    yystate = yyn;
                    yyvsp = yyvsp.offset(1);
                    *yyvsp = yylval;
                    current_block = 16265518892899938795;
                }
            }
        }
        match current_block {
            13941258588466156190 => {
                yyn = yydefact[yystate as usize] as libc::c_int;
                if yyn == 0 as libc::c_int {
                    current_block = 15101389463757124688;
                } else {
                    current_block = 1658610937227270576;
                }
            }
            _ => {}
        }
        match current_block {
            15101389463757124688 => {
                if yyerrstatus == 0 {
                    yynerrs += 1;
                    yyerror(b"syntax error\0" as *const u8 as *const libc::c_char);
                }
                if yyerrstatus == 3 as libc::c_int {
                    if yychar <= 0 as libc::c_int {
                        if yychar == 0 as libc::c_int {
                            current_block = 15222471318721994018;
                            break;
                        }
                    } else {
                        yydestruct(
                            b"Error: discarding\0" as *const u8 as *const libc::c_char,
                            yytoken,
                            addr_of_mut!(yylval),
                        );
                        yychar = -(2 as libc::c_int);
                    }
                }
                yyerrstatus = 3 as libc::c_int;
                loop {
                    yyn = yypact[yystate as usize] as libc::c_int;
                    if yyn != -(200 as libc::c_int) {
                        yyn += 1 as libc::c_int;
                        if 0 as libc::c_int <= yyn
                            && yyn <= 676 as libc::c_int
                            && yycheck[yyn as usize] as libc::c_int == 1 as libc::c_int
                        {
                            yyn = yytable[yyn as usize] as libc::c_int;
                            if (0 as libc::c_int) < yyn {
                                break;
                            }
                        }
                    }
                    if yyssp == yyss {
                        current_block = 15222471318721994018;
                        break '_yysetstate;
                    }
                    yydestruct(
                        b"Error: popping\0" as *const u8 as *const libc::c_char,
                        yystos[yystate as usize] as libc::c_int,
                        yyvsp,
                    );
                    yyvsp = yyvsp.offset(-(1 as libc::c_int as isize));
                    yyssp = yyssp.offset(-(1 as libc::c_int as isize));
                    yystate = *yyssp as libc::c_int;
                }
                if yyn == 54 as libc::c_int {
                    current_block = 13192644061987812392;
                    break;
                }
                yyvsp = yyvsp.offset(1);
                *yyvsp = yylval;
                yystate = yyn;
            }
            1658610937227270576 => {
                yylen = yyr2[yyn as usize] as libc::c_int;
                yyval = *yyvsp.offset((1 as libc::c_int - yylen) as isize);
                match yyn {
                    2 => {
                        yyval.expr = ast_expr_identifier_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize)).string,
                        );
                    }
                    3 => {
                        yyval.expr = ast_expr_arbarg_create();
                    }
                    4 => {
                        yyval.expr = ast_expr_constant_create(parse_int(yytext));
                    }
                    5 => {
                        yyval.expr = ast_expr_constant_create_char(parse_char(yytext));
                    }
                    6 => {
                        yyval.expr = ast_expr_literal_create(strip_quotes(yytext));
                    }
                    7 => {
                        yyval.expr = ast_expr_bracketed_create(
                            (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                        );
                    }
                    8 => {
                        yyval.expr = ast_expr_alloc_create(
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                        );
                    }
                    9 => {
                        yyval.expr = ast_expr_dealloc_create(
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                        );
                    }
                    10 => {
                        yyval.expr = ast_expr_clump_create(
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                        );
                    }
                    12 => {
                        yyval.expr = ast_expr_unary_create(
                            ast_expr_binary_create(
                                (*yyvsp.offset((1 as libc::c_int - 4 as libc::c_int) as isize))
                                    .expr,
                                BINARY_OP_ADDITION,
                                (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                    .expr,
                            ),
                            UNARY_OP_DEREFERENCE,
                        );
                    }
                    13 => {
                        yyval.expr = ast_expr_call_create(
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                            0 as libc::c_int,
                            0 as *mut *mut AstExpr,
                        );
                    }
                    14 => {
                        yyval.expr = ast_expr_call_create(
                            (*yyvsp.offset((1 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .expr_array
                                .n,
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .expr_array
                                .expr,
                        );
                    }
                    15 => {
                        yyval.expr = ast_expr_member_create(
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize)).string,
                        );
                    }
                    16 => {
                        yyval.expr = ast_expr_member_create(
                            ast_expr_unary_create(
                                ast_expr_binary_create(
                                    (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                        .expr,
                                    BINARY_OP_ADDITION,
                                    ast_expr_constant_create(0 as libc::c_int),
                                ),
                                UNARY_OP_DEREFERENCE,
                            ),
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize)).string,
                        );
                    }
                    17 => {
                        yyval.expr = ast_expr_incdec_create(
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                            1 as libc::c_int != 0,
                            0 as libc::c_int != 0,
                        );
                    }
                    18 => {
                        yyval.expr = ast_expr_incdec_create(
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                            0 as libc::c_int != 0,
                            0 as libc::c_int != 0,
                        );
                    }
                    20 => {
                        yyval.expr_array = expr_array_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize)).expr,
                        );
                    }
                    21 => {
                        yyval.expr_array = expr_array_append(
                            &mut (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                .expr_array,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                        );
                    }
                    23 => {
                        yyval.expr = ast_expr_isdeallocand_create(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                        );
                    }
                    24 => {
                        yyval.expr = ast_expr_isdereferencable_create(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                        );
                    }
                    26 => {
                        yyval.expr = ast_expr_incdec_create(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                            1 as libc::c_int != 0,
                            1 as libc::c_int != 0,
                        );
                    }
                    27 => {
                        yyval.expr = ast_expr_incdec_create(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                            0 as libc::c_int != 0,
                            1 as libc::c_int != 0,
                        );
                    }
                    28 => {
                        yyval.expr = ast_expr_unary_create(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                            ast_unary_operator::from_bits_truncate(
                                (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                    .unary_operator as u32,
                            ),
                        );
                    }
                    29 => {
                        yyval.expr = ast_expr_constant_create(1 as libc::c_int);
                    }
                    30 => {
                        yyval.unary_operator = UNARY_OP_ADDRESS.bits() as i32;
                    }
                    31 => {
                        yyval.unary_operator = UNARY_OP_DEREFERENCE.bits() as i32;
                    }
                    32 => {
                        yyval.unary_operator = UNARY_OP_POSITIVE.bits() as i32;
                    }
                    33 => {
                        yyval.unary_operator = UNARY_OP_NEGATIVE.bits() as i32;
                    }
                    34 => {
                        yyval.unary_operator = UNARY_OP_ONES_COMPLEMENT.bits() as i32;
                    }
                    35 => {
                        yyval.unary_operator = UNARY_OP_BANG.bits() as i32;
                    }
                    41 => {
                        yyval.expr = ast_expr_binary_create(
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                            BINARY_OP_ADDITION,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                        );
                    }
                    42 => {
                        yyval.expr = ast_expr_binary_create(
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                            BINARY_OP_SUBTRACTION,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                        );
                    }
                    46 => {
                        yyval.binary_operator = BINARY_OP_LT.bits() as i32;
                    }
                    47 => {
                        yyval.binary_operator = BINARY_OP_GT.bits() as i32;
                    }
                    48 => {
                        yyval.binary_operator = BINARY_OP_LE.bits() as i32;
                    }
                    49 => {
                        yyval.binary_operator = BINARY_OP_GE.bits() as i32;
                    }
                    53 => {
                        yyval.expr = ast_expr_binary_create(
                            (*yyvsp.offset((1 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                            ast_binary_operator::from_bits_truncate(
                                (*yyvsp.offset((2 as libc::c_int - 4 as libc::c_int) as isize))
                                    .binary_operator as u32,
                            ),
                            (*yyvsp.offset((4 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                        );
                    }
                    54 => {
                        yyval.binary_operator = BINARY_OP_EQ.bits() as i32;
                    }
                    55 => {
                        yyval.binary_operator = BINARY_OP_NE.bits() as i32;
                    }
                    57 => {
                        yyval.expr = ast_expr_binary_create(
                            (*yyvsp.offset((1 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                            ast_binary_operator::from_bits_truncate(
                                (*yyvsp.offset((2 as libc::c_int - 4 as libc::c_int) as isize))
                                    .binary_operator as u32,
                            ),
                            (*yyvsp.offset((4 as libc::c_int - 4 as libc::c_int) as isize)).expr,
                        );
                    }
                    67 => {
                        yyval.expr = ast_expr_assignment_create(
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                        );
                    }
                    81 => {
                        let mut name: *mut libc::c_char = ast_type_struct_tag(
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize)).type_0,
                        );
                        if name.is_null() as libc::c_int as libc::c_long != 0 {
                            __assert_rtn(
                                (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                    b"yyparse\0",
                                ))
                                .as_ptr(),
                                b"gram.y\0" as *const u8 as *const libc::c_char,
                                404 as libc::c_int,
                                b"name\0" as *const u8 as *const libc::c_char,
                            );
                        } else {
                        };
                        yyval.declaration = {
                            let mut init = declaration {
                                name: name,
                                t: (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                    .type_0,
                            };
                            init
                        };
                    }
                    82 => {
                        let mut i: libc::c_int = 0 as libc::c_int;
                        while i
                            < (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize))
                                .declarator
                                .ptr_valence
                        {
                            if ((*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                .type_0)
                                .is_null() as libc::c_int
                                as libc::c_long
                                != 0
                            {
                                __assert_rtn(
                                    (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                        b"yyparse\0",
                                    ))
                                    .as_ptr(),
                                    b"gram.y\0" as *const u8 as *const libc::c_char,
                                    412 as libc::c_int,
                                    b"(yyvsp[(1) - (3)].type)\0" as *const u8
                                        as *const libc::c_char,
                                );
                            } else {
                            };
                            let ref mut fresh4 = (*yyvsp
                                .offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                            .type_0;
                            *fresh4 = ast_type_create_ptr(
                                (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                    .type_0,
                            );
                            i += 1;
                        }
                        yyval.declaration = {
                            let mut init = declaration {
                                name: (*yyvsp
                                    .offset((2 as libc::c_int - 3 as libc::c_int) as isize))
                                .declarator
                                .name,
                                t: (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                    .type_0,
                            };
                            init
                        };
                    }
                    86 => {
                        yyval.type_0 =
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).type_0;
                    }
                    87 => {
                        ast_type_mod_or(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).type_0,
                            ast_type_modifier::from_bits_truncate(
                                (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                    .type_modifier as u32,
                            ),
                        );
                        yyval.type_0 =
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).type_0;
                    }
                    91 => {
                        yyval.type_modifier = MOD_TYPEDEF.bits() as i32;
                        installclass = IC_TYPE;
                    }
                    92 => {
                        yyval.type_modifier = MOD_EXTERN.bits() as i32;
                    }
                    93 => {
                        yyval.type_modifier = MOD_STATIC.bits() as i32;
                    }
                    94 => {
                        yyval.type_modifier = MOD_AUTO.bits() as i32;
                    }
                    95 => {
                        yyval.type_modifier = MOD_REGISTER.bits() as i32;
                    }
                    96 => {
                        yyval.type_0 = ast_type_create(TYPE_VOID, ast_type_modifier::empty());
                    }
                    97 => {
                        yyval.type_0 = ast_type_create(TYPE_CHAR, ast_type_modifier::empty());
                    }
                    98 => {
                        yyval.type_0 = ast_type_create(TYPE_INT, ast_type_modifier::empty());
                    }
                    100 => {
                        yyval.type_0 = ast_type_create_userdef(dynamic_str(yytext));
                    }
                    101 => {
                        if ((*yyvsp.offset((1 as libc::c_int - 5 as libc::c_int) as isize)).boolean
                            == 0) as libc::c_int as libc::c_long
                            != 0
                        {
                            __assert_rtn(
                                (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                    b"yyparse\0",
                                ))
                                .as_ptr(),
                                b"gram.y\0" as *const u8 as *const libc::c_char,
                                479 as libc::c_int,
                                b"(yyvsp[(1) - (5)].boolean)\0" as *const u8 as *const libc::c_char,
                            );
                        } else {
                        };
                        yyval.type_0 = ast_type_create_struct(
                            (*yyvsp.offset((2 as libc::c_int - 5 as libc::c_int) as isize)).string,
                            (*yyvsp.offset((4 as libc::c_int - 5 as libc::c_int) as isize))
                                .variable_array,
                        );
                    }
                    102 => {
                        if ((*yyvsp.offset((1 as libc::c_int - 4 as libc::c_int) as isize)).boolean
                            == 0) as libc::c_int as libc::c_long
                            != 0
                        {
                            __assert_rtn(
                                (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                    b"yyparse\0",
                                ))
                                .as_ptr(),
                                b"gram.y\0" as *const u8 as *const libc::c_char,
                                481 as libc::c_int,
                                b"(yyvsp[(1) - (4)].boolean)\0" as *const u8 as *const libc::c_char,
                            );
                        } else {
                        };
                        yyval.type_0 = ast_type_create_struct_anonym(
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .variable_array,
                        );
                    }
                    103 => {
                        if ((*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize)).boolean
                            == 0) as libc::c_int as libc::c_long
                            != 0
                        {
                            __assert_rtn(
                                (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                    b"yyparse\0",
                                ))
                                .as_ptr(),
                                b"gram.y\0" as *const u8 as *const libc::c_char,
                                483 as libc::c_int,
                                b"(yyvsp[(1) - (2)].boolean)\0" as *const u8 as *const libc::c_char,
                            );
                        } else {
                        };
                        yyval.type_0 = ast_type_create_struct_partial(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).string,
                        );
                    }
                    104 => {
                        yyval.boolean = 1 as libc::c_int;
                    }
                    105 => {
                        yyval.boolean = 0 as libc::c_int;
                    }
                    106 => {
                        yyval.variable_array = variable_array_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .variable,
                        );
                    }
                    107 => {
                        yyval.variable_array = variable_array_append(
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .variable_array,
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .variable,
                        );
                    }
                    108 => {
                        yyval.variable = ast_variable_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .declaration
                                .name,
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .declaration
                                .t,
                        );
                    }
                    113 => {
                        yyval.type_modifier = MOD_CONST.bits() as i32;
                    }
                    114 => {
                        yyval.type_modifier = MOD_VOLATILE.bits() as i32;
                    }
                    115 => {
                        yyval.function_declarator = {
                            let mut init = function_declarator {
                                ptr_valence: (*yyvsp
                                    .offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .integer,
                                decl: (*yyvsp
                                    .offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .direct_function_declarator,
                            };
                            init
                        };
                    }
                    116 => {
                        yyval.function_declarator = {
                            let mut init = function_declarator {
                                ptr_valence: 0 as libc::c_int,
                                decl: (*yyvsp
                                    .offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .direct_function_declarator,
                            };
                            init
                        };
                    }
                    117 => {
                        yyval.string = dynamic_str(yytext);
                    }
                    118 => {
                        yyval.direct_function_declarator = {
                            let mut init = direct_function_declarator {
                                name: (*yyvsp
                                    .offset((1 as libc::c_int - 4 as libc::c_int) as isize))
                                .string,
                                n: ast_variable_arr_n(
                                    (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                        .variable_array,
                                ),
                                param: ast_variable_arr_v(
                                    (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                        .variable_array,
                                ),
                            };
                            init
                        };
                    }
                    119 => {
                        yyval.direct_function_declarator = {
                            let mut init = direct_function_declarator {
                                name: (*yyvsp
                                    .offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                .string,
                                n: 0 as libc::c_int,
                                param: 0 as *mut *mut ast_variable,
                            };
                            init
                        };
                    }
                    120 => {
                        yyval.declarator = {
                            let mut init = declarator {
                                ptr_valence: (*yyvsp
                                    .offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .integer,
                                name: (*yyvsp
                                    .offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .string,
                            };
                            init
                        };
                    }
                    121 => {
                        yyval.declarator = {
                            let mut init = declarator {
                                ptr_valence: 0 as libc::c_int,
                                name: (*yyvsp
                                    .offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .string,
                            };
                            init
                        };
                    }
                    122 => {
                        yyval.declarator = {
                            let mut init = declarator {
                                ptr_valence: (*yyvsp
                                    .offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .integer,
                                name: 0 as *mut libc::c_char,
                            };
                            init
                        };
                    }
                    126 => {
                        yyval.integer = 1 as libc::c_int;
                    }
                    127 => {
                        yyval.integer =
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).integer
                                + 1 as libc::c_int;
                    }
                    129 => {
                        yyval.variable_array = variable_array_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .variable,
                        );
                    }
                    130 => {
                        yyval.variable_array = variable_array_append(
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                .variable_array,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize))
                                .variable,
                        );
                    }
                    131 => {
                        let mut i_0: libc::c_int = 0 as libc::c_int;
                        while i_0
                            < (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .declarator
                                .ptr_valence
                        {
                            if ((*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .type_0)
                                .is_null() as libc::c_int
                                as libc::c_long
                                != 0
                            {
                                __assert_rtn(
                                    (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                        b"yyparse\0",
                                    ))
                                    .as_ptr(),
                                    b"gram.y\0" as *const u8 as *const libc::c_char,
                                    622 as libc::c_int,
                                    b"(yyvsp[(1) - (2)].type)\0" as *const u8
                                        as *const libc::c_char,
                                );
                            } else {
                            };
                            let ref mut fresh5 = (*yyvsp
                                .offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                            .type_0;
                            *fresh5 = ast_type_create_ptr(
                                (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                    .type_0,
                            );
                            i_0 += 1;
                        }
                        yyval.variable = ast_variable_create(
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .declarator
                                .name,
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize)).type_0,
                        );
                    }
                    132 => {
                        yyval.variable = ast_variable_create(
                            dynamic_str(b"\0" as *const u8 as *const libc::c_char),
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize)).type_0,
                        );
                    }
                    137 => {
                        yyval.statement = ast_stmt_create_compound(
                            lexloc(),
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize)).block,
                        );
                    }
                    142 => {
                        yyval.statement = ast_stmt_create_iter_e(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .statement,
                        );
                    }
                    143 => {
                        yyval.statement = ast_stmt_create_compound_v(
                            lexloc(),
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize)).block,
                        );
                    }
                    144 => {
                        yyval.statement = ast_stmt_create_labelled(
                            lexloc(),
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).string,
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize))
                                .statement,
                        );
                    }
                    145 => {
                        yyval.statement = ast_stmt_create_nop(lexloc());
                    }
                    146 => {
                        yyval.statement = ast_stmt_create_nop(lexloc());
                    }
                    147 => {
                        yyval.block =
                            (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize)).block;
                    }
                    148 => {
                        yyval.block = ast_block_create(
                            0 as *mut *mut ast_variable,
                            0 as libc::c_int,
                            0 as *mut *mut ast_stmt,
                            0 as libc::c_int,
                        );
                    }
                    149 => {
                        yyval.block = ast_block_create(
                            0 as *mut *mut ast_variable,
                            0 as libc::c_int,
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .stmt_array
                                .stmt,
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .stmt_array
                                .n,
                        );
                    }
                    150 => {
                        yyval.block = ast_block_create(
                            ast_variable_arr_v(
                                (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                    .variable_array,
                            ),
                            ast_variable_arr_n(
                                (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                    .variable_array,
                            ),
                            0 as *mut *mut ast_stmt,
                            0 as libc::c_int,
                        );
                    }
                    151 => {
                        yyval.block = ast_block_create(
                            ast_variable_arr_v(
                                (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                    .variable_array,
                            ),
                            ast_variable_arr_n(
                                (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                    .variable_array,
                            ),
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .stmt_array
                                .stmt,
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .stmt_array
                                .n,
                        );
                    }
                    152 => {
                        yyval.statement = (*yyvsp
                            .offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                        .statement;
                    }
                    153 => {
                        yyval.block = ast_block_create(
                            0 as *mut *mut ast_variable,
                            0 as libc::c_int,
                            0 as *mut *mut ast_stmt,
                            0 as libc::c_int,
                        );
                    }
                    154 => {
                        yyval.block =
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize)).block;
                    }
                    155 => {
                        yyval.variable_array = variable_array_create(ast_variable_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .declaration
                                .name,
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .declaration
                                .t,
                        ));
                    }
                    156 => {
                        yyval.variable_array = variable_array_append(
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .variable_array,
                            ast_variable_create(
                                (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                    .declaration
                                    .name,
                                (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                    .declaration
                                    .t,
                            ),
                        );
                    }
                    157 => {
                        yyval.stmt_array = stmt_array_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .statement,
                        );
                    }
                    158 => {
                        yyval.stmt_array = stmt_array_append(
                            &mut (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .stmt_array,
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .statement,
                        );
                    }
                    159 => {
                        yyval.statement = ast_stmt_create_nop(lexloc());
                    }
                    160 => {
                        yyval.statement = ast_stmt_create_expr(
                            lexloc(),
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize)).expr,
                        );
                    }
                    161 => {
                        yyval.statement = ast_stmt_create_sel(
                            lexloc(),
                            0 as libc::c_int != 0,
                            (*yyvsp.offset((3 as libc::c_int - 5 as libc::c_int) as isize)).expr,
                            (*yyvsp.offset((5 as libc::c_int - 5 as libc::c_int) as isize))
                                .statement,
                            0 as *mut ast_stmt,
                        );
                    }
                    162 => {
                        let mut neg_cond: *mut AstExpr = ast_expr_unary_create(
                            ast_expr_copy(
                                (*yyvsp.offset((3 as libc::c_int - 7 as libc::c_int) as isize))
                                    .expr,
                            ),
                            UNARY_OP_BANG,
                        );
                        let mut _else: *mut ast_stmt = ast_stmt_create_sel(
                            lexloc(),
                            0 as libc::c_int != 0,
                            neg_cond,
                            (*yyvsp.offset((7 as libc::c_int - 7 as libc::c_int) as isize))
                                .statement,
                            0 as *mut ast_stmt,
                        );
                        yyval.statement = ast_stmt_create_sel(
                            lexloc(),
                            0 as libc::c_int != 0,
                            (*yyvsp.offset((3 as libc::c_int - 7 as libc::c_int) as isize)).expr,
                            (*yyvsp.offset((5 as libc::c_int - 7 as libc::c_int) as isize))
                                .statement,
                            _else,
                        );
                    }
                    163 => {
                        yyval.statement = ast_stmt_create_nop(lexloc());
                    }
                    166 => {
                        yyval.block = ast_block_create(
                            0 as *mut *mut ast_variable,
                            0 as libc::c_int,
                            0 as *mut *mut ast_stmt,
                            0 as libc::c_int,
                        );
                    }
                    168 => {
                        yyval.statement = ast_stmt_create_iter(
                            lexloc(),
                            (*yyvsp.offset((3 as libc::c_int - 8 as libc::c_int) as isize))
                                .statement,
                            (*yyvsp.offset((4 as libc::c_int - 8 as libc::c_int) as isize))
                                .statement,
                            (*yyvsp.offset((5 as libc::c_int - 8 as libc::c_int) as isize)).expr,
                            (*yyvsp.offset((7 as libc::c_int - 8 as libc::c_int) as isize)).block,
                            (*yyvsp.offset((8 as libc::c_int - 8 as libc::c_int) as isize))
                                .statement,
                        );
                    }
                    170 => {
                        yyval.statement = ast_stmt_create_jump(
                            lexloc(),
                            JUMP_RETURN,
                            (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize)).expr,
                        );
                    }
                    171 => {
                        root = ast_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .externdecl,
                        );
                    }
                    172 => {
                        root = ast_append(
                            root,
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .externdecl,
                        );
                    }
                    173 => {
                        yyval.externdecl = ast_functiondecl_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .function,
                        );
                    }
                    174 => {
                        yyval.externdecl = ast_decl_create(
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .declaration
                                .name,
                            (*yyvsp.offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .declaration
                                .t,
                        );
                    }
                    175 => {
                        yyval.block_statement = {
                            let mut init = block_statement {
                                abstract_0: 0 as *mut ast_block,
                                body: 0 as *mut ast_block,
                            };
                            init
                        };
                    }
                    176 => {
                        yyval.block_statement = {
                            let mut init = block_statement {
                                abstract_0: 0 as *mut ast_block,
                                body: (*yyvsp
                                    .offset((1 as libc::c_int - 1 as libc::c_int) as isize))
                                .block,
                            };
                            init
                        };
                    }
                    177 => {
                        yyval.block_statement = {
                            let mut init = block_statement {
                                abstract_0: (*yyvsp
                                    .offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .block,
                                body: 0 as *mut ast_block,
                            };
                            init
                        };
                    }
                    178 => {
                        if ((*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize)).block)
                            .is_null() as libc::c_int as libc::c_long
                            != 0
                        {
                            __assert_rtn(
                                (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                    b"yyparse\0",
                                ))
                                .as_ptr(),
                                b"gram.y\0" as *const u8 as *const libc::c_char,
                                832 as libc::c_int,
                                b"(yyvsp[(2) - (2)].block)\0" as *const u8 as *const libc::c_char,
                            );
                        } else {
                        };
                        yyval.block_statement = {
                            let mut init = block_statement {
                                abstract_0: (*yyvsp
                                    .offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .block,
                                body: (*yyvsp
                                    .offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .block,
                            };
                            init
                        };
                    }
                    179 => {
                        let mut i_1: libc::c_int = 0 as libc::c_int;
                        while i_1
                            < (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .function_declarator
                                .ptr_valence
                        {
                            if ((*yyvsp.offset((2 as libc::c_int - 4 as libc::c_int) as isize))
                                .type_0)
                                .is_null() as libc::c_int
                                as libc::c_long
                                != 0
                            {
                                __assert_rtn(
                                    (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                        b"yyparse\0",
                                    ))
                                    .as_ptr(),
                                    b"gram.y\0" as *const u8 as *const libc::c_char,
                                    843 as libc::c_int,
                                    b"(yyvsp[(2) - (4)].type)\0" as *const u8
                                        as *const libc::c_char,
                                );
                            } else {
                            };
                            let ref mut fresh6 = (*yyvsp
                                .offset((2 as libc::c_int - 4 as libc::c_int) as isize))
                            .type_0;
                            *fresh6 = ast_type_create_ptr(
                                (*yyvsp.offset((2 as libc::c_int - 4 as libc::c_int) as isize))
                                    .type_0,
                            );
                            i_1 += 1;
                        }
                        yyval.function = ast_function_create(
                            1 as libc::c_int != 0,
                            (*yyvsp.offset((2 as libc::c_int - 4 as libc::c_int) as isize)).type_0,
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .name,
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .n,
                            (*yyvsp.offset((3 as libc::c_int - 4 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .param,
                            if !((*yyvsp.offset((4 as libc::c_int - 4 as libc::c_int) as isize))
                                .block_statement
                                .abstract_0)
                                .is_null()
                            {
                                (*yyvsp.offset((4 as libc::c_int - 4 as libc::c_int) as isize))
                                    .block_statement
                                    .abstract_0
                            } else {
                                ast_block_create(
                                    0 as *mut *mut ast_variable,
                                    0 as libc::c_int,
                                    0 as *mut *mut ast_stmt,
                                    0 as libc::c_int,
                                )
                            },
                            (*yyvsp.offset((4 as libc::c_int - 4 as libc::c_int) as isize))
                                .block_statement
                                .body,
                        );
                    }
                    180 => {
                        let mut i_2: libc::c_int = 0 as libc::c_int;
                        while i_2
                            < (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize))
                                .function_declarator
                                .ptr_valence
                        {
                            if ((*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                .type_0)
                                .is_null() as libc::c_int
                                as libc::c_long
                                != 0
                            {
                                __assert_rtn(
                                    (*::core::mem::transmute::<&[u8; 8], &[libc::c_char; 8]>(
                                        b"yyparse\0",
                                    ))
                                    .as_ptr(),
                                    b"gram.y\0" as *const u8 as *const libc::c_char,
                                    858 as libc::c_int,
                                    b"(yyvsp[(1) - (3)].type)\0" as *const u8
                                        as *const libc::c_char,
                                );
                            } else {
                            };
                            let ref mut fresh7 = (*yyvsp
                                .offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                            .type_0;
                            *fresh7 = ast_type_create_ptr(
                                (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize))
                                    .type_0,
                            );
                            i_2 += 1;
                        }
                        yyval.function = ast_function_create(
                            0 as libc::c_int != 0,
                            (*yyvsp.offset((1 as libc::c_int - 3 as libc::c_int) as isize)).type_0,
                            (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .name,
                            (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .n,
                            (*yyvsp.offset((2 as libc::c_int - 3 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .param,
                            if !((*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize))
                                .block_statement
                                .abstract_0)
                                .is_null()
                            {
                                (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize))
                                    .block_statement
                                    .abstract_0
                            } else {
                                ast_block_create(
                                    0 as *mut *mut ast_variable,
                                    0 as libc::c_int,
                                    0 as *mut *mut ast_stmt,
                                    0 as libc::c_int,
                                )
                            },
                            (*yyvsp.offset((3 as libc::c_int - 3 as libc::c_int) as isize))
                                .block_statement
                                .body,
                        );
                    }
                    181 => {
                        yyval.function = ast_function_create(
                            0 as libc::c_int != 0,
                            ast_type_create(TYPE_VOID, ast_type_modifier::empty()),
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .name,
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .n,
                            (*yyvsp.offset((1 as libc::c_int - 2 as libc::c_int) as isize))
                                .function_declarator
                                .decl
                                .param,
                            if !((*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .block_statement
                                .abstract_0)
                                .is_null()
                            {
                                (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                    .block_statement
                                    .abstract_0
                            } else {
                                ast_block_create(
                                    0 as *mut *mut ast_variable,
                                    0 as libc::c_int,
                                    0 as *mut *mut ast_stmt,
                                    0 as libc::c_int,
                                )
                            },
                            (*yyvsp.offset((2 as libc::c_int - 2 as libc::c_int) as isize))
                                .block_statement
                                .body,
                        );
                    }
                    _ => {}
                }
                yyvsp = yyvsp.offset(-(yylen as isize));
                yyssp = yyssp.offset(-(yylen as isize));
                yylen = 0 as libc::c_int;
                yyvsp = yyvsp.offset(1);
                *yyvsp = yyval;
                yyn = yyr1[yyn as usize] as libc::c_int;
                yystate = yypgoto[(yyn - 91 as libc::c_int) as usize] as libc::c_int
                    + *yyssp as libc::c_int;
                if 0 as libc::c_int <= yystate
                    && yystate <= 676 as libc::c_int
                    && yycheck[yystate as usize] as libc::c_int == *yyssp as libc::c_int
                {
                    yystate = yytable[yystate as usize] as libc::c_int;
                } else {
                    yystate = yydefgoto[(yyn - 91 as libc::c_int) as usize] as libc::c_int;
                }
            }
            _ => {}
        }
        yyssp = yyssp.offset(1);
    }
    match current_block {
        15222471318721994018 => {
            yyresult = 1 as libc::c_int;
        }
        13192644061987812392 => {
            yyresult = 0 as libc::c_int;
        }
        _ => {
            yyerror(b"memory exhausted\0" as *const u8 as *const libc::c_char);
            yyresult = 2 as libc::c_int;
        }
    }
    if yychar != 0 as libc::c_int && yychar != -(2 as libc::c_int) {
        yydestruct(
            b"Cleanup: discarding lookahead\0" as *const u8 as *const libc::c_char,
            yytoken,
            addr_of_mut!(yylval),
        );
    }
    yyvsp = yyvsp.offset(-(yylen as isize));
    yyssp = yyssp.offset(-(yylen as isize));
    while yyssp != yyss {
        yydestruct(
            b"Cleanup: popping\0" as *const u8 as *const libc::c_char,
            yystos[*yyssp as usize] as libc::c_int,
            yyvsp,
        );
        yyvsp = yyvsp.offset(-(1 as libc::c_int as isize));
        yyssp = yyssp.offset(-(1 as libc::c_int as isize));
    }
    if yyss != yyssa.as_mut_ptr() {
        free(yyss as *mut libc::c_void);
    }
    return yyresult;
}
