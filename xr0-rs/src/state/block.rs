#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{calloc, free, malloc, realloc};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_difference_create,
    ast_expr_eq_create, ast_expr_sum_create,
};
use crate::c_util::__assert_rtn;
use crate::object::{
    object_abstractcopy, object_arr_append, object_arr_copy, object_arr_create, object_arr_destroy,
    object_arr_index, object_arr_index_upperincl, object_arr_insert, object_arr_nobjects,
    object_arr_objects, object_arr_remove, object_contig_precedes, object_dealloc, object_from,
    object_isdeallocand, object_isvalue, object_lower, object_range_create, object_references,
    object_referencesheap, object_str, object_upper, object_upto, object_value_create,
    range_create,
};
use crate::state::heap::heap_newblock;
use crate::state::{state_alloc, state_eval};
use crate::util::{error, error_create, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{
    object_arr, AstExpr as ast_expr, Heap as heap, Location as location, Object as object,
    State as state, StrBuilder as strbuilder, Value as value,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct block {
    pub arr: *mut object_arr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct block_arr {
    pub n: libc::c_int,
    pub block: *mut *mut block,
}
#[no_mangle]
pub unsafe fn block_create() -> *mut block {
    let mut b: *mut block = malloc(::core::mem::size_of::<block>()) as *mut block;
    (*b).arr = object_arr_create();
    return b;
}
#[no_mangle]
pub unsafe fn block_destroy(mut b: *mut block) {
    object_arr_destroy((*b).arr);
    free(b as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn block_copy(mut old: *mut block) -> *mut block {
    let mut new: *mut block = malloc(::core::mem::size_of::<block>()) as *mut block;
    (*new).arr = object_arr_copy((*old).arr);
    return new;
}
#[no_mangle]
pub unsafe fn block_str(mut block: *mut block) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut obj: *mut *mut object = object_arr_objects((*block).arr);
    let mut n: libc::c_int = object_arr_nobjects((*block).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut s: *mut libc::c_char = object_str(*obj.offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%s\0" as *const u8 as *const libc::c_char,
            s,
            if (i + 1 as libc::c_int) < n {
                b", \0" as *const u8 as *const libc::c_char
            } else {
                b"\0" as *const u8 as *const libc::c_char
            },
        );
        free(s as *mut libc::c_void);
        i += 1;
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe fn block_install(mut b: *mut block, mut obj: *mut object) {
    if !(object_arr_nobjects((*b).arr) == 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"block_install\0"))
                .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            59 as libc::c_int,
            b"object_arr_nobjects(b->arr) == 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    object_arr_append((*b).arr, obj);
}
#[no_mangle]
pub unsafe fn block_observe(
    mut b: *mut block,
    mut offset: *mut ast_expr,
    mut s: *mut state,
    mut constructive: bool,
) -> *mut object {
    let mut index: libc::c_int = object_arr_index((*b).arr, offset, s);
    if index == -(1 as libc::c_int) {
        if !constructive {
            return 0 as *mut object;
        }
        let mut obj: *mut object = object_value_create(ast_expr_copy(offset), 0 as *mut value);
        object_arr_append((*b).arr, obj);
        return obj;
    }
    let mut obj_0: *mut object = *(object_arr_objects((*b).arr)).offset(index as isize);
    if object_isvalue(obj_0) {
        return obj_0;
    }
    let mut lw: *mut ast_expr = ast_expr_copy(offset);
    let mut up: *mut ast_expr = ast_expr_sum_create(
        ast_expr_copy(offset),
        ast_expr_constant_create(1 as libc::c_int),
    );
    let mut upto: *mut object = object_upto(obj_0, lw, s);
    let mut observed: *mut object = object_value_create(ast_expr_copy(lw), state_alloc(s));
    let mut from: *mut object = object_from(obj_0, up, s);
    ast_expr_destroy(up);
    ast_expr_destroy(lw);
    let mut err: *mut error = object_dealloc(obj_0, s);
    if !err.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"block_observe\0"))
                .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            104 as libc::c_int,
            b"!err\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    object_arr_remove((*b).arr, index);
    if !upto.is_null() {
        let fresh0 = index;
        index = index + 1;
        object_arr_insert((*b).arr, fresh0, upto);
    }
    let fresh1 = index;
    index = index + 1;
    object_arr_insert((*b).arr, fresh1, observed);
    if !from.is_null() {
        object_arr_insert((*b).arr, index, from);
    }
    return observed;
}
#[no_mangle]
pub unsafe fn block_references(
    mut b: *mut block,
    mut loc: *mut location,
    mut s: *mut state,
) -> bool {
    let mut n: libc::c_int = object_arr_nobjects((*b).arr);
    let mut obj: *mut *mut object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if object_references(*obj.offset(i as isize), loc, s) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}
#[no_mangle]
pub unsafe fn block_range_alloc(
    mut b: *mut block,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut heap: *mut heap,
) -> *mut error {
    if !(object_arr_nobjects((*b).arr) == 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"block_range_alloc\0"))
                .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            136 as libc::c_int,
            b"object_arr_nobjects(b->arr) == 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    object_arr_append(
        (*b).arr,
        object_range_create(
            ast_expr_copy(lw),
            range_create(
                ast_expr_difference_create(ast_expr_copy(up), ast_expr_copy(lw)),
                heap_newblock(heap),
            ),
        ),
    );
    return 0 as *mut error;
}
#[no_mangle]
pub unsafe fn block_range_aredeallocands(
    mut b: *mut block,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut s: *mut state,
) -> bool {
    if hack_first_object_is_exactly_bounds(b, lw, up, s) {
        return 1 as libc::c_int != 0;
    }
    let mut lw_index: libc::c_int = object_arr_index((*b).arr, lw, s);
    if lw_index == -(1 as libc::c_int) {
        return 0 as libc::c_int != 0;
    }
    let mut up_index: libc::c_int = object_arr_index_upperincl((*b).arr, up, s);
    if up_index == -(1 as libc::c_int) {
        return 0 as libc::c_int != 0;
    }
    let mut obj: *mut *mut object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = lw_index;
    while i < up_index {
        if !object_isdeallocand(*obj.offset(i as isize), s) {
            return 0 as libc::c_int != 0;
        }
        if !object_contig_precedes(
            *obj.offset(i as isize),
            *obj.offset((i + 1 as libc::c_int) as isize),
            s,
        ) {
            return 0 as libc::c_int != 0;
        }
        i += 1;
    }
    if !object_isdeallocand(*obj.offset(up_index as isize), s) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                b"block_range_aredeallocands\0",
            ))
            .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            187 as libc::c_int,
            b"object_isdeallocand(obj[up_index], s)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return 1 as libc::c_int != 0;
}
unsafe fn hack_first_object_is_exactly_bounds(
    mut b: *mut block,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut s: *mut state,
) -> bool {
    if object_arr_nobjects((*b).arr) == 0 as libc::c_int {
        return 0 as libc::c_int != 0;
    }
    let mut obj: *mut object = *(object_arr_objects((*b).arr)).offset(0 as libc::c_int as isize);
    if !object_isdeallocand(obj, s) {
        return 0 as libc::c_int != 0;
    }
    let mut same_lw: *mut ast_expr = ast_expr_eq_create(lw, object_lower(obj));
    let mut same_up: *mut ast_expr = ast_expr_eq_create(up, object_upper(obj));
    return state_eval(s, same_lw) as libc::c_int != 0
        && state_eval(s, same_up) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe fn block_range_dealloc(
    mut b: *mut block,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut s: *mut state,
) -> *mut error {
    if hack_first_object_is_exactly_bounds(b, lw, up, s) {
        let mut err: *mut error = object_dealloc(
            *(object_arr_objects((*b).arr)).offset(0 as libc::c_int as isize),
            s,
        );
        if !err.is_null() {
            return err;
        }
        object_arr_remove((*b).arr, 0 as libc::c_int);
        return 0 as *mut error;
    }
    let mut lw_index: libc::c_int = object_arr_index((*b).arr, lw, s);
    if lw_index == -(1 as libc::c_int) {
        return error_create(
            b"lower bound not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut up_index: libc::c_int = object_arr_index_upperincl((*b).arr, up, s);
    if up_index == -(1 as libc::c_int) {
        return error_create(
            b"upper bound not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut n: libc::c_int = object_arr_nobjects((*b).arr);
    let mut obj: *mut *mut object = object_arr_objects((*b).arr);
    let mut upto: *mut object = object_upto(*obj.offset(lw_index as isize), lw, s);
    let mut from: *mut object = object_from(*obj.offset(up_index as isize), up, s);
    let mut new: *mut object_arr = object_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < lw_index {
        object_arr_append(new, *obj.offset(i as isize));
        i += 1;
    }
    if !upto.is_null() {
        object_arr_append((*b).arr, upto);
    }
    if !from.is_null() {
        object_arr_append((*b).arr, from);
    }
    let mut i_0: libc::c_int = up_index + 1 as libc::c_int;
    while i_0 < n {
        object_arr_append(new, *obj.offset(i_0 as isize));
        i_0 += 1;
    }
    let mut i_1: libc::c_int = lw_index;
    while i_1 <= up_index {
        let mut err_0: *mut error = object_dealloc(*obj.offset(i_1 as isize), s);
        if !err_0.is_null() {
            return err_0;
        }
        i_1 += 1;
    }
    (*b).arr = new;
    return 0 as *mut error;
}
#[no_mangle]
pub unsafe fn block_undeclare(mut b: *mut block, mut s: *mut state) {
    let mut new: *mut object_arr = object_arr_create();
    let mut n: libc::c_int = object_arr_nobjects((*b).arr);
    let mut object: *mut *mut object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut obj: *mut object = *object.offset(i as isize);
        if object_referencesheap(obj, s) {
            object_arr_append(new, object_abstractcopy(obj, s));
        }
        i += 1;
    }
    object_arr_destroy((*b).arr);
    (*b).arr = new;
}
#[no_mangle]
pub unsafe fn block_arr_create() -> *mut block_arr {
    let mut arr: *mut block_arr = calloc(1, ::core::mem::size_of::<block_arr>()) as *mut block_arr;
    if arr.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 17], &[libc::c_char; 17]>(b"block_arr_create\0"))
                .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            295 as libc::c_int,
            b"arr\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return arr;
}
#[no_mangle]
pub unsafe fn block_arr_destroy(mut arr: *mut block_arr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        block_destroy(*((*arr).block).offset(i as isize));
        i += 1;
    }
    free((*arr).block as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn block_arr_copy(mut old: *mut block_arr) -> *mut block_arr {
    let mut new: *mut block_arr = block_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        block_arr_append(new, block_copy(*((*old).block).offset(i as isize)));
        i += 1;
    }
    return new;
}
#[no_mangle]
pub unsafe fn block_arr_blocks(mut arr: *mut block_arr) -> *mut *mut block {
    return (*arr).block;
}
#[no_mangle]
pub unsafe fn block_arr_nblocks(mut arr: *mut block_arr) -> libc::c_int {
    return (*arr).n;
}
#[no_mangle]
pub unsafe fn block_arr_append(mut arr: *mut block_arr, mut b: *mut block) -> libc::c_int {
    (*arr).n += 1;
    (*arr).block = realloc(
        (*arr).block as *mut libc::c_void,
        (::core::mem::size_of::<block_arr>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut block;
    if ((*arr).block).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 17], &[libc::c_char; 17]>(b"block_arr_append\0"))
                .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            336 as libc::c_int,
            b"arr->block\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut loc: libc::c_int = (*arr).n - 1 as libc::c_int;
    let ref mut fresh2 = *((*arr).block).offset(loc as isize);
    *fresh2 = b;
    return loc;
}
#[no_mangle]
pub unsafe fn block_arr_delete(mut arr: *mut block_arr, mut address: libc::c_int) {
    if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 17], &[libc::c_char; 17]>(b"block_arr_delete\0"))
                .as_ptr(),
            b"block.c\0" as *const u8 as *const libc::c_char,
            345 as libc::c_int,
            b"false\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
}
