#![allow(
    dead_code,
    mutable_transmutes,
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
use crate::object::{
    object_abstractcopy, object_arr_append, object_arr_copy, object_arr_create, object_arr_destroy,
    object_arr_index, object_arr_index_upperincl, object_arr_insert, object_arr_nobjects,
    object_arr_objects, object_arr_remove, object_contig_precedes, object_dealloc, object_from,
    object_isdeallocand, object_isvalue, object_lower, object_range_create, object_references,
    object_referencesheap, object_str, object_upper, object_upto, object_value_create,
    range_create,
};
use crate::state::heap::heap_newblock;
use crate::state::state::{state_alloc, state_eval};
use crate::util::{error, error_create, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{AstExpr, Heap, Location, Object, ObjectArr, State, StrBuilder, Value};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Block {
    pub arr: *mut ObjectArr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct BlockArr {
    pub n: libc::c_int,
    pub block: *mut *mut Block,
}

pub unsafe fn block_create() -> *mut Block {
    let mut b: *mut Block = malloc(::core::mem::size_of::<Block>()) as *mut Block;
    (*b).arr = object_arr_create();
    return b;
}

pub unsafe fn block_destroy(mut b: *mut Block) {
    object_arr_destroy((*b).arr);
    free(b as *mut libc::c_void);
}

pub unsafe fn block_copy(mut old: *mut Block) -> *mut Block {
    let mut new: *mut Block = malloc(::core::mem::size_of::<Block>()) as *mut Block;
    (*new).arr = object_arr_copy((*old).arr);
    return new;
}

pub unsafe fn block_str(mut block: *mut Block) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut obj: *mut *mut Object = object_arr_objects((*block).arr);
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

pub unsafe fn block_install(mut b: *mut Block, mut obj: *mut Object) {
    if !(object_arr_nobjects((*b).arr) == 0 as libc::c_int) {
        panic!();
    } else {
    };
    object_arr_append((*b).arr, obj);
}

pub unsafe fn block_observe(
    mut b: *mut Block,
    mut offset: *mut AstExpr,
    mut s: *mut State,
    mut constructive: bool,
) -> *mut Object {
    let mut index: libc::c_int = object_arr_index((*b).arr, offset, s);
    if index == -(1 as libc::c_int) {
        if !constructive {
            return 0 as *mut Object;
        }
        let mut obj: *mut Object = object_value_create(ast_expr_copy(offset), 0 as *mut Value);
        object_arr_append((*b).arr, obj);
        return obj;
    }
    let mut obj_0: *mut Object = *(object_arr_objects((*b).arr)).offset(index as isize);
    if object_isvalue(obj_0) {
        return obj_0;
    }
    let mut lw: *mut AstExpr = ast_expr_copy(offset);
    let mut up: *mut AstExpr = ast_expr_sum_create(
        ast_expr_copy(offset),
        ast_expr_constant_create(1 as libc::c_int),
    );
    let mut upto: *mut Object = object_upto(obj_0, lw, s);
    let mut observed: *mut Object = object_value_create(ast_expr_copy(lw), state_alloc(s));
    let mut from: *mut Object = object_from(obj_0, up, s);
    ast_expr_destroy(up);
    ast_expr_destroy(lw);
    let mut err: *mut error = object_dealloc(obj_0, s);
    if !err.is_null() {
        panic!();
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

pub unsafe fn block_references(
    mut b: *mut Block,
    mut loc: *mut Location,
    mut s: *mut State,
) -> bool {
    let mut n: libc::c_int = object_arr_nobjects((*b).arr);
    let mut obj: *mut *mut Object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if object_references(*obj.offset(i as isize), loc, s) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}

pub unsafe fn block_range_alloc(
    mut b: *mut Block,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut heap: *mut Heap,
) -> *mut error {
    if !(object_arr_nobjects((*b).arr) == 0 as libc::c_int) {
        panic!();
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

pub unsafe fn block_range_aredeallocands(
    mut b: *mut Block,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut s: *mut State,
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
    let mut obj: *mut *mut Object = object_arr_objects((*b).arr);
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
    if !object_isdeallocand(*obj.offset(up_index as isize), s) {
        panic!();
    } else {
    };
    return 1 as libc::c_int != 0;
}
unsafe fn hack_first_object_is_exactly_bounds(
    mut b: *mut Block,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut s: *mut State,
) -> bool {
    if object_arr_nobjects((*b).arr) == 0 as libc::c_int {
        return 0 as libc::c_int != 0;
    }
    let mut obj: *mut Object = *(object_arr_objects((*b).arr)).offset(0 as libc::c_int as isize);
    if !object_isdeallocand(obj, s) {
        return 0 as libc::c_int != 0;
    }
    let mut same_lw: *mut AstExpr = ast_expr_eq_create(lw, object_lower(obj));
    let mut same_up: *mut AstExpr = ast_expr_eq_create(up, object_upper(obj));
    return state_eval(s, same_lw) as libc::c_int != 0
        && state_eval(s, same_up) as libc::c_int != 0;
}

pub unsafe fn block_range_dealloc(
    mut b: *mut Block,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut s: *mut State,
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
    let mut obj: *mut *mut Object = object_arr_objects((*b).arr);
    let mut upto: *mut Object = object_upto(*obj.offset(lw_index as isize), lw, s);
    let mut from: *mut Object = object_from(*obj.offset(up_index as isize), up, s);
    let mut new: *mut ObjectArr = object_arr_create();
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

pub unsafe fn block_undeclare(mut b: *mut Block, mut s: *mut State) {
    let mut new: *mut ObjectArr = object_arr_create();
    let mut n: libc::c_int = object_arr_nobjects((*b).arr);
    let mut object: *mut *mut Object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut obj: *mut Object = *object.offset(i as isize);
        if object_referencesheap(obj, s) {
            object_arr_append(new, object_abstractcopy(obj, s));
        }
        i += 1;
    }
    object_arr_destroy((*b).arr);
    (*b).arr = new;
}

pub unsafe fn block_arr_create() -> *mut BlockArr {
    let mut arr: *mut BlockArr = calloc(1, ::core::mem::size_of::<BlockArr>()) as *mut BlockArr;
    if arr.is_null() {
        panic!();
    } else {
    };
    return arr;
}

pub unsafe fn block_arr_destroy(mut arr: *mut BlockArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        block_destroy(*((*arr).block).offset(i as isize));
        i += 1;
    }
    free((*arr).block as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}

pub unsafe fn block_arr_copy(mut old: *mut BlockArr) -> *mut BlockArr {
    let mut new: *mut BlockArr = block_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        block_arr_append(new, block_copy(*((*old).block).offset(i as isize)));
        i += 1;
    }
    return new;
}

pub unsafe fn block_arr_blocks(mut arr: *mut BlockArr) -> *mut *mut Block {
    return (*arr).block;
}

pub unsafe fn block_arr_nblocks(mut arr: *mut BlockArr) -> libc::c_int {
    return (*arr).n;
}

pub unsafe fn block_arr_append(mut arr: *mut BlockArr, mut b: *mut Block) -> libc::c_int {
    (*arr).n += 1;
    (*arr).block = realloc(
        (*arr).block as *mut libc::c_void,
        (::core::mem::size_of::<BlockArr>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut Block;
    if ((*arr).block).is_null() {
        panic!();
    }
    let mut loc: libc::c_int = (*arr).n - 1 as libc::c_int;
    let ref mut fresh2 = *((*arr).block).offset(loc as isize);
    *fresh2 = b;
    return loc;
}

pub unsafe fn block_arr_delete(mut arr: *mut BlockArr, mut address: libc::c_int) {
    panic!();
}