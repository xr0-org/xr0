#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use libc::{calloc, free, malloc, realloc};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_difference_create, ast_expr_eq_create,
    ast_expr_sum_create,
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
use crate::util::{error_create, strbuilder_build, strbuilder_create, Result};
use crate::{
    cstr, strbuilder_write, AstExpr, Heap, Location, Object, ObjectArr, State, StrBuilder,
};

pub struct Block {
    pub arr: *mut ObjectArr,
}

pub struct BlockArr {
    pub n: libc::c_int,
    pub block: *mut *mut Block,
}

pub unsafe fn block_create() -> *mut Block {
    let b: *mut Block = malloc(::core::mem::size_of::<Block>()) as *mut Block;
    (*b).arr = object_arr_create();
    b
}

pub unsafe fn block_destroy(b: *mut Block) {
    object_arr_destroy((*b).arr);
    free(b as *mut libc::c_void);
}

pub unsafe fn block_copy(old: *mut Block) -> *mut Block {
    let new: *mut Block = malloc(::core::mem::size_of::<Block>()) as *mut Block;
    (*new).arr = object_arr_copy((*old).arr);
    new
}

pub unsafe fn block_str(block: *mut Block) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let obj: *mut *mut Object = object_arr_objects((*block).arr);
    let n: libc::c_int = object_arr_nobjects((*block).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let s: *mut libc::c_char = object_str(*obj.offset(i as isize));
        strbuilder_write!(
            b,
            "{}{}",
            cstr!(s),
            if (i + 1 as libc::c_int) < n { ", " } else { "" },
        );
        free(s as *mut libc::c_void);
        i += 1;
    }
    strbuilder_build(b)
}

pub unsafe fn block_install(b: *mut Block, obj: *mut Object) {
    if !(object_arr_nobjects((*b).arr) == 0 as libc::c_int) {
        panic!();
    }
    object_arr_append((*b).arr, obj);
}

pub unsafe fn block_observe(
    b: *mut Block,
    offset: &AstExpr,
    s: *mut State,
    constructive: bool,
) -> *mut Object {
    let mut index: libc::c_int = object_arr_index((*b).arr, offset, s);
    if index == -(1 as libc::c_int) {
        if !constructive {
            return ptr::null_mut();
        }
        let obj: *mut Object =
            object_value_create(Box::into_raw(ast_expr_copy(offset)), ptr::null_mut());
        object_arr_append((*b).arr, obj);
        return obj;
    }
    let obj_0: *mut Object = *(object_arr_objects((*b).arr)).offset(index as isize);
    if object_isvalue(obj_0) {
        return obj_0;
    }
    let lw = ast_expr_copy(offset);
    let up = ast_expr_sum_create(
        ast_expr_copy(offset),
        ast_expr_constant_create(1 as libc::c_int),
    );
    // Note: Original stores `lw` in `upto` but then also destroys `lw` a few lines down. I don't
    // know why it isn't a double free.
    let lw_ptr = Box::into_raw(lw);
    let upto: *mut Object = object_upto(obj_0, lw_ptr, s);
    let observed: *mut Object =
        object_value_create(Box::into_raw(ast_expr_copy(&*lw_ptr)), state_alloc(s));
    let from: *mut Object = object_from(obj_0, &up, s);
    drop(up);
    drop(Box::from_raw(lw_ptr));

    object_dealloc(obj_0, s).unwrap();
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
    observed
}

pub unsafe fn block_references(b: *mut Block, loc: *mut Location, s: *mut State) -> bool {
    let n: libc::c_int = object_arr_nobjects((*b).arr);
    let obj: *mut *mut Object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if object_references(*obj.offset(i as isize), loc, s) {
            return true;
        }
        i += 1;
    }
    false
}

pub unsafe fn block_range_alloc(
    b: &Block,
    lw: &AstExpr,
    up: &AstExpr,
    heap: *mut Heap,
) -> Result<()> {
    if !(object_arr_nobjects(b.arr) == 0 as libc::c_int) {
        panic!();
    }
    object_arr_append(
        b.arr,
        object_range_create(
            Box::into_raw(ast_expr_copy(lw)),
            range_create(
                Box::into_raw(ast_expr_difference_create(
                    ast_expr_copy(up),
                    ast_expr_copy(lw),
                )),
                heap_newblock(heap),
            ),
        ),
    );
    Ok(())
}

pub unsafe fn block_range_aredeallocands(
    b: &Block,
    lw: &AstExpr,
    up: &AstExpr,
    s: *mut State,
) -> bool {
    if hack_first_object_is_exactly_bounds(b, lw, up, s) {
        return true;
    }
    let lw_index: libc::c_int = object_arr_index(b.arr, lw, s);
    if lw_index == -(1 as libc::c_int) {
        return false;
    }
    let up_index: libc::c_int = object_arr_index_upperincl(b.arr, up, s);
    if up_index == -(1 as libc::c_int) {
        return false;
    }
    let obj: *mut *mut Object = object_arr_objects(b.arr);
    let mut i: libc::c_int = lw_index;
    while i < up_index {
        if !object_isdeallocand(*obj.offset(i as isize), s) {
            return false;
        }
        if !object_contig_precedes(
            *obj.offset(i as isize),
            *obj.offset((i + 1 as libc::c_int) as isize),
            s,
        ) {
            return false;
        }
        i += 1;
    }
    if !object_isdeallocand(*obj.offset(up_index as isize), s) {
        panic!();
    }
    true
}

unsafe fn hack_first_object_is_exactly_bounds(
    b: &Block,
    lw: &AstExpr,
    up: &AstExpr,
    s: *mut State,
) -> bool {
    if object_arr_nobjects(b.arr) == 0 as libc::c_int {
        return false;
    }
    let obj: *mut Object = *(object_arr_objects(b.arr)).offset(0 as libc::c_int as isize);
    if !object_isdeallocand(obj, s) {
        return false;
    }
    // Note: Original leaks these outer expressions to avoid double-freeing the inner ones.
    let same_lw = ast_expr_eq_create(
        Box::from_raw(lw as *const AstExpr as *mut AstExpr),
        Box::from_raw(object_lower(obj)),
    );
    let same_up = ast_expr_eq_create(
        Box::from_raw(up as *const AstExpr as *mut AstExpr),
        Box::from_raw(object_upper(obj)),
    );
    let result = state_eval(s, &same_lw) && state_eval(s, &same_up);
    std::mem::forget(same_lw);
    std::mem::forget(same_up);
    result
}

pub unsafe fn block_range_dealloc(
    b: *mut Block,
    lw: &AstExpr,
    up: &AstExpr,
    s: *mut State,
) -> Result<()> {
    if hack_first_object_is_exactly_bounds(&*b, lw, up, s) {
        object_dealloc(
            *(object_arr_objects((*b).arr)).offset(0 as libc::c_int as isize),
            s,
        )?;
        object_arr_remove((*b).arr, 0 as libc::c_int);
        return Ok(());
    }
    let lw_index: libc::c_int = object_arr_index((*b).arr, lw, s);
    if lw_index == -(1 as libc::c_int) {
        return Err(error_create(
            b"lower bound not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    let up_index: libc::c_int = object_arr_index_upperincl((*b).arr, up, s);
    if up_index == -(1 as libc::c_int) {
        return Err(error_create(
            b"upper bound not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    let n: libc::c_int = object_arr_nobjects((*b).arr);
    let obj: *mut *mut Object = object_arr_objects((*b).arr);
    // Note: Original stores `lw` in `upto` but then the caller presumably also destroys `lw`. I
    // don't know why it isn't a double free.
    let upto: *mut Object = object_upto(
        *obj.offset(lw_index as isize),
        lw as *const AstExpr as *mut AstExpr,
        s,
    );
    let from: *mut Object = object_from(*obj.offset(up_index as isize), up, s);
    let new: *mut ObjectArr = object_arr_create();
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
        object_dealloc(*obj.offset(i_1 as isize), s)?;
        i_1 += 1;
    }
    (*b).arr = new;
    Ok(())
}

pub unsafe fn block_undeclare(b: *mut Block, s: *mut State) {
    let new: *mut ObjectArr = object_arr_create();
    let n: libc::c_int = object_arr_nobjects((*b).arr);
    let object: *mut *mut Object = object_arr_objects((*b).arr);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let obj: *mut Object = *object.offset(i as isize);
        if object_referencesheap(obj, s) {
            object_arr_append(new, object_abstractcopy(obj, s));
        }
        i += 1;
    }
    object_arr_destroy((*b).arr);
    (*b).arr = new;
}

pub unsafe fn block_arr_create() -> *mut BlockArr {
    let arr: *mut BlockArr = calloc(1, ::core::mem::size_of::<BlockArr>()) as *mut BlockArr;
    if arr.is_null() {
        panic!();
    }
    arr
}

pub unsafe fn block_arr_destroy(arr: *mut BlockArr) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*arr).n {
        block_destroy(*((*arr).block).offset(i as isize));
        i += 1;
    }
    free((*arr).block as *mut libc::c_void);
    free(arr as *mut libc::c_void);
}

pub unsafe fn block_arr_copy(old: *mut BlockArr) -> *mut BlockArr {
    let new: *mut BlockArr = block_arr_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*old).n {
        block_arr_append(new, block_copy(*((*old).block).offset(i as isize)));
        i += 1;
    }
    new
}

pub unsafe fn block_arr_blocks(arr: *mut BlockArr) -> *mut *mut Block {
    (*arr).block
}

pub unsafe fn block_arr_nblocks(arr: *mut BlockArr) -> libc::c_int {
    (*arr).n
}

pub unsafe fn block_arr_append(arr: *mut BlockArr, b: *mut Block) -> libc::c_int {
    (*arr).n += 1;
    (*arr).block = realloc(
        (*arr).block as *mut libc::c_void,
        (::core::mem::size_of::<BlockArr>()).wrapping_mul((*arr).n as usize),
    ) as *mut *mut Block;
    if ((*arr).block).is_null() {
        panic!();
    }
    let loc: libc::c_int = (*arr).n - 1 as libc::c_int;
    let ref mut fresh2 = *((*arr).block).offset(loc as isize);
    *fresh2 = b;
    loc
}

pub unsafe fn block_arr_delete(arr: *mut BlockArr, address: libc::c_int) {
    panic!();
}
