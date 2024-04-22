#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use libc::{calloc, free, realloc};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_difference_create, ast_expr_eq_create,
    ast_expr_sum_create,
};
use crate::object::{
    object_abstractcopy, object_arr_index, object_arr_index_upperincl, object_contig_precedes,
    object_copy, object_dealloc, object_destroy, object_from, object_isdeallocand, object_isvalue,
    object_lower, object_range_create, object_references, object_referencesheap, object_str,
    object_upper, object_upto, object_value_create, range_create,
};
use crate::state::heap::heap_newblock;
use crate::state::state::{state_alloc, state_eval};
use crate::util::{strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::{strbuilder_write, AstExpr, Heap, Location, Object, State, StrBuilder};

pub struct Block {
    pub arr: Vec<*mut Object>,
}

pub struct BlockArr {
    pub n: libc::c_int,
    pub block: *mut *mut Block,
}

pub unsafe fn block_create() -> *mut Block {
    Box::into_raw(Box::new(Block { arr: vec![] }))
}

pub unsafe fn block_destroy(b: *mut Block) {
    drop(Box::from_raw(b));
}

impl Drop for Block {
    fn drop(&mut self) {
        unsafe {
            for &obj in &self.arr {
                object_destroy(obj);
            }
        }
    }
}

impl Clone for Block {
    fn clone(&self) -> Block {
        Block {
            arr: self
                .arr
                .iter()
                .copied()
                .map(|obj| unsafe { object_copy(obj) })
                .collect(),
        }
    }
}

pub unsafe fn block_copy(old: *mut Block) -> *mut Block {
    Box::into_raw(Box::new((*old).clone()))
}

pub unsafe fn block_str(block: &Block) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    let n = block.arr.len();
    for (i, &obj) in block.arr.iter().enumerate() {
        let s = object_str(obj);
        strbuilder_write!(b, "{s}{}", if i + 1 < n { ", " } else { "" });
    }
    strbuilder_build(b)
}

pub unsafe fn block_install(b: *mut Block, obj: *mut Object) {
    assert!((*b).arr.is_empty());
    (*b).arr.push(obj);
}

pub unsafe fn block_observe(
    b: *mut Block,
    offset: &AstExpr,
    s: *mut State,
    constructive: bool,
) -> *mut Object {
    let Some(mut index) = object_arr_index(&(*b).arr, offset, s) else {
        if !constructive {
            return ptr::null_mut();
        }
        let obj: *mut Object =
            object_value_create(Box::into_raw(ast_expr_copy(offset)), ptr::null_mut());
        (*b).arr.push(obj);
        return obj;
    };
    let obj: *mut Object = (*b).arr[index];
    if object_isvalue(obj) {
        return obj;
    }
    let lw = ast_expr_copy(offset);
    let up = ast_expr_sum_create(
        ast_expr_copy(offset),
        ast_expr_constant_create(1 as libc::c_int),
    );
    // Note: Original stores `lw` in `upto` but then also destroys `lw` a few lines down. I don't
    // know why it isn't a double free.
    let lw_ptr = Box::into_raw(lw);
    let upto: *mut Object = object_upto(obj, lw_ptr, s);
    let observed: *mut Object =
        object_value_create(Box::into_raw(ast_expr_copy(&*lw_ptr)), state_alloc(s));
    let from: *mut Object = object_from(obj, &up, s);
    drop(up);
    drop(Box::from_raw(lw_ptr));

    object_dealloc(obj, s).unwrap();
    (*b).arr.remove(index);
    if !upto.is_null() {
        (*b).arr.insert(index, upto);
        index += 1;
    }
    (*b).arr.insert(index, observed);
    index += 1;
    if !from.is_null() {
        (*b).arr.insert(index, from);
    }
    observed
}

pub unsafe fn block_references(b: *mut Block, loc: &Location, s: *mut State) -> bool {
    (*b).arr.iter().any(|&obj| object_references(obj, loc, s))
}

pub unsafe fn block_range_alloc(
    b: &mut Block,
    lw: &AstExpr,
    up: &AstExpr,
    heap: *mut Heap,
) -> Result<()> {
    assert!(b.arr.is_empty());
    b.arr.push(object_range_create(
        Box::into_raw(ast_expr_copy(lw)),
        range_create(
            ast_expr_difference_create(ast_expr_copy(up), ast_expr_copy(lw)),
            Box::from_raw(heap_newblock(heap)),
        ),
    ));
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
    let Some(lw_index) = object_arr_index(&b.arr, lw, s) else {
        return false;
    };
    let Some(up_index) = object_arr_index_upperincl(&b.arr, up, s) else {
        return false;
    };
    for i in lw_index..up_index {
        if !object_isdeallocand(b.arr[i], s) {
            return false;
        }
        if !object_contig_precedes(b.arr[i], b.arr[i + 1], s) {
            return false;
        }
    }
    assert!(object_isdeallocand(b.arr[up_index], s));
    true
}

unsafe fn hack_first_object_is_exactly_bounds(
    b: &Block,
    lw: &AstExpr,
    up: &AstExpr,
    s: *mut State,
) -> bool {
    assert!(!b.arr.is_empty());
    let obj: *mut Object = b.arr[0];
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
    b: &mut Block,
    lw: &AstExpr,
    up: &AstExpr,
    s: *mut State,
) -> Result<()> {
    if hack_first_object_is_exactly_bounds(b, lw, up, s) {
        object_dealloc(b.arr[0], s)?;
        b.arr.remove(0);
        return Ok(());
    }
    let Some(lw_index) = object_arr_index(&b.arr, lw, s) else {
        return Err(Error::new("lower bound not allocated".to_string()));
    };
    let Some(up_index) = object_arr_index_upperincl(&b.arr, up, s) else {
        return Err(Error::new("upper bound not allocated".to_string()));
    };
    let n = b.arr.len();
    // Note: Original stores `lw` in `upto` but then the caller presumably also destroys `lw`. I
    // don't know why it isn't a double free.
    let upto: *mut Object = object_upto(b.arr[lw_index], lw as *const AstExpr as *mut AstExpr, s);
    let from: *mut Object = object_from(b.arr[up_index], up, s);
    let mut new = b.arr[..lw_index].to_vec();
    if !upto.is_null() {
        // Note: Possibly appending so that they'll be destroyed? But then b.arr is overwritten without destroying it.
        b.arr.push(upto);
    }
    if !from.is_null() {
        b.arr.push(from);
    }
    for i in (up_index + 1)..n {
        // Note: Original uses `obj` after `object_arr_append` which might invalidate it. XXX BIG point
        // in favor of Rust.
        new.push(b.arr[i]);
    }
    for i in lw_index..=up_index {
        object_dealloc(b.arr[i], s)?;
    }
    b.arr = new;
    Ok(())
}

pub unsafe fn block_undeclare(b: *mut Block, s: *mut State) {
    let mut new = vec![];
    for &obj in &(*b).arr {
        if object_referencesheap(obj, s) {
            new.push(object_abstractcopy(obj, s));
        }
    }
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
