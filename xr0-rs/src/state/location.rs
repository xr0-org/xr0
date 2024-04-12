#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_variables
)]

use libc::{free, malloc, printf};

use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_destroy, ast_expr_equal, ast_expr_str,
};
use crate::object::object_referencesheap;
use crate::state::block::{
    block_range_aredeallocands, block_range_dealloc, block_references, block_str,
};
use crate::state::clump::clump_getblock;
use crate::state::heap::{heap_blockisfreed, heap_deallocblock, heap_getblock};
use crate::state::r#static::static_memory_getblock;
use crate::state::stack::{stack_getblock, stack_getframe};
use crate::state::state::{
    object_res, state_alloc, state_clump, state_get, state_getblock, state_getheap,
};
use crate::util::{error, error_create, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{
    static_memory, vconst, AstExpr as ast_expr, Block as block, Clump as clump, Heap as heap,
    Stack as stack, State as state, StrBuilder as strbuilder, Value as value,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct location {
    pub type_0: location_type,
    pub u: C2RustUnnamed,
    pub block: libc::c_int,
    pub offset: *mut ast_expr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub frame: libc::c_int,
}
pub type location_type = libc::c_uint;
pub const LOCATION_DYNAMIC: location_type = 4;
pub const LOCATION_AUTOMATIC: location_type = 3;
pub const LOCATION_DEREFERENCABLE: location_type = 2;
pub const LOCATION_VCONST: location_type = 1;
pub const LOCATION_STATIC: location_type = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct block_res {
    pub b: *mut block,
    pub err: *mut error,
}

pub unsafe fn location_create_vconst(
    mut block: libc::c_int,
    mut offset: *mut ast_expr,
) -> *mut location {
    let mut loc: *mut location = malloc(::core::mem::size_of::<location>()) as *mut location;
    if loc.is_null() {
        panic!();
    }
    (*loc).type_0 = LOCATION_VCONST;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_dereferencable(
    mut block: libc::c_int,
    mut offset: *mut ast_expr,
) -> *mut location {
    let mut loc: *mut location = malloc(::core::mem::size_of::<location>()) as *mut location;
    if loc.is_null() {
        panic!();
    }
    (*loc).type_0 = LOCATION_DEREFERENCABLE;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_static(
    mut block: libc::c_int,
    mut offset: *mut ast_expr,
) -> *mut location {
    let mut loc: *mut location = malloc(::core::mem::size_of::<location>()) as *mut location;
    if loc.is_null() {
        panic!();
    }
    (*loc).type_0 = LOCATION_STATIC;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_dynamic(
    mut block: libc::c_int,
    mut offset: *mut ast_expr,
) -> *mut location {
    let mut loc: *mut location = malloc(::core::mem::size_of::<location>()) as *mut location;
    if loc.is_null() {
        panic!();
    }
    (*loc).type_0 = LOCATION_DYNAMIC;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_automatic(
    mut frame: libc::c_int,
    mut block: libc::c_int,
    mut offset: *mut ast_expr,
) -> *mut location {
    let mut loc: *mut location = malloc(::core::mem::size_of::<location>()) as *mut location;
    if loc.is_null() {
        panic!();
    }
    (*loc).type_0 = LOCATION_AUTOMATIC;
    (*loc).u.frame = frame;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_transfigure(mut loc: *mut location, mut compare: *mut state) -> *mut value {
    match (*loc).type_0 as libc::c_uint {
        3 | 2 => state_clump(compare),
        4 => state_alloc(compare),
        _ => panic!(),
    }
}

pub unsafe fn location_destroy(mut loc: *mut location) {
    ast_expr_destroy((*loc).offset);
    free(loc as *mut libc::c_void);
}

pub unsafe fn location_str(mut loc: *mut location) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    match (*loc).type_0 as libc::c_uint {
        0 => {
            strbuilder_printf(b, b"static:\0" as *const u8 as *const libc::c_char);
        }
        3 => {
            strbuilder_printf(
                b,
                b"stack[%d]:\0" as *const u8 as *const libc::c_char,
                (*loc).u.frame,
            );
        }
        4 => {
            strbuilder_printf(b, b"heap:\0" as *const u8 as *const libc::c_char);
        }
        2 => {
            strbuilder_printf(b, b"clump:\0" as *const u8 as *const libc::c_char);
        }
        _ => panic!(),
    }
    strbuilder_printf(b, b"%d\0" as *const u8 as *const libc::c_char, (*loc).block);
    if !offsetzero(loc) {
        let mut offset: *mut libc::c_char = ast_expr_str((*loc).offset);
        strbuilder_printf(b, b"+%s\0" as *const u8 as *const libc::c_char, offset);
        free(offset as *mut libc::c_void);
    }
    return strbuilder_build(b);
}
unsafe fn offsetzero(mut loc: *mut location) -> bool {
    let mut zero: *mut ast_expr = ast_expr_constant_create(0 as libc::c_int);
    let mut eq: bool = ast_expr_equal((*loc).offset, zero);
    ast_expr_destroy(zero);
    return eq;
}

pub unsafe fn location_type(mut loc: *mut location) -> location_type {
    return (*loc).type_0;
}

pub unsafe fn location_block(mut loc: *mut location) -> libc::c_int {
    return (*loc).block;
}

pub unsafe fn location_offset(mut loc: *mut location) -> *mut ast_expr {
    return (*loc).offset;
}

pub unsafe fn location_copy(mut loc: *mut location) -> *mut location {
    match (*loc).type_0 as libc::c_uint {
        0 => location_create_static((*loc).block, ast_expr_copy((*loc).offset)),
        1 => location_create_vconst((*loc).block, ast_expr_copy((*loc).offset)),
        2 => location_create_dereferencable((*loc).block, ast_expr_copy((*loc).offset)),
        3 => location_create_automatic((*loc).u.frame, (*loc).block, ast_expr_copy((*loc).offset)),
        4 => location_create_dynamic((*loc).block, ast_expr_copy((*loc).offset)),
        _ => panic!(),
    }
}

pub unsafe fn location_with_offset(
    mut loc: *mut location,
    mut offset: *mut ast_expr,
) -> *mut location {
    if !offsetzero(loc) {
        panic!();
    }
    let mut copy: *mut location = location_copy(loc);
    (*copy).offset = ast_expr_copy(offset);
    return copy;
}

pub unsafe fn location_tostatic(mut loc: *mut location, mut sm: *mut static_memory) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_STATIC as libc::c_int as libc::c_uint;
    let mut b: *mut block = static_memory_getblock(sm, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_toheap(mut loc: *mut location, mut h: *mut heap) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let mut b: *mut block = heap_getblock(h, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_tostack(mut loc: *mut location, mut s: *mut stack) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
    let mut b: *mut block = stack_getblock(s, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_toclump(mut loc: *mut location, mut c: *mut clump) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_DEREFERENCABLE as libc::c_int as libc::c_uint;
    let mut b: *mut block = clump_getblock(c, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_equal(mut l1: *mut location, mut l2: *mut location) -> bool {
    return (*l1).type_0 as libc::c_uint == (*l2).type_0 as libc::c_uint
        && (*l1).block == (*l2).block
        && ast_expr_equal((*l1).offset, (*l2).offset) as libc::c_int != 0;
}

pub unsafe fn location_references(
    mut l1: *mut location,
    mut l2: *mut location,
    mut s: *mut state,
) -> bool {
    if location_equal(l1, l2) {
        return 1 as libc::c_int != 0;
    }
    let mut b: *mut block = state_getblock(s, l1);
    return !b.is_null() && block_references(b, l2, s) as libc::c_int != 0;
}

pub unsafe fn location_isauto(mut loc: *mut location) -> bool {
    return (*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
}

pub unsafe fn location_referencesheap(mut l: *mut location, mut s: *mut state) -> bool {
    if (*l).type_0 as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        if heap_blockisfreed(state_getheap(s), (*l).block) {
            return 0 as libc::c_int != 0;
        }
        return 1 as libc::c_int != 0;
    }
    let mut res: object_res = state_get(s, l, 0 as libc::c_int != 0);
    if !(res.err).is_null() {
        panic!();
    }
    return !(res.obj).is_null() && object_referencesheap(res.obj, s) as libc::c_int != 0;
}

pub unsafe fn location_getblock(
    mut loc: *mut location,
    mut sm: *mut static_memory,
    mut v: *mut vconst,
    mut s: *mut stack,
    mut h: *mut heap,
    mut c: *mut clump,
) -> block_res {
    if s.is_null() {
        panic!();
    }
    match (*loc).type_0 as libc::c_uint {
        0 => block_res {
            b: static_memory_getblock(sm, (*loc).block),
            err: 0 as *mut error,
        },
        3 => location_auto_getblock(loc, s),
        4 => block_res {
            b: heap_getblock(h, (*loc).block),
            err: 0 as *mut error,
        },
        2 => block_res {
            b: clump_getblock(c, (*loc).block),
            err: 0 as *mut error,
        },
        _ => panic!(),
    }
}

unsafe fn location_auto_getblock(mut loc: *mut location, mut s: *mut stack) -> block_res {
    let mut f: *mut stack = stack_getframe(s, (*loc).u.frame);
    if f.is_null() {
        return {
            let mut init = block_res {
                b: 0 as *mut block,
                err: error_create(
                    b"stack frame doesn't exist\0" as *const u8 as *const libc::c_char
                        as *mut libc::c_char,
                ),
            };
            init
        };
    }
    return {
        let mut init = block_res {
            b: stack_getblock(f, (*loc).block),
            err: 0 as *mut error,
        };
        init
    };
}

pub unsafe fn location_getstackblock(mut loc: *mut location, mut s: *mut stack) -> *mut block {
    if !((*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    return stack_getblock(s, (*loc).block);
}

pub unsafe fn location_dealloc(mut loc: *mut location, mut heap: *mut heap) -> *mut error {
    if (*loc).type_0 as libc::c_uint != LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        return error_create(
            b"not heap location\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return heap_deallocblock(heap, (*loc).block);
}

pub unsafe fn location_range_dealloc(
    mut loc: *mut location,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
    mut state: *mut state,
) -> *mut error {
    if !offsetzero(loc) {
        panic!();
    }
    let mut b: *mut block = state_getblock(state, loc);
    if b.is_null() {
        return error_create(
            b"cannot get block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    if !block_range_aredeallocands(b, lw, up, state) {
        printf(
            b"block: %s\n\0" as *const u8 as *const libc::c_char,
            block_str(b),
        );
        printf(
            b"lw: %s, up: %s\n\0" as *const u8 as *const libc::c_char,
            ast_expr_str(lw),
            ast_expr_str(up),
        );
        debug_assert!(false);
        return error_create(
            b"some values not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return block_range_dealloc(b, lw, up, state);
}
