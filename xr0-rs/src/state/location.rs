#![allow(
    dead_code,
    mutable_transmutes,
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
    state_alloc, state_clump, state_get, state_getblock, state_getheap, ObjectRes,
};
use crate::util::{error, error_create, strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{AstExpr, Block, Clump, Heap, Stack, State, StaticMemory, StrBuilder, VConst, Value};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Location {
    pub type_0: LocationType,
    pub u: C2RustUnnamed,
    pub block: libc::c_int,
    pub offset: *mut AstExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub frame: libc::c_int,
}
pub type LocationType = libc::c_uint;
pub const LOCATION_DYNAMIC: LocationType = 4;
pub const LOCATION_AUTOMATIC: LocationType = 3;
pub const LOCATION_DEREFERENCABLE: LocationType = 2;
pub const LOCATION_VCONST: LocationType = 1;
pub const LOCATION_STATIC: LocationType = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct BlockRes {
    pub b: *mut Block,
    pub err: *mut error,
}

pub unsafe fn location_create_vconst(
    mut block: libc::c_int,
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
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
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
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
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
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
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
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
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
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

pub unsafe fn location_transfigure(mut loc: *mut Location, mut compare: *mut State) -> *mut Value {
    match (*loc).type_0 {
        3 | 2 => state_clump(compare),
        4 => state_alloc(compare),
        _ => panic!(),
    }
}

pub unsafe fn location_destroy(mut loc: *mut Location) {
    ast_expr_destroy((*loc).offset);
    free(loc as *mut libc::c_void);
}

pub unsafe fn location_str(mut loc: *mut Location) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    match (*loc).type_0 {
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
unsafe fn offsetzero(mut loc: *mut Location) -> bool {
    let mut zero: *mut AstExpr = ast_expr_constant_create(0 as libc::c_int);
    let mut eq: bool = ast_expr_equal((*loc).offset, zero);
    ast_expr_destroy(zero);
    return eq;
}

pub unsafe fn location_type(mut loc: *mut Location) -> LocationType {
    return (*loc).type_0;
}

pub unsafe fn location_block(mut loc: *mut Location) -> libc::c_int {
    return (*loc).block;
}

pub unsafe fn location_offset(mut loc: *mut Location) -> *mut AstExpr {
    return (*loc).offset;
}

pub unsafe fn location_copy(mut loc: *mut Location) -> *mut Location {
    match (*loc).type_0 {
        0 => location_create_static((*loc).block, ast_expr_copy((*loc).offset)),
        1 => location_create_vconst((*loc).block, ast_expr_copy((*loc).offset)),
        2 => location_create_dereferencable((*loc).block, ast_expr_copy((*loc).offset)),
        3 => location_create_automatic((*loc).u.frame, (*loc).block, ast_expr_copy((*loc).offset)),
        4 => location_create_dynamic((*loc).block, ast_expr_copy((*loc).offset)),
        _ => panic!(),
    }
}

pub unsafe fn location_with_offset(
    mut loc: *mut Location,
    mut offset: *mut AstExpr,
) -> *mut Location {
    if !offsetzero(loc) {
        panic!();
    }
    let mut copy: *mut Location = location_copy(loc);
    (*copy).offset = ast_expr_copy(offset);
    return copy;
}

pub unsafe fn location_tostatic(mut loc: *mut Location, mut sm: *mut StaticMemory) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_STATIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = static_memory_getblock(sm, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_toheap(mut loc: *mut Location, mut h: *mut Heap) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = heap_getblock(h, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_tostack(mut loc: *mut Location, mut s: *mut Stack) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = stack_getblock(s, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_toclump(mut loc: *mut Location, mut c: *mut Clump) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_DEREFERENCABLE as libc::c_int as libc::c_uint;
    let mut b: *mut Block = clump_getblock(c, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_equal(mut l1: *mut Location, mut l2: *mut Location) -> bool {
    return (*l1).type_0 as libc::c_uint == (*l2).type_0 as libc::c_uint
        && (*l1).block == (*l2).block
        && ast_expr_equal((*l1).offset, (*l2).offset) as libc::c_int != 0;
}

pub unsafe fn location_references(
    mut l1: *mut Location,
    mut l2: *mut Location,
    mut s: *mut State,
) -> bool {
    if location_equal(l1, l2) {
        return 1 as libc::c_int != 0;
    }
    let mut b: *mut Block = state_getblock(s, l1);
    return !b.is_null() && block_references(b, l2, s) as libc::c_int != 0;
}

pub unsafe fn location_isauto(mut loc: *mut Location) -> bool {
    return (*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
}

pub unsafe fn location_referencesheap(mut l: *mut Location, mut s: *mut State) -> bool {
    if (*l).type_0 as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        if heap_blockisfreed(state_getheap(s), (*l).block) {
            return 0 as libc::c_int != 0;
        }
        return 1 as libc::c_int != 0;
    }
    let mut res: ObjectRes = state_get(s, l, 0 as libc::c_int != 0);
    if !(res.err).is_null() {
        panic!();
    }
    return !(res.obj).is_null() && object_referencesheap(res.obj, s) as libc::c_int != 0;
}

pub unsafe fn location_getblock(
    mut loc: *mut Location,
    mut sm: *mut StaticMemory,
    mut v: *mut VConst,
    mut s: *mut Stack,
    mut h: *mut Heap,
    mut c: *mut Clump,
) -> BlockRes {
    if s.is_null() {
        panic!();
    }
    match (*loc).type_0 {
        0 => BlockRes {
            b: static_memory_getblock(sm, (*loc).block),
            err: 0 as *mut error,
        },
        3 => location_auto_getblock(loc, s),
        4 => BlockRes {
            b: heap_getblock(h, (*loc).block),
            err: 0 as *mut error,
        },
        2 => BlockRes {
            b: clump_getblock(c, (*loc).block),
            err: 0 as *mut error,
        },
        _ => panic!(),
    }
}

unsafe fn location_auto_getblock(mut loc: *mut Location, mut s: *mut Stack) -> BlockRes {
    let mut f: *mut Stack = stack_getframe(s, (*loc).u.frame);
    if f.is_null() {
        return {
            let mut init = BlockRes {
                b: 0 as *mut Block,
                err: error_create(
                    b"stack frame doesn't exist\0" as *const u8 as *const libc::c_char
                        as *mut libc::c_char,
                ),
            };
            init
        };
    }
    return {
        let mut init = BlockRes {
            b: stack_getblock(f, (*loc).block),
            err: 0 as *mut error,
        };
        init
    };
}

pub unsafe fn location_getstackblock(mut loc: *mut Location, mut s: *mut Stack) -> *mut Block {
    if !((*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    return stack_getblock(s, (*loc).block);
}

pub unsafe fn location_dealloc(mut loc: *mut Location, mut heap: *mut Heap) -> *mut error {
    if (*loc).type_0 as libc::c_uint != LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        return error_create(
            b"not heap location\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return heap_deallocblock(heap, (*loc).block);
}

pub unsafe fn location_range_dealloc(
    mut loc: *mut Location,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut state: *mut State,
) -> *mut error {
    if !offsetzero(loc) {
        panic!();
    }
    let mut b: *mut Block = state_getblock(state, loc);
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
