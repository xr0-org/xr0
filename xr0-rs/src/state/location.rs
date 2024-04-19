#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
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
use crate::state::state::{state_alloc, state_clump, state_get, state_getblock, state_getheap};
use crate::util::{error_create, strbuilder_build, strbuilder_create, Result};
use crate::{
    cstr, strbuilder_write, AstExpr, Block, Clump, Heap, Stack, State, StaticMemory, StrBuilder,
    VConst, Value,
};

#[derive(Copy, Clone)]
pub struct Location {
    pub r#type: LocationType,
    pub u: C2RustUnnamed,
    pub block: libc::c_int,
    pub offset: *mut AstExpr,
}
#[derive(Copy, Clone)]
pub union C2RustUnnamed {
    pub frame: libc::c_int,
}
pub type LocationType = libc::c_uint;
pub const LOCATION_DYNAMIC: LocationType = 4;
pub const LOCATION_AUTOMATIC: LocationType = 3;
pub const LOCATION_DEREFERENCABLE: LocationType = 2;
pub const LOCATION_VCONST: LocationType = 1;
pub const LOCATION_STATIC: LocationType = 0;

pub unsafe fn location_create_vconst(block: libc::c_int, offset: *mut AstExpr) -> *mut Location {
    let loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
    if loc.is_null() {
        panic!();
    }
    (*loc).r#type = LOCATION_VCONST;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_dereferencable(
    block: libc::c_int,
    offset: *mut AstExpr,
) -> *mut Location {
    let loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
    if loc.is_null() {
        panic!();
    }
    (*loc).r#type = LOCATION_DEREFERENCABLE;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_static(block: libc::c_int, offset: *mut AstExpr) -> *mut Location {
    let loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
    if loc.is_null() {
        panic!();
    }
    (*loc).r#type = LOCATION_STATIC;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_dynamic(block: libc::c_int, offset: *mut AstExpr) -> *mut Location {
    let loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
    if loc.is_null() {
        panic!();
    }
    (*loc).r#type = LOCATION_DYNAMIC;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_create_automatic(
    frame: libc::c_int,
    block: libc::c_int,
    offset: *mut AstExpr,
) -> *mut Location {
    let loc: *mut Location = malloc(::core::mem::size_of::<Location>()) as *mut Location;
    if loc.is_null() {
        panic!();
    }
    (*loc).r#type = LOCATION_AUTOMATIC;
    (*loc).u.frame = frame;
    (*loc).block = block;
    if offset.is_null() {
        panic!();
    }
    (*loc).offset = offset;
    return loc;
}

pub unsafe fn location_transfigure(loc: *mut Location, compare: *mut State) -> *mut Value {
    match (*loc).r#type {
        3 | 2 => state_clump(compare),
        4 => state_alloc(compare),
        _ => panic!(),
    }
}

pub unsafe fn location_destroy(loc: *mut Location) {
    ast_expr_destroy((*loc).offset);
    free(loc as *mut libc::c_void);
}

pub unsafe fn location_str(loc: *mut Location) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match (*loc).r#type {
        0 => {
            strbuilder_write!(b, "static:");
        }
        3 => {
            strbuilder_write!(b, "stack[{}]:", (*loc).u.frame);
        }
        4 => {
            strbuilder_write!(b, "heap:");
        }
        2 => {
            strbuilder_write!(b, "clump:");
        }
        _ => panic!(),
    }
    strbuilder_write!(b, "{}", (*loc).block);
    if !offsetzero(loc) {
        let offset: *mut libc::c_char = ast_expr_str(&*(*loc).offset);
        strbuilder_write!(b, "+{}", cstr!(offset));
        free(offset as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

unsafe fn offsetzero(loc: *mut Location) -> bool {
    let zero = ast_expr_constant_create(0 as libc::c_int);
    ast_expr_equal(&*(*loc).offset, &zero)
}

pub unsafe fn location_type(loc: *mut Location) -> LocationType {
    return (*loc).r#type;
}

pub unsafe fn location_block(loc: *mut Location) -> libc::c_int {
    return (*loc).block;
}

pub unsafe fn location_offset(loc: *mut Location) -> *mut AstExpr {
    return (*loc).offset;
}

pub unsafe fn location_copy(loc: *mut Location) -> *mut Location {
    match (*loc).r#type {
        0 => location_create_static((*loc).block, Box::into_raw(ast_expr_copy(&*(*loc).offset))),
        1 => location_create_vconst((*loc).block, Box::into_raw(ast_expr_copy(&*(*loc).offset))),
        2 => location_create_dereferencable(
            (*loc).block,
            Box::into_raw(ast_expr_copy(&*(*loc).offset)),
        ),
        3 => location_create_automatic(
            (*loc).u.frame,
            (*loc).block,
            Box::into_raw(ast_expr_copy(&*(*loc).offset)),
        ),
        4 => location_create_dynamic((*loc).block, Box::into_raw(ast_expr_copy(&*(*loc).offset))),
        _ => panic!(),
    }
}

pub unsafe fn location_with_offset(loc: *mut Location, offset: &AstExpr) -> *mut Location {
    if !offsetzero(loc) {
        panic!();
    }
    let copy: *mut Location = location_copy(loc);
    (*copy).offset = Box::into_raw(ast_expr_copy(offset));
    return copy;
}

pub unsafe fn location_tostatic(loc: *mut Location, sm: *mut StaticMemory) -> bool {
    let type_equal: bool =
        (*loc).r#type as libc::c_uint == LOCATION_STATIC as libc::c_int as libc::c_uint;
    let b: *mut Block = static_memory_getblock(sm, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_toheap(loc: *mut Location, h: *mut Heap) -> bool {
    let type_equal: bool =
        (*loc).r#type as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let b: *mut Block = heap_getblock(h, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_tostack(loc: *mut Location, s: *mut Stack) -> bool {
    let type_equal: bool =
        (*loc).r#type as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
    let b: *mut Block = stack_getblock(s, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_toclump(loc: *mut Location, c: *mut Clump) -> bool {
    let type_equal: bool =
        (*loc).r#type as libc::c_uint == LOCATION_DEREFERENCABLE as libc::c_int as libc::c_uint;
    let b: *mut Block = clump_getblock(c, (*loc).block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn location_equal(l1: *mut Location, l2: *mut Location) -> bool {
    return (*l1).r#type as libc::c_uint == (*l2).r#type as libc::c_uint
        && (*l1).block == (*l2).block
        && ast_expr_equal(&*(*l1).offset, &*(*l2).offset) as libc::c_int != 0;
}

pub unsafe fn location_references(l1: *mut Location, l2: *mut Location, s: *mut State) -> bool {
    if location_equal(l1, l2) {
        return true;
    }
    let b: *mut Block = state_getblock(s, l1);
    return !b.is_null() && block_references(b, l2, s) as libc::c_int != 0;
}

pub unsafe fn location_isauto(loc: *mut Location) -> bool {
    return (*loc).r#type as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
}

pub unsafe fn location_referencesheap(l: *mut Location, s: *mut State) -> bool {
    if (*l).r#type as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        if heap_blockisfreed(state_getheap(s), (*l).block) {
            return false;
        }
        return true;
    }
    let obj = state_get(s, l, false).unwrap();
    !obj.is_null() && object_referencesheap(obj, s)
}

pub unsafe fn location_getblock(
    loc: *mut Location,
    sm: *mut StaticMemory,
    v: *mut VConst,
    s: *mut Stack,
    h: *mut Heap,
    c: *mut Clump,
) -> Result<*mut Block> {
    if s.is_null() {
        panic!();
    }
    match (*loc).r#type {
        0 => Ok(static_memory_getblock(sm, (*loc).block)),
        3 => location_auto_getblock(loc, s),
        4 => Ok(heap_getblock(h, (*loc).block)),
        2 => Ok(clump_getblock(c, (*loc).block)),
        _ => panic!(),
    }
}

unsafe fn location_auto_getblock(loc: *mut Location, s: *mut Stack) -> Result<*mut Block> {
    let f: *mut Stack = stack_getframe(s, (*loc).u.frame);
    if f.is_null() {
        return Err(error_create(
            b"stack frame doesn't exist\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    Ok(stack_getblock(f, (*loc).block))
}

pub unsafe fn location_getstackblock(loc: *mut Location, s: *mut Stack) -> *mut Block {
    if !((*loc).r#type as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    return stack_getblock(s, (*loc).block);
}

pub unsafe fn location_dealloc(loc: *mut Location, heap: *mut Heap) -> Result<()> {
    if (*loc).r#type as libc::c_uint != LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        return Err(error_create(
            b"not heap location\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    heap_deallocblock(heap, (*loc).block)
}

pub unsafe fn location_range_dealloc(
    loc: *mut Location,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    if !offsetzero(loc) {
        panic!();
    }
    let b: *mut Block = state_getblock(state, loc);
    if b.is_null() {
        return Err(error_create(
            b"cannot get block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    if !block_range_aredeallocands(&*b, lw, up, state) {
        printf(
            b"block: %s\n\0" as *const u8 as *const libc::c_char,
            block_str(b),
        );
        printf(
            b"lw: %s, up: %s\n\0" as *const u8 as *const libc::c_char,
            ast_expr_str(&*lw),
            ast_expr_str(&*up),
        );
        debug_assert!(false);
        return Err(error_create(
            b"some values not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    block_range_dealloc(b, lw, up, state)
}
