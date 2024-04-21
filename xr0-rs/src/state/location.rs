use libc::{free, printf};

use crate::ast::{ast_expr_constant_create, ast_expr_copy, ast_expr_equal, ast_expr_str};
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

pub struct Location {
    kind: LocationKind,
    block: libc::c_int,
    offset: Box<AstExpr>,
}

#[derive(PartialEq)]
enum LocationKind {
    Static,
    VConst,
    Dereferencable,
    Automatic { frame: libc::c_int },
    Dynamic,
}

impl Location {
    pub fn type_is_dynamic(&self) -> bool {
        matches!(self.kind, LocationKind::Dynamic)
    }

    pub fn type_is_dereferencable(&self) -> bool {
        matches!(self.kind, LocationKind::Dereferencable)
    }

    pub fn type_is_vconst(&self) -> bool {
        matches!(self.kind, LocationKind::VConst)
    }
}

pub unsafe fn location_create_vconst(block: libc::c_int, offset: Box<AstExpr>) -> *mut Location {
    Box::into_raw(Box::new(Location {
        kind: LocationKind::VConst,
        block,
        offset,
    }))
}

pub unsafe fn location_create_dereferencable(
    block: libc::c_int,
    offset: Box<AstExpr>,
) -> *mut Location {
    Box::into_raw(Box::new(Location {
        kind: LocationKind::Dereferencable,
        block,
        offset,
    }))
}

pub unsafe fn location_create_static(block: libc::c_int, offset: Box<AstExpr>) -> *mut Location {
    Box::into_raw(Box::new(Location {
        kind: LocationKind::Static,
        block,
        offset,
    }))
}

pub unsafe fn location_create_dynamic(block: libc::c_int, offset: Box<AstExpr>) -> *mut Location {
    Box::into_raw(Box::new(Location {
        kind: LocationKind::Dynamic,
        block,
        offset,
    }))
}

pub unsafe fn location_create_automatic(
    frame: libc::c_int,
    block: libc::c_int,
    offset: Box<AstExpr>,
) -> *mut Location {
    Box::into_raw(Box::new(Location {
        kind: LocationKind::Automatic { frame },
        block,
        offset,
    }))
}

pub unsafe fn location_transfigure(loc: *mut Location, compare: *mut State) -> *mut Value {
    match &(*loc).kind {
        LocationKind::Automatic { .. } | LocationKind::Dereferencable => state_clump(compare),
        LocationKind::Dynamic => state_alloc(compare),
        _ => panic!(),
    }
}

pub unsafe fn location_destroy(loc: *mut Location) {
    drop(Box::from_raw(loc))
}

pub unsafe fn location_str(loc: &Location) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    match &loc.kind {
        LocationKind::Static => {
            strbuilder_write!(b, "static:");
        }
        LocationKind::Automatic { frame } => {
            strbuilder_write!(b, "stack[{}]:", *frame);
        }
        LocationKind::Dynamic => {
            strbuilder_write!(b, "heap:");
        }
        LocationKind::Dereferencable => {
            strbuilder_write!(b, "clump:");
        }
        _ => panic!(),
    }
    strbuilder_write!(b, "{}", loc.block);
    if !offsetzero(loc) {
        let offset: *mut libc::c_char = ast_expr_str(&loc.offset);
        strbuilder_write!(b, "+{}", cstr!(offset));
        free(offset as *mut libc::c_void);
    }
    strbuilder_build(b)
}

unsafe fn offsetzero(loc: &Location) -> bool {
    let zero = ast_expr_constant_create(0);
    ast_expr_equal(&loc.offset, &zero)
}

pub unsafe fn location_offset(loc: &Location) -> &AstExpr {
    &loc.offset
}

pub unsafe fn location_copy(loc: &Location) -> *mut Location {
    match &loc.kind {
        LocationKind::Static => location_create_static(loc.block, ast_expr_copy(&loc.offset)),
        LocationKind::VConst => location_create_vconst(loc.block, ast_expr_copy(&loc.offset)),
        LocationKind::Dereferencable => {
            location_create_dereferencable(loc.block, ast_expr_copy(&loc.offset))
        }
        LocationKind::Automatic { frame } => {
            location_create_automatic(*frame, loc.block, ast_expr_copy(&loc.offset))
        }
        LocationKind::Dynamic => location_create_dynamic(loc.block, ast_expr_copy(&loc.offset)),
    }
}

pub unsafe fn location_with_offset(loc: &Location, offset: &AstExpr) -> *mut Location {
    if !offsetzero(loc) {
        panic!();
    }
    let copy: *mut Location = location_copy(loc);
    (*copy).offset = ast_expr_copy(offset);
    copy
}

pub unsafe fn location_tostatic(loc: *mut Location, sm: *mut StaticMemory) -> bool {
    let type_equal = matches!((*loc).kind, LocationKind::Static);
    let b: *mut Block = static_memory_getblock(sm, (*loc).block);
    type_equal && !b.is_null()
}

pub unsafe fn location_toheap(loc: *mut Location, h: *mut Heap) -> bool {
    let type_equal = matches!((*loc).kind, LocationKind::Dynamic);
    let b: *mut Block = heap_getblock(h, (*loc).block);
    type_equal && !b.is_null()
}

pub unsafe fn location_tostack(loc: *mut Location, s: *mut Stack) -> bool {
    let type_equal = matches!((*loc).kind, LocationKind::Automatic { .. });
    let b: *mut Block = stack_getblock(s, (*loc).block);
    type_equal && !b.is_null()
}

pub unsafe fn location_toclump(loc: *mut Location, c: *mut Clump) -> bool {
    let type_equal = matches!((*loc).kind, LocationKind::Dereferencable);
    let b: *mut Block = clump_getblock(c, (*loc).block);
    type_equal && !b.is_null()
}

unsafe fn location_equal(l1: &Location, l2: &Location) -> bool {
    // Note: Original did not compare the `frame` field of automatic locations.
    l1.kind == l2.kind && l1.block == l2.block && ast_expr_equal(&l1.offset, &l2.offset)
}

pub unsafe fn location_references(l1: *mut Location, l2: *mut Location, s: *mut State) -> bool {
    if location_equal(&*l1, &*l2) {
        return true;
    }
    match state_getblock(&mut *s, &*l1) {
        None => false,
        Some(b) => block_references(b, l2, s),
    }
}

pub unsafe fn location_referencesheap(l: &Location, s: *mut State) -> bool {
    if matches!(l.kind, LocationKind::Dynamic) {
        if heap_blockisfreed(state_getheap(s), (*l).block) {
            return false;
        }
        return true;
    }
    let obj = state_get(s, l, false).unwrap();
    !obj.is_null() && object_referencesheap(obj, s)
}

pub unsafe fn location_getblock(
    loc: &Location,
    sm: *mut StaticMemory,
    v: *mut VConst,
    s: *mut Stack,
    h: *mut Heap,
    c: *mut Clump,
) -> Result<*mut Block> {
    if s.is_null() {
        panic!();
    }
    match loc.kind {
        LocationKind::Static => Ok(static_memory_getblock(sm, loc.block)),
        LocationKind::Automatic { .. } => location_auto_getblock(loc, s),
        LocationKind::Dynamic => Ok(heap_getblock(h, loc.block)),
        LocationKind::Dereferencable => Ok(clump_getblock(c, loc.block)),
        _ => panic!(),
    }
}

unsafe fn location_auto_getblock(loc: &Location, s: *mut Stack) -> Result<*mut Block> {
    let LocationKind::Automatic { frame } = &loc.kind else {
        panic!();
    };
    let f: *mut Stack = stack_getframe(s, *frame);
    if f.is_null() {
        return Err(error_create(
            b"stack frame doesn't exist\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    Ok(stack_getblock(f, loc.block))
}

pub unsafe fn location_getstackblock(loc: *mut Location, s: *mut Stack) -> *mut Block {
    assert!(matches!((*loc).kind, LocationKind::Automatic { .. }));
    stack_getblock(s, (*loc).block)
}

pub unsafe fn location_dealloc(loc: *mut Location, heap: *mut Heap) -> Result<()> {
    if !matches!((*loc).kind, LocationKind::Dynamic) {
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
    if !offsetzero(&*loc) {
        panic!();
    }
    let Some(b) = state_getblock(&mut *state, &*loc) else {
        return Err(error_create(
            b"cannot get block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    };
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
        return Err(error_create(
            b"some values not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    block_range_dealloc(b, lw, up, state)
}
