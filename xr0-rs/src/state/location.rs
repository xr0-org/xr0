use std::ptr;

use crate::ast::{ast_expr_constant_create, ast_expr_copy, ast_expr_equal};
use crate::object::object_referencesheap;
use crate::state::block::{
    block_range_aredeallocands, block_range_dealloc, block_references, block_str,
};
use crate::state::heap::{heap_blockisfreed, heap_deallocblock, heap_getblock};
use crate::state::stack::{stack_getblock, stack_getframe};
use crate::state::state::{state_alloc, state_clump, state_get, state_getblock, state_getheap};
use crate::state::{Block, Clump, Heap, Stack, State, StaticMemory, VConst};
use crate::util::{strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::{strbuilder_write, AstExpr, Value};

#[derive(Clone)]
pub struct Location {
    kind: LocationKind,
    block: libc::c_int,
    offset: Box<AstExpr>,
}

#[derive(Clone, PartialEq)]
enum LocationKind {
    Static,
    #[allow(dead_code)]
    VConst,
    Dereferencable,
    Automatic {
        frame: libc::c_int,
    },
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

#[allow(dead_code)]
pub fn location_create_vconst(block: libc::c_int, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::VConst,
        block,
        offset,
    })
}

pub fn location_create_dereferencable(block: libc::c_int, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Dereferencable,
        block,
        offset,
    })
}

pub fn location_create_static(block: libc::c_int, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Static,
        block,
        offset,
    })
}

pub fn location_create_dynamic(block: libc::c_int, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Dynamic,
        block,
        offset,
    })
}

pub fn location_create_automatic(
    frame: libc::c_int,
    block: libc::c_int,
    offset: Box<AstExpr>,
) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Automatic { frame },
        block,
        offset,
    })
}

pub unsafe fn location_transfigure(loc: &Location, compare: *mut State) -> *mut Value {
    match &loc.kind {
        LocationKind::Automatic { .. } | LocationKind::Dereferencable => {
            Box::into_raw(state_clump(compare))
        }
        LocationKind::Dynamic => Box::into_raw(state_alloc(compare)),
        _ => panic!(),
    }
}

pub unsafe fn location_destroy(loc: *mut Location) {
    drop(Box::from_raw(loc))
}

pub fn location_str(loc: &Location) -> OwningCStr {
    let mut b = strbuilder_create();
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
        strbuilder_write!(b, "+{}", loc.offset);
    }
    strbuilder_build(b)
}

fn offsetzero(loc: &Location) -> bool {
    let zero = ast_expr_constant_create(0);
    ast_expr_equal(&loc.offset, &zero)
}

pub fn location_offset(loc: &Location) -> &AstExpr {
    &loc.offset
}

pub fn location_copy(loc: &Location) -> Box<Location> {
    Box::new(loc.clone())
}

pub fn location_with_offset(loc: &Location, offset: &AstExpr) -> Box<Location> {
    if !offsetzero(loc) {
        panic!();
    }
    let mut copy = location_copy(loc);
    copy.offset = ast_expr_copy(offset);
    copy
}

pub fn location_tostatic(loc: &Location, sm: &StaticMemory) -> bool {
    let type_equal = matches!(loc.kind, LocationKind::Static);
    type_equal && sm.has_block(loc.block)
}

pub unsafe fn location_toheap(loc: &Location, h: *mut Heap) -> bool {
    let type_equal = matches!(loc.kind, LocationKind::Dynamic);
    let b: *mut Block = heap_getblock(h, loc.block);
    type_equal && !b.is_null()
}

pub unsafe fn location_tostack(loc: &Location, s: *mut Stack) -> bool {
    let type_equal = matches!(loc.kind, LocationKind::Automatic { .. });
    let b: *mut Block = stack_getblock(s, loc.block);
    type_equal && !b.is_null()
}

pub fn location_toclump(loc: &Location, c: &mut Clump) -> bool {
    let type_equal = matches!(loc.kind, LocationKind::Dereferencable);
    let b = c.get_block(loc.block);
    type_equal && b.is_some()
}

unsafe fn location_equal(l1: &Location, l2: &Location) -> bool {
    // Note: Original did not compare the `frame` field of automatic locations.
    l1.kind == l2.kind && l1.block == l2.block && ast_expr_equal(&l1.offset, &l2.offset)
}

pub unsafe fn location_references(l1: &Location, l2: &Location, s: *mut State) -> bool {
    if location_equal(l1, l2) {
        return true;
    }
    match state_getblock(&mut *s, l1) {
        None => false,
        Some(b) => block_references(b, l2, s),
    }
}

pub unsafe fn location_referencesheap(l: &Location, s: *mut State) -> bool {
    if matches!(l.kind, LocationKind::Dynamic) {
        if heap_blockisfreed(state_getheap(s), l.block) {
            return false;
        }
        return true;
    }
    let obj = state_get(s, l, false).unwrap();
    !obj.is_null() && object_referencesheap(obj, s)
}

pub unsafe fn location_getblock(
    loc: &Location,
    sm: &mut StaticMemory,
    _v: &VConst,
    s: *mut Stack,
    h: *mut Heap,
    c: &mut Clump,
) -> Result<*mut Block> {
    if s.is_null() {
        panic!();
    }
    match loc.kind {
        LocationKind::Static => {
            let block_ptr = match sm.get_block(loc.block) {
                Some(block) => block,
                None => ptr::null_mut(),
            };
            Ok(block_ptr)
        }
        LocationKind::Automatic { .. } => location_auto_getblock(loc, s),
        LocationKind::Dynamic => Ok(heap_getblock(h, loc.block)),
        LocationKind::Dereferencable => Ok(c
            .get_block(loc.block)
            .map_or(ptr::null_mut(), |blk| blk as *mut Block)),
        _ => panic!(),
    }
}

pub unsafe fn location_auto_getblock(loc: &Location, s: *mut Stack) -> Result<*mut Block> {
    let LocationKind::Automatic { frame } = &loc.kind else {
        panic!();
    };
    let f: *mut Stack = stack_getframe(s, *frame);
    if f.is_null() {
        return Err(Error::new("stack frame doesn't exist".to_string()));
    }
    Ok(stack_getblock(f, loc.block))
}

pub unsafe fn location_getstackblock(loc: &Location, s: *mut Stack) -> *mut Block {
    assert!(matches!(loc.kind, LocationKind::Automatic { .. }));
    stack_getblock(s, loc.block)
}

pub unsafe fn location_dealloc(loc: &Location, heap: *mut Heap) -> Result<()> {
    if !matches!(loc.kind, LocationKind::Dynamic) {
        return Err(Error::new("not heap location".to_string()));
    }
    heap_deallocblock(heap, loc.block)
}

pub unsafe fn location_range_dealloc(
    loc: &Location,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    if !offsetzero(loc) {
        panic!();
    }
    let Some(b) = state_getblock(&mut *state, loc) else {
        return Err(Error::new("cannot get block".to_string()));
    };
    if !block_range_aredeallocands(b, lw, up, state) {
        println!("block: {}", block_str(b));
        println!("lw: {lw}, up: {up}");
        debug_assert!(false);
        return Err(Error::new("some values not allocated".to_string()));
    }
    block_range_dealloc(b, lw, up, state)
}
