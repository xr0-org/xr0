use std::fmt::{self, Display, Formatter};

use crate::ast::{ast_expr_constant_create, ast_expr_copy, ast_expr_equal};
use crate::object::object_referencesheap;
use crate::state::block::{block_range_aredeallocands, block_range_dealloc, block_references};
use crate::state::stack::{stack_getblock, stack_getframe};
use crate::state::state::{state_alloc, state_clump, state_get, state_getblock, state_getheap};
use crate::state::{Block, Clump, Heap, Stack, State, StaticMemory, VConst};
use crate::util::{Error, Result};
use crate::{AstExpr, Value};

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

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            LocationKind::Static => write!(f, "static:")?,
            LocationKind::Automatic { frame } => write!(f, "stack[{}]:", *frame)?,
            LocationKind::Dynamic => write!(f, "heap:")?,
            LocationKind::Dereferencable => write!(f, "clump:")?,
            _ => panic!(),
        }
        write!(f, "{}", self.block)?;
        if !offsetzero(self) {
            write!(f, "+{}", self.offset)?;
        }
        Ok(())
    }
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
    let b = (*h).get_block(loc.block);
    type_equal && b.is_some()
}

pub unsafe fn location_tostack(loc: &Location, _s: *mut Stack) -> bool {
    // Note: Original adds a null check that can't fail.
    matches!(loc.kind, LocationKind::Automatic { .. })
}

pub fn location_toclump(loc: &Location, c: &mut Clump) -> bool {
    let type_equal = matches!(loc.kind, LocationKind::Dereferencable);
    let b = c.get_block(loc.block);
    type_equal && b.is_some()
}

fn location_equal(l1: &Location, l2: &Location) -> bool {
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
        if (*state_getheap(s)).block_is_freed(l.block) {
            return false;
        }
        return true;
    }
    let obj = state_get(s, l, false).unwrap();
    !obj.is_null() && object_referencesheap(obj, s)
}

pub unsafe fn location_getblock<'s>(
    loc: &Location,
    sm: &'s mut StaticMemory,
    _v: &'s mut VConst,
    s: &'s mut Stack,
    h: &'s mut Heap,
    c: &'s mut Clump,
) -> Result<Option<&'s mut Block>> {
    match loc.kind {
        LocationKind::Static => Ok(sm.get_block(loc.block)),
        LocationKind::Automatic { .. } => location_auto_getblock(loc, &mut *s).map(Some),
        LocationKind::Dynamic => Ok((*h).get_block(loc.block)),
        LocationKind::Dereferencable => Ok(c.get_block(loc.block)),
        _ => panic!(),
    }
}

pub unsafe fn location_auto_getblock<'stack>(
    loc: &Location,
    s: &'stack mut Stack,
) -> Result<&'stack mut Block> {
    let LocationKind::Automatic { frame } = &loc.kind else {
        panic!();
    };
    let Some(f) = stack_getframe(s, *frame) else {
        return Err(Error::new("stack frame doesn't exist".to_string()));
    };
    Ok(stack_getblock(f, loc.block))
}

pub unsafe fn location_getstackblock<'s>(loc: &Location, s: &'s mut Stack) -> &'s mut Block {
    assert!(matches!(loc.kind, LocationKind::Automatic { .. }));
    stack_getblock(s, loc.block)
}

pub unsafe fn location_dealloc(loc: &Location, heap: *mut Heap) -> Result<()> {
    if !matches!(loc.kind, LocationKind::Dynamic) {
        return Err(Error::new("not heap location".to_string()));
    }
    (*heap).dealloc_block(loc.block)
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
        println!("block: {b}");
        println!("lw: {lw}, up: {up}");
        debug_assert!(false);
        return Err(Error::new("some values not allocated".to_string()));
    }
    block_range_dealloc(b, lw, up, state)
}
