use std::fmt::{self, Display, Formatter};

use crate::ast::{ast_expr_constant_create, ast_expr_copy, ast_expr_equal};
use crate::object::object_referencesheap;
use crate::state::state::{state_get, state_getheap};
use crate::state::{Block, Clump, Heap, Stack, State, StaticMemory, VConst};
use crate::util::{Error, Result};
use crate::{AstExpr, Value};

#[derive(Clone)]
pub struct Location {
    pub kind: LocationKind,
    pub block: usize,
    pub offset: Box<AstExpr>,
}

#[derive(Clone, PartialEq)]
pub enum LocationKind {
    Static,
    #[allow(dead_code)]
    VConst,
    Dereferencable,
    Automatic {
        frame: usize,
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
pub fn location_create_vconst(block: usize, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::VConst,
        block,
        offset,
    })
}

pub fn location_create_dereferencable(block: usize, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Dereferencable,
        block,
        offset,
    })
}

pub fn location_create_static(block: usize, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Static,
        block,
        offset,
    })
}

pub fn location_create_dynamic(block: usize, offset: Box<AstExpr>) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Dynamic,
        block,
        offset,
    })
}

pub fn location_create_automatic(
    frame: usize,
    block: usize,
    offset: Box<AstExpr>,
) -> Box<Location> {
    Box::new(Location {
        kind: LocationKind::Automatic { frame },
        block,
        offset,
    })
}

pub fn location_transfigure(loc: &Location, compare: &mut State) -> Box<Value> {
    match &loc.kind {
        LocationKind::Automatic { .. } | LocationKind::Dereferencable => (*compare).clump(),
        LocationKind::Dynamic => (*compare).alloc(),
        _ => panic!(),
    }
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
    assert!(offsetzero(loc));
    let mut copy = location_copy(loc);
    copy.offset = ast_expr_copy(offset);
    copy
}

pub fn location_toheap(loc: &Location, h: &mut Heap) -> bool {
    let type_equal = matches!(loc.kind, LocationKind::Dynamic);
    let b = (*h).get_block(loc.block);
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
    match (*s).get_block(l1) {
        None => false,
        Some(b) => b.references(l2, s),
    }
}

pub unsafe fn location_referencesheap(l: &Location, s: *mut State) -> bool {
    if matches!(l.kind, LocationKind::Dynamic) {
        if (*state_getheap(s)).block_is_freed(l.block) {
            return false;
        }
        return true;
    }
    let Some(obj) = state_get(&mut *s, l, false).unwrap() else {
        return false;
    };
    object_referencesheap(&*obj, s)
}

pub fn location_getblock<'s>(
    loc: &Location,
    sm: &'s mut StaticMemory,
    _v: &'s mut VConst,
    s: &'s mut Stack,
    h: &'s mut Heap,
    c: &'s mut Clump,
) -> Result<Option<&'s mut Block>> {
    match loc.kind {
        LocationKind::Static => Ok(sm.get_block(loc.block)),
        LocationKind::Automatic { .. } => location_auto_getblock(loc, s).map(Some),
        LocationKind::Dynamic => Ok((*h).get_block(loc.block)),
        LocationKind::Dereferencable => Ok(c.get_block(loc.block)),
        _ => panic!(),
    }
}

/// Return all three parts of the given location, which must be automatic.
///
/// The parts are (frame_id, block_id, offset).
pub fn location_auto_parts(loc: &Location) -> (usize, usize, &AstExpr) {
    let LocationKind::Automatic { frame } = &loc.kind else {
        panic!();
    };
    (*frame, loc.block, &loc.offset)
}

pub fn location_auto_get_block_id(loc: &Location) -> usize {
    assert!(matches!(loc.kind, LocationKind::Automatic { .. }));
    loc.block
}

pub fn location_auto_getblock<'stack>(
    loc: &Location,
    stack: &'stack mut Stack,
) -> Result<&'stack mut Block> {
    let LocationKind::Automatic { frame: id } = &loc.kind else {
        panic!();
    };
    let Some(frame) = stack.get_frame(*id) else {
        return Err(Error::new("stack frame doesn't exist".to_string()));
    };
    Ok(frame.get_block(loc.block))
}

pub fn location_dealloc(loc: &Location, heap: &mut Heap) -> Result<()> {
    if !matches!(loc.kind, LocationKind::Dynamic) {
        return Err(Error::new("not heap location".to_string()));
    }
    heap.dealloc_block(loc.block)
}

pub unsafe fn location_range_dealloc(
    loc: &Location,
    lw: &AstExpr,
    up: &AstExpr,
    state: *mut State,
) -> Result<()> {
    assert!(offsetzero(loc));
    let Some(b) = (*state).get_block(loc) else {
        return Err(Error::new("cannot get block".to_string()));
    };
    if !b.range_aredeallocands(lw, up, state) {
        println!("block: {b}");
        println!("lw: {lw}, up: {up}");
        debug_assert!(false);
        return Err(Error::new("some values not allocated".to_string()));
    }
    b.range_dealloc(lw, up, state)
}
