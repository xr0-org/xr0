use std::fmt::{self, Display, Formatter};

use super::State;
use crate::ast::ast_expr_copy;
use crate::object::Range;
use crate::state::state::state_eval;
use crate::util::Result;
use crate::{AstExpr, Location, Object};

/// A region of memory in the heap, stack, clump, global, etc.
///
/// I'm not sure the C standard has a term that refers to exactly this. Basically a Block is a
/// maximal contiguous chunk that could be copied using `memcpy`: an allocation, in a sense that
/// includes more than just heap allocation.
///
/// Examples: Each global variable gets a `Block`. When verifying a function, XR0 makes a distinct
/// Block for each parameter. When entering a block (in the `AstBlock` sense of a compound
/// statement) a new `Block` is allocated for each local variable declared in that block. Each call
/// to `malloc` allocates a single new `Block` for the whole allocation.
///
/// But I think XR0 also allocates separate Blocks for regions inside a `malloc` allocation and
/// maybe struct fields.
#[derive(Clone)]
pub struct Block {
    pub arr: Vec<Box<Object>>,
}

//=block_str
impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let n = self.arr.len();
        for (i, obj) in self.arr.iter().enumerate() {
            write!(f, "{obj}{}", if i + 1 < n { ", " } else { "" })?;
        }
        Ok(())
    }
}

impl Block {
    //=block_create
    pub fn new() -> Box<Block> {
        Box::new(Block { arr: vec![] })
    }

    //=block_install
    pub fn install(&mut self, obj: Box<Object>) {
        assert!(self.arr.is_empty());
        self.arr.push(obj);
    }

    pub fn observe_read_only<'b>(&'b self, offset: &AstExpr, state: &State) -> Option<&'b Object> {
        let index = object_arr_index(&self.arr, offset, state)?;
        Some(&self.arr[index])
    }

    //=block_references
    pub fn references(&self, loc: &Location, s: &State) -> bool {
        self.arr.iter().any(|obj| obj.references(loc, s))
    }

    /// Fill bytes `lw..up` of this block with a single new range object. `self` must be a
    /// completely uninitialized allocation.
    ///
    //=block_range_alloc
    pub fn range_alloc(
        &mut self,
        lw: &AstExpr,
        up: &AstExpr,
        new_block: Box<Location>,
    ) -> Result<()> {
        // Implementation note: Original took the heap as a parameter. To make Rust happy,
        // the caller must give us the location of a new block instead.
        // XXX: confirm is single object that has never been assigned to
        assert!(self.arr.is_empty());
        self.arr.push(Object::with_range(
            ast_expr_copy(lw),
            Range::new(
                AstExpr::new_difference(ast_expr_copy(up), ast_expr_copy(lw)),
                new_block,
            ),
        ));
        Ok(())
    }

    //=block_range_aredeallocands
    pub fn range_aredeallocands(&self, lw: &AstExpr, up: &AstExpr, s: &State) -> bool {
        if self.hack_first_object_is_exactly_bounds(lw, up, s) {
            return true;
        }
        let Some(lw_index) = object_arr_index(&self.arr, lw, s) else {
            return false;
        };
        let Some(up_index) = object_arr_index_upperincl(&self.arr, up, s) else {
            return false;
        };
        for i in lw_index..up_index {
            if !self.arr[i].is_deallocand(s) {
                return false;
            }
            if !self.arr[i].contig_precedes(&self.arr[i + 1], s) {
                return false;
            }
        }
        assert!(self.arr[up_index].is_deallocand(s));
        true
    }

    pub(crate) fn hack_first_object_is_exactly_bounds(
        &self,
        lw: &AstExpr,
        up: &AstExpr,
        s: &State,
    ) -> bool {
        assert!(!self.arr.is_empty());
        let obj = &self.arr[0];
        if !obj.is_deallocand(s) {
            return false;
        }
        // Note: Original does not do these copies; instead it leaks the outer expressions created
        // here to avoid double-freeing the inner ones.
        let same_lw = AstExpr::new_eq(ast_expr_copy(lw), ast_expr_copy(&obj.offset));
        let same_up = AstExpr::new_eq(ast_expr_copy(up), obj.end());
        state_eval(s, &same_lw) && state_eval(s, &same_up)
    }
}

pub fn object_arr_index(arr: &[Box<Object>], offset: &AstExpr, state: &State) -> Option<usize> {
    for (i, obj) in arr.iter().enumerate() {
        if obj.contains(offset, state) {
            return Some(i);
        }
    }
    None
}

pub fn object_arr_index_upperincl(
    arr: &[Box<Object>],
    offset: &AstExpr,
    state: &State,
) -> Option<usize> {
    for (i, obj) in arr.iter().enumerate() {
        if obj.contains_upperincl(offset, state) {
            return Some(i);
        }
    }
    None
}
