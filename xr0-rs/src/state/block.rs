use std::fmt::{self, Display, Formatter};

use super::State;
use crate::ast::ast_expr_copy;
use crate::object::Range;
use crate::state::state::state_eval;
use crate::util::{Error, Result};
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

    /// Returns the object at `offset` in this block.
    ///
    /// If there's no object at `offset`, or (due to abstractness) it's impossible to tell for sure
    /// that `offset` is within the bounds of a particular existing object, this returns `None`,
    /// unless `constructive` is `true`. In that case, it creates a new `Object` at that offset,
    /// with no value. The new object is stored in this block and a reference is returned. When
    /// `constructive` is `true`, this method never returns `None`.
    ///
    /// Otherwise, `offset` falls within the bounds of at least one object in this block. If the
    /// first such object is a value object, whether or not it actually has a value, this makes no
    /// changes and returns a reference to that object.
    ///
    /// Otherwise, `offset` is in a range object--that is, a region of allocated but uninitialized
    /// memory, with no effective type. This means `self` is a heap or clump block, as all
    /// other blocks (variables, parameters, statics, temporaries) have a static type. In this case
    /// we punch a hole in the range, replacing it with up to three `Object`s, representing the memory
    /// before, inside, and after `offset`, respectively. The "before" and "after" objects will be
    /// range objects; the new one at `offset` will be a value object, but with no value assigned.
    ///
    /// It's unclear how this copes with vagueness in `offset`. It's also unclear why it does what
    /// it does in the case where it punches a hole in a range object -- it's implemented (I think)
    /// by freeing the entire block `self` and allocating a new block, which can't be right.
    ///
    /// XXX FIXME inherently UB function: mut aliasing: `*s` owns `*self`.
    //=block_observe
    pub fn observe<'b>(
        &'b mut self,
        offset: &AstExpr,
        s: &mut State,
        constructive: bool,
    ) -> Option<&'b mut Object> {
        let Some(mut index) = object_arr_index(&self.arr, offset, s) else {
            if !constructive {
                return None;
            }
            let obj = Object::with_value(ast_expr_copy(offset), None);
            let index = self.arr.len();
            self.arr.push(obj);
            return Some(&mut self.arr[index]);
        };
        let obj = &self.arr[index];

        if obj.is_value() {
            return Some(&mut self.arr[index]);
        }

        // range around observand at offset
        let lw = ast_expr_copy(offset);
        let up = AstExpr::new_sum(ast_expr_copy(offset), AstExpr::new_constant(1));

        // ordering makes them sequential in heap
        // Note: Original stores `lw` in `upto` but then also destroys `lw` a few lines down.
        // `upto` is then appended to `b->arr` where it may be used (although the `lw` part of it
        // has been freed) and will later be destroyed (a clear double free). Undefined behvaior,
        // but the scenario does not happen in the test suite.
        let upto = obj.slice_upto(&lw, s);
        let observed = Object::with_value(lw, Some(s.alloc()));
        let from = obj.slice_from(&up, s);
        drop(up);

        // delete current struct block
        // Note: 99-program/000-matrix.x gets here, so the code is exercised; but it doesn't make
        // sense to free the allocation as a side effect here.
        obj.dealloc(s).unwrap();
        self.arr.remove(index);

        if let Some(upto) = upto {
            self.arr.insert(index, upto);
            index += 1;
        }
        let observed_index = index;
        self.arr.insert(index, observed);
        index += 1;
        if let Some(from) = from {
            self.arr.insert(index, from);
        }
        Some(&mut self.arr[observed_index])
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

    fn hack_first_object_is_exactly_bounds(&self, lw: &AstExpr, up: &AstExpr, s: &State) -> bool {
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

    //=block_range_dealloc
    // XXX FIXME: Inherently UB function: mut aliasing: `*s` contains `*self`.
    pub fn range_dealloc(&mut self, lw: &AstExpr, up: &AstExpr, s: &mut State) -> Result<()> {
        if self.hack_first_object_is_exactly_bounds(lw, up, s) {
            self.arr[0].dealloc(s)?;
            self.arr.remove(0);
            return Ok(());
        }
        let Some(lw_index) = object_arr_index(&self.arr, lw, s) else {
            return Err(Error::new("lower bound not allocated".to_string()));
        };
        let Some(up_index) = object_arr_index_upperincl(&self.arr, up, s) else {
            return Err(Error::new("upper bound not allocated".to_string()));
        };

        // Note: Original stores `lw` in `upto` but then the caller presumably also destroys `lw`.
        // It would be a double free but for a counterbug (read comments below).
        #[allow(unused_variables)]
        let upto = self.arr[lw_index].slice_upto(lw, s);
        #[allow(unused_variables)]
        let from = self.arr[up_index].slice_from(up, s);

        // Retain `arr[0..lw_index]`, replace the range `arr[lw_index..=up_index]` with `upto` and `from`,
        // then retain `arr[up_index + 1..]`.
        let mut tail = self.arr.split_off(up_index + 1);
        for obj in self.arr.drain(lw_index..=up_index) {
            obj.dealloc(s)?;
        }
        // Note: Original pushes these to `self.arr` instead of `new` so that they are lost and
        // leaked when `self.arr` is overwritten with `new`. Bug in original, I'm pretty sure.
        // Interestingly, Rust would have caught this, because the original then uses a pointer to
        // the original array `obj` after using `object_arr_append` which invalidates that pointer.
        // This is an example of how Rust's restrictions on aliasing are actually helpful.
        //
        // if let Some(upto) = upto {
        //     self.arr.push(upto);
        // }
        // if let Some(from) = from {
        //     self.arr.push(from);
        // }
        //
        // Note: Original assigns a new array to `b->arr` without freeing the old one, a leak.
        self.arr.append(&mut tail);
        Ok(())
    }

    //=block_undeclare
    pub fn undeclare(&mut self, s: &mut State) {
        let mut new = vec![];
        for obj in &self.arr {
            if obj.references_heap(s) {
                new.push(obj.abstract_copy(s));
            }
        }
        self.arr = new;
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
