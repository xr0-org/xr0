use std::fmt::{self, Display, Formatter};

use super::{Heap, State};
use crate::ast::{
    ast_expr_constant_create, ast_expr_copy, ast_expr_difference_create, ast_expr_eq_create,
    ast_expr_sum_create,
};
use crate::object::{
    object_abstractcopy, object_arr_index, object_arr_index_upperincl, object_contig_precedes,
    object_dealloc, object_from, object_isdeallocand, object_isvalue, object_range_create,
    object_references, object_referencesheap, object_upper, object_upto, object_value_create,
    range_create,
};
use crate::state::state::state_eval;
use crate::util::{Error, Result};
use crate::{AstExpr, Location, Object};

/// A region of memory in the heap, stack, clump, global, etc.
///
/// The C standard calls these "objects". Specifically, a Block is an object that is _not_ contained
/// in some larger object (an array or struct).
///
/// Examples: Each global variable gets a `Block`. When verifying a function, XR0 makes a distinct
/// Block for each parameter. When entering a block (in the `AstBlock` sense of a compound
/// statement) a new `Block` is allocated for each local variable declared in that block. Each call
/// to `malloc` allocates a single new `Block` for the whole allocation.
#[derive(Clone)]
pub struct Block {
    pub arr: Vec<Box<Object>>,
}

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
    pub fn new() -> Box<Block> {
        Box::new(Block { arr: vec![] })
    }

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
    pub unsafe fn observe<'b>(
        &'b mut self,
        offset: &AstExpr,
        s: *mut State,
        constructive: bool,
    ) -> Option<&'b mut Object> {
        let Some(mut index) = object_arr_index(&self.arr, offset, &*s) else {
            if !constructive {
                return None;
            }
            let obj = object_value_create(ast_expr_copy(offset), None);
            let index = self.arr.len();
            self.arr.push(obj);
            return Some(&mut self.arr[index]);
        };
        let obj = &self.arr[index];

        if object_isvalue(obj) {
            return Some(&mut self.arr[index]);
        }

        // range around observand at offset
        let lw = ast_expr_copy(offset);
        let up = ast_expr_sum_create(ast_expr_copy(offset), ast_expr_constant_create(1));

        // ordering makes them sequential in heap
        // Note: Original stores `lw` in `upto` but then also destroys `lw` a few lines down.
        // `upto` is then appended to `b->arr` where it may be used (although the `lw` part of it
        // has been freed) and will later be destroyed (a clear double free). Undefined behvaior,
        // but the scenario does not happen in the test suite.
        let upto = object_upto(obj, &lw, &mut *s);
        let observed = object_value_create(lw, Some((*s).alloc()));
        let from = object_from(obj, &up, &mut *s);
        drop(up);

        // delete current struct block
        // Note: 99-program/000-matrix.x gets here, so the code is exercised; but it doesn't make
        // sense to free the allocation as a side effect here.
        object_dealloc(obj, &mut *s).unwrap();
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

    pub fn references(&self, loc: &Location, s: &mut State) -> bool {
        self.arr.iter().any(|obj| object_references(obj, loc, s))
    }

    // XXX FIXME: `self` may be an element of `heap`, verboten aliasing
    pub fn range_alloc(&mut self, lw: &AstExpr, up: &AstExpr, heap: &mut Heap) -> Result<()> {
        assert!(self.arr.is_empty());
        self.arr.push(object_range_create(
            ast_expr_copy(lw),
            range_create(
                ast_expr_difference_create(ast_expr_copy(up), ast_expr_copy(lw)),
                heap.new_block(),
            ),
        ));
        Ok(())
    }

    pub fn range_aredeallocands(&self, lw: &AstExpr, up: &AstExpr, s: &mut State) -> bool {
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
            if !object_isdeallocand(&self.arr[i], s) {
                return false;
            }
            if !object_contig_precedes(&self.arr[i], &self.arr[i + 1], s) {
                return false;
            }
        }
        assert!(object_isdeallocand(&self.arr[up_index], s));
        true
    }

    fn hack_first_object_is_exactly_bounds(
        &self,
        lw: &AstExpr,
        up: &AstExpr,
        s: &mut State,
    ) -> bool {
        assert!(!self.arr.is_empty());
        let obj = &self.arr[0];
        if !object_isdeallocand(obj, s) {
            return false;
        }
        // Note: Original does not do these copies; instead it leaks the outer expressions created
        // here to avoid double-freeing the inner ones.
        let same_lw = ast_expr_eq_create(ast_expr_copy(lw), ast_expr_copy(&obj.offset));
        let same_up = ast_expr_eq_create(ast_expr_copy(up), object_upper(obj));
        state_eval(s, &same_lw) && state_eval(s, &same_up)
    }

    pub fn range_dealloc(&mut self, lw: &AstExpr, up: &AstExpr, s: &mut State) -> Result<()> {
        if self.hack_first_object_is_exactly_bounds(lw, up, s) {
            object_dealloc(&self.arr[0], s)?;
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
        let upto = object_upto(&self.arr[lw_index], lw, s);
        #[allow(unused_variables)]
        let from = object_from(&self.arr[up_index], up, s);

        // Retain `arr[0..lw_index]`, replace the range `arr[lw_index..=up_index]` with `upto` and `from`,
        // then retain `arr[up_index + 1..]`.
        let mut tail = self.arr.split_off(up_index + 1);
        for obj in self.arr.drain(lw_index..=up_index) {
            object_dealloc(&obj, s)?;
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

    pub fn undeclare(&mut self, s: &mut State) {
        let mut new = vec![];
        for obj in &self.arr {
            if object_referencesheap(obj, s) {
                new.push(object_abstractcopy(obj, s));
            }
        }
        self.arr = new;
    }
}
