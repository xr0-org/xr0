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

    pub unsafe fn observe<'b>(
        &'b mut self,
        offset: &AstExpr,
        s: *mut State,
        constructive: bool,
    ) -> Option<&'b Object> {
        let Some(mut index) = object_arr_index(&self.arr, offset, &*s) else {
            if !constructive {
                return None;
            }
            let obj = object_value_create(ast_expr_copy(offset), None);
            let index = self.arr.len();
            self.arr.push(obj);
            return Some(&self.arr[index]);
        };
        let obj = &self.arr[index];

        if object_isvalue(obj) {
            return Some(&self.arr[index]);
        }

        // range around observand at offset
        let lw = ast_expr_copy(offset);
        let up = ast_expr_sum_create(ast_expr_copy(offset), ast_expr_constant_create(1));

        // ordering makes them sequential in heap
        let lw_ptr = Box::into_raw(lw);
        // Note: Original stores `lw` in `upto` but then also destroys `lw` a few lines down. I don't
        // know why it isn't a double free.
        let upto = object_upto(obj, lw_ptr, s);
        let observed = object_value_create(ast_expr_copy(&*lw_ptr), Some((*s).alloc()));
        let from = object_from(obj, &up, s);
        drop(up);
        drop(Box::from_raw(lw_ptr));

        // delete current struct block
        // Note: This is going to mutate self illicitly.
        object_dealloc(obj, s).unwrap();
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
        Some(&self.arr[observed_index])
    }

    pub unsafe fn references(&self, loc: &Location, s: *mut State) -> bool {
        self.arr.iter().any(|obj| object_references(obj, loc, s))
    }

    // XXX FIXME: `self` may be an element of `heap`, verboten aliasing
    pub unsafe fn range_alloc(
        &mut self,
        lw: &AstExpr,
        up: &AstExpr,
        heap: &mut Heap,
    ) -> Result<()> {
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

    pub unsafe fn range_aredeallocands(&self, lw: &AstExpr, up: &AstExpr, s: *mut State) -> bool {
        if self.hack_first_object_is_exactly_bounds(lw, up, s) {
            return true;
        }
        let Some(lw_index) = object_arr_index(&self.arr, lw, &*s) else {
            return false;
        };
        let Some(up_index) = object_arr_index_upperincl(&self.arr, up, &*s) else {
            return false;
        };
        for i in lw_index..up_index {
            if !object_isdeallocand(&self.arr[i], s) {
                return false;
            }
            if !object_contig_precedes(&self.arr[i], &self.arr[i + 1], &*s) {
                return false;
            }
        }
        assert!(object_isdeallocand(&self.arr[up_index], s));
        true
    }

    unsafe fn hack_first_object_is_exactly_bounds(
        &self,
        lw: &AstExpr,
        up: &AstExpr,
        s: *mut State,
    ) -> bool {
        assert!(!self.arr.is_empty());
        let obj = &self.arr[0];
        if !object_isdeallocand(obj, s) {
            return false;
        }
        // Note: Original leaks these outer expressions to avoid double-freeing the inner ones.
        // XXX FIXME: invalid use of Box - make a borrowed expr type
        let same_lw = ast_expr_eq_create(
            Box::from_raw(lw as *const AstExpr as *mut AstExpr),
            Box::from_raw(&*obj.offset as *const AstExpr as *mut AstExpr),
        );
        let same_up = ast_expr_eq_create(
            Box::from_raw(up as *const AstExpr as *mut AstExpr),
            Box::from_raw(object_upper(obj)),
        );
        let result = state_eval(&*s, &same_lw) && state_eval(&*s, &same_up);
        std::mem::forget(same_lw);
        std::mem::forget(same_up);
        result
    }

    pub unsafe fn range_dealloc(
        &mut self,
        lw: &AstExpr,
        up: &AstExpr,
        s: *mut State,
    ) -> Result<()> {
        if self.hack_first_object_is_exactly_bounds(lw, up, s) {
            object_dealloc(&self.arr[0], s)?;
            self.arr.remove(0);
            return Ok(());
        }
        let Some(lw_index) = object_arr_index(&self.arr, lw, &*s) else {
            return Err(Error::new("lower bound not allocated".to_string()));
        };
        let Some(up_index) = object_arr_index_upperincl(&self.arr, up, &*s) else {
            return Err(Error::new("upper bound not allocated".to_string()));
        };

        // Note: Original stores `lw` in `upto` but then the caller presumably also destroys `lw`. I
        // don't know why it isn't a double free.
        #[allow(unused_variables)]
        let upto = object_upto(&self.arr[lw_index], lw as *const AstExpr as *mut AstExpr, s);
        #[allow(unused_variables)]
        let from = object_from(&self.arr[up_index], up, s);

        // Retain `arr[0..lw_index]`, replace the range `arr[lw_index..=up_index]` with `upto` and `from`,
        // then retain `arr[up_index + 1..]`.
        let mut tail = self.arr.split_off(up_index + 1);
        for obj in self.arr.drain(lw_index..=up_index) {
            object_dealloc(&obj, s)?;
        }
        // Note: Original pushes these to `self.arr` instead of `new` so that they are lost when
        // `self.arr` is overwritten with `new`. Bug in original, I'm pretty sure. Interestingly,
        // Rust would have caught this, because the original then uses a pointer to the original
        // array `obj` after using `object_arr_append` which invalidates that pointer. This is an
        // example of how Rust's restrictions on aliasing are actually helpful.
        // if let Some(upto) = upto {
        //     self.arr.push(upto);
        // }
        // if let Some(from) = from {
        //     self.arr.push(from);
        // }
        self.arr.append(&mut tail);
        Ok(())
    }

    pub unsafe fn undeclare(&mut self, s: *mut State) {
        let mut new = vec![];
        for obj in &self.arr {
            if object_referencesheap(obj, s) {
                new.push(object_abstractcopy(obj, s));
            }
        }
        self.arr = new;
    }
}
