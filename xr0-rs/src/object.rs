use std::fmt::{self, Display, Formatter};

use crate::ast::{ast_expr_copy, ast_type_struct_complete, LValue};
use crate::ext::Externals;
use crate::state::location::location_references;
use crate::state::state::state_eval;
use crate::state::State;
use crate::util::SemiBox;
use crate::value::ValueKind;
use crate::{AstExpr, AstType, Location, Value};

/// A span of memory within a block, part of the abstract state of a program. An object can be the
/// whole block, a field of a struct, an element of an array, a span of bytes within a heap
/// allocation that have been written to, etc.
///
/// An object is either "value" or "range". If it's "value" then it either has a Value, or it's
/// uninitialized.
#[derive(Clone)]
pub struct Object {
    pub kind: ObjectKind,
    /// Expression for the offset of this object within the enclosing block.
    pub offset: Box<AstExpr>,
}

#[derive(Clone)]
pub enum ObjectKind {
    DeallocandRange(Box<Range>),
    Value(Option<Box<Value>>),
}

#[derive(Clone)]
pub struct Range {
    loc: Box<Location>,
    size: Box<AstExpr>,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Object { kind, offset } = self;
        write!(f, "{{{offset}:<{kind}>}}")
    }
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ObjectKind::Value(Some(v)) => write!(f, "{v}"),
            ObjectKind::Value(None) => Ok(()),
            ObjectKind::DeallocandRange(range) => write!(f, "{range}"),
        }
    }
}

impl Object {
    pub fn with_value(offset: Box<AstExpr>, v: Option<Box<Value>>) -> Box<Object> {
        Box::new(Object {
            kind: ObjectKind::Value(v),
            offset,
        })
    }

    pub fn with_range(offset: Box<AstExpr>, r: Box<Range>) -> Box<Object> {
        Box::new(Object {
            kind: ObjectKind::DeallocandRange(r),
            offset,
        })
    }

    pub fn is_value(&self) -> bool {
        matches!(self.kind, ObjectKind::Value(_))
    }

    pub fn has_value(&self) -> bool {
        matches!(self.kind, ObjectKind::Value(Some(_)))
    }

    pub fn as_value(&self) -> Option<&Value> {
        let ObjectKind::Value(v) = &self.kind else {
            panic!();
        };
        v.as_deref()
    }

    pub fn as_value_mut(&mut self) -> Option<&mut Value> {
        let ObjectKind::Value(v) = &mut self.kind else {
            panic!();
        };
        v.as_deref_mut()
    }

    pub fn is_deallocand(&self, s: &State) -> bool {
        match &self.kind {
            ObjectKind::Value(None) => false,
            ObjectKind::Value(Some(v)) => s.loc_is_deallocand(v.as_location()),
            ObjectKind::DeallocandRange(range) => range.is_deallocand(s),
        }
    }

    pub fn abstract_copy(&self, s: &State) -> Box<Object> {
        match &self.kind {
            ObjectKind::DeallocandRange(_) => Box::new(self.clone()),
            ObjectKind::Value(v) => Object::with_value(
                self.offset.clone(),
                v.as_ref().and_then(|v| v.abstract_copy(s)),
            ),
        }
    }

    pub fn references_heap(&self, s: &State) -> bool {
        match &self.kind {
            ObjectKind::Value(Some(v)) => v.references_heap(s),
            ObjectKind::Value(None) => false,
            ObjectKind::DeallocandRange(_) => true,
        }
    }

    pub fn references(&self, loc: &Location, s: &State) -> bool {
        match &self.kind {
            ObjectKind::DeallocandRange(range) => range.references(loc, s),
            ObjectKind::Value(None) => false,
            ObjectKind::Value(Some(v)) => v.references(loc, s),
        }
    }

    pub fn assign(&mut self, val: Option<Box<Value>>) {
        let ObjectKind::Value(v) = &mut self.kind else {
            panic!();
        };
        *v = val;
    }

    fn size(&self) -> Box<AstExpr> {
        match &self.kind {
            ObjectKind::Value(_) => AstExpr::new_constant(1),
            ObjectKind::DeallocandRange(range) => ast_expr_copy(&range.size),
        }
    }

    /// Returns an expression for the starting offset of `self` within the enclosing block.
    #[allow(dead_code)]
    pub fn start(&self) -> &AstExpr {
        &self.offset
    }

    /// Returns an expression for the ending offset of `self` within the enclosing block.
    pub fn end(&self) -> Box<AstExpr> {
        AstExpr::new_sum(ast_expr_copy(&self.offset), self.size())
    }

    pub fn contains(&self, offset: &AstExpr, s: &State) -> bool {
        let lw = &self.offset;
        let up = self.end();
        let of = offset;
        let e1 = AstExpr::new_le(ast_expr_copy(lw), ast_expr_copy(of));
        let e2 = AstExpr::new_lt(ast_expr_copy(of), ast_expr_copy(&up));
        state_eval(s, &e1) && state_eval(s, &e2)
    }

    pub fn contains_upperincl(&self, offset: &AstExpr, s: &State) -> bool {
        let lw = &self.offset;
        let up = self.end();
        let of = offset;
        // Note: These copies are not in the original. Original leaks the expressions to avoid
        // double-freeing subexpressions.
        let lower_bound_expr = AstExpr::new_le(ast_expr_copy(lw), ast_expr_copy(of));
        let upper_bound_expr = AstExpr::new_le(ast_expr_copy(of), up);
        state_eval(s, &lower_bound_expr) && state_eval(s, &upper_bound_expr)
    }

    #[allow(dead_code)]
    pub fn is_empty(&self, s: &State) -> bool {
        let lw = &self.offset;
        let up = self.end();
        // Note: Original does not make a copy of `lw`; instead it leaks the expression to avoid
        // double-freeing subexpressions.
        state_eval(s, &AstExpr::new_eq(ast_expr_copy(lw), up))
    }

    pub fn contig_precedes(&self, after: &Object, s: &State) -> bool {
        // Note: Original does not make a copy of `after.offset`; instead it leaks the eq
        // expression to avoid double-freeing subexpressions.
        state_eval(
            s,
            &AstExpr::new_eq(self.end(), ast_expr_copy(&after.offset)),
        )
    }

    #[allow(dead_code)]
    pub fn object_issingular(obj: &Object, s: &State) -> bool {
        let lw = &obj.offset;
        let up = obj.end();
        // Note: Original does not make a copy of `lw`; instead it leaks the expression to avoid
        // double-freeing subexpressions.
        let lw_succ = AstExpr::new_sum(ast_expr_copy(lw), AstExpr::new_constant(1));
        state_eval(s, &AstExpr::new_eq(lw_succ, up))
    }

    /// Returns a new `Object` covering the slice of `self` up to the offset (within the enclosing
    /// Block) given by `excl_up`; or `None` if the slice would be empty.
    ///
    /// # Panics
    ///
    /// If `excl_up` can't be proved to be `>=` the start offset of `self`.
    pub fn slice_upto(&self, excl_up: &AstExpr, s: &mut State) -> Option<Box<Object>> {
        let lw = &self.offset;
        let up = self.end();
        let prop0 = AstExpr::new_le(ast_expr_copy(lw), ast_expr_copy(excl_up));
        let prop1 = AstExpr::new_eq(ast_expr_copy(lw), ast_expr_copy(excl_up));
        let prop2 = AstExpr::new_eq(up, ast_expr_copy(excl_up));
        let e0: bool = state_eval(s, &prop0);
        let e1: bool = state_eval(s, &prop1);
        let e2: bool = state_eval(s, &prop2);
        drop(prop2);
        drop(prop1);
        drop(prop0);
        assert!(e0, "excl_up must be decidably >= the start offset of self");
        if e1 {
            // `excl_up` is equal to this object's lower bound. Nothing to return.
            return None;
        }
        if e2 {
            // `excl_up` is equal to this object's upper bound. Return everything.
            // I'm not sure why this doesn't return `Some(Box::new(self.clone()))`.
            //
            // Note: Original doesn't null-check the value here; objects can be "isvalue" but not
            // "hasvalue".
            let ObjectKind::Value(Some(v)) = &self.kind else {
                panic!();
            };
            return Some(Object::with_value(
                ast_expr_copy(&self.offset),
                Some(Value::copy(v)),
            ));
        }

        // `excl_up` is not decidably equal to the upper or lower bound of `self`. In fact, we did not
        // insist on `excl_up <= up`, so `excl_up` could be past that end.

        // Note: I think there's a double free in the original, where it creates an expression using
        // `lw` without copying it, but `self` owns that expr.
        Some(Object::with_range(
            ast_expr_copy(&self.offset),
            Range::new(
                AstExpr::new_difference(ast_expr_copy(excl_up), ast_expr_copy(lw)),
                s.alloc().into_location(),
            ),
        ))
    }

    /// Returns a new `Object` covering the slice of `self` from `incl_lw` to the end of `self`; or
    /// `None` if the slice would be empty.
    pub fn slice_from(&self, incl_lw: &AstExpr, s: &mut State) -> Option<Box<Object>> {
        let lw = &self.offset;
        let up = self.end();
        let prop0 = AstExpr::new_ge(ast_expr_copy(incl_lw), ast_expr_copy(&up));
        let prop1 = AstExpr::new_eq(ast_expr_copy(incl_lw), ast_expr_copy(lw));
        let e0: bool = state_eval(s, &prop0);
        let e1: bool = state_eval(s, &prop1);
        drop(prop1);
        drop(prop0);
        if e0 {
            return None;
        }
        if e1 {
            // Note: Original doesn't null-check the value here; objects can be "isvalue" but not
            // "hasvalue".
            //
            // I'm not sure why this doesn't return Some(Box::new(self.clone())).
            let ObjectKind::Value(Some(v)) = &self.kind else {
                panic!();
            };
            return Some(Object::with_value(
                ast_expr_copy(incl_lw),
                Some(Value::copy(v)),
            ));
        }
        Some(Object::with_range(
            ast_expr_copy(incl_lw),
            Range::new(
                AstExpr::new_difference(up, ast_expr_copy(incl_lw)),
                s.alloc().into_location(),
            ),
        ))
    }

    /// Returns a field of `self`, which must be a value object. `t` is the effective type of this
    /// access to `obj`. `member` is the name of the field being accessed.
    //=expr_structmember_lvalue (partial)
    pub fn member_lvalue<'s>(
        &'s mut self,
        t: &AstType,
        member: &str,
        ext: &'s Externals,
    ) -> LValue<'s> {
        let val = self.get_or_create_struct(t, ext);
        let ValueKind::Struct(sv) = &mut val.kind else {
            panic!();
        };

        let obj = sv.m.get_mut(member).map(|boxed| &mut **boxed);
        let t = sv
            .members
            .iter()
            .find(|var| member == var.name)
            .map(|var| &var.type_)
            .unwrap();
        LValue {
            t: SemiBox::Borrowed(t),
            obj,
        }
    }

    // Rust note: Original took the whole state as an argument. Narrowed for Rust's benefit.
    //=getorcreatestruct
    fn get_or_create_struct<'obj>(&'obj mut self, t: &AstType, ext: &Externals) -> &'obj mut Value {
        // XXX FIXME: very silly rust construction because of borrow checker limitation
        if self.as_value_mut().is_some() {
            self.as_value_mut().unwrap()
        } else {
            let complete = ast_type_struct_complete(t, ext).unwrap();
            self.assign(Some(Value::new_struct(complete)));
            self.as_value_mut().unwrap()
        }
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Range { size, loc } = self;
        write!(f, "virt:{size}@{loc}")
    }
}

impl Range {
    pub fn new(size: Box<AstExpr>, loc: Box<Location>) -> Box<Self> {
        Box::new(Range { size, loc })
    }

    pub fn loc(&self) -> &Location {
        &self.loc
    }

    fn is_deallocand(&self, s: &State) -> bool {
        s.loc_is_deallocand(&self.loc)
    }

    fn references(&self, loc: &Location, s: &State) -> bool {
        location_references(&self.loc, loc, s)
    }
}
