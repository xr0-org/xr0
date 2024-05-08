use std::fmt::{self, Display, Formatter};

use super::block::{object_arr_index, object_arr_index_upperincl};
use super::location::{location_copy, location_toheap, LocationKind};
use super::{Block, Clump, Heap, ProgramCounter, Stack, StaticMemory, VConst};
use crate::ast::{ast_expr_copy, AstBlock, AstExpr, AstType, AstVariable, LValue, KEYWORD_RETURN};
use crate::object::{ObjectKind, Range};
use crate::util::{Error, Result, SemiBox};
use crate::{vprint, Externals, Location, Object, Props, Value};

/// The entire state of the abstract machine.
///
/// The state consists of three parts:
/// - The parsed C program.
/// - The abstract state of memory (heap, stack, and so on).
/// - A collection of propositions that we are for the moment assuming to be true.
#[derive(Clone)]
pub struct State<'a> {
    pub ext: &'a Externals,
    pub vconst: VConst,
    pub static_memory: StaticMemory,
    pub clump: Clump,
    pub stack: Stack<'a>,
    pub heap: Heap,
    pub props: Props,
}

impl<'a> Display for State<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "[[")?;
        let ext = self.ext.types_str("\t");
        if !ext.is_empty() {
            writeln!(f, "{ext}")?;
        }
        let static_mem = self.static_memory.str("\t");
        if !static_mem.is_empty() {
            writeln!(f, "{static_mem}")?;
        }
        let vconst = self.vconst.str("\t");
        if !vconst.is_empty() {
            writeln!(f, "{vconst}")?;
        }
        let clump = self.clump.str("\t");
        if !clump.is_empty() {
            writeln!(f, "{clump}")?;
        }
        let stack = self.stack.str(self);
        if !stack.is_empty() {
            writeln!(f, "{stack}")?;
        }
        let props = self.props.str("\t");
        if !props.is_empty() {
            write!(f, "{props}")?;
        }
        let heap = self.heap.str("\t");
        if !heap.is_empty() {
            writeln!(f, "\n{heap}")?;
        }
        writeln!(f, "]]")
    }
}

impl<'a> State<'a> {
    //=state_create
    pub fn new(
        name: String,
        block: &'a AstBlock,
        ret_type: &AstType,
        abstract_: bool,
        ext: &'a Externals,
    ) -> Box<State<'a>> {
        let mut stack = Stack::new();
        stack.push(name, block, ret_type, abstract_);
        Box::new(State {
            ext,
            static_memory: StaticMemory::new(),
            vconst: VConst::new(),
            clump: Clump::new(),
            stack,
            heap: Heap::new(),
            props: Props::new(),
        })
    }

    //=state_create_withprops
    pub fn with_props(
        name: String,
        block: &'a AstBlock,
        ret_type: &'a AstType,
        abstract_: bool,
        ext: &'a Externals,
        props: Props,
    ) -> Box<State<'a>> {
        let mut stack = State::new(name, block, ret_type, abstract_, ext);
        stack.props = props;
        stack
    }

    //=state_copywithname
    pub fn clone_with_name(&self, func_name: String) -> Box<State<'a>> {
        let mut copy = Box::new(self.clone());
        copy.stack = self.stack.clone_with_name(func_name);
        copy
    }

    pub fn ext(&self) -> &'a Externals {
        self.ext
    }

    pub fn props(&mut self) -> &mut Props {
        &mut self.props
    }

    //=state_pushframe
    pub fn push_frame(&mut self, name: String, b: &'a AstBlock, t: &AstType, abstract_: bool) {
        self.stack.push(name, b, t, abstract_);
    }

    //=state_popframe
    pub fn pop_frame(&mut self) {
        self.stack.pop_frame();
    }

    //=state_declare
    pub fn declare(&mut self, var: &AstVariable, isparam: bool) {
        self.stack.declare(var, isparam);
    }

    //=state_vconst
    pub fn vconst(&mut self, t: &AstType, comment: Option<&str>, persist: bool) -> Box<Value> {
        let v = t.vconst(self, comment.unwrap_or(""), persist);
        if v.is_struct() {
            return v;
        }
        let c = self.vconst.declare(v, comment, persist);
        Value::new_sync(AstExpr::new_identifier(c))
    }

    //=state_static_init
    pub fn static_init(&mut self, lit: &str) -> Box<Value> {
        if let Some(loc) = self.static_memory.check_pool(lit) {
            // Note: Original creates a value that points to this existing location. This is a
            // double-free. When the value is destroyed, the location will be destroyed, even though it
            // is still in the static pool.
            return Value::new_ptr(Box::new(loc.clone()));
        }
        let address = self.static_memory.new_block();
        let loc = Location::new_static(address, AstExpr::new_constant(0));
        let obj = self.get_mut(&loc, true).unwrap().unwrap();
        obj.assign(Some(Value::new_literal(lit)));
        self.static_memory.string_pool(lit, &loc);
        Value::new_ptr(loc)
    }

    //=state_clump
    pub fn clump(&mut self) -> Box<Value> {
        // XXX: should type be associated with blocks for type checking when we assign?
        let address = self.clump.new_block();
        let loc = Location::new_dereferencable(address, AstExpr::new_constant(0));
        Value::new_ptr(loc)
    }

    //=state_islval
    pub fn is_lval(&mut self, v: &Value) -> bool {
        if !v.is_location() {
            return false;
        }
        let loc = v.as_location();
        self.get_mut(loc, true).unwrap(); // put object there
        match loc.kind {
            LocationKind::Static => self.static_memory.has_block(loc.block),
            LocationKind::VConst => false,
            LocationKind::Dereferencable => self.clump.get_block_mut(loc.block).is_some(),
            LocationKind::Automatic { .. } => true,
            LocationKind::Dynamic => self.heap.get_block_mut(loc.block).is_some(),
        }
    }

    //=state_isalloc
    pub fn is_alloc(&mut self, v: &Value) -> bool {
        if !v.is_location() {
            return false;
        }
        let loc = v.as_location();
        self.get_mut(loc, true).unwrap(); // put object there
        location_toheap(loc, &mut self.heap)
    }

    //=state_getvconst
    pub fn get_vconst<'s>(&'s self, id: &str) -> Option<&'s Value> {
        self.vconst.get(id)
    }

    //=state_get (non-mut variation)
    pub fn get<'s>(&'s self, loc: &Location) -> Result<Option<&'s Object>> {
        Ok(self
            .get_block(loc)?
            .and_then(|b| b.observe_read_only(&loc.offset, self)))
    }

    /// Gets the object at the given location `loc`.
    ///
    /// On success, this returns a mutable reference to the `Object`. In detail:
    ///
    /// - If `loc` points to a variable, parameter, string literal, or any other existing value
    ///   object (whether it "has a value" or not), this changes nothing and returns a reference to
    ///   the existing object.
    ///
    /// - If `loc` points to some offset within an existing value object, I think this likewise
    ///   returns a reference to that object, although it should probably be an error.
    ///
    /// - If `loc` points at or into a range object--that is, a region of allocated but
    ///   uninitialized memory, with no type, this means `self` is a heap or clump block, as all
    ///   other blocks have a static type. In this case we punch a hole in the range, replacing it
    ///   with up to three `Object`s, representing the memory before, at, and after `offset`,
    ///   respectively. The "before" and "after" objects will be range objects; the new one at
    ///   `offset` will be a value object, but with no value assigned. See text below.
    ///
    /// - If `loc` points to some offset into an existing block, but there is no object at that
    ///   offset, or due to indefiniteness in `loc` it's impossible to tell for sure that it points
    ///   within the bounds of any particular existing object, this returns `Ok(None)`, unless
    ///   `constructive` is `true`. In that case, it creates a new `Object` at that offset, with no
    ///   value. The new object is stored in the block and a reference is returned. See text below.
    ///
    /// - If the block designated by `loc` doesn't exist, this returns `Ok(None)`. (That seems
    ///   suspicious to me; it means we're chasing a dangling pointer which we should not do. I'm
    ///   not sure why it should not be an assertion failure. -jorendorff) If the block refers to a
    ///   stack frame that doesn't exist, this returns an error. (Same. -jorendorff)
    ///
    /// The reason this method creates new `Object`s is so that the caller can set the type and
    /// value of the object, if it's carrying out a write to previously uninitialized heap/clump
    /// memory.
    ///
    /// However, the exact behavior in that case seems sketchy -- it appears to free the entire
    /// block and allocate a new one, which can't be right.
    //=state_get
    pub fn get_mut<'s>(
        &'s mut self,
        loc: &Location,
        constructive: bool,
    ) -> Result<Option<&'s mut Object>> {
        let Some(b) = self.get_block(loc)? else {
            assert!(matches!(
                loc.kind,
                LocationKind::Dynamic | LocationKind::Dereferencable | LocationKind::Static
            ));
            return Ok(None);
        };

        //=block_observe
        let offset = &loc.offset;
        let Some(mut index) = object_arr_index(&b.arr, offset, self) else {
            if !constructive {
                return Ok(None);
            }

            // Rust note: Repeat the block lookup, now with a mut reference. Not in the original.
            // Using non-mut references until now was necessary to convince Rust the above code was
            // safe.
            let b = self.get_block_mut(loc).unwrap().unwrap();
            let obj = Object::with_value(ast_expr_copy(offset), None);
            let index = b.arr.len();
            b.arr.push(obj);
            return Ok(Some(&mut b.arr[index]));
        };

        let obj = &b.arr[index];
        if obj.is_value() {
            let b = self.get_block_mut(loc).unwrap().unwrap();
            return Ok(Some(&mut b.arr[index]));
        }

        // range around observand at offset
        let lw = ast_expr_copy(offset);
        let up = AstExpr::new_sum(ast_expr_copy(offset), AstExpr::new_constant(1));

        // ordering makes them sequential in heap
        //
        // Note: Original stores `lw` in `upto` but then also destroys `lw` a few lines down.
        // `upto` is then appended to `b->arr` where it may be used (although the `lw` part of it
        // has been freed) and will later be destroyed (a clear double free). Undefined behvaior,
        // but the scenario does not happen in the test suite.
        //
        // Rust note: Clone obj to convince Rust that allocating new heap blocks (which
        // requires a mutable reference to State) doesn't invalidate `obj` (which refers to an
        // object owned by `*state`). Not in the original, of course.
        let obj = obj.clone();
        let upto = obj.slice_upto(&lw, self);
        let observed = Object::with_value(lw, Some(self.alloc()));
        let from = obj.slice_from(&up, self);
        drop(up);

        // delete current struct block
        // Note: 99-program/000-matrix.x gets here, so the code is exercised; but it doesn't make
        // sense to free the allocation as a side effect here.
        self.dealloc_object(&obj).unwrap();
        let b = self.get_block_mut(loc).unwrap().unwrap();
        b.arr.remove(index);

        if let Some(upto) = upto {
            b.arr.insert(index, upto);
            index += 1;
        }
        let observed_index = index;
        b.arr.insert(index, observed);
        index += 1;
        if let Some(from) = from {
            b.arr.insert(index, from);
        }
        Ok(Some(&mut b.arr[observed_index]))
    }

    pub fn get_block<'s>(&'s self, loc: &Location) -> Result<Option<&'s Block>> {
        match loc.kind {
            LocationKind::Static => Ok(self.static_memory.get_block(loc.block)),
            LocationKind::Automatic { frame } => {
                let Some(frame) = self.stack.get_frame(frame) else {
                    return Err(Error::new("stack frame doesn't exist".to_string()));
                };
                Ok(Some(frame.get_block(loc.block)))
            }
            LocationKind::Dynamic => Ok(self.heap.get_block(loc.block)),
            LocationKind::Dereferencable => Ok(self.clump.get_block(loc.block)),
            LocationKind::VConst => panic!(),
        }
    }

    pub fn get_block_mut<'s>(&'s mut self, loc: &Location) -> Result<Option<&'s mut Block>> {
        match loc.kind {
            LocationKind::Static => Ok(self.static_memory.get_block_mut(loc.block)),
            LocationKind::Automatic { frame } => {
                let Some(frame) = self.stack.get_frame_mut(frame) else {
                    return Err(Error::new("stack frame doesn't exist".to_string()));
                };
                Ok(Some(frame.get_block_mut(loc.block)))
            }
            LocationKind::Dynamic => Ok(self.heap.get_block_mut(loc.block)),
            LocationKind::Dereferencable => Ok(self.clump.get_block_mut(loc.block)),
            LocationKind::VConst => panic!(),
        }
    }

    //=state_getresult
    pub fn result_mut(&mut self) -> Result<Option<&mut Object>> {
        let result_loc = self.stack.get_result().location().clone();
        self.get_mut(&result_loc, true)
    }

    //=state_getresulttype
    fn result_type(&self) -> &AstType {
        self.stack.get_result().type_()
    }

    pub fn identifier_lvalue<'s>(&'s mut self, id: &str) -> Result<LValue<'s>> {
        let t = SemiBox::Owned(Box::new(self.object_type(id).clone()));
        let obj = self.get_object(id)?;
        Ok(LValue { t, obj })
    }

    //=state_getobjecttype
    pub fn object_type<'s>(&'s self, id: &str) -> &'s AstType {
        if id == KEYWORD_RETURN {
            return self.result_type();
        }
        let v = self.stack.get_variable(id).unwrap();
        v.type_()
    }

    //=state_getloc
    pub fn get_loc(&mut self, id: &str) -> Box<Value> {
        // Note: In the original, this apparently borrows the Location representing the variable's
        // location, but then passes it to value_ptr_create without copying. We copy because I don't
        // see how this isn't a double free otherwise. (In one caller, `call_setupverify`, the Value is
        // always leaked, which partially explains it. The other is `address_eval`, which I don't think
        // is always leaked.)
        let v = self.stack.get_variable(id).unwrap();
        Value::new_ptr(location_copy(v.location()))
    }

    //=state_getobject
    pub fn get_object<'s>(&'s mut self, id: &str) -> Result<Option<&'s mut Object>> {
        if id == KEYWORD_RETURN {
            return self.result_mut();
        }
        let Some(v) = self.stack.get_variable(id) else {
            return Err(Error::new(format!("unknown variable `{id}'")));
        };
        let var_loc = v.location().clone();
        self.get_mut(&var_loc, true)
    }

    //=state_deref
    pub fn deref<'s>(
        &'s mut self,
        ptr_val: &Value,
        index: &AstExpr,
    ) -> Result<Option<&'s mut Object>> {
        if ptr_val.is_sync() {
            return Ok(None);
        }
        let deref_base = ptr_val.as_location();
        // Note: the original leaked this location.
        let deref = deref_base.with_offset(index);
        self.get_mut(&deref, true)
            .map_err(|err| Error::new(format!("undefined indirection: {err}")))
    }

    //=state_alloc
    pub fn alloc(&mut self) -> Box<Value> {
        Value::new_ptr(self.heap.new_block())
    }

    //=state_dealloc
    pub fn dealloc(&mut self, val: &Value) -> Result<()> {
        if !val.is_location() {
            return Err(Error::new(
                "undefined free of value not pointing at heap".to_string(),
            ));
        }
        self.dealloc_location(val.as_location())
    }

    //=location_dealloc
    fn dealloc_location(&mut self, loc: &Location) -> Result<()> {
        if !matches!(loc.kind, LocationKind::Dynamic) {
            return Err(Error::new("not heap location".to_string()));
        }
        self.heap.dealloc_block(loc.block)
    }

    /// Deallocate a block of memory. This function has two modes. If `obj` has a value and the
    /// value is a pointer to a heap allocation, this deallocates that heap allocation. If it has
    /// any other value, it's an error. Otherwise `obj` is a range object and this deallocates that
    /// range. (I wonder if this shouldn't be two different functions.)
    ///
    /// # Panics
    /// If `obj` is an uninitialized variable.
    //=object_dealloc
    pub fn dealloc_object(&mut self, obj: &Object) -> Result<()> {
        // Note: Original doesn't handle the possibility of Value(None) here.
        match &obj.kind {
            ObjectKind::Value(Some(v)) => self.dealloc(v),
            ObjectKind::Value(None) => panic!(),
            ObjectKind::DeallocandRange(range) => self.dealloc_range(range),
        }
    }

    //=range_dealloc
    fn dealloc_range(&mut self, range: &Range) -> Result<()> {
        // Note: The original creates a value that borrows the location from `r`, then leaks the value
        // to avoid double-freeing the location.
        self.dealloc_location(range.loc())
    }

    //=location_range_dealloc
    pub fn dealloc_location_range(
        &mut self,
        loc: &Location,
        lw: &AstExpr,
        up: &AstExpr,
    ) -> Result<()> {
        assert!(loc.offset_is_zero());

        let Some(b) = self.get_block(loc).unwrap() else {
            return Err(Error::new("cannot get block".to_string()));
        };
        if !b.range_aredeallocands(lw, up, self) {
            println!("block: {b}");
            println!("lw: {lw}, up: {up}");
            debug_assert!(false);
            return Err(Error::new("some values not allocated".to_string()));
        }

        self.dealloc_block_range(loc, lw, up)
    }

    //=block_range_dealloc
    pub fn dealloc_block_range(
        &mut self,
        loc: &Location,
        lw: &AstExpr,
        up: &AstExpr,
    ) -> Result<()> {
        let b = self.get_block(loc).unwrap().unwrap();
        if b.hack_first_object_is_exactly_bounds(lw, up, self) {
            // Rust note: Object cloned for Rust's benefit. Not in the original.
            let obj = b.arr[0].clone();
            self.dealloc_object(&obj)?;
            let b = self.get_block_mut(loc).unwrap().unwrap();
            b.arr.remove(0);
            return Ok(());
        }
        let Some(lw_index) = object_arr_index(&b.arr, lw, self) else {
            return Err(Error::new("lower bound not allocated".to_string()));
        };
        let Some(up_index) = object_arr_index_upperincl(&b.arr, up, self) else {
            return Err(Error::new("upper bound not allocated".to_string()));
        };

        // Note: Original stores `lw` in `upto` but then the caller presumably also destroys `lw`.
        // It would be a double free but for a counterbug (read comments below).
        // Rust note: Objects cloned for Rust's benefit. Not in the original.
        let lw_obj = b.arr[lw_index].clone();
        let up_obj = b.arr[up_index].clone();
        #[allow(unused_variables)]
        let upto = lw_obj.slice_upto(lw, self);
        #[allow(unused_variables)]
        let from = up_obj.slice_from(up, self);

        // Retain `arr[0..lw_index]`, replace the range `arr[lw_index..=up_index]` with `upto` and `from`,
        // then retain `arr[up_index + 1..]`.
        // Rust note: Block lookup repeated for Rust's benefit. Not in the original.
        let b = self.get_block_mut(loc).unwrap().unwrap();
        let mut tail = b.arr.split_off(up_index + 1);
        let objects_to_free = b.arr.split_off(lw_index);
        for obj in objects_to_free {
            self.dealloc_object(&obj)?;
        }

        // Note: Original pushes these to `b.arr` instead of `new` so that they are lost and
        // leaked when `b.arr` is overwritten with `new`. Bug in original, I'm pretty sure.
        // Interestingly, Rust would have caught this, because the original then uses a pointer to
        // the original array `obj` after using `object_arr_append` which invalidates that pointer.
        // This is an example of how Rust's restrictions on aliasing can be helpful actually!
        //
        // if let Some(upto) = upto {
        //     b.arr.push(upto);
        // }
        // if let Some(from) = from {
        //     b.arr.push(from);
        // }
        //
        // Note: Original assigns a new array to `b->arr` without freeing the old one, a leak.
        // Rust note: Block lookup repeated for Rust's benefit. Not in the original.
        let b = self.get_block_mut(loc).unwrap().unwrap();
        b.arr.append(&mut tail);
        Ok(())
    }

    pub fn loc_is_deallocand(&self, loc: &Location) -> bool {
        let b = self.get_block(loc).unwrap();
        loc.type_is_dynamic() && b.is_some()
    }

    //=state_hasgarbage
    pub fn has_garbage(&self) -> bool {
        !self.heap.referenced(self)
    }

    //=state_references
    pub fn references(&self, loc: &Location) -> bool {
        self.stack.references(loc, self)
    }

    //=state_eval
    pub fn eval(&self, e: &AstExpr) -> bool {
        self.vconst.eval(e)
    }

    //=state_equal
    pub fn equals(&self, s2: &State) -> bool {
        let mut s1_c = self.clone();
        let mut s2_c = s2.clone();
        s1_c.undeclare_literals();
        s2_c.undeclare_literals();
        s1_c.undeclare_vars();
        s2_c.undeclare_vars();
        s1_c.pop_props();
        s2_c.pop_props();
        let str1 = format!("{s1_c}");
        let str2 = format!("{s2_c}");
        let equal = str1 == str2;
        if !equal {
            vprint!("abstract: {str2}");
            vprint!("actual: {str1}");
        }
        equal
    }

    //=state_undeclareliterals
    fn undeclare_literals(&mut self) {
        self.static_memory = StaticMemory::new();
    }

    //=state_undeclarevars
    fn undeclare_vars(&mut self) {
        Heap::undeclare(self);
        self.vconst.undeclare();
        Stack::undeclare(self);
    }

    //=state_popprops
    fn pop_props(&mut self) {
        self.props = Props::new();
    }

    //=state_atend
    pub fn at_end(&self) -> bool {
        self.stack.at_end()
    }

    //=stack_step
    pub fn step(&mut self) -> Result<()> {
        ProgramCounter::exec(self)
    }

    /// Name of the function currently on top of the stack.
    pub fn fname(&self) -> &str {
        self.stack.top().name()
    }
}
