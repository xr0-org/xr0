use std::ptr;
use std::sync::Arc;

use super::location::{
    location_copy, location_create_dereferencable, location_create_static, location_dealloc,
    location_getblock, location_offset, location_range_dealloc, location_toclump, location_toheap,
    location_tostack, location_tostatic, location_with_offset,
};
use super::{Block, Clump, Heap, Stack, StaticMemory, VConst};
use crate::ast::{
    ast_expr_as_literal, ast_expr_constant_create, ast_expr_equal, ast_expr_identifier_create,
    ast_type_vconst,
};
use crate::object::{object_as_value, object_assign};
use crate::util::{Error, Result};
use crate::value::{
    value_as_location, value_islocation, value_isstruct, value_issync, value_literal_create,
    value_ptr_create, value_sync_create,
};
use crate::{
    str_write, vprintln, AstExpr, AstType, AstVariable, Externals, Location, Object, Props, Value,
};

// Note: The original had a destructor `state_destroy` which used a function
// `static_memory_destroy` which leaked the allocation containing the static_memory value.
#[derive(Clone)]
pub struct State {
    pub ext: Arc<Externals>,
    pub vconst: VConst,
    pub static_memory: StaticMemory,
    pub clump: Clump,
    pub stack: Stack,
    pub heap: Heap,
    pub props: Props,
}

pub unsafe fn state_create(func: String, ext: Arc<Externals>, result_type: &AstType) -> State {
    let mut stack = Stack::new();
    stack.push(func, result_type);
    State {
        ext,
        static_memory: StaticMemory::new(),
        vconst: VConst::new(),
        clump: Clump::new(),
        stack,
        heap: Heap::new(),
        props: Props::new(),
    }
}

pub unsafe fn state_create_withprops(
    func: String,
    ext: Arc<Externals>,
    result_type: &AstType,
    props: Props,
) -> State {
    let mut stack = Stack::new();
    stack.push(func, result_type);
    State {
        ext,
        static_memory: StaticMemory::new(),
        vconst: VConst::new(),
        clump: Clump::new(),
        stack,
        heap: Heap::new(),
        props,
    }
}

pub unsafe fn state_copy(state: &State) -> State {
    state.clone()
}

pub unsafe fn state_copywithname(state: &State, func_name: String) -> State {
    State {
        ext: Arc::clone(&state.ext),
        static_memory: state.static_memory.clone(),
        vconst: state.vconst.clone(),
        clump: state.clump.clone(),
        stack: state.stack.clone_with_name(func_name),
        heap: state.heap.clone(),
        props: state.props.clone(),
    }
}

pub unsafe fn state_str(state: *mut State) -> String {
    let mut b = String::new();
    str_write!(b, "[[\n");
    let ext = (*(*state).ext).types_str("\t");
    if !ext.is_empty() {
        str_write!(b, "{ext}\n");
    }
    let static_mem = (*state).static_memory.str("\t");
    if !static_mem.is_empty() {
        str_write!(b, "{static_mem}\n");
    }
    let vconst = (*state).vconst.str("\t");
    if !vconst.is_empty() {
        str_write!(b, "{vconst}\n");
    }
    let clump = &(*state).clump.str("\t");
    if !clump.is_empty() {
        str_write!(b, "{clump}\n");
    }
    let stack = (*state).stack.str(state);
    str_write!(b, "{stack}\n");
    let props = (*state).props.str("\t");
    if !props.is_empty() {
        str_write!(b, "{props}");
    }
    let heap = (*state).heap.str("\t");
    if !heap.is_empty() {
        str_write!(b, "\n{heap}\n");
    }
    str_write!(b, "]]\n");
    b
}

pub unsafe fn state_getext(s: *mut State) -> *const Externals {
    &*(*s).ext as _
}

impl State {
    pub fn externals_arc(&self) -> Arc<Externals> {
        Arc::clone(&self.ext)
    }
}

pub unsafe fn state_getheap(s: *mut State) -> *mut Heap {
    &mut (*s).heap
}

pub unsafe fn state_getprops(s: &mut State) -> &mut Props {
    &mut s.props
}

pub unsafe fn state_pushframe(state: *mut State, func: String, ret_type: &AstType) {
    (*state).stack.push(func, ret_type);
}

pub unsafe fn state_popframe(state: *mut State) {
    (*state).stack.pop_frame();
}

pub unsafe fn state_declare(state: *mut State, var: &AstVariable, isparam: bool) {
    (*state).stack.declare(var, isparam);
}

pub unsafe fn state_vconst(
    state: *mut State,
    t: &AstType,
    comment: Option<&str>,
    persist: bool,
) -> Box<Value> {
    let v = ast_type_vconst(t, state, comment.unwrap_or(""), persist);
    if value_isstruct(&v) {
        return v;
    }
    let c = (*state).vconst.declare(v, comment, persist);
    value_sync_create(ast_expr_identifier_create(c))
}

pub unsafe fn state_static_init(state: *mut State, expr: &AstExpr) -> Box<Value> {
    let lit = ast_expr_as_literal(expr);
    let loc: *mut Location = (*state).static_memory.check_pool(lit);
    if !loc.is_null() {
        // XXX FIXME this is definitely not kosher - can make multiple boxes point to the same
        // Location
        return value_ptr_create(Box::from_raw(loc));
    }
    let address = (*state).static_memory.new_block();
    let loc = location_create_static(address, ast_expr_constant_create(0));
    let obj = state_get(state, &loc, true).unwrap();
    if obj.is_null() {
        panic!();
    }
    object_assign(&mut *obj, Some(value_literal_create(lit)));
    (*state).static_memory.string_pool(lit, &loc);
    value_ptr_create(loc)
}

impl State {
    pub fn clump(&mut self) -> Box<Value> {
        let address = self.clump.new_block();
        let loc = location_create_dereferencable(address, ast_expr_constant_create(0));
        value_ptr_create(loc)
    }
}

pub unsafe fn state_islval(state: *mut State, v: &Value) -> bool {
    if !value_islocation(v) {
        return false;
    }
    let loc = value_as_location(v);
    state_get(state, loc, true).unwrap();
    location_tostatic(loc, &(*state).static_memory)
        || location_toheap(loc, &mut (*state).heap)
        || location_tostack(loc, &mut (*state).stack)
        || location_toclump(loc, &mut (*state).clump)
}

pub unsafe fn state_isalloc(state: *mut State, v: &Value) -> bool {
    if !value_islocation(v) {
        return false;
    }
    let loc = value_as_location(v);
    state_get(state, loc, true).unwrap();
    location_toheap(loc, &mut (*state).heap)
}

pub unsafe fn state_getvconst<'s>(state: &'s State, id: &str) -> Option<&'s Value> {
    state.vconst.get(id)
}

pub unsafe fn state_get(
    state: *mut State,
    loc: &Location,
    constructive: bool,
) -> Result<*mut Object> {
    let b = location_getblock(
        loc,
        &mut (*state).static_memory,
        &mut (*state).vconst,
        &mut (*state).stack,
        &mut (*state).heap,
        &mut (*state).clump,
    )?;
    match b {
        None => {
            assert!(loc.type_is_dynamic() || loc.type_is_dereferencable());
            Ok(ptr::null_mut())
        }
        Some(b) => Ok(b
            .observe(location_offset(loc), state, constructive)
            .map_or(ptr::null_mut(), |r| r as *const Object as *mut Object)),
    }
}

pub unsafe fn state_getblock<'s>(state: &'s mut State, loc: &Location) -> Option<&'s mut Block> {
    location_getblock(
        loc,
        &mut state.static_memory,
        &mut state.vconst,
        &mut state.stack,
        &mut state.heap,
        &mut state.clump,
    )
    .unwrap()
}

pub unsafe fn state_getresult(state: *mut State) -> *mut Object {
    let v = (*state).stack.get_result();
    state_get(state, v.location(), true).unwrap()
}

unsafe fn state_getresulttype(state: &State) -> &AstType {
    state.stack.get_result().type_()
}

pub unsafe fn state_getobjecttype<'s>(state: &'s State, id: &str) -> &'s AstType {
    if id == "return" {
        return state_getresulttype(state);
    }
    let v = state.stack.get_variable(id).unwrap();
    v.type_()
}

pub unsafe fn state_getloc(state: *mut State, id: &str) -> Box<Value> {
    // In the original, this apparently borrows the Location representing the variable's location,
    // but then passes it to value_ptr_create without copying. We copy because I don't see how this
    // isn't a double free otherwise. (Note: in one caller, `call_setupverify`, the Value is always
    // leaked, which partially explains it. The other is `address_eval`, which I don't think is
    // always leaked.)
    let v = (*state).stack.get_variable(id).unwrap();
    value_ptr_create(location_copy(v.location()))
}

pub unsafe fn state_getobject(state: *mut State, id: &str) -> *mut Object {
    if id == "return" {
        return state_getresult(state);
    }
    let v = (*state).stack.get_variable(id).unwrap();
    state_get(state, v.location(), true).unwrap()
}

pub unsafe fn state_deref(
    state: *mut State,
    ptr_val: &Value,
    index: &AstExpr,
) -> Result<*mut Object> {
    if value_issync(ptr_val) {
        return Ok(ptr::null_mut());
    }
    let deref_base = value_as_location(ptr_val);
    // Note: the original leaked this location.
    let deref = location_with_offset(deref_base, index);
    state_get(state, &deref, true)
        .map_err(|err| Error::new(format!("undefined indirection: {}", err.msg)))
}

pub unsafe fn state_range_alloc(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> Result<()> {
    let Some(arr_val) = object_as_value(&*obj) else {
        return Err(Error::new("no value".to_string()));
    };
    let deref = value_as_location(arr_val);
    let b = location_getblock(
        deref,
        &mut (*state).static_memory,
        &mut (*state).vconst,
        &mut (*state).stack,
        &mut (*state).heap,
        &mut (*state).clump,
    )
    .unwrap(); // panic rather than propagate the error - this is in the original
    let Some(b) = b else {
        return Err(Error::new("no block".to_string()));
    };
    if ast_expr_equal(lw, up) {
        panic!();
    }
    // XXX FIXME: b is mutably borrowed from state and now we're going to mutate the heap
    b.range_alloc(lw, up, &mut (*state).heap)
}

impl State {
    pub fn alloc(&mut self) -> Box<Value> {
        value_ptr_create(self.heap.new_block())
    }
}

pub unsafe fn state_dealloc(state: *mut State, val: &Value) -> Result<()> {
    if !value_islocation(val) {
        return Err(Error::new(
            "undefined free of value not pointing at heap".to_string(),
        ));
    }
    location_dealloc(value_as_location(val), &mut (*state).heap)
}

pub unsafe fn state_range_dealloc(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> Result<()> {
    let Some(arr_val) = object_as_value(&*obj) else {
        return Err(Error::new("no value".to_string()));
    };
    let deref = value_as_location(arr_val);
    location_range_dealloc(deref, lw, up, state)
}

pub unsafe fn state_addresses_deallocand(state: *mut State, obj: *mut Object) -> bool {
    // Note: Original doesn't null-check. Might not be necessary.
    let val = object_as_value(&*obj).unwrap();
    let loc = value_as_location(val);
    state_isdeallocand(state, loc)
}

pub unsafe fn state_isdeallocand(s: *mut State, loc: &Location) -> bool {
    let b = state_getblock(&mut *s, loc);
    loc.type_is_dynamic() && b.is_some()
}

pub unsafe fn state_range_aredeallocands(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> bool {
    if ast_expr_equal(lw, up) {
        return true;
    }
    let Some(arr_val) = object_as_value(&*obj) else {
        return false;
    };
    let deref = value_as_location(arr_val);
    let b = location_getblock(
        deref,
        &mut (*state).static_memory,
        &mut (*state).vconst,
        &mut (*state).stack,
        &mut (*state).heap,
        &mut (*state).clump,
    )
    .unwrap();
    match b {
        Some(b) => b.range_aredeallocands(lw, up, state),
        None => false,
    }
}

pub unsafe fn state_hasgarbage(state: *mut State) -> bool {
    !(*state).heap.referenced(state)
}

pub unsafe fn state_references(s: *mut State, loc: &Location) -> bool {
    (*s).stack.references(loc, s)
}

pub fn state_eval(s: &State, e: &AstExpr) -> bool {
    s.vconst.eval(e)
}

pub unsafe fn state_equal(s1: &State, s2: &State) -> bool {
    let mut s1_c = state_copy(s1);
    let mut s2_c = state_copy(s2);
    state_undeclareliterals(&mut s1_c);
    state_undeclareliterals(&mut s2_c);
    state_undeclarevars(&mut s1_c);
    state_undeclarevars(&mut s2_c);
    state_popprops(&mut s1_c);
    state_popprops(&mut s2_c);
    let str1 = state_str(&mut s1_c);
    let str2 = state_str(&mut s2_c);
    let equal = str1 == str2;
    if !equal {
        vprintln!("actual: {str1}");
        vprintln!("abstract: {str2}");
    }
    equal
}

unsafe fn state_undeclareliterals(s: *mut State) {
    (*s).static_memory = StaticMemory::new();
}

unsafe fn state_undeclarevars(s: *mut State) {
    (*s).heap.undeclare(s);
    (*s).vconst.undeclare();
    (*s).stack.undeclare(s);
}

unsafe fn state_popprops(s: *mut State) {
    (*s).props = Props::new();
}
