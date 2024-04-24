use std::ptr;

use libc::strcmp;

use super::block::{block_observe, block_range_alloc, block_range_aredeallocands};
use super::clump::{clump_create, clump_destroy, clump_newblock, clump_str};
use super::heap::{
    heap_newblock, heap_referenced, heap_str, heap_undeclare, vconst_create, vconst_declare,
    vconst_eval, vconst_get, vconst_str, vconst_undeclare,
};
use super::location::{
    location_create_dereferencable, location_create_static, location_dealloc, location_getblock,
    location_offset, location_range_dealloc, location_toclump, location_toheap, location_tostack,
    location_tostatic, location_with_offset,
};
use super::stack::{
    stack_copy, stack_copywithname, stack_create, stack_declare, stack_destroy, stack_getresult,
    stack_getvariable, stack_prev, stack_references, stack_str, stack_undeclare, variable_location,
    variable_type,
};
use super::static_memory::{
    static_memory_checkpool, static_memory_newblock, static_memory_str, static_memory_stringpool,
};
use crate::ast::{
    ast_expr_as_literal, ast_expr_constant_create, ast_expr_equal, ast_expr_identifier_create,
    ast_type_vconst,
};
use crate::object::{object_as_value, object_assign};
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::value::{
    value_as_location, value_islocation, value_isstruct, value_issync, value_literal_create,
    value_ptr_create, value_sync_create,
};
use crate::{
    strbuilder_write, vprintln, AstExpr, AstType, AstVariable, Block, Clump, Externals, Heap,
    Location, Object, Props, Stack, StaticMemory, VConst, Value, Variable,
};

pub struct State {
    pub ext: *mut Externals,
    pub vconst: VConst,
    pub static_memory: StaticMemory,
    pub clump: *mut Clump,
    pub stack: *mut Stack,
    pub heap: Heap,
    pub props: Props,
}

pub unsafe fn state_create(
    func: *mut libc::c_char,
    ext: *mut Externals,
    result_type: &AstType,
) -> State {
    State {
        ext,
        static_memory: StaticMemory::new(),
        vconst: vconst_create(),
        clump: clump_create(),
        stack: stack_create(func, ptr::null_mut(), result_type),
        heap: Heap::new(),
        props: Props::new(),
    }
}

pub unsafe fn state_create_withprops(
    func: *mut libc::c_char,
    ext: *mut Externals,
    result_type: &AstType,
    props: Props,
) -> State {
    State {
        ext,
        static_memory: StaticMemory::new(),
        vconst: vconst_create(),
        clump: clump_create(),
        stack: stack_create(func, ptr::null_mut(), result_type),
        heap: Heap::new(),
        props,
    }
}

impl Drop for State {
    fn drop(&mut self) {
        unsafe {
            // Note: The original used a function `static_memory_destroy` which leaked the allocation
            // containing the static_memory value.
            clump_destroy(self.clump);
            stack_destroy(self.stack);
        }
    }
}

pub unsafe fn state_copy(state: &State) -> State {
    State {
        ext: state.ext,
        static_memory: state.static_memory.clone(),
        vconst: state.vconst.clone(),
        clump: Box::into_raw(Box::new((*state.clump).clone())),
        stack: stack_copy(state.stack),
        heap: state.heap.clone(),
        props: state.props.clone(),
    }
}

pub unsafe fn state_copywithname(state: &State, func_name: *mut libc::c_char) -> State {
    State {
        ext: state.ext,
        static_memory: state.static_memory.clone(),
        vconst: state.vconst.clone(),
        clump: Box::into_raw(Box::new((*state.clump).clone())),
        stack: stack_copywithname(state.stack, func_name),
        heap: state.heap.clone(),
        props: state.props.clone(),
    }
}

pub unsafe fn state_str(state: *mut State) -> OwningCStr {
    let mut b = strbuilder_create();
    strbuilder_write!(b, "[[\n");
    let ext = (*(*state).ext).types_str("\t");
    if !ext.is_empty() {
        strbuilder_write!(b, "{ext}\n");
    }
    let static_mem = static_memory_str(&(*state).static_memory, "\t");
    if !static_mem.is_empty() {
        strbuilder_write!(b, "{static_mem}\n");
    }
    let vconst = vconst_str(&(*state).vconst, "\t");
    if !vconst.is_empty() {
        strbuilder_write!(b, "{vconst}\n");
    }
    let clump = clump_str((*state).clump, "\t");
    if !clump.is_empty() {
        strbuilder_write!(b, "{clump}\n");
    }
    let stack = stack_str((*state).stack, state);
    strbuilder_write!(b, "{stack}\n");
    let props = (*state).props.str("\t");
    if !props.is_empty() {
        strbuilder_write!(b, "{props}");
    }
    let heap = heap_str(&mut (*state).heap, "\t");
    if !heap.is_empty() {
        strbuilder_write!(b, "\n{heap}\n");
    }
    strbuilder_write!(b, "]]\n");
    strbuilder_build(b)
}

pub unsafe fn state_getext(s: *mut State) -> *mut Externals {
    (*s).ext
}

pub unsafe fn state_getheap(s: *mut State) -> *mut Heap {
    &mut (*s).heap
}

pub unsafe fn state_getprops(s: &mut State) -> &mut Props {
    &mut s.props
}

pub unsafe fn state_pushframe(state: *mut State, func: *mut libc::c_char, ret_type: &AstType) {
    (*state).stack = stack_create(func, (*state).stack, ret_type);
}

pub unsafe fn state_popframe(state: *mut State) {
    let old: *mut Stack = (*state).stack;
    (*state).stack = stack_prev(old);
    if ((*state).stack).is_null() {
        panic!();
    }
    stack_destroy(old);
}

pub unsafe fn state_declare(state: *mut State, var: &AstVariable, isparam: bool) {
    stack_declare((*state).stack, var, isparam);
}

pub unsafe fn state_vconst(
    state: *mut State,
    t: &AstType,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut Value {
    let v: *mut Value = ast_type_vconst(t, state, comment, persist);
    if value_isstruct(&*v) {
        return v;
    }
    let c = vconst_declare(&mut (*state).vconst, v, comment, persist);
    value_sync_create(ast_expr_identifier_create(c))
}

pub unsafe fn state_static_init(state: *mut State, expr: &AstExpr) -> *mut Value {
    let lit = ast_expr_as_literal(expr);
    let mut loc: *mut Location = static_memory_checkpool(&(*state).static_memory, lit.as_ptr());
    if !loc.is_null() {
        return value_ptr_create(Box::from_raw(loc));
    }
    let address: libc::c_int = static_memory_newblock(&mut (*state).static_memory);
    loc = Box::into_raw(location_create_static(address, ast_expr_constant_create(0)));
    let obj = state_get(state, &*loc, true).unwrap();
    if obj.is_null() {
        panic!();
    }
    object_assign(&mut *obj, value_literal_create(dynamic_str(lit.as_ptr())));
    static_memory_stringpool(&mut (*state).static_memory, lit.as_ptr(), loc);
    value_ptr_create(Box::from_raw(loc))
}

pub unsafe fn state_clump(state: *mut State) -> *mut Value {
    let address: libc::c_int = clump_newblock((*state).clump);
    let loc = Box::into_raw(location_create_dereferencable(
        address,
        ast_expr_constant_create(0),
    ));
    value_ptr_create(Box::from_raw(loc))
}

pub unsafe fn state_islval(state: *mut State, v: *mut Value) -> bool {
    if v.is_null() {
        panic!();
    }
    if !value_islocation(&*v) {
        return false;
    }
    let loc = value_as_location(&*v);
    state_get(state, loc, true).unwrap();
    location_tostatic(loc, &(*state).static_memory)
        || location_toheap(loc, &mut (*state).heap)
        || location_tostack(loc, (*state).stack)
        || location_toclump(loc, (*state).clump)
}

pub unsafe fn state_isalloc(state: *mut State, v: *mut Value) -> bool {
    if v.is_null() {
        panic!();
    }
    if !value_islocation(&*v) {
        return false;
    }
    let loc = value_as_location(&*v);
    state_get(state, loc, true).unwrap();
    location_toheap(loc, &mut (*state).heap)
}

pub unsafe fn state_getvconst(state: *mut State, id: *mut libc::c_char) -> *mut Value {
    vconst_get(&(*state).vconst, id)
}

pub unsafe fn state_get(
    state: *mut State,
    loc: &Location,
    constructive: bool,
) -> Result<*mut Object> {
    let b = location_getblock(
        loc,
        &mut (*state).static_memory,
        &(*state).vconst,
        (*state).stack,
        &mut (*state).heap,
        (*state).clump,
    )?;
    if b.is_null() {
        assert!(loc.type_is_dynamic() || loc.type_is_dereferencable());
        return Ok(ptr::null_mut());
    }
    Ok(block_observe(b, location_offset(loc), state, constructive))
}

pub unsafe fn state_getblock<'s>(state: &'s mut State, loc: &Location) -> Option<&'s mut Block> {
    let p = location_getblock(
        loc,
        &mut state.static_memory,
        &state.vconst,
        state.stack,
        &mut state.heap,
        state.clump,
    )
    .unwrap();
    if p.is_null() {
        None
    } else {
        Some(&mut *p)
    }
}

pub unsafe fn state_getresult(state: *mut State) -> *mut Object {
    let v: *mut Variable = stack_getresult(&*(*state).stack);
    if v.is_null() {
        panic!();
    }
    state_get(state, &*variable_location(v), true).unwrap()
}

unsafe fn state_getresulttype(state: &State) -> &AstType {
    let v: *mut Variable = stack_getresult(&*state.stack);
    if v.is_null() {
        panic!();
    }
    &*variable_type(v)
}

pub unsafe fn state_getobjecttype(state: &State, id: *mut libc::c_char) -> &AstType {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresulttype(state);
    }
    let v: *mut Variable = stack_getvariable(state.stack, id);
    if v.is_null() {
        panic!();
    }
    &*variable_type(v)
}

pub unsafe fn state_getloc(state: *mut State, id: *mut libc::c_char) -> *mut Value {
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    value_ptr_create(Box::from_raw(variable_location(v)))
}

pub unsafe fn state_getobject(state: *mut State, id: *mut libc::c_char) -> *mut Object {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresult(state);
    }
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    state_get(state, &*variable_location(v), true).unwrap()
}

pub unsafe fn state_deref(
    state: *mut State,
    ptr_val: *mut Value,
    index: &AstExpr,
) -> Result<*mut Object> {
    if value_issync(&*ptr_val) {
        return Ok(ptr::null_mut());
    }
    let deref_base = value_as_location(&*ptr_val);
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
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return Err(Error::new("no value".to_string()));
    }
    let deref = value_as_location(&*arr_val);
    let b = location_getblock(
        deref,
        &mut (*state).static_memory,
        &(*state).vconst,
        (*state).stack,
        &mut (*state).heap,
        (*state).clump,
    )
    .unwrap(); // panic rather than propagate the error - this is in the original
    if b.is_null() {
        return Err(Error::new("no block".to_string()));
    }
    if ast_expr_equal(lw, up) {
        panic!();
    }
    block_range_alloc(&mut *b, lw, up, &mut (*state).heap)
}

pub unsafe fn state_alloc(state: *mut State) -> *mut Value {
    value_ptr_create(heap_newblock(&mut (*state).heap))
}

pub unsafe fn state_dealloc(state: *mut State, val: *mut Value) -> Result<()> {
    if !value_islocation(&*val) {
        return Err(Error::new(
            "undefined free of value not pointing at heap".to_string(),
        ));
    }
    location_dealloc(&*value_as_location(&*val), &mut (*state).heap)
}

pub unsafe fn state_range_dealloc(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> Result<()> {
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return Err(Error::new("no value".to_string()));
    }
    let deref = value_as_location(&*arr_val);
    location_range_dealloc(deref, lw, up, state)
}

pub unsafe fn state_addresses_deallocand(state: *mut State, obj: *mut Object) -> bool {
    let val: *mut Value = object_as_value(obj);
    let loc = value_as_location(&*val);
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
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return false;
    }
    let deref = value_as_location(&*arr_val);
    let b = location_getblock(
        deref,
        &mut (*state).static_memory,
        &(*state).vconst,
        (*state).stack,
        &mut (*state).heap,
        (*state).clump,
    )
    .unwrap();
    !b.is_null() && block_range_aredeallocands(&*b, lw, up, state)
}

pub unsafe fn state_hasgarbage(state: *mut State) -> bool {
    !heap_referenced(&mut (*state).heap, state)
}

pub unsafe fn state_references(s: *mut State, loc: &Location) -> bool {
    stack_references((*s).stack, loc, s)
}

pub unsafe fn state_eval(s: &State, e: &AstExpr) -> bool {
    vconst_eval(&s.vconst, e)
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
    heap_undeclare(&mut (*s).heap, s);
    vconst_undeclare(&mut (*s).vconst);
    stack_undeclare((*s).stack, s);
}

unsafe fn state_popprops(s: *mut State) {
    (*s).props = Props::new();
}
