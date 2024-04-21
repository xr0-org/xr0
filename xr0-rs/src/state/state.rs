use std::ffi::CStr;
use std::ptr;

use libc::{free, strcmp, strlen};

use super::block::{block_observe, block_range_alloc, block_range_aredeallocands};
use super::clump::{clump_copy, clump_create, clump_destroy, clump_newblock, clump_str};
use super::heap::{
    heap_copy, heap_newblock, heap_referenced, heap_str, heap_undeclare, vconst_copy,
    vconst_create, vconst_declare, vconst_eval, vconst_get, vconst_str, vconst_undeclare,
};
use super::location::{
    location_create_dereferencable, location_create_static, location_dealloc, location_getblock,
    location_offset, location_range_dealloc, location_toclump, location_toheap, location_tostack,
    location_tostatic, location_with_offset,
};
use super::r#static::{
    static_memory_checkpool, static_memory_copy, static_memory_create, static_memory_destroy,
    static_memory_newblock, static_memory_str, static_memory_stringpool,
};
use super::stack::{
    stack_copy, stack_copywithname, stack_create, stack_declare, stack_destroy, stack_getresult,
    stack_getvariable, stack_prev, stack_references, stack_str, stack_undeclare, variable_location,
    variable_type,
};
use crate::ast::{
    ast_expr_as_literal, ast_expr_constant_create, ast_expr_equal, ast_expr_identifier_create,
    ast_type_vconst,
};
use crate::object::{object_as_value, object_assign};
use crate::props::{props_copy, props_create, props_destroy, props_str};
use crate::util::{dynamic_str, error_create, strbuilder_build, strbuilder_create, Result};
use crate::value::{
    value_as_location, value_islocation, value_isstruct, value_issync, value_literal_create,
    value_ptr_create, value_sync_create,
};
use crate::{
    cstr, strbuilder_write, vprintln, AstExpr, AstType, AstVariable, Block, Clump, Externals, Heap,
    Location, Object, Props, Stack, StaticMemory, StrBuilder, VConst, Value, Variable,
};

pub struct State {
    pub ext: *mut Externals,
    pub vconst: *mut VConst,
    pub static_memory: *mut StaticMemory,
    pub clump: *mut Clump,
    pub stack: *mut Stack,
    pub heap: Heap,
    pub props: *mut Props,
}

pub unsafe fn state_create(
    func: *mut libc::c_char,
    ext: *mut Externals,
    result_type: *mut AstType,
) -> *mut State {
    Box::into_raw(Box::new(State {
        ext,
        static_memory: static_memory_create(),
        vconst: vconst_create(),
        clump: clump_create(),
        stack: stack_create(func, ptr::null_mut(), result_type),
        heap: Heap::new(),
        props: props_create(),
    }))
}

pub unsafe fn state_create_withprops(
    func: *mut libc::c_char,
    ext: *mut Externals,
    result_type: *mut AstType,
    props: *mut Props,
) -> *mut State {
    Box::into_raw(Box::new(State {
        ext,
        static_memory: static_memory_create(),
        vconst: vconst_create(),
        clump: clump_create(),
        stack: stack_create(func, ptr::null_mut(), result_type),
        heap: Heap::new(),
        props: props_copy(props),
    }))
}

pub unsafe fn state_destroy(state: *mut State) {
    drop(Box::from_raw(state));
}

impl Drop for State {
    fn drop(&mut self) {
        unsafe {
            static_memory_destroy(self.static_memory);
            clump_destroy(self.clump);
            stack_destroy(self.stack);
            props_destroy(self.props);
        }
    }
}

pub unsafe fn state_copy(state: *mut State) -> *mut State {
    Box::into_raw(Box::new(State {
        ext: (*state).ext,
        static_memory: static_memory_copy((*state).static_memory),
        vconst: vconst_copy((*state).vconst),
        clump: clump_copy((*state).clump),
        stack: stack_copy((*state).stack),
        heap: heap_copy(&(*state).heap),
        props: props_copy((*state).props),
    }))
}

pub unsafe fn state_copywithname(state: *mut State, func_name: *mut libc::c_char) -> *mut State {
    Box::into_raw(Box::new(State {
        ext: (*state).ext,
        static_memory: static_memory_copy((*state).static_memory),
        vconst: vconst_copy((*state).vconst),
        clump: clump_copy((*state).clump),
        stack: stack_copywithname((*state).stack, func_name),
        heap: heap_copy(&(*state).heap),
        props: props_copy((*state).props),
    }))
}

pub unsafe fn state_str(state: *mut State) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_write!(b, "[[\n");
    let ext: *mut libc::c_char =
        (*(*state).ext).types_str(b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char);
    if strlen(ext) > 0 {
        strbuilder_write!(b, "{}\n", cstr!(ext));
    }
    free(ext as *mut libc::c_void);
    let static_mem: *mut libc::c_char = static_memory_str(
        (*state).static_memory,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(static_mem) > 0 {
        strbuilder_write!(b, "{}\n", cstr!(static_mem));
    }
    free(static_mem as *mut libc::c_void);
    let vconst: *mut libc::c_char = vconst_str(
        (*state).vconst,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(vconst) > 0 {
        strbuilder_write!(b, "{}\n", cstr!(vconst));
    }
    free(vconst as *mut libc::c_void);
    let clump: *mut libc::c_char = clump_str(
        (*state).clump,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(clump) > 0 {
        strbuilder_write!(b, "{}\n", cstr!(clump));
    }
    free(clump as *mut libc::c_void);
    let stack: *mut libc::c_char = stack_str((*state).stack, state);
    strbuilder_write!(b, "{}\n", cstr!(stack));
    free(stack as *mut libc::c_void);
    let props: *mut libc::c_char = props_str(
        (*state).props,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(props) > 0 {
        strbuilder_write!(b, "{}", cstr!(props));
    }
    free(props as *mut libc::c_void);
    let heap: *mut libc::c_char = heap_str(
        &mut (*state).heap,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(heap) > 0 {
        strbuilder_write!(b, "\n{}\n", cstr!(heap));
    }
    free(heap as *mut libc::c_void);
    strbuilder_write!(b, "]]\n");
    strbuilder_build(b)
}

pub unsafe fn state_getext(s: *mut State) -> *mut Externals {
    (*s).ext
}

pub unsafe fn state_getheap(s: *mut State) -> *mut Heap {
    &mut (*s).heap
}

pub unsafe fn state_getprops(s: *mut State) -> *mut Props {
    (*s).props
}

pub unsafe fn state_pushframe(state: *mut State, func: *mut libc::c_char, ret_type: *mut AstType) {
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

pub unsafe fn state_declare(state: *mut State, var: *mut AstVariable, isparam: bool) {
    stack_declare((*state).stack, var, isparam);
}

pub unsafe fn state_vconst(
    state: *mut State,
    t: *mut AstType,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut Value {
    let v: *mut Value = ast_type_vconst(t, state, comment, persist);
    if value_isstruct(&*v) {
        return v;
    }
    let c: *mut libc::c_char = vconst_declare((*state).vconst, v, comment, persist);
    value_sync_create(Box::into_raw(ast_expr_identifier_create(c)))
}

pub unsafe fn state_static_init(state: *mut State, expr: &AstExpr) -> *mut Value {
    let lit: *mut libc::c_char = ast_expr_as_literal(expr);
    let mut loc: *mut Location = static_memory_checkpool((*state).static_memory, lit);
    if !loc.is_null() {
        return value_ptr_create(loc);
    }
    let address: libc::c_int = static_memory_newblock((*state).static_memory);
    loc = location_create_static(address, ast_expr_constant_create(0));
    let obj = state_get(state, loc, true).unwrap();
    if obj.is_null() {
        panic!();
    }
    object_assign(obj, value_literal_create(dynamic_str(lit)));
    static_memory_stringpool((*state).static_memory, lit, loc);
    value_ptr_create(loc)
}

pub unsafe fn state_clump(state: *mut State) -> *mut Value {
    let address: libc::c_int = clump_newblock((*state).clump);
    let loc: *mut Location = location_create_dereferencable(address, ast_expr_constant_create(0));
    value_ptr_create(loc)
}

pub unsafe fn state_islval(state: *mut State, v: *mut Value) -> bool {
    if v.is_null() {
        panic!();
    }
    if !value_islocation(&*v) {
        return false;
    }
    let loc: *mut Location = value_as_location(&*v);
    state_get(state, loc, true).unwrap();
    location_tostatic(loc, (*state).static_memory)
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
    let loc: *mut Location = value_as_location(&*v);
    state_get(state, loc, true).unwrap();
    location_toheap(loc, &mut (*state).heap)
}

pub unsafe fn state_getvconst(state: *mut State, id: *mut libc::c_char) -> *mut Value {
    vconst_get((*state).vconst, id)
}

pub unsafe fn state_get(
    state: *mut State,
    loc: *mut Location,
    constructive: bool,
) -> Result<*mut Object> {
    let b = location_getblock(
        &*loc,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        &mut (*state).heap,
        (*state).clump,
    )?;
    if b.is_null() {
        assert!((*loc).type_is_dynamic() || (*loc).type_is_dereferencable());
        return Ok(ptr::null_mut());
    }
    Ok(block_observe(
        b,
        location_offset(&*loc),
        state,
        constructive,
    ))
}

pub unsafe fn state_getblock<'s>(state: &'s mut State, loc: &Location) -> Option<&'s mut Block> {
    let p = location_getblock(
        loc,
        state.static_memory,
        state.vconst,
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
    let v: *mut Variable = stack_getresult((*state).stack);
    if v.is_null() {
        panic!();
    }
    state_get(state, variable_location(v), true).unwrap()
}

unsafe fn state_getresulttype(state: *mut State) -> *mut AstType {
    let v: *mut Variable = stack_getresult((*state).stack);
    if v.is_null() {
        panic!();
    }
    variable_type(v)
}

pub unsafe fn state_getobjecttype(state: *mut State, id: *mut libc::c_char) -> *mut AstType {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresulttype(state);
    }
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    variable_type(v)
}

pub unsafe fn state_getloc(state: *mut State, id: *mut libc::c_char) -> *mut Value {
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    value_ptr_create(variable_location(v))
}

pub unsafe fn state_getobject(state: *mut State, id: *mut libc::c_char) -> *mut Object {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresult(state);
    }
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    state_get(state, variable_location(v), true).unwrap()
}

pub unsafe fn state_deref(
    state: *mut State,
    ptr_val: *mut Value,
    index: &AstExpr,
) -> Result<*mut Object> {
    if value_issync(&*ptr_val) {
        return Ok(ptr::null_mut());
    }
    let deref_base: *mut Location = value_as_location(&*ptr_val);
    if deref_base.is_null() {
        panic!();
    }
    let deref: *mut Location = location_with_offset(&*deref_base, index);
    state_get(state, deref, true).map_err(|err| {
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_write!(b, "undefined indirection: {}", cstr!(err.msg));
        error_create(strbuilder_build(b))
    })
}

pub unsafe fn state_range_alloc(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> Result<()> {
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return Err(error_create(
            b"no value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    let deref: *mut Location = value_as_location(&*arr_val);
    let b = location_getblock(
        &*deref,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        &mut (*state).heap,
        (*state).clump,
    )
    .unwrap(); // panic rather than propagate the error - this is in the original
    if b.is_null() {
        return Err(error_create(
            b"no block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
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
        return Err(error_create(
            b"undefined free of value not pointing at heap\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        ));
    }
    location_dealloc(value_as_location(&*val), &mut (*state).heap)
}

pub unsafe fn state_range_dealloc(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> Result<()> {
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return Err(error_create(
            b"no value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
    }
    let deref: *mut Location = value_as_location(&*arr_val);
    location_range_dealloc(deref, lw, up, state)
}

pub unsafe fn state_addresses_deallocand(state: *mut State, obj: *mut Object) -> bool {
    let val: *mut Value = object_as_value(obj);
    let loc: *mut Location = value_as_location(&*val);
    state_isdeallocand(state, loc)
}

pub unsafe fn state_isdeallocand(s: *mut State, loc: *mut Location) -> bool {
    let b = state_getblock(&mut *s, &*loc);
    (*loc).type_is_dynamic() && b.is_some()
}

pub unsafe fn state_range_aredeallocands(
    state: *mut State,
    obj: *mut Object,
    lw: &AstExpr,
    up: &AstExpr,
) -> bool {
    if ast_expr_equal(&*lw, &*up) {
        return true;
    }
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return false;
    }
    let deref: *mut Location = value_as_location(&*arr_val);
    let b = location_getblock(
        &*deref,
        (*state).static_memory,
        (*state).vconst,
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

pub unsafe fn state_references(s: *mut State, loc: *mut Location) -> bool {
    stack_references((*s).stack, loc, s)
}

pub unsafe fn state_eval(s: *mut State, e: &AstExpr) -> bool {
    vconst_eval((*s).vconst, e)
}

pub unsafe fn state_equal(s1: *mut State, s2: *mut State) -> bool {
    let s1_c: *mut State = state_copy(s1);
    let s2_c: *mut State = state_copy(s2);
    state_undeclareliterals(s1_c);
    state_undeclareliterals(s2_c);
    state_undeclarevars(s1_c);
    state_undeclarevars(s2_c);
    state_popprops(s1_c);
    state_popprops(s2_c);
    let str1: *mut libc::c_char = state_str(s1_c);
    let str2: *mut libc::c_char = state_str(s2_c);
    let equal: bool = strcmp(str1, str2) == 0 as libc::c_int;
    if !equal {
        vprintln!("actual: {}", CStr::from_ptr(str1).to_string_lossy());
        vprintln!("abstract: {}", CStr::from_ptr(str2).to_string_lossy());
    }
    free(str2 as *mut libc::c_void);
    free(str1 as *mut libc::c_void);
    state_destroy(s2_c);
    state_destroy(s1_c);
    equal
}

unsafe fn state_undeclareliterals(s: *mut State) {
    static_memory_destroy((*s).static_memory);
    (*s).static_memory = static_memory_create();
}

unsafe fn state_undeclarevars(s: *mut State) {
    heap_undeclare(&mut (*s).heap, s);
    vconst_undeclare((*s).vconst);
    stack_undeclare((*s).stack, s);
}

unsafe fn state_popprops(s: *mut State) {
    props_destroy((*s).props);
    (*s).props = props_create();
}
