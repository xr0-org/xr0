use std::ffi::CStr;

use libc::{free, malloc, strcmp, strlen};

use super::block::{block_observe, block_range_alloc, block_range_aredeallocands};
use super::clump::{clump_copy, clump_create, clump_destroy, clump_newblock, clump_str};
use super::heap::{
    heap_copy, heap_create, heap_destroy, heap_newblock, heap_referenced, heap_str, heap_undeclare,
    vconst_copy, vconst_create, vconst_declare, vconst_eval, vconst_get, vconst_str,
    vconst_undeclare,
};
use super::location::{
    location_create_dereferencable, location_create_static, location_dealloc, location_getblock,
    location_offset, location_range_dealloc, location_toclump, location_toheap, location_tostack,
    location_tostatic, location_type, location_with_offset, BlockRes, LOCATION_DEREFERENCABLE,
    LOCATION_DYNAMIC,
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
use crate::ext::externals_types_str;
use crate::object::{object_as_value, object_assign};
use crate::props::{props_copy, props_create, props_destroy, props_str};
use crate::util::{
    dynamic_str, error_create, strbuilder_build, strbuilder_create, strbuilder_printf, Error,
};
use crate::value::{
    value_as_location, value_islocation, value_isstruct, value_issync, value_literal_create,
    value_ptr_create, value_sync_create,
};
use crate::{
    vprintln, AstExpr, AstType, AstVariable, Block, Clump, Externals, Heap, Location, Object,
    Props, Stack, StaticMemory, StrBuilder, VConst, Value, Variable,
};

pub struct State {
    pub ext: *mut Externals,
    pub vconst: *mut VConst,
    pub static_memory: *mut StaticMemory,
    pub clump: *mut Clump,
    pub stack: *mut Stack,
    pub heap: *mut Heap,
    pub props: *mut Props,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct ObjectRes {
    pub obj: *mut Object,
    pub err: *mut Error,
}

pub unsafe fn state_create(
    func: *mut libc::c_char,
    ext: *mut Externals,
    result_type: *mut AstType,
) -> *mut State {
    let state: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if state.is_null() {
        panic!();
    }
    (*state).ext = ext;
    (*state).static_memory = static_memory_create();
    (*state).vconst = vconst_create();
    (*state).clump = clump_create();
    (*state).stack = stack_create(func, 0 as *mut Stack, result_type);
    (*state).heap = heap_create();
    (*state).props = props_create();
    return state;
}

pub unsafe fn state_create_withprops(
    func: *mut libc::c_char,
    ext: *mut Externals,
    result_type: *mut AstType,
    props: *mut Props,
) -> *mut State {
    let state: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if state.is_null() {
        panic!();
    }
    (*state).ext = ext;
    (*state).static_memory = static_memory_create();
    (*state).vconst = vconst_create();
    (*state).clump = clump_create();
    (*state).stack = stack_create(func, 0 as *mut Stack, result_type);
    (*state).heap = heap_create();
    (*state).props = props_copy(props);
    return state;
}

pub unsafe fn state_destroy(state: *mut State) {
    static_memory_destroy((*state).static_memory);
    clump_destroy((*state).clump);
    stack_destroy((*state).stack);
    heap_destroy((*state).heap);
    props_destroy((*state).props);
    free(state as *mut libc::c_void);
}

pub unsafe fn state_copy(state: *mut State) -> *mut State {
    let copy: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if copy.is_null() {
        panic!();
    }
    (*copy).ext = (*state).ext;
    (*copy).static_memory = static_memory_copy((*state).static_memory);
    (*copy).vconst = vconst_copy((*state).vconst);
    (*copy).clump = clump_copy((*state).clump);
    (*copy).stack = stack_copy((*state).stack);
    (*copy).heap = heap_copy((*state).heap);
    (*copy).props = props_copy((*state).props);
    return copy;
}

pub unsafe fn state_copywithname(
    state: *mut State,
    func_name: *mut libc::c_char,
) -> *mut State {
    let copy: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if copy.is_null() {
        panic!();
    }
    (*copy).ext = (*state).ext;
    (*copy).static_memory = static_memory_copy((*state).static_memory);
    (*copy).vconst = vconst_copy((*state).vconst);
    (*copy).clump = clump_copy((*state).clump);
    (*copy).stack = stack_copywithname((*state).stack, func_name);
    (*copy).heap = heap_copy((*state).heap);
    (*copy).props = props_copy((*state).props);
    return copy;
}

pub unsafe fn state_str(state: *mut State) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(b, b"[[\n\0" as *const u8 as *const libc::c_char);
    let ext: *mut libc::c_char = externals_types_str(
        (*state).ext,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(ext) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, ext);
    }
    free(ext as *mut libc::c_void);
    let static_mem: *mut libc::c_char = static_memory_str(
        (*state).static_memory,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(static_mem) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, static_mem);
    }
    free(static_mem as *mut libc::c_void);
    let vconst: *mut libc::c_char = vconst_str(
        (*state).vconst,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(vconst) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, vconst);
    }
    free(vconst as *mut libc::c_void);
    let clump: *mut libc::c_char = clump_str(
        (*state).clump,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(clump) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, clump);
    }
    free(clump as *mut libc::c_void);
    let stack: *mut libc::c_char = stack_str((*state).stack, state);
    strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, stack);
    free(stack as *mut libc::c_void);
    let props: *mut libc::c_char = props_str(
        (*state).props,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(props) > 0 {
        strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, props);
    }
    free(props as *mut libc::c_void);
    let heap: *mut libc::c_char = heap_str(
        (*state).heap,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(heap) > 0 {
        strbuilder_printf(b, b"\n%s\n\0" as *const u8 as *const libc::c_char, heap);
    }
    free(heap as *mut libc::c_void);
    strbuilder_printf(b, b"]]\n\0" as *const u8 as *const libc::c_char);
    return strbuilder_build(b);
}

pub unsafe fn state_getext(s: *mut State) -> *mut Externals {
    return (*s).ext;
}

pub unsafe fn state_getheap(s: *mut State) -> *mut Heap {
    return (*s).heap;
}

pub unsafe fn state_getprops(s: *mut State) -> *mut Props {
    return (*s).props;
}

pub unsafe fn state_pushframe(
    state: *mut State,
    func: *mut libc::c_char,
    ret_type: *mut AstType,
) {
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
    if value_isstruct(v) {
        return v;
    }
    let c: *mut libc::c_char = vconst_declare((*state).vconst, v, comment, persist);
    return value_sync_create(ast_expr_identifier_create(c));
}

pub unsafe fn state_static_init(state: *mut State, expr: *mut AstExpr) -> *mut Value {
    let lit: *mut libc::c_char = ast_expr_as_literal(expr);
    let mut loc: *mut Location = static_memory_checkpool((*state).static_memory, lit);
    if !loc.is_null() {
        return value_ptr_create(loc);
    }
    let address: libc::c_int = static_memory_newblock((*state).static_memory);
    loc = location_create_static(address, ast_expr_constant_create(0 as libc::c_int));
    let res: ObjectRes = state_get(state, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        panic!();
    }
    if (res.obj).is_null() {
        panic!();
    }
    object_assign(res.obj, value_literal_create(dynamic_str(lit)));
    static_memory_stringpool((*state).static_memory, lit, loc);
    return value_ptr_create(loc);
}

pub unsafe fn state_clump(state: *mut State) -> *mut Value {
    let address: libc::c_int = clump_newblock((*state).clump);
    let loc: *mut Location =
        location_create_dereferencable(address, ast_expr_constant_create(0 as libc::c_int));
    return value_ptr_create(loc);
}

pub unsafe fn state_islval(state: *mut State, v: *mut Value) -> bool {
    if v.is_null() {
        panic!();
    }
    if !value_islocation(v) {
        return 0 as libc::c_int != 0;
    }
    let loc: *mut Location = value_as_location(v);
    let res: ObjectRes = state_get(state, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        panic!();
    }
    return location_tostatic(loc, (*state).static_memory) as libc::c_int != 0
        || location_toheap(loc, (*state).heap) as libc::c_int != 0
        || location_tostack(loc, (*state).stack) as libc::c_int != 0
        || location_toclump(loc, (*state).clump) as libc::c_int != 0;
}

pub unsafe fn state_isalloc(state: *mut State, v: *mut Value) -> bool {
    if v.is_null() {
        panic!();
    }
    if !value_islocation(v) {
        return 0 as libc::c_int != 0;
    }
    let loc: *mut Location = value_as_location(v);
    let res: ObjectRes = state_get(state, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if true {
            panic!();
        }
    }
    return location_toheap(loc, (*state).heap);
}

pub unsafe fn state_getvconst(state: *mut State, id: *mut libc::c_char) -> *mut Value {
    return vconst_get((*state).vconst, id);
}

pub unsafe fn state_get(
    state: *mut State,
    loc: *mut Location,
    constructive: bool,
) -> ObjectRes {
    let res: BlockRes = location_getblock(
        loc,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        return {
            let init = ObjectRes {
                obj: 0 as *mut Object,
                err: res.err,
            };
            init
        };
    }
    if (res.b).is_null() {
        if !(location_type(loc) as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint
            || location_type(loc) as libc::c_uint
                == LOCATION_DEREFERENCABLE as libc::c_int as libc::c_uint)
        {
            panic!();
        }
        return {
            let init = ObjectRes {
                obj: 0 as *mut Object,
                err: 0 as *mut Error,
            };
            init
        };
    }
    let obj: *mut Object = block_observe(res.b, location_offset(loc), state, constructive);
    return {
        let init = ObjectRes {
            obj,
            err: 0 as *mut Error,
        };
        init
    };
}

pub unsafe fn state_getblock(state: *mut State, loc: *mut Location) -> *mut Block {
    let res: BlockRes = location_getblock(
        loc,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !res.err.is_null() {
        panic!();
    }
    return res.b;
}

pub unsafe fn state_getresult(state: *mut State) -> *mut Object {
    let v: *mut Variable = stack_getresult((*state).stack);
    if v.is_null() {
        panic!();
    }
    let res: ObjectRes = state_get(state, variable_location(v), 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        panic!();
    }
    return res.obj;
}
unsafe fn state_getresulttype(state: *mut State) -> *mut AstType {
    let v: *mut Variable = stack_getresult((*state).stack);
    if v.is_null() {
        panic!();
    }
    return variable_type(v);
}

pub unsafe fn state_getobjecttype(
    state: *mut State,
    id: *mut libc::c_char,
) -> *mut AstType {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresulttype(state);
    }
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    return variable_type(v);
}

pub unsafe fn state_getloc(state: *mut State, id: *mut libc::c_char) -> *mut Value {
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        panic!();
    }
    return value_ptr_create(variable_location(v));
}

pub unsafe fn state_getobject(state: *mut State, id: *mut libc::c_char) -> *mut Object {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresult(state);
    }
    let v: *mut Variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        if true {
            panic!();
        }
    }
    let res: ObjectRes = state_get(state, variable_location(v), 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if true {
            panic!();
        }
    }
    return res.obj;
}

pub unsafe fn state_deref(
    state: *mut State,
    ptr_val: *mut Value,
    index: *mut AstExpr,
) -> ObjectRes {
    if value_issync(ptr_val) {
        return {
            let init = ObjectRes {
                obj: 0 as *mut Object,
                err: 0 as *mut Error,
            };
            init
        };
    }
    let deref_base: *mut Location = value_as_location(ptr_val);
    if deref_base.is_null() {
        panic!();
    }
    let deref: *mut Location = location_with_offset(deref_base, index);
    let res: ObjectRes = state_get(state, deref, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        let b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"undefined indirection: %s\0" as *const u8 as *const libc::c_char,
            (*res.err).msg,
        );
        return {
            let init = ObjectRes {
                obj: 0 as *mut Object,
                err: error_create(strbuilder_build(b)),
            };
            init
        };
    }
    return res;
}

pub unsafe fn state_range_alloc(
    state: *mut State,
    obj: *mut Object,
    lw: *mut AstExpr,
    up: *mut AstExpr,
) -> *mut Error {
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return error_create(
            b"no value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let deref: *mut Location = value_as_location(arr_val);
    let res: BlockRes = location_getblock(
        deref,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        if true {
            panic!();
        }
    }
    if (res.b).is_null() {
        return error_create(
            b"no block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    if ast_expr_equal(lw, up) {
        panic!();
    }
    return block_range_alloc(res.b, lw, up, (*state).heap);
}

pub unsafe fn state_alloc(state: *mut State) -> *mut Value {
    return value_ptr_create(heap_newblock((*state).heap));
}

pub unsafe fn state_dealloc(state: *mut State, val: *mut Value) -> *mut Error {
    if !value_islocation(val) {
        return error_create(
            b"undefined free of value not pointing at heap\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        );
    }
    return location_dealloc(value_as_location(val), (*state).heap);
}

pub unsafe fn state_range_dealloc(
    state: *mut State,
    obj: *mut Object,
    lw: *mut AstExpr,
    up: *mut AstExpr,
) -> *mut Error {
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return error_create(
            b"no value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let deref: *mut Location = value_as_location(arr_val);
    return location_range_dealloc(deref, lw, up, state);
}

pub unsafe fn state_addresses_deallocand(state: *mut State, obj: *mut Object) -> bool {
    let val: *mut Value = object_as_value(obj);
    let loc: *mut Location = value_as_location(val);
    return state_isdeallocand(state, loc);
}

pub unsafe fn state_isdeallocand(s: *mut State, loc: *mut Location) -> bool {
    let type_equal: bool =
        location_type(loc) as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let b: *mut Block = state_getblock(s, loc);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn state_range_aredeallocands(
    state: *mut State,
    obj: *mut Object,
    lw: *mut AstExpr,
    up: *mut AstExpr,
) -> bool {
    if ast_expr_equal(lw, up) {
        return 1 as libc::c_int != 0;
    }
    let arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return 0 as libc::c_int != 0;
    }
    let deref: *mut Location = value_as_location(arr_val);
    let res: BlockRes = location_getblock(
        deref,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        if true {
            panic!();
        }
    }
    return !(res.b).is_null() as libc::c_int != 0
        && block_range_aredeallocands(res.b, lw, up, state) as libc::c_int != 0;
}

pub unsafe fn state_hasgarbage(state: *mut State) -> bool {
    return !heap_referenced((*state).heap, state);
}

pub unsafe fn state_references(s: *mut State, loc: *mut Location) -> bool {
    return stack_references((*s).stack, loc, s);
}

pub unsafe fn state_eval(s: *mut State, e: *mut AstExpr) -> bool {
    return vconst_eval((*s).vconst, e);
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
    return equal;
}
unsafe fn state_undeclareliterals(s: *mut State) {
    static_memory_destroy((*s).static_memory);
    (*s).static_memory = static_memory_create();
}
unsafe fn state_undeclarevars(s: *mut State) {
    heap_undeclare((*s).heap, s);
    vconst_undeclare((*s).vconst);
    stack_undeclare((*s).stack, s);
}
unsafe fn state_popprops(s: *mut State) {
    props_destroy((*s).props);
    (*s).props = props_create();
}
