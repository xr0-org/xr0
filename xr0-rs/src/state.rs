#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

pub mod block;
pub mod clump;
pub mod heap;
pub mod location;
pub mod stack;
pub mod r#static;

use libc::{free, malloc, strcmp, strlen};

use crate::util::{error, v_printf};
use crate::{
    ast_type, ast_variable, static_memory, vconst, AstExpr, Block, Clump, Externals, Heap,
    Location, Object, Props, Stack, StrBuilder, Value, Variable,
};

use crate::ast::{
    ast_expr_as_literal, ast_expr_constant_create, ast_expr_equal, ast_expr_identifier_create,
    ast_type_vconst,
};
use crate::c_util::__assert_rtn;
use crate::ext::externals_types_str;
use crate::object::{object_as_value, object_assign};
use crate::props::{props_copy, props_create, props_destroy, props_str};
use crate::util::{
    dynamic_str, error_create, strbuilder_build, strbuilder_create, strbuilder_printf,
};
use crate::value::{
    value_as_location, value_islocation, value_isstruct, value_issync, value_literal_create,
    value_ptr_create, value_sync_create,
};
use block::{block_install, block_observe, block_range_alloc, block_range_aredeallocands};
use clump::{clump_copy, clump_create, clump_destroy, clump_newblock, clump_str};
use heap::{
    heap_copy, heap_create, heap_destroy, heap_newblock, heap_referenced, heap_str, heap_undeclare,
    vconst_copy, vconst_create, vconst_declare, vconst_eval, vconst_get, vconst_str,
    vconst_undeclare,
};
use location::{
    block_res, location_create_dereferencable, location_create_static, location_dealloc,
    location_destroy, location_getblock, location_offset, location_range_dealloc, location_toclump,
    location_toheap, location_tostack, location_tostatic, location_type, location_with_offset,
    LOCATION_DEREFERENCABLE, LOCATION_DYNAMIC,
};
use r#static::{
    static_memory_checkpool, static_memory_copy, static_memory_create, static_memory_destroy,
    static_memory_newblock, static_memory_str, static_memory_stringpool,
};
use stack::{
    stack_copy, stack_copywithname, stack_create, stack_declare, stack_destroy, stack_getresult,
    stack_getvariable, stack_prev, stack_references, stack_str, stack_undeclare, variable_location,
    variable_type,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct State {
    pub ext: *mut Externals,
    pub vconst: *mut vconst,
    pub static_memory: *mut static_memory,
    pub Clump: *mut Clump,
    pub Stack: *mut Stack,
    pub Heap: *mut Heap,
    pub Props: *mut Props,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct object_res {
    pub obj: *mut Object,
    pub err: *mut error,
}
#[no_mangle]
pub unsafe fn state_create(
    mut func: *mut libc::c_char,
    mut ext: *mut Externals,
    mut result_type: *mut ast_type,
) -> *mut State {
    let mut State: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if State.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_create\0")).as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            35 as libc::c_int,
            b"State\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*State).ext = ext;
    (*State).static_memory = static_memory_create();
    (*State).vconst = vconst_create();
    (*State).Clump = clump_create();
    (*State).Stack = stack_create(func, 0 as *mut Stack, result_type);
    (*State).Heap = heap_create();
    (*State).Props = props_create();
    return State;
}
#[no_mangle]
pub unsafe fn state_create_withprops(
    mut func: *mut libc::c_char,
    mut ext: *mut Externals,
    mut result_type: *mut ast_type,
    mut Props: *mut Props,
) -> *mut State {
    let mut State: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if State.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"state_create_withprops\0",
            ))
            .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            51 as libc::c_int,
            b"State\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*State).ext = ext;
    (*State).static_memory = static_memory_create();
    (*State).vconst = vconst_create();
    (*State).Clump = clump_create();
    (*State).Stack = stack_create(func, 0 as *mut Stack, result_type);
    (*State).Heap = heap_create();
    (*State).Props = props_copy(Props);
    return State;
}
#[no_mangle]
pub unsafe fn state_destroy(mut State: *mut State) {
    static_memory_destroy((*State).static_memory);
    clump_destroy((*State).Clump);
    stack_destroy((*State).Stack);
    heap_destroy((*State).Heap);
    props_destroy((*State).Props);
    free(State as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn state_copy(mut State: *mut State) -> *mut State {
    let mut copy: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if copy.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 11], &[libc::c_char; 11]>(b"state_copy\0")).as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            78 as libc::c_int,
            b"copy\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*copy).ext = (*State).ext;
    (*copy).static_memory = static_memory_copy((*State).static_memory);
    (*copy).vconst = vconst_copy((*State).vconst);
    (*copy).Clump = clump_copy((*State).Clump);
    (*copy).Stack = stack_copy((*State).Stack);
    (*copy).Heap = heap_copy((*State).Heap);
    (*copy).Props = props_copy((*State).Props);
    return copy;
}
#[no_mangle]
pub unsafe fn state_copywithname(
    mut State: *mut State,
    mut func_name: *mut libc::c_char,
) -> *mut State {
    let mut copy: *mut State = malloc(::core::mem::size_of::<State>()) as *mut State;
    if copy.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 19], &[libc::c_char; 19]>(b"state_copywithname\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            93 as libc::c_int,
            b"copy\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*copy).ext = (*State).ext;
    (*copy).static_memory = static_memory_copy((*State).static_memory);
    (*copy).vconst = vconst_copy((*State).vconst);
    (*copy).Clump = clump_copy((*State).Clump);
    (*copy).Stack = stack_copywithname((*State).Stack, func_name);
    (*copy).Heap = heap_copy((*State).Heap);
    (*copy).Props = props_copy((*State).Props);
    return copy;
}
#[no_mangle]
pub unsafe fn state_str(mut State: *mut State) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    strbuilder_printf(b, b"[[\n\0" as *const u8 as *const libc::c_char);
    let mut ext: *mut libc::c_char = externals_types_str(
        (*State).ext,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(ext) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, ext);
    }
    free(ext as *mut libc::c_void);
    let mut static_mem: *mut libc::c_char = static_memory_str(
        (*State).static_memory,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(static_mem) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, static_mem);
    }
    free(static_mem as *mut libc::c_void);
    let mut vconst: *mut libc::c_char = vconst_str(
        (*State).vconst,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(vconst) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, vconst);
    }
    free(vconst as *mut libc::c_void);
    let mut Clump: *mut libc::c_char = clump_str(
        (*State).Clump,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(Clump) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, Clump);
    }
    free(Clump as *mut libc::c_void);
    let mut Stack: *mut libc::c_char = stack_str((*State).Stack, State);
    strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, Stack);
    free(Stack as *mut libc::c_void);
    let mut Props: *mut libc::c_char = props_str(
        (*State).Props,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(Props) > 0 {
        strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, Props);
    }
    free(Props as *mut libc::c_void);
    let mut Heap: *mut libc::c_char = heap_str(
        (*State).Heap,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(Heap) > 0 {
        strbuilder_printf(b, b"\n%s\n\0" as *const u8 as *const libc::c_char, Heap);
    }
    free(Heap as *mut libc::c_void);
    strbuilder_printf(b, b"]]\n\0" as *const u8 as *const libc::c_char);
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe fn state_getext(mut s: *mut State) -> *mut Externals {
    return (*s).ext;
}
#[no_mangle]
pub unsafe fn state_getheap(mut s: *mut State) -> *mut Heap {
    return (*s).Heap;
}
#[no_mangle]
pub unsafe fn state_getprops(mut s: *mut State) -> *mut Props {
    return (*s).Props;
}
#[no_mangle]
pub unsafe fn state_pushframe(
    mut State: *mut State,
    mut func: *mut libc::c_char,
    mut ret_type: *mut ast_type,
) {
    (*State).Stack = stack_create(func, (*State).Stack, ret_type);
}
#[no_mangle]
pub unsafe fn state_popframe(mut State: *mut State) {
    let mut old: *mut Stack = (*State).Stack;
    (*State).Stack = stack_prev(old);
    if ((*State).Stack).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"state_popframe\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            175 as libc::c_int,
            b"State->Stack\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    stack_destroy(old);
}
#[no_mangle]
pub unsafe fn state_declare(mut State: *mut State, mut var: *mut ast_variable, mut isparam: bool) {
    stack_declare((*State).Stack, var, isparam);
}
#[no_mangle]
pub unsafe fn state_vconst(
    mut State: *mut State,
    mut t: *mut ast_type,
    mut comment: *mut libc::c_char,
    mut persist: bool,
) -> *mut Value {
    let mut v: *mut Value = ast_type_vconst(t, State, comment, persist);
    if value_isstruct(v) {
        return v;
    }
    let mut c: *mut libc::c_char = vconst_declare((*State).vconst, v, comment, persist);
    return value_sync_create(ast_expr_identifier_create(c));
}
#[no_mangle]
pub unsafe fn state_static_init(mut State: *mut State, mut expr: *mut AstExpr) -> *mut Value {
    let mut lit: *mut libc::c_char = ast_expr_as_literal(expr);
    let mut loc: *mut Location = static_memory_checkpool((*State).static_memory, lit);
    if !loc.is_null() {
        return value_ptr_create(loc);
    }
    let mut address: libc::c_int = static_memory_newblock((*State).static_memory);
    loc = location_create_static(address, ast_expr_constant_create(0 as libc::c_int));
    let mut res: object_res = state_get(State, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_static_init\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                215 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    if (res.obj).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_static_init\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                218 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    object_assign(res.obj, value_literal_create(dynamic_str(lit)));
    static_memory_stringpool((*State).static_memory, lit, loc);
    return value_ptr_create(loc);
}
#[no_mangle]
pub unsafe fn state_clump(mut State: *mut State) -> *mut Value {
    let mut address: libc::c_int = clump_newblock((*State).Clump);
    let mut loc: *mut Location =
        location_create_dereferencable(address, ast_expr_constant_create(0 as libc::c_int));
    return value_ptr_create(loc);
}
#[no_mangle]
pub unsafe fn state_islval(mut State: *mut State, mut v: *mut Value) -> bool {
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_islval\0")).as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            243 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !value_islocation(v) {
        return 0 as libc::c_int != 0;
    }
    let mut loc: *mut Location = value_as_location(v);
    let mut res: object_res = state_get(State, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_islval\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                250 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return location_tostatic(loc, (*State).static_memory) as libc::c_int != 0
        || location_toheap(loc, (*State).Heap) as libc::c_int != 0
        || location_tostack(loc, (*State).Stack) as libc::c_int != 0
        || location_toclump(loc, (*State).Clump) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe fn state_isalloc(mut State: *mut State, mut v: *mut Value) -> bool {
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"state_isalloc\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            261 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !value_islocation(v) {
        return 0 as libc::c_int != 0;
    }
    let mut loc: *mut Location = value_as_location(v);
    let mut res: object_res = state_get(State, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"state_isalloc\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                268 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return location_toheap(loc, (*State).Heap);
}
#[no_mangle]
pub unsafe fn state_getvconst(mut State: *mut State, mut id: *mut libc::c_char) -> *mut Value {
    return vconst_get((*State).vconst, id);
}
#[no_mangle]
pub unsafe fn state_get(
    mut State: *mut State,
    mut loc: *mut Location,
    mut constructive: bool,
) -> object_res {
    let mut res: block_res = location_getblock(
        loc,
        (*State).static_memory,
        (*State).vconst,
        (*State).Stack,
        (*State).Heap,
        (*State).Clump,
    );
    if !(res.err).is_null() {
        return {
            let mut init = object_res {
                obj: 0 as *mut Object,
                err: res.err,
            };
            init
        };
    }
    if (res.b).is_null() {
        if !(location_type(loc) as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint
            || location_type(loc) as libc::c_uint
                == LOCATION_DEREFERENCABLE as libc::c_int as libc::c_uint) as libc::c_int
            as libc::c_long
            != 0
        {
            __assert_rtn(
                (*::core::mem::transmute::<
                    &[u8; 10],
                    &[libc::c_char; 10],
                >(b"state_get\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                290 as libc::c_int,
                b"location_type(loc) == LOCATION_DYNAMIC || location_type(loc) == LOCATION_DEREFERENCABLE\0"
                    as *const u8 as *const libc::c_char,
            );
        } else {
        };
        return {
            let mut init = object_res {
                obj: 0 as *mut Object,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut obj: *mut Object = block_observe(res.b, location_offset(loc), State, constructive);
    return {
        let mut init = object_res {
            obj: obj,
            err: 0 as *mut error,
        };
        init
    };
}
#[no_mangle]
pub unsafe fn state_blockinstall(mut b: *mut Block, mut obj: *mut Object) {
    block_install(b, obj);
}
#[no_mangle]
pub unsafe fn state_getblock(mut State: *mut State, mut loc: *mut Location) -> *mut Block {
    let mut res: block_res = location_getblock(
        loc,
        (*State).static_memory,
        (*State).vconst,
        (*State).Stack,
        (*State).Heap,
        (*State).Clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"state_getblock\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                310 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return res.b;
}
#[no_mangle]
pub unsafe fn state_getresult(mut State: *mut State) -> *mut Object {
    let mut v: *mut Variable = stack_getresult((*State).Stack);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getresult\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            319 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut res: object_res = state_get(State, variable_location(v), 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getresult\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                323 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return res.obj;
}
unsafe fn state_getresulttype(mut State: *mut State) -> *mut ast_type {
    let mut v: *mut Variable = stack_getresult((*State).Stack);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"state_getresulttype\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            332 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return variable_type(v);
}
#[no_mangle]
pub unsafe fn state_getobjecttype(
    mut State: *mut State,
    mut id: *mut libc::c_char,
) -> *mut ast_type {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresulttype(State);
    }
    let mut v: *mut Variable = stack_getvariable((*State).Stack, id);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"state_getobjecttype\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            345 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return variable_type(v);
}
#[no_mangle]
pub unsafe fn state_getloc(mut State: *mut State, mut id: *mut libc::c_char) -> *mut Value {
    let mut v: *mut Variable = stack_getvariable((*State).Stack, id);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_getloc\0")).as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            354 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return value_ptr_create(variable_location(v));
}
#[no_mangle]
pub unsafe fn state_getobject(mut State: *mut State, mut id: *mut libc::c_char) -> *mut Object {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresult(State);
    }
    let mut v: *mut Variable = stack_getvariable((*State).Stack, id);
    if v.is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getobject\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                368 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    let mut res: object_res = state_get(State, variable_location(v), 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getobject\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                373 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return res.obj;
}
#[no_mangle]
pub unsafe fn state_deref(
    mut State: *mut State,
    mut ptr_val: *mut Value,
    mut index: *mut AstExpr,
) -> object_res {
    if value_issync(ptr_val) {
        return {
            let mut init = object_res {
                obj: 0 as *mut Object,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut deref_base: *mut Location = value_as_location(ptr_val);
    if deref_base.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"state_deref\0")).as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            385 as libc::c_int,
            b"deref_base\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut deref: *mut Location = location_with_offset(deref_base, index);
    let mut res: object_res = state_get(State, deref, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        let mut b: *mut StrBuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"undefined indirection: %s\0" as *const u8 as *const libc::c_char,
            (*res.err).msg,
        );
        return {
            let mut init = object_res {
                obj: 0 as *mut Object,
                err: error_create(strbuilder_build(b)),
            };
            init
        };
    }
    return res;
}
#[no_mangle]
pub unsafe fn state_range_alloc(
    mut State: *mut State,
    mut obj: *mut Object,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
) -> *mut error {
    let mut arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return error_create(
            b"no Value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut deref: *mut Location = value_as_location(arr_val);
    let mut res: block_res = location_getblock(
        deref,
        (*State).static_memory,
        (*State).vconst,
        (*State).Stack,
        (*State).Heap,
        (*State).Clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_range_alloc\0"))
                    .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                416 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    if (res.b).is_null() {
        return error_create(
            b"no Block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    if ast_expr_equal(lw, up) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_range_alloc\0"))
                .as_ptr(),
            b"State.c\0" as *const u8 as *const libc::c_char,
            423 as libc::c_int,
            b"!ast_expr_equal(lw, up)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return block_range_alloc(res.b, lw, up, (*State).Heap);
}
#[no_mangle]
pub unsafe fn state_alloc(mut State: *mut State) -> *mut Value {
    return value_ptr_create(heap_newblock((*State).Heap));
}
#[no_mangle]
pub unsafe fn state_dealloc(mut State: *mut State, mut val: *mut Value) -> *mut error {
    if !value_islocation(val) {
        return error_create(
            b"undefined free of Value not pointing at Heap\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        );
    }
    return location_dealloc(value_as_location(val), (*State).Heap);
}
#[no_mangle]
pub unsafe fn state_range_dealloc(
    mut State: *mut State,
    mut obj: *mut Object,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
) -> *mut error {
    let mut arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return error_create(
            b"no Value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut deref: *mut Location = value_as_location(arr_val);
    return location_range_dealloc(deref, lw, up, State);
}
#[no_mangle]
pub unsafe fn state_addresses_deallocand(mut State: *mut State, mut obj: *mut Object) -> bool {
    let mut val: *mut Value = object_as_value(obj);
    let mut loc: *mut Location = value_as_location(val);
    return state_isdeallocand(State, loc);
}
#[no_mangle]
pub unsafe fn state_isdeallocand(mut s: *mut State, mut loc: *mut Location) -> bool {
    let mut type_equal: bool =
        location_type(loc) as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = state_getblock(s, loc);
    return type_equal as libc::c_int != 0 && !b.is_null();
}
#[no_mangle]
pub unsafe fn state_range_aredeallocands(
    mut State: *mut State,
    mut obj: *mut Object,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
) -> bool {
    if ast_expr_equal(lw, up) {
        return 1 as libc::c_int != 0;
    }
    let mut arr_val: *mut Value = object_as_value(obj);
    if arr_val.is_null() {
        return 0 as libc::c_int != 0;
    }
    let mut deref: *mut Location = value_as_location(arr_val);
    let mut res: block_res = location_getblock(
        deref,
        (*State).static_memory,
        (*State).vconst,
        (*State).Stack,
        (*State).Heap,
        (*State).Clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                    b"state_range_aredeallocands\0",
                ))
                .as_ptr(),
                b"State.c\0" as *const u8 as *const libc::c_char,
                493 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return !(res.b).is_null() as libc::c_int != 0
        && block_range_aredeallocands(res.b, lw, up, State) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe fn state_hasgarbage(mut State: *mut State) -> bool {
    return !heap_referenced((*State).Heap, State);
}
#[no_mangle]
pub unsafe fn state_location_destroy(mut loc: *mut Location) {
    location_destroy(loc);
}
#[no_mangle]
pub unsafe fn state_references(mut s: *mut State, mut loc: *mut Location) -> bool {
    return stack_references((*s).Stack, loc, s);
}
#[no_mangle]
pub unsafe fn state_eval(mut s: *mut State, mut e: *mut AstExpr) -> bool {
    return vconst_eval((*s).vconst, e);
}
#[no_mangle]
pub unsafe fn state_equal(mut s1: *mut State, mut s2: *mut State) -> bool {
    let mut s1_c: *mut State = state_copy(s1);
    let mut s2_c: *mut State = state_copy(s2);
    state_undeclareliterals(s1_c);
    state_undeclareliterals(s2_c);
    state_undeclarevars(s1_c);
    state_undeclarevars(s2_c);
    state_popprops(s1_c);
    state_popprops(s2_c);
    let mut str1: *mut libc::c_char = state_str(s1_c);
    let mut str2: *mut libc::c_char = state_str(s2_c);
    let mut equal: bool = strcmp(str1, str2) == 0 as libc::c_int;
    if !equal {
        v_printf(
            b"actual: %s\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
            str1,
        );
        v_printf(
            b"abstract: %s\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
            str2,
        );
    }
    free(str2 as *mut libc::c_void);
    free(str1 as *mut libc::c_void);
    state_destroy(s2_c);
    state_destroy(s1_c);
    return equal;
}
unsafe fn state_undeclareliterals(mut s: *mut State) {
    static_memory_destroy((*s).static_memory);
    (*s).static_memory = static_memory_create();
}
unsafe fn state_undeclarevars(mut s: *mut State) {
    heap_undeclare((*s).Heap, s);
    vconst_undeclare((*s).vconst);
    stack_undeclare((*s).Stack, s);
}
unsafe fn state_popprops(mut s: *mut State) {
    props_destroy((*s).Props);
    (*s).Props = props_create();
}
