use libc::{free, malloc, strcmp, strlen};

use super::block::{block_install, block_observe, block_range_alloc, block_range_aredeallocands};
use super::clump::{clump_copy, clump_create, clump_destroy, clump_newblock, clump_str};
use super::heap::{
    heap_copy, heap_create, heap_destroy, heap_newblock, heap_referenced, heap_str, heap_undeclare,
    vconst_copy, vconst_create, vconst_declare, vconst_eval, vconst_get, vconst_str,
    vconst_undeclare,
};
use super::location::{
    block_res, location_create_dereferencable, location_create_static, location_dealloc,
    location_destroy, location_getblock, location_offset, location_range_dealloc, location_toclump,
    location_toheap, location_tostack, location_tostatic, location_type, location_with_offset,
    LOCATION_DEREFERENCABLE, LOCATION_DYNAMIC,
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
use crate::c_util::__assert_rtn;
use crate::ext::externals_types_str;
use crate::object::{object_as_value, object_assign};
use crate::props::{props_copy, props_create, props_destroy, props_str};
use crate::util::{
    dynamic_str, error, error_create, strbuilder_build, strbuilder_create, strbuilder_printf,
    v_printf,
};
use crate::value::{
    value_as_location, value_islocation, value_isstruct, value_issync, value_literal_create,
    value_ptr_create, value_sync_create,
};
use crate::{
    ast_type, ast_variable, static_memory, vconst, AstExpr as ast_expr, Block as block,
    Clump as clump, Externals as externals, Heap as heap, Location as location, Object as object,
    Props as props, Stack as stack, StrBuilder as strbuilder, Value as value, Variable as variable,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct state {
    pub ext: *mut externals,
    pub vconst: *mut vconst,
    pub static_memory: *mut static_memory,
    pub clump: *mut clump,
    pub stack: *mut stack,
    pub heap: *mut heap,
    pub props: *mut props,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct object_res {
    pub obj: *mut object,
    pub err: *mut error,
}

pub unsafe fn state_create(
    mut func: *mut libc::c_char,
    mut ext: *mut externals,
    mut result_type: *mut ast_type,
) -> *mut state {
    let mut state: *mut state = malloc(::core::mem::size_of::<state>()) as *mut state;
    if state.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_create\0")).as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            35 as libc::c_int,
            b"state\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*state).ext = ext;
    (*state).static_memory = static_memory_create();
    (*state).vconst = vconst_create();
    (*state).clump = clump_create();
    (*state).stack = stack_create(func, 0 as *mut stack, result_type);
    (*state).heap = heap_create();
    (*state).props = props_create();
    return state;
}

pub unsafe fn state_create_withprops(
    mut func: *mut libc::c_char,
    mut ext: *mut externals,
    mut result_type: *mut ast_type,
    mut props: *mut props,
) -> *mut state {
    let mut state: *mut state = malloc(::core::mem::size_of::<state>()) as *mut state;
    if state.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"state_create_withprops\0",
            ))
            .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            51 as libc::c_int,
            b"state\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*state).ext = ext;
    (*state).static_memory = static_memory_create();
    (*state).vconst = vconst_create();
    (*state).clump = clump_create();
    (*state).stack = stack_create(func, 0 as *mut stack, result_type);
    (*state).heap = heap_create();
    (*state).props = props_copy(props);
    return state;
}

pub unsafe fn state_destroy(mut state: *mut state) {
    static_memory_destroy((*state).static_memory);
    clump_destroy((*state).clump);
    stack_destroy((*state).stack);
    heap_destroy((*state).heap);
    props_destroy((*state).props);
    free(state as *mut libc::c_void);
}

pub unsafe fn state_copy(mut state: *mut state) -> *mut state {
    let mut copy: *mut state = malloc(::core::mem::size_of::<state>()) as *mut state;
    if copy.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 11], &[libc::c_char; 11]>(b"state_copy\0")).as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            78 as libc::c_int,
            b"copy\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
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
    mut state: *mut state,
    mut func_name: *mut libc::c_char,
) -> *mut state {
    let mut copy: *mut state = malloc(::core::mem::size_of::<state>()) as *mut state;
    if copy.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 19], &[libc::c_char; 19]>(b"state_copywithname\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            93 as libc::c_int,
            b"copy\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*copy).ext = (*state).ext;
    (*copy).static_memory = static_memory_copy((*state).static_memory);
    (*copy).vconst = vconst_copy((*state).vconst);
    (*copy).clump = clump_copy((*state).clump);
    (*copy).stack = stack_copywithname((*state).stack, func_name);
    (*copy).heap = heap_copy((*state).heap);
    (*copy).props = props_copy((*state).props);
    return copy;
}

pub unsafe fn state_str(mut state: *mut state) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    strbuilder_printf(b, b"[[\n\0" as *const u8 as *const libc::c_char);
    let mut ext: *mut libc::c_char = externals_types_str(
        (*state).ext,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(ext) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, ext);
    }
    free(ext as *mut libc::c_void);
    let mut static_mem: *mut libc::c_char = static_memory_str(
        (*state).static_memory,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(static_mem) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, static_mem);
    }
    free(static_mem as *mut libc::c_void);
    let mut vconst: *mut libc::c_char = vconst_str(
        (*state).vconst,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(vconst) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, vconst);
    }
    free(vconst as *mut libc::c_void);
    let mut clump: *mut libc::c_char = clump_str(
        (*state).clump,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(clump) > 0 {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, clump);
    }
    free(clump as *mut libc::c_void);
    let mut stack: *mut libc::c_char = stack_str((*state).stack, state);
    strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, stack);
    free(stack as *mut libc::c_void);
    let mut props: *mut libc::c_char = props_str(
        (*state).props,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(props) > 0 {
        strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, props);
    }
    free(props as *mut libc::c_void);
    let mut heap: *mut libc::c_char = heap_str(
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

pub unsafe fn state_getext(mut s: *mut state) -> *mut externals {
    return (*s).ext;
}

pub unsafe fn state_getheap(mut s: *mut state) -> *mut heap {
    return (*s).heap;
}

pub unsafe fn state_getprops(mut s: *mut state) -> *mut props {
    return (*s).props;
}

pub unsafe fn state_pushframe(
    mut state: *mut state,
    mut func: *mut libc::c_char,
    mut ret_type: *mut ast_type,
) {
    (*state).stack = stack_create(func, (*state).stack, ret_type);
}

pub unsafe fn state_popframe(mut state: *mut state) {
    let mut old: *mut stack = (*state).stack;
    (*state).stack = stack_prev(old);
    if ((*state).stack).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"state_popframe\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            175 as libc::c_int,
            b"state->stack\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    stack_destroy(old);
}

pub unsafe fn state_declare(mut state: *mut state, mut var: *mut ast_variable, mut isparam: bool) {
    stack_declare((*state).stack, var, isparam);
}

pub unsafe fn state_vconst(
    mut state: *mut state,
    mut t: *mut ast_type,
    mut comment: *mut libc::c_char,
    mut persist: bool,
) -> *mut value {
    let mut v: *mut value = ast_type_vconst(t, state, comment, persist);
    if value_isstruct(v) {
        return v;
    }
    let mut c: *mut libc::c_char = vconst_declare((*state).vconst, v, comment, persist);
    return value_sync_create(ast_expr_identifier_create(c));
}

pub unsafe fn state_static_init(mut state: *mut state, mut expr: *mut ast_expr) -> *mut value {
    let mut lit: *mut libc::c_char = ast_expr_as_literal(expr);
    let mut loc: *mut location = static_memory_checkpool((*state).static_memory, lit);
    if !loc.is_null() {
        return value_ptr_create(loc);
    }
    let mut address: libc::c_int = static_memory_newblock((*state).static_memory);
    loc = location_create_static(address, ast_expr_constant_create(0 as libc::c_int));
    let mut res: object_res = state_get(state, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_static_init\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
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
                b"state.c\0" as *const u8 as *const libc::c_char,
                218 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    object_assign(res.obj, value_literal_create(dynamic_str(lit)));
    static_memory_stringpool((*state).static_memory, lit, loc);
    return value_ptr_create(loc);
}

pub unsafe fn state_clump(mut state: *mut state) -> *mut value {
    let mut address: libc::c_int = clump_newblock((*state).clump);
    let mut loc: *mut location =
        location_create_dereferencable(address, ast_expr_constant_create(0 as libc::c_int));
    return value_ptr_create(loc);
}

pub unsafe fn state_islval(mut state: *mut state, mut v: *mut value) -> bool {
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_islval\0")).as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            243 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !value_islocation(v) {
        return 0 as libc::c_int != 0;
    }
    let mut loc: *mut location = value_as_location(v);
    let mut res: object_res = state_get(state, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_islval\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                250 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return location_tostatic(loc, (*state).static_memory) as libc::c_int != 0
        || location_toheap(loc, (*state).heap) as libc::c_int != 0
        || location_tostack(loc, (*state).stack) as libc::c_int != 0
        || location_toclump(loc, (*state).clump) as libc::c_int != 0;
}

pub unsafe fn state_isalloc(mut state: *mut state, mut v: *mut value) -> bool {
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"state_isalloc\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            261 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !value_islocation(v) {
        return 0 as libc::c_int != 0;
    }
    let mut loc: *mut location = value_as_location(v);
    let mut res: object_res = state_get(state, loc, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"state_isalloc\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                268 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return location_toheap(loc, (*state).heap);
}

pub unsafe fn state_getvconst(mut state: *mut state, mut id: *mut libc::c_char) -> *mut value {
    return vconst_get((*state).vconst, id);
}

pub unsafe fn state_get(
    mut state: *mut state,
    mut loc: *mut location,
    mut constructive: bool,
) -> object_res {
    let mut res: block_res = location_getblock(
        loc,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        return {
            let mut init = object_res {
                obj: 0 as *mut object,
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
                b"state.c\0" as *const u8 as *const libc::c_char,
                290 as libc::c_int,
                b"location_type(loc) == LOCATION_DYNAMIC || location_type(loc) == LOCATION_DEREFERENCABLE\0"
                    as *const u8 as *const libc::c_char,
            );
        } else {
        };
        return {
            let mut init = object_res {
                obj: 0 as *mut object,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut obj: *mut object = block_observe(res.b, location_offset(loc), state, constructive);
    return {
        let mut init = object_res {
            obj: obj,
            err: 0 as *mut error,
        };
        init
    };
}

pub unsafe fn state_blockinstall(mut b: *mut block, mut obj: *mut object) {
    block_install(b, obj);
}

pub unsafe fn state_getblock(mut state: *mut state, mut loc: *mut location) -> *mut block {
    let mut res: block_res = location_getblock(
        loc,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"state_getblock\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                310 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return res.b;
}

pub unsafe fn state_getresult(mut state: *mut state) -> *mut object {
    let mut v: *mut variable = stack_getresult((*state).stack);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getresult\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            319 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut res: object_res = state_get(state, variable_location(v), 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getresult\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                323 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return res.obj;
}
unsafe fn state_getresulttype(mut state: *mut state) -> *mut ast_type {
    let mut v: *mut variable = stack_getresult((*state).stack);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"state_getresulttype\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            332 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return variable_type(v);
}

pub unsafe fn state_getobjecttype(
    mut state: *mut state,
    mut id: *mut libc::c_char,
) -> *mut ast_type {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresulttype(state);
    }
    let mut v: *mut variable = stack_getvariable((*state).stack, id);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"state_getobjecttype\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            345 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return variable_type(v);
}

pub unsafe fn state_getloc(mut state: *mut state, mut id: *mut libc::c_char) -> *mut value {
    let mut v: *mut variable = stack_getvariable((*state).stack, id);
    if v.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"state_getloc\0")).as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            354 as libc::c_int,
            b"v\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return value_ptr_create(variable_location(v));
}

pub unsafe fn state_getobject(mut state: *mut state, mut id: *mut libc::c_char) -> *mut object {
    if strcmp(id, b"return\0" as *const u8 as *const libc::c_char) == 0 as libc::c_int {
        return state_getresult(state);
    }
    let mut v: *mut variable = stack_getvariable((*state).stack, id);
    if v.is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getobject\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                368 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    let mut res: object_res = state_get(state, variable_location(v), 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"state_getobject\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                373 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return res.obj;
}

pub unsafe fn state_deref(
    mut state: *mut state,
    mut ptr_val: *mut value,
    mut index: *mut ast_expr,
) -> object_res {
    if value_issync(ptr_val) {
        return {
            let mut init = object_res {
                obj: 0 as *mut object,
                err: 0 as *mut error,
            };
            init
        };
    }
    let mut deref_base: *mut location = value_as_location(ptr_val);
    if deref_base.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"state_deref\0")).as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            385 as libc::c_int,
            b"deref_base\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut deref: *mut location = location_with_offset(deref_base, index);
    let mut res: object_res = state_get(state, deref, 1 as libc::c_int != 0);
    if !(res.err).is_null() {
        let mut b: *mut strbuilder = strbuilder_create();
        strbuilder_printf(
            b,
            b"undefined indirection: %s\0" as *const u8 as *const libc::c_char,
            (*res.err).msg,
        );
        return {
            let mut init = object_res {
                obj: 0 as *mut object,
                err: error_create(strbuilder_build(b)),
            };
            init
        };
    }
    return res;
}

pub unsafe fn state_range_alloc(
    mut state: *mut state,
    mut obj: *mut object,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
) -> *mut error {
    let mut arr_val: *mut value = object_as_value(obj);
    if arr_val.is_null() {
        return error_create(
            b"no value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut deref: *mut location = value_as_location(arr_val);
    let mut res: block_res = location_getblock(
        deref,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_range_alloc\0"))
                    .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                416 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    if (res.b).is_null() {
        return error_create(
            b"no block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    if ast_expr_equal(lw, up) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"state_range_alloc\0"))
                .as_ptr(),
            b"state.c\0" as *const u8 as *const libc::c_char,
            423 as libc::c_int,
            b"!ast_expr_equal(lw, up)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return block_range_alloc(res.b, lw, up, (*state).heap);
}

pub unsafe fn state_alloc(mut state: *mut state) -> *mut value {
    return value_ptr_create(heap_newblock((*state).heap));
}

pub unsafe fn state_dealloc(mut state: *mut state, mut val: *mut value) -> *mut error {
    if !value_islocation(val) {
        return error_create(
            b"undefined free of value not pointing at heap\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        );
    }
    return location_dealloc(value_as_location(val), (*state).heap);
}

pub unsafe fn state_range_dealloc(
    mut state: *mut state,
    mut obj: *mut object,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
) -> *mut error {
    let mut arr_val: *mut value = object_as_value(obj);
    if arr_val.is_null() {
        return error_create(
            b"no value\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    let mut deref: *mut location = value_as_location(arr_val);
    return location_range_dealloc(deref, lw, up, state);
}

pub unsafe fn state_addresses_deallocand(mut state: *mut state, mut obj: *mut object) -> bool {
    let mut val: *mut value = object_as_value(obj);
    let mut loc: *mut location = value_as_location(val);
    return state_isdeallocand(state, loc);
}

pub unsafe fn state_isdeallocand(mut s: *mut state, mut loc: *mut location) -> bool {
    let mut type_equal: bool =
        location_type(loc) as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let mut b: *mut block = state_getblock(s, loc);
    return type_equal as libc::c_int != 0 && !b.is_null();
}

pub unsafe fn state_range_aredeallocands(
    mut state: *mut state,
    mut obj: *mut object,
    mut lw: *mut ast_expr,
    mut up: *mut ast_expr,
) -> bool {
    if ast_expr_equal(lw, up) {
        return 1 as libc::c_int != 0;
    }
    let mut arr_val: *mut value = object_as_value(obj);
    if arr_val.is_null() {
        return 0 as libc::c_int != 0;
    }
    let mut deref: *mut location = value_as_location(arr_val);
    let mut res: block_res = location_getblock(
        deref,
        (*state).static_memory,
        (*state).vconst,
        (*state).stack,
        (*state).heap,
        (*state).clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 27], &[libc::c_char; 27]>(
                    b"state_range_aredeallocands\0",
                ))
                .as_ptr(),
                b"state.c\0" as *const u8 as *const libc::c_char,
                493 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return !(res.b).is_null() as libc::c_int != 0
        && block_range_aredeallocands(res.b, lw, up, state) as libc::c_int != 0;
}

pub unsafe fn state_hasgarbage(mut state: *mut state) -> bool {
    return !heap_referenced((*state).heap, state);
}

pub unsafe fn state_location_destroy(mut loc: *mut location) {
    location_destroy(loc);
}

pub unsafe fn state_references(mut s: *mut state, mut loc: *mut location) -> bool {
    return stack_references((*s).stack, loc, s);
}

pub unsafe fn state_eval(mut s: *mut state, mut e: *mut ast_expr) -> bool {
    return vconst_eval((*s).vconst, e);
}

pub unsafe fn state_equal(mut s1: *mut state, mut s2: *mut state) -> bool {
    let mut s1_c: *mut state = state_copy(s1);
    let mut s2_c: *mut state = state_copy(s2);
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
unsafe fn state_undeclareliterals(mut s: *mut state) {
    static_memory_destroy((*s).static_memory);
    (*s).static_memory = static_memory_create();
}
unsafe fn state_undeclarevars(mut s: *mut state) {
    heap_undeclare((*s).heap, s);
    vconst_undeclare((*s).vconst);
    stack_undeclare((*s).stack, s);
}
unsafe fn state_popprops(mut s: *mut state) {
    props_destroy((*s).props);
    (*s).props = props_create();
}
