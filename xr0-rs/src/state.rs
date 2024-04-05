#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#![feature(extern_types)]

mod block;
mod clump;
mod heap;
mod location;
mod stack;
mod r#static;

extern "C" {
    pub type strbuilder;
    pub type ast_expr;
    pub type props;
    pub type heap;
    pub type stack;
    pub type clump;
    pub type static_memory;
    pub type vconst;
    pub type externals;
    pub type ast_variable;
    pub type ast_type;
    pub type value;
    pub type block;
    pub type object;
    pub type location;
    pub type variable;
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
    fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    fn dynamic_str(_: *const libc::c_char) -> *mut libc::c_char;
    fn strbuilder_create() -> *mut strbuilder;
    fn strbuilder_printf(b: *mut strbuilder, fmt: *const libc::c_char, _: ...) -> libc::c_int;
    fn strbuilder_build(b: *mut strbuilder) -> *mut libc::c_char;
    fn error_create(s: *mut libc::c_char) -> *mut error;
    fn v_printf(fmt: *mut libc::c_char, _: ...) -> libc::c_int;
    fn ast_expr_identifier_create(_: *mut libc::c_char) -> *mut ast_expr;
    fn ast_expr_constant_create(_: libc::c_int) -> *mut ast_expr;
    fn ast_expr_as_literal(_: *mut ast_expr) -> *mut libc::c_char;
    fn ast_expr_equal(e1: *mut ast_expr, e2: *mut ast_expr) -> bool;
    fn ast_type_vconst(
        _: *mut ast_type,
        s: *mut state,
        comment: *mut libc::c_char,
        persist: bool,
    ) -> *mut value;
    fn block_install(_: *mut block, _: *mut object);
    fn block_observe(
        _: *mut block,
        offset: *mut ast_expr,
        _: *mut state,
        constructive: bool,
    ) -> *mut object;
    fn block_range_alloc(
        b: *mut block,
        lw: *mut ast_expr,
        up: *mut ast_expr,
        heap: *mut heap,
    ) -> *mut error;
    fn block_range_aredeallocands(
        _: *mut block,
        lw: *mut ast_expr,
        up: *mut ast_expr,
        _: *mut state,
    ) -> bool;
    fn clump_create() -> *mut clump;
    fn clump_destroy(_: *mut clump);
    fn clump_str(_: *mut clump, indent: *mut libc::c_char) -> *mut libc::c_char;
    fn clump_copy(_: *mut clump) -> *mut clump;
    fn clump_newblock(_: *mut clump) -> libc::c_int;
    fn externals_types_str(_: *mut externals, indent: *mut libc::c_char) -> *mut libc::c_char;
    fn heap_create() -> *mut heap;
    fn heap_destroy();
    fn heap_copy(_: *mut heap) -> *mut heap;
    fn heap_str(_: *mut heap, indent: *mut libc::c_char) -> *mut libc::c_char;
    fn heap_newblock(h: *mut heap) -> *mut location;
    fn heap_referenced(h: *mut heap, _: *mut state) -> bool;
    fn heap_undeclare(_: *mut heap, _: *mut state);
    fn vconst_create() -> *mut vconst;
    fn vconst_copy(_: *mut vconst) -> *mut vconst;
    fn vconst_str(_: *mut vconst, indent: *mut libc::c_char) -> *mut libc::c_char;
    fn vconst_declare(
        _: *mut vconst,
        _: *mut value,
        comment: *mut libc::c_char,
        persist: bool,
    ) -> *mut libc::c_char;
    fn vconst_get(_: *mut vconst, id: *mut libc::c_char) -> *mut value;
    fn vconst_undeclare(_: *mut vconst);
    fn vconst_eval(_: *mut vconst, _: *mut ast_expr) -> bool;
    fn static_memory_create() -> *mut static_memory;
    fn static_memory_destroy(_: *mut static_memory);
    fn static_memory_str(_: *mut static_memory, indent: *mut libc::c_char) -> *mut libc::c_char;
    fn static_memory_copy(_: *mut static_memory) -> *mut static_memory;
    fn static_memory_newblock(_: *mut static_memory) -> libc::c_int;
    fn static_memory_stringpool(sm: *mut static_memory, lit: *mut libc::c_char, _: *mut location);
    fn static_memory_checkpool(_: *mut static_memory, _: *mut libc::c_char) -> *mut location;
    fn location_create_static(block: libc::c_int, offset: *mut ast_expr) -> *mut location;
    fn location_create_dereferencable(block: libc::c_int, offset: *mut ast_expr) -> *mut location;
    fn location_destroy(_: *mut location);
    fn location_tostatic(_: *mut location, _: *mut static_memory) -> bool;
    fn location_toheap(_: *mut location, _: *mut heap) -> bool;
    fn location_tostack(_: *mut location, _: *mut stack) -> bool;
    fn location_toclump(_: *mut location, _: *mut clump) -> bool;
    fn location_type(loc: *mut location) -> location_type;
    fn location_offset(loc: *mut location) -> *mut ast_expr;
    fn location_with_offset(loc: *mut location, offset: *mut ast_expr) -> *mut location;
    fn location_getblock(
        _: *mut location,
        _: *mut static_memory,
        _: *mut vconst,
        _: *mut stack,
        _: *mut heap,
        _: *mut clump,
    ) -> block_res;
    fn location_dealloc(_: *mut location, _: *mut heap) -> *mut error;
    fn location_range_dealloc(
        loc: *mut location,
        lw: *mut ast_expr,
        up: *mut ast_expr,
        _: *mut state,
    ) -> *mut error;
    fn object_as_value(_: *mut object) -> *mut value;
    fn object_assign(_: *mut object, _: *mut value) -> *mut error;
    fn props_create() -> *mut props;
    fn props_copy(_: *mut props) -> *mut props;
    fn props_destroy(_: *mut props);
    fn props_str(_: *mut props, indent: *mut libc::c_char) -> *mut libc::c_char;
    fn stack_create(
        name: *mut libc::c_char,
        prev: *mut stack,
        ret_type: *mut ast_type,
    ) -> *mut stack;
    fn stack_destroy(_: *mut stack);
    fn stack_copy(_: *mut stack) -> *mut stack;
    fn stack_copywithname(_: *mut stack, new_name: *mut libc::c_char) -> *mut stack;
    fn stack_str(_: *mut stack, _: *mut state) -> *mut libc::c_char;
    fn stack_prev(_: *mut stack) -> *mut stack;
    fn stack_declare(_: *mut stack, var: *mut ast_variable, isparam: bool);
    fn stack_undeclare(stack: *mut stack, state: *mut state);
    fn stack_getresult(_: *mut stack) -> *mut variable;
    fn stack_getvariable(s: *mut stack, id: *mut libc::c_char) -> *mut variable;
    fn stack_references(s: *mut stack, loc: *mut location, state: *mut state) -> bool;
    fn variable_location(_: *mut variable) -> *mut location;
    fn variable_type(_: *mut variable) -> *mut ast_type;
    fn value_ptr_create(loc: *mut location) -> *mut value;
    fn value_literal_create(_: *mut libc::c_char) -> *mut value;
    fn value_sync_create(_: *mut ast_expr) -> *mut value;
    fn value_isstruct(v: *mut value) -> bool;
    fn value_islocation(_: *mut value) -> bool;
    fn value_as_location(_: *mut value) -> *mut location;
    fn value_issync(v: *mut value) -> bool;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct error {
    pub msg: *mut libc::c_char,
    pub inner: *mut error,
}
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
pub type location_type = libc::c_uint;
pub const LOCATION_DYNAMIC: location_type = 4;
pub const LOCATION_AUTOMATIC: location_type = 3;
pub const LOCATION_DEREFERENCABLE: location_type = 2;
pub const LOCATION_VCONST: location_type = 1;
pub const LOCATION_STATIC: location_type = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct block_res {
    pub b: *mut block,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct object_res {
    pub obj: *mut object,
    pub err: *mut error,
}
#[no_mangle]
pub unsafe extern "C" fn state_create(
    mut func: *mut libc::c_char,
    mut ext: *mut externals,
    mut result_type: *mut ast_type,
) -> *mut state {
    let mut state: *mut state =
        malloc(::core::mem::size_of::<state>() as libc::c_ulong) as *mut state;
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
#[no_mangle]
pub unsafe extern "C" fn state_create_withprops(
    mut func: *mut libc::c_char,
    mut ext: *mut externals,
    mut result_type: *mut ast_type,
    mut props: *mut props,
) -> *mut state {
    let mut state: *mut state =
        malloc(::core::mem::size_of::<state>() as libc::c_ulong) as *mut state;
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
#[no_mangle]
pub unsafe extern "C" fn state_destroy(mut state: *mut state) {
    static_memory_destroy((*state).static_memory);
    clump_destroy((*state).clump);
    stack_destroy((*state).stack);
    heap_destroy((*state).heap);
    props_destroy((*state).props);
    free(state as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn state_copy(mut state: *mut state) -> *mut state {
    let mut copy: *mut state =
        malloc(::core::mem::size_of::<state>() as libc::c_ulong) as *mut state;
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
#[no_mangle]
pub unsafe extern "C" fn state_copywithname(
    mut state: *mut state,
    mut func_name: *mut libc::c_char,
) -> *mut state {
    let mut copy: *mut state =
        malloc(::core::mem::size_of::<state>() as libc::c_ulong) as *mut state;
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
#[no_mangle]
pub unsafe extern "C" fn state_str(mut state: *mut state) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    strbuilder_printf(b, b"[[\n\0" as *const u8 as *const libc::c_char);
    let mut ext: *mut libc::c_char = externals_types_str(
        (*state).ext,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(ext) > 0 as libc::c_int as libc::c_ulong {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, ext);
    }
    free(ext as *mut libc::c_void);
    let mut static_mem: *mut libc::c_char = static_memory_str(
        (*state).static_memory,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(static_mem) > 0 as libc::c_int as libc::c_ulong {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, static_mem);
    }
    free(static_mem as *mut libc::c_void);
    let mut vconst: *mut libc::c_char = vconst_str(
        (*state).vconst,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(vconst) > 0 as libc::c_int as libc::c_ulong {
        strbuilder_printf(b, b"%s\n\0" as *const u8 as *const libc::c_char, vconst);
    }
    free(vconst as *mut libc::c_void);
    let mut clump: *mut libc::c_char = clump_str(
        (*state).clump,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(clump) > 0 as libc::c_int as libc::c_ulong {
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
    if strlen(props) > 0 as libc::c_int as libc::c_ulong {
        strbuilder_printf(b, b"%s\0" as *const u8 as *const libc::c_char, props);
    }
    free(props as *mut libc::c_void);
    let mut heap: *mut libc::c_char = heap_str(
        (*state).heap,
        b"\t\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
    );
    if strlen(heap) > 0 as libc::c_int as libc::c_ulong {
        strbuilder_printf(b, b"\n%s\n\0" as *const u8 as *const libc::c_char, heap);
    }
    free(heap as *mut libc::c_void);
    strbuilder_printf(b, b"]]\n\0" as *const u8 as *const libc::c_char);
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe extern "C" fn state_getext(mut s: *mut state) -> *mut externals {
    return (*s).ext;
}
#[no_mangle]
pub unsafe extern "C" fn state_getheap(mut s: *mut state) -> *mut heap {
    return (*s).heap;
}
#[no_mangle]
pub unsafe extern "C" fn state_getprops(mut s: *mut state) -> *mut props {
    return (*s).props;
}
#[no_mangle]
pub unsafe extern "C" fn state_pushframe(
    mut state: *mut state,
    mut func: *mut libc::c_char,
    mut ret_type: *mut ast_type,
) {
    (*state).stack = stack_create(func, (*state).stack, ret_type);
}
#[no_mangle]
pub unsafe extern "C" fn state_popframe(mut state: *mut state) {
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
#[no_mangle]
pub unsafe extern "C" fn state_declare(
    mut state: *mut state,
    mut var: *mut ast_variable,
    mut isparam: bool,
) {
    stack_declare((*state).stack, var, isparam);
}
#[no_mangle]
pub unsafe extern "C" fn state_vconst(
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
#[no_mangle]
pub unsafe extern "C" fn state_static_init(
    mut state: *mut state,
    mut expr: *mut ast_expr,
) -> *mut value {
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
#[no_mangle]
pub unsafe extern "C" fn state_clump(mut state: *mut state) -> *mut value {
    let mut address: libc::c_int = clump_newblock((*state).clump);
    let mut loc: *mut location =
        location_create_dereferencable(address, ast_expr_constant_create(0 as libc::c_int));
    return value_ptr_create(loc);
}
#[no_mangle]
pub unsafe extern "C" fn state_islval(mut state: *mut state, mut v: *mut value) -> bool {
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
#[no_mangle]
pub unsafe extern "C" fn state_isalloc(mut state: *mut state, mut v: *mut value) -> bool {
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
#[no_mangle]
pub unsafe extern "C" fn state_getvconst(
    mut state: *mut state,
    mut id: *mut libc::c_char,
) -> *mut value {
    return vconst_get((*state).vconst, id);
}
#[no_mangle]
pub unsafe extern "C" fn state_get(
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
#[no_mangle]
pub unsafe extern "C" fn state_blockinstall(mut b: *mut block, mut obj: *mut object) {
    block_install(b, obj);
}
#[no_mangle]
pub unsafe extern "C" fn state_getblock(
    mut state: *mut state,
    mut loc: *mut location,
) -> *mut block {
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
#[no_mangle]
pub unsafe extern "C" fn state_getresult(mut state: *mut state) -> *mut object {
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
unsafe extern "C" fn state_getresulttype(mut state: *mut state) -> *mut ast_type {
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
#[no_mangle]
pub unsafe extern "C" fn state_getobjecttype(
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
#[no_mangle]
pub unsafe extern "C" fn state_getloc(
    mut state: *mut state,
    mut id: *mut libc::c_char,
) -> *mut value {
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
#[no_mangle]
pub unsafe extern "C" fn state_getobject(
    mut state: *mut state,
    mut id: *mut libc::c_char,
) -> *mut object {
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
#[no_mangle]
pub unsafe extern "C" fn state_deref(
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
#[no_mangle]
pub unsafe extern "C" fn state_range_alloc(
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
#[no_mangle]
pub unsafe extern "C" fn state_alloc(mut state: *mut state) -> *mut value {
    return value_ptr_create(heap_newblock((*state).heap));
}
#[no_mangle]
pub unsafe extern "C" fn state_dealloc(mut state: *mut state, mut val: *mut value) -> *mut error {
    if !value_islocation(val) {
        return error_create(
            b"undefined free of value not pointing at heap\0" as *const u8 as *const libc::c_char
                as *mut libc::c_char,
        );
    }
    return location_dealloc(value_as_location(val), (*state).heap);
}
#[no_mangle]
pub unsafe extern "C" fn state_range_dealloc(
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
#[no_mangle]
pub unsafe extern "C" fn state_addresses_deallocand(
    mut state: *mut state,
    mut obj: *mut object,
) -> bool {
    let mut val: *mut value = object_as_value(obj);
    let mut loc: *mut location = value_as_location(val);
    return state_isdeallocand(state, loc);
}
#[no_mangle]
pub unsafe extern "C" fn state_isdeallocand(mut s: *mut state, mut loc: *mut location) -> bool {
    let mut type_equal: bool =
        location_type(loc) as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let mut b: *mut block = state_getblock(s, loc);
    return type_equal as libc::c_int != 0 && !b.is_null();
}
#[no_mangle]
pub unsafe extern "C" fn state_range_aredeallocands(
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
#[no_mangle]
pub unsafe extern "C" fn state_hasgarbage(mut state: *mut state) -> bool {
    return !heap_referenced((*state).heap, state);
}
#[no_mangle]
pub unsafe extern "C" fn state_location_destroy(mut loc: *mut location) {
    location_destroy(loc);
}
#[no_mangle]
pub unsafe extern "C" fn state_references(mut s: *mut state, mut loc: *mut location) -> bool {
    return stack_references((*s).stack, loc, s);
}
#[no_mangle]
pub unsafe extern "C" fn state_eval(mut s: *mut state, mut e: *mut ast_expr) -> bool {
    return vconst_eval((*s).vconst, e);
}
#[no_mangle]
pub unsafe extern "C" fn state_equal(mut s1: *mut state, mut s2: *mut state) -> bool {
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
unsafe extern "C" fn state_undeclareliterals(mut s: *mut state) {
    static_memory_destroy((*s).static_memory);
    (*s).static_memory = static_memory_create();
}
unsafe extern "C" fn state_undeclarevars(mut s: *mut state) {
    heap_undeclare((*s).heap, s);
    vconst_undeclare((*s).vconst);
    stack_undeclare((*s).stack, s);
}
unsafe extern "C" fn state_popprops(mut s: *mut state) {
    props_destroy((*s).props);
    (*s).props = props_create();
}
