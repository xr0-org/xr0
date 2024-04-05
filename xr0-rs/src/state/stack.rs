#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case, non_upper_case_globals, unused_assignments, unused_mut)]
#![feature(extern_types)]
extern "C" {
    pub type strbuilder;
    pub type ast_expr;
    pub type state;
    pub type ast_variable;
    pub type ast_type;
    pub type value;
    pub type location;
    pub type static_memory;
    pub type heap;
    pub type block_arr;
    pub type clump;
    pub type vconst;
    pub type object;
    pub type block;
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
    fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
    fn ast_expr_constant_create(_: libc::c_int) -> *mut ast_expr;
    fn ast_type_destroy(_: *mut ast_type);
    fn ast_type_str(_: *mut ast_type) -> *mut libc::c_char;
    fn ast_type_copy(t: *mut ast_type) -> *mut ast_type;
    fn ast_variable_name(_: *mut ast_variable) -> *mut libc::c_char;
    fn ast_variable_type(_: *mut ast_variable) -> *mut ast_type;
    fn dynamic_str(_: *const libc::c_char) -> *mut libc::c_char;
    fn map_create() -> *mut map;
    fn map_destroy(_: *mut map);
    fn map_get(_: *mut map, key: *const libc::c_char) -> *mut libc::c_void;
    fn map_set(_: *mut map, key: *const libc::c_char, value: *const libc::c_void);
    fn strbuilder_create() -> *mut strbuilder;
    fn strbuilder_printf(
        b: *mut strbuilder,
        fmt: *const libc::c_char,
        _: ...
    ) -> libc::c_int;
    fn strbuilder_putc(b: *mut strbuilder, c: libc::c_char);
    fn strbuilder_build(b: *mut strbuilder) -> *mut libc::c_char;
    fn location_create_automatic(
        frame: libc::c_int,
        block: libc::c_int,
        offset: *mut ast_expr,
    ) -> *mut location;
    fn location_copy(loc: *mut location) -> *mut location;
    fn location_destroy(_: *mut location);
    fn location_str(_: *mut location) -> *mut libc::c_char;
    fn location_type(loc: *mut location) -> location_type;
    fn location_offset(loc: *mut location) -> *mut ast_expr;
    fn location_references(
        loc1: *mut location,
        loc2: *mut location,
        _: *mut state,
    ) -> bool;
    fn location_getblock(
        _: *mut location,
        _: *mut static_memory,
        _: *mut vconst,
        _: *mut stack,
        _: *mut heap,
        _: *mut clump,
    ) -> block_res;
    fn location_getstackblock(loc: *mut location, s: *mut stack) -> *mut block;
    fn block_create() -> *mut block;
    fn block_install(_: *mut block, _: *mut object);
    fn block_observe(
        _: *mut block,
        offset: *mut ast_expr,
        _: *mut state,
        constructive: bool,
    ) -> *mut object;
    fn block_arr_create() -> *mut block_arr;
    fn block_arr_destroy(_: *mut block_arr);
    fn block_arr_copy(_: *mut block_arr) -> *mut block_arr;
    fn block_arr_blocks(_: *mut block_arr) -> *mut *mut block;
    fn block_arr_nblocks(_: *mut block_arr) -> libc::c_int;
    fn block_arr_append(_: *mut block_arr, _: *mut block) -> libc::c_int;
    fn state_get(
        state: *mut state,
        loc: *mut location,
        constructive: bool,
    ) -> object_res;
    fn object_value_create(offset: *mut ast_expr, _: *mut value) -> *mut object;
    fn object_str(_: *mut object) -> *mut libc::c_char;
    fn object_isvalue(_: *mut object) -> bool;
    fn object_as_value(_: *mut object) -> *mut value;
    fn object_assign(_: *mut object, _: *mut value) -> *mut error;
    fn value_abstractcopy(_: *mut value, s: *mut state) -> *mut value;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct map {
    pub entry: *mut entry,
    pub n: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct entry {
    pub key: *mut libc::c_char,
    pub value: *const libc::c_void,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct error {
    pub msg: *mut libc::c_char,
    pub inner: *mut error,
}
pub type location_type = libc::c_uint;
pub const LOCATION_DYNAMIC: location_type = 4;
pub const LOCATION_AUTOMATIC: location_type = 3;
pub const LOCATION_DEREFERENCABLE: location_type = 2;
pub const LOCATION_VCONST: location_type = 1;
pub const LOCATION_STATIC: location_type = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct stack {
    pub name: *mut libc::c_char,
    pub frame: *mut block_arr,
    pub varmap: *mut map,
    pub result: *mut variable,
    pub id: libc::c_int,
    pub prev: *mut stack,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct variable {
    pub type_0: *mut ast_type,
    pub loc: *mut location,
    pub isparam: bool,
}
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
pub unsafe extern "C" fn stack_newblock(mut stack: *mut stack) -> *mut location {
    let mut address: libc::c_int = block_arr_append((*stack).frame, block_create());
    let mut loc: *mut location = location_create_automatic(
        (*stack).id,
        address,
        ast_expr_constant_create(0 as libc::c_int),
    );
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn stack_create(
    mut name: *mut libc::c_char,
    mut prev: *mut stack,
    mut return_type: *mut ast_type,
) -> *mut stack {
    let mut stack: *mut stack = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<stack>() as libc::c_ulong,
    ) as *mut stack;
    if stack.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 13],
                &[libc::c_char; 13],
            >(b"stack_create\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            41 as libc::c_int,
            b"stack\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    (*stack).name = name;
    (*stack).frame = block_arr_create();
    (*stack).varmap = map_create();
    (*stack).prev = prev;
    (*stack)
        .id = if !prev.is_null() {
        (*prev).id + 1 as libc::c_int
    } else {
        0 as libc::c_int
    };
    (*stack).result = variable_create(return_type, stack, 0 as libc::c_int != 0);
    return stack;
}
#[no_mangle]
pub unsafe extern "C" fn stack_getframe(
    mut s: *mut stack,
    mut frame: libc::c_int,
) -> *mut stack {
    if s.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 15],
                &[libc::c_char; 15],
            >(b"stack_getframe\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            58 as libc::c_int,
            b"s\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    if !(frame >= 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 15],
                &[libc::c_char; 15],
            >(b"stack_getframe\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            59 as libc::c_int,
            b"frame >= 0\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    if (*s).id == frame {
        return s;
    }
    if ((*s).prev).is_null() {
        return 0 as *mut stack;
    }
    return stack_getframe((*s).prev, frame);
}
#[no_mangle]
pub unsafe extern "C" fn stack_destroy(mut stack: *mut stack) {
    block_arr_destroy((*stack).frame);
    let mut m: *mut map = (*stack).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        variable_destroy((*((*m).entry).offset(i as isize)).value as *mut variable);
        i += 1;
        i;
    }
    map_destroy(m);
    variable_destroy((*stack).result);
    free(stack as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn stack_prev(mut s: *mut stack) -> *mut stack {
    return (*s).prev;
}
#[no_mangle]
pub unsafe extern "C" fn stack_copy(mut stack: *mut stack) -> *mut stack {
    let mut copy: *mut stack = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::core::mem::size_of::<stack>() as libc::c_ulong,
    ) as *mut stack;
    (*copy).name = dynamic_str((*stack).name);
    (*copy).frame = block_arr_copy((*stack).frame);
    (*copy).varmap = varmap_copy((*stack).varmap);
    (*copy).id = (*stack).id;
    (*copy).result = variable_copy((*stack).result);
    if !((*stack).prev).is_null() {
        (*copy).prev = stack_copy((*stack).prev);
    }
    return copy;
}
#[no_mangle]
pub unsafe extern "C" fn stack_copywithname(
    mut stack: *mut stack,
    mut new_name: *mut libc::c_char,
) -> *mut stack {
    let mut copy: *mut stack = stack_copy(stack);
    free((*copy).name as *mut libc::c_void);
    (*copy).name = new_name;
    return copy;
}
unsafe extern "C" fn varmap_copy(mut m: *mut map) -> *mut map {
    let mut m_copy: *mut map = map_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        map_set(
            m_copy,
            dynamic_str(e.key),
            variable_copy(e.value as *mut variable) as *const libc::c_void,
        );
        i += 1;
        i;
    }
    return m_copy;
}
#[no_mangle]
pub unsafe extern "C" fn stack_str(
    mut stack: *mut stack,
    mut state: *mut state,
) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut m: *mut map = (*stack).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        let mut var: *mut libc::c_char = variable_str(
            e.value as *mut variable,
            stack,
            state,
        );
        strbuilder_printf(
            b,
            b"\t%s: %s\0" as *const u8 as *const libc::c_char,
            e.key,
            var,
        );
        free(var as *mut libc::c_void);
        strbuilder_putc(b, '\n' as i32 as libc::c_char);
        i += 1;
        i;
    }
    let mut result: *mut libc::c_char = variable_str((*stack).result, stack, state);
    strbuilder_printf(
        b,
        b"\treturn: %s\n\0" as *const u8 as *const libc::c_char,
        result,
    );
    free(result as *mut libc::c_void);
    strbuilder_printf(b, b"\t\0" as *const u8 as *const libc::c_char);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    let mut len: libc::c_int = 30 as libc::c_int;
    while i_0 < len - 2 as libc::c_int {
        strbuilder_putc(b, '-' as i32 as libc::c_char);
        i_0 += 1;
        i_0;
    }
    strbuilder_printf(b, b" %s\n\0" as *const u8 as *const libc::c_char, (*stack).name);
    if !((*stack).prev).is_null() {
        let mut prev: *mut libc::c_char = stack_str((*stack).prev, state);
        strbuilder_printf(b, prev);
        free(prev as *mut libc::c_void);
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe extern "C" fn stack_declare(
    mut stack: *mut stack,
    mut var: *mut ast_variable,
    mut isparam: bool,
) {
    let mut id: *mut libc::c_char = ast_variable_name(var);
    if !(map_get((*stack).varmap, id)).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 14],
                &[libc::c_char; 14],
            >(b"stack_declare\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            167 as libc::c_int,
            b"!map_get(stack->varmap, id)\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    map_set(
        (*stack).varmap,
        dynamic_str(id),
        variable_create(ast_variable_type(var), stack, isparam) as *const libc::c_void,
    );
}
#[no_mangle]
pub unsafe extern "C" fn stack_undeclare(mut stack: *mut stack, mut state: *mut state) {
    let mut old_result: *mut variable = (*stack).result;
    (*stack).result = variable_abstractcopy(old_result, state);
    variable_destroy(old_result);
    let mut m: *mut map = (*stack).varmap;
    (*stack).varmap = map_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        let mut v: *mut variable = e.value as *mut variable;
        if variable_isparam(v) {
            map_set(
                (*stack).varmap,
                dynamic_str(e.key),
                variable_abstractcopy(v, state) as *const libc::c_void,
            );
        }
        variable_destroy(v);
        i += 1;
        i;
    }
    map_destroy(m);
}
#[no_mangle]
pub unsafe extern "C" fn stack_getresult(mut s: *mut stack) -> *mut variable {
    return (*s).result;
}
#[no_mangle]
pub unsafe extern "C" fn stack_getvarmap(mut s: *mut stack) -> *mut map {
    return (*s).varmap;
}
#[no_mangle]
pub unsafe extern "C" fn stack_getvariable(
    mut s: *mut stack,
    mut id: *mut libc::c_char,
) -> *mut variable {
    if !(strcmp(id, b"return\0" as *const u8 as *const libc::c_char) != 0 as libc::c_int)
        as libc::c_int as libc::c_long != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 18],
                &[libc::c_char; 18],
            >(b"stack_getvariable\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            216 as libc::c_int,
            b"strcmp(id, KEYWORD_RETURN) != 0\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    return map_get((*s).varmap, id) as *mut variable;
}
#[no_mangle]
pub unsafe extern "C" fn stack_references(
    mut s: *mut stack,
    mut loc: *mut location,
    mut state: *mut state,
) -> bool {
    let mut result: *mut variable = stack_getresult(s);
    if !result.is_null() && variable_references(result, loc, state) as libc::c_int != 0 {
        return 1 as libc::c_int != 0;
    }
    let mut m: *mut map = (*s).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut var: *mut variable = (*((*m).entry).offset(i as isize)).value
            as *mut variable;
        if variable_isparam(var) as libc::c_int != 0
            && variable_references(var, loc, state) as libc::c_int != 0
        {
            return 1 as libc::c_int != 0;
        }
        i += 1;
        i;
    }
    return 0 as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn stack_getblock(
    mut s: *mut stack,
    mut address: libc::c_int,
) -> *mut block {
    if !(address < block_arr_nblocks((*s).frame)) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 15],
                &[libc::c_char; 15],
            >(b"stack_getblock\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            244 as libc::c_int,
            b"address < block_arr_nblocks(s->frame)\0" as *const u8
                as *const libc::c_char,
        );
    } else {};
    return *(block_arr_blocks((*s).frame)).offset(address as isize);
}
#[no_mangle]
pub unsafe extern "C" fn variable_create(
    mut type_0: *mut ast_type,
    mut stack: *mut stack,
    mut isparam: bool,
) -> *mut variable {
    let mut v: *mut variable = malloc(
        ::core::mem::size_of::<variable>() as libc::c_ulong,
    ) as *mut variable;
    (*v).type_0 = ast_type_copy(type_0);
    (*v).isparam = isparam;
    (*v).loc = stack_newblock(stack);
    let mut res: block_res = location_getblock(
        (*v).loc,
        0 as *mut static_memory,
        0 as *mut vconst,
        stack,
        0 as *mut heap,
        0 as *mut clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<
                    &[u8; 16],
                    &[libc::c_char; 16],
                >(b"variable_create\0"))
                    .as_ptr(),
                b"stack.c\0" as *const u8 as *const libc::c_char,
                268 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {};
    }
    if (res.b).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 16],
                &[libc::c_char; 16],
            >(b"variable_create\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            270 as libc::c_int,
            b"res.b\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    block_install(
        res.b,
        object_value_create(ast_expr_constant_create(0 as libc::c_int), 0 as *mut value),
    );
    return v;
}
#[no_mangle]
pub unsafe extern "C" fn variable_destroy(mut v: *mut variable) {
    ast_type_destroy((*v).type_0);
    location_destroy((*v).loc);
    free(v as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn variable_copy(mut old: *mut variable) -> *mut variable {
    let mut new: *mut variable = malloc(
        ::core::mem::size_of::<variable>() as libc::c_ulong,
    ) as *mut variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    return new;
}
unsafe extern "C" fn variable_abstractcopy(
    mut old: *mut variable,
    mut s: *mut state,
) -> *mut variable {
    let mut new: *mut variable = malloc(
        ::core::mem::size_of::<variable>() as libc::c_ulong,
    ) as *mut variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    let mut res: object_res = state_get(s, (*new).loc, 0 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<
                    &[u8; 22],
                    &[libc::c_char; 22],
                >(b"variable_abstractcopy\0"))
                    .as_ptr(),
                b"stack.c\0" as *const u8 as *const libc::c_char,
                303 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {};
    }
    if (res.obj).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 22],
                &[libc::c_char; 22],
            >(b"variable_abstractcopy\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            305 as libc::c_int,
            b"res.obj\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    if object_isvalue(res.obj) {
        let mut v: *mut value = object_as_value(res.obj);
        if !v.is_null() {
            object_assign(res.obj, value_abstractcopy(v, s));
        }
    }
    return new;
}
#[no_mangle]
pub unsafe extern "C" fn variable_str(
    mut var: *mut variable,
    mut stack: *mut stack,
    mut state: *mut state,
) -> *mut libc::c_char {
    if !(location_type((*var).loc) as libc::c_uint
        != LOCATION_VCONST as libc::c_int as libc::c_uint) as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 13],
                &[libc::c_char; 13],
            >(b"variable_str\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            321 as libc::c_int,
            b"location_type(var->loc) != LOCATION_VCONST\0" as *const u8
                as *const libc::c_char,
        );
    } else {};
    let mut b: *mut strbuilder = strbuilder_create();
    let mut type_0: *mut libc::c_char = ast_type_str((*var).type_0);
    let mut loc: *mut libc::c_char = location_str((*var).loc);
    let mut isparam: *mut libc::c_char = (if (*var).isparam as libc::c_int != 0 {
        b"param \0" as *const u8 as *const libc::c_char
    } else {
        b"\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    let mut obj_str: *mut libc::c_char = object_or_nothing_str((*var).loc, stack, state);
    strbuilder_printf(
        b,
        b"{%s%s := %s} @ %s\0" as *const u8 as *const libc::c_char,
        isparam,
        type_0,
        obj_str,
        loc,
    );
    free(obj_str as *mut libc::c_void);
    free(loc as *mut libc::c_void);
    free(type_0 as *mut libc::c_void);
    return strbuilder_build(b);
}
unsafe extern "C" fn object_or_nothing_str(
    mut loc: *mut location,
    mut stack: *mut stack,
    mut state: *mut state,
) -> *mut libc::c_char {
    let mut b: *mut block = location_getstackblock(loc, stack);
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 22],
                &[libc::c_char; 22],
            >(b"object_or_nothing_str\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            339 as libc::c_int,
            b"b\0" as *const u8 as *const libc::c_char,
        );
    } else {};
    let mut obj: *mut object = block_observe(
        b,
        location_offset(loc),
        state,
        0 as libc::c_int != 0,
    );
    if !obj.is_null() {
        return object_str(obj);
    }
    return dynamic_str(b"\0" as *const u8 as *const libc::c_char);
}
#[no_mangle]
pub unsafe extern "C" fn variable_location(mut v: *mut variable) -> *mut location {
    return (*v).loc;
}
#[no_mangle]
pub unsafe extern "C" fn variable_type(mut v: *mut variable) -> *mut ast_type {
    return (*v).type_0;
}
#[no_mangle]
pub unsafe extern "C" fn variable_references(
    mut v: *mut variable,
    mut loc: *mut location,
    mut s: *mut state,
) -> bool {
    if !(location_type(loc) as libc::c_uint
        != LOCATION_VCONST as libc::c_int as libc::c_uint) as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<
                &[u8; 20],
                &[libc::c_char; 20],
            >(b"variable_references\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            362 as libc::c_int,
            b"location_type(loc) != LOCATION_VCONST\0" as *const u8
                as *const libc::c_char,
        );
    } else {};
    return location_references((*v).loc, loc, s);
}
#[no_mangle]
pub unsafe extern "C" fn variable_isparam(mut v: *mut variable) -> bool {
    return (*v).isparam;
}
