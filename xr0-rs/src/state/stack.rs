#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{calloc, free, malloc, strcmp};

use crate::ast::{
    ast_expr_constant_create, ast_type_copy, ast_type_destroy, ast_type_str, ast_variable_name,
    ast_variable_type,
};
use crate::c_util::__assert_rtn;
use crate::object::{
    object_as_value, object_assign, object_isvalue, object_str, object_value_create,
};
use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_install, block_observe,
};
use crate::state::location::block_res;
use crate::state::location::LOCATION_VCONST;
use crate::state::location::{
    location_copy, location_create_automatic, location_destroy, location_getblock,
    location_getstackblock, location_offset, location_references, location_str, location_type,
};
use crate::state::{object_res, state_get};
use crate::util::{
    dynamic_str, entry, map, map_create, map_destroy, map_get, map_set, strbuilder_build,
    strbuilder_create, strbuilder_printf, strbuilder_putc,
};
use crate::value::{value_abstractcopy, Value};
use crate::{
    ast_type, ast_variable, block_arr, static_memory, vconst, Block, Clump, Heap, Location, Object,
    State, StrBuilder,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Stack {
    pub name: *mut libc::c_char,
    pub frame: *mut block_arr,
    pub varmap: *mut map,
    pub result: *mut Variable,
    pub id: libc::c_int,
    pub prev: *mut Stack,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Variable {
    pub type_0: *mut ast_type,
    pub loc: *mut Location,
    pub isparam: bool,
}

#[no_mangle]
pub unsafe fn stack_newblock(mut Stack: *mut Stack) -> *mut Location {
    let mut address: libc::c_int = block_arr_append((*Stack).frame, block_create());
    let mut loc: *mut Location = location_create_automatic(
        (*Stack).id,
        address,
        ast_expr_constant_create(0 as libc::c_int),
    );
    return loc;
}

#[no_mangle]
pub unsafe fn stack_create(
    mut name: *mut libc::c_char,
    mut prev: *mut Stack,
    mut return_type: *mut ast_type,
) -> *mut Stack {
    let mut Stack: *mut Stack = calloc(1, ::core::mem::size_of::<Stack>()) as *mut Stack;
    if Stack.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"stack_create\0")).as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            41 as libc::c_int,
            b"Stack\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*Stack).name = name;
    (*Stack).frame = block_arr_create();
    (*Stack).varmap = map_create();
    (*Stack).prev = prev;
    (*Stack).id = if !prev.is_null() {
        (*prev).id + 1 as libc::c_int
    } else {
        0 as libc::c_int
    };
    (*Stack).result = variable_create(return_type, Stack, 0 as libc::c_int != 0);
    return Stack;
}

#[no_mangle]
pub unsafe fn stack_getframe(mut s: *mut Stack, mut frame: libc::c_int) -> *mut Stack {
    if s.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"stack_getframe\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            58 as libc::c_int,
            b"s\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !(frame >= 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"stack_getframe\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            59 as libc::c_int,
            b"frame >= 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if (*s).id == frame {
        return s;
    }
    if ((*s).prev).is_null() {
        return 0 as *mut Stack;
    }
    return stack_getframe((*s).prev, frame);
}

#[no_mangle]
pub unsafe fn stack_destroy(mut Stack: *mut Stack) {
    block_arr_destroy((*Stack).frame);
    let mut m: *mut map = (*Stack).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        variable_destroy((*((*m).entry).offset(i as isize)).value as *mut Variable);
        i += 1;
    }
    map_destroy(m);
    variable_destroy((*Stack).result);
    free(Stack as *mut libc::c_void);
}

#[no_mangle]
pub unsafe fn stack_prev(mut s: *mut Stack) -> *mut Stack {
    return (*s).prev;
}

#[no_mangle]
pub unsafe fn stack_copy(mut Stack: *mut Stack) -> *mut Stack {
    let mut copy: *mut Stack = calloc(1, ::core::mem::size_of::<Stack>()) as *mut Stack;
    (*copy).name = dynamic_str((*Stack).name);
    (*copy).frame = block_arr_copy((*Stack).frame);
    (*copy).varmap = varmap_copy((*Stack).varmap);
    (*copy).id = (*Stack).id;
    (*copy).result = variable_copy((*Stack).result);
    if !((*Stack).prev).is_null() {
        (*copy).prev = stack_copy((*Stack).prev);
    }
    return copy;
}

#[no_mangle]
pub unsafe fn stack_copywithname(
    mut Stack: *mut Stack,
    mut new_name: *mut libc::c_char,
) -> *mut Stack {
    let mut copy: *mut Stack = stack_copy(Stack);
    free((*copy).name as *mut libc::c_void);
    (*copy).name = new_name;
    return copy;
}

unsafe fn varmap_copy(mut m: *mut map) -> *mut map {
    let mut m_copy: *mut map = map_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        map_set(
            m_copy,
            dynamic_str(e.key),
            variable_copy(e.value as *mut Variable) as *const libc::c_void,
        );
        i += 1;
    }
    return m_copy;
}

#[no_mangle]
pub unsafe fn stack_str(mut Stack: *mut Stack, mut State: *mut State) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut m: *mut map = (*Stack).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        let mut var: *mut libc::c_char = variable_str(e.value as *mut Variable, Stack, State);
        strbuilder_printf(
            b,
            b"\t%s: %s\0" as *const u8 as *const libc::c_char,
            e.key,
            var,
        );
        free(var as *mut libc::c_void);
        strbuilder_putc(b, '\n' as i32 as libc::c_char);
        i += 1;
    }
    let mut result: *mut libc::c_char = variable_str((*Stack).result, Stack, State);
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
    }
    strbuilder_printf(
        b,
        b" %s\n\0" as *const u8 as *const libc::c_char,
        (*Stack).name,
    );
    if !((*Stack).prev).is_null() {
        let mut prev: *mut libc::c_char = stack_str((*Stack).prev, State);
        strbuilder_printf(b, prev);
        free(prev as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

#[no_mangle]
pub unsafe fn stack_declare(mut Stack: *mut Stack, mut var: *mut ast_variable, mut isparam: bool) {
    let mut id: *mut libc::c_char = ast_variable_name(var);
    if !(map_get((*Stack).varmap, id)).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"stack_declare\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            167 as libc::c_int,
            b"!map_get(Stack->varmap, id)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    map_set(
        (*Stack).varmap,
        dynamic_str(id),
        variable_create(ast_variable_type(var), Stack, isparam) as *const libc::c_void,
    );
}

#[no_mangle]
pub unsafe fn stack_undeclare(mut Stack: *mut Stack, mut State: *mut State) {
    let mut old_result: *mut Variable = (*Stack).result;
    (*Stack).result = variable_abstractcopy(old_result, State);
    variable_destroy(old_result);
    let mut m: *mut map = (*Stack).varmap;
    (*Stack).varmap = map_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        let mut v: *mut Variable = e.value as *mut Variable;
        if variable_isparam(v) {
            map_set(
                (*Stack).varmap,
                dynamic_str(e.key),
                variable_abstractcopy(v, State) as *const libc::c_void,
            );
        }
        variable_destroy(v);
        i += 1;
    }
    map_destroy(m);
}

#[no_mangle]
pub unsafe fn stack_getresult(mut s: *mut Stack) -> *mut Variable {
    return (*s).result;
}

#[no_mangle]
pub unsafe fn stack_getvarmap(mut s: *mut Stack) -> *mut map {
    return (*s).varmap;
}

#[no_mangle]
pub unsafe fn stack_getvariable(mut s: *mut Stack, mut id: *mut libc::c_char) -> *mut Variable {
    if !(strcmp(id, b"return\0" as *const u8 as *const libc::c_char) != 0 as libc::c_int)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"stack_getvariable\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            216 as libc::c_int,
            b"strcmp(id, KEYWORD_RETURN) != 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return map_get((*s).varmap, id) as *mut Variable;
}

#[no_mangle]
pub unsafe fn stack_references(
    mut s: *mut Stack,
    mut loc: *mut Location,
    mut State: *mut State,
) -> bool {
    let mut result: *mut Variable = stack_getresult(s);
    if !result.is_null() && variable_references(result, loc, State) as libc::c_int != 0 {
        return 1 as libc::c_int != 0;
    }
    let mut m: *mut map = (*s).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut var: *mut Variable = (*((*m).entry).offset(i as isize)).value as *mut Variable;
        if variable_isparam(var) as libc::c_int != 0
            && variable_references(var, loc, State) as libc::c_int != 0
        {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}

#[no_mangle]
pub unsafe fn stack_getblock(mut s: *mut Stack, mut address: libc::c_int) -> *mut Block {
    if !(address < block_arr_nblocks((*s).frame)) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"stack_getblock\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            244 as libc::c_int,
            b"address < block_arr_nblocks(s->frame)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return *(block_arr_blocks((*s).frame)).offset(address as isize);
}

#[no_mangle]
pub unsafe fn variable_create(
    mut type_0: *mut ast_type,
    mut Stack: *mut Stack,
    mut isparam: bool,
) -> *mut Variable {
    let mut v: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*v).type_0 = ast_type_copy(type_0);
    (*v).isparam = isparam;
    (*v).loc = stack_newblock(Stack);
    let mut res: block_res = location_getblock(
        (*v).loc,
        0 as *mut static_memory,
        0 as *mut vconst,
        Stack,
        0 as *mut Heap,
        0 as *mut Clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"variable_create\0"))
                    .as_ptr(),
                b"Stack.c\0" as *const u8 as *const libc::c_char,
                268 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    if (res.b).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"variable_create\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            270 as libc::c_int,
            b"res.b\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    block_install(
        res.b,
        object_value_create(ast_expr_constant_create(0 as libc::c_int), 0 as *mut Value),
    );
    return v;
}

#[no_mangle]
pub unsafe fn variable_destroy(mut v: *mut Variable) {
    ast_type_destroy((*v).type_0);
    location_destroy((*v).loc);
    free(v as *mut libc::c_void);
}

#[no_mangle]
pub unsafe fn variable_copy(mut old: *mut Variable) -> *mut Variable {
    let mut new: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    return new;
}

unsafe fn variable_abstractcopy(mut old: *mut Variable, mut s: *mut State) -> *mut Variable {
    let mut new: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    let mut res: object_res = state_get(s, (*new).loc, 0 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 22], &[libc::c_char; 22]>(
                    b"variable_abstractcopy\0",
                ))
                .as_ptr(),
                b"Stack.c\0" as *const u8 as *const libc::c_char,
                303 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    if (res.obj).is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 22], &[libc::c_char; 22]>(b"variable_abstractcopy\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            305 as libc::c_int,
            b"res.obj\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if object_isvalue(res.obj) {
        let mut v: *mut Value = object_as_value(res.obj);
        if !v.is_null() {
            object_assign(res.obj, value_abstractcopy(v, s));
        }
    }
    return new;
}

#[no_mangle]
pub unsafe fn variable_str(
    mut var: *mut Variable,
    mut Stack: *mut Stack,
    mut State: *mut State,
) -> *mut libc::c_char {
    if !(location_type((*var).loc) as libc::c_uint
        != LOCATION_VCONST as libc::c_int as libc::c_uint) as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"variable_str\0")).as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            321 as libc::c_int,
            b"location_type(var->loc) != LOCATION_VCONST\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut type_0: *mut libc::c_char = ast_type_str((*var).type_0);
    let mut loc: *mut libc::c_char = location_str((*var).loc);
    let mut isparam: *mut libc::c_char = (if (*var).isparam as libc::c_int != 0 {
        b"param \0" as *const u8 as *const libc::c_char
    } else {
        b"\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    let mut obj_str: *mut libc::c_char = object_or_nothing_str((*var).loc, Stack, State);
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

unsafe fn object_or_nothing_str(
    mut loc: *mut Location,
    mut Stack: *mut Stack,
    mut State: *mut State,
) -> *mut libc::c_char {
    let mut b: *mut Block = location_getstackblock(loc, Stack);
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 22], &[libc::c_char; 22]>(b"object_or_nothing_str\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            339 as libc::c_int,
            b"b\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut obj: *mut Object = block_observe(b, location_offset(loc), State, 0 as libc::c_int != 0);
    if !obj.is_null() {
        return object_str(obj);
    }
    return dynamic_str(b"\0" as *const u8 as *const libc::c_char);
}

#[no_mangle]
pub unsafe fn variable_location(mut v: *mut Variable) -> *mut Location {
    return (*v).loc;
}

#[no_mangle]
pub unsafe fn variable_type(mut v: *mut Variable) -> *mut ast_type {
    return (*v).type_0;
}

#[no_mangle]
pub unsafe fn variable_references(
    mut v: *mut Variable,
    mut loc: *mut Location,
    mut s: *mut State,
) -> bool {
    if !(location_type(loc) as libc::c_uint != LOCATION_VCONST as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"variable_references\0"))
                .as_ptr(),
            b"Stack.c\0" as *const u8 as *const libc::c_char,
            362 as libc::c_int,
            b"location_type(loc) != LOCATION_VCONST\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return location_references((*v).loc, loc, s);
}

#[no_mangle]
pub unsafe fn variable_isparam(mut v: *mut Variable) -> bool {
    return (*v).isparam;
}
