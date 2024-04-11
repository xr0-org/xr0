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
use crate::state::state::{object_res, state_get};
use crate::util::{
    dynamic_str, map, strbuilder_build, strbuilder_create, strbuilder_printf, strbuilder_putc,
};
use crate::value::value_abstractcopy;
use crate::{
    ast_type, ast_variable, block_arr, static_memory, vconst, Block as block, Clump as clump,
    Heap as heap, Location as location, Object as object, State as state, StrBuilder as strbuilder,
    Value as value,
};

pub struct stack {
    pub name: *mut libc::c_char,
    pub frame: *mut block_arr,
    pub varmap: Box<map>,
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

pub unsafe fn stack_newblock(mut stack: *mut stack) -> *mut location {
    let mut address: libc::c_int = block_arr_append((*stack).frame, block_create());
    let mut loc: *mut location = location_create_automatic(
        (*stack).id,
        address,
        ast_expr_constant_create(0 as libc::c_int),
    );
    return loc;
}

pub unsafe fn stack_create(
    mut name: *mut libc::c_char,
    mut prev: *mut stack,
    mut return_type: *mut ast_type,
) -> *mut stack {
    let mut stack: *mut stack = calloc(1, ::core::mem::size_of::<stack>()) as *mut stack;
    assert!(!stack.is_null());
    std::ptr::write(
        stack,
        stack {
            name,
            frame: block_arr_create(),
            varmap: map::new(),
            prev,
            id: if !prev.is_null() {
                (*prev).id + 1 as libc::c_int
            } else {
                0 as libc::c_int
            },
            result: std::ptr::null_mut(),
        },
    );
    (*stack).result = variable_create(return_type, stack, 0 as libc::c_int != 0);
    stack
}

pub unsafe fn stack_getframe(mut s: *mut stack, mut frame: libc::c_int) -> *mut stack {
    if s.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"stack_getframe\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            58 as libc::c_int,
            b"s\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if !(frame >= 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"stack_getframe\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            59 as libc::c_int,
            b"frame >= 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if (*s).id == frame {
        return s;
    }
    if ((*s).prev).is_null() {
        return 0 as *mut stack;
    }
    return stack_getframe((*s).prev, frame);
}

pub unsafe fn stack_destroy(mut stack: *mut stack) {
    let stack_val = std::ptr::read(stack);
    block_arr_destroy(stack_val.frame);
    let mut m = stack_val.varmap;
    for p in m.values() {
        variable_destroy(p as *mut variable);
    }
    m.destroy();
    variable_destroy(stack_val.result);
    free(stack as *mut libc::c_void);
}

pub unsafe fn stack_prev(mut s: *mut stack) -> *mut stack {
    return (*s).prev;
}

pub unsafe fn stack_copy(mut stack: *mut stack) -> *mut stack {
    let mut copy: *mut stack = calloc(1, ::core::mem::size_of::<stack>()) as *mut stack;
    std::ptr::write(
        copy,
        stack {
            name: dynamic_str((*stack).name),
            frame: block_arr_copy((*stack).frame),
            varmap: varmap_copy(&(*stack).varmap),
            id: (*stack).id,
            result: variable_copy((*stack).result),
            prev: std::ptr::null_mut(),
        },
    );
    if !((*stack).prev).is_null() {
        (*copy).prev = stack_copy((*stack).prev);
    }
    return copy;
}

pub unsafe fn stack_copywithname(
    mut stack: *mut stack,
    mut new_name: *mut libc::c_char,
) -> *mut stack {
    let mut copy: *mut stack = stack_copy(stack);
    free((*copy).name as *mut libc::c_void);
    (*copy).name = new_name;
    return copy;
}

unsafe fn varmap_copy(mut m: &map) -> Box<map> {
    let mut m_copy = map::new();
    for (k, v) in m.pairs() {
        m_copy.set(
            dynamic_str(k),
            variable_copy(v as *mut variable) as *const libc::c_void,
        );
    }
    return m_copy;
}

pub unsafe fn stack_str(mut stack: *mut stack, mut state: *mut state) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut m: &map = &(*stack).varmap;
    for (k, v) in m.pairs() {
        let mut var: *mut libc::c_char = variable_str(v as *mut variable, stack, state);
        strbuilder_printf(b, b"\t%s: %s\0" as *const u8 as *const libc::c_char, k, var);
        free(var as *mut libc::c_void);
        strbuilder_putc(b, '\n' as i32 as libc::c_char);
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
    }
    strbuilder_printf(
        b,
        b" %s\n\0" as *const u8 as *const libc::c_char,
        (*stack).name,
    );
    if !((*stack).prev).is_null() {
        let mut prev: *mut libc::c_char = stack_str((*stack).prev, state);
        strbuilder_printf(b, prev);
        free(prev as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

pub unsafe fn stack_declare(mut stack: *mut stack, mut var: *mut ast_variable, mut isparam: bool) {
    let mut id: *mut libc::c_char = ast_variable_name(var);
    if !((*stack).varmap.get(id)).is_null() {
        panic!("expected varmap.get(id) to be null");
    }
    (*stack).varmap.set(
        dynamic_str(id),
        variable_create(ast_variable_type(var), stack, isparam) as *const libc::c_void,
    );
}

pub unsafe fn stack_undeclare(mut stack: *mut stack, mut state: *mut state) {
    let mut old_result: *mut variable = (*stack).result;
    (*stack).result = variable_abstractcopy(old_result, state);
    variable_destroy(old_result);
    let mut m = {
        let stack_ref = &mut *stack;
        std::mem::replace(&mut stack_ref.varmap, map::new())
    };
    for (k, v) in m.pairs() {
        let mut v = v as *mut variable;
        if variable_isparam(v) {
            (*stack).varmap.set(
                dynamic_str(k),
                variable_abstractcopy(v, state) as *const libc::c_void,
            );
        }
        variable_destroy(v);
    }
    m.destroy();
}

pub unsafe fn stack_getresult(mut s: *mut stack) -> *mut variable {
    return (*s).result;
}

pub unsafe fn stack_getvarmap(mut s: &mut stack) -> &mut map {
    return &mut (*s).varmap;
}

pub unsafe fn stack_getvariable(mut s: *mut stack, mut id: *mut libc::c_char) -> *mut variable {
    if !(strcmp(id, b"return\0" as *const u8 as *const libc::c_char) != 0 as libc::c_int)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"stack_getvariable\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            216 as libc::c_int,
            b"strcmp(id, KEYWORD_RETURN) != 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return (*s).varmap.get(id) as *mut variable;
}

pub unsafe fn stack_references(
    mut s: *mut stack,
    mut loc: *mut location,
    mut state: *mut state,
) -> bool {
    let mut result: *mut variable = stack_getresult(s);
    if !result.is_null() && variable_references(result, loc, state) as libc::c_int != 0 {
        return true;
    }
    let mut m = &(*s).varmap;
    for p in m.values() {
        let mut var = p as *mut variable;
        if variable_isparam(var) as libc::c_int != 0
            && variable_references(var, loc, state) as libc::c_int != 0
        {
            return true;
        }
    }
    false
}

pub unsafe fn stack_getblock(mut s: *mut stack, mut address: libc::c_int) -> *mut block {
    if !(address < block_arr_nblocks((*s).frame)) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"stack_getblock\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            244 as libc::c_int,
            b"address < block_arr_nblocks(s->frame)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return *(block_arr_blocks((*s).frame)).offset(address as isize);
}

pub unsafe fn variable_create(
    mut type_0: *mut ast_type,
    mut stack: *mut stack,
    mut isparam: bool,
) -> *mut variable {
    let mut v: *mut variable = malloc(::core::mem::size_of::<variable>()) as *mut variable;
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
                (*::core::mem::transmute::<&[u8; 16], &[libc::c_char; 16]>(b"variable_create\0"))
                    .as_ptr(),
                b"stack.c\0" as *const u8 as *const libc::c_char,
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
            b"stack.c\0" as *const u8 as *const libc::c_char,
            270 as libc::c_int,
            b"res.b\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    block_install(
        res.b,
        object_value_create(ast_expr_constant_create(0 as libc::c_int), 0 as *mut value),
    );
    return v;
}

pub unsafe fn variable_destroy(mut v: *mut variable) {
    ast_type_destroy((*v).type_0);
    location_destroy((*v).loc);
    free(v as *mut libc::c_void);
}

pub unsafe fn variable_copy(mut old: *mut variable) -> *mut variable {
    let mut new: *mut variable = malloc(::core::mem::size_of::<variable>()) as *mut variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    return new;
}
unsafe fn variable_abstractcopy(mut old: *mut variable, mut s: *mut state) -> *mut variable {
    let mut new: *mut variable = malloc(::core::mem::size_of::<variable>()) as *mut variable;
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
                b"stack.c\0" as *const u8 as *const libc::c_char,
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
            b"stack.c\0" as *const u8 as *const libc::c_char,
            305 as libc::c_int,
            b"res.obj\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if object_isvalue(res.obj) {
        let mut v: *mut value = object_as_value(res.obj);
        if !v.is_null() {
            object_assign(res.obj, value_abstractcopy(v, s));
        }
    }
    return new;
}

pub unsafe fn variable_str(
    mut var: *mut variable,
    mut stack: *mut stack,
    mut state: *mut state,
) -> *mut libc::c_char {
    if !(location_type((*var).loc) as libc::c_uint
        != LOCATION_VCONST as libc::c_int as libc::c_uint) as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"variable_str\0")).as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            321 as libc::c_int,
            b"location_type(var->loc) != LOCATION_VCONST\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
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
unsafe fn object_or_nothing_str(
    mut loc: *mut location,
    mut stack: *mut stack,
    mut state: *mut state,
) -> *mut libc::c_char {
    let mut b: *mut block = location_getstackblock(loc, stack);
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 22], &[libc::c_char; 22]>(b"object_or_nothing_str\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            339 as libc::c_int,
            b"b\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut obj: *mut object = block_observe(b, location_offset(loc), state, 0 as libc::c_int != 0);
    if !obj.is_null() {
        return object_str(obj);
    }
    return dynamic_str(b"\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn variable_location(mut v: *mut variable) -> *mut location {
    return (*v).loc;
}

pub unsafe fn variable_type(mut v: *mut variable) -> *mut ast_type {
    return (*v).type_0;
}

pub unsafe fn variable_references(
    mut v: *mut variable,
    mut loc: *mut location,
    mut s: *mut state,
) -> bool {
    if !(location_type(loc) as libc::c_uint != LOCATION_VCONST as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 20], &[libc::c_char; 20]>(b"variable_references\0"))
                .as_ptr(),
            b"stack.c\0" as *const u8 as *const libc::c_char,
            362 as libc::c_int,
            b"location_type(loc) != LOCATION_VCONST\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return location_references((*v).loc, loc, s);
}

pub unsafe fn variable_isparam(mut v: *mut variable) -> bool {
    return (*v).isparam;
}
