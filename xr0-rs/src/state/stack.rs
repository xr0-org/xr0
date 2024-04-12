#![allow(
    dead_code,
    mutable_transmutes,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments
)]

use libc::{calloc, free, malloc, strcmp};

use crate::ast::{
    ast_expr_constant_create, ast_type_copy, ast_type_destroy, ast_type_str, ast_variable_name,
    ast_variable_type,
};
use crate::object::{
    object_as_value, object_assign, object_isvalue, object_str, object_value_create,
};
use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_install, block_observe,
};
use crate::state::location::BlockRes;
use crate::state::location::LOCATION_VCONST;
use crate::state::location::{
    location_copy, location_create_automatic, location_destroy, location_getblock,
    location_getstackblock, location_offset, location_references, location_str, location_type,
};
use crate::state::state::{state_get, ObjectRes};
use crate::util::{
    dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf, strbuilder_putc, Map,
};
use crate::value::value_abstractcopy;
use crate::{
    AstType, AstVariable, Block, BlockArr, Clump, Heap, Location, Object, State, StaticMemory,
    StrBuilder, VConst, Value,
};

pub struct Stack {
    pub name: *mut libc::c_char,
    pub frame: *mut BlockArr,
    pub varmap: Box<Map>,
    pub result: *mut Variable,
    pub id: libc::c_int,
    pub prev: *mut Stack,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Variable {
    pub type_0: *mut AstType,
    pub loc: *mut Location,
    pub isparam: bool,
}

pub unsafe fn stack_newblock(stack: *mut Stack) -> *mut Location {
    let address: libc::c_int = block_arr_append((*stack).frame, block_create());
    let loc: *mut Location = location_create_automatic(
        (*stack).id,
        address,
        ast_expr_constant_create(0 as libc::c_int),
    );
    return loc;
}

pub unsafe fn stack_create(
    name: *mut libc::c_char,
    prev: *mut Stack,
    return_type: *mut AstType,
) -> *mut Stack {
    let stack: *mut Stack = calloc(1, ::core::mem::size_of::<Stack>()) as *mut Stack;
    assert!(!stack.is_null());
    std::ptr::write(
        stack,
        Stack {
            name,
            frame: block_arr_create(),
            varmap: Map::new(),
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

pub unsafe fn stack_getframe(s: *mut Stack, frame: libc::c_int) -> *mut Stack {
    if s.is_null() as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    if !(frame >= 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    if (*s).id == frame {
        return s;
    }
    if ((*s).prev).is_null() {
        return 0 as *mut Stack;
    }
    return stack_getframe((*s).prev, frame);
}

pub unsafe fn stack_destroy(stack: *mut Stack) {
    let stack_val = std::ptr::read(stack);
    block_arr_destroy(stack_val.frame);
    let m = stack_val.varmap;
    for p in m.values() {
        variable_destroy(p as *mut Variable);
    }
    m.destroy();
    variable_destroy(stack_val.result);
    free(stack as *mut libc::c_void);
}

pub unsafe fn stack_prev(s: *mut Stack) -> *mut Stack {
    return (*s).prev;
}

pub unsafe fn stack_copy(stack: *mut Stack) -> *mut Stack {
    let copy: *mut Stack = calloc(1, ::core::mem::size_of::<Stack>()) as *mut Stack;
    std::ptr::write(
        copy,
        Stack {
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
    stack: *mut Stack,
    new_name: *mut libc::c_char,
) -> *mut Stack {
    let copy: *mut Stack = stack_copy(stack);
    free((*copy).name as *mut libc::c_void);
    (*copy).name = new_name;
    return copy;
}

unsafe fn varmap_copy(m: &Map) -> Box<Map> {
    let mut m_copy = Map::new();
    for (k, v) in m.pairs() {
        m_copy.set(
            dynamic_str(k),
            variable_copy(v as *mut Variable) as *const libc::c_void,
        );
    }
    return m_copy;
}

pub unsafe fn stack_str(stack: *mut Stack, state: *mut State) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let m: &Map = &(*stack).varmap;
    for (k, v) in m.pairs() {
        let var: *mut libc::c_char = variable_str(v as *mut Variable, stack, state);
        strbuilder_printf(b, b"\t%s: %s\0" as *const u8 as *const libc::c_char, k, var);
        free(var as *mut libc::c_void);
        strbuilder_putc(b, '\n' as i32 as libc::c_char);
    }
    let result: *mut libc::c_char = variable_str((*stack).result, stack, state);
    strbuilder_printf(
        b,
        b"\treturn: %s\n\0" as *const u8 as *const libc::c_char,
        result,
    );
    free(result as *mut libc::c_void);
    strbuilder_printf(b, b"\t\0" as *const u8 as *const libc::c_char);
    let mut i_0: libc::c_int = 0 as libc::c_int;
    let len: libc::c_int = 30 as libc::c_int;
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
        let prev: *mut libc::c_char = stack_str((*stack).prev, state);
        strbuilder_printf(b, prev);
        free(prev as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

pub unsafe fn stack_declare(stack: *mut Stack, var: *mut AstVariable, isparam: bool) {
    let id: *mut libc::c_char = ast_variable_name(var);
    if !((*stack).varmap.get(id)).is_null() {
        panic!("expected varmap.get(id) to be null");
    }
    (*stack).varmap.set(
        dynamic_str(id),
        variable_create(ast_variable_type(var), stack, isparam) as *const libc::c_void,
    );
}

pub unsafe fn stack_undeclare(stack: *mut Stack, state: *mut State) {
    let old_result: *mut Variable = (*stack).result;
    (*stack).result = variable_abstractcopy(old_result, state);
    variable_destroy(old_result);
    let m = {
        let stack_ref = &mut *stack;
        std::mem::replace(&mut stack_ref.varmap, Map::new())
    };
    for (k, v) in m.pairs() {
        let v = v as *mut Variable;
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

pub unsafe fn stack_getresult(s: *mut Stack) -> *mut Variable {
    return (*s).result;
}

pub unsafe fn stack_getvarmap(s: &mut Stack) -> &mut Map {
    return &mut (*s).varmap;
}

pub unsafe fn stack_getvariable(s: *mut Stack, id: *mut libc::c_char) -> *mut Variable {
    if !(strcmp(id, b"return\0" as *const u8 as *const libc::c_char) != 0 as libc::c_int) {
        panic!();
    }
    return (*s).varmap.get(id) as *mut Variable;
}

pub unsafe fn stack_references(
    s: *mut Stack,
    loc: *mut Location,
    state: *mut State,
) -> bool {
    let result: *mut Variable = stack_getresult(s);
    if !result.is_null() && variable_references(result, loc, state) as libc::c_int != 0 {
        return true;
    }
    let m = &(*s).varmap;
    for p in m.values() {
        let var = p as *mut Variable;
        if variable_isparam(var) as libc::c_int != 0
            && variable_references(var, loc, state) as libc::c_int != 0
        {
            return true;
        }
    }
    false
}

pub unsafe fn stack_getblock(s: *mut Stack, address: libc::c_int) -> *mut Block {
    if !(address < block_arr_nblocks((*s).frame)) as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    return *(block_arr_blocks((*s).frame)).offset(address as isize);
}

pub unsafe fn variable_create(
    type_0: *mut AstType,
    stack: *mut Stack,
    isparam: bool,
) -> *mut Variable {
    let v: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*v).type_0 = ast_type_copy(type_0);
    (*v).isparam = isparam;
    (*v).loc = stack_newblock(stack);
    let res: BlockRes = location_getblock(
        (*v).loc,
        0 as *mut StaticMemory,
        0 as *mut VConst,
        stack,
        0 as *mut Heap,
        0 as *mut Clump,
    );
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            panic!();
        }
    }
    if (res.b).is_null() as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    block_install(
        res.b,
        object_value_create(ast_expr_constant_create(0 as libc::c_int), 0 as *mut Value),
    );
    return v;
}

pub unsafe fn variable_destroy(v: *mut Variable) {
    ast_type_destroy((*v).type_0);
    location_destroy((*v).loc);
    free(v as *mut libc::c_void);
}

pub unsafe fn variable_copy(old: *mut Variable) -> *mut Variable {
    let new: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    return new;
}
unsafe fn variable_abstractcopy(old: *mut Variable, s: *mut State) -> *mut Variable {
    let new: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*new).type_0 = ast_type_copy((*old).type_0);
    (*new).isparam = (*old).isparam;
    (*new).loc = location_copy((*old).loc);
    let res: ObjectRes = state_get(s, (*new).loc, 0 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            panic!();
        }
    }
    if (res.obj).is_null() as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    if object_isvalue(res.obj) {
        let v: *mut Value = object_as_value(res.obj);
        if !v.is_null() {
            object_assign(res.obj, value_abstractcopy(v, s));
        }
    }
    return new;
}

pub unsafe fn variable_str(
    var: *mut Variable,
    stack: *mut Stack,
    state: *mut State,
) -> *mut libc::c_char {
    if !(location_type((*var).loc) as libc::c_uint
        != LOCATION_VCONST as libc::c_int as libc::c_uint) as libc::c_int as libc::c_long
        != 0
    {
        panic!();
    }
    let b: *mut StrBuilder = strbuilder_create();
    let type_0: *mut libc::c_char = ast_type_str((*var).type_0);
    let loc: *mut libc::c_char = location_str((*var).loc);
    let isparam: *mut libc::c_char = (if (*var).isparam as libc::c_int != 0 {
        b"param \0" as *const u8 as *const libc::c_char
    } else {
        b"\0" as *const u8 as *const libc::c_char
    }) as *mut libc::c_char;
    let obj_str: *mut libc::c_char = object_or_nothing_str((*var).loc, stack, state);
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
    loc: *mut Location,
    stack: *mut Stack,
    state: *mut State,
) -> *mut libc::c_char {
    let b: *mut Block = location_getstackblock(loc, stack);
    if b.is_null() as libc::c_int as libc::c_long != 0 {
        panic!();
    }
    let obj: *mut Object = block_observe(b, location_offset(loc), state, 0 as libc::c_int != 0);
    if !obj.is_null() {
        return object_str(obj);
    }
    return dynamic_str(b"\0" as *const u8 as *const libc::c_char);
}

pub unsafe fn variable_location(v: *mut Variable) -> *mut Location {
    return (*v).loc;
}

pub unsafe fn variable_type(v: *mut Variable) -> *mut AstType {
    return (*v).type_0;
}

pub unsafe fn variable_references(
    v: *mut Variable,
    loc: *mut Location,
    s: *mut State,
) -> bool {
    if !(location_type(loc) as libc::c_uint != LOCATION_VCONST as libc::c_int as libc::c_uint) {
        panic!();
    }
    return location_references((*v).loc, loc, s);
}

pub unsafe fn variable_isparam(v: *mut Variable) -> bool {
    return (*v).isparam;
}
