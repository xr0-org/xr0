#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use libc::{free, malloc, strcmp};

use crate::ast::{
    ast_expr_constant_create, ast_type_copy, ast_type_destroy, ast_type_str, ast_variable_name,
    ast_variable_type,
};
use crate::object::{
    object_as_value, object_assign, object_isvalue, object_str, object_value_create,
};
use crate::state::block::{block_create, block_install, block_observe};
use crate::state::location::{
    location_auto_getblock, location_copy, location_create_automatic, location_destroy,
    location_getstackblock, location_offset, location_references, location_str,
};
use crate::state::state::state_get;
use crate::util::{
    dynamic_str, strbuilder_build, strbuilder_create, strbuilder_putc, Map, OwningCStr,
};
use crate::value::value_abstractcopy;
use crate::{
    cstr, strbuilder_write, AstType, AstVariable, Block, Location, Object, State, StrBuilder, Value,
};

pub struct Stack {
    pub name: *mut libc::c_char,
    pub frame: Vec<Box<Block>>,
    pub varmap: Box<Map>,
    pub result: *mut Variable,
    pub id: libc::c_int,
    pub prev: *mut Stack,
}

pub struct Variable {
    pub r#type: *mut AstType,
    pub loc: *mut Location,
    pub is_param: bool,
}

pub unsafe fn stack_newblock(stack: *mut Stack) -> *mut Location {
    let address = (*stack).frame.len() as libc::c_int;
    (*stack).frame.push(Box::from_raw(block_create()));
    let loc: *mut Location = location_create_automatic(
        (*stack).id,
        address,
        ast_expr_constant_create(0 as libc::c_int),
    );
    loc
}

pub unsafe fn stack_create(
    name: *mut libc::c_char,
    prev: *mut Stack,
    return_type: *mut AstType,
) -> *mut Stack {
    let mut stack = Box::new(Stack {
        name,
        frame: vec![],
        varmap: Map::new(),
        prev,
        id: if !prev.is_null() { (*prev).id + 1 } else { 0 },
        result: ptr::null_mut(),
    });
    let v = variable_create(return_type, &mut *stack, false);
    stack.result = v;
    Box::into_raw(stack)
}

pub unsafe fn stack_getframe(s: *mut Stack, frame: libc::c_int) -> *mut Stack {
    if s.is_null() {
        panic!();
    }
    assert!(frame >= 0);
    if (*s).id == frame {
        return s;
    }
    if ((*s).prev).is_null() {
        return ptr::null_mut();
    }
    stack_getframe((*s).prev, frame)
}

pub unsafe fn stack_destroy(stack: *mut Stack) {
    drop(Box::from_raw(stack));
}

impl Drop for Stack {
    fn drop(&mut self) {
        unsafe {
            let m = std::mem::replace(&mut self.varmap, Map::new());
            for p in m.values() {
                variable_destroy(p as *mut Variable);
            }
            m.destroy();
            variable_destroy(self.result);
        }
    }
}

pub unsafe fn stack_prev(s: *mut Stack) -> *mut Stack {
    (*s).prev
}

pub unsafe fn stack_copy(stack: *mut Stack) -> *mut Stack {
    Box::into_raw(Box::new(Stack {
        name: dynamic_str((*stack).name),
        frame: (*stack).frame.clone(),
        varmap: varmap_copy(&(*stack).varmap),
        id: (*stack).id,
        result: variable_copy((*stack).result),
        prev: if ((*stack).prev).is_null() {
            ptr::null_mut()
        } else {
            stack_copy((*stack).prev)
        },
    }))
}

pub unsafe fn stack_copywithname(stack: *mut Stack, new_name: *mut libc::c_char) -> *mut Stack {
    let copy: *mut Stack = stack_copy(stack);
    free((*copy).name as *mut libc::c_void);
    (*copy).name = new_name;
    copy
}

unsafe fn varmap_copy(m: &Map) -> Box<Map> {
    let mut m_copy = Map::new();
    for (k, v) in m.pairs() {
        m_copy.set(
            dynamic_str(k),
            variable_copy(v as *mut Variable) as *const libc::c_void,
        );
    }
    m_copy
}

pub unsafe fn stack_str(stack: *mut Stack, state: *mut State) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    let m: &Map = &(*stack).varmap;
    for (k, v) in m.pairs() {
        let var = variable_str(v as *mut Variable, stack, state);
        strbuilder_write!(b, "\t{}: {var}", cstr!(k));
        strbuilder_putc(b, '\n' as i32 as libc::c_char);
    }
    let result = variable_str((*stack).result, stack, state);
    strbuilder_write!(b, "\treturn: {result}\n");
    strbuilder_write!(b, "\t");
    let mut i_0: libc::c_int = 0 as libc::c_int;
    let len: libc::c_int = 30 as libc::c_int;
    while i_0 < len - 2 as libc::c_int {
        strbuilder_putc(b, '-' as i32 as libc::c_char);
        i_0 += 1;
    }
    strbuilder_write!(b, " {}\n", cstr!((*stack).name));
    if !((*stack).prev).is_null() {
        strbuilder_write!(b, "{}", stack_str((*stack).prev, state));
    }
    strbuilder_build(b)
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
    (*s).result
}

pub unsafe fn stack_getvariable(s: *mut Stack, id: *mut libc::c_char) -> *mut Variable {
    assert!(strcmp(id, b"return\0" as *const u8 as *const libc::c_char) != 0);
    (*s).varmap.get(id) as *mut Variable
}

pub unsafe fn stack_references(s: *mut Stack, loc: &Location, state: *mut State) -> bool {
    let result: *mut Variable = stack_getresult(s);
    if !result.is_null() && variable_references(result, loc, state) {
        return true;
    }
    let m = &(*s).varmap;
    for p in m.values() {
        let var = p as *mut Variable;
        if variable_isparam(var) && variable_references(var, loc, state) {
            return true;
        }
    }
    false
}

pub unsafe fn stack_getblock(s: *mut Stack, address: libc::c_int) -> *mut Block {
    assert!((address as usize) < (*s).frame.len());
    &mut *(*s).frame[address as usize]
}

pub unsafe fn variable_create(
    type_0: *mut AstType,
    stack: *mut Stack,
    isparam: bool,
) -> *mut Variable {
    let v: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*v).r#type = ast_type_copy(type_0);
    (*v).is_param = isparam;
    (*v).loc = stack_newblock(stack);
    let b = location_auto_getblock(&*(*v).loc, stack).unwrap();
    if b.is_null() {
        panic!();
    }
    block_install(
        b,
        object_value_create(
            Box::into_raw(ast_expr_constant_create(0 as libc::c_int)),
            ptr::null_mut(),
        ),
    );
    v
}

pub unsafe fn variable_destroy(v: *mut Variable) {
    ast_type_destroy((*v).r#type);
    location_destroy((*v).loc);
    free(v as *mut libc::c_void);
}

pub unsafe fn variable_copy(old: *mut Variable) -> *mut Variable {
    let new: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*new).r#type = ast_type_copy((*old).r#type);
    (*new).is_param = (*old).is_param;
    (*new).loc = location_copy(&*(*old).loc);
    new
}

unsafe fn variable_abstractcopy(old: *mut Variable, s: *mut State) -> *mut Variable {
    let new: *mut Variable = malloc(::core::mem::size_of::<Variable>()) as *mut Variable;
    (*new).r#type = ast_type_copy((*old).r#type);
    (*new).is_param = (*old).is_param;
    (*new).loc = location_copy(&*(*old).loc);
    let obj = state_get(s, &*(*new).loc, false).unwrap();
    if obj.is_null() {
        panic!();
    }
    if object_isvalue(obj) {
        let v: *mut Value = object_as_value(obj);
        if !v.is_null() {
            object_assign(obj, value_abstractcopy(&*v, s));
        }
    }
    new
}

pub unsafe fn variable_str(var: *mut Variable, stack: *mut Stack, state: *mut State) -> OwningCStr {
    assert!(!(*(*var).loc).type_is_vconst());
    let b: *mut StrBuilder = strbuilder_create();
    let type_0 = ast_type_str((*var).r#type);
    let isparam = if (*var).is_param { "param " } else { "" };
    let obj_str = object_or_nothing_str((*var).loc, stack, state);
    let loc = location_str(&*(*var).loc);
    strbuilder_write!(b, "{{{isparam}{type_0} := {obj_str}}} @ {loc}");
    strbuilder_build(b)
}

unsafe fn object_or_nothing_str(
    loc: *mut Location,
    stack: *mut Stack,
    state: *mut State,
) -> OwningCStr {
    let b: *mut Block = location_getstackblock(loc, stack);
    if b.is_null() {
        panic!();
    }
    let obj: *mut Object = block_observe(b, location_offset(&*loc), state, false);
    if !obj.is_null() {
        return object_str(obj);
    }
    OwningCStr::empty()
}

pub unsafe fn variable_location(v: *mut Variable) -> *mut Location {
    (*v).loc
}

pub unsafe fn variable_type(v: *mut Variable) -> *mut AstType {
    (*v).r#type
}

pub unsafe fn variable_references(v: *mut Variable, loc: &Location, s: *mut State) -> bool {
    assert!(!(*loc).type_is_vconst());
    location_references(&*(*v).loc, loc, s)
}

pub unsafe fn variable_isparam(v: *mut Variable) -> bool {
    (*v).is_param
}
