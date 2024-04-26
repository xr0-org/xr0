use std::ptr;

use super::{Block, State};
use crate::ast::{
    ast_expr_constant_create, ast_type_copy, ast_type_str, ast_variable_name, ast_variable_type,
};
use crate::object::{object_as_value, object_assign, object_isvalue, object_value_create};
use crate::state::block::block_observe;
use crate::state::location::{
    location_auto_getblock, location_copy, location_create_automatic, location_destroy,
    location_getstackblock, location_offset, location_references,
};
use crate::state::state::state_get;
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Map, OwningCStr};
use crate::value::value_abstractcopy;
use crate::{cstr, strbuilder_write, AstType, AstVariable, Location, Object, Value};

pub struct Stack {
    pub name: OwningCStr,
    pub frame: Vec<Box<Block>>,
    pub varmap: Box<Map>,
    pub result: *mut Variable,
    pub id: libc::c_int,
    pub prev: *mut Stack,
}

pub struct Variable {
    pub type_: Box<AstType>,
    pub loc: *mut Location,
    pub is_param: bool,
}

impl Stack {
    pub fn new_block(&mut self) -> Box<Location> {
        let address = self.frame.len() as libc::c_int;
        self.frame.push(Block::new());
        location_create_automatic(self.id, address, ast_expr_constant_create(0))
    }
}

pub unsafe fn stack_create(
    name: OwningCStr,
    prev: *mut Stack,
    return_type: &AstType,
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
    stack.result = Box::into_raw(v);
    Box::into_raw(stack)
}

pub unsafe fn stack_getframe(s: &mut Stack, frame: libc::c_int) -> Option<&mut Stack> {
    assert!(frame >= 0);
    if s.id == frame {
        return Some(s);
    }
    if ((*s).prev).is_null() {
        return None;
    }
    stack_getframe(&mut *(*s).prev, frame)
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
        name: (*stack).name.clone(),
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

pub unsafe fn stack_copywithname(stack: *mut Stack, new_name: OwningCStr) -> *mut Stack {
    let copy: *mut Stack = stack_copy(stack);
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
    let mut b = strbuilder_create();
    let m: &Map = &(*stack).varmap;
    for (k, v) in m.pairs() {
        let var = variable_str(v as *mut Variable, stack, state);
        strbuilder_write!(b, "\t{}: {var}", cstr!(k));
        b.push('\n');
    }
    let result = variable_str((*stack).result, stack, state);
    strbuilder_write!(b, "\treturn: {result}\n");
    strbuilder_write!(b, "\t");
    let mut i_0: libc::c_int = 0 as libc::c_int;
    let len: libc::c_int = 30 as libc::c_int;
    while i_0 < len - 2 as libc::c_int {
        b.push('-');
        i_0 += 1;
    }
    strbuilder_write!(b, " {}\n", (*stack).name);
    if !((*stack).prev).is_null() {
        strbuilder_write!(b, "{}", stack_str((*stack).prev, state));
    }
    strbuilder_build(b)
}

pub unsafe fn stack_declare(stack: *mut Stack, var: &AstVariable, isparam: bool) {
    let id = ast_variable_name(var);
    if !((*stack).varmap.get(id.as_ptr())).is_null() {
        panic!("expected varmap.get(id) to be null");
    }
    (*stack).varmap.set(
        dynamic_str(id.as_ptr()),
        Box::into_raw(variable_create(
            ast_variable_type(var),
            &mut *stack,
            isparam,
        )) as *const libc::c_void,
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

pub unsafe fn stack_getresult(s: &Stack) -> &Variable {
    &*s.result
}

pub unsafe fn stack_getvariable(s: *mut Stack, id: &str) -> *mut Variable {
    assert_ne!(id, "return");
    (*s).varmap.get_by_str(id) as *mut Variable
}

pub unsafe fn stack_references(s: *mut Stack, loc: &Location, state: *mut State) -> bool {
    let result = stack_getresult(&*s);
    if variable_references(result, loc, state) {
        return true;
    }
    let m = &(*s).varmap;
    for p in m.values() {
        let var = p as *mut Variable;
        if variable_isparam(var) && variable_references(&*var, loc, state) {
            return true;
        }
    }
    false
}

pub unsafe fn stack_getblock(s: &mut Stack, address: libc::c_int) -> &mut Block {
    &mut s.frame[address as usize]
}

pub unsafe fn variable_create(type_: &AstType, stack: &mut Stack, isparam: bool) -> Box<Variable> {
    let loc = (*stack).new_block();
    let b = location_auto_getblock(&loc, stack).unwrap();
    b.install(object_value_create(
        ast_expr_constant_create(0),
        ptr::null_mut(),
    ));
    Box::new(Variable {
        type_: ast_type_copy(type_),
        is_param: isparam,
        loc: Box::into_raw(loc),
    })
}

pub unsafe fn variable_destroy(v: *mut Variable) {
    drop(Box::from_raw(v))
}

impl Drop for Variable {
    fn drop(&mut self) {
        unsafe {
            location_destroy(self.loc);
        }
    }
}

pub unsafe fn variable_copy(old: *mut Variable) -> *mut Variable {
    Box::into_raw(Box::new(Variable {
        type_: ast_type_copy(&(*old).type_),
        is_param: (*old).is_param,
        loc: Box::into_raw(location_copy(&*(*old).loc)),
    }))
}

unsafe fn variable_abstractcopy(old: *mut Variable, s: *mut State) -> *mut Variable {
    let new = Box::new(Variable {
        type_: ast_type_copy(&(*old).type_),
        is_param: (*old).is_param,
        loc: Box::into_raw(location_copy(&*(*old).loc)),
    });
    let obj = state_get(s, &*new.loc, false).unwrap();
    if obj.is_null() {
        panic!();
    }
    if object_isvalue(obj) {
        let v: *mut Value = object_as_value(obj);
        if !v.is_null() {
            object_assign(
                &mut *obj,
                value_abstractcopy(&*v, s).map_or(ptr::null_mut(), Box::into_raw),
            );
        }
    }
    Box::into_raw(new)
}

pub unsafe fn variable_str(var: *mut Variable, stack: *mut Stack, state: *mut State) -> OwningCStr {
    assert!(!(*(*var).loc).type_is_vconst());
    let mut b = strbuilder_create();
    let type_ = ast_type_str(&(*var).type_);
    let isparam = if (*var).is_param { "param " } else { "" };
    let obj_str = object_or_nothing_str((*var).loc, stack, state);
    let loc = &*(*var).loc;
    strbuilder_write!(b, "{{{isparam}{type_} := {obj_str}}} @ {loc}");
    strbuilder_build(b)
}

unsafe fn object_or_nothing_str(
    loc: *mut Location,
    stack: *mut Stack,
    state: *mut State,
) -> OwningCStr {
    let b = location_getstackblock(&*loc, &mut *stack);
    let obj: *mut Object = block_observe(b, location_offset(&*loc), state, false);
    if !obj.is_null() {
        return OwningCStr::from(format!("{}", &*obj));
    }
    OwningCStr::empty()
}

pub unsafe fn variable_location(v: &Variable) -> &Location {
    &*v.loc
}

pub unsafe fn variable_type(v: &Variable) -> &AstType {
    &v.type_
}

pub unsafe fn variable_references(v: &Variable, loc: &Location, s: *mut State) -> bool {
    assert!(!loc.type_is_vconst());
    location_references(&*v.loc, loc, s)
}

pub unsafe fn variable_isparam(v: *mut Variable) -> bool {
    (*v).is_param
}
