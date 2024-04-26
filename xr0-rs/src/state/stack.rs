use std::ptr;

use super::{Block, State};
use crate::ast::{
    ast_expr_constant_create, ast_type_copy, ast_type_str, ast_variable_name, ast_variable_type,
};
use crate::object::{object_as_value, object_assign, object_isvalue, object_value_create};
use crate::state::location::{
    location_auto_get_block_id, location_auto_getblock, location_copy, location_create_automatic,
    location_destroy, location_offset, location_references,
};
use crate::state::state::state_get;
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Map, OwningCStr};
use crate::value::value_abstractcopy;
use crate::{cstr, strbuilder_write, AstType, AstVariable, Location};

#[derive(Clone)]
pub struct Stack {
    frames: Vec<StackFrame>,
}

pub struct StackFrame {
    pub name: OwningCStr,
    // Note: This field is called `frame` in the original.
    pub blocks: Vec<Box<Block>>,
    pub varmap: Box<Map>,
    pub result: *mut Variable,
    pub id: libc::c_int,
}

pub struct Variable {
    pub type_: Box<AstType>,
    pub loc: *mut Location,
    pub is_param: bool,
}

impl Drop for StackFrame {
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

impl Clone for StackFrame {
    fn clone(&self) -> Self {
        unsafe {
            StackFrame {
                name: self.name.clone(),
                blocks: self.blocks.clone(),
                varmap: varmap_copy(&self.varmap),
                id: self.id,
                result: variable_copy(self.result),
            }
        }
    }
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
    let n = (*stack).frames.len();
    for i in 0..n {
        let frame: *mut StackFrame = &mut (*stack).frames[i];
        let m: &Map = &(*frame).varmap;
        for (k, v) in m.pairs() {
            let var = variable_str(v as *mut Variable, stack, state);
            strbuilder_write!(b, "\t{}: {var}", cstr!(k));
            b.push('\n');
        }
        let result = variable_str((*frame).result, stack, state);
        strbuilder_write!(b, "\treturn: {result}\n");
        strbuilder_write!(b, "\t");
        let mut i_0: libc::c_int = 0 as libc::c_int;
        let len: libc::c_int = 30 as libc::c_int;
        while i_0 < len - 2 as libc::c_int {
            b.push('-');
            i_0 += 1;
        }
        strbuilder_write!(b, " {}\n", (*frame).name);
    }
    strbuilder_build(b)
}

impl Stack {
    pub fn new() -> Self {
        Stack { frames: vec![] }
    }

    pub fn clone_with_name(&self, new_name: OwningCStr) -> Stack {
        let mut copy = self.clone();
        copy.top_mut().name = new_name;
        copy
    }

    pub fn get_frame(&mut self, frame: libc::c_int) -> Option<&mut StackFrame> {
        self.frames.get_mut(frame as usize)
    }

    pub fn push(&mut self, name: OwningCStr, return_type: &AstType) -> &mut StackFrame {
        let id = self.frames.len() as libc::c_int;
        let mut frame = StackFrame {
            name,
            blocks: vec![],
            varmap: Map::new(),
            id,
            result: ptr::null_mut(),
        };
        let v = unsafe { variable_create(return_type, &mut frame, false) };
        frame.result = Box::into_raw(v);
        self.frames.push(frame);
        self.frames.last_mut().unwrap()
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn top(&self) -> &StackFrame {
        self.frames.last().unwrap()
    }

    fn top_mut(&mut self) -> &mut StackFrame {
        self.frames.last_mut().unwrap()
    }

    pub unsafe fn references(&self, loc: &Location, state: *mut State) -> bool {
        // Note: Original only checks the top stack frame.
        self.top().references(loc, state)
    }

    pub unsafe fn declare(&mut self, var: &AstVariable, is_param: bool) {
        self.top_mut().declare(var, is_param);
    }

    pub unsafe fn undeclare(&mut self, state: *mut State) {
        self.top_mut().undeclare(state);
    }

    pub unsafe fn get_result(&self) -> &Variable {
        self.top().get_result()
    }

    pub unsafe fn get_variable(&self, id: &str) -> *mut Variable {
        self.top().get_variable(id)
    }

    pub unsafe fn str(&mut self, state: *mut State) -> OwningCStr {
        stack_str(self, state)
    }
}

impl StackFrame {
    pub fn new_block(&mut self) -> Box<Location> {
        let address = self.blocks.len() as libc::c_int;
        self.blocks.push(Block::new());
        location_create_automatic(self.id, address, ast_expr_constant_create(0))
    }

    pub unsafe fn declare(&mut self, var: &AstVariable, isparam: bool) {
        let id = ast_variable_name(var);
        if !(self.varmap.get(id.as_ptr())).is_null() {
            panic!("expected varmap.get(id) to be null");
        }
        let var = variable_create(ast_variable_type(var), self, isparam);
        self.varmap.set(
            dynamic_str(id.as_ptr()),
            Box::into_raw(var) as *const libc::c_void,
        );
    }

    pub unsafe fn undeclare(&mut self, state: *mut State) {
        let old_result: *mut Variable = self.result;
        self.result = variable_abstractcopy(old_result, state);
        variable_destroy(old_result);
        let m = std::mem::replace(&mut self.varmap, Map::new());
        for (k, v) in m.pairs() {
            let v = v as *mut Variable;
            if variable_isparam(v) {
                self.varmap.set(
                    dynamic_str(k),
                    variable_abstractcopy(v, state) as *const libc::c_void,
                );
            }
            variable_destroy(v);
        }
        m.destroy();
    }

    pub unsafe fn get_result(&self) -> &Variable {
        &*self.result
    }

    pub unsafe fn get_variable(&self, id: &str) -> *mut Variable {
        assert_ne!(id, "return");
        self.varmap.get_by_str(id) as *mut Variable
    }

    pub unsafe fn references(&self, loc: &Location, state: *mut State) -> bool {
        if variable_references(&*self.result, loc, state) {
            return true;
        }
        for p in self.varmap.values() {
            let var = p as *mut Variable;
            if variable_isparam(var) && variable_references(&*var, loc, state) {
                return true;
            }
        }
        false
    }

    pub fn get_block(&mut self, address: libc::c_int) -> &mut Block {
        &mut self.blocks[address as usize]
    }
}

pub unsafe fn variable_create(
    type_: &AstType,
    frame: &mut StackFrame,
    isparam: bool,
) -> Box<Variable> {
    let loc = frame.new_block();
    let block_id = location_auto_get_block_id(&loc);
    let b = &mut frame.blocks[block_id as usize];
    b.install(object_value_create(ast_expr_constant_create(0), None));
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
    if object_isvalue(&*obj) {
        if let Some(v) = object_as_value(&*obj) {
            object_assign(&mut *obj, value_abstractcopy(v, s));
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
    let b = location_auto_getblock(&*loc, &mut *stack).unwrap();
    if let Some(obj) = b.observe(location_offset(&*loc), state, false) {
        format!("{obj}").into()
    } else {
        OwningCStr::empty()
    }
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
