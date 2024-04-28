use super::{Block, State};
use crate::ast::{
    ast_expr_constant_create, ast_type_copy, ast_type_str, ast_variable_name, ast_variable_type,
    AstExpr,
};
use crate::object::{object_as_value, object_isvalue, object_value_create};
use crate::state::location::{
    location_auto_get_block_id, location_auto_parts, location_create_automatic, location_references,
};
use crate::state::state::state_get;
use crate::util::InsertionOrderMap;
use crate::value::value_abstractcopy;
use crate::{str_write, AstType, AstVariable, Location};

#[derive(Clone)]
pub struct Stack {
    frames: Vec<StackFrame>,
}

type VarMap = InsertionOrderMap<String, Box<Variable>>;

#[derive(Clone)]
pub struct StackFrame {
    pub name: String,
    // Note: This field is called `frame` in the original.
    pub blocks: Vec<Box<Block>>,
    pub varmap: Box<VarMap>,
    pub result: Box<Variable>,
    pub id: usize,
}

#[derive(Clone)]
pub struct Variable {
    type_: Box<AstType>,
    loc: Box<Location>,
    is_param: bool,
}

pub fn stack_str(stack: &Stack, state: &State) -> String {
    let mut b = String::new();
    let n = stack.frames.len();
    for i in 0..n {
        let frame = &stack.frames[i];
        let m: &VarMap = &frame.varmap;
        for (k, v) in m {
            let var = variable_str(v, state);
            str_write!(b, "\t{k}: {var}");
            b.push('\n');
        }
        let result = variable_str(&frame.result, state);
        str_write!(b, "\treturn: {result}\n");
        str_write!(b, "\t");
        for _ in 0..28 {
            b.push('-');
        }
        str_write!(b, " {}\n", frame.name);
    }
    b
}

impl Stack {
    pub fn new() -> Self {
        Stack { frames: vec![] }
    }

    pub fn clone_with_name(&self, new_name: String) -> Stack {
        let mut copy = self.clone();
        copy.top_mut().name = new_name;
        copy
    }

    pub fn get_frame(&mut self, frame: usize) -> Option<&mut StackFrame> {
        self.frames.get_mut(frame)
    }

    pub fn push(&mut self, name: String, return_type: &AstType) -> &mut StackFrame {
        let id = self.frames.len();
        // Note: The original `stack_create` not have to manually lay out all this. Instead,
        // `result` was initially null and it was filled in by calling `variable_create`. In Rust,
        // it is not nullable, so we can't partially initialize the stack frame, then call a method
        // on it to create a variable, then initialize the last field.
        let mut frame = StackFrame {
            name,
            blocks: vec![Block::new()],
            varmap: Box::new(VarMap::new()),
            id,
            result: Box::new(Variable {
                type_: ast_type_copy(return_type),
                loc: location_create_automatic(id, 0, ast_expr_constant_create(0)),
                is_param: false,
            }),
        };
        frame.blocks[0].install(object_value_create(ast_expr_constant_create(0), None));

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

    pub fn references(&self, loc: &Location, state: &mut State) -> bool {
        // Note: Original only checks the top stack frame.
        self.top().references(loc, state)
    }

    pub fn declare(&mut self, var: &AstVariable, is_param: bool) {
        self.top_mut().declare(var, is_param);
    }

    pub fn undeclare(&mut self, state: &mut State) {
        self.top_mut().undeclare(state);
    }

    pub fn get_result(&self) -> &Variable {
        self.top().get_result()
    }

    pub fn get_variable(&self, id: &str) -> Option<&Variable> {
        self.top().get_variable(id)
    }

    pub fn str(&self, state: &State) -> String {
        stack_str(self, state)
    }
}

impl StackFrame {
    pub fn new_block(&mut self) -> Box<Location> {
        let address = self.blocks.len();
        self.blocks.push(Block::new());
        location_create_automatic(self.id, address, ast_expr_constant_create(0))
    }

    pub fn declare(&mut self, var: &AstVariable, isparam: bool) {
        let id = ast_variable_name(var);
        assert!(self.varmap.get(id).is_none());
        let var = variable_create(ast_variable_type(var), self, isparam);
        self.varmap.insert(id.to_string(), var);
    }

    pub fn undeclare(&mut self, state: &mut State) {
        let new_result = variable_abstractcopy(&self.result, state);
        self.result = new_result;
        let m = std::mem::replace(&mut self.varmap, Box::new(VarMap::new()));
        for (k, v) in &*m {
            if v.is_param() {
                self.varmap
                    .insert(k.clone(), variable_abstractcopy(v, state));
            }
        }
    }

    pub fn get_result(&self) -> &Variable {
        &self.result
    }

    pub fn get_variable(&self, id: &str) -> Option<&Variable> {
        assert_ne!(id, "return");
        self.varmap.get(id).map(|boxed| &**boxed)
    }

    pub fn references(&self, loc: &Location, state: &mut State) -> bool {
        if variable_references(&self.result, loc, state) {
            return true;
        }
        for (_id, var) in &*self.varmap {
            if var.is_param() && variable_references(var, loc, state) {
                return true;
            }
        }
        false
    }

    pub fn get_block(&mut self, address: usize) -> &mut Block {
        &mut self.blocks[address]
    }
}

pub fn variable_create(type_: &AstType, frame: &mut StackFrame, isparam: bool) -> Box<Variable> {
    let loc = frame.new_block();
    let block_id = location_auto_get_block_id(&loc);
    let b = &mut frame.blocks[block_id];
    b.install(object_value_create(ast_expr_constant_create(0), None));
    Box::new(Variable {
        type_: ast_type_copy(type_),
        is_param: isparam,
        loc,
    })
}

fn variable_abstractcopy(old: &Variable, s: &mut State) -> Box<Variable> {
    let new = Box::new(Variable {
        type_: old.type_.clone(),
        is_param: old.is_param,
        loc: old.loc.clone(),
    });
    let s: *mut State = s;
    unsafe {
        let obj = state_get(&mut *s, &new.loc, false).unwrap().unwrap();
        if object_isvalue(obj) {
            if let Some(v) = object_as_value(obj) {
                obj.assign(value_abstractcopy(v, &mut *s));
            }
        }
    }
    new
}

pub fn variable_str(var: &Variable, state: &State) -> String {
    let (frame_id, block_id, offset) = location_auto_parts(&var.loc);
    let type_ = ast_type_str(&var.type_);
    let isparam = if var.is_param { "param " } else { "" };
    let obj_str = object_or_nothing_str(state, frame_id, block_id, offset);
    let loc = &var.loc;
    format!("{{{isparam}{type_} := {obj_str}}} @ {loc}")
}

fn object_or_nothing_str(
    state: &State,
    frame_id: usize,
    block_id: usize,
    offset: &AstExpr,
) -> String {
    let b = &state.stack.frames[frame_id].blocks[block_id];
    if let Some(obj) = b.observe_read_only(offset, state) {
        format!("{obj}")
    } else {
        "".to_string()
    }
}

impl Variable {
    pub fn location(&self) -> &Location {
        &self.loc
    }

    pub fn type_(&self) -> &AstType {
        &self.type_
    }

    pub fn is_param(&self) -> bool {
        self.is_param
    }
}

pub fn variable_references(v: &Variable, loc: &Location, s: &mut State) -> bool {
    assert!(!loc.type_is_vconst());
    location_references(&v.loc, loc, s)
}
