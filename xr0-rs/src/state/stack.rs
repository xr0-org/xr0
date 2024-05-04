use super::{Block, State};
use crate::ast::{
    ast_stmt_absprocess, ast_stmt_ispre, ast_stmt_isterminal, ast_stmt_process, ast_type_copy,
    AstBlock, AstExpr,
};
use crate::object::Object;
use crate::state::location::{
    location_auto_get_block_id, location_auto_parts, location_references,
};
use crate::state::state::state_declare;
use crate::util::{InsertionOrderMap, Result};
use crate::{str_write, AstType, AstVariable, Location};

#[derive(Clone)]
pub struct Stack<'a> {
    pub(super) frames: Vec<StackFrame<'a>>,
}

type VarMap = InsertionOrderMap<String, Box<Variable>>;

#[derive(Clone)]
pub struct StackFrame<'a> {
    pub pc: Box<ProgramCounter<'a>>,
    pub abstract_: bool,

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

#[derive(Clone)]
pub struct ProgramCounter<'a> {
    b: &'a AstBlock,
    s: ProgramCounterState,
    name: String,
    index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramCounterState {
    Decls,
    Stmts,
    AtEnd,
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
        str_write!(b, " {}\n", frame.pc.name);
    }
    b
}

impl<'a> Stack<'a> {
    pub fn new() -> Self {
        Stack { frames: vec![] }
    }

    //=stack_copywithname
    pub fn clone_with_name(&self, new_name: String) -> Stack<'a> {
        let mut copy = self.clone();
        copy.top_mut().pc.name = new_name;
        copy
    }

    pub fn get_frame(&self, frame: usize) -> Option<&StackFrame<'a>> {
        self.frames.get(frame)
    }

    pub fn get_frame_mut(&mut self, frame: usize) -> Option<&mut StackFrame<'a>> {
        self.frames.get_mut(frame)
    }

    pub fn push(
        &mut self,
        name: String,
        b: &'a AstBlock,
        ret_type: &AstType,
        abstract_: bool,
    ) -> &mut StackFrame<'a> {
        let id = self.frames.len();
        // Note: The original `stack_create` does not manually lay all this out. Instead, `result`
        // was initially null and it was filled in by calling `variable_create`. In Rust, it is not
        // nullable, so we can't partially initialize the stack frame, then call a method on it to
        // create a variable, then initialize the last field.
        let mut frame = StackFrame {
            pc: ProgramCounter::new(b, &name),
            abstract_,
            blocks: vec![Block::new()],
            varmap: Box::new(VarMap::new()),
            id,
            result: Box::new(Variable {
                type_: ast_type_copy(ret_type),
                loc: Location::new_automatic(id, 0, AstExpr::new_constant(0)),
                is_param: false,
            }),
        };
        frame.blocks[0].install(Object::with_value(AstExpr::new_constant(0), None));

        self.frames.push(frame);
        self.frames.last_mut().unwrap()
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn top(&self) -> &StackFrame<'a> {
        self.frames.last().unwrap()
    }

    fn top_mut(&mut self) -> &mut StackFrame<'a> {
        self.frames.last_mut().unwrap()
    }

    pub fn references(&self, loc: &Location, state: &State) -> bool {
        // Note: Original only checks the top stack frame.
        self.top().references(loc, state)
    }

    pub fn declare(&mut self, var: &AstVariable, is_param: bool) {
        self.top_mut().declare(var, is_param);
    }

    //=stack_undeclare
    pub fn undeclare(state: &mut State) {
        // Note: The clone here is not in the original. Necessary to convince Rust that
        // variable_abstractcopy's mut use of state will not invalidate it.
        let old_result = state.stack.top().result.clone();
        let new_result = variable_abstractcopy(&old_result, state);
        state.stack.top_mut().result = new_result;

        // Note: the extra empty box is not in the original. It's necessary to move the varmap out
        // of State before Rust will let us use `variable_abstractcopy` in safe code.
        let m = std::mem::replace(&mut state.stack.top_mut().varmap, Box::new(VarMap::new()));

        let mut new_map = Box::new(VarMap::new());
        for (k, v) in &*m {
            if v.is_param() {
                new_map.insert(k.clone(), variable_abstractcopy(v, &mut *state));
            }
        }
        state.stack.top_mut().varmap = new_map;
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

    //=stack_atend
    pub fn at_end(&self) -> bool {
        self.frames.len() == 1 && self.frames[0].pc.at_end()
    }
}

impl<'a> StackFrame<'a> {
    pub fn name(&self) -> &str {
        &self.pc.name
    }

    pub fn new_block(&mut self) -> Box<Location> {
        let address = self.blocks.len();
        self.blocks.push(Block::new());
        Location::new_automatic(self.id, address, AstExpr::new_constant(0))
    }

    pub fn declare(&mut self, var: &AstVariable, isparam: bool) {
        let id = &var.name;
        assert!(self.varmap.get(id).is_none());
        let var = variable_create(&var.type_, self, isparam);
        self.varmap.insert(id.to_string(), var);
    }

    pub fn get_result(&self) -> &Variable {
        &self.result
    }

    pub fn get_variable(&self, id: &str) -> Option<&Variable> {
        assert_ne!(id, "return");
        self.varmap.get(id).map(|boxed| &**boxed)
    }

    pub fn references(&self, loc: &Location, state: &State) -> bool {
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

    pub fn get_block(&self, address: usize) -> &Block {
        &self.blocks[address]
    }

    pub fn get_block_mut(&mut self, address: usize) -> &mut Block {
        &mut self.blocks[address]
    }
}

pub fn variable_create(type_: &AstType, frame: &mut StackFrame, isparam: bool) -> Box<Variable> {
    let loc = frame.new_block();
    let block_id = location_auto_get_block_id(&loc);
    let b = &mut frame.blocks[block_id];
    b.install(Object::with_value(AstExpr::new_constant(0), None));
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

    let obj = s.get(&new.loc).unwrap().unwrap();
    if obj.is_value() {
        if let Some(v) = obj.as_value() {
            let v = v.abstract_copy(s);
            // Note: The repeated "get" on the next line is not in the original. Rust requires it
            // to upgrade from our non-mut reference to a mut reference. (If we asked for a mut
            // reference the first time, it would be invalidated by the use of `s` in
            // `Value::abstract_copy`.)
            let obj = s.get_mut(&new.loc, false).unwrap().unwrap();
            obj.assign(v);
        }
    }

    new
}

pub fn variable_str(var: &Variable, state: &State) -> String {
    let (frame_id, block_id, offset) = location_auto_parts(&var.loc);
    let type_ = &var.type_;
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

pub fn variable_references(v: &Variable, loc: &Location, s: &State) -> bool {
    assert!(!loc.type_is_vconst());
    location_references(&v.loc, loc, s)
}

impl<'a> ProgramCounter<'a> {
    //=program_counter_create
    fn new(b: &'a AstBlock, name: &str) -> Box<Self> {
        Box::new(ProgramCounter {
            b,
            s: ProgramCounter::state_init(b),
            index: 0,
            name: name.to_string(),
        })
    }

    //=program_counter_state_init
    fn state_init(b: &'a AstBlock) -> ProgramCounterState {
        if !b.decls.is_empty() {
            ProgramCounterState::Decls
        } else if !b.stmts.is_empty() {
            ProgramCounterState::Stmts
        } else {
            ProgramCounterState::AtEnd
        }
    }

    //=program_counter_atend
    pub fn at_end(&self) -> bool {
        self.s == ProgramCounterState::AtEnd
    }

    //=program_counter_nextdecl
    pub fn next_decl(&mut self) {
        assert_eq!(self.s, ProgramCounterState::Decls);

        self.index += 1;
        if self.index >= self.b.decls.len() {
            self.s = if !self.b.stmts.is_empty() {
                ProgramCounterState::Stmts
            } else {
                ProgramCounterState::AtEnd
            };
            self.index = 0;
        }
    }

    //=program_counter_stmt_atend
    fn stmt_at_end(s: &mut State) -> bool {
        let pc = &mut s.stack.frames.last_mut().unwrap().pc;
        let stmt = &pc.b.stmts[pc.index];
        ast_stmt_isterminal(stmt, s) || {
            let pc = &mut s.stack.frames.last_mut().unwrap().pc;
            pc.index += 1;
            pc.index >= pc.b.stmts.len()
        }
    }

    //=program_counter_exec
    pub fn exec(s: &mut State) -> Result<()> {
        let frame = s.stack.frames.last_mut().unwrap();
        let abstract_ = frame.abstract_;
        let pc = &mut frame.pc;
        match pc.s {
            ProgramCounterState::Decls => {
                let decl = &pc.b.decls[pc.index];
                state_declare(s, decl, false);
                s.stack.frames.last_mut().unwrap().pc.next_decl();
                Ok(())
            }
            ProgramCounterState::Stmts => {
                //=program_counter_stmt_step
                //=program_counter_stmt_process
                let stmt = &pc.b.stmts[pc.index];
                if abstract_ {
                    if !ast_stmt_ispre(stmt) {
                        ast_stmt_absprocess(stmt, s, true)?;
                    }
                } else {
                    let name = pc.name.clone();
                    ast_stmt_process(stmt, &name, s)?
                }

                //=program_counter_nextstmt
                if Self::stmt_at_end(s) {
                    let pc = &mut s.stack.frames.last_mut().unwrap().pc;
                    pc.s = ProgramCounterState::AtEnd;
                }
                Ok(())
            }
            ProgramCounterState::AtEnd => panic!(),
        }
    }
}
