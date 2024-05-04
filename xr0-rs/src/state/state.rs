use super::location::{location_copy, location_dealloc, location_toheap, LocationKind};
use super::{Block, Clump, Heap, ProgramCounter, Stack, StaticMemory, VConst};
use crate::ast::{
    ast_type_vconst, AstBlock, AstExpr, AstType, AstVariable, LValue, KEYWORD_RETURN,
};
use crate::util::{Error, Result};
use crate::value::value_issync;
use crate::{str_write, vprint, Externals, Location, Object, Props, Value};

/// The entire state of the abstract machine.
///
/// The state consists of three parts:
/// - The parsed C program.
/// - The abstract state of memory (heap, stack, and so on).
/// - A collection of propositions that we are for the moment assuming to be true.
#[derive(Clone)]
pub struct State<'a> {
    pub ext: &'a Externals,
    pub vconst: VConst,
    pub static_memory: StaticMemory,
    pub clump: Clump,
    pub stack: Stack<'a>,
    pub heap: Heap,
    pub props: Props,
}

pub fn state_create<'a>(
    name: String,
    block: &'a AstBlock,
    ret_type: &AstType,
    abstract_: bool,
    ext: &'a Externals,
) -> Box<State<'a>> {
    let mut stack = Stack::new();
    stack.push(name, block, ret_type, abstract_);
    Box::new(State {
        ext,
        static_memory: StaticMemory::new(),
        vconst: VConst::new(),
        clump: Clump::new(),
        stack,
        heap: Heap::new(),
        props: Props::new(),
    })
}

pub fn state_create_withprops<'a>(
    name: String,
    block: &'a AstBlock,
    ret_type: &'a AstType,
    abstract_: bool,
    ext: &'a Externals,
    props: Props,
) -> Box<State<'a>> {
    let mut stack = state_create(name, block, ret_type, abstract_, ext);
    stack.props = props;
    stack
}

pub fn state_copy<'a>(state: &State<'a>) -> Box<State<'a>> {
    Box::new(state.clone())
}

pub fn state_copywithname<'a>(state: &State<'a>, func_name: String) -> Box<State<'a>> {
    let mut copy = state_copy(state);
    copy.stack = state.stack.clone_with_name(func_name);
    copy
}

pub fn state_str(state: &State) -> String {
    let mut b = String::new();
    str_write!(b, "[[\n");
    let ext = (*state.ext).types_str("\t");
    if !ext.is_empty() {
        str_write!(b, "{ext}\n");
    }
    let static_mem = state.static_memory.str("\t");
    if !static_mem.is_empty() {
        str_write!(b, "{static_mem}\n");
    }
    let vconst = state.vconst.str("\t");
    if !vconst.is_empty() {
        str_write!(b, "{vconst}\n");
    }
    let clump = state.clump.str("\t");
    if !clump.is_empty() {
        str_write!(b, "{clump}\n");
    }
    let stack = state.stack.str(state);
    if !stack.is_empty() {
        str_write!(b, "{stack}\n");
    }
    let props = state.props.str("\t");
    if !props.is_empty() {
        str_write!(b, "{props}");
    }
    let heap = state.heap.str("\t");
    if !heap.is_empty() {
        str_write!(b, "\n{heap}\n");
    }
    str_write!(b, "]]\n");
    b
}

impl<'a> State<'a> {
    pub fn ext(&self) -> &'a Externals {
        self.ext
    }

    pub fn props(&mut self) -> &mut Props {
        &mut self.props
    }
}

pub fn state_pushframe<'a>(
    state: &mut State<'a>,
    name: String,
    b: &'a AstBlock,
    t: &AstType,
    abstract_: bool,
) {
    state.stack.push(name, b, t, abstract_);
}

pub fn state_popframe(state: &mut State) {
    state.stack.pop_frame();
}

pub fn state_declare(state: &mut State, var: &AstVariable, isparam: bool) {
    state.stack.declare(var, isparam);
}

pub fn state_vconst(
    state: &mut State,
    t: &AstType,
    comment: Option<&str>,
    persist: bool,
) -> Box<Value> {
    let v = ast_type_vconst(t, state, comment.unwrap_or(""), persist);
    if v.is_struct() {
        return v;
    }
    let c = state.vconst.declare(v, comment, persist);
    Value::new_sync(AstExpr::new_identifier(c))
}

pub fn state_static_init(state: &mut State, lit: &str) -> Box<Value> {
    if let Some(loc) = state.static_memory.check_pool(lit) {
        // Note: Original creates a value that points to this existing location. This is a
        // double-free. When the value is destroyed, the location will be destroyed, even though it
        // is still in the static pool.
        return Value::new_ptr(Box::new(loc.clone()));
    }
    let address = state.static_memory.new_block();
    let loc = Location::new_static(address, AstExpr::new_constant(0));
    let obj = state.get_mut(&loc, true).unwrap().unwrap();
    obj.assign(Some(Value::new_literal(lit)));
    state.static_memory.string_pool(lit, &loc);
    Value::new_ptr(loc)
}

impl<'a> State<'a> {
    pub fn clump(&mut self) -> Box<Value> {
        let address = self.clump.new_block();
        let loc = Location::new_dereferencable(address, AstExpr::new_constant(0));
        Value::new_ptr(loc)
    }
}

pub fn state_islval(state: &mut State, v: &Value) -> bool {
    if !v.is_location() {
        return false;
    }
    let loc = v.as_location();
    state.get_mut(loc, true).unwrap();
    match loc.kind {
        LocationKind::Static => state.static_memory.has_block(loc.block),
        LocationKind::VConst => false,
        LocationKind::Dereferencable => state.clump.get_block_mut(loc.block).is_some(),
        LocationKind::Automatic { .. } => true,
        LocationKind::Dynamic => state.heap.get_block_mut(loc.block).is_some(),
    }
}

pub fn state_isalloc(state: &mut State, v: &Value) -> bool {
    if !v.is_location() {
        return false;
    }
    let loc = v.as_location();
    state.get_mut(loc, true).unwrap();
    location_toheap(loc, &mut state.heap)
}

pub fn state_getvconst<'s>(state: &'s State, id: &str) -> Option<&'s Value> {
    state.vconst.get(id)
}

impl<'a> State<'a> {
    //=state_get (non-mut variation)
    pub fn get<'s>(&'s self, loc: &Location) -> Result<Option<&'s Object>> {
        Ok(self
            .get_block(loc)?
            .and_then(|b| b.observe_read_only(&loc.offset, self)))
    }

    /// Gets the object at the given location `loc`.
    ///
    /// On success, this returns a mutable reference to the `Object`. If the block designated by
    /// `loc` doesn't exist, this returns Ok(None). (That seems suspicious to me; it means we're
    /// chasing a dangling pointer which we should not do. I'm not sure why it should not be an
    /// assertion failure. -jorendorff) If the block refers to a stack frame that doesn't exist,
    /// this returns an error. (Same. -jorendorff)
    //=state_get
    pub fn get_mut<'s>(
        &'s mut self,
        loc: &Location,
        constructive: bool,
    ) -> Result<Option<&'s mut Object>> {
        let state_ptr: *mut State = self;
        unsafe {
            // Unsafe because Block::observe is inherently UB when the state contains the block.
            let b = (*state_ptr).get_block_mut(loc)?;
            match b {
                None => {
                    assert!(matches!(
                        loc.kind,
                        LocationKind::Dynamic | LocationKind::Dereferencable | LocationKind::Static
                    ));
                    Ok(None)
                }
                Some(b) => Ok(b.observe(&loc.offset, &mut *state_ptr, constructive)),
            }
        }
    }

    pub fn get_block<'s>(&'s self, loc: &Location) -> Result<Option<&'s Block>> {
        match loc.kind {
            LocationKind::Static => Ok(self.static_memory.get_block(loc.block)),
            LocationKind::Automatic { frame } => {
                let Some(frame) = self.stack.get_frame(frame) else {
                    return Err(Error::new("stack frame doesn't exist".to_string()));
                };
                Ok(Some(frame.get_block(loc.block)))
            }
            LocationKind::Dynamic => Ok(self.heap.get_block(loc.block)),
            LocationKind::Dereferencable => Ok(self.clump.get_block(loc.block)),
            LocationKind::VConst => panic!(),
        }
    }

    pub fn get_block_mut<'s>(&'s mut self, loc: &Location) -> Result<Option<&'s mut Block>> {
        match loc.kind {
            LocationKind::Static => Ok(self.static_memory.get_block_mut(loc.block)),
            LocationKind::Automatic { frame } => {
                let Some(frame) = self.stack.get_frame_mut(frame) else {
                    return Err(Error::new("stack frame doesn't exist".to_string()));
                };
                Ok(Some(frame.get_block_mut(loc.block)))
            }
            LocationKind::Dynamic => Ok(self.heap.get_block_mut(loc.block)),
            LocationKind::Dereferencable => Ok(self.clump.get_block_mut(loc.block)),
            LocationKind::VConst => panic!(),
        }
    }
}

pub fn state_getresult<'s>(state: &'s mut State) -> Result<Option<&'s mut Object>> {
    let result_loc = state.stack.get_result().location().clone();
    state.get_mut(&result_loc, true)
}

fn state_getresulttype<'s>(state: &'s State) -> &'s AstType {
    state.stack.get_result().type_()
}

impl<'a> State<'a> {
    pub fn identifier_lvalue<'s>(&'s mut self, id: &str) -> Result<LValue<'s>> {
        let state: *mut State = self;
        unsafe {
            // Unsafe because we fetch references to two pieces of information. Need a single State
            // method that fetches both. Underneath all this is Block::observe.
            let obj = state_getobject(&mut *state, id)?;
            Ok(LValue {
                t: state_getobjecttype(&*state, id),
                obj,
            })
        }
    }
}

pub fn state_getobjecttype<'s>(state: &'s State, id: &str) -> &'s AstType {
    if id == KEYWORD_RETURN {
        return state_getresulttype(state);
    }
    let v = state.stack.get_variable(id).unwrap();
    v.type_()
}

pub fn state_getloc(state: &mut State, id: &str) -> Box<Value> {
    // In the original, this apparently borrows the Location representing the variable's location,
    // but then passes it to value_ptr_create without copying. We copy because I don't see how this
    // isn't a double free otherwise. (Note: in one caller, `call_setupverify`, the Value is always
    // leaked, which partially explains it. The other is `address_eval`, which I don't think is
    // always leaked.)
    let v = state.stack.get_variable(id).unwrap();
    Value::new_ptr(location_copy(v.location()))
}

pub fn state_getobject<'s>(state: &'s mut State, id: &str) -> Result<Option<&'s mut Object>> {
    if id == KEYWORD_RETURN {
        return state_getresult(state);
    }
    let Some(v) = state.stack.get_variable(id) else {
        return Err(Error::new(format!("unknown variable `{id}'")));
    };
    let var_loc = v.location().clone();
    state.get_mut(&var_loc, true)
}

pub fn state_deref<'s>(
    state: &'s mut State,
    ptr_val: &Value,
    index: &AstExpr,
) -> Result<Option<&'s mut Object>> {
    if value_issync(ptr_val) {
        return Ok(None);
    }
    let deref_base = ptr_val.as_location();
    // Note: the original leaked this location.
    let deref = deref_base.with_offset(index);
    state
        .get_mut(&deref, true)
        .map_err(|err| Error::new(format!("undefined indirection: {err}")))
}

impl<'a> State<'a> {
    pub fn alloc(&mut self) -> Box<Value> {
        Value::new_ptr(self.heap.new_block())
    }

    pub fn dealloc(&mut self, val: &Value) -> Result<()> {
        if !val.is_location() {
            return Err(Error::new(
                "undefined free of value not pointing at heap".to_string(),
            ));
        }
        location_dealloc(val.as_location(), &mut self.heap)
    }
}

pub fn state_addresses_deallocand(state: &State, obj: &Object) -> bool {
    // Note: Original doesn't null-check.
    let val = obj.as_value().unwrap();
    let loc = val.as_location();
    state.loc_is_deallocand(loc)
}

impl<'a> State<'a> {
    pub fn loc_is_deallocand(&self, loc: &Location) -> bool {
        let b = self.get_block(loc).unwrap();
        loc.type_is_dynamic() && b.is_some()
    }
}

pub fn state_hasgarbage(state: &State) -> bool {
    !state.heap.referenced(state)
}

pub fn state_references(s: &State, loc: &Location) -> bool {
    s.stack.references(loc, s)
}

pub fn state_eval(s: &State, e: &AstExpr) -> bool {
    s.vconst.eval(e)
}

pub fn state_equal(s1: &State, s2: &State) -> bool {
    let mut s1_c = state_copy(s1);
    let mut s2_c = state_copy(s2);
    state_undeclareliterals(&mut s1_c);
    state_undeclareliterals(&mut s2_c);
    state_undeclarevars(&mut s1_c);
    state_undeclarevars(&mut s2_c);
    state_popprops(&mut s1_c);
    state_popprops(&mut s2_c);
    let str1 = state_str(&s1_c);
    let str2 = state_str(&s2_c);
    let equal = str1 == str2;
    if !equal {
        vprint!("abstract: {str2}");
        vprint!("actual: {str1}");
    }
    equal
}

fn state_undeclareliterals(s: &mut State) {
    s.static_memory = StaticMemory::new();
}

fn state_undeclarevars(s: &mut State) {
    Heap::undeclare(s);
    s.vconst.undeclare();
    Stack::undeclare(s);
}

fn state_popprops(s: &mut State) {
    s.props = Props::new();
}

impl<'a> State<'a> {
    //=state_atend
    pub fn at_end(&self) -> bool {
        self.stack.at_end()
    }

    //=stack_step
    pub fn step(&mut self) -> Result<()> {
        ProgramCounter::exec(self)
    }

    /// Name of the function currently on top of the stack.
    pub fn fname(&self) -> &str {
        self.stack.top().name()
    }
}
