use std::collections::{BTreeMap, HashMap};

use super::{Block, State};
use crate::ast::ast_expr_matheval;
use crate::state::state::state_references;
use crate::util::{Error, Result};
use crate::{str_write, AstExpr, Location, Value};

#[derive(Clone)]
pub struct Heap {
    blocks: Vec<HeapBlock>,
}

#[derive(Clone)]
struct HeapBlock {
    block: Box<Block>,
    freed: bool,
}

#[derive(Clone)]
pub struct VConst {
    // Note: Iteration order of varmap is significant in vconst_str: XR0 uses string comparison to
    // see if abstract states agree.
    pub varmap: BTreeMap<String, Box<Value>>,
    pub comment: HashMap<String, String>,
    pub persist: HashMap<String, bool>,
}

impl Heap {
    //=heap_create
    pub fn new() -> Self {
        Heap { blocks: vec![] }
    }

    //=heap_str
    pub fn str(&self, indent: &str) -> String {
        let mut b = String::new();
        for (i, hb) in self.blocks.iter().enumerate() {
            if !hb.freed {
                str_write!(
                    b,
                    "{indent}{i}: {}{}",
                    hb.block,
                    if self.print_delim(i) { "\n" } else { "" },
                );
            }
        }
        b
    }

    //=printdelim
    fn print_delim(&self, start: usize) -> bool {
        for i in start + 1..self.blocks.len() {
            if !self.blocks[i].freed {
                return true;
            }
        }
        false
    }

    //=heap_newblock
    pub fn new_block(&mut self) -> Box<Location> {
        let address = self.blocks.len();
        self.blocks.push(HeapBlock {
            block: Block::new(),
            freed: false,
        });
        Location::new_dynamic(address, AstExpr::new_constant(0))
    }

    //=heap_getblock (non-mut version)
    pub fn get_block(&self, address: usize) -> Option<&Block> {
        if address >= self.blocks.len() {
            return None;
        }
        let hb = &self.blocks[address];
        if hb.freed {
            return None;
        }
        Some(&hb.block)
    }

    //=heap_getblock
    pub fn get_block_mut(&mut self, address: usize) -> Option<&mut Block> {
        if address >= self.blocks.len() {
            return None;
        }
        let hb = &mut self.blocks[address];
        if hb.freed {
            return None;
        }
        Some(&mut hb.block)
    }

    //=heap_deallocblock
    pub fn dealloc_block(&mut self, address: usize) -> Result<()> {
        if self.blocks[address].freed {
            return Err(Error::new("double free".to_string()));
        }
        self.blocks[address].freed = true;
        Ok(())
    }

    //=heap_blockisfreed
    pub fn block_is_freed(&self, address: usize) -> bool {
        self.blocks[address].freed
    }

    // XXX FIXME: inherently UB API
    // `s` aliases `self` and `Block::undeclare` actually accesses `self`
    //=heap_undeclare
    pub fn undeclare(&mut self, s: &mut State) {
        for block in &mut self.blocks {
            if !block.freed {
                block.block.undeclare(s);
            }
        }
    }

    // XXX FIXME: inherently UB API because `self` aliases mut `s`.
    //=heap_referenced
    pub fn referenced(&self, s: &mut State) -> bool {
        for i in 0..self.blocks.len() {
            if !self.blocks[i].freed && !block_referenced(s, i) {
                return false;
            }
        }
        true
    }
}

fn block_referenced(s: &mut State, addr: usize) -> bool {
    let loc = Location::new_dynamic(addr, AstExpr::new_constant(0));
    state_references(s, &loc)
}

impl VConst {
    //=vconst_create
    pub fn new() -> VConst {
        VConst {
            varmap: BTreeMap::new(),
            comment: HashMap::new(),
            persist: HashMap::new(),
        }
    }

    //=vconst_declare
    pub fn declare(&mut self, val: Box<Value>, comment: Option<&str>, persist: bool) -> String {
        let s = self.id(persist);
        let s_string = s.to_string();
        self.varmap.insert(s_string.clone(), val);
        if let Some(comment) = comment {
            self.comment.insert(s_string.clone(), comment.to_string());
        }
        self.persist.insert(s_string, persist);
        s
    }

    //=vconst_id
    fn id(&mut self, persist: bool) -> String {
        let npersist = self.persist.values().filter(|&&b| b).count();
        let mut b = String::new();
        if persist {
            str_write!(b, "${npersist}");
        } else {
            str_write!(b, "#{}", self.varmap.len() - npersist);
        }
        b
    }

    //=vconst_get
    pub fn get(&self, id: &str) -> Option<&Value> {
        self.varmap.get(id).map(|boxed| &**boxed)
    }

    //=vconst_undeclare
    pub fn undeclare(&mut self) {
        let mut varmap = BTreeMap::new();
        let mut comment = HashMap::new();
        let mut persist = HashMap::new();
        for (key, value) in &self.varmap {
            if self.persist.get(key).copied().unwrap_or(false) {
                varmap.insert(key.clone(), value.clone());
                if let Some(c) = self.comment.get(key) {
                    comment.insert(key.clone(), c.clone());
                }
                persist.insert(key.clone(), true);
            }
        }
        self.varmap = varmap;
        self.comment = comment;
        self.persist = persist;
    }

    //=vconst_tsr
    pub fn str(&self, indent: &str) -> String {
        let mut b = String::new();
        for (k, val) in &self.varmap {
            str_write!(b, "{indent}{k}: {val}");
            if let Some(comment) = self.comment.get(k) {
                str_write!(b, "\t({comment})");
            }
            str_write!(b, "\n");
        }
        b
    }

    /// Evaluate a boolean expression. This works only for comparisons of linear polynomial integer
    /// expressions, like `x < 0` or `x + y == z`.
    ///
    /// # Panics
    /// If the expression is not of a supported form.
    //=vconst_eval
    pub fn eval(&self, e: &AstExpr) -> bool {
        ast_expr_matheval(e)
    }
}
