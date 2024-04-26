use std::collections::{BTreeMap, HashMap};

use super::{Block, State};
use crate::ast::{ast_expr_constant_create, ast_expr_matheval};
use crate::state::location::location_create_dynamic;
use crate::state::state::state_references;
use crate::util::{strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::{strbuilder_write, AstExpr, Location, Value};

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
    pub unsafe fn new() -> Self {
        Heap { blocks: vec![] }
    }

    pub unsafe fn str(&self, indent: &str) -> OwningCStr {
        let mut b = strbuilder_create();
        for (i, hb) in self.blocks.iter().enumerate() {
            if !hb.freed {
                strbuilder_write!(
                    b,
                    "{indent}{i}: {}{}",
                    hb.block,
                    if self.print_delim(i) { "\n" } else { "" },
                );
            }
        }
        strbuilder_build(b)
    }

    fn print_delim(&self, start: usize) -> bool {
        for i in start + 1..self.blocks.len() {
            if !self.blocks[i].freed {
                return true;
            }
        }
        false
    }

    pub fn new_block(&mut self) -> Box<Location> {
        let address = self.blocks.len() as libc::c_int;
        self.blocks.push(HeapBlock {
            block: Block::new(),
            freed: false,
        });
        location_create_dynamic(address, ast_expr_constant_create(0))
    }

    pub fn get_block(&mut self, address: libc::c_int) -> Option<&mut Block> {
        if address as usize >= self.blocks.len() {
            return None;
        }
        let hb = &mut self.blocks[address as usize];
        if hb.freed {
            return None;
        }
        Some(&mut hb.block)
    }

    pub fn dealloc_block(&mut self, address: libc::c_int) -> Result<()> {
        if self.blocks[address as usize].freed {
            return Err(Error::new("double free".to_string()));
        }
        self.blocks[address as usize].freed = true;
        Ok(())
    }

    pub fn block_is_freed(&self, address: libc::c_int) -> bool {
        self.blocks[address as usize].freed
    }

    pub unsafe fn undeclare(&mut self, s: *mut State) {
        // XXX FIXME: s aliases h and `Block::undeclare` actually accesses h.
        for block in &mut self.blocks {
            if !block.freed {
                block.block.undeclare(s);
            }
        }
    }

    pub unsafe fn referenced(&self, s: *mut State) -> bool {
        for i in 0..self.blocks.len() {
            if !self.blocks[i].freed && !block_referenced(s, i as libc::c_int) {
                return false;
            }
        }
        true
    }
}

unsafe fn block_referenced(s: *mut State, addr: libc::c_int) -> bool {
    let loc = location_create_dynamic(addr, ast_expr_constant_create(0));
    state_references(s, &loc)
}

impl VConst {
    pub fn new() -> VConst {
        VConst {
            varmap: BTreeMap::new(),
            comment: HashMap::new(),
            persist: HashMap::new(),
        }
    }

    pub fn declare(&mut self, val: Box<Value>, comment: Option<&str>, persist: bool) -> OwningCStr {
        let s = self.id(persist);
        let s_string = s.to_string();
        self.varmap.insert(s_string.clone(), val);
        if let Some(comment) = comment {
            self.comment.insert(s_string.clone(), comment.to_string());
        }
        self.persist.insert(s_string, persist);
        s
    }

    fn id(&mut self, persist: bool) -> OwningCStr {
        let npersist = self.persist.values().filter(|&&b| b).count();
        let mut b = strbuilder_create();
        if persist {
            strbuilder_write!(b, "${npersist}");
        } else {
            strbuilder_write!(b, "#{}", self.varmap.len() - npersist);
        }
        strbuilder_build(b)
    }

    pub fn get(&self, id: &str) -> Option<&Value> {
        self.varmap.get(id).map(|boxed| &**boxed)
    }

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

    pub fn str(&self, indent: &str) -> OwningCStr {
        let mut b = strbuilder_create();
        for (k, val) in &self.varmap {
            strbuilder_write!(b, "{indent}{k}: {val}");
            if let Some(comment) = self.comment.get(k) {
                strbuilder_write!(b, "\t({comment})");
            }
            strbuilder_write!(b, "\n");
        }
        strbuilder_build(b)
    }

    /// Evaluate a boolean expression. This works only for comparisons of linear polynomial integer
    /// expressions, like `x < 0` or `x + y == z`.
    ///
    /// # Panics
    /// If the expression is not of a supported form.
    pub fn eval(&self, e: &AstExpr) -> bool {
        ast_expr_matheval(e)
    }
}
