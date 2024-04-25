use std::collections::{BTreeMap, HashMap};
use std::ptr;

use super::{Block, State};
use crate::ast::{ast_expr_constant_create, ast_expr_matheval};
use crate::state::block::{block_create, block_str, block_undeclare};
use crate::state::location::location_create_dynamic;
use crate::state::state::state_references;
use crate::util::{strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::value::value_str;
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
    // Note: Iteration order of varmap is significant in vconst_str.
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
                    block_str(&hb.block),
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
            block: block_create(),
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
        // DANGER: s aliases h and `block_undeclare` actually accesses h.
        for i in 0..self.blocks.len() {
            if !self.blocks[i].freed {
                block_undeclare(&mut *self.blocks[i].block, s);
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

pub unsafe fn vconst_create() -> VConst {
    VConst {
        varmap: BTreeMap::new(),
        comment: HashMap::new(),
        persist: HashMap::new(),
    }
}

pub unsafe fn vconst_declare(
    v: &mut VConst,
    val: *mut Value,
    comment: Option<&str>,
    persist: bool,
) -> OwningCStr {
    let m = &mut v.varmap;
    let s = vconst_id(m, &v.persist, persist);
    let s_string = s.to_string();
    m.insert(s_string.clone(), Box::from_raw(val));
    if let Some(comment) = comment {
        v.comment.insert(s_string.clone(), comment.to_string());
    }
    v.persist.insert(s_string, persist);
    s
}

unsafe fn vconst_id(
    varmap: &BTreeMap<String, Box<Value>>,
    persistmap: &HashMap<String, bool>,
    persist: bool,
) -> OwningCStr {
    let npersist = count_true(persistmap);
    let mut b = strbuilder_create();
    if persist {
        strbuilder_write!(b, "${npersist}");
    } else {
        strbuilder_write!(b, "#{}", varmap.len() - npersist);
    }
    strbuilder_build(b)
}

unsafe fn count_true(m: &HashMap<String, bool>) -> usize {
    m.values().filter(|&&b| b).count()
}

pub unsafe fn vconst_get(v: &VConst, id: &str) -> *mut Value {
    v.varmap.get(id).map_or(ptr::null_mut(), |value| {
        &**value as *const Value as *mut Value
    })
}

pub unsafe fn vconst_undeclare(v: &mut VConst) {
    let mut varmap = BTreeMap::new();
    let mut comment = HashMap::new();
    let mut persist = HashMap::new();
    for (key, value) in &v.varmap {
        if v.persist.get(key).copied().unwrap_or(false) {
            varmap.insert(key.clone(), value.clone());
            if let Some(c) = v.comment.get(key) {
                comment.insert(key.clone(), c.clone());
            }
            persist.insert(key.clone(), true);
        }
    }
    v.varmap = varmap;
    v.comment = comment;
    v.persist = persist;
}

pub unsafe fn vconst_str(v: &VConst, indent: &str) -> OwningCStr {
    let mut b = strbuilder_create();
    for (k, val) in &v.varmap {
        let value = value_str(val);
        strbuilder_write!(b, "{indent}{k}: {value}");
        if let Some(comment) = v.comment.get(k) {
            strbuilder_write!(b, "\t({comment})");
        }
        strbuilder_write!(b, "\n");
    }
    strbuilder_build(b)
}

pub unsafe fn vconst_eval(_v: &VConst, e: &AstExpr) -> bool {
    ast_expr_matheval(e)
}
