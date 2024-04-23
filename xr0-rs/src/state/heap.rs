use std::collections::{BTreeMap, HashMap};
use std::ffi::CStr;
use std::ptr;

use crate::ast::{ast_expr_constant_create, ast_expr_matheval};
use crate::state::block::{block_create, block_str, block_undeclare};
use crate::state::location::location_create_dynamic;
use crate::state::state::state_references;
use crate::util::{strbuilder_build, strbuilder_create, Error, OwningCStr, Result};
use crate::value::{value_copy, value_destroy, value_str};
use crate::{strbuilder_write, AstExpr, Block, Location, State, StrBuilder, Value};

#[derive(Clone)]
pub struct Heap {
    blocks: Vec<HeapBlock>,
}

#[derive(Clone)]
struct HeapBlock {
    block: Box<Block>,
    freed: bool,
}

pub struct VConst {
    // Note: Iteration order of varmap is significant in vconst_str.
    pub varmap: BTreeMap<String, *mut Value>,
    pub comment: HashMap<String, String>,
    pub persist: HashMap<String, bool>,
}

impl Heap {
    pub unsafe fn new() -> Self {
        Heap { blocks: vec![] }
    }
}

pub unsafe fn heap_copy(h: &Heap) -> Heap {
    h.clone()
}

pub unsafe fn heap_str(h: *mut Heap, indent: &str) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    for (i, hb) in (*h).blocks.iter().enumerate() {
        if !hb.freed {
            strbuilder_write!(
                b,
                "{indent}{i}: {}{}",
                block_str(&hb.block),
                if printdelim(h, i) { "\n" } else { "" },
            );
        }
    }
    strbuilder_build(b)
}

unsafe fn printdelim(h: *mut Heap, start: usize) -> bool {
    for i in start + 1..(*h).blocks.len() {
        if !(*h).blocks[i].freed {
            return true;
        }
    }
    false
}

pub unsafe fn heap_newblock(h: *mut Heap) -> Box<Location> {
    let address = (*h).blocks.len() as libc::c_int;
    (*h).blocks.push(HeapBlock {
        block: block_create(),
        freed: false,
    });
    location_create_dynamic(address, ast_expr_constant_create(0))
}

pub unsafe fn heap_getblock(h: *mut Heap, address: libc::c_int) -> *mut Block {
    if address as usize >= (*h).blocks.len() {
        return ptr::null_mut();
    }
    let hb = &mut (*h).blocks[address as usize];
    if hb.freed {
        return ptr::null_mut();
    }
    &mut *hb.block
}

pub unsafe fn heap_deallocblock(h: *mut Heap, address: libc::c_int) -> Result<()> {
    if (*h).blocks[address as usize].freed {
        return Err(Error::new("double free".to_string()));
    }
    (*h).blocks[address as usize].freed = true;
    Ok(())
}

pub unsafe fn heap_blockisfreed(h: *mut Heap, address: libc::c_int) -> bool {
    (*h).blocks[address as usize].freed
}

pub unsafe fn heap_undeclare(h: *mut Heap, s: *mut State) {
    // DANGER: s aliases h and `block_undeclare` actually accesses h.
    for i in 0..(*h).blocks.len() {
        if !(*h).blocks[i].freed {
            block_undeclare(&mut *(*h).blocks[i].block, s);
        }
    }
}

pub unsafe fn heap_referenced(h: *mut Heap, s: *mut State) -> bool {
    for i in 0..(*h).blocks.len() {
        if !(*h).blocks[i].freed && !block_referenced(s, i as libc::c_int) {
            return false;
        }
    }
    true
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

impl Drop for VConst {
    fn drop(&mut self) {
        for &v in self.varmap.values() {
            unsafe {
                value_destroy(v);
            }
        }
    }
}

pub unsafe fn vconst_copy(old: &VConst) -> VConst {
    let varmap = old
        .varmap
        .iter()
        .map(|(k, &v)| (k.clone(), value_copy(&*v)))
        .collect();
    VConst {
        varmap,
        comment: old.comment.clone(),
        persist: old.persist.clone(),
    }
}

pub unsafe fn vconst_declare(
    v: &mut VConst,
    val: *mut Value,
    comment: *mut libc::c_char,
    persist: bool,
) -> OwningCStr {
    let m = &mut v.varmap;
    let s = vconst_id(m, &v.persist, persist);
    let s_string = s.to_string();
    m.insert(s_string.clone(), val);
    if !comment.is_null() {
        let comment_string = CStr::from_ptr(comment).to_str().unwrap().to_string();
        v.comment.insert(s_string.clone(), comment_string);
    }
    v.persist.insert(s_string, persist);
    s
}

unsafe fn vconst_id(
    varmap: &BTreeMap<String, *mut Value>,
    persistmap: &HashMap<String, bool>,
    persist: bool,
) -> OwningCStr {
    let npersist = count_true(persistmap);
    let b: *mut StrBuilder = strbuilder_create();
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

pub unsafe fn vconst_get(v: &VConst, id: *mut libc::c_char) -> *mut Value {
    let id_str = CStr::from_ptr(id).to_str().unwrap();
    v.varmap.get(id_str).copied().unwrap_or(ptr::null_mut())
}

pub unsafe fn vconst_undeclare(v: &mut VConst) {
    let mut varmap = BTreeMap::new();
    let mut comment = HashMap::new();
    let mut persist = HashMap::new();
    for key in v.varmap.keys() {
        if v.persist.get(key).copied().unwrap_or(false) {
            varmap.insert(
                key.clone(),
                value_copy(&*(v.varmap.get(key).copied().unwrap())),
            );
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
    let b: *mut StrBuilder = strbuilder_create();
    for (k, &val) in &v.varmap {
        let value = value_str(&*val);
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
