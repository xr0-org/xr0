use std::collections::{BTreeMap, HashMap};
use std::ffi::CStr;
use std::ptr;

use libc::{free, malloc, realloc};

use crate::ast::{ast_expr_constant_create, ast_expr_matheval};
use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str, block_undeclare,
};
use crate::state::location::{location_create_dynamic, location_destroy};
use crate::state::state::state_references;
use crate::util::{strbuilder_build, strbuilder_create, Error, Result};
use crate::value::{value_copy, value_destroy, value_str};
use crate::{cstr, strbuilder_write, AstExpr, Block, BlockArr, Location, State, StrBuilder, Value};

pub struct Heap {
    pub blocks: *mut BlockArr,
    pub freed: *mut bool,
}

pub struct VConst {
    // Note: Iteration order of varmap is significant in vconst_str.
    pub varmap: BTreeMap<String, *mut Value>,
    pub comment: HashMap<String, String>,
    pub persist: HashMap<String, bool>,
}

impl Heap {
    pub unsafe fn new() -> Self {
        Heap {
            blocks: block_arr_create(),
            freed: ptr::null_mut(),
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            block_arr_destroy(self.blocks);
            free(self.freed as *mut libc::c_void);
        }
    }
}

pub unsafe fn heap_copy(h: &Heap) -> Heap {
    let blocks = block_arr_copy(h.blocks);
    let n: libc::c_int = block_arr_nblocks(h.blocks);
    let freed_copy: *mut bool =
        malloc((::core::mem::size_of::<bool>()).wrapping_mul(n as usize)) as *mut bool;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        *freed_copy.offset(i as isize) = *(h.freed).offset(i as isize);
        i += 1;
    }
    Heap {
        blocks,
        freed: freed_copy,
    }
}

pub unsafe fn heap_str(h: *mut Heap, indent: *mut libc::c_char) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let arr: *mut *mut Block = block_arr_blocks((*h).blocks);
    let n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            let block: *mut libc::c_char = block_str(*arr.offset(i as isize));
            strbuilder_write!(
                b,
                "{}{i}: {}{}",
                cstr!(indent),
                cstr!(block),
                if printdelim(h, i) { "\n" } else { "" },
            );
            free(block as *mut libc::c_void);
        }
        i += 1;
    }
    strbuilder_build(b)
}

unsafe fn printdelim(h: *mut Heap, start: libc::c_int) -> bool {
    let n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = start + 1 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            return true;
        }
        i += 1;
    }
    false
}

pub unsafe fn heap_newblock(h: *mut Heap) -> *mut Location {
    let address: libc::c_int = block_arr_append((*h).blocks, block_create());
    let n: libc::c_int = block_arr_nblocks((*h).blocks);
    assert!(n > 0);
    (*h).freed = realloc(
        (*h).freed as *mut libc::c_void,
        (::core::mem::size_of::<bool>()).wrapping_mul(n as usize),
    ) as *mut bool;
    *((*h).freed).offset(address as isize) = false;
    location_create_dynamic(address, ast_expr_constant_create(0 as libc::c_int))
}

pub unsafe fn heap_getblock(h: *mut Heap, address: libc::c_int) -> *mut Block {
    if address >= block_arr_nblocks((*h).blocks) {
        return ptr::null_mut();
    }
    if *((*h).freed).offset(address as isize) {
        return ptr::null_mut();
    }
    *(block_arr_blocks((*h).blocks)).offset(address as isize)
}

pub unsafe fn heap_deallocblock(h: *mut Heap, address: libc::c_int) -> Result<()> {
    assert!(address < block_arr_nblocks((*h).blocks));
    if *((*h).freed).offset(address as isize) {
        return Err(Error::new("double free".to_string()));
    }
    *((*h).freed).offset(address as isize) = true;
    Ok(())
}

pub unsafe fn heap_blockisfreed(h: *mut Heap, address: libc::c_int) -> bool {
    *((*h).freed).offset(address as isize)
}

pub unsafe fn heap_undeclare(h: *mut Heap, s: *mut State) {
    let n: libc::c_int = block_arr_nblocks((*h).blocks);
    let b: *mut *mut Block = block_arr_blocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            block_undeclare(*b.offset(i as isize), s);
        }
        i += 1;
    }
}

pub unsafe fn heap_referenced(h: *mut Heap, s: *mut State) -> bool {
    let n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) && !block_referenced(s, i) {
            return false;
        }
        i += 1;
    }
    true
}

unsafe fn block_referenced(s: *mut State, addr: libc::c_int) -> bool {
    let loc: *mut Location =
        location_create_dynamic(addr, ast_expr_constant_create(0 as libc::c_int));
    let referenced = state_references(s, &*loc);
    location_destroy(loc);
    referenced
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
) -> *mut libc::c_char {
    let m = &mut (*v).varmap;
    let s: *mut libc::c_char = vconst_id(m, &(*v).persist, persist);
    let s_string = CStr::from_ptr(s).to_str().unwrap().to_string();
    m.insert(s_string.clone(), val);
    if !comment.is_null() {
        let comment_string = CStr::from_ptr(comment).to_str().unwrap().to_string();
        (*v).comment.insert(s_string.clone(), comment_string);
    }
    (*v).persist.insert(s_string, persist);
    s
}

unsafe fn vconst_id(
    varmap: &BTreeMap<String, *mut Value>,
    persistmap: &HashMap<String, bool>,
    persist: bool,
) -> *mut libc::c_char {
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
    (*v).varmap.get(id_str).copied().unwrap_or(ptr::null_mut())
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

pub unsafe fn vconst_str(v: &VConst, indent: *mut libc::c_char) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    for (k, &val) in &(*v).varmap {
        let value: *mut libc::c_char = value_str(val);
        strbuilder_write!(b, "{}{}: {}", cstr!(indent), k, cstr!(value));
        if let Some(comment) = (*v).comment.get(k) {
            strbuilder_write!(b, "\t({comment})");
        }
        strbuilder_write!(b, "\n");
        free(value as *mut libc::c_void);
    }
    strbuilder_build(b)
}

pub unsafe fn vconst_eval(_v: &VConst, e: &AstExpr) -> bool {
    ast_expr_matheval(e)
}
