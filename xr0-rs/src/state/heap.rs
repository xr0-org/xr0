#![allow(
    dead_code,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_variables
)]

use std::ptr;

use libc::{free, malloc, realloc};

use crate::ast::{ast_expr_constant_create, ast_expr_matheval};
use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str, block_undeclare,
};
use crate::state::location::{location_create_dynamic, location_destroy};
use crate::state::state::state_references;
use crate::util::{dynamic_str, error_create, strbuilder_build, strbuilder_create, Map, Result};
use crate::value::{value_copy, value_destroy, value_str};
use crate::{cstr, strbuilder_write, AstExpr, Block, BlockArr, Location, State, StrBuilder, Value};

pub struct Heap {
    pub blocks: *mut BlockArr,
    pub freed: *mut bool,
}
pub struct VConst {
    pub varmap: Box<Map>,
    pub comment: Box<Map>,
    pub persist: Box<Map>,
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

pub unsafe fn heap_blocks(h: *mut Heap) -> *mut BlockArr {
    if h.is_null() {
        panic!();
    }
    (*h).blocks
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
    if !(address < block_arr_nblocks((*h).blocks)) {
        panic!();
    }
    if *((*h).freed).offset(address as isize) {
        return Err(error_create(
            b"double free\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        ));
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

pub unsafe fn vconst_create() -> *mut VConst {
    let v: *mut VConst = malloc(::core::mem::size_of::<VConst>()) as *mut VConst;
    std::ptr::write(
        v,
        VConst {
            varmap: Map::new(),
            comment: Map::new(),
            persist: Map::new(),
        },
    );
    v
}

pub unsafe fn vconst_destroy(v: *mut VConst) {
    let VConst {
        varmap: m,
        comment,
        persist,
    } = std::ptr::read(v);
    for v in m.values() {
        value_destroy(v as *mut Value);
    }
    m.destroy();
    comment.destroy();
    persist.destroy();
    free(v as *mut libc::c_void);
}

pub unsafe fn vconst_copy(old: *mut VConst) -> *mut VConst {
    let new: *mut VConst = vconst_create();
    let mut m = &(*old).varmap;
    for (k, v) in m.pairs() {
        (*new).varmap.set(
            dynamic_str(k),
            value_copy(&*(v as *mut Value)) as *const libc::c_void,
        );
    }
    m = &(*old).comment;
    for (k, v) in m.pairs() {
        (*new).comment.set(
            dynamic_str(k),
            dynamic_str(v as *const libc::c_char) as *const libc::c_void,
        );
    }
    m = &(*old).persist;
    for (k, v) in m.pairs() {
        (*new).persist.set(dynamic_str(k), v);
    }
    new
}

pub unsafe fn vconst_declare(
    v: *mut VConst,
    val: *mut Value,
    comment: *mut libc::c_char,
    persist: bool,
) -> *mut libc::c_char {
    let m = &mut (*v).varmap;
    let s: *mut libc::c_char = vconst_id(m, &(*v).persist, persist);
    m.set(dynamic_str(s), val as *const libc::c_void);
    if !comment.is_null() {
        (*v).comment
            .set(dynamic_str(s), comment as *const libc::c_void);
    }
    (*v).persist
        .set(dynamic_str(s), persist as usize as *mut libc::c_void);
    s
}
unsafe fn vconst_id(varmap: &Map, persistmap: &Map, persist: bool) -> *mut libc::c_char {
    let npersist: libc::c_int = count_true(persistmap);
    let b: *mut StrBuilder = strbuilder_create();
    if persist {
        strbuilder_write!(b, "${npersist}");
    } else {
        strbuilder_write!(b, "#{}", varmap.len() - npersist);
    }
    strbuilder_build(b)
}
unsafe fn count_true(m: &Map) -> libc::c_int {
    let mut n: libc::c_int = 0 as libc::c_int;
    for v in m.values() {
        if !v.is_null() {
            n += 1;
        }
    }
    n
}

pub unsafe fn vconst_get(v: *mut VConst, id: *mut libc::c_char) -> *mut Value {
    (*v).varmap.get(id) as *mut Value
}

pub unsafe fn vconst_undeclare(v: *mut VConst) {
    let mut varmap = Map::new();
    let mut comment = Map::new();
    let mut persist = Map::new();
    let m = &(*v).varmap;
    for key in m.keys() {
        if !((*v).persist.get(key)).is_null() {
            varmap.set(
                dynamic_str(key),
                value_copy(&*((*v).varmap.get(key) as *mut Value)) as *const libc::c_void,
            );
            let c: *mut libc::c_char = (*v).comment.get(key) as *mut libc::c_char;
            if !c.is_null() {
                comment.set(dynamic_str(key), dynamic_str(c) as *const libc::c_void);
            }
            persist.set(dynamic_str(key), 1 as libc::c_int as *mut libc::c_void);
        }
    }
    std::ptr::write(
        v,
        VConst {
            varmap,
            comment,
            persist,
        },
    );
}

pub unsafe fn vconst_str(v: *mut VConst, indent: *mut libc::c_char) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let m = &(*v).varmap;
    for (k, val) in m.pairs() {
        let value: *mut libc::c_char = value_str(val as *mut Value);
        strbuilder_write!(b, "{}{}: {}", cstr!(indent), cstr!(k), cstr!(value));
        let comment: *mut libc::c_char = (*v).comment.get(k) as *mut libc::c_char;
        if !comment.is_null() {
            strbuilder_write!(b, "\t({})", cstr!(comment));
        }
        strbuilder_write!(b, "\n");
        free(value as *mut libc::c_void);
    }
    strbuilder_build(b)
}

pub unsafe fn vconst_eval(v: *mut VConst, e: &AstExpr) -> bool {
    ast_expr_matheval(e)
}
