#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use libc::{free, malloc};

use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str,
};
use crate::state::location::location_copy;
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Map};
use crate::{cstr, strbuilder_write, Block, BlockArr, Location, StrBuilder};

pub struct StaticMemory {
    pub blocks: *mut BlockArr,
    pub pool: Box<Map>,
}

pub unsafe fn static_memory_create() -> *mut StaticMemory {
    let sm: *mut StaticMemory = malloc(::core::mem::size_of::<StaticMemory>()) as *mut StaticMemory;
    assert!(!sm.is_null());
    std::ptr::write(
        sm,
        StaticMemory {
            blocks: block_arr_create(),
            pool: Map::new(),
        },
    );
    sm
}

pub unsafe fn static_memory_destroy(sm: *mut StaticMemory) {
    block_arr_destroy((*sm).blocks);
}

pub unsafe fn static_memory_str(
    sm: *mut StaticMemory,
    indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let n: libc::c_int = block_arr_nblocks((*sm).blocks);
    let arr: *mut *mut Block = block_arr_blocks((*sm).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let block: *mut libc::c_char = block_str(*arr.offset(i as isize));
        strbuilder_write!(b, "{}{i}: {}\n", cstr!(indent), cstr!(block));
        free(block as *mut libc::c_void);
        i += 1;
    }
    strbuilder_build(b)
}

pub unsafe fn static_memory_copy(sm: *mut StaticMemory) -> *mut StaticMemory {
    let copy: *mut StaticMemory =
        malloc(::core::mem::size_of::<StaticMemory>()) as *mut StaticMemory;
    ptr::write(
        copy,
        StaticMemory {
            blocks: block_arr_copy((*sm).blocks),
            pool: pool_copy(&(*sm).pool),
        },
    );
    copy
}
unsafe fn pool_copy(p: &Map) -> Box<Map> {
    let mut pcopy = Map::new();
    for (k, v) in p.pairs() {
        pcopy.set(
            dynamic_str(k),
            location_copy(&*(v as *mut Location)) as *const libc::c_void,
        );
    }
    pcopy
}

pub unsafe fn static_memory_newblock(sm: *mut StaticMemory) -> libc::c_int {
    let address: libc::c_int = block_arr_append((*sm).blocks, block_create());
    let n: libc::c_int = block_arr_nblocks((*sm).blocks);
    assert!(n > 0);
    address
}

pub unsafe fn static_memory_getblock(sm: *mut StaticMemory, address: libc::c_int) -> *mut Block {
    if address >= block_arr_nblocks((*sm).blocks) {
        return ptr::null_mut();
    }
    *(block_arr_blocks((*sm).blocks)).offset(address as isize)
}

pub unsafe fn static_memory_stringpool(
    sm: *mut StaticMemory,
    lit: *mut libc::c_char,
    loc: *mut Location,
) {
    (*sm).pool.set(
        dynamic_str(lit),
        location_copy(&*loc) as *const libc::c_void,
    );
}

pub unsafe fn static_memory_checkpool(
    sm: *mut StaticMemory,
    lit: *mut libc::c_char,
) -> *mut Location {
    (*sm).pool.get(lit) as *mut Location
}
