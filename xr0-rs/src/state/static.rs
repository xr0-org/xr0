#![allow(
    dead_code,
    mutable_transmutes,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{free, malloc};

use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str,
};
use crate::state::location::location_copy;
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, strbuilder_printf, Map};
use crate::{Block, BlockArr, Location, StrBuilder};

pub struct StaticMemory {
    pub blocks: *mut BlockArr,
    pub pool: Box<Map>,
}

pub unsafe fn static_memory_create() -> *mut StaticMemory {
    let mut sm: *mut StaticMemory =
        malloc(::core::mem::size_of::<StaticMemory>()) as *mut StaticMemory;
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

pub unsafe fn static_memory_destroy(mut sm: *mut StaticMemory) {
    block_arr_destroy((*sm).blocks);
}

pub unsafe fn static_memory_str(
    mut sm: *mut StaticMemory,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut n: libc::c_int = block_arr_nblocks((*sm).blocks);
    let mut arr: *mut *mut Block = block_arr_blocks((*sm).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut block: *mut libc::c_char = block_str(*arr.offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%d: %s\n\0" as *const u8 as *const libc::c_char,
            indent,
            i,
            block,
        );
        free(block as *mut libc::c_void);
        i += 1;
    }
    return strbuilder_build(b);
}

pub unsafe fn static_memory_copy(mut sm: *mut StaticMemory) -> *mut StaticMemory {
    let mut copy: *mut StaticMemory =
        malloc(::core::mem::size_of::<StaticMemory>()) as *mut StaticMemory;
    (*copy).blocks = block_arr_copy((*sm).blocks);
    (*copy).pool = pool_copy(&(*sm).pool);
    return copy;
}
unsafe fn pool_copy(mut p: &Map) -> Box<Map> {
    let mut pcopy = Map::new();
    for (k, v) in p.pairs() {
        pcopy.set(
            dynamic_str(k),
            location_copy(v as *mut Location) as *const libc::c_void,
        );
    }
    return pcopy;
}

pub unsafe fn static_memory_newblock(mut sm: *mut StaticMemory) -> libc::c_int {
    let mut address: libc::c_int = block_arr_append((*sm).blocks, block_create());
    let mut n: libc::c_int = block_arr_nblocks((*sm).blocks);
    assert!(n > 0);
    return address;
}

pub unsafe fn static_memory_getblock(
    mut sm: *mut StaticMemory,
    mut address: libc::c_int,
) -> *mut Block {
    if address >= block_arr_nblocks((*sm).blocks) {
        return 0 as *mut Block;
    }
    return *(block_arr_blocks((*sm).blocks)).offset(address as isize);
}

pub unsafe fn static_memory_stringpool(
    mut sm: *mut StaticMemory,
    mut lit: *mut libc::c_char,
    mut loc: *mut Location,
) {
    (*sm)
        .pool
        .set(dynamic_str(lit), location_copy(loc) as *const libc::c_void);
}

pub unsafe fn static_memory_checkpool(
    mut sm: *mut StaticMemory,
    mut lit: *mut libc::c_char,
) -> *mut Location {
    return (*sm).pool.get(lit) as *mut Location;
}