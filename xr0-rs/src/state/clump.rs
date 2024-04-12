#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
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
use crate::util::{strbuilder_build, strbuilder_create, strbuilder_printf};
use crate::{block_arr, Block as block, StrBuilder as strbuilder};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct clump {
    pub blocks: *mut block_arr,
}

pub unsafe fn clump_create() -> *mut clump {
    let mut c: *mut clump = malloc(::core::mem::size_of::<clump>()) as *mut clump;
    if c.is_null() {
        panic!();
    }
    (*c).blocks = block_arr_create();
    return c;
}

pub unsafe fn clump_destroy(mut c: *mut clump) {
    block_arr_destroy((*c).blocks);
}

pub unsafe fn clump_str(mut c: *mut clump, mut indent: *mut libc::c_char) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut n: libc::c_int = block_arr_nblocks((*c).blocks);
    let mut arr: *mut *mut block = block_arr_blocks((*c).blocks);
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

pub unsafe fn clump_copy(mut c: *mut clump) -> *mut clump {
    let mut copy: *mut clump = malloc(::core::mem::size_of::<clump>()) as *mut clump;
    (*copy).blocks = block_arr_copy((*c).blocks);
    return copy;
}

pub unsafe fn clump_newblock(mut c: *mut clump) -> libc::c_int {
    let mut address: libc::c_int = block_arr_append((*c).blocks, block_create());
    let mut n: libc::c_int = block_arr_nblocks((*c).blocks);
    assert!(n > 0);
    address
}

pub unsafe fn clump_getblock(mut c: *mut clump, mut address: libc::c_int) -> *mut block {
    if address >= block_arr_nblocks((*c).blocks) {
        return 0 as *mut block;
    }
    return *(block_arr_blocks((*c).blocks)).offset(address as isize);
}
