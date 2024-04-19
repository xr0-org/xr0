#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use libc::{free, malloc};

use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str,
};
use crate::util::{strbuilder_build, strbuilder_create};
use crate::{cstr, strbuilder_write, Block, BlockArr, StrBuilder};

#[derive(Copy, Clone)]
pub struct Clump {
    pub blocks: *mut BlockArr,
}

pub unsafe fn clump_create() -> *mut Clump {
    let c: *mut Clump = malloc(::core::mem::size_of::<Clump>()) as *mut Clump;
    if c.is_null() {
        panic!();
    }
    (*c).blocks = block_arr_create();
    return c;
}

pub unsafe fn clump_destroy(c: *mut Clump) {
    block_arr_destroy((*c).blocks);
}

pub unsafe fn clump_str(c: *mut Clump, indent: *mut libc::c_char) -> *mut libc::c_char {
    let b: *mut StrBuilder = strbuilder_create();
    let n: libc::c_int = block_arr_nblocks((*c).blocks);
    let arr: *mut *mut Block = block_arr_blocks((*c).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let block: *mut libc::c_char = block_str(*arr.offset(i as isize));
        strbuilder_write!(b, "{}{i}: {}\n", cstr!(indent), cstr!(block));
        free(block as *mut libc::c_void);
        i += 1;
    }
    return strbuilder_build(b);
}

pub unsafe fn clump_copy(c: *mut Clump) -> *mut Clump {
    let copy: *mut Clump = malloc(::core::mem::size_of::<Clump>()) as *mut Clump;
    (*copy).blocks = block_arr_copy((*c).blocks);
    return copy;
}

pub unsafe fn clump_newblock(c: *mut Clump) -> libc::c_int {
    let address: libc::c_int = block_arr_append((*c).blocks, block_create());
    let n: libc::c_int = block_arr_nblocks((*c).blocks);
    assert!(n > 0);
    address
}

pub unsafe fn clump_getblock(c: *mut Clump, address: libc::c_int) -> *mut Block {
    if address >= block_arr_nblocks((*c).blocks) {
        return 0 as *mut Block;
    }
    return *(block_arr_blocks((*c).blocks)).offset(address as isize);
}
