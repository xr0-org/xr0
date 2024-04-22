#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_assignments)]

use std::ptr;

use crate::state::block::{block_create, block_str};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{cstr, strbuilder_write, Block, StrBuilder};

#[derive(Clone)]
pub struct Clump {
    pub blocks: Vec<Box<Block>>,
}

pub unsafe fn clump_create() -> *mut Clump {
    Box::into_raw(Box::new(Clump { blocks: vec![] }))
}

pub unsafe fn clump_destroy(c: *mut Clump) {
    drop(Box::from_raw(c));
}

pub unsafe fn clump_str(c: *mut Clump, indent: *mut libc::c_char) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    for (i, block) in (*c).blocks.iter().enumerate() {
        strbuilder_write!(b, "{}{i}: {}\n", cstr!(indent), block_str(block));
    }
    strbuilder_build(b)
}

pub unsafe fn clump_newblock(c: *mut Clump) -> libc::c_int {
    let address = (*c).blocks.len() as libc::c_int;
    (*c).blocks.push(Box::from_raw(block_create()));
    address
}

pub unsafe fn clump_getblock(c: *mut Clump, address: libc::c_int) -> *mut Block {
    match (*c).blocks.get_mut(address as usize) {
        Some(block) => &mut **block,
        None => ptr::null_mut(),
    }
}
