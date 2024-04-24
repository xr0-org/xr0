use std::ptr;

use crate::state::block::{block_create, block_str};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{strbuilder_write, Block};

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

pub unsafe fn clump_str(c: *mut Clump, indent: &str) -> OwningCStr {
    let mut b = strbuilder_create();
    for (i, block) in (*c).blocks.iter().enumerate() {
        strbuilder_write!(b, "{indent}{i}: {}\n", block_str(block));
    }
    strbuilder_build(b)
}

pub unsafe fn clump_newblock(c: *mut Clump) -> libc::c_int {
    let address = (*c).blocks.len() as libc::c_int;
    (*c).blocks.push(block_create());
    address
}

pub unsafe fn clump_getblock(c: *mut Clump, address: libc::c_int) -> *mut Block {
    match (*c).blocks.get_mut(address as usize) {
        Some(block) => &mut **block,
        None => ptr::null_mut(),
    }
}
