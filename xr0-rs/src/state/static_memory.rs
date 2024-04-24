use std::collections::HashMap;
use std::ptr;

use super::Block;
use crate::state::block::{block_create, block_str};
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};
use crate::{strbuilder_write, Location};

#[derive(Clone)]
pub struct StaticMemory {
    pub blocks: Vec<Box<Block>>,
    pub pool: HashMap<String, Box<Location>>,
}

impl StaticMemory {
    pub fn new() -> Self {
        StaticMemory {
            blocks: vec![],
            pool: HashMap::new(),
        }
    }
}

pub unsafe fn static_memory_str(sm: &StaticMemory, indent: &str) -> OwningCStr {
    let mut b = strbuilder_create();
    for (i, block) in sm.blocks.iter().enumerate() {
        strbuilder_write!(b, "{indent}{i}: {}\n", block_str(block));
    }
    strbuilder_build(b)
}

pub unsafe fn static_memory_newblock(sm: &mut StaticMemory) -> libc::c_int {
    let address = sm.blocks.len() as libc::c_int;
    sm.blocks.push(block_create());
    address
}

pub unsafe fn static_memory_hasblock(sm: &StaticMemory, address: libc::c_int) -> bool {
    (address as usize) < sm.blocks.len()
}

pub unsafe fn static_memory_getblock(
    sm: &mut StaticMemory,
    address: libc::c_int,
) -> Option<&mut Block> {
    sm.blocks
        .get_mut(address as usize)
        .map(|block| &mut **block)
}

/* string pooling to write one string literal to static_memory and all subsequent
 * literals reference it is used to avoid infinite loops in splitting logic with
 * different literals per selection condition */
pub unsafe fn static_memory_stringpool(sm: &mut StaticMemory, lit: &str, loc: &Location) {
    sm.pool.insert(lit.to_string(), Box::new(loc.clone()));
}

pub unsafe fn static_memory_checkpool(sm: &StaticMemory, lit: &str) -> *mut Location {
    sm.pool.get(lit).map_or(ptr::null_mut(), |boxed| {
        &**boxed as *const Location as *mut Location
    })
}
