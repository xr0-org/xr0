use std::ptr;

use crate::state::block::{block_create, block_str};
use crate::state::location::location_copy;
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Map, OwningCStr};
use crate::{cstr, strbuilder_write, Block, Location, StrBuilder};

pub struct StaticMemory {
    pub blocks: Vec<Box<Block>>,
    pub pool: Box<Map>,
}

pub unsafe fn static_memory_create() -> *mut StaticMemory {
    Box::into_raw(Box::new(StaticMemory {
        blocks: vec![],
        pool: Map::new(),
    }))
}

pub unsafe fn static_memory_destroy(sm: *mut StaticMemory) {
    // Note: Original leaked the StaticMemory itself.
    drop(Box::from_raw(sm));
}

pub unsafe fn static_memory_str(sm: *mut StaticMemory, indent: *mut libc::c_char) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    for (i, block) in (*sm).blocks.iter().enumerate() {
        strbuilder_write!(b, "{}{i}: {}\n", cstr!(indent), block_str(block));
    }
    strbuilder_build(b)
}

pub unsafe fn static_memory_copy(sm: *mut StaticMemory) -> *mut StaticMemory {
    Box::into_raw(Box::new((*sm).clone()))
}

impl Clone for StaticMemory {
    fn clone(&self) -> Self {
        unsafe {
            StaticMemory {
                blocks: self.blocks.clone(),
                pool: pool_copy(&self.pool),
            }
        }
    }
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
    let address = (*sm).blocks.len() as libc::c_int;
    (*sm).blocks.push(block_create());
    address
}

pub unsafe fn static_memory_getblock(sm: *mut StaticMemory, address: libc::c_int) -> *mut Block {
    if address as usize >= (*sm).blocks.len() {
        return ptr::null_mut();
    }
    &mut *(*sm).blocks[address as usize]
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
