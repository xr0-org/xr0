use crate::state::block::{block_create, block_str};
use crate::state::location::location_copy;
use crate::util::{dynamic_str, strbuilder_build, strbuilder_create, Map, OwningCStr};
use crate::{strbuilder_write, Block, Location, StrBuilder};

pub struct StaticMemory {
    pub blocks: Vec<Box<Block>>,
    pub pool: Box<Map>,
}

impl StaticMemory {
    pub fn new() -> Self {
        StaticMemory {
            blocks: vec![],
            pool: Map::new(),
        }
    }
}

pub unsafe fn static_memory_str(sm: &StaticMemory, indent: &str) -> OwningCStr {
    let b: *mut StrBuilder = strbuilder_create();
    for (i, block) in sm.blocks.iter().enumerate() {
        strbuilder_write!(b, "{indent}{i}: {}\n", block_str(block));
    }
    strbuilder_build(b)
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
            Box::into_raw(location_copy(&*(v as *mut Location))) as *const libc::c_void,
        );
    }
    pcopy
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

pub unsafe fn static_memory_stringpool(
    sm: &mut StaticMemory,
    lit: *mut libc::c_char,
    loc: *mut Location,
) {
    sm.pool.set(
        dynamic_str(lit),
        Box::into_raw(location_copy(&*loc)) as *const libc::c_void,
    );
}

pub unsafe fn static_memory_checkpool(sm: &StaticMemory, lit: *mut libc::c_char) -> *mut Location {
    sm.pool.get(lit) as *mut Location
}
