use std::collections::HashMap;
use std::ptr;

use super::Block;
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

    pub unsafe fn str(&self, indent: &str) -> OwningCStr {
        let mut b = strbuilder_create();
        for (i, block) in self.blocks.iter().enumerate() {
            strbuilder_write!(b, "{indent}{i}: {block}\n");
        }
        strbuilder_build(b)
    }

    pub fn new_block(&mut self) -> libc::c_int {
        let address = self.blocks.len() as libc::c_int;
        self.blocks.push(Block::new());
        address
    }

    pub fn has_block(&self, address: libc::c_int) -> bool {
        (address as usize) < self.blocks.len()
    }

    pub fn get_block(&mut self, address: libc::c_int) -> Option<&mut Block> {
        self.blocks
            .get_mut(address as usize)
            .map(|block| &mut **block)
    }

    /* string pooling to write one string literal to static_memory and all subsequent
     * literals reference it is used to avoid infinite loops in splitting logic with
     * different literals per selection condition */
    pub fn string_pool(&mut self, lit: &str, loc: &Location) {
        self.pool.insert(lit.to_string(), Box::new(loc.clone()));
    }

    pub unsafe fn check_pool(&self, lit: &str) -> *mut Location {
        self.pool.get(lit).map_or(ptr::null_mut(), |boxed| {
            &**boxed as *const Location as *mut Location
        })
    }
}
