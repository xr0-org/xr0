use std::collections::HashMap;

use super::Block;
use crate::{str_write, Location};

// Note: The original had a destructor `static_memory_destroy` which leaked the allocation
// containing the static_memory value.
#[derive(Clone)]
pub struct StaticMemory {
    pub blocks: Vec<Box<Block>>,
    pub pool: HashMap<String, Box<Location>>,
}

impl StaticMemory {
    //=static_memory_create
    pub fn new() -> Self {
        StaticMemory {
            blocks: vec![],
            pool: HashMap::new(),
        }
    }

    pub fn str(&self, indent: &str) -> String {
        let mut b = String::new();
        for (i, block) in self.blocks.iter().enumerate() {
            str_write!(b, "{indent}{i}: {block}\n");
        }
        b
    }

    pub fn new_block(&mut self) -> usize {
        let address = self.blocks.len();
        self.blocks.push(Block::new());
        address
    }

    pub fn has_block(&self, address: usize) -> bool {
        address < self.blocks.len()
    }

    pub fn get_block(&self, address: usize) -> Option<&Block> {
        self.blocks.get(address).map(|block| &**block)
    }

    pub fn get_block_mut(&mut self, address: usize) -> Option<&mut Block> {
        self.blocks.get_mut(address).map(|block| &mut **block)
    }

    /* string pooling to write one string literal to static_memory and all subsequent
     * literals reference it is used to avoid infinite loops in splitting logic with
     * different literals per selection condition */
    pub fn string_pool(&mut self, lit: &str, loc: &Location) {
        self.pool.insert(lit.to_string(), Box::new(loc.clone()));
    }

    pub fn check_pool(&self, lit: &str) -> Option<&Location> {
        self.pool.get(lit).map(|loc| &**loc)
    }
}
