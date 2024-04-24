use super::Block;
use crate::state::block::{block_create, block_str};
use crate::strbuilder_write;
use crate::util::{strbuilder_build, strbuilder_create, OwningCStr};

#[derive(Clone)]
pub struct Clump {
    pub blocks: Vec<Box<Block>>,
}

impl Clump {
    pub fn new() -> Self {
        Clump { blocks: vec![] }
    }

    pub unsafe fn str(&self, indent: &str) -> OwningCStr {
        let mut b = strbuilder_create();
        for (i, block) in self.blocks.iter().enumerate() {
            strbuilder_write!(b, "{indent}{i}: {}\n", block_str(block));
        }
        strbuilder_build(b)
    }

    pub fn new_block(&mut self) -> libc::c_int {
        let address = self.blocks.len() as libc::c_int;
        self.blocks.push(block_create());
        address
    }

    pub fn get_block(&mut self, address: libc::c_int) -> Option<&mut Block> {
        self.blocks.get_mut(address as usize).map(|blk| &mut **blk)
    }
}
