use super::Block;
use crate::str_write;

#[derive(Clone)]
pub struct Clump {
    pub blocks: Vec<Box<Block>>,
}

impl Clump {
    //=clump_create
    pub fn new() -> Self {
        Clump { blocks: vec![] }
    }

    //=clump_str
    pub fn str(&self, indent: &str) -> String {
        let mut b = String::new();
        for (i, block) in self.blocks.iter().enumerate() {
            str_write!(b, "{indent}{i}: {block}\n");
        }
        b
    }

    //=clump_newblock
    pub fn new_block(&mut self) -> usize {
        let address = self.blocks.len();
        self.blocks.push(Block::new());
        address
    }

    //=clump_getblock (non-mut version)
    pub fn get_block(&self, address: usize) -> Option<&Block> {
        self.blocks.get(address).map(|blk| &**blk)
    }

    //=clump_getblock
    pub fn get_block_mut(&mut self, address: usize) -> Option<&mut Block> {
        self.blocks.get_mut(address).map(|blk| &mut **blk)
    }
}
