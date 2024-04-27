use super::Block;
use crate::strbuilder_write;

#[derive(Clone)]
pub struct Clump {
    pub blocks: Vec<Box<Block>>,
}

impl Clump {
    pub fn new() -> Self {
        Clump { blocks: vec![] }
    }

    pub unsafe fn str(&self, indent: &str) -> String {
        let mut b = String::new();
        for (i, block) in self.blocks.iter().enumerate() {
            strbuilder_write!(b, "{indent}{i}: {block}\n");
        }
        b
    }

    pub fn new_block(&mut self) -> libc::c_int {
        let address = self.blocks.len() as libc::c_int;
        self.blocks.push(Block::new());
        address
    }

    pub fn get_block(&mut self, address: libc::c_int) -> Option<&mut Block> {
        self.blocks.get_mut(address as usize).map(|blk| &mut **blk)
    }
}
