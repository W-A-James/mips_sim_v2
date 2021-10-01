#[derive(Debug)]
pub struct Memory;

impl Memory {
    pub fn new() -> Memory {
        Memory {}
    }

    pub fn set(&mut self, address: u32, value: u32) {}
}
