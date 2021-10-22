use std::collections::HashMap;
use super::traits::{ClockedMap, Value};

#[derive(Debug)]
pub struct Memory {
    map: HashMap<u32, u32>,
    write_buffer: Vec<(u32, u32)>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            map: HashMap::new(),
            write_buffer: Vec::new()
        }
    }
}

impl ClockedMap<u32, u32> for Memory {
    fn read(&self, field: u32) -> u32 {
        *self.map.get(&field).unwrap()
    }

    fn clock(&mut self) {
        for (k, v) in self.write_buffer.drain(..) {
            let rem = k % 4; 
            match rem {
                0 => {
                    self.map.insert(k, v).unwrap();
                }
                1.. => {
                    // place 4 - rem upper bytes in lower bytes of field at k - rem 
                    // place last byte at (k - rem) + 4
                    // TODO: handle this
                }
            }
            // TODO: Check whether or not value is word/half/byte aligned
        }
    }

    fn load(&mut self, field: u32, value: u32) {
        self.write_buffer.push((field, value));
    }
}
