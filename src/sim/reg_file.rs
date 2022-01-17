use super::common::Register;
use super::traits::{ClockedMap, Value};
use std::collections::HashMap;

#[cfg(not(test))]
#[derive(Debug, Clone)]
pub struct RegFile {
    current_map: HashMap<Register, u32>,
    write_buffer: HashMap<Register, u32>,
}

#[cfg(test)]
#[derive(Debug, Clone)]
pub struct RegFile {
    current_map: HashMap<Register, u32>,
    pub write_buffer: HashMap<Register, u32>,
}

impl RegFile {
    pub fn new() -> RegFile {
        let mut current_map: HashMap<Register, u32> = HashMap::new();
        for entry in Register::iter() {
            current_map.insert(entry, 0);
        }
        let write_buffer = HashMap::new();

        RegFile {
            current_map,
            write_buffer,
        }
    }

    #[cfg(test)]
    pub fn get_write_buffer(&self) -> HashMap<Register, u32> {
        self.write_buffer.clone()
    }
}

impl Value for u32 {}

impl ClockedMap<Register, u32> for RegFile {
    fn read(&self, field: Register) -> u32 {
        *self.current_map.get(&field).unwrap()
    }

    fn clock(&mut self) {
        for (r, v) in self.write_buffer.drain() {
            match r {
                Register::ZERO => {}
                _ => {
                    self.current_map.insert(r, v).unwrap();
                }
            }
        }
    }

    fn load(&mut self, field: Register, value: u32) {
        self.write_buffer.insert(field, value);
    }

    fn clear_pending(&mut self) {
        self.write_buffer.drain();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_init() {
        let reg = RegFile::new();
        for r in Register::iter() {
            assert_eq!(reg.read(r), 0);
        }
    }

    #[test]
    fn test_clocking() {
        let mut reg = RegFile::new();
        reg.load(Register::A0, 1999);

        assert_eq!(reg.read(Register::A0), 0);
        reg.clock();

        assert_eq!(reg.read(Register::A0), 1999);
    }

    #[test]
    fn test_write_to_zero_reg() {
        let mut reg = RegFile::new();
        reg.load(Register::ZERO, 190124);
        reg.clock();
        assert_eq!(reg.read(Register::ZERO), 0);
    }

    #[test]
    fn test_write_buffer_empty_after_clock() {
        let mut reg = RegFile::new();
        reg.load(Register::V1, 1241);
        reg.load(Register::V0, 1291037);
        reg.load(Register::A2, 500);

        assert_eq!(reg.get_write_buffer().len(), 3);

        reg.clock();
        assert_eq!(reg.get_write_buffer().len(), 0);
    }
}
