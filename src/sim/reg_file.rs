use super::traits::{ClockedMap, Field};
use std::collections::HashMap;
use std::iter::Iterator;

#[repr(u8)]
#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub enum Register {
    ZERO,
    AT,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    T9,
    K0,
    K1,
    GP,
    SP,
    FP,
    RA,
    HI,
    LO,
}

impl Register {
    pub fn iter() -> impl Iterator<Item = Register> {
        use Register::*;
        [
            ZERO, AT, V0, V1, A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, S0, S1, S2, S3, S4,
            S5, S6, S7, T8, T9, K0, K1, GP, SP, FP, RA, HI, LO,
        ]
        .iter()
        .copied()
    }
}

impl Field for Register {}

#[derive(Debug)]
pub struct RegFile {
    current_map: HashMap<Register, u32>,
    write_buffer: Vec<(Register, u32)>,
}

impl RegFile {
    pub fn new() -> RegFile {
        let mut current_map: HashMap<Register, u32> = HashMap::new();
        for entry in Register::iter() {
            current_map.insert(entry, 0);
        }
        let next_map = current_map.clone();
        let write_buffer = Vec::new();

        RegFile {
            current_map,
            write_buffer,
        }
    }
}

impl ClockedMap<Register, u32> for RegFile {
    fn read(&self, field: Register) -> u32 {
        *self.current_map.get(&field).unwrap()
    }

    fn clock(&mut self) {
        for (k, v) in self.write_buffer.drain(..) {
            self.current_map.insert(k, v).unwrap();
        }
    }

    fn load(&mut self, field: Register, value: u32) {
        self.write_buffer.push((field, value));
    }
}
