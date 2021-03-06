use super::common::Register;
use super::pipe_reg::{PipeField, PipeFieldName, DEFAULT_VALUES};
use super::traits::ClockedMap;
use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use serde::Serialize;

#[derive(Hash, Eq, PartialEq, Debug, Clone, Serialize)]
struct PipeVal {
    pub name: PipeFieldName,
    pub value: PipeField,
}


#[derive(Debug, Clone, Serialize)]
pub struct StallingUnit {
    write_in_flight: HashMap<Register, bool>,
    signal: HashMap<PipeFieldName, PipeField>,
    signal_write_buffer: HashMap<PipeFieldName, PipeField>,
}


impl StallingUnit {
    pub fn new() -> StallingUnit {
        let mut write_in_flight = HashMap::new();
        let mut signal = HashMap::new();
        {
            use PipeFieldName::*;
            for v in vec![
                StallFetch,
                StallDecode,
                StallExecute,
                StallMemory,
                StallWriteback,
                BubbleDecode,
                BubbleExecute,
                BubbleMemory,
                BubbleWriteback,
                SquashFetch,
                SquashDecode,
                SquashExecute,
                SquashMemory,
                SquashWriteback,
            ] {
                signal.insert(v, *DEFAULT_VALUES.get(&v).unwrap());
            }
        }

        for r in Register::iter() {
            write_in_flight.insert(r, false);
        }

        StallingUnit {
            write_in_flight,
            signal,
            signal_write_buffer: HashMap::new(),
        }
    }

    pub fn start_write_in_flight(&mut self, reg: Register) {
        self.write_in_flight.insert(reg, true);
    }

    pub fn clear_write_in_flight(&mut self, reg: Register) {
        self.write_in_flight.insert(reg, false);
    }

    pub fn check_write_in_flight(&self, reg: Register) -> bool {
        *self.write_in_flight.get(&reg).unwrap()
    }
}

impl ClockedMap<PipeFieldName, PipeField> for StallingUnit {
    fn load(&mut self, field: PipeFieldName, value: PipeField) {
        if self.signal.contains_key(&field) {
            self.signal_write_buffer.insert(field, value);
        } else {
        }
    }

    fn clock(&mut self) {
        for (k, v) in self.signal_write_buffer.drain() {
            self.signal.insert(k, v).unwrap();
        }
    }

    fn read(&self, field: PipeFieldName) -> PipeField {
        *self.signal.get(&field).unwrap()
    }

    fn clear_pending(&mut self) {
        self.signal_write_buffer.drain();
    }
}
