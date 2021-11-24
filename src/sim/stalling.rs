use super::common::Register;
use super::pipe_reg::{PipeField, PipeFieldName, PipeRegister};
use super::traits::ClockedMap;
use std::collections::HashMap;

#[derive(Debug)]
pub struct StallingUnit {
    write_in_flight: HashMap<Register, bool>,
    signal: HashMap<PipeFieldName, PipeField>,
    // Need state to handle taking branches/jumps
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
                signal.insert(v, PipeField::XXX);
            }
        }

        for r in Register::iter() {
            write_in_flight.insert(r, false);
        }

        StallingUnit {
            write_in_flight,
            signal,
        }
    }

    pub fn update_state(&mut self, p_reg: &PipeRegister) {
        // Get info from decode first,
        // if necessary, overwrite with state from execute stage
        let reg_name = p_reg.get_name();
        if reg_name == "ID/EX" {
            let read_reg_1 = p_reg.read(PipeFieldName::AluSrc1);
            let read_reg_2 = p_reg.read(PipeFieldName::AluSrc2);
            // Check RegDest/WriteReg
            let write_reg = p_reg.read(PipeFieldName::WriteReg);
            let reg_dest = p_reg.read(PipeFieldName::RegDest);

        } else if reg_name == "EX/MEM" {
        }

        // When we are writing to a register
        // add to writes in flight

        // When we are reading from a register that needs to be written to
        // stall the front end of the pipeline (fetch and decode)

        // when we know we are taking a branch or jump (from decode with unconditional
        // or from execute with conditional) flush pipeline of instructions that followed the jump
        //  from conditional
        //      flush fetch
        //      flush decode
        //  from unconditional
        //      flush fetch
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

    pub fn get_state(&self, field_name: PipeFieldName) -> Option<PipeField> {
        match self.signal.get(&field_name) {
            Some(&v) => Some(v),
            None => None,
        }
    }
}
