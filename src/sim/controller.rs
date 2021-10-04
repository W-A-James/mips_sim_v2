use super::common::Register;
use super::pipe_reg::{PipeField, PipeFieldName};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Controller {
    signal: HashMap<PipeFieldName, PipeField>,
    write_in_flight: HashMap<Register, bool>,
}

impl Controller {
    pub fn new() -> Controller {
        let mut signal = HashMap::new();
        let mut write_in_flight = HashMap::new();
        {
            use PipeFieldName::*;
            for v in vec![
                RegDest,
                AluSrc1,
                AluSrc2,
                AluOp,
                MuldivReqValid,
                WriteReg,
                InDelaySlot,
                ReadMem,
                WriteMem,
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
                Halt,
            ] {
                signal.insert(v, PipeField::XXX).unwrap();
            }
        }

        for r in Register::iter() {
            write_in_flight.insert(r, false).unwrap();
        }

        Controller {
            signal,
            write_in_flight,
        }
    }

    pub fn get_next_state(&mut self) {}
}
