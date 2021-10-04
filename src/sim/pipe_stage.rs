use super::Sim;

impl Sim {
    pub fn fetch_stage(&mut self) {
        // Check if this stage is stalling
        // if stalling:
        //      send nop
        // else:
        //      Read from memory at current value of pc
        //      send value to if/id pipe register
    }

    pub fn decode_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      update controller with instruction value
    }

    pub fn execute_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      send info to memory stage
    }

    pub fn memory_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      send info to wb stage
    }

    pub fn writeback_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      do nothing
        //  else:
        //      send info to wb stage
    }
}
