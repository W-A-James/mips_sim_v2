mod alu;
mod controller;
mod instruction;
mod mem;
mod pipe_reg;
mod reg_file;
mod stalling;

pub mod common;
pub mod traits;
use common::*;
use pipe_reg::{PipeField, PipeFieldName};
use traits::ClockedMap;

#[derive(Debug)]
pub struct Sim {
    alu: alu::ALU,
    stalling_unit: stalling::StallingUnit,
    reg_file: reg_file::RegFile,
    if_id_reg: pipe_reg::PipeRegister,
    id_ex_reg: pipe_reg::PipeRegister,
    ex_mem_reg: pipe_reg::PipeRegister,
    mem_wb_reg: pipe_reg::PipeRegister,
    pc: pipe_reg::PipeRegister,
    halt: pipe_reg::PipeRegister,

    status_reg: pipe_reg::PipeRegister,
    cause_reg: pipe_reg::PipeRegister,
    epc_reg: pipe_reg::PipeRegister,
    bad_v_addr: pipe_reg::PipeRegister,
    controller: controller::Controller,
    memory: mem::Memory,
}

#[derive(Debug, Clone, Copy)]
pub struct SimState;

// -------- Public API ---------
impl Sim {
    pub fn new() -> Sim {
        let if_id_reg: pipe_reg::PipeRegister;
        let id_ex_reg: pipe_reg::PipeRegister;
        let ex_mem_reg: pipe_reg::PipeRegister;
        let mem_wb_reg: pipe_reg::PipeRegister;
        let pc: pipe_reg::PipeRegister;
        let status_reg: pipe_reg::PipeRegister;
        let epc_reg: pipe_reg::PipeRegister;
        let bad_v_addr: pipe_reg::PipeRegister;
        let cause_reg: pipe_reg::PipeRegister;
        let reg_file = reg_file::RegFile::new();
        let controller = controller::Controller::new();
        let memory = mem::Memory::new();
        let alu = alu::ALU {};
        let stalling_unit = stalling::StallingUnit::new();
        let halt: pipe_reg::PipeRegister;

        {
            use pipe_reg::PipeRegister;
            // TODO: Determine fields needed for each pipe register
            if_id_reg = PipeRegister::new(
                "IF/ID",
                vec![
                    PipeFieldName::PcPlus4,
                    PipeFieldName::Instruction,
                    PipeFieldName::InstructionPc,
                ],
            );
            id_ex_reg = PipeRegister::new(
                "ID/EX",
                vec![
                    PipeFieldName::PcPlus4,
                    PipeFieldName::Reg1,
                    PipeFieldName::Reg2,
                    PipeFieldName::Muldivhi,
                    PipeFieldName::Muldivlo,
                    PipeFieldName::MuldivReqValid,
                    PipeFieldName::SignExtImm,
                    PipeFieldName::Rt,
                    PipeFieldName::Rd,
                    PipeFieldName::Shamt,
                    PipeFieldName::JumpTarget,
                    PipeFieldName::WriteReg,
                    PipeFieldName::ReadMem,
                    PipeFieldName::WriteMem,
                    PipeFieldName::MemWidth,
                    PipeFieldName::MemSigned,
                    PipeFieldName::AluSrc1,
                    PipeFieldName::AluSrc2,
                    PipeFieldName::AluOp,
                    PipeFieldName::AluToReg,
                    PipeFieldName::RegDest,
                    PipeFieldName::Halt,
                    PipeFieldName::IsNop,
                    PipeFieldName::IsBranch,
                    PipeFieldName::BranchType,
                    PipeFieldName::InstructionPc,
                    PipeFieldName::Instruction,
                ],
            );
            ex_mem_reg = PipeRegister::new(
                "EX/MEM",
                vec![
                    PipeFieldName::Reg2,
                    PipeFieldName::RegDest,
                    PipeFieldName::WriteReg,
                    PipeFieldName::WriteMem,
                    PipeFieldName::ReadMem,
                    PipeFieldName::MemWidth,
                    PipeFieldName::MemSigned,
                    PipeFieldName::IsBranch,
                    PipeFieldName::ALURes,
                    PipeFieldName::AluToReg,
                    PipeFieldName::MuldivRes,
                    PipeFieldName::MuldivReqValid,
                    PipeFieldName::JumpTarget,
                    PipeFieldName::Halt,
                    PipeFieldName::IsNop,
                    PipeFieldName::InDelaySlot,
                    PipeFieldName::InstructionPc,
                    PipeFieldName::Instruction,
                ],
            );
            mem_wb_reg = PipeRegister::new(
                "MEM/WB",
                vec![
                    PipeFieldName::ALURes,
                    PipeFieldName::MuldivRes,
                    PipeFieldName::MuldivReqValid,
                    PipeFieldName::MemData,
                    PipeFieldName::MemWidth,
                    PipeFieldName::RegDest,
                    PipeFieldName::WriteReg,
                    PipeFieldName::AluToReg,
                    PipeFieldName::Halt,
                    PipeFieldName::IsNop,
                    PipeFieldName::InDelaySlot,
                    PipeFieldName::Instruction,
                    PipeFieldName::InstructionPc,
                ],
            );
            pc = PipeRegister::new("PC", vec![PipeFieldName::PC]);
            status_reg = PipeRegister::new("STATUS", vec![PipeFieldName::Status]);
            epc_reg = PipeRegister::new("EPC", vec![PipeFieldName::EPC]);
            bad_v_addr = PipeRegister::new("BAD_V_ADDR", vec![PipeFieldName::BadVAddr]);
            cause_reg = PipeRegister::new("CAUSE", vec![PipeFieldName::Cause]);
            halt = PipeRegister::new("HALT", vec![PipeFieldName::Halt]);
        }

        let mut sim = Sim {
            stalling_unit,
            if_id_reg,
            id_ex_reg,
            ex_mem_reg,
            mem_wb_reg,
            pc,
            status_reg,
            epc_reg,
            bad_v_addr,
            cause_reg,
            controller,
            memory,
            alu,
            reg_file,
            halt,
        };

        sim.initialize_registers();

        sim
    }

    pub fn step(&mut self, n: u32) {
        for _ in 0..n {
            self._step();
        }
    }

    pub fn step_to_halt(&mut self) {
        loop {
            match self.halt.read(PipeFieldName::Halt) {
                PipeField::Halt(halt) => {
                    if halt {
                        break;
                    } else {
                        self._step();
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn load_binary(&mut self, instrs: &Vec<u32>, data: &Vec<u32>) {
        let mut mem_index = TEXT_START;
        for v in instrs {
            self.memory.load(mem_index, *v);
            mem_index = mem_index + 4;
        }
        let data_start = mem_index;
        for v in data {
            self.memory.load(mem_index, *v);
            mem_index += 4;
        }

        self.memory.clock();
    }

    pub fn get_state() -> SimState {
        SimState {}
    }
}

// --------- Pipeline stage implementation ---------
impl Sim {
    fn fetch_stage(&mut self) {
        let (stall_fetch, squash_fetch): (bool, bool);
        match (
            self.stalling_unit
                .get_state(PipeFieldName::StallFetch)
                .unwrap(),
            self.stalling_unit
                .get_state(PipeFieldName::SquashFetch)
                .unwrap(),
        ) {
            (PipeField::StallFetch(stall), PipeField::SquashFetch(squash)) => {
                stall_fetch = stall;
                squash_fetch = squash;
            }
            _ => unreachable!(),
        }

        // Check if this stage is being squashed
        if squash_fetch {
            // TODO
            // don't update state and send a bubble
        } else if stall_fetch {
            // TODO
            // Check if this stage should be sending a bubble
            // Check if this stage is stalling
            // if stalling:
            //      send nop
        }
        // else:
        //      Read from memory at current value of pc
        //      send value to if/id pipe register
        else {
            if let PipeField::PC(pc) = self.pc.read(PipeFieldName::PC) {
                let instr = self.memory.read(pc);
                self.if_id_reg
                    .load(PipeFieldName::Instruction, PipeField::Instruction(instr));
                self.if_id_reg
                    .load(PipeFieldName::InstructionPc, PipeField::InstructionPc(pc));
                self.if_id_reg
                    .load(PipeFieldName::PcPlus4, PipeField::InstructionPc(pc + 4));

                let (is_branch, is_jump, alu_res_zero): (bool, bool, bool);
                match (
                    self.ex_mem_reg.read(PipeFieldName::IsBranch),
                    self.ex_mem_reg.read(PipeFieldName::IsJump),
                    self.ex_mem_reg.read(PipeFieldName::ALURes),
                ) {
                    (
                        PipeField::IsBranch(branch),
                        PipeField::IsJump(jump),
                        PipeField::ALURes(alu_res),
                    ) => {
                        is_branch = branch;
                        is_jump = jump;
                        alu_res_zero = alu_res == 0u32;
                    }
                    _ => unreachable!(),
                }

                if (is_branch || is_jump) && alu_res_zero {
                    match self.ex_mem_reg.read(PipeFieldName::JumpTarget) {
                        PipeField::JumpTarget(target) => {
                            self.pc.load(PipeFieldName::PC, PipeField::PC(target))
                        }
                        _ => unreachable!(),
                    }
                } else {
                    self.pc.load(PipeFieldName::PC, PipeField::PC(pc + 4));
                }
            }
        }
    }

    fn decode_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      update controller with instruction value
        self.stalling_unit.update_state(&self.id_ex_reg);
    }

    fn execute_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      send info to memory stage
        self.stalling_unit.update_state(&self.ex_mem_reg);
    }

    fn memory_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      send info to wb stage
    }

    fn writeback_stage(&mut self) {
        // Check if stalling
        // if stalling:
        //      do nothing
        //  else:
        //      send info to registers
    }

    fn _step(&mut self) {
        self.fetch_stage();
        self.decode_stage();
        self.execute_stage();
        self.memory_stage();
        self.writeback_stage();

        self.if_id_reg.clock();
        self.id_ex_reg.clock();
        self.ex_mem_reg.clock();
        self.mem_wb_reg.clock();

        // TODO:
        self.pc.clock();
        self.status_reg.clock();
        self.epc_reg.clock();
        self.bad_v_addr.clock();

        self.reg_file.clock();

        self.memory.clock();
    }

    fn initialize_registers(&mut self) {
        //use common::Register;
        use PipeField::PC;

        self.reg_file.load(Register::SP, STACK_POINTER_INITIAL);
        self.reg_file.load(Register::FP, STACK_POINTER_INITIAL);
        self.pc.load(PipeFieldName::PC, PC(TEXT_START));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_fetch_stage() {
        let mut sim = Sim::new();
    }

    #[test]
    pub fn test_decode_stage() {
        let mut sim = Sim::new();
    }

    #[test]
    pub fn test_execute_stage() {
        let mut sim = Sim::new();
    }

    #[test]
    pub fn test_memory_stage() {
        let mut sim = Sim::new();
    }

    #[test]
    pub fn test_writeback_stage() {
        let mut sim = Sim::new();
    }
}
