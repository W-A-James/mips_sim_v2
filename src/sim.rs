mod alu;
mod controller;
mod instruction;
mod mem;
#[macro_use]
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
                    PipeFieldName::IsJump,
                    PipeFieldName::BranchType,
                    PipeFieldName::InstructionPc,
                    PipeFieldName::Instruction,
                    PipeFieldName::InDelaySlot,
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
                    PipeFieldName::BranchTarget,
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
macro_rules! get_alu_src_val {
    ($sim: expr, $alu_src: expr, $field_name: ident) => {{
        if let PipeField::$field_name(src) = $sim.id_ex_reg.read(PipeFieldName::$field_name) {
            $alu_src = match src {
                ALUSrc::Shamt => {
                    if let PipeField::Shamt(val) = $sim.id_ex_reg.read(PipeFieldName::Shamt) {
                        val as u32
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::Zero => 0,
                ALUSrc::Reg1 => {
                    if let PipeField::Reg1(val) = $sim.id_ex_reg.read(PipeFieldName::Reg1) {
                        val
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::Reg2 => {
                    if let PipeField::Reg2(val) = $sim.id_ex_reg.read(PipeFieldName::Reg2) {
                        val
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::Muldivlo => {
                    if let PipeField::Muldivlo(val) = $sim.id_ex_reg.read(PipeFieldName::Muldivlo) {
                        val
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::Muldivhi => {
                    if let PipeField::Muldivhi(val) = $sim.id_ex_reg.read(PipeFieldName::Muldivhi) {
                        val
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::SignExtImm => {
                    if let PipeField::SignExtImm(val) =
                        $sim.id_ex_reg.read(PipeFieldName::SignExtImm)
                    {
                        val
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::ZeroExtImm => {
                    if let PipeField::SignExtImm(val) =
                        $sim.id_ex_reg.read(PipeFieldName::SignExtImm)
                    {
                        val & 0x0000_FFFF
                    } else {
                        unreachable!();
                    }
                }
                ALUSrc::PcPlus4 => {
                    if let PipeField::PcPlus4(val) = $sim.id_ex_reg.read(PipeFieldName::PcPlus4) {
                        val
                    } else {
                        unreachable!();
                    }
                }
            }
        } else {
            unreachable!();
        }
    }};
}

impl Sim {
    fn sign_ext_imm(imm: u16) -> u32 {
        let mut rv: u32;
        if imm >> 15 == 1 {
            rv = 0xFFFF_0000;
        } else {
            rv = 0x0000_0000;
        }
        rv |= imm as u32;
        rv
    }

    fn fetch_stage(&mut self, stall: bool, squash: bool) {
        // Check if this stage is being squashed
        if squash {
            // TODO
            // don't update state and send a bubble
        } else if stall {
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
                insert_pipe_value![self.if_id_reg, Instruction, instr];
                insert_pipe_value![self.if_id_reg, InstructionPc, pc];
                insert_pipe_value![self.if_id_reg, PcPlus4, pc + 4];

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
                            insert_pipe_value![self.pc, PC, target];
                        }
                        _ => unreachable!(),
                    }
                } else {
                    insert_pipe_value![self.pc, PC, pc + 4];
                }
            }
        }
    }

    fn decode_stage(&mut self, stall: bool, squash: bool) {
        if squash {
            // TODO
        } else if stall {
            // TODO
        } else {
            let instr: u32;
            match (
                self.if_id_reg.read(PipeFieldName::Instruction),
                self.if_id_reg.read(PipeFieldName::InstructionPc),
                self.if_id_reg.read(PipeFieldName::PcPlus4),
            ) {
                (
                    PipeField::Instruction(i),
                    PipeField::InstructionPc(i_pc),
                    PipeField::PcPlus4(pc_p_4),
                ) => {
                    instr = i;
                    insert_pipe_value![self.id_ex_reg, InstructionPc, i_pc];
                    insert_pipe_value![self.id_ex_reg, PcPlus4, pc_p_4];
                }
                _ => unreachable!(),
            }
            let parsed_instr = instruction::Instruction::new(instr);
            match parsed_instr {
                Ok(instruction) => {
                    self.controller.update_state(&instruction);
                    // Reg1
                    match self.controller.get_state(PipeFieldName::Reg1Src).unwrap() {
                        PipeField::Reg1Src(RegSrc::Rt) => {
                            let reg_1_val: u32;
                            if let Some(rt) = instruction.get_rt() {
                                reg_1_val = self.reg_file.read(rt);
                            } else {
                                // NOTE
                                reg_1_val = 0;
                            }

                            insert_pipe_value![self.id_ex_reg, Reg1, reg_1_val];
                        }
                        PipeField::Reg1Src(RegSrc::Rs) => {
                            let reg_1_val: u32;
                            if let Some(rs) = instruction.get_rs() {
                                reg_1_val = self.reg_file.read(rs);
                            } else {
                                // NOTE
                                reg_1_val = 0;
                            }
                            insert_pipe_value![self.id_ex_reg, Reg1, reg_1_val];
                        }
                        PipeField::Reg1Src(RegSrc::XXX) => {}
                        _ => {}
                    }
                    // Reg2
                    match self.controller.get_state(PipeFieldName::Reg1Src).unwrap() {
                        PipeField::Reg1Src(RegSrc::Rt) => {
                            let reg_2_val: u32;
                            if let Some(rt) = instruction.get_rt() {
                                reg_2_val = self.reg_file.read(rt);
                            } else {
                                // NOTE
                                reg_2_val = 0;
                            }

                            insert_pipe_value![self.id_ex_reg, Reg2, reg_2_val];
                        }
                        PipeField::Reg1Src(RegSrc::Rs) => {
                            let reg_2_val: u32;
                            if let Some(rs) = instruction.get_rt() {
                                reg_2_val = self.reg_file.read(rs);
                            } else {
                                // NOTE
                                reg_2_val = 0;
                            }

                            insert_pipe_value![self.id_ex_reg, Reg2, reg_2_val];
                        }
                        PipeField::Reg1Src(RegSrc::XXX) => {}
                        _ => {}
                    }
                    // PcPlus4
                    self.id_ex_reg.load(
                        PipeFieldName::PcPlus4,
                        self.if_id_reg.read(PipeFieldName::PcPlus4),
                    );
                    // Muldivhi
                    insert_pipe_value![self.id_ex_reg, Muldivhi, self.reg_file.read(Register::HI)];
                    // Muldivlo
                    insert_pipe_value![self.id_ex_reg, Muldivlo, self.reg_file.read(Register::LO)];
                    // SignExtImm
                    insert_pipe_value![
                        self.id_ex_reg,
                        SignExtImm,
                        match instruction.get_imm() {
                            Some(shamt) => Sim::sign_ext_imm(shamt),
                            None => 0,
                        }
                    ];
                    // Rt
                    insert_pipe_value![
                        self.id_ex_reg,
                        Rt,
                        match instruction.get_rt() {
                            Some(reg) => reg as u8,
                            None => 0,
                        }
                    ];
                    // Rd
                    insert_pipe_value![
                        self.id_ex_reg,
                        Rd,
                        match instruction.get_rd() {
                            Some(reg) => reg as u8,
                            None => 0,
                        }
                    ];
                    // Shamt
                    insert_pipe_value![
                        self.id_ex_reg,
                        Rd,
                        match instruction.get_shamt() {
                            Some(shamt) => shamt,
                            None => 0,
                        }
                    ];
                    // IsNop
                    insert_pipe_value![self.id_ex_reg, IsNop, instruction.is_nop()];
                    // Halt
                    insert_pipe_value![self.id_ex_reg, Halt, instruction.is_halt()];
                }
                Err(e) => panic!("{:#?}", e),
            }

            // Update the id_ex register
            for (field, value) in self.controller.get_state_vec() {
                // TODO: ensure that only the fields that should be in the id_ex
                //       register are loaded into it
                self.id_ex_reg.load(field, value);
            }
        }
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      update controller with instruction value
        self.stalling_unit.update_state(&self.id_ex_reg);
    }

    fn execute_stage(&mut self, stall: bool, squash: bool) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      send info to memory stage
        if squash {
        } else if stall {
        } else {
            let alu_src_1: u32;
            let alu_src_2: u32;

            // get alu operands
            get_alu_src_val![self, alu_src_1, AluSrc1];
            get_alu_src_val![self, alu_src_2, AluSrc2];

            let alu_result: u32;
            let alu_operation: ALUOperation;
            // Get alu op
            if let PipeField::AluOp(alu_op) = self.id_ex_reg.read(PipeFieldName::AluOp) {
                alu_operation = alu_op;
                match alu::ALU::calculate(alu_src_1, alu_src_2, alu_op) {
                    Ok(result) => {
                        alu_result = result;
                    }
                    Err(e) => {
                        panic!("{:#?}", e);
                    }
                }

                // check if muldiv request is valid
                if let PipeField::MuldivReqValid(muldiv_request) =
                    self.id_ex_reg.read(PipeFieldName::MuldivReqValid)
                {
                    if muldiv_request {
                        let muldiv_result = match alu_operation {
                            ALUOperation::MULT => alu::ALU::multiply(alu_src_1, alu_src_2, true),
                            ALUOperation::MULTU => alu::ALU::multiply(alu_src_1, alu_src_2, false),
                            ALUOperation::DIV => alu::ALU::divide(alu_src_1, alu_src_2, true),
                            ALUOperation::DIVU => alu::ALU::divide(alu_src_1, alu_src_2, false),
                            _ => unreachable!(),
                        };

                        // load this into register
                    }
                }
            }

            // Calculate branch target
            if let PipeField::SignExtImm(sign_ext_imm) =
                self.id_ex_reg.read(PipeFieldName::SignExtImm)
            {
                let offset = 0x0000_FFFF & sign_ext_imm;
                let branch_target = match self.id_ex_reg.read(PipeFieldName::PcPlus4) {
                    PipeField::PcPlus4(pc_plus_4) => pc_plus_4 + (offset << 2),
                    _ => unreachable!(),
                };

                insert_pipe_value![self.ex_mem_reg, BranchTarget, branch_target];
            }

            // Determine Register Dst
            if let PipeField::RegDest(dest) = self.id_ex_reg.read(PipeFieldName::RegDest) {
                match dest {
                    RegDest::Rd => {}
                    RegDest::Ra => {}
                    RegDest::Rt => {}
                    RegDest::MulDivHi => {}
                    RegDest::MulDivLo => {}
                    RegDest::XXX => {}
                }
            }
        }
        self.stalling_unit.update_state(&self.ex_mem_reg);
    }

    fn memory_stage(&mut self, stall: bool, squash: bool) {
        // Check if stalling
        // if stalling:
        //      send nop
        //  else:
        //      send info to wb stage
    }

    fn writeback_stage(&mut self, stall: bool, squash: bool) {
        // Check if stalling
        // if stalling:
        //      do nothing
        //  else:
        //      send info to registers
    }

    fn _step(&mut self) {
        // get signals from stalling unit
        let (stall_fetch, stall_decode, stall_execute, stall_memory, stall_writeback): (
            bool,
            bool,
            bool,
            bool,
            bool,
        );
        let (squash_fetch, squash_decode, squash_execute, squash_memory, squash_writeback): (
            bool,
            bool,
            bool,
            bool,
            bool,
        );
        {
            use PipeFieldName::*;
            let s: &self::stalling::StallingUnit = &self.stalling_unit;
            match (
                s.get_state(StallFetch).unwrap(),
                s.get_state(StallDecode).unwrap(),
                s.get_state(StallExecute).unwrap(),
                s.get_state(StallMemory).unwrap(),
                s.get_state(StallWriteback).unwrap(),
            ) {
                (
                    PipeField::StallFetch(sf),
                    PipeField::StallDecode(sd),
                    PipeField::StallExecute(se),
                    PipeField::StallMemory(sm),
                    PipeField::StallWriteback(sw),
                ) => {
                    stall_fetch = sf;
                    stall_decode = sd;
                    stall_execute = se;
                    stall_memory = sm;
                    stall_writeback = sw;
                }
                _ => unreachable!(),
            }
            match (
                s.get_state(SquashFetch).unwrap(),
                s.get_state(SquashDecode).unwrap(),
                s.get_state(SquashExecute).unwrap(),
                s.get_state(SquashMemory).unwrap(),
                s.get_state(SquashWriteback).unwrap(),
            ) {
                (
                    PipeField::SquashFetch(sf),
                    PipeField::SquashDecode(sd),
                    PipeField::SquashExecute(se),
                    PipeField::SquashMemory(sm),
                    PipeField::SquashWriteback(sw),
                ) => {
                    squash_fetch = sf;
                    squash_decode = sd;
                    squash_execute = se;
                    squash_memory = sm;
                    squash_writeback = sw;
                }
                _ => unreachable!(),
            }
        }
        self.fetch_stage(stall_fetch, squash_fetch);
        self.decode_stage(stall_decode, squash_decode);
        self.execute_stage(stall_execute, squash_execute);
        self.memory_stage(stall_memory, squash_memory);
        self.writeback_stage(stall_writeback, squash_writeback);

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
        // Set values
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
