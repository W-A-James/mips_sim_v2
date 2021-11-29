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
use std::convert::TryFrom;
use traits::ClockedMap;

#[derive(Debug)]
pub struct Sim {
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
                    PipeFieldName::WriteReg,
                    PipeFieldName::RegToWrite,
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
                    PipeFieldName::RegToWrite,
                    PipeFieldName::WriteReg,
                    PipeFieldName::RegToWrite,
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

macro_rules! get_val_from_pipe_reg {
    ($reg: expr, $field_name: ident) => {{
        if let PipeField::$field_name(v) = $reg.read(PipeFieldName::$field_name) {
            Some(v)
        } else {
            None
        }
    }};
}

macro_rules! pass_through_val {
    ($src: expr, $dest: expr, $field_name: ident) => {{
        insert_pipe_value![
            $dest,
            $field_name,
            get_val_from_pipe_reg!($src, $field_name).unwrap()
        ];
    }};
}

macro_rules! get_alu_src_val {
    ($sim: expr, $alu_src: expr, $field_name: ident) => {{
        let src = get_val_from_pipe_reg!($sim.id_ex_reg, $field_name).unwrap();
        $alu_src = match src {
            ALUSrc::Shamt => get_val_from_pipe_reg!($sim.id_ex_reg, Shamt).unwrap() as u32,

            ALUSrc::Zero => 0,
            ALUSrc::Reg1 => get_val_from_pipe_reg!($sim.id_ex_reg, Reg1).unwrap(),
            ALUSrc::Reg2 => get_val_from_pipe_reg!($sim.id_ex_reg, Reg2).unwrap(),
            ALUSrc::Muldivlo => get_val_from_pipe_reg!($sim.id_ex_reg, Muldivlo).unwrap(),
            ALUSrc::Muldivhi => get_val_from_pipe_reg!($sim.id_ex_reg, Muldivhi).unwrap(),
            ALUSrc::SignExtImm => get_val_from_pipe_reg!($sim.id_ex_reg, SignExtImm).unwrap(),
            ALUSrc::ZeroExtImm => {
                get_val_from_pipe_reg!($sim.id_ex_reg, SignExtImm).unwrap() & 0x0000_FFFF
            }
            ALUSrc::PcPlus4 => get_val_from_pipe_reg!($sim.id_ex_reg, PcPlus4).unwrap(),
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
            let pc = get_val_from_pipe_reg!(self.pc, PC).unwrap();
            let instr = self.memory.read(pc);
            insert_pipe_value![self.if_id_reg, Instruction, instr];
            insert_pipe_value![self.if_id_reg, InstructionPc, pc];
            insert_pipe_value![self.if_id_reg, PcPlus4, pc + 4];

            let is_branch = get_val_from_pipe_reg!(self.ex_mem_reg, IsBranch).unwrap();
            let is_jump = get_val_from_pipe_reg!(self.ex_mem_reg, IsJump).unwrap();
            let alu_res_zero = get_val_from_pipe_reg!(self.ex_mem_reg, ALURes).unwrap() == 0;

            if (is_branch || is_jump) && alu_res_zero {
                let jump_target = get_val_from_pipe_reg!(self.ex_mem_reg, JumpTarget).unwrap();
                insert_pipe_value![self.pc, PC, jump_target];
            } else {
                insert_pipe_value![self.pc, PC, pc + 4];
            }
        }
    }

    fn decode_stage(&mut self, stall: bool, squash: bool) {
        if squash {
            // TODO
        } else if stall {
            // TODO
        } else {
            let instr = get_val_from_pipe_reg!(self.if_id_reg, Instruction).unwrap();
            pass_through_val!(self.if_id_reg, self.id_ex_reg, InstructionPc);
            pass_through_val!(self.if_id_reg, self.id_ex_reg, PcPlus4);
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

                    match self.controller.get_state(PipeFieldName::IsJump).unwrap() {
                        PipeField::IsJump(is_jump) => {
                            if is_jump {
                                // Set squash decode to true
                                insert_pipe_value!(self.stalling_unit, SquashFetch, true);
                                // Load jump target into PC
                                insert_pipe_value!(self.pc, PC, instruction.get_address().unwrap());
                            }
                        }
                        _ => unreachable!(),
                    }
                    // PcPlus4
                    pass_through_val!(self.if_id_reg, self.id_ex_reg, PcPlus4);
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

            let alu_operation = get_val_from_pipe_reg!(self.id_ex_reg, AluOp).unwrap();

            let alu_result = alu::calculate(alu_src_1, alu_src_2, alu_operation).unwrap();
            insert_pipe_value![self.ex_mem_reg, ALURes, alu_result];

            if get_val_from_pipe_reg!(self.id_ex_reg, MuldivReqValid).unwrap() {
                let muldiv_result = match alu_operation {
                    ALUOperation::MULT => alu::multiply(alu_src_1, alu_src_2, true),
                    ALUOperation::MULTU => alu::multiply(alu_src_1, alu_src_2, false),
                    ALUOperation::DIV => alu::divide(alu_src_1, alu_src_2, true),
                    ALUOperation::DIVU => alu::divide(alu_src_1, alu_src_2, false),
                    _ => unreachable!(),
                };
                insert_pipe_value![self.ex_mem_reg, MuldivRes, muldiv_result];
            }
            // Calculate branch target
            let sign_ext_imm = get_val_from_pipe_reg!(self.id_ex_reg, SignExtImm).unwrap();
            let offset = 0x0000_FFFF & sign_ext_imm;
            let branch_target =
                get_val_from_pipe_reg!(self.id_ex_reg, PcPlus4).unwrap() + (offset << 2);
            insert_pipe_value![self.ex_mem_reg, BranchTarget, branch_target];

            // Determine RegToWrite
            let dest = get_val_from_pipe_reg!(self.id_ex_reg, RegDest).unwrap();
            match dest {
                RegDest::Rd => {
                    let rd: u8 = get_val_from_pipe_reg!(self.id_ex_reg, Rd).unwrap();
                    insert_pipe_value!(self.ex_mem_reg, RegToWrite, rd);
                }
                RegDest::Rt => {
                    let rt: u8 = get_val_from_pipe_reg!(self.id_ex_reg, Rt).unwrap();
                    insert_pipe_value!(self.ex_mem_reg, RegToWrite, rt);
                }
                RegDest::Ra => {
                    insert_pipe_value![self.ex_mem_reg, RegToWrite, Register::RA as u8];
                }
                RegDest::MulDivHi => {
                    insert_pipe_value![self.ex_mem_reg, RegToWrite, Register::HI as u8];
                }
                RegDest::MulDivLo => {
                    insert_pipe_value![self.ex_mem_reg, RegToWrite, Register::LO as u8];
                }
                RegDest::XXX => {
                    insert_pipe_value![self.ex_mem_reg, RegToWrite, 0];
                }
            }

            let is_branch = get_val_from_pipe_reg!(self.id_ex_reg, IsBranch).unwrap();
            if is_branch && alu_result == 0 {
                // set squash execute and squash_decode
                insert_pipe_value!(self.stalling_unit, SquashExecute, true);
                insert_pipe_value!(self.stalling_unit, SquashDecode, true);
            }
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, Reg2);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, WriteReg);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, WriteMem);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, ReadMem);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, MemWidth);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, MemSigned);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, IsBranch);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, AluToReg);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, MuldivReqValid);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, Halt);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, IsNop);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, Instruction);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, InstructionPc);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, InDelaySlot);
        }
    }

    fn memory_stage(&mut self, stall: bool, squash: bool) {
        if squash {
        } else if stall {
        } else {
            let alu_res = get_val_from_pipe_reg!(self.ex_mem_reg, ALURes).unwrap();
            let write_mem = get_val_from_pipe_reg!(self.ex_mem_reg, WriteMem).unwrap();
            let read_mem = get_val_from_pipe_reg!(self.ex_mem_reg, ReadMem).unwrap();

            if read_mem {
                let mem_val = self.memory.read(alu_res);
                insert_pipe_value!(self.mem_wb_reg, MemData, mem_val);
            } else if write_mem {
                let reg_2_data = get_val_from_pipe_reg!(self.ex_mem_reg, Reg2).unwrap();
                // TODO: Deal with mem width here for lh, lb, lw, etc
                self.memory.load(alu_res, reg_2_data);
            }

            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, ALURes);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, MuldivRes);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, MuldivReqValid);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, MemData);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, MemWidth);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, RegToWrite);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, WriteReg);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, AluToReg);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, Halt);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, IsNop);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, InDelaySlot);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, Instruction);
            pass_through_val!(self.ex_mem_reg, self.mem_wb_reg, InstructionPc);
        }
    }

    fn writeback_stage(&mut self, stall: bool, squash: bool) {
        if squash {
        } else if stall {
        } else {
            let alu_to_reg = get_val_from_pipe_reg!(self.mem_wb_reg, AluToReg).unwrap();
            let reg_write = get_val_from_pipe_reg!(self.mem_wb_reg, WriteReg).unwrap();
            let reg_target = get_val_from_pipe_reg!(self.mem_wb_reg, RegToWrite).unwrap();
            let reg_target = Register::try_from(reg_target).unwrap();

            if reg_write {
                if alu_to_reg {
                    let alu_res = get_val_from_pipe_reg!(self.mem_wb_reg, ALURes).unwrap();
                    self.reg_file.load(reg_target, alu_res);
                } else {
                    let mut mem_data = get_val_from_pipe_reg!(self.mem_wb_reg, MemData).unwrap();
                    let mem_width = get_val_from_pipe_reg!(self.mem_wb_reg, MemWidth).unwrap();

                    match mem_width {
                        1 => {
                            mem_data &= 0x0000_00FF;
                        }
                        2 => {
                            mem_data &= 0x0000_FFFF;
                        }
                        4 => {}
                        _ => unreachable!(),
                    }
                    self.reg_file.load(reg_target, mem_data);
                }

                self.stalling_unit.clear_write_in_flight(reg_target);
            }
        }
    }

    fn _step(&mut self) {
        // get signals from stalling unit
        let stall_fetch = get_val_from_pipe_reg!(self.stalling_unit, StallFetch).unwrap();
        let stall_decode = get_val_from_pipe_reg!(self.stalling_unit, StallDecode).unwrap();
        let stall_execute = get_val_from_pipe_reg!(self.stalling_unit, StallExecute).unwrap();
        let stall_memory = get_val_from_pipe_reg!(self.stalling_unit, StallMemory).unwrap();
        let stall_writeback = get_val_from_pipe_reg!(self.stalling_unit, StallWriteback).unwrap();

        let squash_fetch = get_val_from_pipe_reg!(self.stalling_unit, SquashFetch).unwrap();
        let squash_decode = get_val_from_pipe_reg!(self.stalling_unit, SquashDecode).unwrap();
        let squash_execute = get_val_from_pipe_reg!(self.stalling_unit, SquashExecute).unwrap();
        let squash_memory = get_val_from_pipe_reg!(self.stalling_unit, SquashMemory).unwrap();
        let squash_writeback = get_val_from_pipe_reg!(self.stalling_unit, SquashWriteback).unwrap();

        self.fetch_stage(stall_fetch, squash_fetch);
        self.decode_stage(stall_decode, squash_decode);
        self.execute_stage(stall_execute, squash_execute);
        self.memory_stage(stall_memory, squash_memory);
        self.writeback_stage(stall_writeback, squash_writeback);

        self.if_id_reg.clock();
        self.id_ex_reg.clock();
        self.ex_mem_reg.clock();
        self.mem_wb_reg.clock();

        self.stalling_unit.clock();

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
