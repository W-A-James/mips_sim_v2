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

#[derive(Debug, Clone)]
pub struct SimState {
    pub stalling_unit: stalling::StallingUnit,
    pub reg_file: reg_file::RegFile,
    pub if_id_reg: pipe_reg::PipeRegister,
    pub id_ex_reg: pipe_reg::PipeRegister,
    pub ex_mem_reg: pipe_reg::PipeRegister,
    pub mem_wb_reg: pipe_reg::PipeRegister,
    pub pc: pipe_reg::PipeRegister,
    pub halt: pipe_reg::PipeRegister,

    pub status_reg: pipe_reg::PipeRegister,
    pub cause_reg: pipe_reg::PipeRegister,
    pub epc_reg: pipe_reg::PipeRegister,
    pub bad_v_addr: pipe_reg::PipeRegister,
    pub controller: controller::Controller,
    pub memory: mem::Memory,
}

// -------- Public API ---------
impl Sim {
    pub fn new() -> Sim {
        let if_id_reg: pipe_reg::PipeRegister;
        let id_ex_reg: pipe_reg::PipeRegister;
        let ex_mem_reg: pipe_reg::PipeRegister;
        let mem_wb_reg: pipe_reg::PipeRegister;
        let mut pc: pipe_reg::PipeRegister;
        let status_reg: pipe_reg::PipeRegister;
        let epc_reg: pipe_reg::PipeRegister;
        let bad_v_addr: pipe_reg::PipeRegister;
        let cause_reg: pipe_reg::PipeRegister;
        let reg_file = reg_file::RegFile::new();
        let controller = controller::Controller::new();
        let memory = mem::Memory::new();
        let mut stalling_unit = stalling::StallingUnit::new();
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
                    PipeFieldName::BranchTarget,
                    PipeFieldName::BranchTaken,
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
        for v in data {
            self.memory.load(mem_index, *v);
            mem_index += 4;
        }

        self.memory.clock();
    }

    pub fn get_state(&self) -> SimState {
        SimState {
            stalling_unit: self.stalling_unit.clone(),
            reg_file: self.reg_file.clone(),
            if_id_reg: self.if_id_reg.clone(),
            id_ex_reg: self.id_ex_reg.clone(),
            ex_mem_reg: self.ex_mem_reg.clone(),
            mem_wb_reg: self.mem_wb_reg.clone(),
            pc: self.pc.clone(),
            halt: self.pc.clone(),

            status_reg: self.status_reg.clone(),
            cause_reg: self.cause_reg.clone(),
            epc_reg: self.epc_reg.clone(),
            bad_v_addr: self.bad_v_addr.clone(),
            controller: self.controller.clone(),
            memory: self.memory.clone(),
        }
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

// FIXME
macro_rules! insert_bubble {
    ($sim: expr, FETCH) => {{
        insert_pipe_value![$sim.if_id_reg, PcPlus4, 0];
        insert_pipe_value![$sim.if_id_reg, Instruction, 0];
        insert_pipe_value![$sim.if_id_reg, InstructionPc, 0];
    }};
    ($sim: expr, DECODE) => {{}};
    ($sim: expr, EXECUTE) => {{}};
    ($sim: expr, MEMORY) => {{}};
    ($sim: expr, WRITEBACK) => {{}};
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

    fn load_pc(&mut self, is_branch: bool, is_jump: bool, branch_taken: bool) {
        let current_pc = get_val_from_pipe_reg!(self.pc, PC).unwrap();
        if is_branch && branch_taken {
            let branch_target = get_val_from_pipe_reg!(self.id_ex_reg, BranchTarget).unwrap();
            insert_pipe_value![self.pc, PC, branch_target];
        } else if is_jump {
            let jump_target = get_val_from_pipe_reg!(self.id_ex_reg, JumpTarget).unwrap();
            insert_pipe_value![self.pc, PC, jump_target];
        } else {
            insert_pipe_value![self.pc, PC, current_pc + 4];
        }
    }
    fn fetch_stage(&mut self, stall: bool, squash: bool) {
        // Check if this stage is being squashed
        if squash {
            let is_branch = get_val_from_pipe_reg!(self.id_ex_reg, IsBranch).unwrap();
            let branch_taken = get_val_from_pipe_reg!(self.id_ex_reg, BranchTaken).unwrap();
            let is_jump = get_val_from_pipe_reg!(self.id_ex_reg, IsJump).unwrap();
            self.load_pc(is_branch, is_jump, branch_taken);
            insert_pipe_value!(self.stalling_unit, SquashFetch, false);
            insert_bubble!(self, FETCH);
            // don't update state and send a bubble
        } else if stall {
            // Don't load anything from pc and don't update pc
            // Don't send any new values
        } else {
            let pc = get_val_from_pipe_reg!(self.pc, PC).unwrap();
            let instr = self.memory.read(pc);
            insert_pipe_value![self.if_id_reg, Instruction, instr];
            insert_pipe_value![self.if_id_reg, InstructionPc, pc];
            insert_pipe_value![self.if_id_reg, PcPlus4, pc + 4];

            // FIXME: Check that this is where this needs to be happening
            let is_branch = get_val_from_pipe_reg!(self.id_ex_reg, IsBranch).unwrap();
            let branch_taken = get_val_from_pipe_reg!(self.id_ex_reg, BranchTaken).unwrap();
            let is_jump = get_val_from_pipe_reg!(self.id_ex_reg, IsJump).unwrap();
            self.load_pc(is_branch, is_jump, branch_taken);
        }
    }

    fn decode_stage(&mut self, stall: bool, squash: bool) {
        if squash {
            insert_bubble!(self, DECODE);
            self.controller
                .update_state(&instruction::Instruction::new(0).unwrap());
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
                    let mut r1 = Register::ZERO;
                    let mut r2 = Register::ZERO;
                    // Reg1
                    let reg_1_val = match self.controller.get_state(PipeFieldName::Reg1Src).unwrap()
                    {
                        PipeField::Reg1Src(RegSrc::Rt) => {
                            if let Some(rt) = instruction.get_rt() {
                                r1 = rt;
                                self.reg_file.read(rt)
                            } else {
                                0
                            }
                        }
                        PipeField::Reg1Src(RegSrc::Rs) => {
                            if let Some(rs) = instruction.get_rs() {
                                r1 = rs;
                                self.reg_file.read(rs)
                            } else {
                                0
                            }
                        }
                        _ => {
                            r1 = Register::ZERO;
                            0
                        }
                    };
                    // Reg2
                    let reg_2_val = match self.controller.get_state(PipeFieldName::Reg2Src).unwrap()
                    {
                        PipeField::Reg2Src(RegSrc::Rt) => {
                            if let Some(r) = instruction.get_rt() {
                                // if write_reg:
                                //      set registers to have pending write
                                // Check for stalling conditions
                                // Update the id_ex register
                                r2 = r;
                                self.reg_file.read(r)
                            } else {
                                0
                            }
                        }
                        PipeField::Reg2Src(RegSrc::Rs) => {
                            if let Some(r) = instruction.get_rs() {
                                r2 = r;
                                self.reg_file.read(r)
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    };

                    let start_stalling = self.stalling_unit.check_write_in_flight(r1)
                        | self.stalling_unit.check_write_in_flight(r2);
                    insert_pipe_value!(self.stalling_unit, StallFetch, start_stalling);

                    if let PipeField::WriteReg(write_reg) =
                        self.controller.get_state(PipeFieldName::WriteReg).unwrap()
                    {
                        if !start_stalling && write_reg {
                            let reg_dest =
                                self.controller.get_state(PipeFieldName::RegDest).unwrap();
                            if let PipeField::RegDest(reg_d) = reg_dest {
                                match reg_d {
                                    RegDest::Rt => {
                                        let reg = instruction.get_rt().unwrap();
                                        self.stalling_unit.start_write_in_flight(reg);
                                    }
                                    RegDest::Rd => {
                                        let reg = instruction.get_rd().unwrap();
                                        self.stalling_unit.start_write_in_flight(reg);
                                    }
                                    RegDest::Ra => {
                                        self.stalling_unit.start_write_in_flight(Register::RA);
                                    }
                                    RegDest::MulDivHi => {
                                        self.stalling_unit.start_write_in_flight(Register::HI);
                                    }
                                    RegDest::MulDivLo => {
                                        self.stalling_unit.start_write_in_flight(Register::LO);
                                    }
                                    RegDest::XXX => {}
                                }
                            }
                        }
                    };

                    let branch_compare: i8 = if reg_1_val < reg_2_val {
                        -1
                    } else if reg_1_val > reg_2_val {
                        1
                    } else {
                        0
                    };
                    // Resolve branch conditions here
                    match self.controller.get_state(PipeFieldName::IsBranch).unwrap() {
                        PipeField::IsBranch(true) => {
                            match self
                                .controller
                                .get_state(PipeFieldName::BranchType)
                                .unwrap()
                            {
                                PipeField::BranchType(BranchType::Beq) => {
                                    insert_pipe_value!(
                                        self.id_ex_reg,
                                        BranchTaken,
                                        branch_compare == 0
                                    );
                                }
                                PipeField::BranchType(BranchType::Bne) => {
                                    insert_pipe_value!(
                                        self.id_ex_reg,
                                        BranchTaken,
                                        branch_compare != 0
                                    );
                                }
                                PipeField::BranchType(BranchType::Bgez)
                                | PipeField::BranchType(BranchType::Bgezal) => {
                                    insert_pipe_value!(
                                        self.id_ex_reg,
                                        BranchTaken,
                                        (reg_1_val as i32) >= 0
                                    );
                                }
                                PipeField::BranchType(BranchType::Bgtz) => {
                                    insert_pipe_value!(
                                        self.id_ex_reg,
                                        BranchTaken,
                                        reg_1_val as i32 > 0
                                    );
                                }
                                PipeField::BranchType(BranchType::Blez) => {
                                    insert_pipe_value!(
                                        self.id_ex_reg,
                                        BranchTaken,
                                        reg_1_val as i32 <= 0
                                    );
                                }
                                PipeField::BranchType(BranchType::Bltzal)
                                | PipeField::BranchType(BranchType::Bltz) => {
                                    insert_pipe_value!(
                                        self.id_ex_reg,
                                        BranchTaken,
                                        (reg_1_val as i32) < 0
                                    );
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                    // Calculate branch target
                    let sign_ext_imm = match instruction.get_imm() {
                        Some(shamt) => Sim::sign_ext_imm(shamt),
                        None => 0,
                    };
                    let offset = sign_ext_imm << 2;
                    let branch_target =
                        get_val_from_pipe_reg!(self.id_ex_reg, PcPlus4).unwrap() + offset;
                    insert_pipe_value![self.id_ex_reg, BranchTarget, branch_target];

                    match self.controller.get_state(PipeFieldName::IsJump).unwrap() {
                        PipeField::IsJump(is_jump) => {
                            insert_pipe_value!(self.id_ex_reg, IsJump, is_jump);
                            if is_jump {
                                // Set squash decode to true
                                insert_pipe_value!(self.stalling_unit, SquashFetch, true);
                                insert_pipe_value!(
                                    self.id_ex_reg,
                                    JumpTarget,
                                    instruction.get_address().unwrap()
                                );
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

            // if write_mem | read_mem:
            //      check if source registers have a pending write
            //
            //

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
            insert_bubble!(self, EXECUTE);
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

            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, Reg2);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, WriteReg);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, WriteMem);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, ReadMem);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, MemWidth);
            pass_through_val!(self.id_ex_reg, self.ex_mem_reg, MemSigned);
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
            insert_bubble!(self, MEMORY);
        } else if stall {
        } else {
            let alu_res = get_val_from_pipe_reg!(self.ex_mem_reg, ALURes).unwrap();
            let write_mem = get_val_from_pipe_reg!(self.ex_mem_reg, WriteMem).unwrap();
            let read_mem = get_val_from_pipe_reg!(self.ex_mem_reg, ReadMem).unwrap();
            let mem_width = get_val_from_pipe_reg!(self.ex_mem_reg, MemWidth).unwrap();

            if read_mem {
                let mem_val = self.memory.read(alu_res);
                insert_pipe_value!(self.mem_wb_reg, MemData, mem_val);
            } else if write_mem {
                let reg_2_data = get_val_from_pipe_reg!(self.ex_mem_reg, Reg2).unwrap();

                let rem = alu_res % 4;
                match rem {
                    0 => {
                        let mut input_mask = 0u32;
                        let mut output_mask = 0u32;
                        for _ in 0..mem_width {
                            input_mask = (input_mask << 8) | 0xFF;
                            output_mask = (output_mask | 0xFF00_0000) >> 8;
                        }

                        let lower_bytes = (input_mask & reg_2_data) << ((4 - mem_width) * 8);
                        let current_mem_val = self.memory.read(alu_res);
                        let output_val = (current_mem_val & output_mask) | lower_bytes;
                        self.memory.load(alu_res, output_val);
                    }
                    rem => {
                        if mem_width as u32 > rem {
                            let left_mem_val = self.memory.read(alu_res - rem);
                            let right_mem_val = self.memory.read(alu_res - rem + 4);

                            let mut left_mask = 0;
                            let mut right_mask = 0;

                            for _ in 0..rem {
                                left_mask = (left_mask | 0xFF) << 8;
                                right_mask = (right_mask | 0xFF00_0000) >> 8;
                            }

                            let lower_bytes = (reg_2_data << ((4 - rem) * 8)) & right_mask;
                            let upper_bytes = (reg_2_data >> (rem * 8)) & left_mask;

                            let left_val_to_write = (left_mem_val & !left_mask) | upper_bytes;
                            let right_val_to_write = (right_mem_val & !right_mask) | lower_bytes;

                            self.memory.load(alu_res - rem, left_val_to_write);
                            self.memory.load(alu_res - rem + 4, right_val_to_write);
                        } else {
                            let current_mem_val = self.memory.read(alu_res - rem);
                            let mut mask = 0;
                            for _ in 0..mem_width {
                                mask = (mask | 0xFF) << 8;
                            }
                            let val = reg_2_data << ((4 - rem) * 8);
                            mask = mask << ((4 - rem) * 8);

                            let output_val = (current_mem_val & !mask) | (val & mask);
                            self.memory.load(alu_res - rem, output_val);
                        }
                    }
                }
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
            insert_bubble!(self, WRITEBACK);
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
                insert_pipe_value!(self.stalling_unit, StallFetch, false);
                insert_pipe_value!(self.stalling_unit, StallDecode, false);
            }
        }
    }

    fn _step(&mut self) {
        // get signals from stalling unit
        // Stalling conditions
        // 1a. EX/MEM.RegisterRd = ID/EX.RegisterRs
        // 1b. EX/MEM.RegisterRd = ID/EX.RegisterRt
        // 2a. MEM/WB.RegisterRd = ID/EX.RegisterRs
        // 2b. MEM/WB.RegisterRd = ID/EX.RegisterRt
        let stall_fetch = get_val_from_pipe_reg!(self.stalling_unit, StallFetch).unwrap();
        let stall_decode = get_val_from_pipe_reg!(self.stalling_unit, StallDecode).unwrap();

        let squash_fetch = get_val_from_pipe_reg!(self.stalling_unit, SquashFetch).unwrap();
        let squash_decode = get_val_from_pipe_reg!(self.stalling_unit, SquashDecode).unwrap();

        self.fetch_stage(stall_fetch, squash_fetch);
        self.decode_stage(stall_decode, squash_decode);
        self.execute_stage(false, false);
        self.memory_stage(false, false);
        self.writeback_stage(false, false);

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
        self.halt.load(PipeFieldName::Halt, PipeField::Halt(false));
        self.reg_file.clock();
        self.pc.clock();

        self.halt.clock();
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
