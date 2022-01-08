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
                PipeField::Bool(halt) => {
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

macro_rules! insert_bubble {
    ($sim: expr, FETCH) => {{
        $sim.if_id_reg
            .load(PipeFieldName::PcPlus4, PipeField::UInt(0));
        $sim.if_id_reg
            .load(PipeFieldName::Instruction, PipeField::UInt(0));
        $sim.if_id_reg
            .load(PipeFieldName::InstructionPc, PipeField::UInt(0));
    }};
    ($sim: expr, DECODE) => {{
        $sim.id_ex_reg
            .load(PipeFieldName::WriteReg, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::WriteMem, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::ReadMem, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::IsNop, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::Instruction, PipeField::UInt(0));
        $sim.id_ex_reg
            .load(PipeFieldName::AluOp, PipeField::Op(ALUOperation::ADD));
        $sim.id_ex_reg
            .load(PipeFieldName::AluSrc1, PipeField::ALU(ALUSrc::Zero));
        $sim.id_ex_reg
            .load(PipeFieldName::AluSrc2, PipeField::ALU(ALUSrc::Zero));
        $sim.id_ex_reg
            .load(PipeFieldName::RegDest, PipeField::Dest(RegDest::XXX));
    }};
    ($sim: expr, EXECUTE) => {{
        $sim.ex_mem_reg
            .load(PipeFieldName::WriteReg, PipeField::Bool(false));
        $sim.ex_mem_reg
            .load(PipeFieldName::WriteMem, PipeField::Bool(false));
        $sim.ex_mem_reg
            .load(PipeFieldName::ReadMem, PipeField::Bool(false));
        $sim.ex_mem_reg
            .load(PipeFieldName::MuldivReqValid, PipeField::Bool(false));
    }};
    ($sim: expr, MEMORY) => {{}};
    ($sim: expr, WRITEBACK) => {{}};
}

macro_rules! get_alu_src_val {
    ($sim: expr, $alu_src: expr, $field_name: ident) => {{
        let src = $sim.id_ex_reg.read(PipeFieldName::$field_name);
        $alu_src = match src {
            PipeField::ALU(ALUSrc::Shamt) => match $sim.id_ex_reg.read(PipeFieldName::Shamt) {
                PipeField::Byte(shamt) => shamt as u32,
                _ => panic!(),
            },
            PipeField::ALU(ALUSrc::Zero) => 0,
            PipeField::ALU(ALUSrc::Reg1) => match $sim.id_ex_reg.read(PipeFieldName::Reg1) {
                PipeField::UInt(r) => r,
                _ => panic!(),
            },
            PipeField::ALU(ALUSrc::Reg2) => match $sim.id_ex_reg.read(PipeFieldName::Reg2) {
                PipeField::UInt(r) => r,
                _ => panic!(),
            },
            PipeField::ALU(ALUSrc::Muldivlo) => {
                match $sim.id_ex_reg.read(PipeFieldName::Muldivlo) {
                    PipeField::UInt(m) => m,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::Muldivhi) => {
                match $sim.id_ex_reg.read(PipeFieldName::Muldivhi) {
                    PipeField::UInt(m) => m,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::SignExtImm) => {
                match $sim.id_ex_reg.read(PipeFieldName::SignExtImm) {
                    PipeField::UInt(i) => i,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::ZeroExtImm) => {
                match $sim.id_ex_reg.read(PipeFieldName::SignExtImm) {
                    PipeField::UInt(i) => i & 0x0000_FFFF,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::PcPlus4) => match $sim.id_ex_reg.read(PipeFieldName::PcPlus4) {
                PipeField::UInt(pc_4) => pc_4,
                _ => panic!(),
            },
            _ => panic!(),
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

    fn load_pc(&mut self, is_branch: bool, is_jump: bool, branch_taken: bool) {
        if is_branch && branch_taken {
            eprintln!(
                "Loading pc with branch target: {:#?}",
                self.id_ex_reg.read(PipeFieldName::BranchTarget)
            );
            self.pc.load(
                PipeFieldName::PC,
                self.id_ex_reg.read(PipeFieldName::BranchTarget),
            );
        } else if is_jump {
            eprintln!(
                "Loading pc with jump target: {:#?}",
                self.id_ex_reg.read(PipeFieldName::JumpTarget)
            );
            self.pc.load(
                PipeFieldName::PC,
                self.id_ex_reg.read(PipeFieldName::JumpTarget),
            );
        } else {
            match self.pc.read(PipeFieldName::PC) {
                PipeField::UInt(pc) => {
                    self.pc.load(PipeFieldName::PC, PipeField::UInt(pc + 4));
                    eprintln!("Loading pc with pc + 4: {:#?}", pc + 4);
                }
                _ => panic!("Invalid value in pc"),
            }
        }
    }

    fn fetch_stage(&mut self, stall: bool, squash: bool) {
        let is_branch = match self.id_ex_reg.read(PipeFieldName::IsBranch) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };

        let branch_taken = match self.id_ex_reg.read(PipeFieldName::BranchTaken) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };

        let is_jump = match self.id_ex_reg.read(PipeFieldName::IsJump) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };

        let pc = match self.pc.read(PipeFieldName::PC) {
            PipeField::UInt(pc_) => pc_,
            _ => panic!(),
        };

        // Check if this stage is being squashed
        if squash {
            self.load_pc(is_branch, is_jump, branch_taken);
            self.stalling_unit
                .load(PipeFieldName::SquashFetch, PipeField::Bool(false));
            insert_bubble!(self, FETCH);
            // don't update state and send a bubble
        } else if stall {
            // Don't load anything from pc and don't update pc
            // Don't send any new values
        } else {
            let instr = self.memory.read(pc);

            self.if_id_reg
                .load(PipeFieldName::Instruction, PipeField::UInt(instr));
            self.if_id_reg
                .load(PipeFieldName::InstructionPc, PipeField::UInt(pc));
            self.if_id_reg
                .load(PipeFieldName::PcPlus4, PipeField::UInt(pc + 4));

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
            let instr = match self.if_id_reg.read(PipeFieldName::Instruction) {
                PipeField::UInt(i) => i,
                _ => panic!(),
            };
            self.if_id_reg
                .pass_through(&mut self.id_ex_reg, PipeFieldName::InstructionPc);
            self.if_id_reg
                .pass_through(&mut self.id_ex_reg, PipeFieldName::PcPlus4);

            let parsed_instr = instruction::Instruction::new(instr);
            match parsed_instr {
                Ok(instruction) => {
                    self.controller.update_state(&instruction);
                    let mut r1 = Register::ZERO;
                    let mut r2 = Register::ZERO;
                    // Reg1
                    let reg_1_val = match self.controller.get_state(PipeFieldName::Reg1Src).unwrap()
                    {
                        PipeField::RSrc(RegSrc::Rt) => {
                            if let Some(rt) = instruction.get_rt() {
                                r1 = rt;
                                self.reg_file.read(rt)
                            } else {
                                0
                            }
                        }
                        PipeField::RSrc(RegSrc::Rs) => {
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
                        PipeField::RSrc(RegSrc::Rt) => {
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
                        PipeField::RSrc(RegSrc::Rs) => {
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
                    self.stalling_unit
                        .load(PipeFieldName::StallFetch, PipeField::Bool(start_stalling));

                    match self.controller.get_state(PipeFieldName::WriteReg).unwrap() {
                        PipeField::Bool(write_reg) => {
                            if !start_stalling && write_reg {
                                let reg_dest =
                                    self.controller.get_state(PipeFieldName::RegDest).unwrap();
                                match reg_dest {
                                    PipeField::Dest(r) => match r {
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
                                    },
                                    _ => panic!(),
                                }
                            }
                        }
                        _ => panic!(),
                    }

                    let branch_compare: i8 = if reg_1_val < reg_2_val {
                        -1
                    } else if reg_1_val > reg_2_val {
                        1
                    } else {
                        0
                    };
                    // Resolve branch conditions here
                    match self.controller.get_state(PipeFieldName::IsBranch).unwrap() {
                        PipeField::Bool(true) => {
                            match self
                                .controller
                                .get_state(PipeFieldName::BranchType)
                                .unwrap()
                            {
                                PipeField::Branch(BranchType::Beq) => {
                                    self.id_ex_reg.load(
                                        PipeFieldName::BranchTaken,
                                        PipeField::Bool(branch_compare == 0),
                                    );
                                }
                                PipeField::Branch(BranchType::Bne) => {
                                    self.id_ex_reg.load(
                                        PipeFieldName::BranchTaken,
                                        PipeField::Bool(branch_compare != 0),
                                    );
                                }
                                PipeField::Branch(BranchType::Bgez)
                                | PipeField::Branch(BranchType::Bgezal) => {
                                    self.id_ex_reg.load(
                                        PipeFieldName::BranchTaken,
                                        PipeField::Bool((reg_1_val as i32) >= 0),
                                    );
                                }
                                PipeField::Branch(BranchType::Bgtz) => {
                                    self.id_ex_reg.load(
                                        PipeFieldName::BranchTaken,
                                        PipeField::Bool((reg_1_val as i32) > 0),
                                    );
                                }
                                PipeField::Branch(BranchType::Blez) => {
                                    self.id_ex_reg.load(
                                        PipeFieldName::BranchTaken,
                                        PipeField::Bool((reg_1_val as i32) <= 0),
                                    );
                                }
                                PipeField::Branch(BranchType::Bltzal)
                                | PipeField::Branch(BranchType::Bltz) => {
                                    self.id_ex_reg.load(
                                        PipeFieldName::BranchTaken,
                                        PipeField::Bool((reg_1_val as i32) < 0),
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

                    let pc_plus_4 = match self.id_ex_reg.read(PipeFieldName::PcPlus4) {
                        PipeField::UInt(v) => v,
                        _ => panic!(),
                    };
                    // TODO: Ensure that this is a signed operation
                    let branch_target = pc_plus_4 + offset;
                    self.id_ex_reg
                        .load(PipeFieldName::BranchTarget, PipeField::UInt(branch_target));

                    match self.controller.get_state(PipeFieldName::IsJump).unwrap() {
                        PipeField::Bool(is_jump) => {
                            self.id_ex_reg
                                .load(PipeFieldName::IsJump, PipeField::Bool(is_jump));
                            if is_jump {
                                // Set squash decode to true
                                self.stalling_unit
                                    .load(PipeFieldName::SquashFetch, PipeField::Bool(true));
                                self.id_ex_reg.load(
                                    PipeFieldName::JumpTarget,
                                    PipeField::UInt(instruction.get_address().unwrap()),
                                );
                            }
                        }
                        _ => unreachable!(),
                    }
                    // PcPlus4
                    self.if_id_reg
                        .pass_through(&mut self.id_ex_reg, PipeFieldName::PcPlus4);
                    // SignExtImm
                    self.id_ex_reg.load(
                        PipeFieldName::SignExtImm,
                        PipeField::UInt(match instruction.get_imm() {
                            Some(shamt) => Sim::sign_ext_imm(shamt),
                            None => 0,
                        }),
                    );
                    // Rt
                    self.id_ex_reg.load(
                        PipeFieldName::Rt,
                        PipeField::Byte(match instruction.get_rt() {
                            Some(reg) => reg as u8,
                            None => 0,
                        }),
                    );
                    // Rd
                    self.id_ex_reg.load(
                        PipeFieldName::Rd,
                        PipeField::Byte(match instruction.get_rd() {
                            Some(reg) => reg as u8,
                            None => 0,
                        }),
                    );
                    // Shamt
                    self.id_ex_reg.load(
                        PipeFieldName::Shamt,
                        PipeField::Byte(match instruction.get_shamt() {
                            Some(shamt) => shamt,
                            None => 0,
                        }),
                    );
                    // IsNop
                    self.id_ex_reg
                        .load(PipeFieldName::IsNop, PipeField::Bool(instruction.is_nop()));
                    // Halt
                    self.id_ex_reg
                        .load(PipeFieldName::Halt, PipeField::Bool(instruction.is_halt()));
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

            let alu_operation = match self.id_ex_reg.read(PipeFieldName::AluOp) {
                PipeField::Op(op) => op,
                _ => panic!(),
            };

            let alu_result = alu::calculate(alu_src_1, alu_src_2, alu_operation).unwrap();
            self.ex_mem_reg
                .load(PipeFieldName::ALURes, PipeField::UInt(alu_result));

            let muldiv_req_valid = match self.id_ex_reg.read(PipeFieldName::MuldivReqValid) {
                PipeField::Bool(v) => v,
                _ => panic!(),
            };

            if muldiv_req_valid {
                let muldiv_result = match alu_operation {
                    ALUOperation::MULT => alu::multiply(alu_src_1, alu_src_2, true),
                    ALUOperation::MULTU => alu::multiply(alu_src_1, alu_src_2, false),
                    ALUOperation::DIV => alu::divide(alu_src_1, alu_src_2, true),
                    ALUOperation::DIVU => alu::divide(alu_src_1, alu_src_2, false),
                    _ => unreachable!(),
                };

                self.ex_mem_reg
                    .load(PipeFieldName::MuldivRes, PipeField::U64(muldiv_result));
            }

            // Determine RegToWrite
            let dest = match self.id_ex_reg.read(PipeFieldName::RegDest) {
                PipeField::Dest(d) => d,
                _ => panic!(),
            };

            match dest {
                RegDest::Rd => {
                    let rd: u8 = match self.id_ex_reg.read(PipeFieldName::Rd) {
                        PipeField::Byte(r) => r,
                        _ => panic!(),
                    };
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::Byte(rd));
                }
                RegDest::Rt => {
                    let rt: u8 = match self.id_ex_reg.read(PipeFieldName::Rt) {
                        PipeField::Byte(r) => r,
                        _ => panic!(),
                    };
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::Byte(rt));
                }
                RegDest::Ra => {
                    self.ex_mem_reg.load(
                        PipeFieldName::RegToWrite,
                        PipeField::Byte(Register::RA as u8),
                    );
                }
                RegDest::MulDivHi => {
                    self.ex_mem_reg.load(
                        PipeFieldName::RegToWrite,
                        PipeField::Byte(Register::HI as u8),
                    );
                }
                RegDest::MulDivLo => {
                    self.ex_mem_reg.load(
                        PipeFieldName::RegToWrite,
                        PipeField::Byte(Register::LO as u8),
                    );
                }
                RegDest::XXX => {
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::Byte(0u8));
                }
            }

            self.ex_mem_reg.load(
                PipeFieldName::Reg2,
                self.id_ex_reg.read(PipeFieldName::Reg2),
            );
            self.ex_mem_reg.load(
                PipeFieldName::WriteReg,
                self.id_ex_reg.read(PipeFieldName::WriteReg),
            );
            self.ex_mem_reg.load(
                PipeFieldName::WriteMem,
                self.id_ex_reg.read(PipeFieldName::WriteMem),
            );
            self.ex_mem_reg.load(
                PipeFieldName::ReadMem,
                self.id_ex_reg.read(PipeFieldName::ReadMem),
            );
            self.ex_mem_reg.load(
                PipeFieldName::MemWidth,
                self.id_ex_reg.read(PipeFieldName::MemWidth),
            );
            self.ex_mem_reg.load(
                PipeFieldName::MemSigned,
                self.id_ex_reg.read(PipeFieldName::MemSigned),
            );
            self.ex_mem_reg.load(
                PipeFieldName::AluToReg,
                self.id_ex_reg.read(PipeFieldName::AluToReg),
            );
            self.ex_mem_reg.load(
                PipeFieldName::MuldivReqValid,
                self.id_ex_reg.read(PipeFieldName::MuldivReqValid),
            );
            self.ex_mem_reg.load(
                PipeFieldName::Halt,
                self.id_ex_reg.read(PipeFieldName::Halt),
            );
            self.ex_mem_reg.load(
                PipeFieldName::IsNop,
                self.id_ex_reg.read(PipeFieldName::IsNop),
            );
            self.ex_mem_reg.load(
                PipeFieldName::Instruction,
                self.id_ex_reg.read(PipeFieldName::Instruction),
            );
            self.ex_mem_reg.load(
                PipeFieldName::InstructionPc,
                self.id_ex_reg.read(PipeFieldName::InstructionPc),
            );
            self.ex_mem_reg.load(
                PipeFieldName::InDelaySlot,
                self.id_ex_reg.read(PipeFieldName::InDelaySlot),
            );
        }
    }

    fn memory_stage(&mut self, stall: bool, squash: bool) {
        if squash {
            insert_bubble!(self, MEMORY);
        } else if stall {
        } else {
            let alu_res = match self.ex_mem_reg.read(PipeFieldName::ALURes) {
                PipeField::UInt(r) => r,
                _ => panic!(),
            };
            let write_mem = match self.ex_mem_reg.read(PipeFieldName::WriteMem) {
                PipeField::Bool(b) => b,
                _ => panic!(),
            };
            let read_mem = match self.ex_mem_reg.read(PipeFieldName::ReadMem) {
                PipeField::Bool(b) => b,
                _ => panic!(),
            };
            let mem_width = match self.ex_mem_reg.read(PipeFieldName::MemWidth) {
                PipeField::Byte(w) => w,
                _ => panic!(),
            };

            if read_mem {
                let mem_val = self.memory.read(alu_res);
                self.mem_wb_reg
                    .load(PipeFieldName::MemData, PipeField::UInt(mem_val));
            } else if write_mem {
                let reg_2_data = match self.ex_mem_reg.read(PipeFieldName::Reg2) {
                    PipeField::UInt(d) => d,
                    _ => panic!(),
                };

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

            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::ALURes);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::MuldivRes);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::MuldivReqValid);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::MemWidth);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::RegToWrite);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::WriteReg);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::AluToReg);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::Halt);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::IsNop);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::InDelaySlot);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::Instruction);
            self.ex_mem_reg
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::InstructionPc);
        }
    }

    fn writeback_stage(&mut self, stall: bool, squash: bool) {
        if squash {
            insert_bubble!(self, WRITEBACK);
        } else if stall {
        } else {
            let alu_to_reg = match self.mem_wb_reg.read(PipeFieldName::AluToReg) {
                PipeField::Bool(a) => a,
                _ => panic!(),
            };
            let reg_write = match self.mem_wb_reg.read(PipeFieldName::WriteReg) {
                PipeField::Bool(r) => r,
                _ => panic!(),
            };
            let reg_target = match self.mem_wb_reg.read(PipeFieldName::RegToWrite) {
                PipeField::Byte(t) => t,
                _ => panic!(),
            };

            let reg_target = Register::try_from(reg_target).unwrap();

            if reg_write {
                if alu_to_reg {
                    let alu_res = match self.mem_wb_reg.read(PipeFieldName::ALURes) {
                        PipeField::UInt(res) => res,
                        _ => panic!(),
                    };
                    self.reg_file.load(reg_target, alu_res);
                } else {
                    let mut mem_data = match self.mem_wb_reg.read(PipeFieldName::MemData) {
                        PipeField::UInt(m) => m,
                        _ => panic!(),
                    };
                    let mem_width = match self.mem_wb_reg.read(PipeFieldName::MemWidth) {
                        PipeField::Byte(w) => w,
                        _ => panic!(),
                    };
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
                self.stalling_unit
                    .load(PipeFieldName::StallFetch, PipeField::Bool(false));
                self.stalling_unit
                    .load(PipeFieldName::StallFetch, PipeField::Bool(false));
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
        let stall_fetch = match self.stalling_unit.read(PipeFieldName::StallFetch) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };
        let stall_decode = match self.stalling_unit.read(PipeFieldName::StallDecode) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };

        let squash_fetch = match self.stalling_unit.read(PipeFieldName::SquashFetch) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };
        let squash_decode = match self.stalling_unit.read(PipeFieldName::SquashDecode) {
            PipeField::Bool(b) => b,
            _ => panic!(),
        };

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

        self.reg_file.load(Register::SP, STACK_POINTER_INITIAL);
        self.reg_file.load(Register::FP, STACK_POINTER_INITIAL);
        self.pc.load(PipeFieldName::PC, PipeField::UInt(TEXT_START));
        self.halt.load(PipeFieldName::Halt, PipeField::Bool(false));
        self.reg_file.clock();
        self.pc.clock();
        self.stalling_unit.clock();
        self.status_reg.clock();
        self.epc_reg.clock();
        self.bad_v_addr.clock();
        self.memory.clock();

        self.halt.clock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use instruction::Instruction;
    use pipe_reg::PipeRegister;

    fn assert_pipe_fields(
        pipe_register: &PipeRegister,
        keys: &Vec<PipeFieldName>,
        values: &Vec<PipeField>,
    ) {
        assert!(keys.len() == values.len());

        for (k, v) in keys.iter().zip(values.iter()) {
            assert_eq!(
                pipe_register.read(*k),
                *v,
                "Failed equality check for key: {:#?}",
                k
            );
        }
    }

    fn test_instr(
        instr: Instruction,
        register_name: &str,
        num_cycles: u32,
        fields: &Vec<PipeFieldName>,
        values: &Vec<PipeField>,
    ) {
        let mut instrs: Vec<u32> = Vec::with_capacity(5);
        let mut sim = Sim::new();
        let data: Vec<u32> = Vec::new();
        instrs.push(instr.get_instr_word());

        for _ in 1..instrs.len() {
            instrs.push(0);
        }

        sim.load_binary(&instrs, &data);

        assert_eq!(fields.len(), values.len());

        sim.step(num_cycles);

        let register = match register_name {
            "IF/ID" => sim.get_state().if_id_reg.clone(),
            "ID/EX" => sim.get_state().id_ex_reg.clone(),
            "EX/MEM" => sim.get_state().ex_mem_reg.clone(),
            "MEM/WB" => sim.get_state().mem_wb_reg.clone(),
            "PC" => sim.get_state().pc.clone(),
            "HALT" => sim.get_state().halt.clone(),
            "EPC" => sim.get_state().epc_reg.clone(),
            "CAUSE" => sim.get_state().cause_reg.clone(),
            "BAD_V_ADDR" => sim.get_state().bad_v_addr.clone(),
            _ => panic!(),
        };

        assert_pipe_fields(&register, fields, values);
    }

    #[test]
    pub fn test_r_type_instrs() {}

    #[test]
    pub fn test_i_type_instrs() {}

    #[test]
    pub fn test_j_type_instrs() {}

    #[test]
    pub fn test_raw_dep() {}

    #[test]
    pub fn test_branches() {}

    #[test]
    pub fn test_fetch_stage() {
        // Add instruction
        let instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Add),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "IF/ID",
            1,
            &vec![PipeFieldName::PcPlus4, PipeFieldName::Instruction],
            &vec![
                PipeField::UInt(TEXT_START + 4),
                PipeField::UInt(instr.clone().get_instr_word()),
            ],
        );

        use PipeFieldName::*;
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
    }

    #[test]
    pub fn test_decode_stage() {
        use PipeFieldName::*;
        // ADD
        let mut instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Add),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // ADDU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Addu),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADDU),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // AND
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::And),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::AND),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // CLO
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Mul,
            Some(instruction::FuncCode::Add),
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::ZERO as u8),
                PipeField::Byte(common::Register::T1 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::CLO),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // CLZ
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Mul,
            Some(instruction::FuncCode::Addu),
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::ZERO as u8),
                PipeField::Byte(common::Register::T1 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::CLZ),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // DIV
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Div),
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(true),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::ZERO as u8),
                PipeField::Bool(false),
                PipeField::Op(ALUOperation::DIV),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // DIVU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Divu),
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(true),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::ZERO as u8),
                PipeField::Bool(false),
                PipeField::Op(ALUOperation::DIVU),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // MULT
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Mult),
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(true),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::ZERO as u8),
                PipeField::Bool(false),
                PipeField::Op(ALUOperation::MULT),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // MULTU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Multu),
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(true),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::ZERO as u8),
                PipeField::Bool(false),
                PipeField::Op(ALUOperation::MULTU),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // MUL
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Mul,
            Some(instruction::FuncCode::Srl),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();

        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T2 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::MUL),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // NOR
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Nor),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::NOR),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // OR
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Or),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::OR),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // ADDI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Addi,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(10),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(10),
                PipeField::Byte(8),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // ADDIU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Addiu,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(10),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(10),
                PipeField::Byte(8),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADDU),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // ANDI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Andi,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(10),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(10),
                PipeField::Byte(8),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::AND),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // ORI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Ori,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(10),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(10),
                PipeField::Byte(8),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::OR),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // XORI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Xori,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(10),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(10),
                PipeField::Byte(8),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::XOR),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
        // SLL
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Sll),
            Some(Register::T0),
            None,
            Some(Register::T1),
            Some(10),
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T1 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::SLL),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // SLLV
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Sllv),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T2 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::SLL),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // SRA
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Sra),
            Some(Register::T0),
            None,
            Some(Register::T1),
            Some(10),
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T1 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::SRA),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // SRAV
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Srav),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T2 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::SRA),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // SRL
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Srl),
            Some(Register::T0),
            None,
            Some(Register::T1),
            Some(10),
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T1 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::SRL),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // SRLV
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Srlv),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(common::Register::T0 as u8),
                PipeField::Byte(common::Register::T2 as u8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::SRL),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // SUB
        // SUBU
        // XOR
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Xor),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(8),
                PipeField::Byte(10),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::XOR),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // LUI
        // SLT
        // SLTU
        // SLTI
        // SLTIU
        // BEQ
        // BGEZ
        // BGEZAL
        // BGTZ
        // BLEZ
        // BLTZAL
        // BLTZ
        // BNE
        // J
        // JAL
        // JALR
        // JR
        // LB
        // LBU
        // LH
        // LHU
        // LW
        // LWL
        // LWR
        // SB
        // SH
        // SW
        // SWL
        // SWR
        // MFHI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Mfhi),
            None,
            None,
            Some(common::Register::T0),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(0),
                PipeField::Byte(8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // MFLO
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Mflo),
            None,
            None,
            Some(common::Register::T0),
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(0),
                PipeField::Byte(8),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::Rd),
            ],
        );
        // MTHI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Mthi),
            None,
            Some(common::Register::T0),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(0),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::MulDivHi),
            ],
        );
        // MTLO
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Mtlo),
            None,
            Some(common::Register::T0),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                AluToReg,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(0),
                PipeField::Byte(0),
                PipeField::Bool(true),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Bool(true),
                PipeField::Dest(common::RegDest::MulDivLo),
            ],
        );
        // MOVN
        // MOVZ
        // HALT
        instr = instruction::Instruction::new(HALT_INSTRUCTION).unwrap();
        test_instr(instr, "ID/EX", 2, &vec![Halt], &vec![PipeField::Bool(true)]);
        // NOP
        instr = instruction::Instruction::new(0).unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                Reg1,
                Reg2,
                Muldivhi,
                Muldivlo,
                MuldivReqValid,
                SignExtImm,
                Rt,
                Rd,
                WriteReg,
                AluOp,
                RegDest,
            ],
            &vec![
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::UInt(0),
                PipeField::Bool(false),
                PipeField::UInt(0),
                PipeField::Byte(0),
                PipeField::Byte(0),
                PipeField::Bool(false),
                PipeField::Op(ALUOperation::ADD),
                PipeField::Dest(common::RegDest::XXX),
            ],
        );
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
