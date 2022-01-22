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

use alu::ALUError;
use common::*;
use pipe_reg::{PipeField, PipeFieldName};
use std::convert::TryFrom;
use traits::ClockedMap;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
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
    memory_section_offset: u32,

    cycles: u64,
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
#[wasm_bindgen]
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
                    PipeFieldName::Reg1Src,
                    PipeFieldName::Reg2Src,
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
                    PipeFieldName::MemSigned,
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
            memory_section_offset: 0,
            cycles: 0u64,
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
        eprintln!("Instructions");
        for v in instrs {
            self.memory.load(mem_index, *v);
            eprintln!(
                "0x{:X}: {:#?}",
                mem_index,
                instruction::Instruction::new(*v).unwrap()
            );
            mem_index = mem_index + 4;
        }

        self.memory_section_offset = TEXT_START + 4;

        eprintln!("Data");
        for v in data {
            self.memory.load(mem_index, *v);
            eprintln!("0x{:X}: {}", mem_index, *v);
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
            .load(PipeFieldName::PcPlus4, PipeField::U32(0));
        $sim.if_id_reg
            .load(PipeFieldName::Instruction, PipeField::U32(0));
        $sim.if_id_reg
            .load(PipeFieldName::InstructionPc, PipeField::U32(0));
    }};
    ($sim: expr, DECODE) => {{
        $sim.id_ex_reg
            .load(PipeFieldName::WriteReg, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::WriteMem, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::ReadMem, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::IsNop, PipeField::Bool(true));
        $sim.id_ex_reg
            .load(PipeFieldName::Instruction, PipeField::U32(0));
        $sim.id_ex_reg
            .load(PipeFieldName::AluOp, PipeField::Op(ALUOperation::ADD));
        $sim.id_ex_reg
            .load(PipeFieldName::AluSrc1, PipeField::ALU(ALUSrc::Zero));
        $sim.id_ex_reg
            .load(PipeFieldName::AluSrc2, PipeField::ALU(ALUSrc::Zero));
        $sim.id_ex_reg
            .load(PipeFieldName::RegDest, PipeField::Dest(RegDest::XXX));
        $sim.id_ex_reg
            .load(PipeFieldName::IsBranch, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::IsJump, PipeField::Bool(false));
        $sim.id_ex_reg
            .load(PipeFieldName::BranchTaken, PipeField::Bool(false));
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

        let pc = if is_branch && branch_taken {
            match self.id_ex_reg.read(PipeFieldName::BranchTarget) {
                PipeField::U32(t) => {
                    self.stalling_unit
                        .load(PipeFieldName::SquashDecode, PipeField::Bool(false));
                    t
                }
                _ => panic!(),
            }
        } else if is_jump {
            match self.id_ex_reg.read(PipeFieldName::JumpTarget) {
                PipeField::U32(t) => {
                    self.stalling_unit
                        .load(PipeFieldName::SquashDecode, PipeField::Bool(false));
                    t
                }
                _ => panic!(),
            }
        } else {
            match self.pc.read(PipeFieldName::PC) {
                PipeField::U32(pc_) => pc_,
                _ => panic!(),
            }
        };
        eprintln!("Fetch: pc:0x{:X}", pc);

        // Check if this stage is being squashed
        if squash {
            // don't update state and send a bubble
        } else if stall {
            // Don't load anything from pc and don't update pc
            // Don't send any new values
        } else {
            let instr = self.memory.read(pc);
            eprintln!("instr word: 0x{:X}", instr);

            self.if_id_reg
                .load(PipeFieldName::Instruction, PipeField::U32(instr));
            self.if_id_reg
                .load(PipeFieldName::InstructionPc, PipeField::U32(pc));
            self.if_id_reg
                .load(PipeFieldName::PcPlus4, PipeField::U32(pc + 4));
            self.pc.load(PipeFieldName::PC, PipeField::U32(pc + 4));
        }
    }

    fn check_register_dependencies(
        &mut self,
        instruction: &instruction::Instruction,
    ) -> (Register, Register) {
        self.controller.update_state(&instruction);
        for (field, value) in self.controller.get_state_vec() {
            // Load default controller values
            self.id_ex_reg.load(field, value);
        }

        let r1 = match self.controller.get_state(PipeFieldName::Reg1Src).unwrap() {
            PipeField::RSrc(RegSrc::Rt) => {
                if let Some(rt) = instruction.get_rt() {
                    rt
                } else {
                    Register::ZERO
                }
            }
            PipeField::RSrc(RegSrc::Rs) => {
                if let Some(rs) = instruction.get_rs() {
                    rs
                } else {
                    Register::ZERO
                }
            }
            _ => Register::ZERO,
        };
        let r2 = match self.controller.get_state(PipeFieldName::Reg2Src).unwrap() {
            PipeField::RSrc(RegSrc::Rt) => {
                if let Some(rt) = instruction.get_rt() {
                    rt
                } else {
                    Register::ZERO
                }
            }
            PipeField::RSrc(RegSrc::Rs) => {
                if let Some(rs) = instruction.get_rs() {
                    rs
                } else {
                    Register::ZERO
                }
            }
            _ => Register::ZERO,
        };

        (r1, r2)
    }

    fn decode_stage(&mut self, _stall: bool, squash: bool) {
        if squash {
            insert_bubble!(self, DECODE);
            self.controller
                .update_state(&instruction::Instruction::new(0).unwrap());
        } else {
            let instr = match self.if_id_reg.read(PipeFieldName::Instruction) {
                PipeField::U32(i) => i,
                _ => panic!(),
            };
            self.if_id_reg
                .pass_through(&mut self.id_ex_reg, PipeFieldName::InstructionPc);
            self.if_id_reg
                .pass_through(&mut self.id_ex_reg, PipeFieldName::PcPlus4);

            let parsed_instr = instruction::Instruction::new(instr);
            eprintln!("Decode: instr: 0x{:X}", instr);
            match parsed_instr {
                Ok(instruction) => {
                    eprintln!("Decode: halt: {}", instruction.is_halt());
                    eprintln!("Instr word: 0x{:X}", instruction.get_instr_word());
                    self.controller.update_state(&instruction);
                    for (field, value) in self.controller.get_state_vec() {
                        // Load controller values
                        self.id_ex_reg.load(field, value);
                    }

                    //self.id_ex_reg.load(PipeFieldName::AluSrc1, self.controller.get_state())

                    // Check if is halt or nop
                    match self.controller.get_state(PipeFieldName::Halt).unwrap() {
                        PipeField::Bool(true) => {
                            eprintln!("Decode: FOUND HALT INSTRUCTION");
                            self.id_ex_reg.clear_pending();
                            self.id_ex_reg
                                .load(PipeFieldName::Halt, PipeField::Bool(true));
                            return;
                        }

                        _ => {}
                    }

                    match self.controller.get_state(PipeFieldName::IsNop).unwrap() {
                        PipeField::Bool(true) => {
                            return;
                        }
                        _ => {}
                    }

                    let (r1, r2) = self.check_register_dependencies(&instruction);

                    let start_stalling = self.stalling_unit.check_write_in_flight(r1)
                        || self.stalling_unit.check_write_in_flight(r2);
                    eprintln!("start_stalling {}", start_stalling);

                    // If fetch was stalling previously and we are no longer stalling, run fetch_stage again
                    match self.stalling_unit.read(PipeFieldName::StallFetch) {
                        PipeField::Bool(true) => {
                            if !start_stalling {
                                self.fetch_stage(
                                    false,
                                    match self.stalling_unit.read(PipeFieldName::SquashFetch) {
                                        PipeField::Bool(b) => b,
                                        _ => panic!(),
                                    },
                                );
                            }
                        }
                        _ => {}
                    }

                    self.stalling_unit
                        .load(PipeFieldName::StallFetch, PipeField::Bool(start_stalling));

                    if start_stalling {
                        // Send execute bubble
                        self.if_id_reg.clear_pending();
                        self.pc.clear_pending();

                        self.if_id_reg.load(
                            PipeFieldName::PcPlus4,
                            self.if_id_reg.read(PipeFieldName::PcPlus4),
                        );
                        self.if_id_reg.load(
                            PipeFieldName::Instruction,
                            self.if_id_reg.read(PipeFieldName::Instruction),
                        );
                        self.if_id_reg.load(
                            PipeFieldName::InstructionPc,
                            self.if_id_reg.read(PipeFieldName::InstructionPc),
                        );
                        self.pc
                            .load(PipeFieldName::PC, self.pc.read(PipeFieldName::PC));
                        insert_bubble!(self, DECODE);
                        return;
                    }

                    let reg_1_val = self.reg_file.read(r1);
                    let reg_2_val = self.reg_file.read(r2);

                    self.id_ex_reg
                        .load(PipeFieldName::Reg1, PipeField::U32(reg_1_val));
                    self.id_ex_reg
                        .load(PipeFieldName::Reg2, PipeField::U32(reg_2_val));

                    match self.controller.get_state(PipeFieldName::WriteReg).unwrap() {
                        PipeField::Bool(write_reg) => {
                            if write_reg {
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

                    match self
                        .controller
                        .get_state(PipeFieldName::MuldivReqValid)
                        .unwrap()
                    {
                        PipeField::Bool(true) => {
                            self.stalling_unit.start_write_in_flight(Register::HI);
                            self.stalling_unit.start_write_in_flight(Register::LO);
                        }
                        _ => {}
                    }

                    let branch_compare: i8 = if reg_1_val < reg_2_val {
                        -1
                    } else if reg_1_val > reg_2_val {
                        1
                    } else {
                        0
                    };
                    let mut branch_taken = false;
                    // Resolve branch conditions here
                    match self.controller.get_state(PipeFieldName::IsBranch).unwrap() {
                        PipeField::Bool(true) => {
                            branch_taken = match self
                                .controller
                                .get_state(PipeFieldName::BranchType)
                                .unwrap()
                            {
                                PipeField::Branch(BranchType::Beq) => branch_compare == 0,
                                PipeField::Branch(BranchType::Bne) => branch_compare != 0,
                                PipeField::Branch(BranchType::Bgez)
                                | PipeField::Branch(BranchType::Bgezal) => {
                                    reg_1_val & 0x8000_0000 == 0
                                }
                                PipeField::Branch(BranchType::Bgtz) => reg_1_val as i32 > 0,
                                PipeField::Branch(BranchType::Blez) => (reg_1_val as i32) <= 0,
                                PipeField::Branch(BranchType::Bltzal)
                                | PipeField::Branch(BranchType::Bltz) => reg_1_val & 0x8000_0000 != 0,
                                _ => false,
                            };

                            self.id_ex_reg
                                .load(PipeFieldName::BranchTaken, PipeField::Bool(branch_taken));
                        }
                        _ => {}
                    }
                    // Calculate branch target
                    let sign_ext_imm = Sim::sign_ext_imm(instruction.get_imm().unwrap_or(0));
                    let offset = sign_ext_imm << 2;

                    let pc_plus_4 = match self.if_id_reg.read(PipeFieldName::PcPlus4) {
                        PipeField::U32(v) => v,
                        _ => panic!(),
                    };
                    let branch_target = pc_plus_4.wrapping_add(offset);
                    self.id_ex_reg
                        .load(PipeFieldName::BranchTarget, PipeField::U32(branch_target));
                    if branch_taken {
                        self.stalling_unit
                            .load(PipeFieldName::SquashDecode, PipeField::Bool(true));

                        self.if_id_reg.clear_pending();
                        insert_bubble!(self, FETCH);
                    }

                    match self.controller.get_state(PipeFieldName::IsJump).unwrap() {
                        PipeField::Bool(is_jump) => {
                            self.id_ex_reg
                                .load(PipeFieldName::IsJump, PipeField::Bool(is_jump));

                            if is_jump {
                                self.stalling_unit
                                    .load(PipeFieldName::SquashFetch, PipeField::Bool(true));

                                match instruction.get_op_code() {
                                    instruction::OpCode::J | instruction::OpCode::Jal => {
                                        let target = instruction.get_address().unwrap();
                                        self.id_ex_reg.load(
                                            PipeFieldName::JumpTarget,
                                            PipeField::U32(target),
                                        );
                                    }
                                    instruction::OpCode::RType => {
                                        let target =
                                            self.reg_file.read(instruction.get_rs().unwrap());

                                        self.id_ex_reg.load(
                                            PipeFieldName::JumpTarget,
                                            PipeField::U32(target),
                                        );
                                    }
                                    _ => panic!(
                                        "Invalid jump opcode: {:#?}",
                                        instruction.get_op_code()
                                    ),
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                    // PcPlus4
                    self.if_id_reg
                        .pass_through(&mut self.id_ex_reg, PipeFieldName::PcPlus4);
                    // SignExtImm
                    self.id_ex_reg
                        .load(PipeFieldName::SignExtImm, PipeField::U32(sign_ext_imm));
                    // Rt
                    self.id_ex_reg.load(
                        PipeFieldName::Rt,
                        PipeField::U8(instruction.get_rt().unwrap_or(Register::ZERO) as u8),
                    );
                    // Rd
                    self.id_ex_reg.load(
                        PipeFieldName::Rd,
                        PipeField::U8(instruction.get_rd().unwrap_or(Register::ZERO) as u8),
                    );
                    // Shamt
                    self.id_ex_reg.load(
                        PipeFieldName::Shamt,
                        PipeField::U8(instruction.get_shamt().unwrap_or(0)),
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
        }
    }

    fn handle_execute_exception(&mut self, err: ALUError) {
        let epc = match self.id_ex_reg.read(PipeFieldName::InstructionPc) {
            PipeField::U32(v) => v,
            _ => panic!(),
        };
        // Write to halt register
        eprintln!(
            "Halting immediately due to exception: {:#?} at pc: 0x{:X}",
            err, epc
        );
        self.halt.load(PipeFieldName::Halt, PipeField::Bool(true));
        // TODO:
        // Write to cause register
        // Write to epc
        self.epc_reg.load(PipeFieldName::EPC, PipeField::U32(epc));
    }

    fn get_alu_src_val(&mut self, src: PipeFieldName) -> u32 {
        let src = self.id_ex_reg.read(src);
        match src {
            PipeField::ALU(ALUSrc::Shamt) => match self.id_ex_reg.read(PipeFieldName::Shamt) {
                PipeField::U8(shamt) => shamt as u32,
                _ => panic!(),
            },
            PipeField::ALU(ALUSrc::Zero) => 0,
            PipeField::ALU(ALUSrc::Reg1) => match self.id_ex_reg.read(PipeFieldName::Reg1) {
                PipeField::U32(r) => r,
                _ => panic!(),
            },
            PipeField::ALU(ALUSrc::Reg2) => match self.id_ex_reg.read(PipeFieldName::Reg2) {
                PipeField::U32(r) => r,
                _ => panic!(),
            },
            PipeField::ALU(ALUSrc::Muldivlo) => {
                match self.id_ex_reg.read(PipeFieldName::Muldivlo) {
                    PipeField::U32(m) => m,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::Muldivhi) => {
                match self.id_ex_reg.read(PipeFieldName::Muldivhi) {
                    PipeField::U32(m) => m,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::SignExtImm) => {
                match self.id_ex_reg.read(PipeFieldName::SignExtImm) {
                    PipeField::U32(i) => i,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::ZeroExtImm) => {
                match self.id_ex_reg.read(PipeFieldName::SignExtImm) {
                    PipeField::U32(i) => i & 0x0000_FFFF,
                    _ => panic!(),
                }
            }
            PipeField::ALU(ALUSrc::PcPlus4) => match self.id_ex_reg.read(PipeFieldName::PcPlus4) {
                PipeField::U32(pc_4) => pc_4,
                _ => panic!(),
            },
            _ => panic!(),
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
            // Check if is halt
            match self.id_ex_reg.read(PipeFieldName::Halt) {
                PipeField::Bool(true) => {
                    eprintln!("Execute: FOUND HALT INSTRUCTION");
                    self.id_ex_reg
                        .pass_through(&mut self.ex_mem_reg, PipeFieldName::Halt);
                    return;
                }
                _ => {}
            }

            // get alu operands
            let alu_src_1 = self.get_alu_src_val(PipeFieldName::AluSrc1);
            let alu_src_2 = self.get_alu_src_val(PipeFieldName::AluSrc2);

            let alu_operation = match self.id_ex_reg.read(PipeFieldName::AluOp) {
                PipeField::Op(op) => op,
                _ => panic!(),
            };

            let alu_result = match alu::calculate(alu_src_1, alu_src_2, alu_operation) {
                Ok(res) => res,
                Err(e) => {
                    self.handle_execute_exception(e);
                    0
                }
            };
            eprintln!(
                "Execute\nalu_val_1: 0x{:X} from {:?}, alu_val_2: 0x{:X} from {:?}\nalu_op: {:?}, alu_result: 0x{:X} to be written to Register {:?}",
                alu_src_1,
                self.id_ex_reg.read(PipeFieldName::AluSrc1),
                alu_src_2,
                self.id_ex_reg.read(PipeFieldName::AluSrc2),
                alu_operation, alu_result,
                match self.id_ex_reg.read(PipeFieldName::RegDest) {
                    PipeField::Dest(r) => r,
                    _ => panic!()
            }
            );
            self.ex_mem_reg
                .load(PipeFieldName::ALURes, PipeField::U32(alu_result));

            let muldiv_req_valid = match self.id_ex_reg.read(PipeFieldName::MuldivReqValid) {
                PipeField::Bool(v) => v,
                _ => panic!(),
            };

            if muldiv_req_valid {
                let muldiv_result = match alu_operation {
                    ALUOperation::MULT => match alu::multiply(alu_src_1, alu_src_2, true) {
                        Ok(r) => r,
                        Err(e) => {
                            self.handle_execute_exception(e);
                            0
                        }
                    },
                    ALUOperation::MULTU => match alu::multiply(alu_src_1, alu_src_2, false) {
                        Ok(r) => r,
                        Err(e) => {
                            self.handle_execute_exception(e);
                            0
                        }
                    },
                    ALUOperation::DIV => match alu::divide(alu_src_1, alu_src_2, true) {
                        Ok(r) => r,
                        Err(e) => {
                            self.handle_execute_exception(e);
                            0
                        }
                    },
                    ALUOperation::DIVU => match alu::divide(alu_src_1, alu_src_2, false) {
                        Ok(r) => r,
                        Err(e) => {
                            self.handle_execute_exception(e);
                            0
                        }
                    },
                    _ => unreachable!(),
                };

                self.ex_mem_reg
                    .load(PipeFieldName::MuldivRes, PipeField::U64(muldiv_result));
            } else {
                self.ex_mem_reg
                    .load(PipeFieldName::MuldivRes, PipeField::U64(0));
            }

            // Determine RegToWrite
            let dest = match self.id_ex_reg.read(PipeFieldName::RegDest) {
                PipeField::Dest(d) => d,
                _ => panic!(),
            };

            match dest {
                RegDest::Rd => {
                    let rd: u8 = match self.id_ex_reg.read(PipeFieldName::Rd) {
                        PipeField::U8(r) => r,
                        _ => panic!(),
                    };
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::U8(rd));
                }
                RegDest::Rt => {
                    let rt: u8 = match self.id_ex_reg.read(PipeFieldName::Rt) {
                        PipeField::U8(r) => r,
                        _ => panic!(),
                    };
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::U8(rt));
                }
                RegDest::Ra => {
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::U8(Register::RA as u8));
                }
                RegDest::MulDivHi => {
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::U8(Register::HI as u8));
                }
                RegDest::MulDivLo => {
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::U8(Register::LO as u8));
                }
                RegDest::XXX => {
                    self.ex_mem_reg
                        .load(PipeFieldName::RegToWrite, PipeField::U8(0u8));
                }
            }

            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::Reg2);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::WriteReg);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::WriteMem);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::ReadMem);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::MemWidth);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::MemSigned);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::AluToReg);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::MuldivReqValid);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::Halt);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::IsNop);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::Instruction);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::InstructionPc);
            self.id_ex_reg
                .pass_through(&mut self.ex_mem_reg, PipeFieldName::InDelaySlot);
        }
    }

    fn memory_stage(&mut self, stall: bool, squash: bool) {
        if squash {
            insert_bubble!(self, MEMORY);
        } else if stall {
        } else {
            match self.ex_mem_reg.read(PipeFieldName::Halt) {
                PipeField::Bool(true) => {
                    eprintln!("Memory: FOUND HALT INSTRUCTION");
                    self.ex_mem_reg
                        .pass_through(&mut self.mem_wb_reg, PipeFieldName::Halt);
                    self.mem_wb_reg
                        .load(PipeFieldName::IsNop, PipeField::Bool(false));
                    return;
                }

                _ => {}
            }
            let alu_res = match self.ex_mem_reg.read(PipeFieldName::ALURes) {
                PipeField::U32(r) => r,
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
                PipeField::U8(w) => w,
                _ => panic!(),
            };

            let address =
                alu::calculate(alu_res, self.memory_section_offset, ALUOperation::ADD).unwrap();
            if read_mem {
                eprintln!("Memory: Reading memory at address: 0x{:X}", address);
                let mem_val = self.memory.read(address);
                eprintln!("value: 0x{:X}", mem_val);
                self.mem_wb_reg
                    .load(PipeFieldName::MemData, PipeField::U32(mem_val));
            } else if write_mem {
                eprintln!("Start writing data to address: 0x{:X}", address);
                let reg_2_data = match self.ex_mem_reg.read(PipeFieldName::Reg2) {
                    PipeField::U32(d) => d,
                    _ => panic!(),
                };

                let rem = address % 4;
                match rem {
                    0 => {
                        // TODO: Check that loading bytes works properly
                        let mut input_mask = 0u32;
                        for _ in 0..mem_width {
                            input_mask = (input_mask << 8) | 0xFF;
                        }
                        let output_mask = !input_mask;

                        let lower_bytes = (input_mask & reg_2_data) << ((4 - mem_width) * 8);
                        let current_mem_val = self.memory.read(address);
                        let output_val = (current_mem_val & output_mask) | lower_bytes;
                        self.memory.load(address, output_val);
                    }
                    rem => {
                        // FIXME
                        if mem_width as u32 > rem {
                            let left_mem_val = self.memory.read(address - rem);
                            let right_mem_val = self.memory.read(address - rem + 4);

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

                            eprintln!(
                                "Writing '0x{:X}' to location: 0x{:X} and '0x{:X} to location '0x{:X}'",
                                left_val_to_write,
                                address - rem,
                                right_val_to_write,
                                address - rem + 4
                            );
                            self.memory.load(address - rem, left_val_to_write);
                            self.memory.load(address - rem + 4, right_val_to_write);
                        } else {
                            let current_mem_val = self.memory.read(address - rem);
                            let mut mask = 0;
                            for _ in 0..mem_width {
                                mask = (mask | 0xFF) << 8;
                            }
                            let val = reg_2_data << ((4 - rem) * 8);
                            mask = mask << ((4 - rem) * 8);

                            let output_val = (current_mem_val & !mask)
                                | ((val >> (rem * 8)) & mask) >> (rem * 8);
                            eprintln!(
                                "Writing '0x{:X}' to location: 0x{:X}",
                                output_val,
                                address - rem
                            );
                            self.memory.load(address - rem, output_val);
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
                .pass_through(&mut self.mem_wb_reg, PipeFieldName::MemSigned);
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
            match self.mem_wb_reg.read(PipeFieldName::IsNop) {
                PipeField::Bool(true) => {
                    eprintln!("Writeback: FOUND NOP");
                    return;
                }
                _ => {}
            }
            match self.mem_wb_reg.read(PipeFieldName::Halt) {
                PipeField::Bool(true) => {
                    eprintln!("Writeback: FOUND HALT INSTRUCTION");
                    self.mem_wb_reg
                        .pass_through(&mut self.halt, PipeFieldName::Halt);
                    return;
                }

                _ => {}
            }
            let alu_to_reg = match self.mem_wb_reg.read(PipeFieldName::AluToReg) {
                PipeField::Bool(a) => a,
                _ => panic!(),
            };
            let reg_write = match self.mem_wb_reg.read(PipeFieldName::WriteReg) {
                PipeField::Bool(r) => r,
                _ => panic!(),
            };
            let reg_target = match self.mem_wb_reg.read(PipeFieldName::RegToWrite) {
                PipeField::U8(t) => t,
                _ => panic!(),
            };

            let reg_target = Register::try_from(reg_target).unwrap();

            if reg_write {
                let data = if alu_to_reg {
                    match self.mem_wb_reg.read(PipeFieldName::ALURes) {
                        PipeField::U32(res) => res,
                        _ => panic!(),
                    }
                } else {
                    let mut mem_data = match self.mem_wb_reg.read(PipeFieldName::MemData) {
                        PipeField::U32(m) => m,
                        _ => panic!(),
                    };
                    let mem_width = match self.mem_wb_reg.read(PipeFieldName::MemWidth) {
                        PipeField::U8(w) => w,
                        _ => panic!(),
                    };
                    let mem_signed = match self.mem_wb_reg.read(PipeFieldName::MemSigned) {
                        PipeField::Bool(b) => b,
                        _ => panic!(),
                    };
                    // TODO: Check if mem is signed
                    // TODO: The ordering of bytes is different than what is expected
                    if mem_signed {
                        // Account for sign extension
                        match mem_width {
                            1 => {
                                mem_data = mem_data >> (8 * 3);
                                if mem_data & 0x80 != 0 {
                                    mem_data |= 0xFFFF_FF00;
                                } else {
                                    mem_data &= 0x0000_00FF;
                                }
                            }
                            2 => {
                                mem_data = mem_data >> (8 * 2);
                                if mem_data & 0x8000 != 0 {
                                    mem_data |= 0xFFFF_0000;
                                } else {
                                    mem_data &= 0x0000_FFFF;
                                }
                            }
                            4 => {}
                            _ => unreachable!(),
                        }
                    } else {
                        match mem_width {
                            1 => {
                                mem_data = mem_data >> (8 * 3);
                                mem_data &= 0x0000_00FF;
                            }
                            2 => {
                                mem_data = mem_data >> (8 * 2);
                                mem_data &= 0x0000_FFFF;
                            }
                            4 => {}
                            _ => unreachable!(),
                        }
                    }
                    eprintln!(
                        "Writing data 0x{:X} to register {:#?}",
                        mem_data, reg_target
                    );
                    mem_data
                };

                self.reg_file.load(reg_target, data);
                self.stalling_unit.clear_write_in_flight(reg_target);
                eprintln!(
                    "Cycle {} Writeback to reg: {:#?} with value 0x{:X}",
                    self.cycles, reg_target, data
                );
            }

            match self.mem_wb_reg.read(PipeFieldName::MuldivReqValid) {
                PipeField::Bool(true) => {
                    let muldiv_res = match self.mem_wb_reg.read(PipeFieldName::MuldivRes) {
                        PipeField::U64(v) => v,
                        x => panic!("Invalid enum in MuldivRes: {:#?}", x),
                    };
                    eprintln!("Writeback: muldiv_res: 0x{:X}", muldiv_res);
                    self.reg_file.load(
                        Register::HI,
                        ((muldiv_res & 0xFFFF_FFFF_0000_0000) >> 32) as u32,
                    );
                    self.reg_file.load(
                        Register::LO,
                        ((muldiv_res & 0x0000_0000_FFFF_FFFF) >> 32) as u32,
                    );

                    self.stalling_unit.clear_write_in_flight(Register::HI);
                    self.stalling_unit.clear_write_in_flight(Register::LO);
                }
                _ => {}
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
        eprintln!("Cycle: {}", self.cycles);
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

        self.stalling_unit.clock();
        // Check squash and stall signals here and set registers accordingly

        self.if_id_reg.clock();
        self.id_ex_reg.clock();
        self.ex_mem_reg.clock();
        self.mem_wb_reg.clock();

        // TODO:
        self.pc.clock();
        self.status_reg.clock();
        self.epc_reg.clock();
        self.bad_v_addr.clock();
        self.halt.clock();

        self.reg_file.clock();

        self.memory.clock();
        // eprintln!("Cycle: {}\n{:#?}", self.cycles, self.get_state());
        // eprintln!("Cycle: {}\n{:#?}", self.cycles, self.get_state());

        self.cycles += 1;
    }

    fn initialize_registers(&mut self) {
        //use common::Register;

        self.reg_file.load(Register::SP, STACK_POINTER_INITIAL);
        self.reg_file.load(Register::FP, STACK_POINTER_INITIAL);
        self.pc.load(PipeFieldName::PC, PipeField::U32(TEXT_START));
        self.halt.load(PipeFieldName::Halt, PipeField::Bool(false));

        self.reg_file.clock();
        self.pc.clock();
        self.halt.clock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use instruction::Instruction;

    fn assert_pipe_fields(
        pipe_register: Box<dyn ClockedMap<PipeFieldName, PipeField>>,
        keys: &Vec<(PipeFieldName, PipeField)>,
    ) {
        for (k, v) in keys {
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
        fields: &Vec<(PipeFieldName, PipeField)>,
    ) {
        eprintln!("testing instr: {:#?}", instr);
        let mut instrs: Vec<u32> = Vec::with_capacity(5);
        let mut sim = Sim::new();
        let data: Vec<u32> = Vec::new();
        instrs.push(instr.get_instr_word());

        for _ in 1..instrs.len() {
            instrs.push(0);
        }

        sim.load_binary(&instrs, &data);

        sim.step(num_cycles);

        let register: Box<dyn ClockedMap<PipeFieldName, PipeField>> = match register_name {
            "IF/ID" => Box::new(sim.get_state().if_id_reg.clone()),
            "ID/EX" => Box::new(sim.get_state().id_ex_reg.clone()),
            "EX/MEM" => Box::new(sim.get_state().ex_mem_reg.clone()),
            "MEM/WB" => Box::new(sim.get_state().mem_wb_reg.clone()),
            "PC" => Box::new(sim.get_state().pc.clone()),
            "HALT" => Box::new(sim.get_state().halt.clone()),
            "EPC" => Box::new(sim.get_state().epc_reg.clone()),
            "CAUSE" => Box::new(sim.get_state().cause_reg.clone()),
            "BAD_V_ADDR" => Box::new(sim.get_state().bad_v_addr.clone()),
            "STALLING_UNIT" => Box::new(sim.get_state().stalling_unit.clone()),
            _ => panic!(),
        };

        assert_pipe_fields(register, fields);
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
            &vec![
                (PipeFieldName::PcPlus4, PipeField::U32(TEXT_START + 4)),
                (
                    PipeFieldName::Instruction,
                    PipeField::U32(instr.clone().get_instr_word()),
                ),
            ],
        );

        use PipeFieldName::*;
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADDU)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::AND)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::ZERO as u8)),
                (Rd, PipeField::U8(common::Register::T1 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::CLO)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::ZERO as u8)),
                (Rd, PipeField::U8(common::Register::T1 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::CLZ)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(true)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::ZERO as u8)),
                (WriteReg, PipeField::Bool(false)),
                (AluOp, PipeField::Op(ALUOperation::DIV)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(true)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::ZERO as u8)),
                (WriteReg, PipeField::Bool(false)),
                (AluOp, PipeField::Op(ALUOperation::DIVU)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(true)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::ZERO as u8)),
                (WriteReg, PipeField::Bool(false)),
                (AluOp, PipeField::Op(ALUOperation::MULT)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(true)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::ZERO as u8)),
                (WriteReg, PipeField::Bool(false)),
                (AluOp, PipeField::Op(ALUOperation::MULTU)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T2 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::MUL)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::NOR)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::OR)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(10)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(10)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADDU)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(10)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::AND)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(10)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::OR)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(10)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::XOR)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T1 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SLL)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T2 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SLL)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T1 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SRA)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T2 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SRA)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T1 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SRL)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(common::Register::T0 as u8)),
                (Rd, PipeField::U8(common::Register::T2 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SRL)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(8)),
                (Rd, PipeField::U8(10)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::XOR)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
            ],
        );
        // LUI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Lui,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::LUI)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
            ],
        );
        // SLT
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Slt),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0x0)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (Rd, PipeField::U8(Register::T2 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SLT)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
            ],
        );
        // SLTU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Sltu),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0x0)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (Rd, PipeField::U8(Register::T2 as u8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SLTU)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
            ],
        );
        // SLTI
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Slti,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SLT)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
            ],
        );
        // SLTIU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Sltiu,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::SLTU)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
            ],
        );
        // BEQ
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Beq,
            None,
            Some(Register::T0),
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (
                    BranchTarget,
                    PipeField::U32((TEXT_START + 4) + (0x0000_0020 << 2)),
                ),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(true)),
                (BranchType, PipeField::Branch(common::BranchType::Beq)),
            ],
        );

        test_instr(
            instr,
            "PC",
            3,
            &vec![(
                PC,
                PipeField::U32(4 + (TEXT_START + 4) + (0x0000_0020 << 2)),
            )],
        );

        // BGEZ
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Bgelt,
            None,
            Some(Register::AT),
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(true)),
                (BranchType, PipeField::Branch(common::BranchType::Bgez)),
            ],
        );
        // BGEZAL
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Bgelt,
            None,
            Some(Register::S1),
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(true)),
                (BranchType, PipeField::Branch(common::BranchType::Bgezal)),
            ],
        );
        // BGTZ
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Bgtz,
            None,
            None,
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(false)),
                (BranchType, PipeField::Branch(common::BranchType::Bgtz)),
            ],
        );
        // BLEZ
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Blez,
            None,
            None,
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(true)),
                (BranchType, PipeField::Branch(common::BranchType::Blez)),
            ],
        );
        // BLTZAL
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Bgelt,
            None,
            Some(Register::S0),
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(false)),
                (BranchType, PipeField::Branch(common::BranchType::Bltzal)),
            ],
        );
        // BLTZ
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Bgelt,
            None,
            Some(Register::ZERO),
            Some(Register::T0),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(false)),
                (BranchType, PipeField::Branch(common::BranchType::Bltz)),
            ],
        );
        // BNE
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Bne,
            None,
            Some(Register::ZERO),
            Some(Register::FP),
            None,
            None,
            Some(0x0020),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(STACK_POINTER_INITIAL)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0x0000_0020)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (IsBranch, PipeField::Bool(true)),
                (BranchTaken, PipeField::Bool(true)),
                (BranchType, PipeField::Branch(common::BranchType::Bne)),
            ],
        );
        test_instr(
            instr,
            "STALLING_UNIT",
            2,
            &vec![(SquashFetch, PipeField::Bool(false))],
        );
        // J
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::J,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(TEXT_START),
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (IsJump, PipeField::Bool(true)),
                (IsBranch, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (ReadMem, PipeField::Bool(false)),
                (JumpTarget, PipeField::U32(TEXT_START)),
            ],
        );
        test_instr(
            instr,
            "STALLING_UNIT",
            2,
            &vec![(SquashFetch, PipeField::Bool(true))],
        );
        // JAL
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Jal,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(TEXT_START),
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (IsJump, PipeField::Bool(true)),
                (WriteReg, PipeField::Bool(true)),
                (WriteMem, PipeField::Bool(false)),
                (ReadMem, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::Ra)),
                (JumpTarget, PipeField::U32(TEXT_START)),
            ],
        );
        test_instr(
            instr,
            "STALLING_UNIT",
            2,
            &vec![(SquashFetch, PipeField::Bool(true))],
        );
        // JALR
        // JR
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::RType,
            Some(instruction::FuncCode::Jr),
            None,
            Some(Register::FP),
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
                (IsJump, PipeField::Bool(true)),
                (WriteReg, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(false)),
                (ReadMem, PipeField::Bool(false)),
                (JumpTarget, PipeField::U32(STACK_POINTER_INITIAL)),
            ],
        );
        // LB
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Lb,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemSigned, PipeField::Bool(true)),
                (MemWidth, PipeField::U8(1)),
                (ReadMem, PipeField::Bool(true)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (AluToReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // LBU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Lbu,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemSigned, PipeField::Bool(false)),
                (MemWidth, PipeField::U8(1)),
                (ReadMem, PipeField::Bool(true)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (AluToReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // LH
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Lh,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemSigned, PipeField::Bool(true)),
                (MemWidth, PipeField::U8(2)),
                (ReadMem, PipeField::Bool(true)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (AluToReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // LHU
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Lhu,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemSigned, PipeField::Bool(false)),
                (MemWidth, PipeField::U8(2)),
                (ReadMem, PipeField::Bool(true)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (AluToReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // LW
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Lw,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemSigned, PipeField::Bool(true)),
                (MemWidth, PipeField::U8(4)),
                (ReadMem, PipeField::Bool(true)),
                (WriteMem, PipeField::Bool(false)),
                (WriteReg, PipeField::Bool(true)),
                (AluToReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::Rt)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // TODO(LWL)
        // TODO(LWR)
        // SB
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Sb,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemWidth, PipeField::U8(1)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(true)),
                (WriteReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // SH
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Sh,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemWidth, PipeField::U8(2)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(true)),
                (WriteReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // SW
        instr = instruction::Instruction::from_parts(
            instruction::OpCode::Sw,
            None,
            Some(Register::T0),
            None,
            None,
            None,
            Some(0xffff),
            None,
        )
        .unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (SignExtImm, PipeField::U32(0xffff_ffff)),
                (Rt, PipeField::U8(Register::T0 as u8)),
                (MemSigned, PipeField::Bool(true)),
                (MemWidth, PipeField::U8(4)),
                (ReadMem, PipeField::Bool(false)),
                (WriteMem, PipeField::Bool(true)),
                (WriteReg, PipeField::Bool(false)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
            ],
        );
        // TODO: SWL
        // TODO: SWR
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(0)),
                (Rd, PipeField::U8(8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(0)),
                (Rd, PipeField::U8(8)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::Rd)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(0)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::MulDivHi)),
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
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(0)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(true)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (AluToReg, PipeField::Bool(true)),
                (RegDest, PipeField::Dest(common::RegDest::MulDivLo)),
            ],
        );
        // TODO: MOVN
        // TODO: MOVZ
        // HALT
        instr = instruction::Instruction::new(HALT_INSTRUCTION).unwrap();
        test_instr(instr, "ID/EX", 2, &vec![(Halt, PipeField::Bool(true))]);
        // NOP
        instr = instruction::Instruction::new(0).unwrap();
        test_instr(
            instr,
            "ID/EX",
            2,
            &vec![
                (Reg1, PipeField::U32(0)),
                (Reg2, PipeField::U32(0)),
                (Muldivhi, PipeField::U32(0)),
                (Muldivlo, PipeField::U32(0)),
                (MuldivReqValid, PipeField::Bool(false)),
                (SignExtImm, PipeField::U32(0)),
                (Rt, PipeField::U8(0)),
                (Rd, PipeField::U8(0)),
                (WriteReg, PipeField::Bool(false)),
                (AluOp, PipeField::Op(ALUOperation::ADD)),
                (RegDest, PipeField::Dest(common::RegDest::XXX)),
            ],
        );
    }

    #[test]
    #[ignore]
    pub fn test_execute_stage() {}

    #[test]
    #[ignore]
    pub fn test_memory_stage() {}

    #[test]
    #[ignore]
    pub fn test_writeback_stage() {}
}
