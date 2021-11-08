mod alu;
mod controller;
mod stalling;
mod mem;
mod pipe_reg;
mod pipe_stage;
mod reg_file;
mod instruction;

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

    status_reg: pipe_reg::PipeRegister,
    cause_reg: pipe_reg::PipeRegister,
    epc_reg: pipe_reg::PipeRegister,
    bad_v_addr: pipe_reg::PipeRegister,
    controller: controller::Controller,
    memory: mem::Memory,
}

#[derive(Debug, Clone, Copy)]
pub struct SimState;

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

        {
            use pipe_reg::PipeRegister;
            // TODO: Determine fields needed for each pipe register
            if_id_reg = PipeRegister::new("IF/ID", vec![
                PipeFieldName::PcPlus4,
                PipeFieldName::Instruction,
                PipeFieldName::InstructionPc,
            ]);
            id_ex_reg = PipeRegister::new("ID/EX", vec![
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
                PipeFieldName::Instruction
            ]);
            ex_mem_reg = PipeRegister::new("EX/MEM", vec![
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
                PipeFieldName::Instruction
            ]);
            mem_wb_reg = PipeRegister::new("MEM/WB", vec![
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
                PipeFieldName::InstructionPc
            ]);
            pc = PipeRegister::new("PC", vec![PipeFieldName::PC]);
            status_reg = PipeRegister::new("STATUS", vec![PipeFieldName::Status]);
            epc_reg = PipeRegister::new("EPC", vec![PipeFieldName::EPC]);
            bad_v_addr = PipeRegister::new("BAD_V_ADDR", vec![PipeFieldName::BadVAddr]);
            cause_reg = PipeRegister::new("CAUSE", vec![PipeFieldName::Cause]);
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
        };

        sim.initialize_registers();

        sim
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

        self.pc.clock();
        self.status_reg.clock();
        self.epc_reg.clock();
        self.bad_v_addr.clock();

        self.reg_file.clock();

        self.memory.clock();
    }

    pub fn step(&mut self, n: u32) {
        for _ in 0..n {
            self._step();
        }
    }

    pub fn step_to_halt() {}

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

    fn initialize_registers(&mut self) {
        //use common::Register;
        use PipeField::PC;

        self.reg_file.load(Register::SP, STACK_POINTER_INITIAL);
        self.reg_file.load(Register::FP, STACK_POINTER_INITIAL);
        self.pc.load(PipeFieldName::PC, PC(TEXT_START));
    }
}
