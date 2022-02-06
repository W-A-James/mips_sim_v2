use super::common;
use super::common::{ALUOperation, ALUSrc, RegSrc};
use super::traits::{ClockedMap, Field, Value};
use lazy_static::lazy_static;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipeFieldName {
    PcPlus4,
    Reg1,
    Reg2,
    Muldivhi,
    Muldivlo,
    MuldivReqValid,
    SignExtImm,
    Rt,
    Rd,
    Shamt,
    JumpTarget,
    WriteReg,
    ReadMem,
    WriteMem,
    MemWidth,
    MemSigned,
    MemData,
    AluSrc1,
    AluSrc2,
    ALURes,
    AluToReg,
    AluOp,
    RegDest,
    RegToWrite,
    Halt,
    IsNop,
    IsBranch,
    IsJump,
    BranchType,
    BranchTarget,
    BranchTaken,
    InstructionPc,
    Instruction,
    InDelaySlot,
    ExitDelaySlot,
    MuldivRes,
    PC,
    Status,
    EPC,
    BadVAddr,
    Cause,
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
    Reg1Src,
    Reg2Src,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipeField {
    U32(u32),
    U64(u64),
    U8(u8),
    Bool(bool),
    RSrc(common::RegSrc),
    Op(common::ALUOperation),
    Dest(common::RegDest),
    ALU(common::ALUSrc),
    Branch(common::BranchType),
}

lazy_static! {
    pub static ref DEFAULT_VALUES: HashMap<PipeFieldName, PipeField> = {
        let mut m = HashMap::new();
        use PipeFieldName::*;
        m.insert(PcPlus4, PipeField::U32(0));
        m.insert(Reg1, PipeField::U32(0));
        m.insert(Reg2, PipeField::U32(0));
        m.insert(Reg1Src, PipeField::RSrc(RegSrc::Rs));
        m.insert(Reg2Src, PipeField::RSrc(RegSrc::Rt));
        m.insert(Muldivhi, PipeField::U32(0));
        m.insert(Muldivlo, PipeField::U32(0));
        m.insert(MuldivReqValid, PipeField::Bool(false));
        m.insert(SignExtImm, PipeField::U32(0));
        m.insert(Rt, PipeField::U8(0));
        m.insert(Rd, PipeField::U8(0));
        m.insert(Shamt, PipeField::U8(0));
        m.insert(JumpTarget, PipeField::U32(0));
        m.insert(WriteReg, PipeField::Bool(false));
        m.insert(ReadMem, PipeField::Bool(false));
        m.insert(WriteMem, PipeField::Bool(false));
        m.insert(MemWidth, PipeField::U8(4));
        m.insert(MemSigned, PipeField::Bool(true));
        m.insert(MemData, PipeField::U32(0));
        m.insert(AluSrc1, PipeField::ALU(ALUSrc::Zero));
        m.insert(AluSrc2, PipeField::ALU(ALUSrc::Zero));
        m.insert(ALURes, PipeField::U32(0));
        m.insert(AluToReg, PipeField::Bool(true));
        m.insert(AluOp, PipeField::Op(ALUOperation::ADD));
        m.insert(RegDest, PipeField::Dest(common::RegDest::XXX));
        m.insert(RegToWrite, PipeField::U8(0));
        m.insert(Halt, PipeField::Bool(false));
        m.insert(IsNop, PipeField::Bool(true));
        m.insert(IsBranch, PipeField::Bool(false));
        m.insert(IsJump, PipeField::Bool(false));
        m.insert(BranchType, PipeField::Branch(common::BranchType::Beq));
        m.insert(BranchTarget, PipeField::U32(0));
        m.insert(BranchTaken, PipeField::Bool(false));
        m.insert(InstructionPc, PipeField::U32(0));
        m.insert(Instruction, PipeField::U32(0));
        m.insert(InDelaySlot, PipeField::Bool(false));
        m.insert(ExitDelaySlot, PipeField::Bool(false));
        m.insert(MuldivRes, PipeField::U64(0));
        m.insert(PC, PipeField::U32(0));
        m.insert(Status, PipeField::U32(0));
        m.insert(EPC, PipeField::U32(0));
        m.insert(BadVAddr, PipeField::U32(0));
        m.insert(Cause, PipeField::U32(0));
        m.insert(StallFetch, PipeField::Bool(false));
        m.insert(StallDecode, PipeField::Bool(false));
        m.insert(StallExecute, PipeField::Bool(false));
        m.insert(StallMemory, PipeField::Bool(false));
        m.insert(StallWriteback, PipeField::Bool(false));
        m.insert(BubbleDecode, PipeField::Bool(false));
        m.insert(BubbleExecute, PipeField::Bool(false));
        m.insert(BubbleMemory, PipeField::Bool(false));
        m.insert(BubbleWriteback, PipeField::Bool(false));
        m.insert(SquashFetch, PipeField::Bool(false));
        m.insert(SquashDecode, PipeField::Bool(false));
        m.insert(SquashExecute, PipeField::Bool(false));
        m.insert(SquashMemory, PipeField::Bool(false));
        m.insert(SquashWriteback, PipeField::Bool(false));

        m
    };
}

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct PipeRegister {
    name: String,
    current_map: HashMap<PipeFieldName, PipeField>,
    write_buffer: HashMap<PipeFieldName, PipeField>,
}

impl PipeRegister {
    pub fn new(name: &str, fields: Vec<PipeFieldName>) -> PipeRegister {
        let mut current_map: HashMap<PipeFieldName, PipeField> =
            HashMap::with_capacity(fields.len());
        for f in fields {
            current_map.insert(f, *DEFAULT_VALUES.get(&f).unwrap());
        }
        let write_buffer = HashMap::new();
        PipeRegister {
            name: String::from(name),
            current_map,
            write_buffer,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn pass_through(&self, other: &mut PipeRegister, field: PipeFieldName) {
        other.load(field, self.read(field));
    }
}

impl Field for PipeFieldName {}
impl Value for PipeField {}
impl ClockedMap<PipeFieldName, PipeField> for PipeRegister {
    fn load(&mut self, field: PipeFieldName, value: PipeField) {
        if self.current_map.contains_key(&field) {
            // Check that value matches type of default
            self.write_buffer.insert(field, value);
        } else {
            panic!(
                "Invalid field '{:#?}' for pipe_register with name: {}",
                field, self.name
            );
        }
    }

    fn clock(&mut self) {
        for (k, v) in self.write_buffer.drain() {
            self.current_map.insert(k, v).unwrap();
        }
    }

    fn read(&self, field: PipeFieldName) -> PipeField {
        *self.current_map.get(&field).unwrap()
    }

    fn clear_pending(&mut self) {
        self.write_buffer.clear();
    }
}
