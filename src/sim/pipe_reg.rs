use super::traits::{ClockedMap, Field, Value};
use super::common::{ALUSrc, ALUOperation, RegDest, BranchType};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum PipeField {
    PcPlus4(u32),
    Reg1(u32),
    Reg2(u32),
    Muldivhi(u32),
    Muldivlo(u32),
    MuldivReqValid(bool),
    SignExtImm(u32),
    Rt(u8),
    Rd(u8),
    Shamt(u8),
    JumpTarget(u32),
    WriteReg(bool),
    ReadMem(bool),
    WriteMem(bool),
    MemWidth(u8),
    MemSigned(bool),
    MemData(u32),
    AluSrc1(ALUSrc), 
    AluSrc2(ALUSrc),
    ALURes(u32),
    AluToReg(bool),
    AluOp(ALUOperation),
    RegDest(RegDest),
    Halt(bool),
    IsNop(bool),
    IsBranch(bool),
    IsJump(bool),
    TakeJump(bool),
    BranchType(BranchType),
    InstructionPc(u32),
    Instruction(u32),
    InDelaySlot(bool),
    MuldivRes(u64),
    PC(u32),
    Status(u32),
    EPC(u32),
    BadVAddr(u32),
    Cause(u32),
    StallFetch(bool),
    StallDecode(bool),
    StallExecute(bool),
    StallMemory(bool),
    StallWriteback(bool),
    BubbleDecode(bool),
    BubbleExecute(bool),
    BubbleMemory(bool),
    BubbleWriteback(bool),
    SquashFetch(bool),
    SquashDecode(bool),
    SquashExecute(bool),
    SquashMemory(bool),
    SquashWriteback(bool),
    XXX,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
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
    Halt,
    IsNop,
    IsBranch,
    IsJump,
    TakeJump,
    BranchType,
    InstructionPc,
    Instruction,
    InDelaySlot,
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
}

impl Value for PipeField {}
impl Field for PipeFieldName {}

#[derive(Debug)]
pub struct PipeRegister {
    name: String,
    current_map: HashMap<PipeFieldName, PipeField>,
    write_buffer: Vec<(PipeFieldName, PipeField)>,
}

impl PipeRegister {
    pub fn new(name: &str, fields: Vec<PipeFieldName>) -> PipeRegister {
        let mut current_map: HashMap<PipeFieldName, PipeField> =
            HashMap::with_capacity(fields.len());
        for f in fields {
            current_map.insert(f, PipeField::XXX);
        }
        let write_buffer = Vec::new();
        PipeRegister {
            name: String::from(name),
            current_map,
            write_buffer,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

impl ClockedMap<PipeFieldName, PipeField> for PipeRegister {
    fn load(&mut self, field: PipeFieldName, value: PipeField) {
        if self.current_map.contains_key(&field) {
            self.write_buffer.push((field, value));
        } else {
            panic!(
                "Invalid field '{:#?}' for pipe_register with name: {}",
                field, self.name
            );
        }
    }

    fn clock(&mut self) {
        for (k, v) in self.write_buffer.drain(..) {
            self.current_map.insert(k, v).unwrap();
        }
    }

    fn read(&self, field: PipeFieldName) -> PipeField {
        *self.current_map.get(&field).unwrap()
    }
}
