use super::common::{ALUOperation, ALUSrc, BranchType, RegDest, RegSrc, Register};
use super::instruction::{FuncCode, Instruction, OpCode};
use super::pipe_reg::{PipeField, PipeFieldName, DEFAULT_VALUES};
use std::collections::HashMap;
use std::iter::Iterator;

use wasm_bindgen::prelude::*;

macro_rules! set_signal_value {
    ($item: ident, $field_name: ident, $value: expr) => {{
        $item.signal.insert(PipeFieldName::$field_name, $value);
    }};
}


#[derive(Debug, Clone)]
pub struct Controller {
    signal: HashMap<PipeFieldName, PipeField>,
}

impl Controller {
    pub fn new() -> Controller {
        let mut signal: HashMap<PipeFieldName, PipeField> = HashMap::new();
        {
            use PipeFieldName::*;
            for v in vec![
                AluSrc1,
                AluSrc2,
                AluOp,
                AluToReg,
                MuldivReqValid,
                WriteReg,
                RegDest,
                InDelaySlot,
                ReadMem,
                WriteMem,
                MemWidth,
                Halt,
                IsNop,
                BranchType,
                IsBranch,
                IsJump,
                MemSigned,
                Reg1Src,
                Reg2Src,
            ] {
                signal.insert(v, *DEFAULT_VALUES.get(&v).unwrap());
            }
        }

        Controller { signal }
    }

    pub fn update_state(&mut self, instr: &Instruction) {
        if instr.is_nop() {
            set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
            set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
            set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
            set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
            set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
            set_signal_value!(self, WriteReg, PipeField::Bool(false));
            set_signal_value!(self, ReadMem, PipeField::Bool(false));
            set_signal_value!(self, WriteMem, PipeField::Bool(false));
            set_signal_value!(self, Halt, PipeField::Bool(false));
            set_signal_value!(self, IsNop, PipeField::Bool(true));
            set_signal_value!(self, IsBranch, PipeField::Bool(false));
            set_signal_value!(self, IsJump, PipeField::Bool(false));
            set_signal_value!(self, AluToReg, PipeField::Bool(true));
        } else if instr.is_halt() {
            set_signal_value!(self, Halt, PipeField::Bool(true));
            set_signal_value!(self, IsNop, PipeField::Bool(false));
        } else {
            set_signal_value!(self, Halt, PipeField::Bool(false));
            set_signal_value!(self, IsNop, PipeField::Bool(false));
            match instr.get_op_code() {
                OpCode::RType => {
                    match instr.get_func_code() {
                        Some(func_code) => match func_code {
                            FuncCode::Add => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Addu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADDU));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::And => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::AND));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            // NOTE: WriteReg signal only governs the general purpose registers
                            FuncCode::Div => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::DIV));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Divu => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::DIVU));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Mult => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::MULT));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Multu => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::MULTU));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Nor => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::NOR));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Or => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Sll => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Shamt));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLL));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Sllv => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rs));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLL));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Sra => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Shamt));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRA));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Srav => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rs));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRA));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Srl => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Shamt));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRL));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Srlv => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rs));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRL));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Sub => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUB));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Subu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUBU));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Xor => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::XOR));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                            }
                            FuncCode::Slt => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLT));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Sltu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLTU));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                            }
                            FuncCode::Jalr => {
                                // TODO
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::PcPlus4));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, IsJump, PipeField::Bool(true));

                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));
                            }
                            FuncCode::Jr => {
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, IsJump, PipeField::Bool(true));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));
                            }
                            FuncCode::Tge => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUB));
                            }
                            FuncCode::Tgeu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUBU));
                            }
                            FuncCode::Tlt => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUB));
                            }
                            FuncCode::Tltu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUBU));
                            }
                            FuncCode::Mfhi => {
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Muldivhi));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));
                            }
                            FuncCode::Mflo => {
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Muldivlo));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));

                                set_signal_value!(self, AluToReg, PipeField::Bool(true));
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));
                            }
                            FuncCode::Mthi => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(
                                    self,
                                    RegDest,
                                    PipeField::Dest(RegDest::MulDivHi)
                                );
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));
                            }
                            FuncCode::Mtlo => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(
                                    self,
                                    RegDest,
                                    PipeField::Dest(RegDest::MulDivLo)
                                );
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                set_signal_value!(self, Halt, PipeField::Bool(false));
                                set_signal_value!(self, IsNop, PipeField::Bool(false));
                                set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                set_signal_value!(self, IsJump, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(true));

                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));
                            }
                            FuncCode::Movn => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                            }
                            FuncCode::Movz => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                            }
                            FuncCode::Syscall => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                            }
                            FuncCode::Break => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                            }
                        },
                        None => {
                            panic!("Invalid instruction");
                        }
                    }
                }
                OpCode::Addi => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Addiu => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADDU));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Andi => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::AND));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::ZeroExtImm));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Mul => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));
                    match instr.get_func_code() {
                        Some(func_code) => {
                            match func_code {
                                // Mul
                                FuncCode::Srl => {
                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::MUL)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));

                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));

                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, Halt, PipeField::Bool(false));
                                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                                }
                                // Clo
                                FuncCode::Add => {
                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::CLO)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, Halt, PipeField::Bool(false));
                                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                                }
                                // Clz
                                FuncCode::Addu => {
                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::CLZ)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, Halt, PipeField::Bool(false));
                                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                                }

                                _ => unreachable!(),
                            }
                        }
                        None => unreachable!(),
                    }
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                }
                OpCode::Ori => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::ZeroExtImm));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Xori => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::XOR));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::ZeroExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Lui => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::LUI));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Slti => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLT));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Sltiu => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLTU));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Beq => {
                    // TODO: add signals to disable unwanted behaviour
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));

                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, BranchType, PipeField::Branch(BranchType::Beq));
                }
                OpCode::Bgelt => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                    // TODO: handle the different cases here
                    match instr.get_rt() {
                        Some(rt) => {
                            match rt {
                                // TODO:
                                // BLTZ
                                Register::ZERO => {
                                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::ADD)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));

                                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(
                                        self,
                                        BranchType,
                                        PipeField::Branch(BranchType::Bltz)
                                    );
                                }
                                // BLTZAL
                                // TODO
                                Register::S0 => {
                                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::ADD)
                                    );
                                    set_signal_value!(
                                        self,
                                        AluSrc1,
                                        PipeField::ALU(ALUSrc::PcPlus4)
                                    );
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Ra));

                                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(
                                        self,
                                        BranchType,
                                        PipeField::Branch(BranchType::Bltzal)
                                    );
                                }
                                // BGEZAL
                                // TODO
                                Register::S1 => {
                                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::ADD)
                                    );
                                    set_signal_value!(
                                        self,
                                        AluSrc1,
                                        PipeField::ALU(ALUSrc::PcPlus4)
                                    );
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Ra));

                                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(
                                        self,
                                        BranchType,
                                        PipeField::Branch(BranchType::Bgezal)
                                    );
                                }
                                // BGEZ
                                Register::AT => {
                                    // if leading zeros >= 1, then 0 or positive
                                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::ADD)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));

                                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                                    set_signal_value!(
                                        self,
                                        BranchType,
                                        PipeField::Branch(BranchType::Bgez)
                                    );
                                }

                                _ => unreachable!(),
                            }
                        }
                        None => {}
                    }
                }
                OpCode::Bgtz => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLT));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));

                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                    set_signal_value!(self, BranchType, PipeField::Branch(BranchType::Bgtz));
                }
                OpCode::Blez => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));

                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, BranchType, PipeField::Branch(BranchType::Blez));
                }
                OpCode::Bne => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));

                    set_signal_value!(self, IsBranch, PipeField::Bool(true));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, BranchType, PipeField::Branch(BranchType::Bne));
                }
                OpCode::J => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(true));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                }
                OpCode::Jal => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::PcPlus4));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Ra));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, IsJump, PipeField::Bool(true));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                }
                OpCode::Lb => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(1));
                    set_signal_value!(self, MemSigned, PipeField::Bool(true));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Lbu => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(1));
                    set_signal_value!(self, MemSigned, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Lh => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(2));
                    set_signal_value!(self, MemSigned, PipeField::Bool(true));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Lhu => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(2));
                    set_signal_value!(self, MemSigned, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Lw => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(4));
                    set_signal_value!(self, MemSigned, PipeField::Bool(true));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Lwl => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                }
                OpCode::Lwr => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, ReadMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));

                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Ll => {
                    todo!();
                }
                OpCode::Sb => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(1));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Sh => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(2));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Sw => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::U8(4));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Swl => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Swr => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));

                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                    set_signal_value!(self, MuldivReqValid, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, Halt, PipeField::Bool(false));
                    set_signal_value!(self, IsNop, PipeField::Bool(false));
                    set_signal_value!(self, IsBranch, PipeField::Bool(false));
                    set_signal_value!(self, IsJump, PipeField::Bool(false));
                }
                OpCode::Eret => {
                    todo!();
                }
            }
        }
    }

    pub fn get_state(&self, field_name: PipeFieldName) -> Option<PipeField> {
        match self.signal.get(&field_name) {
            Some(&v) => Some(v),
            None => None,
        }
    }

    pub fn get_state_vec(&self) -> Vec<(PipeFieldName, PipeField)> {
        self.signal.iter().map(|(k, v)| (*k, *v)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::Instruction;
    use super::*;

    macro_rules! assert_state_eq {
        ($controller: ident, $field_name: ident, $value: expr) => {{
            assert_eq!(
                $controller.get_state(PipeFieldName::$field_name).unwrap(),
                $value
            );
        }};
    }

    #[test]
    pub fn test_controller_state_update() {
        // TODO: Test ALL instructions in this way
        let mut controller = Controller::new();

        // add $t0, $t1, $t2
        let mut instr = Instruction::from_parts(
            OpCode::RType,
            Some(FuncCode::Add),
            Some(Register::T0),
            Some(Register::T1),
            Some(Register::T2),
            None,
            None,
            None,
        )
        .unwrap();

        controller.update_state(&instr);

        assert_state_eq!(controller, WriteReg, PipeField::Bool(true));
        assert_state_eq!(controller, WriteMem, PipeField::Bool(false));
        assert_state_eq!(controller, ReadMem, PipeField::Bool(false));
        assert_state_eq!(controller, AluOp, PipeField::Op(ALUOperation::ADD));
        assert_state_eq!(controller, AluToReg, PipeField::Bool(true));
        assert_state_eq!(controller, RegDest, PipeField::Dest(RegDest::Rd));
        assert_state_eq!(controller, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
        assert_state_eq!(controller, AluSrc2, PipeField::ALU(ALUSrc::Reg2));

        // addi $t0, $t1, 0xabcd
        instr = Instruction::from_parts(
            OpCode::Addi,
            None,
            Some(Register::T0),
            Some(Register::T1),
            None,
            None,
            Some(0xabcd),
            None,
        )
        .unwrap();

        controller.update_state(&instr);

        assert_state_eq!(controller, WriteReg, PipeField::Bool(true));
        assert_state_eq!(controller, WriteMem, PipeField::Bool(false));
        assert_state_eq!(controller, ReadMem, PipeField::Bool(false));
        assert_state_eq!(controller, AluOp, PipeField::Op(ALUOperation::ADD));
        assert_state_eq!(controller, AluToReg, PipeField::Bool(true));
        assert_state_eq!(controller, RegDest, PipeField::Dest(RegDest::Rt));
        assert_state_eq!(controller, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
        assert_state_eq!(controller, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

        // j 0x00ff_ffff
        instr = Instruction::from_parts(
            OpCode::J,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(0x00ff_ffff),
        )
        .unwrap();

        controller.update_state(&instr);

        assert_state_eq!(controller, WriteReg, PipeField::Bool(false));
        assert_state_eq!(controller, WriteMem, PipeField::Bool(false));
        assert_state_eq!(controller, ReadMem, PipeField::Bool(false));
        assert_state_eq!(controller, AluOp, PipeField::Op(ALUOperation::OR));
        assert_state_eq!(controller, AluSrc1, PipeField::ALU(ALUSrc::Zero));
        assert_state_eq!(controller, AluSrc2, PipeField::ALU(ALUSrc::Zero));
    }
}
