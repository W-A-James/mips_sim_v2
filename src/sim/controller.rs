use super::common::{ALUOperation, ALUSrc, RegDest, Register};
use super::instruction::{FuncCode, Instruction, OpCode};
use super::pipe_reg::{PipeField, PipeFieldName};
use std::collections::HashMap;

macro_rules! set_signal_value {
    ($item: ident, $field_name: ident, $value: expr) => {{
        $item
            .signal
            .insert(PipeFieldName::$field_name, PipeField::$field_name($value));
    }};
}

#[derive(Debug)]
pub struct Controller {
    signal: HashMap<PipeFieldName, PipeField>,
    write_in_flight: HashMap<Register, bool>,
}

impl Controller {
    pub fn new() -> Controller {
        let mut signal = HashMap::new();
        let mut write_in_flight = HashMap::new();
        {
            use PipeFieldName::*;
            for v in vec![
                RegDest,
                AluSrc1,
                AluSrc2,
                AluOp,
                AluToReg,
                MuldivReqValid,
                WriteReg,
                InDelaySlot,
                ReadMem,
                WriteMem,
                MemWidth,
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
                Halt,
            ] {
                signal.insert(v, PipeField::XXX).unwrap();
            }
        }

        for r in Register::iter() {
            write_in_flight.insert(r, false).unwrap();
        }

        Controller {
            signal,
            write_in_flight,
        }
    }

    pub fn update_state(&mut self, instr: &Instruction) {
        if instr.is_nop() {
            set_signal_value!(self, RegDest, RegDest::XXX);
            set_signal_value!(self, AluSrc1, ALUSrc::Zero);
            set_signal_value!(self, AluSrc2, ALUSrc::Zero);
            set_signal_value!(self, AluOp, ALUOperation::ADD);
            set_signal_value!(self, MuldivReqValid, false);
            set_signal_value!(self, WriteReg, false);
            set_signal_value!(self, ReadMem, false);
            set_signal_value!(self, WriteMem, false);
        } else if instr.is_halt() {
            set_signal_value!(self, Halt, true);
        } else {
            match instr.get_op_code() {
                OpCode::RType => {
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                    set_signal_value!(self, RegDest, RegDest::Rd);
                    set_signal_value!(self, WriteReg, true);
                    set_signal_value!(self, AluToReg, true);

                    match instr.get_func_code() {
                        Some(func_code) => match func_code {
                            FuncCode::Add => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Addu => {
                                set_signal_value!(self, AluOp, ALUOperation::ADDU);
                            }
                            FuncCode::And => {
                                set_signal_value!(self, AluOp, ALUOperation::AND);
                            }
                            FuncCode::Div => {
                                set_signal_value!(self, AluOp, ALUOperation::DIV);
                            }
                            FuncCode::Divu => {
                                set_signal_value!(self, AluOp, ALUOperation::DIVU);
                            }
                            FuncCode::Mult => {
                                set_signal_value!(self, AluOp, ALUOperation::MULT);
                            }
                            FuncCode::Multu => {
                                set_signal_value!(self, AluOp, ALUOperation::MULTU);
                            }
                            FuncCode::Nor => {
                                set_signal_value!(self, AluOp, ALUOperation::NOR);
                            }
                            FuncCode::Or => {
                                set_signal_value!(self, AluOp, ALUOperation::OR);
                            }
                            FuncCode::Sll => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc2, ALUSrc::Shamt);
                                set_signal_value!(self, AluOp, ALUOperation::SLL);
                            }
                            FuncCode::Sllv => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                                set_signal_value!(self, AluSrc2, ALUSrc::Reg1);
                                set_signal_value!(self, AluOp, ALUOperation::SLL);
                            }
                            FuncCode::Sra => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc2, ALUSrc::Shamt);
                                set_signal_value!(self, AluOp, ALUOperation::SRA);
                            }
                            FuncCode::Srav => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                                set_signal_value!(self, AluSrc2, ALUSrc::Reg1);
                                set_signal_value!(self, AluOp, ALUOperation::SRA);
                            }
                            FuncCode::Srl => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc2, ALUSrc::Shamt);
                                set_signal_value!(self, AluOp, ALUOperation::SRL);
                            }
                            FuncCode::Srlv => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                                set_signal_value!(self, AluSrc2, ALUSrc::Reg1);
                                set_signal_value!(self, AluOp, ALUOperation::SRL);
                            }
                            FuncCode::Sub => {
                                set_signal_value!(self, AluOp, ALUOperation::SUB);
                            }
                            FuncCode::Subu => {
                                set_signal_value!(self, AluOp, ALUOperation::SUBU);
                            }
                            FuncCode::Xor => {
                                set_signal_value!(self, AluOp, ALUOperation::XOR);
                            }
                            FuncCode::Slt => {
                                set_signal_value!(self, AluOp, ALUOperation::SLT);
                            }
                            FuncCode::Sltu => {
                                set_signal_value!(self, AluOp, ALUOperation::SLTU);
                            }
                            FuncCode::Jalr => {
                                // TODO
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Jr => {
                                set_signal_value!(self, WriteReg, false);
                                set_signal_value!(self, AluToReg, false);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Tge => {
                                set_signal_value!(self, AluOp, ALUOperation::SUB);
                            }
                            FuncCode::Tgeu => {
                                set_signal_value!(self, AluOp, ALUOperation::SUBU);
                            }
                            FuncCode::Tlt => {
                                set_signal_value!(self, AluOp, ALUOperation::SUB);
                            }
                            FuncCode::Tltu => {
                                set_signal_value!(self, AluOp, ALUOperation::SUBU);
                            }
                            FuncCode::Mfhi => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Mflo => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Mthi => {
                                set_signal_value!(self, RegDest, RegDest::MulDivHi);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Mtlo => {
                                set_signal_value!(self, RegDest, RegDest::MulDivLo);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Movn => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Movz => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Syscall => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Break => {
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                        },
                        None => {}
                    }
                }
                OpCode::Addi => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Addiu => {
                    set_signal_value!(self, AluOp, ALUOperation::ADDU);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Andi => {
                    set_signal_value!(self, AluOp, ALUOperation::AND);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Mul => {
                    set_signal_value!(self, AluOp, ALUOperation::MULT);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                }
                OpCode::Ori => {
                    set_signal_value!(self, AluOp, ALUOperation::OR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Xori => {
                    set_signal_value!(self, AluOp, ALUOperation::XOR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Lui => {
                    set_signal_value!(self, AluOp, ALUOperation::LUI);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Slti => {
                    set_signal_value!(self, AluOp, ALUOperation::SLT);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Sltiu => {
                    set_signal_value!(self, AluOp, ALUOperation::SLTU);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Beq => {
                    set_signal_value!(self, AluOp, ALUOperation::XOR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, ReadMem, false);
                }
                OpCode::Bgelt => {}
                OpCode::Bgtz => {}
                OpCode::Bne => {
                    set_signal_value!(self, AluOp, ALUOperation::XOR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, ReadMem, false);
                }
                OpCode::J => {}
                OpCode::Jal => {}
                OpCode::Lb => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, WriteReg, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 1);
                    set_signal_value!(self, MemSigned, true);
                }
                OpCode::Lbu => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, WriteReg, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 1);
                    set_signal_value!(self, MemSigned, false);
                }
                OpCode::Lh => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, WriteReg, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 2);
                    set_signal_value!(self, MemSigned, true);
                }
                OpCode::Lhu => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, WriteReg, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 2);
                    set_signal_value!(self, MemSigned, false);
                }
                OpCode::Lw => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, WriteReg, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 4);
                    set_signal_value!(self, MemSigned, true);
                }
                OpCode::Lwl => {}
                OpCode::Lwr => {}
                OpCode::Ll => {}
                OpCode::Sb => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, false);
                    set_signal_value!(self, WriteMem, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 1);
                }
                OpCode::Sh => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, false);
                    set_signal_value!(self, WriteMem, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 2);
                }
                OpCode::Sw => {
                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, false);
                    set_signal_value!(self, WriteMem, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 4);
                }
                OpCode::Swl => {}
                OpCode::Swr => {}
                OpCode::Eret => {}
            }
        }
    }
}
