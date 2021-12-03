use super::common::{ALUOperation, ALUSrc, RegDest, RegSrc, Register};
use super::instruction::{FuncCode, Instruction, OpCode};
use super::pipe_reg::{PipeField, PipeFieldName};
use std::collections::HashMap;
use std::iter::Iterator;

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
}

impl Controller {
    pub fn new() -> Controller {
        let mut signal = HashMap::new();
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
                BranchType,
                JumpTarget,
                IsJump,
                TakeJump,
                MemSigned,
                Reg1Src,
                Reg2Src,
            ] {
                signal.insert(v, PipeField::XXX);
            }
        }

        Controller { signal }
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
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, ReadMem, false);

                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::Rt);

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
                            // NOTE:
                            // WriteReg signal only governs the general purpose
                            // registers
                            FuncCode::Div => {
                                set_signal_value!(self, MuldivReqValid, true);
                                set_signal_value!(self, WriteReg, false);
                                set_signal_value!(self, AluOp, ALUOperation::DIV);
                            }
                            FuncCode::Divu => {
                                set_signal_value!(self, MuldivReqValid, true);
                                set_signal_value!(self, WriteReg, false);
                                set_signal_value!(self, AluOp, ALUOperation::DIVU);
                            }
                            FuncCode::Mult => {
                                set_signal_value!(self, MuldivReqValid, true);
                                set_signal_value!(self, WriteReg, false);
                                set_signal_value!(self, AluOp, ALUOperation::MULT);
                            }
                            FuncCode::Multu => {
                                set_signal_value!(self, MuldivReqValid, true);
                                set_signal_value!(self, WriteReg, false);
                                set_signal_value!(self, AluOp, ALUOperation::MULTU);
                            }
                            FuncCode::Nor => {
                                set_signal_value!(self, AluOp, ALUOperation::NOR);
                            }
                            FuncCode::Or => {
                                set_signal_value!(self, AluOp, ALUOperation::OR);
                            }
                            FuncCode::Sll => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rt);
                                set_signal_value!(self, Reg2Src, RegSrc::XXX);

                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc2, ALUSrc::Shamt);
                                set_signal_value!(self, AluOp, ALUOperation::SLL);
                            }
                            FuncCode::Sllv => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rt);
                                set_signal_value!(self, Reg2Src, RegSrc::Rs);

                                set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                                set_signal_value!(self, AluSrc2, ALUSrc::Reg1);
                                set_signal_value!(self, AluOp, ALUOperation::SLL);
                            }
                            FuncCode::Sra => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rt);
                                set_signal_value!(self, Reg2Src, RegSrc::XXX);

                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc2, ALUSrc::Shamt);
                                set_signal_value!(self, AluOp, ALUOperation::SRA);
                            }
                            FuncCode::Srav => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rt);
                                set_signal_value!(self, Reg2Src, RegSrc::Rs);

                                set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                                set_signal_value!(self, AluSrc2, ALUSrc::Reg1);
                                set_signal_value!(self, AluOp, ALUOperation::SRA);
                            }
                            FuncCode::Srl => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rt);
                                set_signal_value!(self, Reg2Src, RegSrc::XXX);

                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc2, ALUSrc::Shamt);
                                set_signal_value!(self, AluOp, ALUOperation::SRL);
                            }
                            FuncCode::Srlv => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rt);
                                set_signal_value!(self, Reg2Src, RegSrc::Rs);

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
                                set_signal_value!(self, AluSrc1, ALUSrc::PcPlus4);
                                set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                set_signal_value!(self, RegDest, RegDest::Rd);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                                set_signal_value!(self, IsJump, true);
                            }
                            FuncCode::Jr => {
                                set_signal_value!(self, WriteReg, false);
                                set_signal_value!(self, AluToReg, false);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                                set_signal_value!(self, IsJump, true);
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
                                set_signal_value!(self, AluSrc1, ALUSrc::Muldivhi);
                                set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Mflo => {
                                set_signal_value!(self, AluSrc1, ALUSrc::Muldivlo);
                                set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                            }
                            FuncCode::Mthi => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rs);
                                set_signal_value!(self, Reg1Src, RegSrc::XXX);

                                set_signal_value!(self, RegDest, RegDest::MulDivHi);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc1, ALUSrc::Zero);
                            }
                            FuncCode::Mtlo => {
                                set_signal_value!(self, Reg1Src, RegSrc::Rs);
                                set_signal_value!(self, Reg1Src, RegSrc::XXX);

                                set_signal_value!(self, RegDest, RegDest::MulDivLo);
                                set_signal_value!(self, AluOp, ALUOperation::ADD);
                                set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                set_signal_value!(self, AluSrc1, ALUSrc::Zero);
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
                        None => {
                            panic!("Invalid instruction");
                        }
                    }
                }
                OpCode::Addi => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Addiu => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADDU);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Andi => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::AND);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Mul => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::Rt);
                    match instr.get_func_code() {
                        Some(func_code) => {
                            match func_code {
                                // Mul
                                FuncCode::Srl => {
                                    set_signal_value!(self, AluOp, ALUOperation::MUL);
                                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                                }
                                // Clo
                                FuncCode::Add => {
                                    set_signal_value!(self, AluOp, ALUOperation::CLO);
                                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                }
                                // Clz
                                FuncCode::Addu => {
                                    set_signal_value!(self, AluOp, ALUOperation::CLZ);
                                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                }

                                _ => unreachable!(),
                            }
                        }
                        None => unreachable!(),
                    }
                    set_signal_value!(self, AluOp, ALUOperation::MULT);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                }
                OpCode::Ori => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::OR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Xori => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::XOR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Lui => {
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::LUI);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Slti => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::SLT);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Sltiu => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::SLTU);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg2);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                }
                OpCode::Beq => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::Rt);

                    set_signal_value!(self, AluOp, ALUOperation::XOR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, ReadMem, false);
                }
                OpCode::Bgelt => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::Rt);

                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                    // TODO: handle the different cases here
                    match instr.get_rt() {
                        Some(rt) => {
                            match rt {
                                // TODO:
                                // BLTZ
                                Register::ZERO => {
                                    set_signal_value!(self, AluOp, ALUOperation::SLT);
                                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                    set_signal_value!(self, WriteReg, false);
                                    set_signal_value!(self, WriteMem, false);
                                    set_signal_value!(self, ReadMem, false);
                                }
                                // BLTZAL
                                // TODO
                                Register::S0 => {}
                                // BGEZAL
                                // TODO
                                Register::S1 => {}
                                // BGEZ
                                Register::AT => {
                                    // if leading zeros >= 1, then 0 or positive
                                    set_signal_value!(self, AluOp, ALUOperation::CLZ);
                                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                                    set_signal_value!(self, WriteReg, false);
                                    set_signal_value!(self, WriteMem, false);
                                    set_signal_value!(self, ReadMem, false);
                                }

                                _ => unreachable!(),
                            }
                        }
                        None => {}
                    }
                }
                OpCode::Bgtz => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, AluOp, ALUOperation::SLT);
                    set_signal_value!(self, AluSrc1, ALUSrc::Zero);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg1);
                }
                OpCode::Bne => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rs);
                    set_signal_value!(self, Reg2Src, RegSrc::Rt);

                    set_signal_value!(self, AluOp, ALUOperation::XOR);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::Reg2);
                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, WriteMem, false);
                    set_signal_value!(self, ReadMem, false);
                }
                OpCode::J => {
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, AluSrc1, ALUSrc::Zero);
                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                    set_signal_value!(self, AluOp, ALUOperation::OR);
                }
                OpCode::Jal => {
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluSrc1, ALUSrc::PcPlus4);
                    set_signal_value!(self, AluSrc2, ALUSrc::Zero);
                    set_signal_value!(self, AluOp, ALUOperation::OR);
                    set_signal_value!(self, RegDest, RegDest::Ra);
                }
                OpCode::Lb => {
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                OpCode::Lwl => {
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, AluToReg, false);
                }
                OpCode::Lwr => {
                    set_signal_value!(self, Reg1Src, RegSrc::XXX);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, ReadMem, true);
                    set_signal_value!(self, AluToReg, false);
                }
                OpCode::Ll => {
                    todo!();
                }
                OpCode::Sb => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rt);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                    set_signal_value!(self, Reg1Src, RegSrc::Rt);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

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
                    set_signal_value!(self, Reg1Src, RegSrc::Rt);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, RegDest, RegDest::Rt);
                    set_signal_value!(self, ReadMem, false);
                    set_signal_value!(self, WriteMem, true);
                    set_signal_value!(self, AluToReg, false);
                    set_signal_value!(self, MemWidth, 4);
                }
                OpCode::Swl => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rt);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, WriteMem, true);
                }
                OpCode::Swr => {
                    set_signal_value!(self, Reg1Src, RegSrc::Rt);
                    set_signal_value!(self, Reg2Src, RegSrc::XXX);

                    set_signal_value!(self, AluOp, ALUOperation::ADD);
                    set_signal_value!(self, WriteReg, false);
                    set_signal_value!(self, AluSrc1, ALUSrc::Reg1);
                    set_signal_value!(self, AluSrc2, ALUSrc::SignExtImm);
                    set_signal_value!(self, WriteMem, true);
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
            match $controller.get_state(PipeFieldName::$field_name) {
                Some(result) => match result {
                    PipeField::$field_name(v) => {
                        assert_eq!(v, $value);
                    }
                    _ => unreachable!(),
                },
                None => assert!(false),
            }
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

        assert_state_eq!(controller, WriteReg, true);
        assert_state_eq!(controller, WriteMem, false);
        assert_state_eq!(controller, ReadMem, false);
        assert_state_eq!(controller, AluOp, ALUOperation::ADD);
        assert_state_eq!(controller, AluToReg, true);
        assert_state_eq!(controller, RegDest, RegDest::Rd);
        assert_state_eq!(controller, AluSrc1, ALUSrc::Reg1);
        assert_state_eq!(controller, AluSrc2, ALUSrc::Reg2);

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

        assert_state_eq!(controller, WriteReg, true);
        assert_state_eq!(controller, WriteMem, false);
        assert_state_eq!(controller, ReadMem, false);
        assert_state_eq!(controller, AluOp, ALUOperation::ADD);
        assert_state_eq!(controller, AluToReg, true);
        assert_state_eq!(controller, RegDest, RegDest::Rd);
        assert_state_eq!(controller, AluSrc1, ALUSrc::Reg1);
        assert_state_eq!(controller, AluSrc2, ALUSrc::SignExtImm);

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

        assert_state_eq!(controller, WriteReg, false);
        assert_state_eq!(controller, WriteMem, false);
        assert_state_eq!(controller, ReadMem, false);
        assert_state_eq!(controller, AluOp, ALUOperation::OR);
        assert_state_eq!(controller, AluSrc1, ALUSrc::Zero);
        assert_state_eq!(controller, AluSrc2, ALUSrc::Zero);
    }
}
