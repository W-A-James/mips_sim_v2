use super::common::{ALUOperation, ALUSrc, RegDest, RegSrc, Register};
use super::instruction::{FuncCode, Instruction, OpCode};
use super::pipe_reg::{PipeField, PipeFieldName, DEFAULT_VALUES};
use std::collections::HashMap;
use std::iter::Iterator;

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
                BranchType,
                JumpTarget,
                IsBranch,
                IsJump,
                TakeJump,
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
        } else if instr.is_halt() {
            set_signal_value!(self, Halt, PipeField::Bool(true));
        } else {
            match instr.get_op_code() {
                OpCode::RType => {
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));

                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    match instr.get_func_code() {
                        Some(func_code) => match func_code {
                            FuncCode::Add => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                            }
                            FuncCode::Addu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADDU));
                            }
                            FuncCode::And => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::AND));
                            }
                            // NOTE:
                            // WriteReg signal only governs the general purpose
                            // registers
                            FuncCode::Div => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::DIV));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                            }
                            FuncCode::Divu => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::DIVU));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                            }
                            FuncCode::Mult => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::MULT));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                            }
                            FuncCode::Multu => {
                                set_signal_value!(self, MuldivReqValid, PipeField::Bool(true));
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::MULTU));

                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::XXX));
                            }
                            FuncCode::Nor => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::NOR));
                            }
                            FuncCode::Or => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));
                            }
                            FuncCode::Sll => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Shamt));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLL));
                            }
                            FuncCode::Sllv => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rs));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLL));
                            }
                            FuncCode::Sra => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Shamt));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRA));
                            }
                            FuncCode::Srav => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rs));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRA));
                            }
                            FuncCode::Srl => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Shamt));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRL));
                            }
                            FuncCode::Srlv => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                                set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rs));

                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SRL));
                            }
                            FuncCode::Sub => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUB));
                            }
                            FuncCode::Subu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SUBU));
                            }
                            FuncCode::Xor => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::XOR));
                            }
                            FuncCode::Slt => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLT));
                            }
                            FuncCode::Sltu => {
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLTU));
                            }
                            FuncCode::Jalr => {
                                // TODO
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::PcPlus4));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rd));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, IsJump, PipeField::Bool(true));
                            }
                            FuncCode::Jr => {
                                set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                set_signal_value!(self, AluToReg, PipeField::Bool(false));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, IsJump, PipeField::Bool(true));
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
                            }
                            FuncCode::Mflo => {
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Muldivlo));
                                set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                            }
                            FuncCode::Mthi => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(
                                    self,
                                    RegDest,
                                    PipeField::Dest(RegDest::MulDivHi)
                                );
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                            }
                            FuncCode::Mtlo => {
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                                set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));

                                set_signal_value!(
                                    self,
                                    RegDest,
                                    PipeField::Dest(RegDest::MulDivLo)
                                );
                                set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
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
                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                }
                OpCode::Addiu => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADDU));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                }
                OpCode::Andi => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::AND));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
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
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                }
                OpCode::Xori => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::XOR));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));

                    set_signal_value!(self, WriteReg, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(true));
                }
                OpCode::Lui => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::LUI));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                }
                OpCode::Slti => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLT));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                }
                OpCode::Sltiu => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::SLTU));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                }
                OpCode::Beq => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::XOR));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
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
                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::SLT)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
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
                                    set_signal_value!(
                                        self,
                                        AluOp,
                                        PipeField::Op(ALUOperation::CLZ)
                                    );
                                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
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
                }
                OpCode::Bne => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rs));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::Rt));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::XOR));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Reg2));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(false));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                }
                OpCode::J => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));
                }
                OpCode::Jal => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::XXX));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::PcPlus4));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::Zero));
                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::OR));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Ra));
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
                    set_signal_value!(self, MemWidth, PipeField::Byte(1));
                    set_signal_value!(self, MemSigned, PipeField::Bool(true));
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
                    set_signal_value!(self, MemWidth, PipeField::Byte(1));
                    set_signal_value!(self, MemSigned, PipeField::Bool(false));
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
                    set_signal_value!(self, MemWidth, PipeField::Byte(2));
                    set_signal_value!(self, MemSigned, PipeField::Bool(true));
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
                    set_signal_value!(self, MemWidth, PipeField::Byte(2));
                    set_signal_value!(self, MemSigned, PipeField::Bool(false));
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
                    set_signal_value!(self, MemWidth, PipeField::Byte(4));
                    set_signal_value!(self, MemSigned, PipeField::Bool(true));
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
                }
                OpCode::Ll => {
                    todo!();
                }
                OpCode::Sb => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::Byte(1));
                }
                OpCode::Sh => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::Byte(2));
                }
                OpCode::Sw => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, RegDest, PipeField::Dest(RegDest::Rt));
                    set_signal_value!(self, ReadMem, PipeField::Bool(false));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                    set_signal_value!(self, AluToReg, PipeField::Bool(false));
                    set_signal_value!(self, MemWidth, PipeField::Byte(4));
                }
                OpCode::Swl => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
                }
                OpCode::Swr => {
                    set_signal_value!(self, Reg1Src, PipeField::RSrc(RegSrc::Rt));
                    set_signal_value!(self, Reg2Src, PipeField::RSrc(RegSrc::XXX));

                    set_signal_value!(self, AluOp, PipeField::Op(ALUOperation::ADD));
                    set_signal_value!(self, WriteReg, PipeField::Bool(false));
                    set_signal_value!(self, AluSrc1, PipeField::ALU(ALUSrc::Reg1));
                    set_signal_value!(self, AluSrc2, PipeField::ALU(ALUSrc::SignExtImm));
                    set_signal_value!(self, WriteMem, PipeField::Bool(true));
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
        assert_state_eq!(controller, RegDest, PipeField::Dest(RegDest::Rd));
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
