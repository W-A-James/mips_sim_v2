use super::common::Register;
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::error::Error;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    RType = 0,
    Addi = 8,
    Addiu = 9,
    Andi = 0xc,
    Mul = 0x1c,
    Ori = 0xd,
    Xori = 0xe,
    Lui = 0xf,
    Slti = 0xa,
    Sltiu = 0xb,
    Beq = 0x4,
    Bgelt = 0x1,
    Bgtz = 0x7,
    Bne = 0x5,
    J = 0x2,
    Jal = 0x3,
    Lb = 0x20,
    Lbu = 0x24,
    Lh = 0x21,
    Lhu = 0x25,
    Lw = 0x23,
    Lwl = 0x22,
    Lwr = 0x26,
    Ll = 0x30,
    Sb = 0x28,
    Sh = 0x29,
    Sw = 0x2b,
    Swl = 0x2a,
    Swr = 0x2e,
    Eret = 0x10,
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum FuncCode {
    Add = 0x20,
    Addu = 0x21,
    And = 0x24,
    Div = 0x1a,
    Divu = 0x1b,
    Mult = 0x18,
    Multu = 0x19,
    Nor = 0x27,
    Or = 0x25,
    Sll = 0x00,
    Sllv = 0x04,
    Sra = 0x03,
    Srav = 0x07,
    Srl = 0x02,
    Srlv = 0x06,
    Sub = 0x22,
    Subu = 0x23,
    Xor = 0x26,
    Slt = 0x2a,
    Sltu = 0x2b,
    Jalr = 0x09,
    Jr = 0x08,
    Tge = 0x30,
    Tgeu = 0x31,
    Tlt = 0x32,
    Tltu = 0x33,
    Mfhi = 0x10,
    Mflo = 0x12,
    Mthi = 0x11,
    Mtlo = 0x13,
    Movn = 0x0b,
    Movz = 0x0a,
    Syscall = 0x0c,
    Break = 0xd,
}

pub trait InstructionError: std::fmt::Debug {}
#[derive(Debug)]
pub struct InvalidInstructionError;
impl std::fmt::Display for InvalidInstructionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InvalidInstructionError")
    }
}

pub struct Instruction {
    rs: Option<Register>,
    rt: Option<Register>,
    rd: Option<Register>,
    op_code: OpCode,
    func_code: Option<FuncCode>,
    shamt: Option<u8>,
    imm: Option<u16>,
    address: Option<u32>,
    instr_word: u32,
}

fn extract_rs(instruction_word: u32) -> u8 {
    ((instruction_word & 0x03E0_0000) >> 21) as u8
}

fn extract_rt(instruction_word: u32) -> u8 {
    ((instruction_word & 0x001f_0000) >> 16) as u8
}

fn extract_rd(instruction_word: u32) -> u8 {
    ((instruction_word & 0x0000_F800) >> 11) as u8
}

impl Instruction {
    pub fn new(instruction_word: u32) -> Result<Instruction, InvalidInstructionError> {
        let op_code: u8 = ((instruction_word & 0x3f00_0000) >> 24) as u8;
        let op_code = OpCode::try_from(op_code);
        let registers: Vec<Register> = Register::iter().collect();
        match op_code {
            Ok(op) => match op {
                RType => {
                    let func_code = (instruction_word & 0x0000_003f) as u8;
                    let func_code = FuncCode::try_from(func_code);

                    match func_code {
                        Ok(func_code) => match func_code {
                            FuncCode::Add => {}
                            FuncCode::Addu => {}
                            FuncCode::And => {}
                            FuncCode::Div => {}
                            FuncCode::Divu => {}
                            FuncCode::Mult => {}
                            FuncCode::Multu => {}
                            FuncCode::Nor => {}
                            FuncCode::Or => {}
                            FuncCode::Sll => {}
                            FuncCode::Sllv => {}
                            FuncCode::Sra => {}
                            FuncCode::Srav => {}
                            FuncCode::Srl => {}
                            FuncCode::Srlv => {}
                            FuncCode::Sub => {}
                            FuncCode::Subu => {}
                            FuncCode::Xor => {}
                            FuncCode::Slt => {}
                            FuncCode::Sltu => {}
                            FuncCode::Jalr => {}
                            FuncCode::Jr => {}
                            FuncCode::Tge => {}
                            FuncCode::Tgeu => {}
                            FuncCode::Tlt => {}
                            FuncCode::Tltu => {}
                            FuncCode::Mfhi => {}
                            FuncCode::Mflo => {}
                            FuncCode::Mthi => {}
                            FuncCode::Mtlo => {}
                            FuncCode::Movn => {}
                            FuncCode::Movz => {}
                            FuncCode::Syscall => {}
                            FuncCode::Break => {}
                        },
                        Err(_) => Err(InvalidInstructionError {}),
                    }
                }
                OpCode::Addi => {}
                OpCode::Addiu => {}
                OpCode::Andi => {}
                OpCode::Mul => {}
                OpCode::Ori => {}
                OpCode::Xori => {}
                OpCode::Lui => {}
                OpCode::Slti => {}
                OpCode::Sltiu => {}
                OpCode::Beq => {}
                OpCode::Bgelt => {}
                OpCode::Bgtz => {}
                OpCode::Bne => {}
                OpCode::J => {}
                OpCode::Jal => {}
                OpCode::Lb => {}
                OpCode::Lbu => {}
                OpCode::Lh => {}
                OpCode::Lhu => {}
                OpCode::Lw => {}
                OpCode::Lwl => {}
                OpCode::Lwr => {}
                OpCode::Ll => {}
                OpCode::Sb => {}
                OpCode::Sh => {}
                OpCode::Sw => {}
                OpCode::Swl => {}
                OpCode::Swr => {}
                OpCode::Eret => {}
            },
            Err(_) => Err(InvalidInstructionError {}),
        }
    }
    pub fn get_rs(&self) -> Option<Register> {
        self.rs
    }
    pub fn get_rt(&self) -> Option<Register> {
        self.rt
    }
    pub fn get_rd(&self) -> Option<Register> {
        self.rd
    }
    pub fn get_op_code(&self) -> OpCode {
        self.op_code
    }
    pub fn get_func_code(&self) -> Option<FuncCode> {
        self.func_code
    }
    pub fn get_shamt(&self) -> Option<u8> {
        self.shamt
    }
    pub fn get_imm(&self) -> Option<u16> {
        self.imm
    }
    pub fn get_address(&self) -> Option<u32> {
        self.address
    }
    pub fn get_instr_word(&self) -> u32 {
        self.instr_word
    }
}
