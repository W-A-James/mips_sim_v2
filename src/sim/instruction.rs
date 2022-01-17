use super::common::Register;
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;

use super::common::HALT_INSTRUCTION;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    RType = 0,
    Addi = 0x8,
    Addiu = 0x9,
    Andi = 0xc,
    Mul = 0x1c,
    Ori = 0xd,
    Xori = 0xe,
    Lui = 0xf,
    Slti = 0xa,
    Sltiu = 0xb,
    Beq = 0x4,
    Blez = 0x6,
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

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Clone, Copy)]
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

pub const OP_CODE_OFFSET_BITS: u32 = 26;
pub const RT_OFFSET_BITS: u32 = 16;
pub const RS_OFFSET_BITS: u32 = 21;
pub const RD_OFFSET_BITS: u32 = 11;
pub const SHAMT_OFFSET_BITS: u32 = 6;

#[derive(Clone, Copy, Debug)]
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

fn extract_op_code(instruction_word: u32) -> u8 {
    ((instruction_word & 0xFC00_0000) >> OP_CODE_OFFSET_BITS) as u8
}
fn extract_rs(instruction_word: u32) -> u8 {
    ((instruction_word & 0x03E0_0000) >> RS_OFFSET_BITS) as u8
}

fn extract_rt(instruction_word: u32) -> u8 {
    ((instruction_word & 0x001F_0000) >> RT_OFFSET_BITS) as u8
}

fn extract_rd(instruction_word: u32) -> u8 {
    ((instruction_word & 0x0000_F800) >> RD_OFFSET_BITS) as u8
}

fn extract_shamt(instruction_word: u32) -> u8 {
    ((instruction_word & 0x0000_07C0) >> SHAMT_OFFSET_BITS) as u8
}

fn extract_imm(instruction_word: u32) -> u16 {
    (instruction_word & 0x0000_FFFF) as u16
}

fn extract_address(instruction_word: u32) -> u32 {
    instruction_word & 0x03FF_FFFF
}

impl Instruction {
    pub fn new(instruction_word: u32) -> Result<Instruction, InvalidInstructionError> {
        let op_code: u8 = extract_op_code(instruction_word);
        let op_code = OpCode::try_from(op_code);
        let registers: Vec<Register> = Register::iter().collect();
        let mut rs: Option<Register> = Some(registers[extract_rs(instruction_word) as usize]);
        let mut rt: Option<Register> = Some(registers[extract_rt(instruction_word) as usize]);
        let mut rd: Option<Register> = Some(registers[extract_rd(instruction_word) as usize]);
        let mut shamt: Option<u8> = Some(extract_shamt(instruction_word));
        let mut imm: Option<u16> = Some(extract_imm(instruction_word));

        if instruction_word == 0 || instruction_word == HALT_INSTRUCTION {
            Ok(Instruction {
                rs: None,
                rt: None,
                rd: None,
                imm: None,
                shamt: None,
                address: None,
                func_code: None,
                instr_word: instruction_word,
                op_code: OpCode::RType,
            })
        } else {
            match op_code {
                Ok(op) => match op {
                    OpCode::RType => {
                        let func_code = (instruction_word & 0x0000_003f) as u8;
                        let func_code = FuncCode::try_from(func_code);
                        imm = None;

                        let mut errored = false;
                        match func_code {
                            Ok(func) => match func {
                                FuncCode::Add
                                | FuncCode::Addu
                                | FuncCode::And
                                | FuncCode::Div
                                | FuncCode::Divu
                                | FuncCode::Mult
                                | FuncCode::Multu
                                | FuncCode::Nor
                                | FuncCode::Or
                                | FuncCode::Sub
                                | FuncCode::Subu
                                | FuncCode::Xor
                                | FuncCode::Sllv
                                | FuncCode::Srav
                                | FuncCode::Srlv
                                | FuncCode::Slt
                                | FuncCode::Sltu => {
                                    shamt = None;
                                }
                                FuncCode::Sll | FuncCode::Sra | FuncCode::Srl => {}
                                FuncCode::Jalr => {
                                    shamt = None;
                                    rt = None;
                                }
                                FuncCode::Jr => {
                                    shamt = None;
                                    rt = None;
                                    rd = None;
                                }
                                FuncCode::Tge | FuncCode::Tgeu | FuncCode::Tlt | FuncCode::Tltu => {
                                    rd = None;
                                    shamt = None;
                                }
                                FuncCode::Mfhi | FuncCode::Mflo => {
                                    rs = None;
                                    rt = None;
                                    shamt = None;
                                }
                                FuncCode::Mthi | FuncCode::Mtlo => {
                                    rt = None;
                                    rd = None;
                                    shamt = None;
                                }
                                FuncCode::Movn | FuncCode::Movz => {
                                    shamt = None;
                                }
                                FuncCode::Syscall => {}
                                FuncCode::Break => {}
                            },
                            Err(_) => {
                                errored = true;
                            }
                        };
                        if errored {
                            Err(InvalidInstructionError {})
                        } else {
                            Ok(Instruction {
                                rs,
                                rt,
                                rd,
                                op_code: op,
                                func_code: Some(func_code.clone().unwrap()),
                                shamt,
                                imm,
                                address: None,
                                instr_word: instruction_word,
                            })
                        }
                    }
                    OpCode::Addi
                    | OpCode::Addiu
                    | OpCode::Andi
                    | OpCode::Ori
                    | OpCode::Xori
                    | OpCode::Lui
                    | OpCode::Slti
                    | OpCode::Sltiu
                    | OpCode::Lb
                    | OpCode::Lbu
                    | OpCode::Lh
                    | OpCode::Lhu
                    | OpCode::Lw
                    | OpCode::Lwl
                    | OpCode::Lwr
                    | OpCode::Sw
                    | OpCode::Sh
                    | OpCode::Sb
                    | OpCode::Swl
                    | OpCode::Swr
                    | OpCode::Ll
                    | OpCode::Beq
                    | OpCode::Bne
                    | OpCode::Bgelt
                    | OpCode::Bgtz
                    | OpCode::Blez => {
                        Ok(Instruction {
                            rs,
                            rt,
                            rd: None,
                            op_code: op,
                            func_code: None,
                            shamt: None,
                            imm,
                            address: None,
                            instr_word: instruction_word,
                        })
                    }
                    OpCode::Mul => {
                        let func_code = (instruction_word & 0x0000_003f) as u8;
                        let func_code = FuncCode::try_from(func_code);

                        Ok(Instruction {
                            rs,
                            rt,
                            rd,
                            op_code: op,
                            func_code: Some(func_code.unwrap()),
                            shamt: None,
                            imm: None,
                            address: None,
                            instr_word: instruction_word,
                        })
                    }
                    OpCode::J | OpCode::Jal | OpCode::Eret => {
                        let address = Some(extract_address(instruction_word));

                        Ok(Instruction {
                            rs: None,
                            rt: None,
                            rd: None,
                            op_code: op,
                            func_code: None,
                            shamt: None,
                            imm: None,
                            address,
                            instr_word: instruction_word,
                        })
                    }
                },
                Err(_) => Err(InvalidInstructionError {}),
            }
        }
    }

    pub fn from_parts(
        op_code: OpCode,
        func_code: Option<FuncCode>,
        rt: Option<Register>,
        rs: Option<Register>,
        rd: Option<Register>,
        shamt: Option<u8>,
        imm: Option<u16>,
        address: Option<u32>,
    ) -> Result<Instruction, InvalidInstructionError> {
        let mut instruction_word: u32 = (op_code as u32) << OP_CODE_OFFSET_BITS;
        match func_code {
            Some(func_code) => {
                instruction_word |= func_code as u32;
            }
            None => {}
        }
        match rt {
            Some(rt) => {
                instruction_word |= (rt as u32) << RT_OFFSET_BITS;
            }
            None => {}
        }
        match rs {
            Some(rs) => {
                instruction_word |= (rs as u32) << RS_OFFSET_BITS;
            }
            None => {}
        }
        match rd {
            Some(rd) => {
                instruction_word |= (rd as u32) << RD_OFFSET_BITS;
            }
            None => {}
        }
        match shamt {
            Some(shamt) => {
                instruction_word |= (shamt as u32) << SHAMT_OFFSET_BITS;
            }
            None => {}
        }
        match imm {
            Some(imm) => {
                instruction_word |= imm as u32;
            }
            None => {}
        }
        match address {
            Some(address) => {
                instruction_word |= address;
            }
            None => {}
        }

        Instruction::new(instruction_word)
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

    pub fn is_halt(&self) -> bool {
        self.instr_word == HALT_INSTRUCTION
    }

    pub fn is_nop(&self) -> bool {
        self.instr_word == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    pub fn test_constructor_with_invalid_op_code() {
        Instruction::new(0xdead_dead).unwrap();
    }

    #[test]
    #[should_panic]
    pub fn test_constructor_with_invalid_func_code() {
        Instruction::new(0x0000_00ff).unwrap();
    }

    #[test]
    pub fn test_constructor_for_r_types() {
        let instr = Instruction::new(0x0300_0820);
        assert!(instr.is_ok());

        let instr = instr.unwrap();
        assert_eq!(instr.get_op_code(), OpCode::RType);

        eprintln!("{:#?}", instr.get_rs().unwrap());
        assert_eq!(instr.get_rs().unwrap(), Register::T8);

        assert_eq!(instr.get_rt().unwrap(), Register::ZERO);

        assert_eq!(instr.get_rd().unwrap(), Register::AT);

        assert_eq!(instr.get_func_code().unwrap(), FuncCode::Add);
    }

    #[test]
    pub fn test_constructor_for_i_types() {
        let instr = Instruction::new(0x2380_0800);
        assert!(instr.is_ok());

        let instr = instr.unwrap();
        assert_eq!(instr.get_func_code(), None);
        assert_eq!(instr.get_op_code(), OpCode::Addi);
        assert_eq!(instr.get_rs(), Some(Register::GP));
        assert_eq!(instr.get_rt(), Some(Register::ZERO));
        assert_eq!(instr.get_rd(), None);
        assert_eq!(instr.get_imm(), Some(0x0800));
    }

    #[test]
    pub fn test_constructor_for_j_types() {
        let instr = Instruction::new(0x0800_ffff);
        assert!(instr.is_ok());
        let instr = instr.unwrap();

        assert_eq!(instr.get_op_code(), OpCode::J);
        assert_eq!(instr.get_address(), Some(0x0000_FFFF));

        let instr = Instruction::new(0x0CFF_FFFF);
        let instr = instr.unwrap();

        assert_eq!(instr.get_op_code(), OpCode::Jal);
        assert_eq!(instr.get_address(), Some(0x00FF_FFFF));
    }

    #[test]
    pub fn test_constructor_with_nop() {
        let nop = Instruction::new(0).unwrap();
        assert!(nop.is_nop());
    }

    #[test]
    pub fn test_constructor_with_halt() {
        let halt = Instruction::new(HALT_INSTRUCTION).unwrap();
        assert!(halt.is_halt());
    }
}
