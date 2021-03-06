use super::traits::Field;
use std::iter::Iterator;
use num_enum::TryFromPrimitive;
use wasm_bindgen::prelude::*;
use serde::Serialize;

pub const TEXT_START: u32 = 0x0040_0000;
pub const STACK_POINTER_INITIAL: u32 = 0x7fff_ffff;
pub const HALT_INSTRUCTION: u32 = 0xDEAD_BEEF;

#[wasm_bindgen]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum ALUSrc {
    Zero,
    Reg1,
    Reg2,
    Muldivhi,
    Muldivlo,
    PcPlus4,
    SignExtImm,
    ZeroExtImm,
    Shamt,
}

#[wasm_bindgen]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum ALUOperation {
    ADD,
    ADDU,
    SUB,
    SUBU,
    MUL,
    MULU,
    MULT,
    MULTU,
    DIV,
    DIVU,
    XOR,
    NOR,
    AND,
    OR,
    SLT,
    SLTU,
    LUI,
    SRL,
    SLL,
    SRA,
    CLO,
    CLZ,
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum RegDest {
    Rt,
    Rd,
    MulDivHi,
    MulDivLo,
    Ra,
    XXX,
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum BranchType {
    Beq,
    Bgez,
    Bgezal,
    Bgtz,
    Blez,
    Bltzal,
    Bltz,
    Bne,
    XXX,
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum JumpType {
    J,
    Jal,
    Jr,
    Jalr,
    XXX,
}

#[wasm_bindgen]
#[repr(u8)]
#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, TryFromPrimitive, Serialize)]
pub enum Register {
    ZERO, // $0
    AT,   // $1
    V0,   // $2
    V1,   // $3
    A0,   // $4
    A1,   // $5
    A2,   // $6
    A3,   // $7
    T0,   // $8
    T1,   // $9
    T2,   // $10
    T3,   // $11
    T4,   // $12
    T5,   // $13
    T6,   // $14
    T7,   // $15
    S0,   // $16
    S1,   // $17
    S2,   // $18
    S3,   // $19
    S4,   // $20
    S5,   // $21
    S6,   // $22
    S7,   // $23
    T8,   // $24
    T9,   // $25
    K0,   // $26
    K1,   // $27
    GP,   // $28
    SP,   // $29
    FP,   // $30
    RA,   // $31
    HI,
    LO,
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum RegSrc {
    Rt,
    Rs,
    XXX
}

impl Field for Register {}

impl Register {
    pub fn iter() -> Vec<Register> {
        use Register::*;
        [
            ZERO, AT, V0, V1, A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, S0, S1, S2, S3, S4,
            S5, S6, S7, T8, T9, K0, K1, GP, SP, FP, RA, HI, LO,
        ]
        .iter()
        .copied()
        .collect()
    }
}

impl Field for u32 {}
