use super::common::ALUOperation;
use std::error::Error;

#[derive(Debug)]
pub struct ALU;

pub trait ALUError: std::fmt::Debug {}

#[derive(Debug)]
pub struct OverflowError;
impl ALUError for OverflowError {}
impl std::fmt::Display for OverflowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "OverflowError")
    }
}

impl Error for OverflowError {}

#[derive(Debug)]
pub struct DivideByZeroError;
impl ALUError for DivideByZeroError {}
impl std::fmt::Display for DivideByZeroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DivideByZeroError")
    }
}
impl Error for DivideByZeroError {}

impl ALU {
    // Compute absolute value of v
    fn abs(v: u32) -> u32 {
        if v & 0x8000_0000 != 0 {
            ALU::complement_u32(v)
        } else {
            v
        }
    }

    fn complement_u32(v: u32) -> u32 {
        (!v).wrapping_add(1)
    }
    fn complement_u64(v: u64) -> u64 {
        (!v).wrapping_add(1)
    }

    pub fn calculate(a: u32, b: u32, op: ALUOperation) -> Result<u32, impl ALUError> {
        use ALUOperation::*;
        match op {
            ADD => {
                let result = a.overflowing_add(b);
                if result.1 {
                    Err(OverflowError {})
                } else {
                    Ok(result.0)
                }
            }
            ADDU => Ok(a.wrapping_add(b)),
            SUB => {
                let result = a.overflowing_sub(b);
                if result.1 {
                    Err(OverflowError {})
                } else {
                    Ok(result.0)
                }
            }
            SUBU => Ok(a.wrapping_sub(b)),
            MUL => {
                let result = a.overflowing_mul(b);
                if result.1 {
                    Err(OverflowError {})
                } else {
                    Ok(result.0)
                }
            }
            MULU => Ok(a.wrapping_mul(b)),
            MULT | MULTU | DIV | DIVU => Ok(0),
            XOR => Ok(a ^ b),
            NOR => Ok(!(a | b)),
            AND => Ok(a & b),
            OR => Ok(a | b),
            SLT => {
                if (a as i32) < (b as i32) {
                    Ok(1)
                } else {
                    Ok(0)
                }
            }
            SLTU => {
                if a < b {
                    Ok(1)
                } else {
                    Ok(0)
                }
            }

            LUI => Ok(a.wrapping_shl(16)),
            SRL => Ok(a.wrapping_shr(b)),
            SLL => Ok(a.wrapping_shl(b)),
            SRA => {
                let upper = a & 0x8000_0000;
                if upper == 0 {
                    Ok(a.wrapping_shr(b))
                } else {
                    let mut res = a.wrapping_shr(b);
                    for i in 0..b {
                        if i == 32 {
                            break;
                        }
                        res |= 0x8000_0000u32.wrapping_shr(i);
                    }
                    Ok(res)
                }
            }
            CLO => Ok(a.leading_ones()),
            CLZ => Ok(a.leading_zeros()),
        }
    }

    pub fn multiply(a: u32, b: u32, signed: bool) -> u64 {
        if signed {
            let abs_a = ALU::abs(a);
            let abs_b = ALU::abs(b);

            // if only one is negative, result is negative
            if (abs_a != a) ^ (abs_b != b) {
                ALU::complement_u64(abs_a as u64 * abs_b as u64)
            } else {
                abs_a as u64 * abs_b as u64
            }
        } else {
            a as u64 * b as u64
        }
    }

    pub fn divide(a: u32, b: u32, signed: bool) -> u64 {
        let div: u32;
        let rem: u32;

        if signed {
            let abs_a = ALU::abs(a);
            let abs_b = ALU::abs(b);
            if (abs_a != a) ^ (abs_b != b) {
                div = ALU::complement_u32(abs_a / abs_b);
                rem = ALU::complement_u32(abs_a % abs_b);
            } else {
                div = abs_a / abs_b;
                rem = abs_a % abs_b;
            }
        } else {
            div = a / b;
            rem = a % b;
        }

        (div as u64) | ((rem as u64) << 32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub fn test_multiply() {
        let mut res: u64;
        res = ALU::multiply(1, 2, true);
        assert_eq!(res, 2);
        res = ALU::multiply((-1i32) as u32, 2, true);
        assert_eq!(res, -2i64 as u64);

        res = ALU::multiply((-1i32) as u32, 2, false);
        assert_eq!(res, 0x0000_0001_FFFF_FFFE);

        res = ALU::multiply((-1i32) as u32, (-1i32) as u32, true);
        assert_eq!(res, 1);
    }

    #[test]
    pub fn test_divide() {
        let mut res: u64;
        res = ALU::divide(100, 50, true);
        assert_eq!(res & 0x0000_0000_FFFF_FFFF, 2);

        res = ALU::divide(50, 100, true);
        assert_eq!((res & 0xFFFF_FFFF_0000_0000) >> 32, 50);
    }

    #[test]
    pub fn test_add_overflow() {
        assert!(ALU::calculate(0xffff_ffff, 1, ALUOperation::ADD).is_err());
        assert!(ALU::calculate(0xffff_fffe, 1, ALUOperation::ADD).is_ok());
    }

    #[test]
    pub fn test_addu() {
        assert!(ALU::calculate(0xffff_ffff, 1, ALUOperation::ADDU).is_ok());
    }

    #[test]
    pub fn test_sub_overflow() {
        assert!(ALU::calculate(0, 1, ALUOperation::SUB).is_err());
        assert!(ALU::calculate(1, 1, ALUOperation::SUB).is_ok());
    }

    #[test]
    pub fn test_subu() {
        assert!(ALU::calculate(0, 1, ALUOperation::SUBU).is_ok());
    }

    #[test]
    pub fn test_xor() {
        assert_eq!(ALU::calculate(1, 1, ALUOperation::XOR).unwrap(), 0);
        assert_eq!(ALU::calculate(0, 1, ALUOperation::XOR).unwrap(), 1);
    }

    #[test]
    pub fn test_nor() {}

    #[test]
    pub fn test_and() {}

    #[test]
    pub fn test_or() {}

    #[test]
    pub fn test_slt() {
        assert_eq!(ALU::calculate(10, 20, ALUOperation::SLT).unwrap(), 1);
        assert_eq!(
            ALU::calculate(-10i32 as u32, 20, ALUOperation::SLT).unwrap(),
            1
        );
        assert_eq!(
            ALU::calculate(-10i32 as u32, -20i32 as u32, ALUOperation::SLT).unwrap(),
            0
        );
        assert_eq!(ALU::calculate(100, 10, ALUOperation::SLT).unwrap(), 0);
    }

    #[test]
    pub fn test_stlu() {
        assert_eq!(ALU::calculate(10, 20, ALUOperation::SLTU).unwrap(), 1);
        assert_eq!(
            ALU::calculate(-10i32 as u32, 20, ALUOperation::SLTU).unwrap(),
            0
        );
        assert_eq!(
            ALU::calculate(-10i32 as u32, -20i32 as u32, ALUOperation::SLTU).unwrap(),
            0
        );
        assert_eq!(ALU::calculate(100, 10, ALUOperation::SLTU).unwrap(), 0);
    }

    #[test]
    pub fn test_lui() {
        assert_eq!(
            ALU::calculate(0x0000_ffff, 0, ALUOperation::LUI).unwrap(),
            0xffff_0000
        );
    }

    #[test]
    pub fn test_srl() {
        assert_eq!(
            ALU::calculate(0x0000_ffff, 4, ALUOperation::SRL).unwrap(),
            0x0000_0fff
        );
        assert_eq!(
            ALU::calculate(0xffff_ffff, 4, ALUOperation::SRL).unwrap(),
            0x0fff_ffff
        );
    }

    #[test]
    pub fn test_sll() {}

    #[test]
    pub fn test_sra() {}

    #[test]
    pub fn test_clo() {}

    #[test]
    pub fn test_clz() {}
}
