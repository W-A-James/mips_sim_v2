use super::common::ALUOperation;
use std::error::Error;


#[derive(Debug)]
pub enum ALUError {
    OverflowError,
    DivideByZeroError
}

impl std::fmt::Display for ALUError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ALUError::OverflowError => {
                write!(f, "OverflowError!")
            }
            ALUError::DivideByZeroError => {
                write!(f, "DivideByZeroError!")
            }
        }
    }
}
impl Error for ALUError{}

// Compute absolute value of v
fn abs(v: u32) -> u32 {
    if v & 0x8000_0000 != 0 {
        complement_u32(v)
    } else {
        v
    }
}

pub fn complement_u32(v: u32) -> u32 {
    (!v).wrapping_add(1)
}
fn complement_u64(v: u64) -> u64 {
    (!v).wrapping_add(1)
}

pub fn calculate(a: u32, b: u32, op: ALUOperation) -> Result<u32, ALUError> {
    use ALUOperation::*;
    match op {
        ADD => {
            let a_i32 = a as i32;
            let b_i32 = b as i32;
            let result = a_i32.overflowing_add(b_i32);
            if result.1 {
                Err(ALUError::OverflowError)
            } else {
                Ok(result.0 as u32)
            }
        }
        ADDU => {
            let result = a.overflowing_add(b);
            if result.1 {
                Err(ALUError::OverflowError)
            } else {
                Ok(result.0)
            }
        },
        SUB => {
            let a_i32 = a as i32;
            let b_i32 = b as i32;
            let result = a_i32.overflowing_sub(b_i32);
            if result.1 {
                Err(ALUError::OverflowError)
            } else {
                Ok(result.0 as u32)
            }
        }
        SUBU => {
            let result = a.overflowing_sub(b);
            if result.1 {
                Err(ALUError::OverflowError)
            } else {
                Ok(result.0)
            }
        }
        MUL => {
            let result = a.overflowing_mul(b);
            if result.1 {
                Err(ALUError::OverflowError)
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

pub fn multiply(a: u32, b: u32, signed: bool) -> Result<u64, ALUError> {
    if signed {
        let abs_a = abs(a);
        let abs_b = abs(b);

        // if only one is negative, result is negative
        if (abs_a != a) ^ (abs_b != b) {
            Ok(complement_u64(abs_a as u64 * abs_b as u64))
        } else {
            Ok(abs_a as u64 * abs_b as u64)
        }
    } else {
        Ok(a as u64 * b as u64)
    }
}

pub fn divide(a: u32, b: u32, signed: bool) -> Result<u64, ALUError> {
    let div: u32;
    let rem: u32;

    if b == 0 {
        Err(ALUError::DivideByZeroError)
    } else {
        if signed {
            let abs_a = abs(a);
            let abs_b = abs(b);
            if (abs_a != a) ^ (abs_b != b) {
                div = complement_u32(abs_a / abs_b);
                rem = complement_u32(abs_a % abs_b);
            } else {
                div = abs_a / abs_b;
                rem = abs_a % abs_b;
            }
        } else {
            div = a / b;
            rem = a % b;
        }

        Ok((div as u64) | ((rem as u64) << 32))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub fn test_multiply() {
        let mut res: u64;
        res = multiply(1, 2, true).unwrap();
        assert_eq!(res, 2);
        res = multiply((-1i32) as u32, 2, true).unwrap();
        assert_eq!(res, -2i64 as u64);

        res = multiply((-1i32) as u32, 2, false).unwrap();
        assert_eq!(res, 0x0000_0001_FFFF_FFFE);

        res = multiply((-1i32) as u32, (-1i32) as u32, true).unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    pub fn test_divide() {
        let mut res: u64;
        res = divide(100, 50, true).unwrap();
        assert_eq!(res & 0x0000_0000_FFFF_FFFF, 2);

        res = divide(50, 100, true).unwrap();
        assert_eq!((res & 0xFFFF_FFFF_0000_0000) >> 32, 50);
    }

    #[test]
    pub fn test_add_overflow() {
        assert!(calculate(std::i32::MIN as u32, 0xffff_ffff, ALUOperation::ADD).is_err());
        assert!(calculate(0xffff_ffff, 1, ALUOperation::ADD).is_ok());
    }

    #[test]
    pub fn test_addu() {
        assert!(calculate(0xffff_ffff, 1, ALUOperation::ADDU).is_err());
    }

    #[test]
    pub fn test_sub_overflow() {
        assert!(calculate(std::i32::MIN as u32, 1, ALUOperation::SUB).is_err());
        assert!(calculate(1, 1, ALUOperation::SUB).is_ok());
    }

    #[test]
    pub fn test_subu() {
        assert!(calculate(0, 1, ALUOperation::SUBU).is_err());
    }

    #[test]
    pub fn test_xor() {
        assert_eq!(calculate(1, 1, ALUOperation::XOR).unwrap(), 0);
        assert_eq!(calculate(0, 1, ALUOperation::XOR).unwrap(), 1);
    }

    #[test]
    pub fn test_slt() {
        assert_eq!(calculate(10, 20, ALUOperation::SLT).unwrap(), 1);
        assert_eq!(calculate(-10i32 as u32, 20, ALUOperation::SLT).unwrap(), 1);
        assert_eq!(
            calculate(-10i32 as u32, -20i32 as u32, ALUOperation::SLT).unwrap(),
            0
        );
        assert_eq!(calculate(100, 10, ALUOperation::SLT).unwrap(), 0);
    }

    #[test]
    pub fn test_stlu() {
        assert_eq!(calculate(10, 20, ALUOperation::SLTU).unwrap(), 1);
        assert_eq!(calculate(-10i32 as u32, 20, ALUOperation::SLTU).unwrap(), 0);
        assert_eq!(
            calculate(-10i32 as u32, -20i32 as u32, ALUOperation::SLTU).unwrap(),
            0
        );
        assert_eq!(calculate(100, 10, ALUOperation::SLTU).unwrap(), 0);
    }

    #[test]
    pub fn test_lui() {
        assert_eq!(
            calculate(0x0000_ffff, 0, ALUOperation::LUI).unwrap(),
            0xffff_0000
        );
    }

    #[test]
    pub fn test_srl() {
        assert_eq!(
            calculate(0x0000_ffff, 4, ALUOperation::SRL).unwrap(),
            0x0000_0fff
        );
        assert_eq!(
            calculate(0xffff_ffff, 4, ALUOperation::SRL).unwrap(),
            0x0fff_ffff
        );
    }

    #[test]
    pub fn test_sll() {
        assert_eq!(
            calculate(0x0fff_ffff, 4, ALUOperation::SLL).unwrap(),
            0xffff_fff0
        );
    }

    #[test]
    pub fn test_sra() {}

    #[test]
    pub fn test_clo() {}

    #[test]
    pub fn test_clz() {}

    #[test]
    pub fn test_nor() {}

    #[test]
    pub fn test_and() {}

    #[test]
    pub fn test_or() {}
}
