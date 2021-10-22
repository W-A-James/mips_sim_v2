use super::traits::{ClockedMap, Value};
use std::collections::HashMap;

#[derive(Debug)]
pub struct InvalidMemoryAddressRequest;
impl std::fmt::Display for InvalidMemoryAddressRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InvalidMemoryAddressRequest")
    }
}

#[derive(Debug)]
pub struct Memory {
    map: HashMap<u32, u32>,
    write_buffer: Vec<(u32, u32)>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            map: HashMap::new(),
            write_buffer: Vec::new(),
        }
    }
}

impl Memory {
    pub fn get(&self, addr: u32) -> Result<u32, InvalidMemoryAddressRequest> {
        if addr % 4 == 0 {
            Ok(self.read(addr))
        } else {
            Err(InvalidMemoryAddressRequest{})
        }
    }
}

impl ClockedMap<u32, u32> for Memory {
    fn read(&self, field: u32) -> u32 {
        match self.map.get(&field) {
            Some(v) => *v,
            None => 0,
        }
    }

    fn clock(&mut self) {
        for (k, v) in self.write_buffer.drain(..) {
            let rem = k % 4;
            match rem {
                0 => {
                    self.map.insert(k, v);
                }
                1.. => {
                    let mut lower_bytes_mask: u32 = 0x00;
                    for _ in 0..rem {
                        lower_bytes_mask = (lower_bytes_mask << 8) | 0xFF;
                    }
                    let lower_bytes = (lower_bytes_mask & v) << (8 * (4 - rem));
                    let upper_bytes = ((!lower_bytes_mask) & v) >> (8 * rem);

                    // place 4 - rem upper bytes in lower bytes of field at k - rem
                    let val_at_lower_address = match self.map.get(&(k - rem)) {
                        Some(x) => {
                            let intermediate = *x & lower_bytes_mask;
                            intermediate | upper_bytes
                        }
                        None => upper_bytes,
                    };

                    self.map.insert(k - rem, val_at_lower_address);

                    // place last bytes at (k - rem) + 4
                    let val_at_higher_address = match self.map.get(&(k + 4 - rem)) {
                        Some(x) => {
                            let intermediate = *x & !lower_bytes_mask;
                            intermediate | lower_bytes
                        }
                        None => lower_bytes,
                    };

                    self.map.insert(k + 4 - rem, val_at_higher_address);
                }
            }
        }
    }

    fn load(&mut self, field: u32, value: u32) {
        self.write_buffer.push((field, value));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_aligned_writes() {
        let mut mem = Memory::new();

        mem.load(0, 0xdead_beef);
        mem.load(4, 0xbeef_beef);
        mem.load(8, 0xabcd_efff);
        mem.clock();

        assert_eq!(mem.read(0), 0xdead_beef);
        assert_eq!(mem.read(4), 0xbeef_beef);
        assert_eq!(mem.read(8), 0xabcd_efff);
    }

    #[test]
    pub fn test_unaligned_writes() {
        let mut mem = Memory::new();
        mem.load(1, 0xffff_ffff);
        mem.clock();
        assert_eq!(mem.read(0), 0x00ff_ffff);
        assert_eq!(mem.read(4), 0xff00_0000);

        mem = Memory::new();
        mem.load(2, 0xffff_ffff);
        mem.clock();
        assert_eq!(mem.read(0), 0x0000_ffff);
        assert_eq!(mem.read(4), 0xffff_0000);

        mem = Memory::new();
        mem.load(3, 0xffff_ffff);
        mem.clock();

        assert_eq!(mem.read(0), 0x0000_00ff);
        assert_eq!(mem.read(4), 0xffff_ff00);
    }

    #[test]
    pub fn test_aligned_reads() {
        let mut mem = Memory::new();

        mem.load(0, 0xabcd_abcd);
        mem.load(4, 0xffff_ffff);
        mem.load(8, 0xaaaa_ffff);
        mem.load(12, 0x1234_1234);

        mem.clock();

        assert_eq!(mem.read(0), 0xabcd_abcd);
        assert_eq!(mem.read(4), 0xffff_ffff);
        assert_eq!(mem.read(8), 0xaaaa_ffff);
        assert_eq!(mem.read(12), 0x1234_1234);
    }

    #[test]
    pub fn test_unaligned_reads() {
        let mut mem = Memory::new();

        mem.load(0, 0x1234_1234);
        mem.load(4, 0xabcd_abcd);
        mem.clock();

        assert!(mem.get(1).is_err());
    }
}
