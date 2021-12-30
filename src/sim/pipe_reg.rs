use super::common::{ALUOperation, ALUSrc, BranchType, RegDest, RegSrc};
use super::traits::{ClockedMap, Field, Value};
use paste::paste;
use std::collections::{HashMap, HashSet};

macro_rules! insert_pipe_value {
    ($item: expr, $field_name: ident, $value: expr) => {{
        $item.load(PipeFieldName::$field_name, PipeField::$field_name($value));
    }};
}

// This macro takes in the specification for the PipeField enum and generates
// that enum as well as the corresponding PipeFieldName macro
macro_rules! gen_field_and_name{
    ( $enum_name:ident {
        $(
            $value:ident $(: $type:tt)?
        ),*
    } ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum $enum_name {
            $(
                $value$(($type))?
            ),*
        }

        paste! {
            #[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
            pub enum [< $enum_name Name >] {
                $( $value ),*
            }
        }
    }
}

gen_field_and_name!(PipeField {
    PcPlus4: u32,
    Reg1: u32,
    Reg2: u32,
    Muldivhi: u32,
    Muldivlo: u32,
    MuldivReqValid: bool,
    SignExtImm: u32,
    Rt: u8,
    Rd: u8,
    Shamt: u8,
    JumpTarget: u32,
    WriteReg: bool,
    ReadMem: bool,
    WriteMem: bool,
    MemWidth: u8,
    MemSigned: bool,
    MemData: u32,
    AluSrc1: ALUSrc,
    AluSrc2: ALUSrc,
    ALURes: u32,
    AluToReg: bool,
    AluOp: ALUOperation,
    RegDest: RegDest,
    RegToWrite: u8,
    Halt: bool,
    IsNop: bool,
    IsBranch: bool,
    IsJump: bool,
    TakeJump: bool,
    BranchType: BranchType,
    BranchTarget: u32,
    BranchTaken: bool,
    InstructionPc: u32,
    Instruction: u32,
    InDelaySlot: bool,
    MuldivRes: u64,
    PC: u32,
    Status: u32,
    EPC: u32,
    BadVAddr: u32,
    Cause: u32,
    StallFetch: bool,
    StallDecode: bool,
    StallExecute: bool,
    StallMemory: bool,
    StallWriteback: bool,
    BubbleDecode: bool,
    BubbleExecute: bool,
    BubbleMemory: bool,
    BubbleWriteback: bool,
    SquashFetch: bool,
    SquashDecode: bool,
    SquashExecute: bool,
    SquashMemory: bool,
    SquashWriteback: bool,
    Reg1Src: RegSrc,
    Reg2Src: RegSrc,
    XXX
});

impl Value for PipeField {}
impl Field for PipeFieldName {}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
struct PipeVal {
    pub name: PipeFieldName,
    pub value: PipeField,
}

#[derive(Debug, Clone)]
pub struct PipeRegister {
    name: String,
    current_map: HashMap<PipeFieldName, PipeField>,
    write_buffer: HashSet<PipeVal>,
}

impl PipeRegister {
    pub fn new(name: &str, fields: Vec<PipeFieldName>) -> PipeRegister {
        let mut current_map: HashMap<PipeFieldName, PipeField> =
            HashMap::with_capacity(fields.len());
        for f in fields {
            current_map.insert(f, PipeField::XXX);
        }
        let write_buffer = HashSet::new();
        PipeRegister {
            name: String::from(name),
            current_map,
            write_buffer,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

impl ClockedMap<PipeFieldName, PipeField> for PipeRegister {
    fn load(&mut self, field: PipeFieldName, value: PipeField) {
        if self.current_map.contains_key(&field) {
            self.write_buffer.insert(PipeVal { name: field, value });
        } else {
            // NOTE:
            // Silently ignore here?
            eprintln!(
                "Invalid field '{:#?}' for pipe_register with name: {}",
                field, self.name
            );
        }
    }

    fn clock(&mut self) {
        for pipe_val in self.write_buffer.drain() {
            self.current_map
                .insert(pipe_val.name, pipe_val.value)
                .unwrap();
        }
    }

    fn read(&self, field: PipeFieldName) -> PipeField {
        *self.current_map.get(&field).unwrap()
    }
}
