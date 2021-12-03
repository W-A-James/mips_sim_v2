use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::process::exit;
use std::io::{BufReader, BufWriter};
use std::io::prelude::*;

use regex::Regex;

const HELP_TEXT: &str = "
usage: ./compile_to_rout <source> -o <output_file>
       to write output to a named output file
or
       ./compile_to_rout <source>
       to write output to stdout";
fn fill_no_ops(code_map: &mut HashMap<i32, i32>, start_index: i32, end_index: i32) {
    let i = start_index;
    while i < end_index {
        code_map[i] = 0;
        i += 4;
    }
}

fn parse_file(file_name: &str) -> HashMap<i32, i32>{
    let disasm_regex = Regex::new(r"([0-9a-f]+):\s+([0-9a-f]{8})");
    let nop_seq_regex = Regex::new(r"\.\.\.");

    let mut rv = HashMap<i32, i32>::new();
    let mut last_valid_index = -1;
    let mut in_nop_seq = false;
    let mut file = File::open(file_name).unwrap();
    let mut contents = String::new();

    file.read_to_string(&contents);
    for line in contents.as_str().split('\n') {
        // Match  
        // if matches disasm_regex split out pc and instr
        //      if in_nop_seq: fill_no_ops
        //      last_valid_index = pc
        // if matches nop_seq_regex set in_nop_seq to true
    }

    rv
}

fn dump(code_map: &HashMap<i32, i32>, outfile: Option<&str>) {
    //for k, v in code_map.keys
}
pub fn main() {
    let mut out_flag_present = false;
    let mut out_file_present = false;
    let mut has_source = false;
    let mut i = 0;
    let args = env::args();

    for arg in args {
        match i {
            0 => {
                continue;
            }
            1 => {
                has_source = true;
            }
            2 => {
                if arg != "-o" {
                    println!(HELP_TEXT);
                    exit(1);
                } else {
                    out_flag_present = true;
                }
            }
            3 => {
                if out_flag_present {
                    out_file_present = true;
                } else {
                    println!(HELP_TEXT);
                    exit(1);
                }
            }
            _ => {
                println!(HELP_TEXT);
                exit(1);
            }
            i++;
        }
    }

    if !has_source || (out_flag_present && !out_file_present) {
        println!(HELP_TEXT);
        exit(1);
    }

    let output_map = parse_file(args[1]);
    if out_flag_present && out_file_present {
        dump(output_map, Some(args[3]));
    } else {
        dump(output_map, None)
    }
}
