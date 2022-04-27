extern crate mips_sim;
extern crate web_sys;

mod utils {
    use mips_sim::sim;
    use mips_sim::sim::common::*;
    use mips_sim::sim::traits::*;
    use std::fs::File;
    use std::io::prelude::*;
    use std::io::BufReader;
    pub const ASSEMBLY_TESTS_ROOT: &str = "tests/assembly_tests/";
    pub fn load(path: &str, is_instr: bool) -> (Vec<u32>, u32) {
        let file = File::open(path).unwrap();
        let mut buf_reader = BufReader::new(file);
        let mut contents = String::new();

        buf_reader.read_to_string(&mut contents).unwrap();

        let mut lines = contents.lines();

        let entry = 0x40_0000;
        /*let entry = match lines.next() { Some(s) => s.parse::<u32>().unwrap(),
            _ => 0x40_0000
        };*/
        let len = match lines.next() {
            Some(s) => s.parse::<usize>().unwrap(),
            _ => 0,
        };

        let mut v: Vec<u32> = Vec::with_capacity(len + 1);
        for l in lines {
            v.push(l.parse::<u32>().unwrap());
        }
        if is_instr {
            v.push(mips_sim::sim::common::HALT_INSTRUCTION);
        }

        (v, entry)
    }

    pub fn load_sample_program(sim: &mut sim::Sim, program_path: &str, data_path: &str) {
        let (instrs, entry) = load(program_path, true);
        let (data, _) = load(data_path, false);

        sim.load_binary(&instrs, &data, entry);
    }

    pub fn run_assembly_test(test_name: &str, check_list: Vec<(Register, u32)>) {
        let mut sim = sim::Sim::new();
        let instr_path = &format!("{}{}.rout", ASSEMBLY_TESTS_ROOT, test_name);
        let data_path = &format!("{}{}.rdata", ASSEMBLY_TESTS_ROOT, test_name);

        load_sample_program(&mut sim, instr_path, data_path);

        sim.step_to_halt();

        let sim_state = sim.get_state();
        eprintln!("Registers:\n{:#?}", sim_state.reg_file);
        eprintln!("Memory:\n{:#?}", sim_state.memory);
        for (r, v) in check_list {
            assert_eq!(
                sim_state.reg_file.read(r),
                v,
                "Failed on test: {} checking register: {:#?}",
                test_name,
                r
            );
        }
    }
}
#[cfg(test)]
mod tests {
    use mips_sim::sim::common::*;
    use paste::paste;

    use super::utils::*;

    macro_rules! build_test {
        ($name: ident, $check_list: expr) => {
            paste! {
            #[test]
            pub fn [<test_ $name>]() {
                run_assembly_test(stringify!($name), $check_list);
            }}
        };
        ($name: ident, $check_list: expr, ignore) => {
            paste! {
            #[test]
            #[ignore]
            pub fn [<test_ $name>]() {
                run_assembly_test(stringify!($name), $check_list);
            }}
        };
    }

    use Register::*;

    build_test! {addi, vec![(V0, 5), (V1, 4)]}
    build_test! {addiu, vec![(V0, 5), (V1, 5)]}
    build_test! {add, vec![(T0, 70), (T1, 130), (T3, 0xFFFF_0041), (T4, 0xFFFF_0005)]}
    build_test! {and, vec![(T2, 0), (T3, 0), (T4, 0), (T5, 1)]}
    build_test! {andi, vec![(T1, 0x8000_0000), (T2, 0), (T3, 0), (T4, 0), (T5, 1), (T6, 0x0000_0000)]}
    // NOTE:
    // Ignoring CLO and CLZ since these two are not supported in the R2000 architecture (according
    // to the compiler)
    build_test! {clo, vec![(T0, 32), (T1, 17), (T2, 18), (T3, 19), (T4, 0)], ignore}
    build_test! {clz, vec![(T0, 0), (T1, 17), (T2, 18), (T3, 20), (T4, 32)], ignore}
    build_test! {div, vec![(T3, 0x10d6), (T4, 3)], ignore}
    build_test! {divu, vec![(T3, 0x10d6), (T4, 3)], ignore}
    build_test! {mul, vec![(T3, 0x2000_0000)], ignore}
    build_test! {mulu, vec![(T3, 0x2000_0000)], ignore}
    build_test! {mult, vec![], ignore}
    build_test! {multu, vec![], ignore}
    build_test! {nor, vec![(T2, 0xFFFF_FFFF), (T3, 0xFFFF_FFFE), (T4, 0xFFFF_FFFE), (T5, 0xFFFF_FFFE)]}
    build_test! {or, vec![(T2, 0), (T3, 1), (T4, 1), (T5, 1)]}
    build_test! {ori, vec![(T2, 0), (T3, 1), (T4, 1), (T5, 1)]}
    build_test! {sll, vec![
        (T0, 1),
        (T1, 1 << 1),
        (T2, 1 << 2),
        (T3, 1 << 3),
        (T4, 1 << 4),
        (T5, 1 << 5),
        (T6, 1 << 6),
        (T7, 1 << 7),
        (T8, 1 << 8),
        (V0, 1 << 9),
        (V1, 1 << 10),
        (A0, 1 << 11),
        (A1, 1 << 12),
        (A2, 1 << 13),
        (A3, 1 << 14)
    ]}
    build_test! {sllv, vec![
        (T0, 1),
        (T1, 13),

        (T2, 1 << 1),
        (T3, 1 << 2),
        (T4, 1 << 3),
        (T5, 1 << 4),
        (T6, 1 << 5),
        (T7, 1 << 6),
        (T8, 1 << 7),
        (V0, 1 << 8),
        (V1, 1 << 9),
        (A0, 1 << 10),
        (A1, 1 << 11),
        (A2, 1 << 12),
        (A3, 1 << 13)
    ]}
    build_test! {srl, vec![
        (T0, 0x8000_0000),
        (T1, 0x8000_0000 >> 1),
        (T2, 0x8000_0000 >> 2),
        (T3, 0x8000_0000 >> 3),
        (T4, 0x8000_0000 >> 4),
        (T5, 0x8000_0000 >> 5),
        (T6, 0x8000_0000 >> 6),
        (T7, 0x8000_0000 >> 7),
        (T8, 0x8000_0000 >> 8),
        (V0, 0x8000_0000 >> 9),
        (V1, 0x8000_0000 >> 10),
        (A0, 0x8000_0000 >> 11),
        (A1, 0x8000_0000 >> 12),
        (A2, 0x8000_0000 >> 13),
        (A3, 0x8000_0000 >> 14),
    ]}
    build_test! {srlv, vec![
        (T0, 0x8000_0000),
        (T1, 13),

        (T2, 0x8000_0000 >> 1),
        (T3, 0x8000_0000 >> 2),
        (T4, 0x8000_0000 >> 3),
        (T5, 0x8000_0000 >> 4),
        (T6, 0x8000_0000 >> 5),
        (T7, 0x8000_0000 >> 6),
        (T8, 0x8000_0000 >> 7),
        (V0, 0x8000_0000 >> 8),
        (V1, 0x8000_0000 >> 9),
        (A0, 0x8000_0000 >> 10),
        (A1, 0x8000_0000 >> 11),
        (A2, 0x8000_0000 >> 12),
        (A3, 0x8000_0000 >> 13),
    ]}

    build_test! {sub, vec![
        (T0, 100),
        (T1, -100i32 as u32)
    ]}

    build_test! {subu, vec![
        (T0, 100),
        (T1, 0)
    ]}

    build_test! {xor, vec![
        (T2, 0),
        (T3, 1),
        (T4, 1),
        (T5, 0)
    ]}

    build_test! {xori, vec![
        (T0, 1),
        (T1, 0x8000_0000),
        (T2, 0),
        (T3, 1),
        (T4, 1),
        (T5, 0),
        (T6, 0x8000_ffff)
    ]}

    build_test! {lui, vec![
        (T0, 0xffff_0000),
        (T1, 0xdead_0000),
        (T2, 0xbeef_0000),
        (T3, 0xfeed_0000)
    ]}

    build_test! {beq, vec![
        (T0, 0)
    ]
    }

    build_test! {ble, vec![], ignore}

    build_test! {blez, vec![
        (T0, 1),
        (T1, 202)
    ]}

    build_test! {bltz, vec![
        (T0, 0),
        (T1, 200)
    ]}

    build_test! {bne, vec![(T0, 40), (T1, 0)]}

    build_test! {bgez, vec![(T1, -1i32 as u32), (T0, 1 << 11)]}

    build_test! {bgtz, vec![(T0, 0)]}

    build_test! {j, vec![(T0, 100), (T1, 200)]}

    build_test! {back_j, vec![(T0, 300), (T1, 200)]}

    build_test! {jr, vec![(T0, 0), (T1, 1)]}

    build_test! {jalr, vec![(V0, 300)]}

    build_test! {jal, vec![(V0, 300)]}

    build_test! {lb, vec![
        (T1, 0xffff_ffab),
        (T2, 0xffff_ffcd),
        (T3, 0),
        (T4, 0),
        (T5, 0xffff_ffde)
    ]}

    build_test! {lbu, vec![
        (T1, 0x00ab),
        (T2, 0x00cd),
        (T3, 0),
        (T4, 0),
        (T5, 0x00de)
    ]}

    build_test! {lh, vec![
        (T1, 0xffff_abcd),
        (T2, 0xffff_cd00),
        (T3, 0),
        (T4, 0x00de),
        (T5, 0xffff_dead)
    ]}

    build_test! {lhu, vec![
        (T1, 0x0000_abcd),
        (T2, 0x0000_cd00),
        (T3, 0),
        (T4, 0x00de),
        (T5, 0x0000_dead)
    ]}

    build_test! {lw, vec![
        (T1, 0xabcd_abcd),
        (T2, 0x1111_0000),
        (T3, 0x1100_1010)
    ]}

    build_test! {lwl, vec![], ignore}

    build_test! {lwr, vec![], ignore}

    build_test! {sb, vec![
        (T0, 64),
        (T1, 64),
    ]}

    build_test! {sh, vec![
        (T0, 0xabff),
        (T1, 0xabff)
    ]}

    build_test! {sw, vec![
        (T0, 0xAAAA_BBBB),
        (T1, 0xAAAA_BBBB)
    ]}

    build_test! {swl, vec![], ignore}

    build_test! {swr, vec![], ignore}

    build_test! {mtlo_mthi, vec![(HI, 52), (LO, 50)]}

    build_test! {movn, vec![], ignore}

    build_test! {movz, vec![], ignore}

    build_test! {eret, vec![], ignore}

    build_test! {syscall, vec![], ignore}

    build_test! {break, vec![], ignore}

    build_test! {test_raw_dep, vec![(T0, 0xffff_000f)]}

    build_test! {test_raw_dep2, vec![(T0, 0x0001_0000),(T1, 0x0001_0000), (T2, 0x0002_0000)]}

    build_test! {test_back_to_back_raw_dep, vec![(T3, 8)]}

    build_test! {fib, vec![(T0, 55)]}

    build_test! {delay_slot, vec![(T0, 20), (T1, 20), (T2, 0)]}
}

#[cfg(test)]
mod perf {
    use mips_sim::sim::Sim;
    use mips_sim::sim::traits::*;
    use std::time;
    use wasm_bindgen_test::*;
    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    use std::fs::File;
    use std::io::prelude::*;
    use std::io::BufWriter;


    pub const ITERS: u32 = 30_000;
    pub const NUM_RUNS: u32 = 1000;
    pub const RESULT_FILE: &str = "tests/benchmark.out";

    fn nop_loop() -> Vec<f64> {
        let mut res_vec = Vec::new();
        for _ in 0..NUM_RUNS {
            let mut sim = Sim::new();
            let start = time::Instant::now();
            sim.step(ITERS);
            let elapsed = start.elapsed();
            res_vec.push(elapsed.as_secs_f64());
        }
        res_vec
    }

    fn nop_loop_wasm() -> TestRes {
        let mut time_vec = Vec::new();
        let mut cps_vec = Vec::new();
        for _ in 0..NUM_RUNS {
            let mut sim = Sim::new();
            let start = js_sys::Date::now();
            for _ in 0..ITERS {
                sim.step(1);
                let s = sim.get_state();
                s.reg_file.read(mips_sim::sim::common::Register::ZERO);
            }
            let elapsed = (js_sys::Date::now() - start) / (1000.0f64); // Convert to seconds
            let cps = (ITERS as f64) / elapsed;
            time_vec.push(elapsed);
            cps_vec.push(cps)
        }
        TestRes{elapsed_s: time_vec, cps: cps_vec, cycles: ITERS}
    }
    #[test]
    #[ignore]
    pub fn test_nop_loop_native() {
        let res_vec = nop_loop();
        let l = res_vec.len() as f64;

        let average = res_vec.into_iter().reduce(|acc, x| acc + x).unwrap() / l;
        let f = File::create(RESULT_FILE).unwrap();
        let mut w = BufWriter::new(f);

        w.write(
            format!(
                "Average Duration: {}\nCycles: {}\nCPS: {}",
                average,
                ITERS,
                (ITERS as f64) / average
            )
            .as_bytes(),
        )
        .unwrap();
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct TestRes {
        pub elapsed_s: Vec<f64>,
        pub cps: Vec<f64>,
        pub cycles: u32,
    }

    #[wasm_bindgen_test]
    pub fn test_nop_loop_wasm() {
        let test_res = nop_loop_wasm();
        let l = test_res.elapsed_s.len();

        web_sys::console::log_1(&wasm_bindgen::JsValue::from("elapsed_s,cycles_per_second,cycles"));
        for i in 0..l {
            let s = &format!("{},{},{}", test_res.elapsed_s[i], test_res.cps[i], test_res.cycles);
            web_sys::console::log_1(&wasm_bindgen::JsValue::from(s));
        }
        
    }
}
