pub mod sim;

use lazy_static::lazy_static;
use std::sync::Mutex;
use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

lazy_static! {
    pub static ref SIM: Mutex<sim::Sim> = Mutex::new(sim::Sim::new());
}

#[wasm_bindgen]
pub fn init_sim() {
    (*SIM).lock().unwrap().reinit();
}

#[wasm_bindgen]
pub fn step(n: u32) {
    (*SIM).lock().unwrap().step(n);
}

#[wasm_bindgen]
pub fn step_to_halt() {
    (*SIM).lock().unwrap().step_to_halt();
}

#[wasm_bindgen]
pub fn load_binary(instrs: Vec<u32>, data: Vec<u32>) {
    (*SIM).lock().unwrap().load_binary(&instrs, &data);
}

#[wasm_bindgen]
pub fn get_state() {
    let state = (*SIM).lock().unwrap().get_state();
}
