pub mod sim;

use wasm_bindgen::prelude::*;

#[cfg(feature="wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
#[wasm_bindgen]
pub struct State {
}

impl State {
    pub fn from_sim_state(s: sim::SimState) -> State {
        State{}
    }
}

#[wasm_bindgen]
pub fn init_sim() -> sim::Sim {
    sim::Sim::new()
}

#[wasm_bindgen]
pub fn step_sim(s: &mut sim::Sim, n: u32) {
    s.step(n);
}

#[wasm_bindgen]
pub fn step_to_halt(s: &mut sim::Sim) {
    s.step_to_halt();
}

#[wasm_bindgen]
pub fn get_sim_state(s: &sim::Sim) -> State {
    State::from_sim_state(s.get_state())
}
