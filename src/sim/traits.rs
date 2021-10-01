use std::cmp::{Eq, PartialEq};
use std::fmt::Debug;
use std::hash::Hash;
// A struct that implements the ClockedMap trait mirrors the behaviour of a
// hardware flip flop in that when loading the value, it doesn't reflect on the
// output until the next clock cycle

pub trait ClockedMap<K, V>
where
    K: Field,
    V: Copy + Clone
{
    fn read(&self, field: K) -> V;
    fn clock(&mut self);
    fn load(&mut self, field: K, value: V);
}

pub trait Field: Debug + Hash + PartialEq + Eq {}

pub trait Value: Debug + PartialEq + Eq {}
