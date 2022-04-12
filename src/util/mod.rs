#[macro_use]
pub mod general;

pub mod stack;
pub mod term;
pub mod tmv;
pub mod vec;

// I *would* replace this with std::collections::hash_map::HashMap, but was
// getting some kind of trait error, so whatever.
pub use hashbrown::hash_map::Entry;
pub use hashbrown::HashMap;
pub use lazy_static::lazy_static;

pub use general::*;
pub use stack::*;
pub use tmv::*;
pub use vec::*;
