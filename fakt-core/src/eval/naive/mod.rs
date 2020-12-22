use crate::{
    collections::Interned,
    eval::{Error, Evaluate},
    lang::ast::Package,
};
use fxhash::FxHashMap;

// TODO: The idea is that we'll have a list of properties with pointers to the decision
//   tree nodes that decide their value. Decision tree nodes have unique IDs for caching.
//   The trees themselves will point to parent nodes and so on. The ID should be hash-based
//   using the actual rule representing the tree node.
struct Props {}

pub struct NaiveEvaluator {
    logic: FxHashMap<Interned<str>, Props>,
}

impl<T> Evaluate<T> for NaiveEvaluator {
    fn evaluate(prop: &str) -> Result<T, Error> {
        Err(Error::PropertyNotFound)
    }
}
