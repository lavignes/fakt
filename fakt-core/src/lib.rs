#![deny(
    rustdoc::broken_intra_doc_links,
    rustdoc::private_intra_doc_links,
    elided_lifetimes_in_paths,
    unreachable_pub,
    unused_crate_dependencies,
    unused_qualifications,
    noop_method_call
)]

pub mod collections;
pub mod eval;
pub mod lang;

// TODO(lavignes): remove :)
use log as _;
