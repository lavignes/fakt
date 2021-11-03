pub mod naive;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("property not found")]
    PropertyNotFound,
}

pub trait Evaluate<T> {
    fn evaluate(prop: &str) -> Result<T, Error>;
}
