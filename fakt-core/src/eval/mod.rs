use std::{
    error,
    fmt::{self, Display, Formatter},
};

pub mod naive;

#[derive(Debug)]
pub enum Error {}

impl Display for Error {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl error::Error for Error {
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            _ => None,
        }
    }
}

pub trait Evaluate<T> {
    fn evaluate() -> Result<T, Error>;
}
