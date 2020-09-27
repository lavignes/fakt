use std::{
    error,
    fmt::{self, Display, Formatter},
    io::{self, Bytes, Read},
    str,
};

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    Utf8Error(str::Utf8Error),
}

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
            Error::IoError(err) => Some(err),
            Error::Utf8Error(err) => Some(err),
        }
    }
}

impl From<io::Error> for Error {
    #[inline]
    fn from(err: io::Error) -> Error {
        Self::IoError(err)
    }
}

impl From<str::Utf8Error> for Error {
    #[inline]
    fn from(err: str::Utf8Error) -> Error {
        Self::Utf8Error(err)
    }
}

pub struct ReadChars<R> {
    inner: Bytes<R>,
}

pub trait ReadCharsExt {
    fn chars(self) -> ReadChars<Self>
        where
            Self: Sized;
}

impl<R: Read> ReadCharsExt for R {
    fn chars(self) -> ReadChars<R> {
        ReadChars {
            inner: self.bytes(),
        }
    }
}

impl<R: Read> Iterator for ReadChars<R> {
    type Item = Result<char, Error>;

    fn next(&mut self) -> Option<Result<char, Error>> {
        let mut buffer = [0, 0, 0, 0];
        for i in 0..buffer.len() as usize {
            let result = self.inner.next();
            // If we got to the end of input but have stuff in the buffer
            // then this *must be invalid utf-8*
            if result.is_none() {
                if i > 0 {
                    break;
                } else {
                    return None;
                }
            }

            // Handle normal io-errors
            let result = result.unwrap();
            if let Err(err) = result {
                return Some(Err(err.into()));
            }
            buffer[i] = result.unwrap();

            let result = str::from_utf8(&buffer[..i + 1]);
            if let Ok(s) = result {
                return Some(Ok(s.chars().next().unwrap()));
            }
        }

        // We didn't find a character, so return the utf-8 error we would have thrown
        Some(Err(str::from_utf8(&buffer)
            .map_err(Error::from)
            .err()
            .unwrap()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn empty() {
        let mut input = Cursor::new("").chars();
        assert!(input.next().is_none());
    }

    #[test]
    fn one() {
        let mut input = Cursor::new("1").chars();
        assert!(matches!(input.next(), Some(Ok('1'))));
    }

    #[test]
    fn invalid_sequence() {
        let mut input = Cursor::new([0xe2, 0x28, 0xa1]).chars();
        assert!(matches!(input.next(), Some(Err(Error::Utf8Error(_)))));
    }

    #[test]
    fn invalid_sequence_at_end() {
        let mut input = Cursor::new([b'h', b'e', b'l', b'l', b'o', 0xe2, 0x28, 0xa1]).chars();
        assert!(matches!(input.next(), Some(Ok('h'))));
        assert!(matches!(input.next(), Some(Ok('e'))));
        assert!(matches!(input.next(), Some(Ok('l'))));
        assert!(matches!(input.next(), Some(Ok('l'))));
        assert!(matches!(input.next(), Some(Ok('o'))));
        assert!(matches!(input.next(), Some(Err(Error::Utf8Error(_)))));
    }
}
