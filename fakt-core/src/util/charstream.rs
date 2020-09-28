use futures::{
    task::{Context, Poll},
    AsyncRead, Stream,
};
use std::{
    error, fmt,
    fmt::{Display, Formatter},
    io,
    pin::Pin,
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
        Error::IoError(err)
    }
}

impl From<str::Utf8Error> for Error {
    #[inline]
    fn from(err: str::Utf8Error) -> Error {
        Error::Utf8Error(err)
    }
}

pub struct CharStream<R> {
    buf: [u8; 4],
    len: usize,
    reader: R,
}

pub trait CharStreamExt<R: AsyncRead> {
    fn char_stream(self) -> CharStream<R>;
}

impl<R: AsyncRead> CharStreamExt<R> for R {
    #[inline]
    fn char_stream(self) -> CharStream<R> {
        CharStream {
            buf: [0, 0, 0, 0],
            len: 0,
            reader: self,
        }
    }
}

impl<'a, R: AsyncRead + Unpin> Stream for CharStream<R> {
    type Item = Result<char, Error>;

    fn poll_next(
        self: Pin<&mut CharStream<R>>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<char, Error>>> {
        let (buf, len, mut reader) = {
            let s = self.get_mut();
            (&mut s.buf, &mut s.len, Pin::new(&mut s.reader))
        };
        match reader.as_mut().poll_read(cx, &mut buf[*len..*len + 1]) {
            Poll::Ready(Ok(l)) => {
                // End of input
                if l == 0 {
                    if *len == 0 {
                        Poll::Ready(None)
                    } else {
                        Poll::Ready(Some(Err(str::from_utf8(&buf[0..*len])
                            .map_err(Error::from)
                            .err()
                            .unwrap())))
                    }
                } else if (*len + 1) == 4 {
                    *len = 0;
                    Poll::Ready(Some(
                        str::from_utf8(&buf[0..(*len + 1)])
                            .map_err(Error::from)
                            .map(|s| s.chars().next().unwrap()),
                    ))
                } else {
                    *len = *len + 1;
                    // read as many as possible.
                    if let Ok(c) = str::from_utf8(&buf[0..*len]) {
                        *len = 0;
                        Poll::Ready(Some(Ok(c.chars().next().unwrap())))
                    } else {
                        Poll::Pending
                    }
                }
            }
            Poll::Ready(Err(err)) => match err.kind() {
                io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted => Poll::Pending,
                _ => Poll::Ready(Some(Err(err.into()))),
            },
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::{io::Cursor, task};

    fn cx<'a>() -> Context<'a> {
        Context::from_waker(task::noop_waker_ref())
    }

    fn assert_none<R: AsyncRead + Unpin>(cx: &mut Context<'_>, stream: &mut CharStream<R>) {
        assert!(matches!(Pin::new(stream).poll_next(cx), Poll::Ready(None)));
    }

    fn assert_pending<R: AsyncRead + Unpin>(cx: &mut Context<'_>, stream: &mut CharStream<R>) {
        assert!(matches!(Pin::new(stream).poll_next(cx), Poll::Pending));
    }

    fn assert_char<R: AsyncRead + Unpin>(
        cx: &mut Context<'_>,
        stream: &mut CharStream<R>,
        c: char,
    ) {
        assert!(matches!(
            Pin::new(stream).poll_next(cx),
            Poll::Ready(Some(Ok(cc))) if cc == c
        ));
    }

    fn assert_utf8_error<R: AsyncRead + Unpin>(cx: &mut Context<'_>, stream: &mut CharStream<R>) {
        assert!(matches!(
            Pin::new(stream).poll_next(cx),
            Poll::Ready(Some(Err(Error::Utf8Error(_))))
        ));
    }

    #[test]
    fn empty() {
        let mut cx = cx();
        let mut stream = &mut Cursor::new("").char_stream();
        assert_none(&mut cx, &mut stream);
    }

    #[test]
    fn single_char() {
        let mut cx = cx();
        let mut stream = &mut Cursor::new("a").char_stream();
        assert_char(&mut cx, &mut stream, 'a');
        assert_none(&mut cx, &mut stream);
    }

    #[test]
    fn single_multibyte_char() {
        let mut cx = cx();
        let mut stream = &mut Cursor::new("ท").char_stream();
        // 3 byte char
        assert_pending(&mut cx, &mut stream);
        assert_pending(&mut cx, &mut stream);
        assert_char(&mut cx, &mut stream, 'ท');
        assert_none(&mut cx, &mut stream);
    }

    #[test]
    fn word() {
        let mut cx = cx();
        let mut stream = &mut Cursor::new("hello").char_stream();
        assert_char(&mut cx, &mut stream, 'h');
        assert_char(&mut cx, &mut stream, 'e');
        assert_char(&mut cx, &mut stream, 'l');
        assert_char(&mut cx, &mut stream, 'l');
        assert_char(&mut cx, &mut stream, 'o');
        assert_none(&mut cx, &mut stream);
    }

    #[test]
    fn invalid_sequence() {
        let mut cx = cx();
        let mut stream = &mut Cursor::new([0xe2, 0x28, 0xa1]).char_stream();
        assert_pending(&mut cx, &mut stream);
        assert_pending(&mut cx, &mut stream);
        assert_pending(&mut cx, &mut stream);
        assert_utf8_error(&mut cx, &mut stream);
    }
}
