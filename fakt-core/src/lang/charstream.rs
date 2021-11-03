use std::{io, pin::Pin, str};

use futures::{
    task::{Context, Poll},
    AsyncRead, Stream,
};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error(transparent)]
    IoError(#[from] io::Error),

    #[error(transparent)]
    Utf8Error(#[from] str::Utf8Error),
}

pub(crate) struct CharStream<R> {
    buf: [u8; 4],
    len: usize,
    reader: R,
}

pub(crate) trait CharStreamExt<R: AsyncRead> {
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

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Result<char, Error>>> {
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
                    *len += 1;
                    // read as many as possible.
                    if let Ok(c) = str::from_utf8(&buf[0..*len]) {
                        *len = 0;
                        Poll::Ready(Some(Ok(c.chars().next().unwrap())))
                    } else {
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                }
            }
            Poll::Ready(Err(err)) => Poll::Ready(Some(Err(err.into()))),
            Poll::Pending => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use futures::{io::Cursor, task};

    use super::*;

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
        let mut stream = Cursor::new("a").char_stream();
        assert_char(&mut cx, &mut stream, 'a');
        assert_none(&mut cx, &mut stream);
    }

    #[test]
    fn single_multibyte_char() {
        let mut cx = cx();
        let mut stream = Cursor::new("ท").char_stream();
        // 3 byte char
        assert_pending(&mut cx, &mut stream);
        assert_pending(&mut cx, &mut stream);
        assert_char(&mut cx, &mut stream, 'ท');
        assert_none(&mut cx, &mut stream);
    }

    #[test]
    fn word() {
        let mut cx = cx();
        let mut stream = Cursor::new("hello").char_stream();
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
        let mut stream = Cursor::new([0xe2, 0x28, 0xa1]).char_stream();
        assert_pending(&mut cx, &mut stream);
        assert_pending(&mut cx, &mut stream);
        assert_pending(&mut cx, &mut stream);
        assert_utf8_error(&mut cx, &mut stream);
    }
}
