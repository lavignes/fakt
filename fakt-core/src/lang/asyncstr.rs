use std::{io, pin::Pin, str};

use futures::{
    io::BufReader,
    task::{Context, Poll},
    AsyncBufRead, AsyncRead,
};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error(transparent)]
    IoError(#[from] io::Error),

    #[error(transparent)]
    Utf8Error(#[from] str::Utf8Error),
}

pin_project_lite::pin_project! {
    pub(crate) struct AsyncStrReader<R> {
        #[pin]
        inner: BufReader<R>,
    }
}

impl<R: AsyncRead + Unpin> AsyncStrReader<R> {
    #[inline]
    pub(crate) fn new(inner: R) -> Self {
        Self {
            inner: BufReader::new(inner),
        }
    }

    #[inline]
    pub(crate) fn with_capacity(capacity: usize, inner: R) -> Self {
        Self {
            inner: BufReader::with_capacity(capacity, inner),
        }
    }

    pub(crate) fn poll_fill_buf(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Result<&str, Error>> {
        let this = self.project();
        match this.inner.poll_fill_buf(cx) {
            Poll::Ready(Ok(buf)) => {
                match str::from_utf8(buf) {
                    Ok(s) => Poll::Ready(Ok(s)),
                    Err(err) => {
                        // The data is just plain invalid unicode
                        if err.error_len().is_some() {
                            Poll::Ready(Err(err.into()))
                        } else {
                            let valid_len = err.valid_up_to();
                            // Safety: We know the string is partially valid
                            //         up to `valid_len`
                            unsafe { Poll::Ready(Ok(str::from_utf8_unchecked(&buf[..valid_len]))) }
                        }
                    }
                }
            }
            Poll::Ready(Err(err)) => Poll::Ready(Err(err.into())),
            Poll::Pending => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }

    #[inline]
    pub(crate) fn consume(self: Pin<&mut Self>, amt: usize) {
        let this = self.project();
        this.inner.consume(amt);
    }
}

#[cfg(test)]
mod tests {
    use futures::{io::Cursor, task};

    use super::*;

    fn cx<'a>() -> Context<'a> {
        Context::from_waker(task::noop_waker_ref())
    }

    fn assert_str<R: AsyncRead + Unpin>(
        cx: &mut Context<'_>,
        buf: &mut AsyncStrReader<R>,
        s: &str,
    ) {
        let mut buf = Pin::new(buf);
        assert!(matches!(
            buf.as_mut().poll_fill_buf(cx),
            Poll::Ready(Ok(ss)) if ss == s
        ));
        buf.consume(s.len());
    }

    fn assert_utf8_error<R: AsyncRead + Unpin>(cx: &mut Context<'_>, buf: &mut AsyncStrReader<R>) {
        assert!(matches!(
            Pin::new(buf).poll_fill_buf(cx),
            Poll::Ready(Err(Error::Utf8Error(_)))
        ));
    }

    #[test]
    fn empty() {
        let mut cx = cx();
        let mut buf = AsyncStrReader::new(Cursor::new(""));
        assert_str(&mut cx, &mut buf, "");
    }

    #[test]
    fn single_char() {
        let mut cx = cx();
        let mut buf = AsyncStrReader::new(Cursor::new("a"));
        assert_str(&mut cx, &mut buf, "a");
        assert_str(&mut cx, &mut buf, "");
    }

    #[test]
    fn single_multibyte_char() {
        let mut cx = cx();
        let mut buf = AsyncStrReader::new(Cursor::new("ท"));
        assert_str(&mut cx, &mut buf, "ท");
        assert_str(&mut cx, &mut buf, "");
    }

    #[test]
    fn word() {
        let mut cx = cx();
        let mut buf = AsyncStrReader::new(Cursor::new("hello"));
        assert_str(&mut cx, &mut buf, "hello");
        assert_str(&mut cx, &mut buf, "");
    }

    #[test]
    fn invalid_sequence() {
        let mut cx = cx();
        let mut buf = AsyncStrReader::new(Cursor::new([0xe2, 0x28, 0xa1]));
        assert_utf8_error(&mut cx, &mut buf);
    }

    #[test]
    fn buffer_full() {
        let mut cx = cx();
        let mut buf = AsyncStrReader::with_capacity(5, Cursor::new("helloworld"));
        assert_str(&mut cx, &mut buf, "hello");
        assert_str(&mut cx, &mut buf, "world");
        assert_str(&mut cx, &mut buf, "");
    }
}
