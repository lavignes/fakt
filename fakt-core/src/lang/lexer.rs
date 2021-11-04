use std::{
    fmt::{self, Display, Formatter},
    io,
    pin::Pin,
    rc::Rc,
    str,
};

use futures::{
    task::{Context, Poll},
    AsyncRead, Stream,
};

use crate::{
    collections::{Interned, Interner, InternerRcWriteExt},
    lang::charstream::{self, CharStream, CharStreamExt},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Location {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl Display for Location {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{}", .source)]
    IoError {
        location: Location,
        source: io::Error,
    },

    #[error("{}", .source)]
    Utf8Error {
        location: Location,
        source: str::Utf8Error,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum Token {
    Comment,
    Pkg,
    And,
    Or,
    Xor,
    Not,
    Bang,
    Colon,
    Comma,
    Dot,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    String(Interned<str>),
    Identifier(Interned<str>),
}

impl Display for Token {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Comment => "comment",
                Token::Pkg => "\"pkg\"",
                Token::And => "\"and\"",
                Token::Or => "\"or\"",
                Token::Xor => "\"xor\"",
                Token::Not => "\"not\"",
                Token::Bang => "\"!\"",
                Token::Colon => "\":\"",
                Token::Comma => "\",\"",
                Token::Dot => "\".\"",
                Token::ParenOpen => "\"(\"",
                Token::ParenClose => "\")\"",
                Token::BraceOpen => "\"{\"",
                Token::BraceClose => "\"}\"",
                Token::BracketOpen => "\"[\"",
                Token::BracketClose => "\"]\"",
                Token::String(_) => "\"string\"",
                Token::Identifier(_) => "\"identifier\"",
            }
        )
    }
}

#[derive(Debug)]
enum State {
    Root,

    InComment,
    InIdentifier,
    InUnquotedString,
    InQuotedString,
    InDoubleQuotedString,
}

pub(crate) struct Lexer<R> {
    inner: CharStream<R>,
    pub(crate) interner: Rc<Interner<str>>,
    start_location: Location,
    pub(crate) location: Location,
    buf: String,
    stash: Option<char>,
    state: State,
}

impl<R: AsyncRead + Unpin> Lexer<R> {
    #[cfg(test)]
    #[inline]
    pub(crate) fn new(reader: R) -> Self {
        Self::with_interner(reader, Rc::new(Interner::new()))
    }

    #[inline]
    pub(crate) fn with_interner(reader: R, interner: Rc<Interner<str>>) -> Self {
        Self {
            inner: reader.char_stream(),
            interner,
            start_location: Location { line: 1, column: 1 },
            location: Location { line: 1, column: 0 },
            buf: String::new(),
            stash: None,
            state: State::Root,
        }
    }
}

#[inline]
fn is_identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

#[inline]
fn check_symbol(c: char) -> Option<Token> {
    match c {
        '!' => Some(Token::Bang),
        ':' => Some(Token::Colon),
        ',' => Some(Token::Comma),
        '.' => Some(Token::Dot),
        '(' => Some(Token::ParenOpen),
        ')' => Some(Token::ParenClose),
        '{' => Some(Token::BraceOpen),
        '}' => Some(Token::BraceClose),
        '[' => Some(Token::BracketOpen),
        ']' => Some(Token::BracketClose),
        _ => None,
    }
}

#[inline]
fn check_keyword(identifier: &str) -> Option<Token> {
    match identifier {
        "pkg" => Some(Token::Pkg),
        "and" => Some(Token::And),
        "or" => Some(Token::Or),
        "xor" => Some(Token::Xor),
        "not" => Some(Token::Not),
        _ => None,
    }
}

impl<R: AsyncRead + Unpin> Stream for Lexer<R> {
    type Item = Result<(Location, Token), Error>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<(Location, Token), Error>>> {
        let (mut stream, interner, start_location, location, buf, stash, state) = {
            let s = self.get_mut();
            (
                Pin::new(&mut s.inner),
                &mut s.interner,
                &mut s.start_location,
                &mut s.location,
                &mut s.buf,
                &mut s.stash,
                &mut s.state,
            )
        };

        // If we find a char that ends a token, we put it back into the stash.
        // Here we try to remove that item from the stash and if it doesn't exist, we poll.
        match stash
            .take()
            .map(|c| Poll::Ready(Some(Ok(c))))
            .or_else(|| {
                // Raw number tracking from the polled stream
                Some(match stream.as_mut().poll_next(cx) {
                    Poll::Ready(Some(Ok(c))) => {
                        location.column += 1;
                        if c == '\n' {
                            location.line += 1;
                            location.column = 1;
                        }
                        Poll::Ready(Some(Ok(c)))
                    }
                    other => other,
                })
            })
            .unwrap()
        {
            Poll::Ready(Some(Ok(c))) => match *state {
                State::Root => {
                    if let Some(sym) = check_symbol(c) {
                        *start_location = *location;
                        return Poll::Ready(Some(Ok((*start_location, sym))));
                    } else if is_identifier_start(c) {
                        buf.push(c);
                        *start_location = *location;
                        *state = State::InIdentifier;
                    } else if c == '\'' {
                        *start_location = *location;
                        *state = State::InQuotedString;
                    } else if c == '"' {
                        *start_location = *location;
                        *state = State::InDoubleQuotedString;
                    } else if c == '#' {
                        *start_location = *location;
                        *state = State::InComment;
                    } else if c.is_whitespace() {
                        cx.waker().wake_by_ref();
                        return Poll::Pending;
                    } else {
                        buf.push(c);
                        *start_location = *location;
                        *state = State::InUnquotedString;
                    }
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }

                State::InComment => {
                    if c == '\n' {
                        *state = State::Root;
                        return Poll::Ready(Some(Ok((*start_location, Token::Comment))));
                    }
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }

                State::InIdentifier => {
                    if c.is_alphanumeric() {
                        buf.push(c);
                    } else {
                        // save it for later
                        *stash = Some(c);
                        *state = State::Root;
                        return if let Some(keyword) = check_keyword(buf.as_str()) {
                            buf.clear();
                            Poll::Ready(Some(Ok((*start_location, keyword))))
                        } else {
                            let interned = interner.intern(buf.as_str()).unwrap();
                            buf.clear();
                            Poll::Ready(Some(Ok((*start_location, Token::Identifier(interned)))))
                        };
                    }
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }

                State::InUnquotedString => {
                    // Unquoted strings terminate on symbols or whitespace.
                    // TODO(lavignes): What if new symbols are added to the language?
                    if check_symbol(c).is_some() || c.is_whitespace() {
                        *stash = Some(c);
                        *state = State::Root;
                        let interned = interner.intern(buf.as_str()).unwrap();
                        buf.clear();
                        return Poll::Ready(Some(Ok((*start_location, Token::String(interned)))));
                    } else {
                        buf.push(c);
                    }
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }

                State::InQuotedString => {
                    if c == '\'' {
                        *state = State::Root;
                        let interned = interner.intern(buf.as_str()).unwrap();
                        buf.clear();
                        return Poll::Ready(Some(Ok((*start_location, Token::String(interned)))));
                    } else {
                        buf.push(c);
                    }
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }

                State::InDoubleQuotedString => {
                    if c == '"' {
                        *state = State::Root;
                        let interned = interner.intern(buf.as_str()).unwrap();
                        buf.clear();
                        return Poll::Ready(Some(Ok((*start_location, Token::String(interned)))));
                    } else {
                        buf.push(c);
                    }
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
            },
            Poll::Ready(None) => {
                // If we're at the end of input we might have something to flush in the buffer
                //   or we're just in a comment
                match *state {
                    State::InIdentifier => {
                        *state = State::Root;
                        if let Some(keyword) = check_keyword(buf.as_str()) {
                            buf.clear();
                            Poll::Ready(Some(Ok((*start_location, keyword))))
                        } else {
                            let interned = interner.intern(buf.as_str()).unwrap();
                            buf.clear();
                            Poll::Ready(Some(Ok((*start_location, Token::Identifier(interned)))))
                        }
                    }

                    State::InUnquotedString => {
                        *state = State::Root;
                        let interned = interner.intern(buf.as_str()).unwrap();
                        buf.clear();
                        Poll::Ready(Some(Ok((*start_location, Token::String(interned)))))
                    }

                    State::InComment => {
                        *state = State::Root;
                        Poll::Ready(Some(Ok((*start_location, Token::Comment))))
                    }

                    _ => Poll::Ready(None),
                }
            }

            Poll::Ready(Some(Err(err))) => Poll::Ready(Some(Err(match err {
                charstream::Error::Utf8Error(err) => Error::Utf8Error {
                    location: *location,
                    source: err,
                },
                charstream::Error::IoError(err) => Error::IoError {
                    location: *location,
                    source: err,
                },
            }))),
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

    fn assert_none<R: AsyncRead + Unpin>(cx: &mut Context<'_>, lexer: &mut Lexer<R>) {
        assert!(matches!(Pin::new(lexer).poll_next(cx), Poll::Ready(None)));
    }

    fn assert_pending<R: AsyncRead + Unpin>(cx: &mut Context<'_>, lexer: &mut Lexer<R>) {
        assert!(matches!(Pin::new(lexer).poll_next(cx), Poll::Pending));
    }

    fn assert_token<R: AsyncRead + Unpin>(
        cx: &mut Context<'_>,
        lexer: &mut Lexer<R>,
        location: (usize, usize),
        token: Token,
    ) {
        let result = Pin::new(lexer).poll_next(cx);
        assert!(matches!(result, Poll::Ready(Some(Ok(_)))));
        if let Poll::Ready(Some(Ok(tok))) = result {
            assert_eq!(
                Location {
                    line: location.0,
                    column: location.1
                },
                tok.0
            );
            assert_eq!(token, tok.1);
        }
    }

    #[test]
    fn empty() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new(""));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn keywords() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("pkg and or xor not"));
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Pkg);

        assert_pending(&mut cx, &mut lexer);

        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 5), Token::And);

        assert_pending(&mut cx, &mut lexer);

        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 9), Token::Or);

        assert_pending(&mut cx, &mut lexer);

        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 12), Token::Xor);

        assert_pending(&mut cx, &mut lexer);

        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 16), Token::Not);

        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn symbols() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("! : , . ( ) { } [ ]"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Bang);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 3), Token::Colon);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 5), Token::Comma);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 7), Token::Dot);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 9), Token::ParenOpen);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 11), Token::ParenClose);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 13), Token::BraceOpen);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 15), Token::BraceClose);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 17), Token::BracketOpen);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 19), Token::BracketClose);

        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn comment() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("#test\n"));
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Comment);
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn comment_no_newline() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("#test"));
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Comment);
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn quoted_string() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("'test'"));
        let test = lexer.interner.intern("test").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(test));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn double_quoted_string() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("\"test\""));
        let test = lexer.interner.intern("test").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(test));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn identifier() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("test"));
        let test = lexer.interner.intern("test").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Identifier(test));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn unquoted_string1() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("42"));
        let forty_two = lexer.interner.intern("42").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(forty_two));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn unquoted_string2() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("1test-is-a-test"));
        let this_is_a_test = lexer.interner.intern("1test-is-a-test").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(this_is_a_test));
        assert_none(&mut cx, &mut lexer);
    }
}
