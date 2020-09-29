use crate::{
    collections::{Interned, Interner, InternerRcWriteExt},
    util::charstream::{self, CharStream, CharStreamExt},
};
use futures::{
    io::ErrorKind,
    task::{Context, Poll},
    AsyncRead, Stream,
};
use std::{
    error,
    fmt::{self, Display, Formatter},
    io,
    pin::Pin,
    rc::Rc,
    str,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Location {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

#[derive(Debug)]
pub enum Error {
    IoError(Location, io::Error),
    Utf8Error(Location, str::Utf8Error),
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
            Error::IoError(_, err) => Some(err),
            Error::Utf8Error(_, err) => Some(err),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Comment,
    Pkg,
    And,
    Or,
    Xor,
    Not,
    True,
    False,
    Bang,
    Semi,
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
                Token::True => "\"true\"",
                Token::False => "\"false\"",
                Token::Bang => "\"!\"",
                Token::Semi => "\";\"",
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
}

pub struct Lexer<R> {
    inner: CharStream<R>,
    pub(crate) interner: Rc<Interner<str>>,
    start_location: Location,
    pub(crate) location: Location,
    buf: String,
    stash: Option<char>,
    state: State,
}

impl<R: AsyncRead + Unpin> Lexer<R> {
    #[inline]
    pub fn new(reader: R) -> Lexer<R> {
        Lexer::with_interner(reader, Rc::new(Interner::new()))
    }

    #[inline]
    pub fn with_interner(reader: R, interner: Rc<Interner<str>>) -> Lexer<R> {
        Lexer {
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
        ';' => Some(Token::Semi),
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
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        _ => None,
    }
}

impl<R: AsyncRead + Unpin> Stream for Lexer<R> {
    type Item = Result<(Location, Token), Error>;

    fn poll_next(
        self: Pin<&mut Lexer<R>>,
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
                    } else if c == '"' {
                        *start_location = *location;
                        *state = State::InQuotedString;
                    } else if c == '#' {
                        *start_location = *location;
                        *state = State::InComment;
                    } else if c.is_alphanumeric() {
                        buf.push(c);
                        *start_location = *location;
                        *state = State::InUnquotedString;
                    } else if c.is_whitespace() {
                        cx.waker().wake_by_ref();
                        return Poll::Pending;
                    } else {
                        return Poll::Ready(Some(Err(Error::IoError(
                            *location,
                            io::Error::new(
                                ErrorKind::InvalidInput,
                                format!("invalid input \"{}\"", c),
                            ),
                        ))));
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
                    // TODO: The conditions to exit an unquoted string are ambiguous
                    if check_symbol(c).is_some() {
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
                if buf.is_empty() {
                    Poll::Ready(None)
                } else {
                    match *state {
                        State::InIdentifier => {
                            *state = State::Root;
                            if let Some(keyword) = check_keyword(buf.as_str()) {
                                buf.clear();
                                Poll::Ready(Some(Ok((*start_location, keyword))))
                            } else {
                                let interned = interner.intern(buf.as_str()).unwrap();
                                buf.clear();
                                Poll::Ready(Some(Ok((
                                    *start_location,
                                    Token::Identifier(interned),
                                ))))
                            }
                        }

                        State::InUnquotedString => {
                            *state = State::Root;
                            let interned = interner.intern(buf.as_str()).unwrap();
                            buf.clear();
                            Poll::Ready(Some(Ok((*start_location, Token::String(interned)))))
                        }

                        _ => unreachable!(),
                    }
                }
            }

            Poll::Ready(Some(Err(err))) => Poll::Ready(Some(Err(match err {
                charstream::Error::Utf8Error(err) => Error::Utf8Error(*location, err),
                charstream::Error::IoError(err) => Error::IoError(*location, err),
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
    use super::*;
    use futures::{io::Cursor, task};

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
        let mut lexer = Lexer::new(Cursor::new("pkg and or xor not true false"));
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

        assert_pending(&mut cx, &mut lexer);

        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 20), Token::True);

        assert_pending(&mut cx, &mut lexer);

        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 25), Token::False);

        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn symbols() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("! ; : , . ( ) { } [ ]"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Bang);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 3), Token::Semi);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 5), Token::Colon);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 7), Token::Comma);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 9), Token::Dot);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 11), Token::ParenOpen);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 13), Token::ParenClose);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 15), Token::BraceOpen);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 17), Token::BraceClose);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 19), Token::BracketOpen);

        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 21), Token::BracketClose);

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
    fn quoted_string() {
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
    fn unquoted_string() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("42"));
        let forty_two = lexer.interner.intern("42").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(forty_two));
        assert_none(&mut cx, &mut lexer);
    }
}
