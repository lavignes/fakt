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
    lang::asyncstr::{self, AsyncStrReader},
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

#[allow(clippy::enum_variant_names)]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{} malformed number", .location)]
    MalformedNumber { location: Location },

    #[error("{} {}", .location, .source)]
    IoError {
        location: Location,
        source: io::Error,
    },

    #[error("{} {}", .location, .source)]
    Utf8Error {
        location: Location,
        source: str::Utf8Error,
    },
}

#[derive(Debug, Clone)]
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
    Int(i64),
    UInt(u64),
    Float(f64),
    String(Interned<str>),
    Identifier(Interned<str>),
}

impl PartialEq for Token {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Comment, Self::Comment) => true,
            (Self::Pkg, Self::Pkg) => true,
            (Self::And, Self::And) => true,
            (Self::Or, Self::Or) => true,
            (Self::Xor, Self::Xor) => true,
            (Self::Not, Self::Not) => true,
            (Self::Bang, Self::Bang) => true,
            (Self::Colon, Self::Colon) => true,
            (Self::Comma, Self::Comma) => true,
            (Self::Dot, Self::Dot) => true,
            (Self::ParenOpen, Self::ParenOpen) => true,
            (Self::ParenClose, Self::ParenClose) => true,
            (Self::BraceOpen, Self::BraceOpen) => true,
            (Self::BraceClose, Self::BraceClose) => true,
            (Self::BracketOpen, Self::BracketOpen) => true,
            (Self::BracketClose, Self::BracketClose) => true,
            (Self::Int(a), Self::Int(b)) if a == b => true,
            (Self::UInt(a), Self::UInt(b)) if a == b => true,
            (Self::Float(a), Self::Float(b)) if a == b => true,
            (Self::String(a), Self::String(b)) if a == b => true,
            (Self::Identifier(a), Self::Identifier(b)) if a == b => true,
            (_, _) => false,
        }
    }
}

impl Eq for Token {}

impl Display for Token {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Comment => "comment",
                Self::Pkg => "\"pkg\"",
                Self::And => "\"and\"",
                Self::Or => "\"or\"",
                Self::Xor => "\"xor\"",
                Self::Not => "\"not\"",
                Self::Bang => "\"!\"",
                Self::Colon => "\":\"",
                Self::Comma => "\",\"",
                Self::Dot => "\".\"",
                Self::ParenOpen => "\"(\"",
                Self::ParenClose => "\")\"",
                Self::BraceOpen => "\"{\"",
                Self::BraceClose => "\"}\"",
                Self::BracketOpen => "\"[\"",
                Self::BracketClose => "\"]\"",
                Self::Int(_) => "\"integer\"",
                Self::UInt(_) => "\"unsigned integer\"",
                Self::Float(_) => "\"float\"",
                Self::String(_) => "\"string\"",
                Self::Identifier(_) => "\"identifier\"",
            }
        )
    }
}

// FIXME(lavignes): Need to make more states for things like
//   string escapes.
#[derive(Debug)]
enum State {
    Root,

    InComment,
    InNumeric,
    InIdentifier,
    InUnquotedString,
    InQuotedString,
    InDoubleQuotedString,
}

pin_project_lite::pin_project! {
    pub(crate) struct Lexer<R> {
        #[pin]
        reader: AsyncStrReader<R>,
        pub(crate) interner: Rc<Interner<str>>,
        start_location: Location,
        pub(crate) location: Location,
        buf: String,
        state: State,
    }
}

impl<'a, R: AsyncRead + Unpin> Lexer<R> {
    #[cfg(test)]
    #[inline]
    pub(crate) fn new(reader: R) -> Self {
        Self::with_interner(reader, Rc::new(Interner::new()))
    }

    #[inline]
    pub(crate) fn with_interner(reader: R, interner: Rc<Interner<str>>) -> Self {
        Self {
            reader: AsyncStrReader::new(reader),
            interner,
            start_location: Location { line: 1, column: 1 },
            location: Location { line: 1, column: 0 },
            buf: String::new(),
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

#[inline]
fn is_numeric(ch: char) -> bool {
    ch.is_numeric() || (ch == '.') || (ch == '+') || (ch == '-')
}

impl<R: AsyncRead + Unpin> Stream for Lexer<R> {
    type Item = Result<(Location, Token), Error>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<(Location, Token), Error>>> {
        let mut this = self.project();

        let s = match this.reader.as_mut().poll_fill_buf(cx) {
            Poll::Ready(Ok(s)) => s,
            Poll::Ready(Err(err)) => {
                return Poll::Ready(Some(Err(match err {
                    asyncstr::Error::Utf8Error(err) => Error::Utf8Error {
                        location: *this.location,
                        source: err,
                    },

                    asyncstr::Error::IoError(err) => Error::IoError {
                        location: *this.location,
                        source: err,
                    },
                })));
            }
            Poll::Pending => {
                return Poll::Pending;
            }
        };

        // If we're at the end of input we might have something to flush in the buffer
        // or we're just in a comment
        if s.is_empty() {
            return match *this.state {
                State::InIdentifier => {
                    *this.state = State::Root;
                    if let Some(keyword) = check_keyword(this.buf.as_str()) {
                        this.buf.clear();
                        Poll::Ready(Some(Ok((*this.start_location, keyword))))
                    } else {
                        let interned = this.interner.intern(this.buf.as_str()).unwrap();
                        this.buf.clear();
                        Poll::Ready(Some(Ok((
                            *this.start_location,
                            Token::Identifier(interned),
                        ))))
                    }
                }

                State::InUnquotedString => {
                    *this.state = State::Root;
                    let interned = this.interner.intern(this.buf.as_str()).unwrap();
                    this.buf.clear();
                    Poll::Ready(Some(Ok((*this.start_location, Token::String(interned)))))
                }

                State::InComment => {
                    *this.state = State::Root;
                    Poll::Ready(Some(Ok((*this.start_location, Token::Comment))))
                }

                State::InNumeric => {
                    *this.state = State::Root;
                    if let Ok(int) = this.buf.parse::<i64>() {
                        this.buf.clear();
                        Poll::Ready(Some(Ok((*this.start_location, Token::Int(int)))))
                    } else if let Ok(uint) = this.buf.parse::<u64>() {
                        this.buf.clear();
                        Poll::Ready(Some(Ok((*this.start_location, Token::UInt(uint)))))
                    } else {
                        let result = this.buf.parse::<f64>();
                        this.buf.clear();
                        Poll::Ready(Some(match result {
                            Ok(float) => Ok((*this.start_location, Token::Float(float))),
                            Err(_) => Err(Error::MalformedNumber {
                                location: *this.start_location,
                            }),
                        }))
                    }
                }

                _ => {
                    // youd hope that the buf is empty here otherwise we'd be in the middle of some
                    // token most likely
                    debug_assert!(this.buf.is_empty());
                    Poll::Ready(None)
                }
            };
        }

        let mut consume_amount = 0;
        for c in s.chars() {
            let old_location = *this.location;
            consume_amount += c.len_utf8();
            this.location.column += 1;
            if c == '\n' {
                this.location.line += 1;
                this.location.column = 1;
            }
            match *this.state {
                State::Root => {
                    if c.is_whitespace() {
                        continue;
                    } else if let Some(sym) = check_symbol(c) {
                        *this.start_location = *this.location;
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(Ok((*this.start_location, sym))));
                    } else if is_identifier_start(c) {
                        this.buf.push(c);
                        *this.start_location = *this.location;
                        *this.state = State::InIdentifier;
                    } else if c == '\'' {
                        *this.start_location = *this.location;
                        *this.state = State::InQuotedString;
                    } else if c == '"' {
                        *this.start_location = *this.location;
                        *this.state = State::InDoubleQuotedString;
                    } else if c == '#' {
                        *this.start_location = *this.location;
                        *this.state = State::InComment;
                    } else if is_numeric(c) {
                        this.buf.push(c);
                        *this.start_location = *this.location;
                        *this.state = State::InNumeric;
                    } else {
                        this.buf.push(c);
                        *this.start_location = *this.location;
                        *this.state = State::InUnquotedString;
                    }
                }

                State::InComment => {
                    if c == '\n' {
                        *this.state = State::Root;
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(Ok((*this.start_location, Token::Comment))));
                    }
                }

                State::InNumeric => {
                    if is_numeric(c) {
                        this.buf.push(c);
                    } else if c == 'i' {
                        *this.state = State::Root;
                        let result = this.buf.parse::<i64>();
                        this.buf.clear();
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(match result {
                            Ok(int) => Ok((*this.start_location, Token::Int(int))),
                            Err(_) => Err(Error::MalformedNumber {
                                location: *this.start_location,
                            }),
                        }));
                    } else if c == 'u' {
                        *this.state = State::Root;
                        let result = this.buf.parse::<u64>();
                        this.buf.clear();
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(match result {
                            Ok(uint) => Ok((*this.start_location, Token::UInt(uint))),
                            Err(_) => Err(Error::MalformedNumber {
                                location: *this.start_location,
                            }),
                        }));
                    } else if c == 'f' {
                        *this.state = State::Root;
                        let result = this.buf.parse::<f64>();
                        this.buf.clear();
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(match result {
                            Ok(float) => Ok((*this.start_location, Token::Float(float))),
                            Err(_) => Err(Error::MalformedNumber {
                                location: *this.start_location,
                            }),
                        }));
                    } else {
                        // save it for later
                        *this.location = old_location;
                        consume_amount -= c.len_utf8();
                        this.reader.as_mut().consume(consume_amount);
                        *this.state = State::Root;
                        if let Ok(int) = this.buf.parse::<i64>() {
                            this.buf.clear();
                            return Poll::Ready(Some(Ok((*this.start_location, Token::Int(int)))));
                        }
                        if let Ok(uint) = this.buf.parse::<u64>() {
                            this.buf.clear();
                            return Poll::Ready(Some(Ok((
                                *this.start_location,
                                Token::UInt(uint),
                            ))));
                        }
                        let result = this.buf.parse::<f64>();
                        this.buf.clear();
                        return Poll::Ready(Some(match result {
                            Ok(float) => Ok((*this.start_location, Token::Float(float))),
                            Err(_) => Err(Error::MalformedNumber {
                                location: *this.start_location,
                            }),
                        }));
                    }
                }

                State::InIdentifier => {
                    if c.is_alphanumeric() {
                        this.buf.push(c);
                    } else {
                        // save it for later
                        *this.location = old_location;
                        consume_amount -= c.len_utf8();
                        this.reader.as_mut().consume(consume_amount);
                        *this.state = State::Root;
                        return if let Some(keyword) = check_keyword(this.buf.as_str()) {
                            this.buf.clear();
                            Poll::Ready(Some(Ok((*this.start_location, keyword))))
                        } else {
                            let interned = this.interner.intern(this.buf.as_str()).unwrap();
                            this.buf.clear();
                            Poll::Ready(Some(Ok((
                                *this.start_location,
                                Token::Identifier(interned),
                            ))))
                        };
                    }
                }

                State::InUnquotedString => {
                    // Unquoted strings terminate on symbols or whitespace.
                    // TODO(lavignes): What if new symbols are added to the language?
                    if check_symbol(c).is_some() || c.is_whitespace() {
                        // save it for later
                        *this.location = old_location;
                        consume_amount -= c.len_utf8();
                        this.reader.as_mut().consume(consume_amount);
                        *this.state = State::Root;
                        let interned = this.interner.intern(this.buf.as_str()).unwrap();
                        this.buf.clear();
                        return Poll::Ready(Some(Ok((
                            *this.start_location,
                            Token::String(interned),
                        ))));
                    } else {
                        this.buf.push(c);
                    }
                }

                State::InQuotedString => {
                    if c == '\'' {
                        *this.state = State::Root;
                        let interned = this.interner.intern(this.buf.as_str()).unwrap();
                        this.buf.clear();
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(Ok((
                            *this.start_location,
                            Token::String(interned),
                        ))));
                    } else {
                        this.buf.push(c);
                    }
                }

                State::InDoubleQuotedString => {
                    if c == '"' {
                        *this.state = State::Root;
                        let interned = this.interner.intern(this.buf.as_str()).unwrap();
                        this.buf.clear();
                        this.reader.as_mut().consume(consume_amount);
                        return Poll::Ready(Some(Ok((
                            *this.start_location,
                            Token::String(interned),
                        ))));
                    } else {
                        this.buf.push(c);
                    }
                }
            }
        }

        this.reader.consume(consume_amount);
        cx.waker().wake_by_ref();
        Poll::Pending
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

    fn assert_malformed_number<R: AsyncRead + Unpin>(
        cx: &mut Context<'_>,
        lexer: &mut Lexer<R>,
        location: (usize, usize),
    ) {
        let result = Pin::new(lexer).poll_next(cx);
        assert!(matches!(
            result,
            Poll::Ready(Some(Err(Error::MalformedNumber { .. })))
        ));
        if let Poll::Ready(Some(Err(Error::MalformedNumber { location: loc }))) = result {
            assert_eq!(
                Location {
                    line: location.0,
                    column: location.1
                },
                loc
            );
        }
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
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Pkg);
        assert_token(&mut cx, &mut lexer, (1, 5), Token::And);
        assert_token(&mut cx, &mut lexer, (1, 9), Token::Or);
        assert_token(&mut cx, &mut lexer, (1, 12), Token::Xor);
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 16), Token::Not);
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn symbols() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("! : , . ( ) { } [ ]"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Bang);
        assert_token(&mut cx, &mut lexer, (1, 3), Token::Colon);
        assert_token(&mut cx, &mut lexer, (1, 5), Token::Comma);
        assert_token(&mut cx, &mut lexer, (1, 7), Token::Dot);
        assert_token(&mut cx, &mut lexer, (1, 9), Token::ParenOpen);
        assert_token(&mut cx, &mut lexer, (1, 11), Token::ParenClose);
        assert_token(&mut cx, &mut lexer, (1, 13), Token::BraceOpen);
        assert_token(&mut cx, &mut lexer, (1, 15), Token::BraceClose);
        assert_token(&mut cx, &mut lexer, (1, 17), Token::BracketOpen);
        assert_token(&mut cx, &mut lexer, (1, 19), Token::BracketClose);
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn comment() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("#test\n"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Comment);
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn comment_no_newline() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("#test"));
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Comment);
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn integer() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("1234i"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Int(1234));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn unsinged_integer() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("1234u"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::UInt(1234));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn float() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("1234f"));
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Float(1234.0));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn ambiguous_integer() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("1234"));
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Int(1234));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn malformed_number() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("-1234.."));
        assert_pending(&mut cx, &mut lexer);
        assert_malformed_number(&mut cx, &mut lexer, (1, 1));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn quoted_string() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("'test'"));
        let test = lexer.interner.intern("test").unwrap();
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(test));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn double_quoted_string() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("\"test\""));
        let test = lexer.interner.intern("test").unwrap();
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(test));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn identifier() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("test"));
        let test = lexer.interner.intern("test").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::Identifier(test));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn unquoted_string1() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("~nice"));
        let nice = lexer.interner.intern("~nice").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(nice));
        assert_none(&mut cx, &mut lexer);
    }

    #[test]
    fn unquoted_string2() {
        let mut cx = cx();
        let mut lexer = Lexer::new(Cursor::new("*test-is-a-test"));
        let this_is_a_test = lexer.interner.intern("*test-is-a-test").unwrap();
        assert_pending(&mut cx, &mut lexer);
        assert_token(&mut cx, &mut lexer, (1, 1), Token::String(this_is_a_test));
        assert_none(&mut cx, &mut lexer);
    }
}
