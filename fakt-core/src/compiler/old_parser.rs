use std::{
    error,
    fmt::{self, Display, Formatter},
    io::{self, ErrorKind, Read},
    result,
};

use crate::util::readchars::{self, ReadCharsExt};
use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
enum State {
    Comment,

    Root,
    PkgP,
    PkgK,
    PkgG,

    PkgIdentifier,
    OptionalPkgIdentifierDot,
    PkgNameSemi,

    OptionalRuleOrApplier,

    RuleOrPropertyIdentifier,
    OptionalRuleOrPropertyIdentifierDot,

    PropertyValue,
    PropertyValueSemi,

    RuleBraceOpen,
    RuleBraceClose,
    //    Name,
    //    Identifier,
    //    Semi,
    //    Dot,
    //    Not,
    //    And,
    //    Or,
    //    Xor,
    //    Eq,
    //    NewLine,
    //    BraceOpen,
    //    BraceClose,
    //    ParenOpen,
    //    ParenClose,
    //    Rule,
    //    Fact,
    //    Thus,
    //    Value,
}

#[derive(Debug)]
pub enum Error {
    IoError((usize, usize), io::Error),
    ParseError((usize, usize), String),
    InvalidState((usize, usize)),
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
            _ => None,
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum AstCondition {
    /// special condition that is always false
    Never,
    Fact(String, Vec<String>),
    And(Vec<AstCondition>),
    Or(Vec<AstCondition>),
    Xor(Vec<AstCondition>),
    Not(Box<AstCondition>),
}

#[derive(Debug, Copy, Clone)]
pub enum AstPropertyValue {
    Null,
    Bool(bool),
    String(String),
    Array(Vec<String>),
    Map(HashMap<String, AstPropertyValue>),
}

impl Default for AstPropertyValue {
    fn default() -> AstPropertyValue {
        AstPropertyValue::Null
    }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct AstProperty {
    name: String,
    value: AstPropertyValue,
}

#[derive(Debug)]
pub struct AstRule {
    /// Optional rule description that can be sent used for debugging when rules match
    description: Option<String>,
    condition: AstCondition,
    properties: Option<Vec<AstProperty>>,
    children: Option<Vec<AstRule>>,
}

#[derive(Debug)]
pub struct AstPackage {
    name: String,
    rules: Vec<AstRule>,
}

// TODO: Maybe finish this parser.. It is faster and gives better error messages.
pub struct Parser {
    line: usize,
    column: usize,
    state_stack: Vec<State>,
    string_buffer: String,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            line: 1,
            column: 0,
            state_stack: vec![
                State::Root,
                State::OptionalRuleOrApplier,
                State::PkgNameSemi,
                State::OptionalPkgIdentifierDot,
                State::PkgIdentifier,
                State::PkgG,
                State::PkgK,
                State::PkgP,
            ],
            string_buffer: String::with_capacity(32),
        }
    }

    pub fn parse<R: Read>(reader: R) -> Result<AstPackage> {
        let mut parser = Parser::new();
        parser.parse_now(reader)
    }

    fn parse_now<R: Read>(&mut self, reader: R) -> Result<AstPackage> {
        let mut package_name = String::new();
        let mut property = AstProperty::default();
        let mut rules = Vec::new();

        let chars = reader.char_stream();
        'next_char: for result in chars {
            self.column += 1;
            let c = result.map_err(|err| {
                Error::IoError(
                    (self.line, self.column),
                    match err {
                        readchars::Error::IoError(err) => err,
                        readchars::Error::Utf8Error(err) => {
                            io::Error::new(ErrorKind::InvalidData, err)
                        }
                    },
                )
            })?;

            'check_char: loop {
                if c.is_whitespace() {
                    // Update the parser location
                    if c == '\n' {
                        self.column = 1;
                        self.line += 1;
                        // Also if we're in a comment we can exit when we see a newline
                        if let State::Comment = self.peek()? {
                            self.pop()?;
                        }
                    }
                    continue 'next_char;
                }
                match self.peek()? {
                    State::Comment => {}
                    State::PkgP => {
                        if self.transitions_to_comment(c) {
                            continue 'next_char;
                        } else if c == 'p' {
                            self.pop()?;
                        } else {
                            return self.parse_err(format!("Unexpected input: \"{}\"", c));
                        }
                    }
                    State::PkgK => {
                        if c == 'k' {
                            self.pop()?;
                        } else {
                            return self.parse_err(format!("Unexpected input: \"p{}\"", c));
                        }
                    }
                    State::PkgG => {
                        if c == 'g' {
                            self.pop()?;
                        } else {
                            return self.parse_err(format!("Unexpected input: \"pk{}\"", c));
                        }
                    }
                    State::PkgIdentifier | State::RuleOrPropertyIdentifier => {
                        if self.string_buffer.is_empty() {
                            if Self::is_identifier_start(c) {
                                self.string_buffer.push(c);
                            } else {
                                return self.parse_err(format!("Unexpected input: \"{}\"", c));
                            }
                        } else if c.is_alphanumeric() {
                            self.string_buffer.push(c);
                        } else {
                            self.pop()?;
                            continue 'check_char;
                        }
                    }
                    State::OptionalPkgIdentifierDot => {
                        // We see another dot, so there must be another identifier
                        if c == '.' {
                            self.string_buffer.push(c);
                            self.push(State::PkgIdentifier);
                        } else {
                            self.pop()?;
                            continue 'check_char;
                        }
                    }
                    State::PkgNameSemi => {
                        if c == ';' {
                            package_name.push_str(self.string_buffer.as_str());
                            self.string_buffer.clear();
                            self.pop()?;
                        } else {
                            return self.parse_err(format!("Unexpected input: \"{}\"", c));
                        }
                    }
                    State::OptionalRuleOrApplier => {
                        if self.transitions_to_comment(c) {
                            continue 'next_char;
                        } else if Self::is_identifier_start(c) {
                            // We need to prepare for either a fact or an property.
                            // We won't know which is happening until we see a colon later.
                            self.pop()?;
                            self.push(State::OptionalRuleOrPropertyIdentifierDot);
                            self.push(State::RuleOrPropertyIdentifier);
                            continue 'check_char;
                        } else {
                            return self.parse_err(format!("Unexpected input: \"{}\"", c));
                        }
                    }
                    State::OptionalRuleOrPropertyIdentifierDot => {
                        // We see another dot, so there must be another identifier
                        if c == '.' {
                            self.string_buffer.push(c);
                            self.push(State::RuleOrPropertyIdentifier);
                        } else if c == ':' {
                            // yay its a property for sure
                            property.name.push_str(self.string_buffer.as_str());
                            self.string_buffer.clear();
                            self.pop()?;
                            self.pop()?;
                            self.push(State::PropertyValueSemi);
                            self.push(State::PropertyValue);
                        } else {
                            // Ok, so we can just assume this is a fact
                            self.pop()?;
                            self.pop()?;
                            self.push(State::RuleBraceClose);
                            self.push(State::OptionalRuleOrApplier);
                            self.push(State::RuleBraceOpen);

                            continue 'check_char;
                        }
                    }
                    _ => unimplemented!(),
                }
                continue 'next_char;
            }
        }

        match self.pop()? {
            State::Root | State::OptionalRuleOrApplier => Ok(AstPackage {
                name: package_name,
                rules,
            }),
            _ => self.parse_err("Unexpected end of input".to_string()),
        }
    }

    #[inline]
    fn parse_err(&self, msg: String) -> Result<AstPackage> {
        Err(Error::ParseError(
            (self.line, self.column),
            match Self::expecting_what(self.peek()?) {
                Some(expecting) => format!("{}, expecting {}", msg, expecting),
                None => msg,
            },
        ))
    }

    #[inline]
    fn transitions_to_comment(&mut self, c: char) -> bool {
        if c == '#' {
            self.push(State::Comment);
            true
        } else {
            false
        }
    }

    #[inline]
    fn is_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    #[inline]
    fn push(&mut self, state: State) {
        self.state_stack.push(state)
    }

    #[inline]
    fn pop(&mut self) -> Result<State> {
        self.state_stack
            .pop()
            .ok_or(Error::InvalidState((self.line, self.column)))
    }

    #[inline]
    fn peek(&self) -> Result<State> {
        self.state_stack
            .last()
            .copied()
            .ok_or(Error::InvalidState((self.line, self.column)))
    }

    #[inline]
    fn expecting_what(state: State) -> Option<&'static str> {
        match state {
            State::Comment => None,
            State::Root => None,

            State::PkgP | State::PkgK | State::PkgG => {
                Some("start of package (ex: \"pkg my.package.name;\")")
            }
            State::PkgIdentifier | State::OptionalPkgIdentifierDot | State::PkgNameSemi => {
                Some("package name (ex: \"pkg my.package.name;\")")
            }

            State::OptionalRuleOrApplier | State::RuleOrPropertyIdentifier => {
                Some("fact or property name")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn assert_parse_err(input: &str, offset: (usize, usize), msg: &str) {
        match Parser::parse(Cursor::new(input)) {
            Err(Error::ParseError(o, m)) if o == offset && msg == m => {}
            other => panic!("Unexpected result {:?}", other),
        }
    }

    #[test]
    fn no_package_name() {
        let pkg = r#"
            # just a comment :(
        "#;
        assert_parse_err(
            pkg,
            (3, 9),
            "Unexpected end of input, expecting start of package (ex: \"pkg my.package.name;\")",
        );
    }

    #[test]
    fn pkg_k() {
        let pkg = r#"
            poof
        "#;
        assert_parse_err(
            pkg,
            (2, 15),
            "Unexpected input: \"po\", expecting start of package (ex: \"pkg my.package.name;\")",
        );
    }

    #[test]
    fn pkg_g() {
        let pkg = r#"
            pkno
        "#;
        assert_parse_err(
            pkg,
            (2, 16),
            "Unexpected input: \"pkn\", expecting start of package (ex: \"pkg my.package.name;\")",
        );
    }

    #[test]
    fn pkg_end_of_input() {
        let pkg = r#"
            pkg
        "#;
        assert_parse_err(
            pkg,
            (3, 9),
            "Unexpected end of input, expecting package name (ex: \"pkg my.package.name;\")",
        );
    }

    #[test]
    fn pkg_not_identifier() {
        let pkg = r#"
            pkg 2
        "#;
        assert_parse_err(
            pkg,
            (2, 18),
            "Unexpected input: \"2\", expecting package name (ex: \"pkg my.package.name;\")",
        );
    }

    #[test]
    fn pkg_not_identifier_partial() {
        let pkg = r#"
            pkg foo.2
        "#;
        assert_parse_err(
            pkg,
            (3, 9),
            "Unexpected end of input, expecting package name (ex: \"pkg my.package.name;\")",
        );
    }

    #[test]
    fn empty_package() {
        let pkg = r#"
            # hello
            pkg hello.world;
        "#;
        let pkg = Parser::parse(Cursor::new(pkg)).unwrap();
        assert_eq!("hello.world", pkg.name);
        assert!(pkg.rules.is_empty());
    }
}
