use crate::{
    collections::Interner,
    compiler::{
        ast::{Condition, Package, Rule},
        lexer::{self, Lexer, Location, Token},
    },
};
use futures::{AsyncRead, StreamExt};
use std::{
    error,
    fmt::{self, Display, Formatter},
    io,
    rc::Rc,
    str,
};

#[derive(Debug)]
pub enum Error {
    IoError(Location, io::Error),
    Utf8Error(Location, str::Utf8Error),
    SyntaxError(Location, String),
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
            _ => None,
        }
    }
}

impl From<lexer::Error> for Error {
    #[inline]
    fn from(err: lexer::Error) -> Error {
        match err {
            lexer::Error::IoError(location, err) => Error::IoError(location, err),
            lexer::Error::Utf8Error(location, err) => Error::Utf8Error(location, err),
        }
    }
}

struct Parser<R> {
    lexer: Lexer<R>,
    stash: Option<(Location, Token)>,
}

impl<R: AsyncRead + Unpin> Parser<R> {
    #[inline]
    pub fn new(reader: R) -> Parser<R> {
        Parser::with_interner(reader, Rc::new(Interner::new()))
    }

    #[inline]
    pub fn with_interner(reader: R, interner: Rc<Interner<str>>) -> Parser<R> {
        Parser {
            lexer: Lexer::with_interner(reader, interner),
            stash: None,
        }
    }

    pub async fn parse(&mut self) -> Result<Package, Error> {
        let name = self.package_name().await?;
        Ok(Package {
            name,
            rules: self.rules().await?,
        })
    }

    async fn package_name(&mut self) -> Result<String, Error> {
        self.expect_simple_token(Token::Pkg).await?;
        let (_, name) = self.expect_qualified_identifier().await?;
        self.expect_simple_token(Token::Semi).await?;
        Ok(name)
    }

    async fn rules(&mut self) -> Result<Vec<Rule>, Error> {
        if let Some(result) = self.rules_opt().await {
            Ok(result?)
        } else {
            Err(Error::SyntaxError(
                self.lexer.location,
                "expecting a rule".into(),
            ))
        }
    }

    #[async_recursion::async_recursion(?Send)]
    async fn rules_opt(&mut self) -> Option<Result<Vec<Rule>, Error>> {
        let mut rules = Vec::new();
        while let Some(result) = self.rule_opt().await {
            match result {
                Ok(rule) => rules.push(rule),
                Err(err) => {
                    return Some(Err(err));
                }
            }
        }
        Some(Ok(rules))
    }

    async fn rule_opt(&mut self) -> Option<Result<Rule, Error>> {
        match self.condition_opt().await? {
            Ok(condition) => {
                if let Err(err) = self.expect_simple_token(Token::BraceOpen).await {
                    return Some(Err(err));
                }
                let children = match self.rules_opt().await {
                    Some(Ok(rules)) => Some(rules),
                    Some(Err(err)) => {
                        return Some(Err(err));
                    }
                    None => None,
                };
                if let Err(err) = self.expect_simple_token(Token::BraceClose).await {
                    return Some(Err(err));
                }
                Some(Ok(Rule {
                    condition,
                    properties: None,
                    children,
                }))
            }
            Err(err) => Some(Err(err)),
        }
    }

    #[async_recursion::async_recursion(?Send)]
    async fn condition(&mut self) -> Result<Condition, Error> {
        let (_, tok) = match self.next().await {
            Some(Ok((location, tok))) => (location, tok),
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return {
                    Err(Error::SyntaxError(
                        self.lexer.location,
                        "unexpected end of input, expected a rule condition".into(),
                    ))
                };
            }
        };
        match tok {
            Token::ParenOpen => {
                let cond = self.condition().await?;
                self.expect_simple_token(Token::ParenClose).await?;
                Ok(cond)
            }
            Token::Not | Token::Bang => Ok(Condition::Not(Box::new(self.condition().await?))),
            Token::Identifier(_) => {
                let (_, ident) = self.expect_qualified_identifier().await?;
                // TODO: args
                Ok(Condition::Fact(ident, None))
            }
            other => Err(Error::SyntaxError(
                self.lexer.location,
                format!("unexpected {}, expected a rule condition", other),
            )),
        }
    }

    async fn condition_opt(&mut self) -> Option<Result<Condition, Error>> {
        let (location, tok) = match self.next().await? {
            Ok((location, tok)) => (location, tok),
            Err(err) => {
                return Some(Err(err));
            }
        };
        match tok {
            Token::ParenOpen | Token::Not | Token::Bang | Token::Identifier(_) => {
                self.stash = Some((location, tok));
                Some(self.condition().await)
            }
            _ => None,
        }
    }

    async fn expect_qualified_identifier(&mut self) -> Result<(Location, String), Error> {
        let mut s = String::new();
        let location = self.lexer.location;
        loop {
            let (_, tok) = self.expect_identifier().await?;
            if let Token::Identifier(ident) = tok {
                s.push_str(self.lexer.interner.get(&ident).unwrap());
            }
            if let Some(result) = self.expect_simple_token_opt(Token::Dot).await {
                result?;
                s.push('.');
                continue;
            } else {
                return Ok((location, s));
            }
        }
    }

    async fn expect_qualified_identifier_opt(
        &mut self,
    ) -> Option<Result<(Location, String), Error>> {
        if let Ok((location, tok)) = self.expect_identifier_opt().await? {
            self.stash = Some((location, tok));
            Some(self.expect_qualified_identifier().await)
        } else {
            None
        }
    }

    async fn expect_simple_token(&mut self, token: Token) -> Result<(Location, Token), Error> {
        match self.next().await {
            Some(Ok((location, tok))) => {
                if tok == token {
                    Ok((location, tok))
                } else {
                    Err(Error::SyntaxError(
                        location,
                        format!("unexpected {}, expected {}", tok, token),
                    ))
                }
            }
            Some(Err(err)) => Err(err),
            None => Err(Error::SyntaxError(
                self.lexer.location,
                format!("unexpected end of input, expected {}", token),
            )),
        }
    }

    async fn expect_simple_token_opt(
        &mut self,
        token: Token,
    ) -> Option<Result<(Location, Token), Error>> {
        match self.next().await {
            Some(Ok((location, tok))) => {
                if tok == token {
                    Some(Ok((location, tok)))
                } else {
                    self.stash = Some((location, tok));
                    None
                }
            }
            other => other,
        }
    }

    async fn expect_identifier(&mut self) -> Result<(Location, Token), Error> {
        match self.next().await {
            Some(Ok((location, tok))) => {
                if let Token::Identifier(_) = tok {
                    Ok((location, tok))
                } else {
                    Err(Error::SyntaxError(
                        location,
                        format!("unexpected {}, expected an identifier", tok),
                    ))
                }
            }
            Some(Err(err)) => Err(err),
            None => Err(Error::SyntaxError(
                self.lexer.location,
                "unexpected end of input, expected an identifier".into(),
            )),
        }
    }

    async fn expect_identifier_opt(&mut self) -> Option<Result<(Location, Token), Error>> {
        match self.next().await {
            Some(Ok((location, tok))) => {
                if let Token::Identifier(_) = tok {
                    Some(Ok((location, tok)))
                } else {
                    self.stash = Some((location, tok));
                    None
                }
            }
            other => other,
        }
    }

    async fn next(&mut self) -> Option<Result<(Location, Token), Error>> {
        // Check if we stashed something first before trying to read another token
        if let Some(stash) = self.stash.take() {
            return Some(Ok(stash));
        }
        if let Some(result) = self.lexer.next().await {
            Some(result.map_err(Error::from))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::{executor, io::Cursor};

    fn assert_syntax_err(result: Result<Package, Error>, location: (usize, usize), msg: &str) {
        assert!(matches!(result, Err(Error::SyntaxError(_, _))));
        if let Err(Error::SyntaxError(loc, m)) = result {
            assert_eq!(location.0, loc.line);
            assert_eq!(location.1, loc.column);
            assert_eq!(msg, m);
        }
    }

    #[test]
    fn empty() {
        let mut p = Parser::new(Cursor::new(""));
        let result = executor::block_on(p.parse());
        assert_syntax_err(result, (1, 0), "unexpected end of input, expected \"pkg\"");
    }

    #[test]
    fn package_no_name() {
        let mut p = Parser::new(Cursor::new("pkg"));
        let result = executor::block_on(p.parse());
        assert_syntax_err(
            result,
            (1, 3),
            "unexpected end of input, expected an identifier",
        );
    }

    #[test]
    fn package_no_semicolon() {
        let mut p = Parser::new(Cursor::new("pkg hello"));
        let result = executor::block_on(p.parse());
        assert_syntax_err(result, (1, 9), "unexpected end of input, expected \";\"");
    }

    #[test]
    fn empty_package() {
        let mut p = Parser::new(Cursor::new("pkg hello.world;"));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);
    }

    #[test]
    fn simple_rule() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            simple {}
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules[0];
        assert!(matches!(rule.condition, Condition::Fact(_, None)));
        if let Condition::Fact(ident, _) = &rule.condition {
            assert_eq!("simple", ident);
        }
        assert!(matches!(rule.children, None));
        assert!(matches!(rule.properties, None));
    }
}
