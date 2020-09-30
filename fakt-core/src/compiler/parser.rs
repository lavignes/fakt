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
        let rules = match self.rules_opt().await {
            Some(Err(err)) => {
                return Err(err);
            }
            Some(Ok(ok)) => Some(ok),
            None => None,
        };
        Ok(Package { name, rules })
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
        // There is actually no allocation for an empty vec
        //   but we signal intent here.
        if rules.is_empty() {
            None
        } else {
            Some(Ok(rules))
        }
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
        let (_, tok) = match self.peek().await {
            Some(Ok(ok)) => ok,
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return Err(Error::SyntaxError(
                    self.lexer.location,
                    "unexpected end of input, expected a rule condition".into(),
                ));
            }
        };
        match tok {
            Token::ParenOpen | Token::Identifier(_) | Token::Not | Token::Bang => {
                self.condition_lowest().await
            }
            other => Err(Error::SyntaxError(
                self.lexer.location,
                format!("unexpected {}, expected a rule condition", other),
            )),
        }
    }

    async fn condition_lowest(&mut self) -> Result<Condition, Error> {
        let low = self.condition_low().await?;

        let (_, tok) = match self.peek().await {
            Some(Ok(ok)) => ok,
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return Err(Error::SyntaxError(
                    self.lexer.location,
                    "unexpected end of input, expected a rule operator or rule body".into(),
                ));
            }
        };

        match tok {
            Token::Or | Token::Comma => {
                self.next().await;
                Ok(Condition::Or(
                    Box::new(low),
                    Box::new(self.condition_low().await?),
                ))
            }
            _ => Ok(low),
        }
    }

    async fn condition_low(&mut self) -> Result<Condition, Error> {
        let low = self.condition_high().await?;

        let (_, tok) = match self.peek().await {
            Some(Ok(ok)) => ok,
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return Err(Error::SyntaxError(
                    self.lexer.location,
                    "unexpected end of input, expected a rule operator or rule body".into(),
                ));
            }
        };

        match tok {
            Token::Xor => {
                self.next().await;
                Ok(Condition::Xor(
                    Box::new(low),
                    Box::new(self.condition_high().await?),
                ))
            }
            _ => Ok(low),
        }
    }

    async fn condition_high(&mut self) -> Result<Condition, Error> {
        let high = self.condition_highest().await?;

        let (_, tok) = match self.peek().await {
            Some(Ok(ok)) => ok,
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return Err(Error::SyntaxError(
                    self.lexer.location,
                    "unexpected end of input, expected a rule operator or rule body".into(),
                ));
            }
        };

        match tok {
            Token::And | Token::Identifier(_) | Token::ParenOpen | Token::Bang | Token::Not => {
                // Since the and operator is implied, we dont want to accidentally consume
                //   the other tokens that can be the RHS of the and condition
                if let Token::And = tok {
                    self.next().await;
                }
                Ok(Condition::And(
                    Box::new(high),
                    Box::new(self.condition().await?),
                ))
            }
            _ => Ok(high),
        }
    }

    #[async_recursion::async_recursion(?Send)]
    async fn condition_highest(&mut self) -> Result<Condition, Error> {
        let (_, tok) = match self.peek().await {
            Some(Ok(ok)) => ok,
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return Err(Error::SyntaxError(
                    self.lexer.location,
                    "unexpected end of input, expected a rule operator or rule body".into(),
                ));
            }
        };

        match tok {
            Token::Bang | Token::Not => {
                self.next().await;
                Ok(Condition::Not(Box::new(self.condition_highest().await?)))
            }
            Token::ParenOpen => {
                self.next().await;
                let cond = self.condition().await?;
                self.expect_simple_token(Token::ParenClose).await?;
                Ok(cond)
            }
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
        let (_, tok) = match self.peek().await? {
            Ok(ok) => ok,
            Err(err) => {
                return Some(Err(err));
            }
        };
        match tok {
            Token::ParenOpen | Token::Not | Token::Bang | Token::Identifier(_) => {
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
        if let Some(Ok((_, Token::Identifier(_)))) = self.peek().await {
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

    /// Peeks the next token and consumes it if it matches the given token
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

    /// Peek the next token
    async fn peek(&mut self) -> Option<Result<(Location, Token), Error>> {
        match self.next().await? {
            Ok((location, tok)) => {
                self.stash = Some((location, tok.clone()));
                Some(Ok((location, tok)))
            }
            Err(err) => Some(Err(err)),
        }
    }

    /// Consume the next token
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

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::Fact(_, None)));
        if let Condition::Fact(ident, _) = &rule.condition {
            assert_eq!("simple", ident);
        }
        assert!(matches!(rule.children, None));
        assert!(matches!(rule.properties, None));
    }

    #[test]
    fn recursive_rule() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            
            level1 { level2 {} }
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::Fact(_, None)));
        if let Condition::Fact(ident, _) = &rule.condition {
            assert_eq!("level1", ident);
        }
        assert!(matches!(rule.children, Some(_)));
        assert!(matches!(rule.properties, None));

        if let Some(children) = &rule.children {
            let rule = &children[0];
            assert!(matches!(rule.condition, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &rule.condition {
                assert_eq!("level2", ident);
            }
            assert!(matches!(rule.children, None));
            assert!(matches!(rule.properties, None));
        }
    }

    #[test]
    fn explicit_and() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            
            lhs and rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::And(_, _)));
        if let Condition::And(lhs, rhs) = &rule.condition {
            assert!(matches!(**lhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**lhs {
                assert_eq!("lhs", ident);
            }
            assert!(matches!(**rhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**rhs {
                assert_eq!("rhs", ident);
            }
        }
    }

    #[test]
    fn implicit_and() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            
            lhs rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::And(_, _)));
        if let Condition::And(lhs, rhs) = &rule.condition {
            assert!(matches!(**lhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**lhs {
                assert_eq!("lhs", ident);
            }
            assert!(matches!(**rhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**rhs {
                assert_eq!("rhs", ident);
            }
        }
    }

    #[test]
    fn explicit_or() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            
            lhs or rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::Or(_, _)));
        if let Condition::Or(lhs, rhs) = &rule.condition {
            assert!(matches!(**lhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**lhs {
                assert_eq!("lhs", ident);
            }
            assert!(matches!(**rhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**rhs {
                assert_eq!("rhs", ident);
            }
        }
    }

    #[test]
    fn comma_or() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            
            lhs, rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::Or(_, _)));
        if let Condition::Or(lhs, rhs) = &rule.condition {
            assert!(matches!(**lhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**lhs {
                assert_eq!("lhs", ident);
            }
            assert!(matches!(**rhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**rhs {
                assert_eq!("rhs", ident);
            }
        }
    }

    #[test]
    fn explicit_xor() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world;
            
            lhs xor rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_eq!("hello.world", pkg.name);

        let rule = &pkg.rules.unwrap()[0];
        assert!(matches!(rule.condition, Condition::Xor(_, _)));
        if let Condition::Xor(lhs, rhs) = &rule.condition {
            assert!(matches!(**lhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**lhs {
                assert_eq!("lhs", ident);
            }
            assert!(matches!(**rhs, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &**rhs {
                assert_eq!("rhs", ident);
            }
        }
    }
}
