use crate::{
    collections::{Interned, Interner},
    lang::{
        ast::{Condition, Name, Package, Property, PropertyValue, Rule, RuleOrProperty},
        lexer::{self, Lexer, Location, Token},
    },
};
use futures::{AsyncRead, StreamExt};
use fxhash::FxHashMap;
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

#[derive(Debug)]
enum ConditionOrProperty {
    Condition(Condition),
    Property(Property),
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
        let children = match self.rule_props_opt().await {
            Some(Err(err)) => {
                return Err(err);
            }
            Some(Ok(ok)) => Some(ok),
            None => None,
        };
        Ok(Package { name, children })
    }

    async fn package_name(&mut self) -> Result<Name, Error> {
        self.expect_simple_token(Token::Pkg).await?;
        let (_, name) = self.expect_name().await?;
        Ok(name)
    }

    #[async_recursion::async_recursion(?Send)]
    async fn rule_props_opt(&mut self) -> Option<Result<Vec<RuleOrProperty>, Error>> {
        let mut rule_props = Vec::new();
        while let Some(result) = self.rule_prop_opt().await {
            match result {
                Ok(rule) => rule_props.push(rule),
                Err(err) => {
                    return Some(Err(err));
                }
            }
        }
        // There is actually no allocation for an empty vec
        //   but we signal intent here.
        if rule_props.is_empty() {
            None
        } else {
            Some(Ok(rule_props))
        }
    }

    async fn rule_prop_opt(&mut self) -> Option<Result<RuleOrProperty, Error>> {
        match self.condition_prop_opt().await? {
            Ok(ConditionOrProperty::Condition(condition)) => {
                if let Err(err) = self.expect_simple_token(Token::BraceOpen).await {
                    return Some(Err(err));
                }
                let children = match self.rule_props_opt().await {
                    Some(Ok(rule_props)) => Some(rule_props),
                    Some(Err(err)) => {
                        return Some(Err(err));
                    }
                    None => None,
                };
                if let Err(err) = self.expect_simple_token(Token::BraceClose).await {
                    return Some(Err(err));
                }
                Some(Ok(RuleOrProperty::Rule(Rule {
                    condition,
                    children,
                })))
            }
            Ok(ConditionOrProperty::Property(property)) => {
                Some(Ok(RuleOrProperty::Property(property)))
            }
            Err(err) => Some(Err(err)),
        }
    }

    #[async_recursion::async_recursion(?Send)]
    async fn condition(&mut self, name: Option<Name>) -> Result<Condition, Error> {
        // This is an edge-case. When parsing whether something is a rule or a property
        //   we have to fully parse a name. If we decide that it really is a rule we
        //   pipe that pre-parsed name down the condition parsing tree.
        if let Some(name) = name {
            return self.condition_lowest(Some(name)).await;
        }

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
                self.condition_lowest(None).await
            }
            other => Err(Error::SyntaxError(
                self.lexer.location,
                format!("unexpected {}, expected a rule condition", other),
            )),
        }
    }

    async fn condition_lowest(&mut self, name: Option<Name>) -> Result<Condition, Error> {
        let lhs = self.condition_low(name).await?;

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
                    Box::new(lhs),
                    Box::new(self.condition_low(None).await?),
                ))
            }
            _ => Ok(lhs),
        }
    }

    async fn condition_low(&mut self, name: Option<Name>) -> Result<Condition, Error> {
        let lhs = self.condition_high(name).await?;

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
                    Box::new(lhs),
                    Box::new(self.condition_high(None).await?),
                ))
            }
            _ => Ok(lhs),
        }
    }

    async fn condition_high(&mut self, name: Option<Name>) -> Result<Condition, Error> {
        let lhs = self.condition_highest(name).await?;

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
                    Box::new(lhs),
                    Box::new(self.condition_highest(None).await?),
                ))
            }
            _ => Ok(lhs),
        }
    }

    #[async_recursion::async_recursion(?Send)]
    async fn condition_highest(&mut self, name: Option<Name>) -> Result<Condition, Error> {
        if let Some(name) = name {
            let args = match self.args_opt().await {
                Some(Ok(rules)) => Some(rules),
                Some(Err(err)) => {
                    return Err(err);
                }
                None => None,
            };
            return Ok(Condition::Fact(name, args));
        }

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
                Ok(Condition::Not(Box::new(
                    self.condition_highest(None).await?,
                )))
            }
            Token::ParenOpen => {
                self.next().await;
                let cond = self.condition(None).await?;
                self.expect_simple_token(Token::ParenClose).await?;
                Ok(cond)
            }
            Token::Identifier(_) => {
                let (_, name) = self.expect_name().await?;
                let args = match self.args_opt().await {
                    Some(Ok(rules)) => Some(rules),
                    Some(Err(err)) => {
                        return Err(err);
                    }
                    None => None,
                };
                Ok(Condition::Fact(name, args))
            }
            other => Err(Error::SyntaxError(
                self.lexer.location,
                format!("unexpected {}, expected a rule condition", other),
            )),
        }
    }

    async fn condition_prop_opt(&mut self) -> Option<Result<ConditionOrProperty, Error>> {
        let (_, tok) = match self.peek().await? {
            Ok(ok) => ok,
            Err(err) => {
                return Some(Err(err));
            }
        };
        match tok {
            Token::ParenOpen | Token::Not | Token::Bang => Some(
                self.condition(None)
                    .await
                    .map(ConditionOrProperty::Condition),
            ),
            Token::Identifier(_) => {
                // We have to check ahead a bit before we can decide whether a name
                //   refers to a rule or property
                let (_, name) = match self.expect_name().await {
                    Ok(ok) => ok,
                    Err(err) => {
                        return Some(Err(err));
                    }
                };
                match self.expect_simple_token_opt(Token::Colon).await {
                    Some(Ok(_)) => {
                        let (_, value) = match self.expect_value().await {
                            Ok(ok) => ok,
                            Err(err) => {
                                return Some(Err(err));
                            }
                        };

                        Some(Ok(ConditionOrProperty::Property(Property { name, value })))
                    }
                    Some(Err(err)) => Some(Err(err)),
                    None => Some(
                        self.condition(Some(name))
                            .await
                            .map(ConditionOrProperty::Condition),
                    ),
                }
            }
            _ => None,
        }
    }

    async fn args_opt(&mut self) -> Option<Result<Vec<Interned<str>>, Error>> {
        match self.expect_simple_token_opt(Token::BracketOpen).await {
            Some(Ok(_)) => {}
            Some(Err(err)) => {
                return Some(Err(err));
            }
            None => {
                return None;
            }
        }

        let mut args = Vec::new();
        loop {
            match self.expect_string().await {
                Ok((_, s)) => args.push(s),
                Err(err) => {
                    return Some(Err(err));
                }
            }

            match self.expect_simple_token_opt(Token::Comma).await {
                Some(Err(err)) => return Some(Err(err)),
                Some(Ok(_)) => {}
                None => {
                    break;
                }
            }
        }

        if let Err(err) = self.expect_simple_token(Token::BracketClose).await {
            return Some(Err(err));
        }

        if args.is_empty() {
            None
        } else {
            Some(Ok(args))
        }
    }

    async fn expect_name(&mut self) -> Result<(Location, Name), Error> {
        let mut parts = Vec::new();
        let location = self.lexer.location;
        loop {
            let (_, tok) = self.expect_identifier().await?;
            if let Token::Identifier(ident) = tok {
                parts.push(ident);
            }
            if let Some(result) = self.expect_simple_token_opt(Token::Dot).await {
                result?;
                continue;
            } else {
                return Ok((location, Name { parts }));
            }
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

    #[async_recursion::async_recursion(?Send)]
    async fn expect_value(&mut self) -> Result<(Location, PropertyValue), Error> {
        let (_, tok) = match self.peek().await {
            Some(Ok(ok)) => ok,
            Some(Err(err)) => {
                return Err(err);
            }
            None => {
                return Err(Error::SyntaxError(
                    self.lexer.location,
                    "unexpected end of input, expected a property value".into(),
                ));
            }
        };

        match tok {
            Token::String(_) | Token::Identifier(_) => {
                let (location, s) = self.expect_string().await?;
                Ok((location, PropertyValue::String(s)))
            }

            Token::BracketOpen => {
                let (location, a) = self.expect_array().await?;
                Ok((location, PropertyValue::Array(a)))
            }

            Token::BraceOpen => {
                let (location, m) = self.expect_map().await?;
                Ok((location, PropertyValue::Map(m)))
            }

            other => Err(Error::SyntaxError(
                self.lexer.location,
                format!("unexpected {}, expected a property value", other),
            )),
        }
    }

    async fn expect_array(&mut self) -> Result<(Location, Vec<PropertyValue>), Error> {
        let (location, _) = self.expect_simple_token(Token::BracketOpen).await?;

        let mut array = Vec::new();
        loop {
            match self.expect_value().await {
                Ok((_, v)) => array.push(v),
                Err(err) => {
                    return Err(err);
                }
            }

            match self.expect_simple_token_opt(Token::Comma).await {
                Some(Err(err)) => return Err(err),
                Some(Ok(_)) => {}
                None => {
                    break;
                }
            }
        }

        self.expect_simple_token(Token::BracketClose).await?;
        Ok((location, array))
    }

    async fn expect_map(
        &mut self,
    ) -> Result<(Location, FxHashMap<Interned<str>, PropertyValue>), Error> {
        let (location, _) = self.expect_simple_token(Token::BraceOpen).await?;

        let mut map = FxHashMap::default();
        loop {
            let key = match self.expect_string().await {
                Ok((_, key)) => key,
                Err(err) => {
                    return Err(err);
                }
            };

            self.expect_simple_token(Token::Colon).await?;

            match self.expect_value().await {
                Ok((_, v)) => {
                    map.insert(key, v);
                }
                Err(err) => {
                    return Err(err);
                }
            }

            match self.expect_simple_token_opt(Token::Comma).await {
                Some(Err(err)) => return Err(err),
                Some(Ok(_)) => {}
                None => {
                    break;
                }
            }
        }

        self.expect_simple_token(Token::BraceClose).await?;
        Ok((location, map))
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

    async fn expect_string(&mut self) -> Result<(Location, Interned<str>), Error> {
        match self.next().await {
            Some(Ok((location, tok))) => match tok {
                Token::String(s) => Ok((location, s)),
                Token::Identifier(s) => Ok((location, s)),
                _ => Err(Error::SyntaxError(
                    location,
                    format!("unexpected {}, expected a string", tok),
                )),
            },
            Some(Err(err)) => Err(err),
            None => Err(Error::SyntaxError(
                self.lexer.location,
                "unexpected end of input, expected a string".into(),
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
        loop {
            match self.lexer.next().await {
                Some(Ok((_, Token::Comment))) => continue,
                Some(result) => {
                    return Some(result.map_err(Error::from));
                }
                None => {
                    return None;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::collections::Interned;
    use crate::lang::ast::PropertyValue;
    use futures::{executor, io::Cursor};

    fn assert_syntax_err(result: Result<Package, Error>, location: (usize, usize), msg: &str) {
        assert!(matches!(result, Err(Error::SyntaxError(_, _))));
        if let Err(Error::SyntaxError(loc, m)) = result {
            assert_eq!(location.0, loc.line);
            assert_eq!(location.1, loc.column);
            assert_eq!(msg, m);
        }
    }

    fn assert_str(interner: &Interner<str>, s: &str, interned: Interned<str>) {
        assert_eq!(Some(true), interner.eq(s, interned));
    }

    fn assert_name(interner: &Interner<str>, s: &str, name: &Name) {
        for (expected, actual) in s.split(".").zip(name.parts.iter()) {
            assert_str(interner, expected, *actual);
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
    fn empty_package() {
        let mut p = Parser::new(Cursor::new("pkg hello.world"));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);
    }

    #[test]
    fn simple_rule() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            simple {}
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &rule.condition {
                assert_name(&p.lexer.interner, "simple", ident);
            }
            assert!(matches!(rule.children, None));
        }
    }

    #[test]
    fn simple_prop() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            prop.name: 12345
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let prop = &pkg.children.unwrap()[0];
        assert!(matches!(prop, RuleOrProperty::Property(_)));
        if let RuleOrProperty::Property(prop) = prop {
            assert_name(&p.lexer.interner, "prop.name", &prop.name);
            assert!(matches!(prop.value, PropertyValue::String(_)));
            if let PropertyValue::String(value) = prop.value {
                assert_str(&p.lexer.interner, "12345", value);
            }
        }
    }

    #[test]
    fn multiple_simple_props() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            prop.name1: 12345
            prop.name2: "hello!"
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let children = &pkg.children.unwrap();
        let prop = &children[0];
        assert!(matches!(prop, RuleOrProperty::Property(_)));
        if let RuleOrProperty::Property(prop) = prop {
            assert_name(&p.lexer.interner, "prop.name1", &prop.name);
            assert!(matches!(prop.value, PropertyValue::String(_)));
            if let PropertyValue::String(value) = prop.value {
                assert_str(&p.lexer.interner, "12345", value);
            }
        }

        let prop = &children[1];
        assert!(matches!(prop, RuleOrProperty::Property(_)));
        if let RuleOrProperty::Property(prop) = prop {
            assert_name(&p.lexer.interner, "prop.name2", &prop.name);
            assert!(matches!(prop.value, PropertyValue::String(_)));
            if let PropertyValue::String(value) = prop.value {
                assert_str(&p.lexer.interner, "hello!", value);
            }
        }
    }

    #[test]
    fn recursive_rule() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            level1 { level2 {} }
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &rule.condition {
                assert_name(&p.lexer.interner, "level1", ident);
            }
            assert!(matches!(rule.children, Some(_)));

            if let Some(children) = &rule.children {
                let rule = &children[0];
                assert!(matches!(rule, RuleOrProperty::Rule(_)));
                if let RuleOrProperty::Rule(rule) = rule {
                    assert!(matches!(rule.condition, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &rule.condition {
                        assert_name(&p.lexer.interner, "level2", ident);
                    }
                    assert!(matches!(rule.children, None));
                }
            }
        }
    }

    #[test]
    fn recursive_rule_with_props() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            level1 {
                level1.prop: hello
                level2 {
                    level2.prop: world
                }
            }
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Fact(_, None)));
            if let Condition::Fact(ident, _) = &rule.condition {
                assert_name(&p.lexer.interner, "level1", ident);
            }
            assert!(matches!(rule.children, Some(_)));

            if let Some(children) = &rule.children {
                let prop = &children[0];
                assert!(matches!(prop, RuleOrProperty::Property(_)));
                if let RuleOrProperty::Property(prop) = prop {
                    assert_name(&p.lexer.interner, "level1.prop", &prop.name);
                    assert!(matches!(prop.value, PropertyValue::String(_)));
                    if let PropertyValue::String(value) = prop.value {
                        assert_str(&p.lexer.interner, "hello", value);
                    }
                }

                let rule = &children[1];
                assert!(matches!(rule, RuleOrProperty::Rule(_)));
                if let RuleOrProperty::Rule(rule) = rule {
                    assert!(matches!(rule.condition, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &rule.condition {
                        assert_name(&p.lexer.interner, "level2", ident);
                    }
                    assert!(matches!(rule.children, Some(_)));
                    if let Some(children) = &rule.children {
                        let prop = &children[0];
                        assert!(matches!(prop, RuleOrProperty::Property(_)));
                        if let RuleOrProperty::Property(prop) = prop {
                            assert_name(&p.lexer.interner, "level2.prop", &prop.name);
                            assert!(matches!(prop.value, PropertyValue::String(_)));
                            if let PropertyValue::String(value) = prop.value {
                                assert_str(&p.lexer.interner, "world", value);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn explicit_and() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            lhs and rhs { } 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::And(_, _)));
            if let Condition::And(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "lhs", ident);
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn implicit_and() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            lhs rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::And(_, _)));
            if let Condition::And(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "lhs", ident);
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn explicit_or() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            lhs or rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Or(_, _)));
            if let Condition::Or(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "lhs", ident);
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn comma_or() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            lhs, rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Or(_, _)));
            if let Condition::Or(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "lhs", ident);
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn explicit_xor() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            lhs xor rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Xor(_, _)));
            if let Condition::Xor(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "lhs", ident);
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn not() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            not simple {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Not(_)));
            if let Condition::Not(rule) = &rule.condition {
                assert!(matches!(**rule, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rule {
                    assert_name(&p.lexer.interner, "simple", ident);
                }
            }
        }
    }

    #[test]
    fn bang() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            !simple {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Not(_)));
            if let Condition::Not(rule) = &rule.condition {
                assert!(matches!(**rule, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rule {
                    assert_name(&p.lexer.interner, "simple", ident);
                }
            }
        }
    }

    #[test]
    fn parens() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            (lhs rhs) {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::And(_, _)));
            if let Condition::And(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "lhs", ident);
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn precendence1() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            one or two and three {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Or(_, _)));
            if let Condition::Or(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "one", ident);
                }
                assert!(matches!(**rhs, Condition::And(_, _)));
                if let Condition::And(lhs, rhs) = &**rhs {
                    assert!(matches!(**lhs, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**lhs {
                        assert_name(&p.lexer.interner, "two", ident);
                    }
                    assert!(matches!(**rhs, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**rhs {
                        assert_name(&p.lexer.interner, "three", ident);
                    }
                }
            }
        }
    }

    #[test]
    fn precendence2() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            one or two xor three {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Or(_, _)));
            if let Condition::Or(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**lhs {
                    assert_name(&p.lexer.interner, "one", ident);
                }
                assert!(matches!(**rhs, Condition::Xor(_, _)));
                if let Condition::Xor(lhs, rhs) = &**rhs {
                    assert!(matches!(**lhs, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**lhs {
                        assert_name(&p.lexer.interner, "two", ident);
                    }
                    assert!(matches!(**rhs, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**rhs {
                        assert_name(&p.lexer.interner, "three", ident);
                    }
                }
            }
        }
    }

    #[test]
    fn precendence3() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            not lhs or rhs {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Or(_, _)));
            if let Condition::Or(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Not(_)));
                if let Condition::Not(rule) = &**lhs {
                    assert!(matches!(**rule, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**rule {
                        assert_name(&p.lexer.interner, "lhs", ident);
                    }
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "rhs", ident);
                }
            }
        }
    }

    #[test]
    fn precendence_parens() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            (one or two) xor three {} 
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Xor(_, _)));
            if let Condition::Xor(lhs, rhs) = &rule.condition {
                assert!(matches!(**lhs, Condition::Or(_, _)));
                if let Condition::Or(lhs, rhs) = &**lhs {
                    assert!(matches!(**lhs, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**lhs {
                        assert_name(&p.lexer.interner, "one", ident);
                    }
                    assert!(matches!(**rhs, Condition::Fact(_, None)));
                    if let Condition::Fact(ident, _) = &**rhs {
                        assert_name(&p.lexer.interner, "two", ident);
                    }
                }
                assert!(matches!(**rhs, Condition::Fact(_, None)));
                if let Condition::Fact(ident, _) = &**rhs {
                    assert_name(&p.lexer.interner, "three", ident);
                }
            }
        }
    }

    #[test]
    fn one_arg() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            simple[arg0] {}
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Fact(_, Some(_))));
            if let Condition::Fact(ident, args) = &rule.condition {
                assert_name(&p.lexer.interner, "simple", ident);
                if let Some(args) = args {
                    assert_str(&p.lexer.interner, "arg0", args[0]);
                }
            }
            assert!(matches!(rule.children, None));
        }
    }

    #[test]
    fn multiple_args() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg hello.world
            
            simple[arg0, "arg1", 12345] {}
            "#,
        ));
        let pkg = executor::block_on(p.parse()).unwrap();
        assert_name(&p.lexer.interner, "hello.world", &pkg.name);

        let rule = &pkg.children.unwrap()[0];
        assert!(matches!(rule, RuleOrProperty::Rule(_)));
        if let RuleOrProperty::Rule(rule) = rule {
            assert!(matches!(rule.condition, Condition::Fact(_, Some(_))));
            if let Condition::Fact(ident, args) = &rule.condition {
                assert_name(&p.lexer.interner, "simple", ident);
                if let Some(args) = args {
                    assert_str(&p.lexer.interner, "arg0", args[0]);
                    assert_str(&p.lexer.interner, "arg1", args[1]);
                    assert_str(&p.lexer.interner, "12345", args[2]);
                }
            }
            assert!(matches!(rule.children, None));
        }
    }

    #[test]
    fn accepts1() {
        let mut p = Parser::new(Cursor::new(
            r#"
            # Is it the weekend?
            pkg test.package
            
            weekend: false
            
            saturday, sunday {
                weekend: true
            }
            "#,
        ));
        assert!(matches!(executor::block_on(p.parse()), Ok(_)));
    }

    #[test]
    fn accepts2() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg test.package weekend:false saturday,sunday{weekend:true}
            "#,
        ));
        assert!(matches!(executor::block_on(p.parse()), Ok(_)));
    }

    #[test]
    fn accepts3() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg test.package
            
            test: [one, two, three]
            "#,
        ));
        assert!(matches!(executor::block_on(p.parse()), Ok(_)));
    }

    #[test]
    fn accepts4() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg test.package
            
            test: { hello: goodbye }
            "#,
        ));
        assert!(matches!(executor::block_on(p.parse()), Ok(_)));
    }

    #[test]
    fn accepts5() {
        let mut p = Parser::new(Cursor::new(
            r#"
            pkg test.package
            
            test: { hello: [one, two, three, four] }
            "#,
        ));
        assert!(matches!(executor::block_on(p.parse()), Ok(_)));
    }
}
