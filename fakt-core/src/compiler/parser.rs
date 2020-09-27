use lalrpop_util::ParseError;

use fakt::PackageParser;

use std::error;

use crate::compiler::ast::{Condition, Package};
use std::fmt::{self, Display, Formatter};

lalrpop_util::lalrpop_mod!(fakt);

#[derive(Debug)]
pub struct Error(String);

impl Display for Error {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl error::Error for Error {
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl<L, T, E> From<ParseError<L, T, E>> for Error
where
    L: fmt::Display,
    T: fmt::Display,
    E: fmt::Display,
{
    fn from(err: ParseError<L, T, E>) -> Error {
        Error(format!("{}", err))
    }
}

pub struct Parser {
    inner: PackageParser,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            inner: PackageParser::new(),
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<Package, Error> {
        self.inner.parse(input).map_err(Error::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse_err(result: Result<Package, Error>, msg: &str) {
        match result {
            Err(err) if err.0 == msg => {}
            other => panic!("Unexpected result: {:?}", other),
        }
    }

    #[test]
    pub fn empty() {
        let mut p = Parser::new();
        assert_parse_err(
            p.parse(""),
            "Unrecognized EOF found at 0\nExpected one of \"pkg\"",
        );
    }

    #[test]
    pub fn bad_pkg() {
        let mut p = Parser::new();
        assert_parse_err(
            p.parse("pkg"),
            "Unrecognized EOF found at 3\nExpected one of identifier",
        );

        assert_parse_err(
            p.parse("pkg 2;"),
            "Unrecognized token `2` found at 4:5\nExpected one of identifier",
        );

        assert_parse_err(
            p.parse("pkg shoe.2;"),
            "Unrecognized token `shoe.2` found at 4:10\nExpected one of identifier",
        );
    }

    #[test]
    pub fn minimal_pkg() {
        let p = Parser::new().parse("pkg hello;").unwrap();
        assert_eq!("hello", p.name);
        assert!(p.rules.is_empty());
    }

    #[test]
    pub fn bad_rules() {
        let mut p = Parser::new();
        assert_parse_err(p.parse(r##"
            pkg hello;
            2
        "##), "Unrecognized token `2` found at 36:37\nExpected one of \"(\", \"never\", \"not\" or identifier");

        let mut p = Parser::new();
        assert_parse_err(p.parse(r##"
            pkg hello;
            foo.2 {}
        "##), "Unrecognized token `foo.2` found at 36:41\nExpected one of \"(\", \"never\", \"not\" or identifier");
    }

    #[test]
    pub fn simple_rule() {
        let p = r##"
            pkg hello;
            
            test {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Fact(name, None) if name == "test"));
    }

    #[test]
    pub fn rule_args() {
        let p = r##"
            pkg hello;
            
            test(arg) {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Fact(_, Some(_))));
        if let Condition::Fact(name, Some(args)) = &p.rules[0].condition {
            assert_eq!("test", name);
            assert_eq!("arg", args[0]);
        }

        let p = r##"
            pkg hello;
            
            test("quoted") {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Fact(_, Some(_))));
        if let Condition::Fact(name, Some(args)) = &p.rules[0].condition {
            assert_eq!("test", name);
            assert_eq!("quoted", args[0]);
        }

        let p = r##"
            pkg hello;
            
            test(1234) {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Fact(_, Some(_))));
        if let Condition::Fact(name, Some(args)) = &p.rules[0].condition {
            assert_eq!("test", name);
            assert_eq!("1234", args[0]);
        }

        let p = r##"
            pkg hello;
            
            test(hello, "strings! ", 12345) {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Fact(_, Some(_))));
        if let Condition::Fact(name, Some(args)) = &p.rules[0].condition {
            assert_eq!("test", name);
            assert_eq!("hello", args[0]);
            assert_eq!("strings! ", args[1]);
            assert_eq!("12345", args[2]);
        }
    }

    #[test]
    pub fn and_rule() {
        let p = r##"
            pkg hello;
            
            test1 and test2 {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::And(_, _)));
        if let Condition::And(lhs, rhs) = &p.rules[0].condition {
            assert!(matches!(&**lhs, Condition::Fact(name, None) if name == "test1"));
            assert!(matches!(&**rhs, Condition::Fact(name, None) if name == "test2"));
        }
    }

    #[test]
    pub fn or_rule() {
        let p = r##"
            pkg hello;
            
            test1 or test2 {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Or(_, _)));
        if let Condition::Or(lhs, rhs) = &p.rules[0].condition {
            assert!(matches!(&**lhs, Condition::Fact(name, None) if name == "test1"));
            assert!(matches!(&**rhs, Condition::Fact(name, None) if name == "test2"));
        }
    }

    #[test]
    pub fn xor_rule() {
        let p = r##"
            pkg hello;
            
            test1 xor test2 {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Xor(_, _)));
        if let Condition::Xor(lhs, rhs) = &p.rules[0].condition {
            assert!(matches!(&**lhs, Condition::Fact(name, None) if name == "test1"));
            assert!(matches!(&**rhs, Condition::Fact(name, None) if name == "test2"));
        }
    }

    #[test]
    pub fn not_rule() {
        let p = r##"
            pkg hello;
            
            not funny {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Not(_)));
        if let Condition::Not(cond) = &p.rules[0].condition {
            assert!(matches!(&**cond, Condition::Fact(name, None) if name == "funny"));
        }
    }

    #[test]
    pub fn never_rule() {
        let p = r##"
            pkg hello;
            
            never {}
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Never));
    }

    #[test]
    pub fn nest_rules() {
        let p = r##"
            pkg hello;
            
            test { inner and more {} }
        "##;
        let p = Parser::new().parse(p).unwrap();
        assert_eq!("hello", p.name);
        assert!(matches!(&p.rules[0].condition, Condition::Fact(name, None) if name == "test"));
        if let Condition::Fact(name, None) = &p.rules[0].condition {
            assert!(p.rules[0].children.is_some());
            let children = p.rules[0].children.as_ref().unwrap();
            assert!(matches!(&children[0].condition, Condition::And(_, _)));
            if let Condition::And(lhs, rhs) = &children[0].condition {
                assert!(matches!(&**lhs, Condition::Fact(name, None) if name == "inner"));
                assert!(matches!(&**rhs, Condition::Fact(name, None) if name == "more"));
            }
        }
    }
}
