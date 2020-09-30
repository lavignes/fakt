use crate::collections::Interned;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Condition {
    Fact(Name, Option<Vec<Interned<str>>>),
    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
    Xor(Box<Condition>, Box<Condition>),
    Not(Box<Condition>),
}

#[derive(Debug)]
pub enum PropertyValue {
    Bool(bool),
    String(Interned<str>),
    Array(Vec<Interned<str>>),
    Map(HashMap<Interned<str>, PropertyValue>),
}

#[derive(Debug)]
pub struct Property {
    pub(crate) name: Name,
    pub(crate) value: PropertyValue,
}

#[derive(Debug)]
pub enum RuleOrProperty {
    Rule(Rule),
    Property(Property),
}

#[derive(Debug)]
pub struct Rule {
    pub(crate) condition: Condition,
    pub(crate) children: Option<Vec<RuleOrProperty>>,
}

#[derive(Debug)]
pub struct Package {
    pub(crate) name: Name,
    pub(crate) children: Option<Vec<RuleOrProperty>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Name {
    pub(crate) parts: Vec<Interned<str>>,
}
