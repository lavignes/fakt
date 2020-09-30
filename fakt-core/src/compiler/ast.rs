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
    pub(crate) value: Option<PropertyValue>,
}

#[derive(Debug)]
pub struct Rule {
    pub(crate) condition: Condition,
    pub(crate) properties: Option<Vec<Property>>,
    pub(crate) children: Option<Vec<Rule>>,
}

#[derive(Debug)]
pub struct Package {
    pub(crate) name: Name,
    pub(crate) rules: Option<Vec<Rule>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Name {
    pub(crate) parts: Vec<Interned<str>>,
}
