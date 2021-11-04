use fxhash::FxHashMap;

use crate::collections::Interned;

// FIXME(lavignes): The AST is a real tree right now, but needs to migrate
//   to a pool data structure.
#[derive(Debug)]
pub enum Condition {
    Fact(Name, Option<Vec<Primitive>>),
    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
    Xor(Box<Condition>, Box<Condition>),
    Not(Box<Condition>),
}

#[derive(Debug)]
pub enum Primitive {
    String(Interned<str>),
    Int(i64),
    UInt(u64),
    Float(f64),
}

#[derive(Debug)]
pub enum PropertyValue {
    Primitive(Primitive),
    Array(Vec<PropertyValue>),
    Map(FxHashMap<Interned<str>, PropertyValue>),
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
