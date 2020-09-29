use std::collections::HashMap;

#[derive(Debug)]
pub enum Condition {
    Fact(String, Option<Vec<String>>),
    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
    Xor(Box<Condition>, Box<Condition>),
    Not(Box<Condition>),
}

#[derive(Debug)]
pub enum PropertyValue {
    Bool(bool),
    String(String),
    Array(Vec<String>),
    Map(HashMap<String, PropertyValue>),
}

#[derive(Debug)]
pub struct Property {
    pub(crate) name: String,
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
    pub(crate) name: String,
    pub(crate) rules: Vec<Rule>,
}
