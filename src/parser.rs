use std::iter;

#[derive(Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Pair(Box<Value>, Box<Value>),
    Symbol(String),
    Integer(i64),
    String(String)
}

pub struct Parser {
}

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <string> | <symbol>
// <symbol> -> <identifier>
// <compound datum> -> <list>
// <list> -> ( <datum>* ) | ( <datum>+ . <datum> ) | <abbreviation>
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
