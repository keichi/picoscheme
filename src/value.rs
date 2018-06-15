use std::fmt;
use std::rc::Rc;

use environment::Environment;

pub type BuiltinFunc = fn (args: &[Value]) -> Result<Value, String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    List(Vec<Value>),
    DottedList(Vec<Value>),
    Boolean(bool),
    Symbol(String),
    Integer(i64),
    String(String),
    Procedure(Procedure),
}

#[derive(Clone)]
pub enum Procedure {
    Builtin(BuiltinFunc),
    Scheme(Vec<Value>, Option<Box<Value>>, Rc<Environment>, Vec<Value>)
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Procedure::Builtin(_) => write!(f, "Builtin"),
            &Procedure::Scheme(_, _, _, _) => write!(f, "Scheme")
        }
    }
}

impl PartialEq for Procedure {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::List(ref vs) => {
                if vs.len() > 1 {
                    if let &Value::Symbol(ref s) = &vs[0] {
                        match s.as_str() {
                            "quote" => return write!(f, "'{}", vs[1]),
                            "quasiquote" => return write!(f, "`{}", vs[1]),
                            "unquote" => return write!(f, ",{}", vs[1]),
                            "unquote-splicing" => return write!(f, ",@{}", vs[1]),
                            _ => {}
                        }
                    }
                }

                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    if i >= 1 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            },
            &Value::DottedList(ref vs) => {
                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    if i == vs.len() - 1 {
                        write!(f, " . ")?;
                    } else if i >= 1 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            },
            &Value::Boolean(true) => write!(f, "#t"),
            &Value::Boolean(false) => write!(f, "#f"),
            &Value::Symbol(ref s) => write!(f, "{}", s),
            &Value::Integer(i) => write!(f, "{}", i),
            &Value::String(ref s) => write!(f, "\"{}\"", s),
            &Value::Procedure(_) => write!(f, "#<procedure>")
        }
    }
}

