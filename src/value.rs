use std::fmt;

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
    Builtin(fn(&[Value]) -> Result<Value, String>),
    Scheme(Vec<Value>, Vec<Value>)
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Procedure::Builtin(_) => write!(f, "Builtin"),
            Procedure::Scheme(_, _) => write!(f, "Scheme")
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
                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    if i >= 1 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")?;
                return Ok(());
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
                write!(f, ")")?;
                return Ok(());
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

