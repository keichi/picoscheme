mod lexer;
mod parser;
use lexer::Lexer;
use parser::{Parser, Value};

fn add(args: &[Value]) -> Result<Value, String> {
    let mut result = 0;

    for v in args.into_iter() {
        match eval(v)? {
            Value::Integer(i) => result += i,
            v => return Err(format!("Cannot add non-integer {}", v))
        }
    }

    Ok(Value::Integer(result))
}

fn apply(f: &Value, args: &[Value]) -> Result<Value, String> {
    match f {
        Value::Symbol(s) => match s.as_str() {
            "+" => add(args),
            "-" => add(args),
            "*" => add(args),
            "/" => add(args),
            s => Err(format!("Unbound variable {}", s))
        },
        v => Err(format!("Invalid application to {}", v))
    }
}

fn eval(value: &Value) -> Result<Value, String> {
    match value {
        Value::List(ref vs) => {
            if vs.is_empty() {
                return Ok(Value::List(Vec::new()))
            }

            return apply(&vs[0], &vs[1..]);
        },
        Value::DottedList(_) => Err("Cannot evaluate dotted list".to_owned()),
        &Value::Boolean(b) => Ok(Value::Boolean(b.clone())),
        &Value::Symbol(ref s) => Ok(Value::Symbol(s.clone())),
        &Value::Integer(i) => Ok(Value::Integer(i.clone())),
        &Value::String(ref s) => Ok(Value::String(s.clone()))
    }
}

fn main() {
    let lexer = Lexer::new("(+ 1 2 3)");
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse();

    match parsed {
        Ok(v) => match eval(&v) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("Error: {}", e)
        },
        Err(e) => println!("Error {}", e)
    }
}
