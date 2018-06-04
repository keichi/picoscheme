mod lexer;
mod parser;
use lexer::Lexer;
use parser::{Parser, Value};

fn add(args: &[Value]) -> Result<Value, String> {
    let mut result = 0;

    for v in args.iter() {
        match &eval(v)? {
            &Value::Integer(i) => result += i,
            v => return Err(format!("Cannot add non-integer {}", v))
        }
    }

    Ok(Value::Integer(result))
}

fn car(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("car requires 1 argument".to_owned())
    }

    match &eval(&args[0])? {
        &Value::List(ref vs) => Ok(vs[0].clone()),
        &Value::DottedList(ref vs) => Ok(vs[0].clone()),
        _ => Err("car requires list".to_owned())
    }
}

fn cdr(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cdr requires 1 argument".to_owned())
    }

    match &eval(&args[0])? {
        &Value::List(ref vs) if vs.is_empty() =>
            Err("Cannot take cdr of empty list".to_owned()),
        &Value::List(ref vs) => Ok(Value::List(vs[1..].to_vec())),
        &Value::DottedList(ref vs) if vs.len() == 2 => Ok(vs[1].clone()),
        &Value::DottedList(ref vs) => Ok(Value::List(vs[1..].to_vec())),
        _ => Err("car requires list".to_owned())
    }
}

fn quote(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("quote requires 1 argument".to_owned())
    }

    Ok(args[0].clone())
}

fn cons(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cons requires 2 arguments".to_owned())
    }

    let car = eval(&args[0])?;
    let mut cdr = eval(&args[1])?;

    match &mut cdr {
        &mut Value::List(ref mut vs) => {
            let mut list = vec![car];
            list.append(vs);
            return Ok(Value::List(list));
        },
        &mut Value::DottedList(ref mut vs) => {
            let mut list = vec![car];
            list.append(vs);
            return Ok(Value::DottedList(list));
        },
        v => Ok(Value::DottedList(vec![car, v.clone()]))
    }
}

fn apply(f: &Value, args: &[Value]) -> Result<Value, String> {
    match f {
        &Value::Symbol(ref s) => match s.as_str() {
            "+" => add(args),
            "car" => car(args),
            "cdr" => cdr(args),
            "cons" => cons(args),
            "quote" => quote(args),
            s => Err(format!("Unbound variable {}", s))
        },
        v => Err(format!("Invalid application to {}", v))
    }
}

fn eval(value: &Value) -> Result<Value, String> {
    match value {
        &Value::List(ref vs) if vs.is_empty() => Ok(Value::List(Vec::new())),
        &Value::List(ref vs) => apply(&vs[0], &vs[1..]),
        &Value::DottedList(_) => Err("Cannot evaluate dotted list".to_owned()),
        &Value::Boolean(b) => Ok(Value::Boolean(b.clone())),
        &Value::Symbol(ref s) => Ok(Value::Symbol(s.clone())),
        &Value::Integer(i) => Ok(Value::Integer(i.clone())),
        &Value::String(ref s) => Ok(Value::String(s.clone()))
    }
}

fn main() {
    let lexer = Lexer::new("(cons 1 '(2 . 3))");
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
