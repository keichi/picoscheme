mod lexer;
mod parser;
use lexer::Lexer;
use parser::{Parser, Value};

fn add(args: &[Value]) -> Result<Value, String> {
    let mut result = 0;

    for v in args.iter() {
        match &eval(v)? {
            &Value::Integer(i) => result += i,
            v => return Err(format!("Cannot apply non-integer {} to +", v))
        }
    }

    Ok(Value::Integer(result))
}

fn sub(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("- requires at least one argument".to_owned())
    } else if args.len() == 1 {
        match &eval(&args[0])? {
            &Value::Integer(i) => return Ok(Value::Integer(-i)),
            v => return Err(format!("Cannot apply non-integer {} to -", v))
        }
    }

    let mut result: i64;

    match &eval(&args[0])? {
        &Value::Integer(i) => result = i,
        v => return Err(format!("Cannot apply non-integer {} to -", v))
    }

    for v in &args[1..] {
        match &eval(v)? {
            &Value::Integer(i) => result -= i,
            v => return Err(format!("Cannot apply non-integer {} to -", v))
        }
    }

    Ok(Value::Integer(result))
}

fn mul(args: &[Value]) -> Result<Value, String> {
    let mut result = 1;

    for v in args.iter() {
        match &eval(v)? {
            &Value::Integer(i) => result *= i,
            v => return Err(format!("Cannot apply non-integer {} to *", v))
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
    let cdr = eval(&args[1])?;

    match &cdr {
        &Value::List(ref vs) => {
            let mut list = vec![car];
            list.extend_from_slice(&vs);
            return Ok(Value::List(list));
        },
        &Value::DottedList(ref vs) => {
            let mut list = vec![car];
            list.extend_from_slice(&vs);
            return Ok(Value::DottedList(list));
        },
        v => Ok(Value::DottedList(vec![car, v.clone()]))
    }
}

fn apply(f: &Value, args: &[Value]) -> Result<Value, String> {
    match f {
        &Value::Symbol(ref s) => match s.as_str() {
            "+" => add(args),
            "-" => sub(args),
            "*" => mul(args),
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

fn eval_str(sexp: &str) -> String {
    let lexer = Lexer::new(sexp);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse().expect("Failed to parse");
    let result = eval(&parsed).expect("Failed to evaluate");

    format!("{}", result)
}

#[test]
fn test_literal() {
    assert_eq!(eval_str("'\"abc\""), "\"abc\"");
    assert_eq!(eval_str("\"abc\""), "\"abc\"");
    assert_eq!(eval_str("'145932"), "145932");
    assert_eq!(eval_str("145932"), "145932");
    assert_eq!(eval_str("'#t"), "#t");
    assert_eq!(eval_str("#t"), "#t");
}

#[test]
fn test_basic_arithmetic() {
    assert_eq!(eval_str("(+ 3 4)"), "7");
    assert_eq!(eval_str("(+ 3)"), "3");
    assert_eq!(eval_str("(+)"), "0");

    assert_eq!(eval_str("(* 4)"), "4");
    assert_eq!(eval_str("(*)"), "1");

    assert_eq!(eval_str("(- 3 4)"), "-1");
    assert_eq!(eval_str("(- 3 4 5)"), "-6");
    assert_eq!(eval_str("(- 3)"), "-3");
}

#[test]
fn test_cons() {
    assert_eq!(eval_str("(cons 'a '())"), "(a)");
    assert_eq!(eval_str("(cons '(a) '(b c d))"), "((a) b c d)");
    assert_eq!(eval_str("(cons \"a\" '(b c))"), "(\"a\" b c)");
    assert_eq!(eval_str("(cons 'a 3)"), "(a . 3)");
    assert_eq!(eval_str("(cons '(a b) 'c)"), "((a b) . c)");
}

#[test]
fn test_car() {
    assert_eq!(eval_str("(car '(a b c))"), "a");
    assert_eq!(eval_str("(car '((a) b c d))"), "(a)");
    assert_eq!(eval_str("(car '(1 . 2))"), "1");
}

#[test]
fn test_cdr() {
    assert_eq!(eval_str("(cdr '((a) b c d))"), "(b c d)");
    assert_eq!(eval_str("(cdr '(1 . 2))"), "2");
}

#[test]
fn test_quote() {
    assert_eq!(eval_str("(quote a)"), "a");
    assert_eq!(eval_str("(quote (+ 1 2))"), "(+ 1 2)");
    assert_eq!(eval_str("'a"), "a");
    assert_eq!(eval_str("'()"), "()");
    assert_eq!(eval_str("'(+ 1 2)"), "(+ 1 2)");
    assert_eq!(eval_str("'(quote a)"), "(quote a)");
    assert_eq!(eval_str("''a"), "(quote a)");
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
