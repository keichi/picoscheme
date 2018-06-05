use lexer::Lexer;
use parser::{Parser, Value};

fn add(args: &[Value]) -> Result<Value, String> {
    let mut result = 0;

    for v in args.iter() {
        match v {
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
        match &args[0] {
            &Value::Integer(i) => return Ok(Value::Integer(-i)),
            v => return Err(format!("Cannot apply non-integer {} to -", v))
        }
    }

    let mut result: i64;

    match &args[0] {
        &Value::Integer(i) => result = i,
        v => return Err(format!("Cannot apply non-integer {} to -", v))
    }

    for v in &args[1..] {
        match v {
            &Value::Integer(i) => result -= i,
            v => return Err(format!("Cannot apply non-integer {} to -", v))
        }
    }

    Ok(Value::Integer(result))
}

fn mul(args: &[Value]) -> Result<Value, String> {
    let mut result = 1;

    for v in args.iter() {
        match v {
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

    match &args[0] {
        &Value::List(ref vs) => Ok(vs[0].clone()),
        &Value::DottedList(ref vs) => Ok(vs[0].clone()),
        _ => Err("car requires list".to_owned())
    }
}

fn cdr(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cdr requires 1 argument".to_owned())
    }

    match &args[0] {
        &Value::List(ref vs) if vs.is_empty() =>
            Err("Cannot take cdr of empty list".to_owned()),
        &Value::List(ref vs) => Ok(Value::List(vs[1..].to_vec())),
        &Value::DottedList(ref vs) if vs.len() == 2 => Ok(vs[1].clone()),
        &Value::DottedList(ref vs) => Ok(Value::List(vs[1..].to_vec())),
        _ => Err("car requires list".to_owned())
    }
}

fn quote_expression(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("quote requires 1 argument".to_owned())
    }

    Ok(args[0].clone())
}

fn if_expression(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("quote requires 2 or 3 arguments".to_owned())
    }

    match eval(&args[0])? {
        Value::Boolean(false) => if args.len() == 2 {
            Ok(Value::List(Vec::new()))
        } else {
            eval(&args[2])
        },
        _ => eval(&args[1]),
    }
}


fn cons(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cons requires 2 arguments".to_owned())
    }

    let car = &args[0];
    let cdr = &args[1];

    match cdr {
        &Value::List(ref vs) => {
            let mut list = vec![car.clone()];
            list.extend_from_slice(&vs[..]);
            return Ok(Value::List(list));
        },
        &Value::DottedList(ref vs) => {
            let mut list = vec![car.clone()];
            list.extend_from_slice(&vs);
            return Ok(Value::DottedList(list));
        },
        v => Ok(Value::DottedList(vec![car.clone(), v.clone()]))
    }
}

fn apply(f: &Value, args: &[Value]) -> Result<Value, String> {
    match &eval(f)? {
        &Value::Symbol(ref s) => match s.as_str() {
            "quote" => quote_expression(args),
            "if" => if_expression(args),
            _ => {
                let args: Vec<_> = try!(args.iter().map(eval).collect());

                match s.as_str() {
                    "+" => add(&args[..]),
                    "-" => sub(&args[..]),
                    "*" => mul(&args[..]),
                    "car" => car(&args[..]),
                    "cdr" => cdr(&args[..]),
                    "cons" => cons(&args[..]),
                    s => Err(format!("Unbound variable {}", s))
                }
            },
        },
        v => Err(format!("Invalid application to {}", v))
    }
}

pub fn eval(value: &Value) -> Result<Value, String> {
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

#[allow(dead_code)]
fn rep(sexp: &str) -> String {
    let lexer = Lexer::new(sexp);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse().expect("Failed to parse");
    let result = eval(&parsed).expect("Failed to evaluate");

    format!("{}", result)
}

#[test]
fn test_literal() {
    assert_eq!(rep("'\"abc\""), "\"abc\"");
    assert_eq!(rep("\"abc\""), "\"abc\"");
    assert_eq!(rep("'145932"), "145932");
    assert_eq!(rep("145932"), "145932");
    assert_eq!(rep("'#t"), "#t");
    assert_eq!(rep("#t"), "#t");
}

#[test]
fn test_basic_arithmetic() {
    assert_eq!(rep("(+ 3 4)"), "7");
    assert_eq!(rep("(+ 3)"), "3");
    assert_eq!(rep("(+)"), "0");

    assert_eq!(rep("(* 4)"), "4");
    assert_eq!(rep("(*)"), "1");

    assert_eq!(rep("(- 3 4)"), "-1");
    assert_eq!(rep("(- 3 4 5)"), "-6");
    assert_eq!(rep("(- 3)"), "-3");
}

#[test]
fn test_cons() {
    assert_eq!(rep("(cons 'a '())"), "(a)");
    assert_eq!(rep("(cons '(a) '(b c d))"), "((a) b c d)");
    assert_eq!(rep("(cons \"a\" '(b c))"), "(\"a\" b c)");
    assert_eq!(rep("(cons 'a 3)"), "(a . 3)");
    assert_eq!(rep("(cons '(a b) 'c)"), "((a b) . c)");
}

#[test]
fn test_car() {
    assert_eq!(rep("(car '(a b c))"), "a");
    assert_eq!(rep("(car '((a) b c d))"), "(a)");
    assert_eq!(rep("(car '(1 . 2))"), "1");
}

#[test]
fn test_cdr() {
    assert_eq!(rep("(cdr '((a) b c d))"), "(b c d)");
    assert_eq!(rep("(cdr '(1 . 2))"), "2");
}

#[test]
fn test_quote() {
    assert_eq!(rep("(quote a)"), "a");
    assert_eq!(rep("(quote (+ 1 2))"), "(+ 1 2)");
    assert_eq!(rep("'a"), "a");
    assert_eq!(rep("'()"), "()");
    assert_eq!(rep("'(+ 1 2)"), "(+ 1 2)");
    assert_eq!(rep("'(quote a)"), "(quote a)");
    assert_eq!(rep("''a"), "(quote a)");
}

#[test]
fn test_if() {
    assert_eq!(rep("(if #t 1 2)"), "1");
    assert_eq!(rep("(if #f 1 2)"), "2")
}
