use std::collections::HashMap;

use lexer::Lexer;
use parser::{Parser, Procedure, Value};

fn add_proc(args: &[Value]) -> Result<Value, String> {
    let mut result = 0;

    for v in args.iter() {
        match v {
            &Value::Integer(i) => result += i,
            v => return Err(format!("Cannot apply non-integer {} to +", v))
        }
    }

    Ok(Value::Integer(result))
}

fn sub_proc(args: &[Value]) -> Result<Value, String> {
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

fn mul_proc(args: &[Value]) -> Result<Value, String> {
    let mut result = 1;

    for v in args.iter() {
        match v {
            &Value::Integer(i) => result *= i,
            v => return Err(format!("Cannot apply non-integer {} to *", v))
        }
    }

    Ok(Value::Integer(result))
}

fn car_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("car requires 1 argument".to_owned())
    }

    match &args[0] {
        &Value::List(ref vs) => Ok(vs[0].clone()),
        &Value::DottedList(ref vs) => Ok(vs[0].clone()),
        _ => Err("car requires list".to_owned())
    }
}

fn cdr_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cdr requires 1 argument".to_owned())
    }

    match &args[0] {
        &Value::List(ref vs) if vs.is_empty() =>
            Err("Cannot take cdr of empty list".to_owned()),
        &Value::List(ref vs) => Ok(Value::List(vs[1..].to_vec())),
        &Value::DottedList(ref vs) if vs.len() == 2 => Ok(vs[1].clone()),
        &Value::DottedList(ref vs) => Ok(Value::List(vs[1..].to_vec())),
        _ => Err("cdr requires list".to_owned())
    }
}

fn cons_proc(args: &[Value]) -> Result<Value, String> {
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

fn quote_exp(args: &[Value], _: &Environment) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("quote requires 1 argument".to_owned())
    }

    Ok(args[0].clone())
}

fn if_exp(args: &[Value], env: &Environment) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("quote requires 2 or 3 arguments".to_owned())
    }

    match eval(&args[0], env)? {
        Value::Boolean(false) => if args.len() == 2 {
            Ok(Value::List(Vec::new()))
        } else {
            eval(&args[2], env)
        },
        _ => eval(&args[1], env)
    }
}

fn eval_list(vs: &[Value], env: &Environment) -> Result<Value, String> {
    let f = &eval(&vs[0], env)?;
    let args = &vs[1..];

    match f {
        &Value::Symbol(ref s) => match s.as_str() {
            "quote" => quote_exp(args, env),
            "if" => if_exp(args, env),
            _ => Err(format!("Invalid application to symbol {}", s))
        },
        &Value::Procedure(Procedure::Builtin(f)) => {
            let args: Result<Vec<Value>, String> = args.iter().map(|arg| eval(arg, env)).collect();

            return f(&args?[..]);
        },
        &Value::Procedure(_) => Err(format!("Not yet implemented")),
        v => Err(format!("Invalid application to {}", v))
    }
}

pub fn eval(value: &Value, env: &Environment) -> Result<Value, String> {
    match value {
        &Value::List(ref vs) if vs.is_empty() => Ok(Value::List(Vec::new())),
        &Value::List(ref vs) => eval_list(vs, env),
        &Value::DottedList(_) => Err("Cannot evaluate dotted list".to_owned()),
        &Value::Boolean(b) => Ok(Value::Boolean(b.clone())),
        &Value::Symbol(ref s) => match s.as_str() {
            "quote" => Ok(value.clone()),
            "if" => Ok(value.clone()),
            _ => match env.kvs.get(s) {
                Some(v) => Ok(v.clone()),
                None => Err(format!("Unbound variable {}", s))
            }
        },
        &Value::Integer(i) => Ok(Value::Integer(i.clone())),
        &Value::String(ref s) => Ok(Value::String(s.clone())),
        &Value::Procedure(ref p) => Ok(Value::Procedure(p.clone()))
    }
}

pub struct Environment {
    kvs: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        let mut tmp = HashMap::new();

        tmp.insert("+".to_owned(), Value::Procedure(Procedure::Builtin(add_proc)));
        tmp.insert("-".to_owned(), Value::Procedure(Procedure::Builtin(sub_proc)));
        tmp.insert("*".to_owned(), Value::Procedure(Procedure::Builtin(mul_proc)));
        tmp.insert("car".to_owned(), Value::Procedure(Procedure::Builtin(car_proc)));
        tmp.insert("cdr".to_owned(), Value::Procedure(Procedure::Builtin(cdr_proc)));
        tmp.insert("cons".to_owned(), Value::Procedure(Procedure::Builtin(cons_proc)));

        Environment {
            kvs: tmp
        }
    }
}

#[allow(dead_code)]
fn rep(sexp: &str) -> String {
    let lexer = Lexer::new(sexp);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse().expect("Failed to parse");
    let env = Environment::new();
    let result = eval(&parsed, &env).expect("Failed to evaluate");

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
