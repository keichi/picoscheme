use std::collections::HashMap;

use builtin::*;
use lexer::Lexer;
use parser::{Parser, Procedure, Value};

fn quote_exp(args: &[Value], _: &Environment) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("quote requires 1 argument".to_owned())
    }

    Ok(args[0].clone())
}

fn if_exp(args: &[Value], env: &mut Environment) -> Result<Value, String> {
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

fn lambda_exp(args: &[Value], _env: &mut Environment) -> Result<Value, String> {
    if args.len() < 2 {
        return Err("lambda requires more than 2 arguments".to_owned());
    }

    let vars = match &args[0] {
        &Value::List(ref vs) => vs.clone(),
        _ => return Err("First argument of lambda must be a list".to_owned())
    };

    let body = args[1..].to_vec();

    Ok(Value::Procedure(Procedure::Scheme(vars, body)))
}

fn eval_proc(vars: &[Value], body: &[Value], args: &[Value], env: &mut Environment) -> Result<Value, String> {
    if vars.len() != args.len() {
        return Err(format!("procedure requires {} arguments but {} supplied",
                           vars.len(), args.len()))
    }

    let mut new_env = env.clone();

    for (var, arg) in vars.iter().zip(args.iter()) {
        match var {
            &Value::Symbol(ref s) => {
                new_env.kvs.insert(s.to_owned(), arg.clone());
            },
            _ => {}
        }
    }

    return body.iter()
        .map(|e| eval(e, &mut new_env))
        .collect::<Result<Vec<Value>, String>>()
        .map(|rs| rs.last().unwrap().clone());
}

fn define_exp(args: &[Value], env: &mut Environment) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("lambda requires 2 arguments".to_owned());
    }

    let var = match &args[0] {
        &Value::Symbol(ref s) => s.clone(),
        _ => return Err("First argument of define must be a symbol".to_owned())
    };

    let exp = eval(&args[1], env)?;

    env.kvs.insert(var, exp.clone());

    return Ok(exp);
}

fn eval_list(vs: &[Value], env: &mut Environment) -> Result<Value, String> {
    let f = &eval(&vs[0], env)?;
    let args = &vs[1..];

    match f {
        &Value::Symbol(ref s) => match s.as_str() {
            "quote" => quote_exp(args, env),
            "if" => if_exp(args, env),
            "lambda" => lambda_exp(args, env),
            "define" => define_exp(args, env),
            _ => Err(format!("Invalid application to symbol {}", s))
        },
        &Value::Procedure(Procedure::Builtin(f)) => {
            let args: Result<Vec<Value>, String>
                = args.iter().map(|arg| eval(arg, env)).collect();

            return f(&args?[..]);
        },
        &Value::Procedure(Procedure::Scheme(ref vars, ref body)) => {
            let args: Result<Vec<Value>, String>
                = args.iter().map(|arg| eval(arg, env)).collect();

            return eval_proc(&vars[..], &body[..], &args?[..], env);
        },
        v => Err(format!("Invalid application to {}", v))
    }
}

pub fn eval(value: &Value, env: &mut Environment) -> Result<Value, String> {
    match value {
        &Value::List(ref vs) if vs.is_empty() => Ok(Value::List(Vec::new())),
        &Value::List(ref vs) => eval_list(vs, env),
        &Value::DottedList(_) => Err("Cannot evaluate dotted list".to_owned()),
        &Value::Boolean(b) => Ok(Value::Boolean(b.clone())),
        &Value::Symbol(ref s) => match s.as_str() {
            "quote" => Ok(value.clone()),
            "if" => Ok(value.clone()),
            "lambda" => Ok(value.clone()),
            "define" => Ok(value.clone()),
            _ => env.kvs.get(s)
                    .ok_or(format!("Unbound variable {}", s))
                    .map(|v| v.clone())
        },
        &Value::Integer(i) => Ok(Value::Integer(i.clone())),
        &Value::String(ref s) => Ok(Value::String(s.clone())),
        &Value::Procedure(ref p) => Ok(Value::Procedure(p.clone()))
    }
}

#[derive(Clone, Debug)]
pub struct Environment {
    kvs: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        let mut tmp = HashMap::new();

        tmp.insert("+".to_owned(), Value::Procedure(Procedure::Builtin(add_proc)));
        tmp.insert("-".to_owned(), Value::Procedure(Procedure::Builtin(sub_proc)));
        tmp.insert("*".to_owned(), Value::Procedure(Procedure::Builtin(mul_proc)));
        tmp.insert("=".to_owned(), Value::Procedure(Procedure::Builtin(eq_proc)));
        tmp.insert(">".to_owned(), Value::Procedure(Procedure::Builtin(gt_proc)));
        tmp.insert("<".to_owned(), Value::Procedure(Procedure::Builtin(lt_proc)));
        tmp.insert("car".to_owned(), Value::Procedure(Procedure::Builtin(car_proc)));
        tmp.insert("cdr".to_owned(), Value::Procedure(Procedure::Builtin(cdr_proc)));
        tmp.insert("cons".to_owned(), Value::Procedure(Procedure::Builtin(cons_proc)));

        Environment {
            kvs: tmp
        }
    }
}

#[allow(dead_code)]
pub fn rep(sexp: &str) -> String {
    let lexer = Lexer::new(sexp);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse().expect("Failed to parse");
    let mut env = Environment::new();
    let result = eval(&parsed, &mut env).expect("Failed to evaluate");

    format!("{}", result)
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
    assert_eq!(rep("(if (> 3 2) 'yes 'no)"), "yes");
    assert_eq!(rep("(if (> 2 3) 'yes 'no)"), "no");
    assert_eq!(rep("(if (> 3 2) (- 3 2) (+ 3 2))"), "1");
}

#[test]
fn test_lambda() {
    assert_eq!(rep("((lambda (x) (+ x x)) 4)"), "8");
}

#[test]
fn test_define() {
    assert_eq!(rep("(define a 1)"), "1");
}
