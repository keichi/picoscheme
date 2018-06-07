use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use builtin::*;
use value::{BuiltinFunc, Procedure, Value};

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

fn lambda_exp(args: &[Value], env: &Environment) -> Result<Value, String> {
    if args.len() < 2 {
        return Err("lambda requires more than 2 arguments".to_owned());
    }

    let vars = match &args[0] {
        &Value::List(ref vs) => vs.clone(),
        _ => return Err("First argument of lambda must be a list".to_owned())
    };

    let body = args[1..].to_vec();

    Ok(Value::Procedure(Procedure::Scheme(vars, env.clone(), body)))
}

fn eval_proc(vars: &[Value], body: &[Value], args: &[Value], env: &Environment) -> Result<Value, String> {
    if vars.len() != args.len() {
        return Err(format!("procedure requires {} arguments but {} supplied",
                           vars.len(), args.len()))
    }

    let new_env = env.clone();

    for (var, arg) in vars.iter().zip(args.iter()) {
        match var {
            &Value::Symbol(ref s) => {
                new_env.define(s, arg);
            },
            _ => {}
        }
    }

    return body.iter()
        .map(|e| eval(e, &new_env))
        .collect::<Result<Vec<Value>, String>>()
        .map(|rs| rs.last().unwrap().clone());
}

fn define_exp(args: &[Value], env: &Environment) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("define requires 2 arguments".to_owned());
    }

    let var = match &args[0] {
        &Value::Symbol(ref s) => s,
        _ => return Err("First argument of define must be a symbol".to_owned())
    };

    let exp = eval(&args[1], env)?;

    env.define(var, &exp);

    return Ok(exp);
}

fn set_exp(args: &[Value], env: &Environment) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("set! requires 2 arguments".to_owned());
    }

    let var = match &args[0] {
        &Value::Symbol(ref s) => s,
        _ => return Err("First argument of set! must be a symbol".to_owned())
    };

    if !env.has(var) {
        return Err(format!("Symbol {} is not defined", var));
    }

    let exp = eval(&args[1], env)?;

    env.set(var, &exp);

    return Ok(exp);
}

fn eval_list(vs: &[Value], env: &Environment) -> Result<Value, String> {
    let f = &eval(&vs[0], env)?;
    let args = &vs[1..];

    match f {
        &Value::Symbol(ref s) => match s.as_str() {
            "quote" => quote_exp(args, env),
            "if" => if_exp(args, env),
            "lambda" => lambda_exp(args, env),
            "define" => define_exp(args, env),
            "set!" => set_exp(args, env),
            _ => Err(format!("Invalid application to symbol {}", s))
        },
        &Value::Procedure(Procedure::Builtin(f)) => {
            let args: Result<Vec<Value>, String>
                = args.iter().map(|arg| eval(arg, env)).collect();

            return f(&args?[..]);
        },
        &Value::Procedure(Procedure::Scheme(ref vars, ref closure, ref body)) => {
            let args: Result<Vec<Value>, String>
                = args.iter().map(|arg| eval(arg, env)).collect();

            return eval_proc(&vars[..], &body[..], &args?[..], &closure.clone());
        },
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
            "lambda" => Ok(value.clone()),
            "define" => Ok(value.clone()),
            "set!" => Ok(value.clone()),
            _ => env.get(s)
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
    parent: Option<Rc<Environment>>,
    kvs: RefCell<HashMap<String, Value>>
}

impl Environment {
    pub fn new_global() -> Self {
        let builtins: Vec<(&str, BuiltinFunc)> =
            vec![
                ("+",    add_proc),
                ("-",    sub_proc),
                ("*",    mul_proc),
                ("=",    eq_proc),
                (">",    gt_proc),
                ("<",    lt_proc),
                ("car",  car_proc),
                ("cdr",  cdr_proc),
                ("cons", cons_proc)
            ];

        let kvs = builtins.into_iter()
            .map(|(s, f)|
                 (s.to_owned(), Value::Procedure(Procedure::Builtin(f))))
            .collect();

        Environment {
            parent: None,
            kvs: RefCell::new(kvs)
        }
    }

    pub fn new_child(parent: Rc<Environment>) -> Self {
        Environment {
            parent: Some(parent.clone()),
            kvs: RefCell::new(HashMap::new())
        }
    }

    pub fn define(&self, key: &str, value: &Value) {
        self.kvs.borrow_mut().insert(key.to_owned(), value.clone());
    }

    pub fn set(&self, key: &str, value: &Value) {
        if self.kvs.borrow().contains_key(key) {
            self.kvs.borrow_mut().insert(key.to_owned(), value.clone());
            return;
        }

        match self.parent {
            Some(ref p) => p.set(key, value),
            None => {}
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        if self.kvs.borrow().contains_key(key) {
            return self.kvs.borrow().get(key).cloned();
        }

        match self.parent {
            Some(ref p) => p.get(key),
            None => None
        }
    }

    pub fn has(&self, key: &str) -> bool {
        if self.kvs.borrow().contains_key(key) {
            return true;
        }

        match self.parent {
            Some(ref p) => p.has(key),
            None => false
        }
    }
}

#[cfg(test)]
mod tests {
    use util::rep;

    #[test]
    fn test_quote() {
        let cases = vec![
            ("(quote a)",       "a"),
            ("(quote (+ 1 2))", "(+ 1 2)"),
            ("'a",              "a"),
            ("'()",             "()"),
            ("'(+ 1 2)",        "(+ 1 2)"),
            ("'(quote a)",      "(quote a)"),
            ("''a",             "(quote a)")
        ];

        for (input, expected) in cases {
            assert_eq!(rep(input), expected);
        }
    }

    #[test]
    fn test_if() {
        let cases = vec![
            ("(if (> 3 2) 'yes 'no)",           "yes"),
            ("(if (> 2 3) 'yes 'no)",           "no"),
            ("(if (> 3 2) (- 3 2) (+ 3 2))",    "1")
        ];

        for (input, expected) in cases {
            assert_eq!(rep(input), expected);
        }
    }

    #[test]
    fn test_lambda() {
        let cases = vec![
            ("((lambda (x) (+ x x)) 4)", "8"),
            ("((lambda (x) (+ (* x x) x 1)) 2)", "7"),
            ("((lambda () 123))", "123"),
            ("((lambda (x) (+ x 1) (* x 2)) 10)", "20")
        ];

        for (input, expected) in cases {
            assert_eq!(rep(input), expected);
        }
    }

    #[test]
    fn test_define() {
        let cases = vec![
            ("(define a 1)", "1")
        ];

        for (input, expected) in cases {
            assert_eq!(rep(input), expected);
        }
    }
}
