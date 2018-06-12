use std::rc::Rc;

use environment::Environment;
use value::{Procedure, Value};

fn quote_exp(args: &[Value], _: Rc<Environment>) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("quote requires 1 argument".to_owned())
    }

    Ok(args[0].clone())
}

fn if_exp(args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("quote requires 2 or 3 arguments".to_owned())
    }

    match eval(&args[0], env.clone())? {
        Value::Boolean(false) => if args.len() == 2 {
            Ok(Value::List(Vec::new()))
        } else {
            eval(&args[2], env)
        },
        _ => eval(&args[1], env)
    }
}

fn lambda_exp(args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
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

fn eval_proc(vars: &[Value], body: &[Value], args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
    if vars.len() != args.len() {
        return Err(format!("procedure requires {} arguments but {} supplied",
                           vars.len(), args.len()))
    }

    let new_env = Rc::new(Environment::new_child(env));

    for (var, arg) in vars.iter().zip(args.iter()) {
        match var {
            &Value::Symbol(ref s) => {
                new_env.define(s, arg);
            },
            _ => {}
        }
    }

    body.iter()
        .map(|e| eval(e, new_env.clone()))
        .collect::<Result<Vec<Value>, String>>()
        .map(|rs| rs.last().unwrap().clone())
}

fn define_exp(args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("define requires 2 arguments".to_owned());
    }

    let var = match &args[0] {
        &Value::Symbol(ref s) => s,
        _ => return Err("First argument of define must be a symbol".to_owned())
    };

    let exp = eval(&args[1], env.clone())?;

    env.define(var, &exp);

    Ok(args[0].clone())
}

fn set_exp(args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
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

    let exp = eval(&args[1], env.clone())?;

    env.set(var, &exp);

    return Ok(exp);
}

fn eval_list(vs: &[Value], env: Rc<Environment>) -> Result<Value, String> {
    let f = &eval(&vs[0], env.clone())?;
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
                = args.iter().map(|arg| eval(arg, env.clone())).collect();

            return f(&args?[..]);
        },
        &Value::Procedure(Procedure::Scheme(ref vars, ref closure, ref body)) => {
            let args: Result<Vec<Value>, String>
                = args.iter().map(|arg| eval(arg, env.clone())).collect();

            return eval_proc(&vars[..], &body[..], &args?[..], closure.clone());
        },
        v => Err(format!("Invalid application to {}", v))
    }
}

pub fn eval(value: &Value, env: Rc<Environment>) -> Result<Value, String> {
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

#[cfg(test)]
mod tests {
    use interpreter::Interpreter;

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

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_if() {
        let cases = vec![
            ("(if (> 3 2) 'yes 'no)",           "yes"),
            ("(if (> 2 3) 'yes 'no)",           "no"),
            ("(if (> 3 2) (- 3 2) (+ 3 2))",    "1")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
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

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_define() {
        let cases = vec![
            ("(define a 1)", "a")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }
}
