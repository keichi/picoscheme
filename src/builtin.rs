use value::Value;

pub fn add_proc(args: &[Value]) -> Result<Value, String> {
    let mut result = 0;

    for v in args.iter() {
        match v {
            &Value::Integer(i) => result += i,
            v => return Err(format!("Cannot apply non-integer {} to +", v))
        }
    }

    Ok(Value::Integer(result))
}

pub fn sub_proc(args: &[Value]) -> Result<Value, String> {
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

pub fn mul_proc(args: &[Value]) -> Result<Value, String> {
    let mut result = 1;

    for v in args.iter() {
        match v {
            &Value::Integer(i) => result *= i,
            v => return Err(format!("Cannot apply non-integer {} to *", v))
        }
    }

    Ok(Value::Integer(result))
}

pub fn eq_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 {
        return Err("= requires 2 or more arguments".to_owned());
    }

    let vs = args.iter()
        .map(|arg| match arg {
            &Value::Integer(i) => Ok(i),
            v => Err(format!("Cannot apply non-integer {} to =", v))
        })
        .collect::<Result<Vec<i64>, String>>()?;

    return Ok(Value::Boolean(vs.iter().all(|&v| v == vs[0])));
}

pub fn lt_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 {
        return Err("= requires 2 or more arguments".to_owned());
    }

    let vs = args.iter()
        .map(|arg| match arg {
            &Value::Integer(i) => Ok(i),
            v => Err(format!("Cannot apply non-integer {} to =", v))
        })
        .collect::<Result<Vec<i64>, String>>()?;

    return Ok(Value::Boolean(
        vs.iter()
        .zip(vs.iter().skip(1))
        .all(|(v1, v2)| v1 < v2)
    ));
}

pub fn gt_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 {
        return Err("= requires 2 or more arguments".to_owned());
    }

    let vs = args.iter()
        .map(|arg| match arg {
            &Value::Integer(i) => Ok(i),
            v => Err(format!("Cannot apply non-integer {} to =", v))
        })
        .collect::<Result<Vec<i64>, String>>()?;

    return Ok(Value::Boolean(
        vs.iter()
        .zip(vs.iter().skip(1))
        .all(|(v1, v2)| v1 > v2)
    ));
}

pub fn car_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("car requires 1 argument".to_owned())
    }

    match &args[0] {
        &Value::List(ref vs) if vs.is_empty() =>
            Err("Cannot take car of empty list".to_owned()),
        &Value::List(ref vs) => Ok(vs[0].clone()),
        &Value::DottedList(ref vs) => Ok(vs[0].clone()),
        _ => Err("car requires list".to_owned())
    }
}

pub fn cdr_proc(args: &[Value]) -> Result<Value, String> {
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

pub fn cons_proc(args: &[Value]) -> Result<Value, String> {
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

pub fn is_eqv_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("eqv? requires 2 arguments".to_owned())
    }

    let is_eqv = match (&args[0], &args[1]) {
        (&Value::Boolean(b1), &Value::Boolean(b2)) => b1 == b2,
        (&Value::Integer(i1), &Value::Integer(i2)) => i1 == i2,
        (&Value::Symbol(ref s1), &Value::Symbol(ref s2)) => s1 == s2,
        (&Value::List(ref l1), &Value::List(ref l2)) => l1.is_empty() && l2.is_empty(),
        _ => false
    };

    Ok(Value::Boolean(is_eqv))
}

pub fn is_boolean_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("boolean? requires 1 argument".to_owned())
    }

    Ok(Value::Boolean(
        match &args[0] {
            &Value::Boolean(_) => true,
            _ => false
        }
    ))
}

pub fn is_pair_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("pair? requires 1 argument".to_owned())
    }

    Ok(Value::Boolean(
        match &args[0] {
            &Value::List(ref l) => !l.is_empty(),
            &Value::DottedList(ref l) => !l.is_empty(),
            _ => false
        }
    ))
}

pub fn is_symbol_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("symbol? requires 1 argument".to_owned())
    }

    Ok(Value::Boolean(
        match &args[0] {
            &Value::Symbol(_) => true,
            _ => false
        }
    ))
}

pub fn is_number_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("number? requires 1 argument".to_owned())
    }

    Ok(Value::Boolean(
        match &args[0] {
            &Value::Integer(_) => true,
            _ => false
        }
    ))
}

pub fn is_string_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string? requires 1 argument".to_owned())
    }

    Ok(Value::Boolean(
        match &args[0] {
            &Value::String(_) => true,
            _ => false
        }
    ))
}

pub fn is_procedure_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("procedure? requires 1 argument".to_owned())
    }

    Ok(Value::Boolean(
        match &args[0] {
            &Value::Procedure(_) => true,
            _ => false
        }
    ))
}

pub fn string_to_number_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string->number requires 1 argument".to_owned())
    }

    match &args[0] {
        &Value::String(ref s) => s.parse().map(|i| Value::Integer(i))
                                          .or(Ok(Value::Boolean(false))),
        _ => Err("string->number requires a string".to_owned())
    }
}

pub fn number_to_string_proc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("number->string requires 1 argument".to_owned())
    }

    match &args[0] {
        &Value::Integer(i) => Ok(Value::String(i.to_string())),
        _ => Err("number->string requires a number".to_owned())
    }
}


#[cfg(test)]
mod tests {
    use interpreter::Interpreter;

    #[test]
    fn test_basic_arithmetic() {
        let cases = vec![
            ("(+ 3 4)",     "7"),
            ("(+ 3)",       "3"),
            ("(+)",         "0"),
            ("(* 4)",       "4"),
            ("(*)",         "1"),
            ("(- 3 4)",     "-1"),
            ("(- 3 4 5)",   "-6"),
            ("(- 3)",       "-3")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_cons() {
        let cases = vec![
            ("(cons 'a '())",           "(a)"),
            ("(cons '(a) '(b c d))",    "((a) b c d)"),
            ("(cons \"a\" '(b c))",     "(\"a\" b c)"),
            ("(cons 'a 3)",             "(a . 3)"),
            ("(cons '(a b) 'c)",        "((a b) . c)")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_car() {
        let cases = vec![
            ("(car '(a b c))",      "a"),
            ("(car '((a) b c d))",  "(a)"),
            ("(car '(1 . 2))",      "1")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_cdr() {
        let cases = vec![
            ("(cdr '((a) b c d))",  "(b c d)"),
            ("(cdr '(1 . 2))",      "2")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_numerical_predicates() {
        let cases = vec![
            ("(= 1 1)",     "#t"),
            ("(= 1 1 1)",   "#t"),
            ("(= 1 2 3 4)", "#f"),

            ("(< 1 1)",         "#f"),
            ("(< 1 2)",         "#t"),
            ("(< 2 1)",         "#f"),
            ("(< 1 2 3 4)",     "#t"),
            ("(< 1 2 3 2 1)",   "#f"),

            ("(> 1 1)",         "#f"),
            ("(> 1 2)",         "#f"),
            ("(> 2 1)",         "#t"),
            ("(> 4 3 2 1)",     "#t"),
            ("(> 1 2 3 2 1)",   "#f")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_eqv() {
        let cases = vec![
            ("(eqv? 'a 'a)",                        "#t"),
            ("(eqv? 'a 'b)",                        "#f"),
            ("(eqv? 2 2)",                          "#t"),
            ("(eqv? '() '())",                      "#t"),
            ("(eqv? 100000000 100000000)",          "#t"),
            ("(eqv? (cons 1 2) (cons 1 2))",        "#f"),
            ("(eqv? (lambda () 1) (lambda () 2))",  "#f"),
            ("(eqv? #f 'nil)",                      "#f")
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_type_predicates() {
        let cases = vec![
            ("(boolean? #t)",   "#t"),
            ("(boolean? #f)",   "#t"),
            ("(boolean? 0)",    "#f"),
            ("(boolean? '())",  "#f"),

            ("(pair? '(1 2 3))",    "#t"),
            ("(pair? '(1 2 . 3))",  "#t"),
            ("(pair? '(a . b))",    "#t"),
            ("(pair? '(a b c))",    "#t"),
            ("(pair? '())",         "#f"),

            ("(symbol? 'a)",            "#t"),
            ("(symbol? (car '(a b)))",  "#t"),
            ("(symbol? \"bar\")",       "#f"),
            ("(symbol? 'nil)",          "#t"),
            ("(symbol? '())",           "#f"),
            ("(symbol? #f)",            "#f"),

            ("(number? 123)",   "#t"),

            ("(string? \"test\")",  "#t"),

            ("(procedure? car)",                    "#t"),
            ("(procedure? 'car)",                   "#f"),
            ("(procedure? (lambda (x) (* x x)))",   "#t"),
            ("(procedure? '(lambda (x) (* x x)))",  "#f"),
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }

    #[test]
    fn test_type_conversions() {
        let cases = vec![
            ("(number->string 123)",    "\"123\""),

            ("(string->number \"456\")",    "456"),
            ("(string->number \"abc\")",    "#f"),
        ];

        let interp = Interpreter::new();

        for (input, expected) in cases {
            assert_eq!(interp.rep_str(input), expected);
        }
    }
}
