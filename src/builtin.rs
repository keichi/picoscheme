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
    use util::rep;

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
    fn test_numerical_predicates() {
        assert_eq!(rep("(= 1 1)"), "#t");
        assert_eq!(rep("(= 1 1 1)"), "#t");
        assert_eq!(rep("(= 1 2 3 4)"), "#f");

        assert_eq!(rep("(< 1 1)"), "#f");
        assert_eq!(rep("(< 1 2)"), "#t");
        assert_eq!(rep("(< 2 1)"), "#f");
        assert_eq!(rep("(< 1 2 3 4)"), "#t");
        assert_eq!(rep("(< 1 2 3 2 1)"), "#f");

        assert_eq!(rep("(> 1 1)"), "#f");
        assert_eq!(rep("(> 1 2)"), "#f");
        assert_eq!(rep("(> 2 1)"), "#t");
        assert_eq!(rep("(> 4 3 2 1)"), "#t");
        assert_eq!(rep("(> 1 2 3 2 1)"), "#f");
    }

    #[test]
    fn test_eqv() {
        assert_eq!(rep("(eqv? 'a 'a)"), "#t");
        assert_eq!(rep("(eqv? 'a 'b)"), "#f");
        assert_eq!(rep("(eqv? 2 2)"), "#t");
        assert_eq!(rep("(eqv? '() '())"), "#t");
        assert_eq!(rep("(eqv? 100000000 100000000)"), "#t");
        assert_eq!(rep("(eqv? (cons 1 2) (cons 1 2))"), "#f");
        assert_eq!(rep("(eqv? (lambda () 1) (lambda () 2))"), "#f");
        assert_eq!(rep("(eqv? #f 'nil)"), "#f");
    }

    #[test]
    fn test_type_predicates() {
        assert_eq!(rep("(boolean? #t)"), "#t");
        assert_eq!(rep("(boolean? #f)"), "#t");
        assert_eq!(rep("(boolean? 0)"), "#f");
        assert_eq!(rep("(boolean? '())"), "#f");

        assert_eq!(rep("(pair? '(1 2 3))"), "#t");
        assert_eq!(rep("(pair? '(1 2 . 3))"), "#t");
        assert_eq!(rep("(pair? '(a . b))"), "#t");
        assert_eq!(rep("(pair? '(a b c))"), "#t");
        assert_eq!(rep("(pair? '())"), "#f");

        assert_eq!(rep("(symbol? 'a)"), "#t");
        assert_eq!(rep("(symbol? (car '(a b)))"), "#t");
        assert_eq!(rep("(symbol? \"bar\")"), "#f");
        assert_eq!(rep("(symbol? 'nil)"), "#t");
        assert_eq!(rep("(symbol? '())"), "#f");
        assert_eq!(rep("(symbol? #f)"), "#f");

        assert_eq!(rep("(number? 123)"), "#t");

        assert_eq!(rep("(string? \"test\")"), "#t");

        assert_eq!(rep("(procedure? car)"), "#t");
        assert_eq!(rep("(procedure? 'car)"), "#f");
        assert_eq!(rep("(procedure? (lambda (x) (* x x)))"), "#t");
        assert_eq!(rep("(procedure? '(lambda (x) (* x x)))"), "#f");
    }

    #[test]
    fn test_type_conversions() {
        assert_eq!(rep("(number->string 123)"), "\"123\"");

        assert_eq!(rep("(string->number \"456\")"), "456");
        assert_eq!(rep("(string->number \"abc\")"), "#f");
    }
}
