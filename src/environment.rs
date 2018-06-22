use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use builtin::*;
use value::{BuiltinFunc, Procedure, Value};

#[derive(Clone, Debug)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    kvs: RefCell<HashMap<String, Value>>,
}

impl Environment {
    pub fn new_global() -> Self {
        let builtins: Vec<(&str, BuiltinFunc)> = vec![
            ("+", add_proc),
            ("-", sub_proc),
            ("*", mul_proc),
            ("=", eq_proc),
            (">", gt_proc),
            ("<", lt_proc),
            (">=", ge_proc),
            ("<=", le_proc),
            ("car", car_proc),
            ("cdr", cdr_proc),
            ("cons", cons_proc),
            ("eqv?", is_eqv_proc),
            ("boolean?", is_boolean_proc),
            ("pair?", is_pair_proc),
            ("symbol?", is_symbol_proc),
            ("number?", is_number_proc),
            ("string?", is_string_proc),
            ("procedure?", is_procedure_proc),
            ("string->number", string_to_number_proc),
            ("number->string", number_to_string_proc),
        ];

        let kvs = builtins
            .into_iter()
            .map(|(s, f)| (s.to_owned(), Value::Procedure(Procedure::Builtin(f))))
            .collect();

        Environment {
            parent: None,
            kvs: RefCell::new(kvs),
        }
    }

    pub fn new_child(parent: Rc<Environment>) -> Self {
        Environment {
            parent: Some(parent.clone()),
            kvs: RefCell::new(HashMap::new()),
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
            None => None,
        }
    }

    pub fn has(&self, key: &str) -> bool {
        if self.kvs.borrow().contains_key(key) {
            return true;
        }

        match self.parent {
            Some(ref p) => p.has(key),
            None => false,
        }
    }
}
