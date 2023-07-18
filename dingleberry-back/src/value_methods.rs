use crate::{value::Value, vm::VM};

pub fn nt_binary_add(_: &mut VM, mut args: Vec<Value>) -> Value {
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();

    match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
        _ => Value::None,
    }
}

pub fn nt_binary_sub(_: &mut VM, mut args: Vec<Value>) -> Value {
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();

    match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        _ => Value::None,
    }
}

pub fn nt_binary_mul(_: &mut VM, mut args: Vec<Value>) -> Value {
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();

    match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
        _ => Value::None,
    }
}

pub fn nt_binary_div(_: &mut VM, mut args: Vec<Value>) -> Value {
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();

    match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        _ => Value::None,
    }
}
