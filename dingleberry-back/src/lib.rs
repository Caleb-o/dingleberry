use object::ObjectData;
use value::Value;
use vm::VM;

pub mod byte_compiler;
pub mod bytecode;
pub mod gc;
pub mod nativefunction;
pub mod object;
pub mod value;
pub mod vm;

fn nt_print(_: &mut VM, args: &[Value]) -> Value {
    for item in args {
        print!("{item}");
    }
    println!();

    Value::None
}

fn nt_len(_: &mut VM, args: &[Value]) -> Value {
    let value = match &args[0] {
        Value::None => 0,
        Value::Number(_) => 0,
        Value::Boolean(_) => 0,
        Value::Object(obj) => match &*obj.upgrade().unwrap().data.borrow() {
            ObjectData::Str(s) => s.len(),
            ObjectData::List(l) => l.len(),
            ObjectData::Tuple(t) => t.len(),
            ObjectData::Function(f) => f.arg_count as usize,
            ObjectData::NativeFunction(f) => f.arg_count.unwrap_or_default() as usize,
            ObjectData::Module(m) => m.items.len(),
        },
    } as f32;

    Value::Number(value)
}
