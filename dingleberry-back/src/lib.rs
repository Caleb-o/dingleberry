use object::ObjectData;
use value::Value;
use vm::VM;

pub mod byte_compiler;
pub mod bytecode;
pub mod gc;
pub mod nativefunction;
pub mod object;
pub mod value;
mod value_methods;
pub mod vm;

fn nt_print(_: &mut VM, args: Vec<Value>) -> Value {
    for item in args {
        print!("{item}");
    }
    println!();

    Value::None
}

fn nt_len(_: &mut VM, args: Vec<Value>) -> Value {
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

            _ => 0,
        },
    } as f32;

    Value::Number(value)
}

fn nt_fields_of(vm: &mut VM, args: Vec<Value>) -> Value {
    let mut fields = Vec::new();

    match &args[0] {
        Value::Object(obj) => match &*obj.upgrade().unwrap().data.borrow() {
            ObjectData::Module(module) => {
                for field in &module.items {
                    let string = vm.allocate_string(field.0.clone(), true);
                    let tuple = vm.allocate(ObjectData::Tuple(Box::new([
                        Value::Object(string),
                        field.1.clone(),
                    ])));

                    fields.push(Value::Object(tuple));
                }
            }

            ObjectData::StructDef(struct_def) => {
                for field in &struct_def.items {
                    let string = vm.allocate_string(field.0.clone(), true);
                    let tuple = vm.allocate(ObjectData::Tuple(Box::new([
                        Value::Object(string),
                        field.1.clone(),
                    ])));

                    fields.push(Value::Object(tuple));
                }
            }

            ObjectData::StructInstance(struct_inst) => {
                for field in &struct_inst.values {
                    let string = vm.allocate_string(field.0.clone(), true);
                    let tuple = vm.allocate(ObjectData::Tuple(Box::new([
                        Value::Object(string),
                        field.1.clone(),
                    ])));

                    fields.push(Value::Object(tuple));
                }
            }

            _ => {}
        },

        _ => {}
    }

    let fields = vm.allocate(ObjectData::List(fields));
    Value::Object(fields)
}

fn nt_freeze(vm: &mut VM, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::Object(obj) => match &*obj.upgrade().unwrap().data.borrow() {
            ObjectData::List(l) => Value::Object(vm.allocate(ObjectData::Tuple(l.clone().into()))),
            _ => Value::None,
        },

        _ => Value::None,
    }
}

fn nt_dbg_stack(vm: &mut VM, _: Vec<Value>) -> Value {
    println!("Stack {:?}", vm.stack);
    Value::None
}

fn nt_dbg_globals(vm: &mut VM, _: Vec<Value>) -> Value {
    println!("Globals {:?}", vm.globals.values());
    Value::None
}
