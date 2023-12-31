use dingleberry_front::token::{Token, TokenKind};
use object::ObjectData;
use value::Value;
use vm::VM;

pub mod byte_compiler;
pub mod bytecode;
pub mod debug;
pub mod gc;
pub mod nativefunction;
pub mod object;
pub mod symbol_table;
pub mod value;
mod value_methods;
pub mod vm;

pub fn get_identifier_or_string(token: &Token) -> String {
    let span = token.lexeme.as_ref().unwrap();
    if token.kind == TokenKind::Identifier {
        span.get_slice().to_string()
    } else {
        let slice = span.get_slice();
        slice[1..slice.len() - 1].to_string()
    }
}

#[inline]
pub fn type_name_to_int(type_name: &str) -> u8 {
    match type_name {
        "none" => 0,
        "number" => 1,
        "boolean" => 2,
        "string" => 3,
        "list" => 4,
        "tuple" => 5,
        "function" => 6,
        "nativefn" => 7,
        "module" => 8,
        "struct" | "struct_instance" => 9,
        "class" | "class_instance" => 10,
        "nativeobj" => 11,
        "coroutine" => 12,
        _ => u8::MAX,
    }
}

fn nt_print(_: &mut VM, args: Vec<Value>) -> Value {
    for item in args {
        print!("{item}");
    }

    Value::None
}

fn nt_println(_: &mut VM, args: Vec<Value>) -> Value {
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
            ObjectData::Function(f, _) => f.arg_count as usize,
            ObjectData::NativeFunction(f) => f.arg_count.unwrap_or_default() as usize,
            ObjectData::Module(m) => m.items.len(),

            _ => 0,
        },
    } as f32;

    Value::Number(value)
}

fn nt_type_of(_: &mut VM, args: Vec<Value>) -> Value {
    let type_name = match &args[0] {
        Value::None => "none",
        Value::Number(_) => "number",
        Value::Boolean(_) => "boolean",
        Value::Object(obj) => match &*obj.upgrade().unwrap().data.borrow() {
            ObjectData::Str(_) => "string",
            ObjectData::List(_) => "list",
            ObjectData::Tuple(_) => "tuple",
            ObjectData::Function(_, _) => "function",
            ObjectData::NativeFunction(_) => "nativefn",
            ObjectData::Module(_) => "module",
            ObjectData::StructDef(_) | ObjectData::StructInstance(_) => "struct",
            ObjectData::ClassDef(_) | ObjectData::ClassInstance(_) => "class",
            ObjectData::NativeObject(_) => "nativeobj",
            ObjectData::Coroutine(_) => "coroutine",
        },
    };

    let type_idx = type_name_to_int(type_name);
    Value::Number(type_idx as f32)
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

            ObjectData::ClassDef(class_def) => {
                for field in &class_def.items {
                    let string = vm.allocate_string(field.0.clone(), true);
                    let tuple = vm.allocate(ObjectData::Tuple(Box::new([
                        Value::Object(string),
                        field.1.clone(),
                    ])));

                    fields.push(Value::Object(tuple));
                }
            }

            ObjectData::ClassInstance(class_inst) => {
                for field in &class_inst.values {
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

fn nt_dbg_coroutine_data(_: &mut VM, args: Vec<Value>) -> Value {
    if let Value::Object(obj) = &args[0] {
        let obj = obj.upgrade().unwrap();
        if let ObjectData::Coroutine(co) = &*obj.data.borrow() {
            println!(
                "Coroutine: IP {}, Identifier '{}'",
                co.call_frame.ip, co.call_frame.identifier
            );
        };
    }
    Value::None
}

fn nt_dbg_print_function(vm: &mut VM, _: Vec<Value>) -> Value {
    let call_frame = vm.get_callstack().last().unwrap();

    let obj = &call_frame.function;
    if let ObjectData::Function(f, _) = &*obj.data.borrow() {
        debug::print_function_code(vm, f);
    };

    Value::None
}

fn nt_dbg_stack(vm: &mut VM, _: Vec<Value>) -> Value {
    print!("Stack [");
    for (idx, item) in vm.stack.iter().enumerate() {
        print!("{item}");

        if idx < vm.stack.len() - 1 {
            print!(", ");
        }
    }
    println!("]");
    Value::None
}

fn nt_dbg_function_stack(vm: &mut VM, _: Vec<Value>) -> Value {
    let call_stack = vm.get_callstack();
    let last_frame = call_stack.last().unwrap();
    let start = last_frame.stack_start;

    print!("Stack [");
    for (idx, item) in vm.stack.iter().skip(start).enumerate() {
        print!("{item}");

        if idx + start < vm.stack.len() - 1 {
            print!(", ");
        }
    }
    println!("]");
    Value::None
}

fn nt_dbg_globals(vm: &mut VM, _: Vec<Value>) -> Value {
    print!("[");
    for (idx, value) in vm.globals.values().enumerate() {
        print!("{value}");

        if idx < vm.globals.len() - 1 {
            print!(", ");
        }
    }
    println!("]");
    Value::None
}
