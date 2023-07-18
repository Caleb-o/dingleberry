use std::fs::{self};

use dingleberry_back::{value::Value, vm::VM};

pub fn register_native_objects(vm: &mut VM) {
    vm.build_module("File", false, |vm, module| {
        module.add_func(vm, "read_from", Some(1), &file_read_from_file);
        module.add_func(vm, "write_to", Some(2), &file_write_to_file);
    })
    .unwrap();
}

fn file_read_from_file(vm: &mut VM, args: &[Value]) -> Value {
    if let Some(file_name) = args[0].get_as_string() {
        if let Ok(content) = fs::read_to_string(&file_name) {
            let obj = vm.allocate_string(content, false);
            return Value::Object(obj);
        }
    }

    Value::None
}

fn file_write_to_file(_: &mut VM, args: &[Value]) -> Value {
    match (args[0].get_as_string(), args[1].get_as_string()) {
        (Some(file_name), Some(content)) => {
            _ = fs::write(file_name, content);
        }
        _ => {}
    }

    Value::None
}
