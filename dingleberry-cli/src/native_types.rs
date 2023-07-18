use std::fs::{self};

use dingleberry_back::{object::ObjectData, value::Value, vm::VM};

pub fn register_native_objects(vm: &mut VM) {
    vm.build_module("File", false, |vm, module| {
        module.add_func(vm, "read_from", Some(1), &file_read_from_file);
        module.add_func(vm, "write_to", Some(2), &file_write_to_file);
    })
    .unwrap();

    vm.build_module("Strings", false, |vm, module| {
        module.add_func(vm, "slice", Some(3), &strings_slice);
        module.add_func(vm, "slice_n", Some(3), &strings_slice_n);
        module.add_func(vm, "clone_no_intern", Some(1), &strings_clone_no_intern);
    })
    .unwrap();

    vm.build_module("List", false, |vm, module| {
        module.add_func(vm, "append", Some(2), &list_append);
        module.add_func(vm, "prepend", Some(2), &list_prepend);
    })
    .unwrap();

    vm.build_module("Gc", false, |vm, module| {
        module.add_func(vm, "collect", None, &gc_collect);
    })
    .unwrap();
}

/*
   === FILES
*/

fn file_read_from_file(vm: &mut VM, args: Vec<Value>) -> Value {
    if let Some(file_name) = args[0].get_as_string() {
        if let Ok(content) = fs::read_to_string(&file_name) {
            let obj = vm.allocate_string(content, false);
            return Value::Object(obj);
        }
    }

    Value::None
}

fn file_write_to_file(_: &mut VM, args: Vec<Value>) -> Value {
    match (args[0].get_as_string(), args[1].get_as_string()) {
        (Some(file_name), Some(content)) => {
            _ = fs::write(file_name, content);
            Value::None
        }
        _ => Value::None,
    }
}

/*
   === STRINGS
*/

fn strings_slice(vm: &mut VM, args: Vec<Value>) -> Value {
    match (
        args[0].get_as_string(),
        args[1].get_as_number(),
        args[2].get_as_number(),
    ) {
        (Some(string), Some(start), Some(end)) => {
            let (start, end) = if end < start {
                (end, start)
            } else {
                (start, end)
            };

            let start = std::cmp::min(start as usize, string.len());
            let end = std::cmp::min(end as usize, string.len());

            let obj = vm.allocate_string(string[start..end].to_string(), true);
            Value::Object(obj)
        }
        _ => Value::None,
    }
}

fn strings_slice_n(vm: &mut VM, args: Vec<Value>) -> Value {
    match (
        args[0].get_as_string(),
        args[1].get_as_number(),
        args[2].get_as_number(),
    ) {
        (Some(string), Some(start), Some(count)) => {
            let start = std::cmp::min(start as usize, string.len());
            let end = std::cmp::min(start as usize + count as usize, string.len());

            let obj = vm.allocate_string(string[start..end].to_string(), true);
            Value::Object(obj)
        }
        _ => Value::None,
    }
}

fn strings_clone_no_intern(vm: &mut VM, args: Vec<Value>) -> Value {
    if let Value::Object(obj) = &args[0] {
        if let ObjectData::Str(s) = &*obj.upgrade().unwrap().data.borrow() {
            let obj = vm.allocate_string(s.clone(), false);
            return Value::Object(obj);
        }
    }
    Value::None
}

/*
   === LIST
*/

fn list_append(_: &mut VM, mut args: Vec<Value>) -> Value {
    if let Some(obj) = args[0].get_as_object() {
        if let ObjectData::List(l) = &mut *obj.upgrade().unwrap().data.borrow_mut() {
            l.push(args.pop().unwrap());
        }
    }
    Value::None
}

fn list_prepend(_: &mut VM, mut args: Vec<Value>) -> Value {
    if let Some(obj) = args[0].get_as_object() {
        if let ObjectData::List(l) = &mut *obj.upgrade().unwrap().data.borrow_mut() {
            l.insert(0, args.pop().unwrap());
        }
    }
    Value::None
}

/*
   === GC
*/

fn gc_collect(vm: &mut VM, _: Vec<Value>) -> Value {
    // Since this can be called by the user, we don't want to bump
    // the next collection size
    vm.collect(false);
    Value::None
}
