use std::fs::{self};

use dingleberry_back::{object::ObjectData, value::Value, vm::VM};
use dingleberry_shared::NativeModuleFlags;
use sdl2::{render::WindowCanvas, EventPump, Sdl};

pub struct Sdl2App {
    pub context: Sdl,
    pub canvas: WindowCanvas,
    pub event_pump: EventPump,
    pub fps_limit: u32,

    running: bool,
}

impl Sdl2App {
    // pub fn new(title: &str, width: u32, height: u32, fps_limit: u32) -> anyhow::Result<Self> {
    //     let sdl_context = sdl2::init().map_err(SdlAppError::new)?;
    //     let video_subsystem = sdl_context.video().map_err(SdlAppError::new)?;
    //     let ttf_context = sdl2::ttf::init()?;

    //     let window = video_subsystem
    //         .window(title, width, height)
    //         .position_centered()
    //         .build()
    //         .unwrap();

    //     let canvas = window.into_canvas().accelerated().build().unwrap();
    //     let event_pump = sdl_context.event_pump().map_err(SdlAppError::new)?;
    //     let texture_creator = canvas.texture_creator();

    //     Ok(Self {
    //         context: sdl_context,
    //         ttf_context,
    //         video: video_subsystem,
    //         canvas,
    //         texture_creator,
    //         event_pump,
    //         fps_limit,
    //         textures: Vec::new(),
    //         running: true,
    //     })
    // }
}

pub fn register_native_objects(vm: &mut VM, native_flags: NativeModuleFlags) {
    if native_flags.file {
        vm.build_module("File", false, |vm, module| {
            module.add_func(vm, "read_from", Some(1), &file_read_from_file);
            module.add_func(vm, "write_to", Some(2), &file_write_to_file);
        })
        .unwrap();
    }

    if native_flags.strings {
        vm.build_module("Strings", false, |vm, module| {
            module.add_func(vm, "slice", Some(3), &strings_slice);
            module.add_func(vm, "slice_n", Some(3), &strings_slice_n);
            module.add_func(vm, "clone_no_intern", Some(1), &strings_clone_no_intern);
        })
        .unwrap();
    }

    if native_flags.list {
        vm.build_module("List", false, |vm, module| {
            module.add_func(vm, "append", Some(2), &list_append);
            module.add_func(vm, "prepend", Some(2), &list_prepend);
        })
        .unwrap();
    }

    if native_flags.runtime {
        vm.build_module("Runtime", false, |vm, module| {
            module.add_func(vm, "collect", None, &runtime_gc_collect);
            module.add_func(vm, "print_current_fn_code", None, &|vm, _| {
                let current_frame = vm.get_callstack().last().unwrap();

                if let ObjectData::Function(func, _) = &*current_frame.function.data.borrow() {
                    println!("Code {:?}", func.code);
                }

                Value::None
            });
        })
        .unwrap();
    }

    if native_flags.coroutine {
        vm.build_module("Coroutine", false, |vm, module| {
            module.add_func(vm, "is_coroutine", None, &coroutine_is_coroutine);
            module.add_func(vm, "is_complete", None, &coroutine_is_complete);
            module.add_func(vm, "get", None, &coroutine_get);
        })
        .unwrap();
    }

    // vm.build_module("Graphics", false, |vm, module| {
    //     module.add_func(vm, "create_window", Some(4), &|vm, args| match (
    //         args[0].get_as_string(),
    //         args[1].get_as_number(),
    //         args[2].get_as_number(),
    //         args[3].get_as_number(),
    //     ) {
    //         (Some(title), Some(width), Some(height), Some(fps_limit)) => Value::None,
    //         _ => Value::None,
    //     });
    // })
    // .unwrap();
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
   === RUNTIME
*/

fn runtime_gc_collect(vm: &mut VM, _: Vec<Value>) -> Value {
    // Since this can be called by the user, we don't want to bump
    // the next collection size
    vm.collect(false);
    Value::None
}

/*
   === COROUTINE
*/

fn coroutine_is_coroutine(_: &mut VM, args: Vec<Value>) -> Value {
    if let Some(obj) = args[0].get_as_object() {
        if let ObjectData::Coroutine(_) = &*obj.upgrade().unwrap().data.borrow() {
            return Value::Boolean(true);
        }
    }
    Value::Boolean(false)
}

fn coroutine_is_complete(_: &mut VM, args: Vec<Value>) -> Value {
    if let Some(obj) = args[0].get_as_object() {
        if let ObjectData::Coroutine(co) = &*obj.upgrade().unwrap().data.borrow() {
            return Value::Boolean(co.is_complete);
        }
    }
    Value::Boolean(false)
}

fn coroutine_get(_: &mut VM, args: Vec<Value>) -> Value {
    if let Some(obj) = args[0].get_as_object() {
        if let ObjectData::Coroutine(co) = &*obj.upgrade().unwrap().data.borrow() {
            return co.result.clone();
        }
    }
    Value::None
}
