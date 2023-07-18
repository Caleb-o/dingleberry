#![allow(dead_code)]

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use std::fmt::Display;
use std::rc::{Rc, Weak};

use crate::byte_compiler::Function;
use crate::nativefunction::NativeFunction;
use crate::object::{Object, ObjectData};
use crate::value::Value;

/// Total initial bytes before a collection occurs
const INITIAL_COLLECTION_SIZE: usize = 1024 * 1024;
/// Threshold multiplier applied to next_sweep size
const SWEEP_FACTOR: usize = 2;
/// Amount of collections before managing the old generation
const GENERATION_SWEEP: u8 = 3;

/// Generation contains objects
/// There is nothing unique in a generation itself,
/// it is just used as a separate pool for objects
pub struct Generation {
    pub objects: Vec<Rc<Object>>,
}

impl Generation {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn sweep(&mut self, stats: &mut GarbageStats) {
        self.objects.retain(|obj| {
            let is_marked = obj.marked.get();
            if cfg!(Debug) && !is_marked {
                // Object is unreachable, "deallocate" it
                // NOTE: Obviously as an RC, it will dealloc once the Rc is dropped
                println!(
                    "Deallocating object with data: {:?} {}",
                    obj.data,
                    Rc::weak_count(&obj),
                );
            }

            if !is_marked {
                stats.frees += 1;
            }

            // Reset mark
            obj.marked.set(false);
            is_marked
        });
        self.objects.shrink_to_fit();
    }

    pub fn transfer(&mut self, from: &mut Self) {
        if cfg!(Debug) {
            for item in &from.objects {
                println!("Moving to next generation {:?}", item.data);
            }
        }
        self.objects.append(&mut from.objects);
        from.objects.clear();
    }
}

pub struct GarbageStats {
    allocs: usize,
    frees: usize,
    collections: usize,
    bytes_allocated: usize,
}

impl GarbageStats {
    fn new() -> Self {
        GarbageStats {
            allocs: 0,
            frees: 0,
            collections: 0,
            bytes_allocated: 0,
        }
    }
}

impl Display for GarbageStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[Allocs {} | Frees {} | Collections {} | Bytes Allocated {}]",
            self.allocs, self.frees, self.collections, self.bytes_allocated
        )
    }
}

pub struct GarbageCollector {
    pub young: Generation,
    pub old: Generation,

    pub bytes_allocated: usize,
    next_sweep: usize,
    generation_counter: u8,

    stats: GarbageStats,
}

/// An object that can be threaded through the garbage collector,
/// as multiple parameters would be gross
pub struct Roots<'a> {
    pub stack: &'a [Value],
    pub globals: &'a HashMap<String, Value>,
    pub interned_strings: &'a HashMap<u32, Rc<Object>>,
}

impl GarbageCollector {
    pub fn new() -> GarbageCollector {
        GarbageCollector {
            young: Generation::new(),
            old: Generation::new(),
            bytes_allocated: 0,
            next_sweep: INITIAL_COLLECTION_SIZE,
            generation_counter: 0,

            stats: GarbageStats::new(),
        }
    }

    #[inline]
    pub fn write_stats(&self) {
        println!("{}", self.stats);
    }

    pub fn allocate<'a>(&mut self, data: ObjectData, roots: Roots<'a>) -> Weak<Object> {
        // NOTE: These are *rough* values to track byte sizes
        let to_alloc_bytes = match &data {
            ObjectData::Str(s) => std::mem::size_of::<String>() + s.as_bytes().len(),
            ObjectData::List(values) => std::mem::size_of::<Value>() * values.capacity(),
            ObjectData::Tuple(values) => std::mem::size_of::<Value>() * values.len(),
            ObjectData::Function(_) => std::mem::size_of::<Function>(),
            ObjectData::NativeFunction(_) => std::mem::size_of::<NativeFunction>(),
            ObjectData::Module(m) => std::mem::size_of::<Value>() * m.items.len(),
            ObjectData::StructDef(s) => std::mem::size_of::<Value>() * s.items.len(),
            ObjectData::StructInstance(s) => std::mem::size_of::<Value>() * s.values.len(),
        } + std::mem::size_of::<ObjectData>();

        self.bytes_allocated += to_alloc_bytes;

        self.stats.allocs += 1;
        self.stats.bytes_allocated += to_alloc_bytes;

        if cfg!(Debug) {
            println!(
                "Allocating {to_alloc_bytes} bytes, total allocated {}",
                self.bytes_allocated
            );
        }

        // Check for next collection
        if self.bytes_allocated >= self.next_sweep {
            self.collect_garbage(roots);
        }

        let obj = Rc::new(Object {
            data: RefCell::new(data),
            marked: Cell::new(false),
        });
        let weak = Rc::downgrade(&obj);
        self.young.objects.push(obj);
        weak
    }

    fn mark(&self, root: &Rc<Object>) {
        if root.marked.get() {
            return;
        }

        match &*root.data.borrow() {
            &ObjectData::List(ref items) => {
                for item in items {
                    if let Value::Object(obj) = item {
                        self.mark(&obj.upgrade().unwrap());
                    }
                }
                root.marked.set(true);
            }

            &ObjectData::Tuple(ref items) => {
                for item in &items[0..] {
                    if let Value::Object(obj) = item {
                        self.mark(&obj.upgrade().unwrap());
                    }
                }
                root.marked.set(true);
            }

            &ObjectData::Module(ref m) => {
                for item in m.items.values() {
                    if let Value::Object(ref obj) = &*item {
                        self.mark(&obj.upgrade().unwrap());
                    }
                }
                root.marked.set(true);
            }

            _ => root.marked.set(true),
        }
    }

    fn sweep(&mut self) {
        self.young.sweep(&mut self.stats);

        self.generation_counter += 1;
        if self.generation_counter >= GENERATION_SWEEP {
            self.old.sweep(&mut self.stats);
            self.generation_counter = 0;
        }

        // Append young to old
        self.old.transfer(&mut self.young);

        // Set next sweep point
        self.next_sweep *= SWEEP_FACTOR;
    }

    fn mark_roots<'a>(&mut self, roots: Roots<'a>) {
        for item in roots.stack {
            if let Value::Object(obj) = item {
                if let Some(object) = obj.upgrade() {
                    self.mark(&object);
                }
            }
        }

        for item in roots.globals.values() {
            if let Value::Object(obj) = item {
                if let Some(object) = obj.upgrade() {
                    self.mark(&object);
                }
            }
        }

        for item in roots.interned_strings.values() {
            self.mark(item);
        }
    }

    /// Returns if it swept old generation
    #[inline]
    pub fn collect_garbage<'a>(&mut self, roots: Roots<'a>) {
        if cfg!(Debug) {
            println!("Collecting garbage");
        }
        self.stats.collections += 1;

        self.mark_roots(roots);
        self.sweep();
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        gc::{ObjectData, Value},
        vm::VM,
    };

    #[test]
    fn simple_collection() {
        let mut vm = VM::new();

        let value = Value::Object(vm.allocate_string("Goodbye".into(), true));
        vm.push(value);

        {
            // Allocate objects
            let obj2 = vm.allocate_string("Hello".into(), false);

            obj2.upgrade().as_mut().map(|obj2| {
                // Modify mutable state within objects
                *obj2.data.borrow_mut() = ObjectData::Str("Hello, World!".into());
            });

            // Obj2 is not on in the roots as it's not interned, so it is not reachable by the GC
            vm.collect();
        }

        assert_eq!(vm.gc.young.objects.len(), 0);
        assert_eq!(vm.gc.old.objects.len(), 1);

        vm.pop();

        assert_eq!(vm.gc.old.objects.len(), 0);
    }

    #[test]
    fn long_loop() {
        let mut vm = VM::new();

        for _ in 0..2048 {
            let hello = Value::Object(vm.allocate_string("hello world".into(), false));
            vm.push(hello);
            vm.pop();
        }

        vm.collect();

        assert_eq!(vm.gc.young.objects.len(), 0);
        assert_eq!(vm.gc.old.objects.len(), 0);
    }
}
