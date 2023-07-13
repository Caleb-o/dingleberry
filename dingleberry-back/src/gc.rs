#![allow(dead_code)]

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Display};

use std::rc::{Rc, Weak};

use dingleberry_front::token::{Token, TokenKind};

use crate::byte_compiler::Function;
use crate::vm::VM;

type NativeFn = &'static dyn Fn(&mut VM, &[Value]) -> Value;

#[derive(Clone)]
pub struct NativeFunction {
    pub identifier: &'static str,
    pub arg_count: Option<u8>,
    pub function: NativeFn,
}

impl NativeFunction {
    pub fn new(identifier: &'static str, arg_count: Option<u8>, function: NativeFn) -> Self {
        Self {
            identifier,
            arg_count,
            function,
        }
    }

    pub fn alloc(
        vm: &mut VM,
        identifier: &'static str,
        arg_count: Option<u8>,
        function: NativeFn,
    ) -> Weak<Object> {
        vm.allocate(ObjectData::NativeFunction(Self::new(
            identifier, arg_count, function,
        )))
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("identifier", &self.identifier)
            .field("arg_count", &self.arg_count)
            .finish()
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.identifier == other.identifier && self.arg_count == other.arg_count
    }
}

/// These are values that live on the stack
#[derive(Debug, Clone)]
pub enum Value {
    None,
    Number(f32),
    Boolean(bool),
    Object(Weak<Object>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Object(o) => write!(f, "{}", o.upgrade().unwrap().data.borrow()),
        }
    }
}

impl Value {
    #[inline]
    pub fn kind_equals(&self, other: &Value) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl From<Token> for Value {
    fn from(value: Token) -> Self {
        let slice = value.lexeme.as_ref().unwrap().get_slice();

        match value.kind {
            TokenKind::None => Value::None,
            TokenKind::Number => Value::Number(slice.parse::<f32>().unwrap()),
            TokenKind::True | TokenKind::False => Value::Boolean(slice.parse::<bool>().unwrap()),

            _ => unreachable!(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Object(l0), Self::Object(r0)) => l0.upgrade().unwrap() == r0.upgrade().unwrap(),
            _ => false,
        }
    }
}

/// Data that lives inside of an object, like heaped values
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectData {
    Str(String),
    List(Vec<Value>),
    Function(Rc<Function>),
    NativeFunction(NativeFunction),
}

impl Display for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(s) => write!(f, "{s}"),
            Self::List(values) => {
                write!(f, "[")?;

                for (idx, value) in values.iter().enumerate() {
                    write!(f, "{value}")?;

                    if idx < values.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "]")
            }
            Self::Function(func) => write!(f, "fn<{}, {}>", func.identifier, func.arg_count),
            Self::NativeFunction(func) => {
                write!(f, "nativefn<{}, {:?}>", func.identifier, func.arg_count)
            }
        }
    }
}

/// Objects are values that live in the heap
#[derive(Debug, PartialEq)]
pub struct Object {
    // Mutable state within the object
    pub data: RefCell<ObjectData>,
    pub marked: Cell<bool>,
}

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

    pub fn sweep(&mut self) {
        self.objects.retain(|obj| {
            let is_marked = obj.marked.get();
            if cfg!(Debug) && !is_marked {
                // Object is unreachable, "deallocate" it
                // NOTE: Obviously as an RC, it will dealloc once the Rc is dropped
                println!(
                    "Deallocating object with data: {:?} {}",
                    obj.data,
                    Rc::strong_count(&obj),
                );
            }

            // Reset mark
            obj.marked.set(false);
            is_marked
        });
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

pub struct GarbageCollector {
    pub young: Generation,
    pub old: Generation,

    pub bytes_allocated: usize,
    next_sweep: usize,
    generation_counter: u8,
}

/// Total initial bytes before a collection occurs
const INITIAL_COLLECTION_SIZE: usize = 1024 * 1024;
/// Threshold multiplier applied to next_sweep size
const SWEEP_FACTOR: usize = 2;
/// Amount of collections before managing the old generation
const GENERATION_SWEEP: u8 = 3;

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
        }
    }

    pub fn allocate<'a>(&mut self, data: ObjectData, roots: Roots<'a>) -> Weak<Object> {
        let to_alloc_bytes = match &data {
            ObjectData::Str(_) => std::mem::size_of::<String>(),
            ObjectData::List(values) => std::mem::size_of::<Value>() * values.capacity(),
            ObjectData::Function(_) => std::mem::size_of::<Function>(),
            ObjectData::NativeFunction(_) => std::mem::size_of::<NativeFunction>(),
        } + std::mem::size_of::<ObjectData>();

        self.bytes_allocated += to_alloc_bytes;

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
            &ObjectData::Str(_) | &ObjectData::Function(_) | &ObjectData::NativeFunction(_) => {
                root.marked.set(true)
            }

            &ObjectData::List(ref items) => {
                for item in items {
                    if let Value::Object(obj) = item {
                        self.mark(&obj.upgrade().unwrap());
                    }
                }
            }
        }
    }

    fn sweep(&mut self) {
        self.young.sweep();

        self.generation_counter += 1;
        if self.generation_counter >= GENERATION_SWEEP {
            self.old.sweep();
            self.generation_counter = 0;
        }

        // Append young to old
        self.old.transfer(&mut self.young);

        // Set next sweep point
        self.next_sweep *= SWEEP_FACTOR;
    }

    #[allow(irrefutable_let_patterns)]
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

    #[inline]
    pub fn collect_garbage<'a>(&mut self, roots: Roots<'a>) {
        if cfg!(Debug) {
            println!("Collecting garbage");
        }
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

        let value = Value::Object(vm.allocate_string("Goodbye".into()));
        vm.push(value);

        {
            // Allocate objects
            let obj2 = vm.allocate_string("Hello".into());

            obj2.upgrade().as_mut().map(|obj2| {
                // Modify mutable state within objects
                *obj2.data.borrow_mut() = ObjectData::Str("Hello, World!".into());
            });

            // Obj2 is not on in the roots, so it is not reachable by the GC
            vm.collect();
        }

        assert_eq!(vm.gc.young.objects.len(), 0);
        assert_eq!(vm.gc.old.objects.len(), 1);

        vm.pop();
        vm.collect_all();

        assert_eq!(vm.gc.old.objects.len(), 0);
    }
}
