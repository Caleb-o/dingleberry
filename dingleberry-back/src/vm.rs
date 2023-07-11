use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{
    byte_compiler::Function,
    gc::{GarbageCollector, Object, ObjectData, Roots, Value},
};

struct CallFrame {
    pub identifier: String,
    pub stack_start: usize,
}

pub struct VM {
    pub stack: Vec<Value>,
    pub gc: GarbageCollector,
    pub constants: Vec<Value>,
    pub globals: Vec<Value>,
    pub interned_strings: HashMap<u32, Rc<Object>>,

    ip: usize,
    call_stack: Vec<CallFrame>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(32),
            gc: GarbageCollector::new(),
            constants: Vec::new(),
            globals: Vec::new(),
            interned_strings: HashMap::with_capacity(8),

            ip: 0,
            call_stack: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, item: Value) {
        self.stack.push(item);
    }

    #[inline]
    pub fn pop(&mut self) {
        assert!(self.stack.len() > 0);
        _ = self.stack.pop();
    }

    pub fn allocate(&mut self, data: ObjectData) -> Weak<Object> {
        self.gc.allocate(
            data,
            Roots {
                stack: &self.stack,
                globals: &self.globals,
                interned_strings: &self.interned_strings,
            },
        )
    }

    pub fn allocate_string(&mut self, data: String) -> Weak<Object> {
        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        let hash = hasher.finish() as u32;

        if let Some(object) = self.interned_strings.get(&hash) {
            return Rc::downgrade(&object);
        }

        self.allocate(ObjectData::Str(data))
    }

    #[inline]
    pub fn collect(&mut self) {
        self.gc.collect_garbage(Roots {
            stack: &self.stack,
            globals: &self.globals,
            interned_strings: &self.interned_strings,
        })
    }

    #[inline]
    pub fn collect_all(&mut self) {
        self.gc.young.sweep();
        self.gc.old.sweep();
    }

    pub fn call(&mut self, func: Box<Function>) -> Result<(), SpruceErr> {
        if self.stack.len() < func.arg_count as usize {
            return Err(SpruceErr::new(format!(""), SpruceErrData::VM));
        }

        self.ip = 0;
        self.call_stack.push(CallFrame {
            identifier: func.identifier.clone(),
            stack_start: self.stack.len() - func.arg_count as usize,
        });

        Ok(())
    }
}
