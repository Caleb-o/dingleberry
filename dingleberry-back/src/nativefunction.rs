use std::{
    fmt::Debug,
    rc::{Rc, Weak},
};

use crate::{
    object::{Object, ObjectData},
    value::Value,
    vm::VM,
};

pub type NativeFn = &'static dyn Fn(&mut VM, Vec<Value>) -> Value;

#[derive(Clone)]
pub struct NativeFunction {
    pub identifier: &'static str,
    pub arg_count: Option<u8>,
    pub function: NativeFn,
}

impl PartialOrd for NativeFunction {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.identifier.partial_cmp(&other.identifier) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.arg_count.partial_cmp(&other.arg_count)
    }
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
        vm.allocate(ObjectData::NativeFunction(Rc::new(Self::new(
            identifier, arg_count, function,
        ))))
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
