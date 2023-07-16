use std::{fmt::Debug, rc::Weak};

use crate::{
    object::{Object, ObjectData},
    value::Value,
    vm::VM,
};

pub type NativeFn = &'static dyn Fn(&mut VM, &[Value]) -> Value;

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
