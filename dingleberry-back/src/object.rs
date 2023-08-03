use std::{
    any::Any,
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    byte_compiler::Function,
    nativefunction::{NativeFn, NativeFunction},
    value::Value,
    vm::VM,
};

#[derive(Clone, PartialEq)]
pub struct Module {
    pub identifier: String,
    pub items: HashMap<String, Value>,
}

impl Module {
    #[inline]
    pub fn add_item(&mut self, identifier: String, value: Value) {
        self.items.insert(identifier, value);
    }

    pub fn add_func(
        &mut self,
        vm: &mut VM,
        identifier: &'static str,
        param_count: Option<u8>,
        func: NativeFn,
    ) {
        let value = vm.create_function(identifier, param_count, func);
        self.items.insert(identifier.to_string(), value);
    }
}

impl PartialOrd for Module {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.identifier.partial_cmp(&other.identifier)
    }
}

#[derive(Clone, PartialEq)]
pub struct StructDef {
    pub identifier: String,
    pub init_items: Option<Vec<String>>,
    pub items: HashMap<String, Value>,
}

impl StructDef {
    #[inline]
    pub fn add_item(&mut self, identifier: String, value: Value) {
        self.items.insert(identifier, value);
    }

    pub fn add_func(
        &mut self,
        vm: &mut VM,
        identifier: &'static str,
        param_count: Option<u8>,
        func: NativeFn,
    ) {
        let value = vm.create_function(identifier, param_count, func);
        self.items.insert(identifier.to_string(), value);
    }
}

impl PartialOrd for StructDef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.identifier.partial_cmp(&other.identifier)
    }
}

#[derive(Clone, PartialEq)]
pub struct StructInstance {
    pub struct_name: String,
    pub values: HashMap<String, Value>,
}

impl PartialOrd for StructInstance {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.struct_name.partial_cmp(&other.struct_name)
    }
}

/// Data that lives inside of an object, like heaped values
#[derive(Clone)]
pub enum ObjectData {
    Str(String),
    List(Vec<Value>),
    Tuple(Box<[Value]>),
    Function(Rc<Function>, bool),
    NativeFunction(Rc<NativeFunction>),
    Module(Rc<Module>),
    StructDef(Rc<StructDef>),
    StructInstance(Rc<StructInstance>),
    NativeObject(Rc<dyn Any>),
}

impl PartialEq for ObjectData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (Self::Function(l0, l1), Self::Function(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::NativeFunction(l0), Self::NativeFunction(r0)) => l0 == r0,
            (Self::Module(l0), Self::Module(r0)) => l0 == r0,
            (Self::StructDef(l0), Self::StructDef(r0)) => l0 == r0,
            (Self::StructInstance(l0), Self::StructInstance(r0)) => l0 == r0,

            _ => false,
        }
    }
}

impl PartialOrd for ObjectData {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ObjectData::Str(l), ObjectData::Str(r)) => l.partial_cmp(r),
            (ObjectData::Function(l, _), ObjectData::Function(r, _)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl Debug for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(_) => write!(f, "String"),
            Self::List(_) => write!(f, "List"),
            Self::Tuple(_) => write!(f, "Tuple"),
            Self::Function(_, _) => write!(f, "Function"),
            Self::NativeFunction(_) => write!(f, "NativeFunction"),
            Self::Module { .. } => write!(f, "Module"),
            Self::StructDef(_) => write!(f, "StructDef"),
            Self::StructInstance(_) => write!(f, "StructInstance"),
            Self::NativeObject(_) => write!(f, "NativeObject"),
        }
    }
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

            Self::Tuple(values) => {
                write!(f, "(")?;

                for (idx, value) in values.iter().enumerate() {
                    write!(f, "{value}")?;

                    if idx < values.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }

            Self::Function(func, _) => write!(
                f,
                "fn<{}, {}>",
                func.identifier.as_ref().unwrap_or(&"Anonymous".into()),
                func.arg_count
            ),

            Self::NativeFunction(func) => {
                write!(f, "nativefn<{}, {:?}>", func.identifier, func.arg_count)
            }

            Self::Module(m) => write!(f, "module<{}>", m.identifier),

            Self::StructDef(s) => write!(f, "struct_def<{}>", s.identifier),

            Self::StructInstance(s) => write!(f, "struct_inst<{}>", s.struct_name),

            Self::NativeObject(o) => write!(f, "native_object<{o:?}>"),
        }
    }
}

/// Objects are values that live in the heap
#[derive(PartialEq, PartialOrd)]
pub struct Object {
    // Mutable state within the object
    pub marked: Cell<bool>,
    pub data: RefCell<ObjectData>,
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data.borrow())
    }
}
