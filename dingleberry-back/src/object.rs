use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{byte_compiler::Function, nativefunction::NativeFunction, value::Value};

#[derive(Clone, PartialEq)]
pub struct Module {
    pub identifier: String,
    pub items: HashMap<String, Value>,
}

#[derive(Clone, PartialEq)]
pub struct StructDef {
    pub identifier: String,
    pub items: HashMap<String, Value>,
}

#[derive(Clone, PartialEq)]
pub struct StructInstance {
    pub struct_name: String,
    pub values: HashMap<String, Value>,
}

/// Data that lives inside of an object, like heaped values
#[derive(Clone, PartialEq)]
pub enum ObjectData {
    Str(String),
    List(Vec<Value>),
    Tuple(Box<[Value]>),
    Function(Rc<Function>),
    NativeFunction(NativeFunction),
    Module(Rc<Module>),
    StructDef(Rc<StructDef>),
    StructInstance(Rc<StructInstance>),
}

impl Debug for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(_) => write!(f, "String"),
            Self::List(_) => write!(f, "List"),
            Self::Tuple(_) => write!(f, "Tuple"),
            Self::Function(_) => write!(f, "Function"),
            Self::NativeFunction(_) => write!(f, "NativeFunction"),
            Self::Module { .. } => write!(f, "Module"),
            Self::StructDef(_) => write!(f, "StructDef"),
            Self::StructInstance(_) => write!(f, "StructInstance"),
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

            Self::Function(func) => write!(
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
        }
    }
}

/// Objects are values that live in the heap
#[derive(PartialEq)]
pub struct Object {
    // Mutable state within the object
    pub data: RefCell<ObjectData>,
    pub marked: Cell<bool>,
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
