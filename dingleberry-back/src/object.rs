use std::{
    cell::{Cell, RefCell},
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{byte_compiler::Function, nativefunction::NativeFunction, value::Value};

/// Data that lives inside of an object, like heaped values
#[derive(Clone, PartialEq)]
pub enum ObjectData {
    Str(String),
    List(Vec<Value>),
    Function(Rc<Function>),
    NativeFunction(NativeFunction),
}

impl Debug for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(_) => write!(f, "String"),
            Self::List(_) => write!(f, "List"),
            Self::Function(_) => write!(f, "Function"),
            Self::NativeFunction(_) => write!(f, "NativeFunction"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data.borrow())
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
            Self::Function(func) => write!(f, "fn<{}, {}>", func.identifier, func.arg_count),
            Self::NativeFunction(func) => {
                write!(f, "nativefn<{}, {:?}>", func.identifier, func.arg_count)
            }
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
