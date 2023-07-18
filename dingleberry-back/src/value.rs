use std::{
    fmt::{Debug, Display},
    rc::Weak,
};

use dingleberry_front::token::{Token, TokenKind};

use crate::object::{Object, ObjectData};

/// These are values that live on the stack
#[derive(Clone)]
pub enum Value {
    None,
    Number(f32),
    Boolean(bool),
    Object(Weak<Object>),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::Number(_) => write!(f, "Number"),
            Self::Boolean(_) => write!(f, "Boolean"),
            Self::Object(o) => write!(f, "Object:{:?}", o.upgrade().unwrap().data.borrow()),
        }
    }
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

    pub fn get_as_string(&self) -> Option<String> {
        if let Value::Object(obj) = &self {
            match &*obj.upgrade().unwrap().data.borrow() {
                ObjectData::Str(ref s) => return Some(s.clone()),
                _ => {}
            }
        }
        None
    }
}

impl From<Token> for Value {
    fn from(value: Token) -> Self {
        match value.kind {
            TokenKind::None => Value::None,
            TokenKind::Number => Value::Number(
                value
                    .lexeme
                    .as_ref()
                    .unwrap()
                    .get_slice()
                    .parse::<f32>()
                    .unwrap(),
            ),
            TokenKind::True => Value::Boolean(true),
            TokenKind::False => Value::Boolean(false),

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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            (Self::Boolean(l0), Self::Boolean(r0)) => l0.partial_cmp(r0),
            (Self::Object(l0), Self::Object(r0)) => {
                l0.upgrade().unwrap().partial_cmp(&r0.upgrade().unwrap())
            }
            _ => None,
        }
    }
}
