use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{
    byte_compiler::Function,
    bytecode::ByteCode,
    gc::{GarbageCollector, NativeFunction, Object, ObjectData, Roots, Value},
};

struct CallFrame {
    pub ip: usize,
    pub identifier: String,
    pub stack_start: usize,
    pub function: Rc<Object>,
}

pub struct VM {
    pub stack: Vec<Value>,
    pub gc: GarbageCollector,
    pub constants: Vec<Value>,
    pub globals: HashMap<String, Value>,
    pub interned_strings: HashMap<u32, Rc<Object>>,

    ip: usize,
    call_stack: Vec<CallFrame>,
    running: bool,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(32),
            gc: GarbageCollector::new(),
            constants: Vec::new(),
            globals: HashMap::new(),
            interned_strings: HashMap::with_capacity(8),

            ip: 0,
            call_stack: Vec::new(),
            running: true,
        }
    }

    #[inline]
    pub fn push(&mut self, item: Value) {
        self.stack.push(item);
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        assert!(self.stack.len() > 0);
        self.stack.pop().unwrap()
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

    pub fn call(&mut self, maybe_function: Value, arg_count: usize) -> Result<(), SpruceErr> {
        if !matches!(maybe_function, Value::Object(_)) {
            return Err(SpruceErr::new(
                "Cannot call values".into(),
                SpruceErrData::VM,
            ));
        }

        let Value::Object(maybe_function) = maybe_function else { unreachable!() };
        let maybe_function = maybe_function.upgrade().unwrap();

        let (identifier, param_count) = match &*maybe_function.data.borrow() {
            ObjectData::Function(function) => {
                (function.identifier.clone(), Some(function.arg_count))
            }
            ObjectData::NativeFunction(function) => {
                (function.identifier.to_string(), function.arg_count)
            }

            n @ (ObjectData::Str(_) | ObjectData::List(_)) => {
                return Err(SpruceErr::new(
                    format!("Cannot call non-function value '{n:?}'"),
                    SpruceErrData::VM,
                ));
            }
        };

        if param_count.is_some() && self.stack.len() < param_count.unwrap() as usize {
            return Err(SpruceErr::new(
                format!(
                    "Function '{}' expected {} arguments but received {}",
                    identifier,
                    param_count.unwrap(),
                    self.stack.len()
                ),
                SpruceErrData::VM,
            ));
        }

        if cfg!(Debug) {
            println!("Calling {identifier}");
        }

        if let ObjectData::NativeFunction(func) = &*maybe_function.data.borrow() {
            let arg_count = if func.arg_count.is_some() {
                func.arg_count.unwrap() as usize
            } else {
                arg_count
            };
            let args = self
                .stack
                .drain((self.stack.len() - arg_count)..)
                .collect::<Vec<Value>>();
            let value = (func.function)(self, &args);
            self.stack.push(value);
            return Ok(());
        }

        self.ip = 0;
        self.call_stack.push(CallFrame {
            ip: 0,
            identifier: identifier.clone(),
            stack_start: self.stack.len() - arg_count,
            function: maybe_function,
        });

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), SpruceErr> {
        self.register_functions();

        while self.running {
            let instruction = self.get_next_inst();
            match instruction {
                ByteCode::ConstantByte(index) => {
                    self.stack.push(self.constants[index as usize].clone());
                }

                ByteCode::Add | ByteCode::Sub | ByteCode::Mul | ByteCode::Div => {
                    let (lhs, rhs) = self.maybe_get_top_two()?;

                    if VM::are_numbers(&lhs, &rhs) {
                        self.binary_op_numbers(instruction.clone(), lhs, rhs)?;
                    } else {
                        return Err(SpruceErr::new(
                            format!("Invalid types in binary op {lhs:?} and {rhs:?}"),
                            SpruceErrData::VM,
                        ));
                    }
                }

                ByteCode::SetLocal(index) => {
                    let start = self.func_stack_start();
                    self.stack[start + index as usize] = self.peek();
                }

                ByteCode::GetLocal(index) => {
                    let start = self.func_stack_start();
                    self.stack.push(self.stack[start + index as usize].clone());
                }

                ByteCode::SetGlobal(identifier) => {
                    if self.globals.contains_key(&identifier) {
                        let val = self.peek();
                        let value = self.globals.get_mut(&identifier).unwrap();
                        *value = val;
                    } else {
                        return Err(SpruceErr::new(
                            format!("Cannot find global '{identifier}'"),
                            SpruceErrData::VM,
                        ));
                    }
                }

                ByteCode::GetGlobal(identifier) => match self.globals.get(&identifier) {
                    Some(value) => {
                        self.stack.push(value.clone());
                    }
                    None => {
                        return Err(SpruceErr::new(
                            format!("Cannot find global '{identifier}'"),
                            SpruceErrData::VM,
                        ));
                    }
                },

                ByteCode::IntoList(count) => {
                    let start = self.func_stack_start();
                    let moved_values = self.stack.drain(start..start + count as usize).collect();

                    let reference = self.allocate(ObjectData::List(moved_values));
                    self.stack.push(Value::Object(reference));
                }

                ByteCode::Call(arg_count) => {
                    let function = self.pop();
                    self.call(function, arg_count as usize)?;
                }

                ByteCode::None => self.stack.push(Value::None),

                ByteCode::Return => {
                    let last_frame = self.call_stack.pop().unwrap();

                    // Reduce a pop to preserve return value
                    _ = self
                        .stack
                        .drain(last_frame.stack_start..self.stack.len() - 1);

                    if self.call_stack.len() == 0 {
                        self.running = false;
                    }
                }

                _ => todo!("VM: {instruction:?}"),
            }
        }

        Ok(())
    }

    fn register_functions(&mut self) {
        let func = NativeFunction::alloc(self, "print", None, &super::nt_print);
        self.globals
            .insert("print".to_string(), Value::Object(func));
    }

    #[inline]
    fn are_numbers(lhs: &Value, rhs: &Value) -> bool {
        matches!(lhs, Value::Number(_)) && lhs.kind_equals(&rhs)
    }

    fn maybe_get_top_two(&mut self) -> Result<(Value, Value), SpruceErr> {
        if self.stack.len() < 2 {
            return Err(SpruceErr::new(
                "Stack underflow".to_string(),
                SpruceErrData::VM,
            ));
        }

        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        if !lhs.kind_equals(&rhs) {
            return Err(SpruceErr::new(
                format!("Type mismatch {lhs:?} and {rhs:?}"),
                SpruceErrData::VM,
            ));
        }

        Ok((lhs, rhs))
    }

    fn binary_op_numbers(&mut self, op: ByteCode, lhs: Value, rhs: Value) -> Result<(), SpruceErr> {
        let Value::Number(rnum) = rhs else { unreachable!() };
        let Value::Number(lnum) = lhs else { unreachable!() };

        self.stack.push(match op {
            ByteCode::Add => Value::Number(lnum + rnum),
            ByteCode::Sub => Value::Number(lnum - rnum),
            ByteCode::Mul => Value::Number(lnum * rnum),
            ByteCode::Div => Value::Number(lnum / rnum),

            _ => unreachable!(),
        });

        Ok(())
    }

    #[inline]
    fn peek(&self) -> Value {
        self.stack.last().unwrap().clone()
    }

    #[inline]
    fn get_function(&self) -> Rc<Function> {
        let ObjectData::Function(ref function) =
            &*self.call_stack.last().unwrap().function.data.borrow() else { unreachable!() };
        Rc::clone(function)
    }

    #[inline]
    fn func_stack_start(&self) -> usize {
        self.call_stack.last().unwrap().stack_start
    }

    #[inline]
    fn get_next_inst(&mut self) -> ByteCode {
        let function = self.get_function();
        let frame = self.call_stack.last_mut().unwrap();

        if function.code.len() == 0 {
            self.stack.push(Value::None);
            return ByteCode::Return;
        }

        let code = function.code[frame.ip].clone();
        frame.ip += 1;

        code
    }
}
