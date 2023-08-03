use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{
    byte_compiler::Function,
    bytecode::ByteCode,
    gc::{GarbageCollector, Roots},
    nativefunction::{NativeFn, NativeFunction},
    object::{Module, Object, ObjectData, StructDef, StructInstance},
    value::Value,
    value_methods,
};

const RUNTIME_INTERNING: bool = true;
const STRINGS_BEFORE_SWEEP: usize = 64;

pub struct CallFrame {
    pub ip: usize,
    pub identifier: String,
    pub arg_count: u8,
    pub stack_start: usize,
    pub function: Rc<Object>,
}

pub struct VM {
    pub stack: Vec<Value>,
    pub gc: GarbageCollector,
    pub constants: Vec<Value>,
    pub globals: HashMap<String, Value>,
    pub interned_strings: HashMap<u32, Rc<Object>>,

    value_methods: HashMap<&'static str, Value>,

    string_allocs: usize,

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
            string_allocs: 0,

            value_methods: HashMap::new(),

            call_stack: Vec::new(),
            running: false,
        }
    }

    #[inline]
    pub fn get_callstack(&self) -> &Vec<CallFrame> {
        &self.call_stack
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
                constants: &self.constants,
                globals: &self.globals,
                value_methods: &self.value_methods,
                interned_strings: &self.interned_strings,
            },
        )
    }

    pub fn allocate_string(&mut self, data: String, intern: bool) -> Weak<Object> {
        if !intern {
            return self.allocate(ObjectData::Str(data));
        }

        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        let hash = hasher.finish() as u32;

        let obj = if let Some(object) = self.interned_strings.get(&hash) {
            Rc::downgrade(object)
        } else {
            let obj = self.allocate(ObjectData::Str(data));
            if intern {
                self.string_allocs += 1;
                self.interned_strings
                    .insert(hash, obj.clone().upgrade().unwrap());
            }

            obj
        };

        if self.string_allocs >= STRINGS_BEFORE_SWEEP {
            self.string_allocs = 0;
            self.cleanup_strings();
        }

        obj
    }

    #[inline]
    pub fn collect(&mut self, bump_next: bool) {
        self.gc.collect_garbage(
            Some(Roots {
                stack: &self.stack,
                constants: &self.constants,
                globals: &self.globals,
                value_methods: &self.value_methods,
                interned_strings: &self.interned_strings,
            }),
            bump_next,
        );
    }

    pub fn build_module<F>(
        &mut self,
        identifier: &'static str,
        override_if_some: bool,
        build_f: F,
    ) -> Result<(), SpruceErr>
    where
        F: FnOnce(&mut Self, &mut Module),
    {
        let identifier = identifier.to_string();

        if !override_if_some && self.globals.contains_key(&identifier) {
            return Err(SpruceErr::new(format!("Cannot build module with name '{identifier}' as an item with that name already exists"), SpruceErrData::VM));
        }

        let mut module = Module {
            identifier: identifier.clone(),
            items: HashMap::new(),
        };
        build_f(self, &mut module);

        let obj = self.allocate(ObjectData::Module(Rc::new(module)));
        self.globals.insert(identifier, Value::Object(obj));
        Ok(())
    }

    pub fn build_struct<F>(
        &mut self,
        identifier: &'static str,
        override_if_some: bool,
        build_f: F,
    ) -> Result<(), SpruceErr>
    where
        F: FnOnce(&mut Self, &mut StructDef),
    {
        let identifier = identifier.to_string();

        if !override_if_some && self.globals.contains_key(&identifier) {
            return Err(SpruceErr::new(format!("Cannot build module with name '{identifier}' as an item with that name already exists"), SpruceErrData::VM));
        }

        let mut struct_ = StructDef {
            identifier: identifier.clone(),
            init_items: None,
            items: HashMap::new(),
        };
        build_f(self, &mut struct_);

        let obj = self.allocate(ObjectData::StructDef(Rc::new(struct_)));
        self.globals.insert(identifier, Value::Object(obj));
        Ok(())
    }

    pub fn create_function(
        &mut self,
        identifier: &'static str,
        param_count: Option<u8>,
        func: NativeFn,
    ) -> Value {
        let func = NativeFunction::alloc(self, identifier, param_count, func);
        Value::Object(func)
    }

    pub fn start(&mut self) {
        self.running = true;
        self.register_functions();
        self.register_value_methods();

        // println!("Code: {:?}", self.get_function().code);

        if let Err(e) = self.run() {
            println!("{e}");
            self.dump_stack_trace();
        }

        self.cleanup();
    }

    pub fn call(&mut self, maybe_function: Value, arg_count: usize) -> Result<(), SpruceErr> {
        if !matches!(maybe_function, Value::Object(_)) {
            return Err(SpruceErr::new(
                format!("Cannot call value {maybe_function}"),
                SpruceErrData::VM,
            ));
        }

        let Value::Object(maybe_function) = maybe_function else { unreachable!() };
        let maybe_function = maybe_function.upgrade().unwrap();

        let (identifier, param_count, has_variadic) = match &*maybe_function.data.borrow() {
            ObjectData::Function(function, has_variadic) => (
                function.identifier.clone().unwrap_or("Anonymous".into()),
                Some(function.arg_count),
                *has_variadic,
            ),

            ObjectData::NativeFunction(function) => {
                (function.identifier.to_string(), function.arg_count, false)
            }

            ObjectData::StructDef(struct_) => {
                // TODO: Check for constructor
                (
                    struct_.identifier.clone(),
                    struct_.init_items.as_ref().map(|items| items.len() as u8),
                    false,
                )
            }

            // TODO: Consider a module constructor
            n @ _ => {
                return Err(SpruceErr::new(
                    format!("Trying to call non-callable value '{n:?}'"),
                    SpruceErrData::VM,
                ));
            }
        };

        if param_count.is_some() {
            let param_count = param_count.unwrap() as usize;

            if (!has_variadic && arg_count != param_count)
                || (has_variadic && arg_count < param_count - 1)
            {
                return Err(SpruceErr::new(
                    format!(
                        "Function '{}' expected {} argument(s) but received {}",
                        identifier, param_count, arg_count
                    ),
                    SpruceErrData::VM,
                ));
            }

            if has_variadic {
                let args = if arg_count > param_count {
                    let variadic_args_c = arg_count - param_count;
                    self.stack
                        .drain(self.stack.len() - 1 - variadic_args_c..)
                        .collect()
                } else {
                    Vec::new()
                };

                let obj = self.allocate(ObjectData::List(args));
                self.push(Value::Object(obj));
            }
        }

        match &*maybe_function.data.borrow() {
            ObjectData::NativeFunction(func) => {
                let arg_count = if func.arg_count.is_some() {
                    func.arg_count.unwrap() as usize
                } else {
                    arg_count
                };

                let args = self
                    .stack
                    .drain(self.stack.len() - arg_count..)
                    .collect::<Vec<Value>>();
                let value = (func.function)(self, args);
                self.stack.push(value);
                return Ok(());
            }

            ObjectData::StructDef(struct_) => {
                let mut values = struct_.items.clone();

                if let Some(init_fields) = &struct_.init_items {
                    for field in init_fields.iter().rev() {
                        values.insert(field.clone(), self.pop());
                    }
                }

                let struct_ = self.allocate(ObjectData::StructInstance(Rc::new(StructInstance {
                    struct_name: struct_.identifier.clone(),
                    values,
                })));

                self.stack.push(Value::Object(struct_));
                return Ok(());
            }

            _ => {}
        }

        let receiver_arg = match &*maybe_function.data.borrow() {
            ObjectData::Function(func, _) => {
                if let Some(rec) = &func.receiver {
                    self.push(rec.clone());
                    1
                } else {
                    0
                }
            }
            _ => 0,
        };

        self.call_stack.push(CallFrame {
            ip: 0,
            identifier: identifier.clone(),
            arg_count: (receiver_arg + param_count.unwrap_or(arg_count as u8)) as u8,
            stack_start: self.stack.len()
                - receiver_arg as usize
                - param_count.unwrap_or(arg_count as u8) as usize,
            function: maybe_function,
        });

        Ok(())
    }

    fn cleanup(&mut self) {
        self.stack.clear();
        self.constants.clear();
        self.globals.clear();
        self.interned_strings.clear();

        self.gc.collect_all_garbage(None);
    }

    fn dump_stack_trace(&self) {
        for (idx, frame) in self.call_stack.iter().rev().enumerate() {
            print!("[{}] ", self.call_stack.len() - 1 - idx);
            match &*frame.function.data.borrow() {
                ObjectData::Function(func, _) => {
                    if let Some(receiver) = &func.receiver {
                        print!("{receiver}:");
                    }

                    println!(
                        "{}({})",
                        frame.identifier,
                        if func.arg_count > 0 { "..." } else { "" }
                    );
                }
                _ => unreachable!(),
            }
        }
    }

    fn run(&mut self) -> Result<(), SpruceErr> {
        while self.running {
            let instruction = self.get_next_inst();
            match instruction {
                ByteCode::ConstantByte(index) => {
                    self.stack.push(self.constants[index as usize].clone());
                }

                ByteCode::Pop => _ = self.stack.pop(),
                ByteCode::PopN(count) => _ = self.stack.drain(self.stack.len() - count as usize..),

                ByteCode::Add | ByteCode::Sub | ByteCode::Mul | ByteCode::Div => {
                    let (lhs, rhs) = self.maybe_get_top_two()?;

                    if VM::are_numbers(&lhs, &rhs) {
                        VM::binary_op_numbers(self, instruction, lhs, rhs)?;
                    } else if VM::are_strings(&lhs, &rhs) {
                        self.binary_op_strings(instruction, lhs, rhs)?;
                    } else {
                        return Err(SpruceErr::new(
                            format!("Invalid values in binary op '{lhs}' and '{rhs}'"),
                            SpruceErrData::VM,
                        ));
                    }
                }

                ByteCode::Greater
                | ByteCode::GreaterEq
                | ByteCode::Less
                | ByteCode::LessEq
                | ByteCode::Equal
                | ByteCode::NotEqual
                | ByteCode::Or
                | ByteCode::And => {
                    let (lhs, rhs) = self.maybe_get_top_two()?;
                    self.logical_op(instruction, lhs, rhs)?;
                }

                ByteCode::Negate => {
                    let value = self.pop();
                    self.push(match value {
                        Value::Number(n) => Value::Number(-n),
                        Value::Boolean(b) => Value::Boolean(!b),
                        v => {
                            return Err(SpruceErr::new(
                                format!("Cannot negate type {v:?}"),
                                SpruceErrData::VM,
                            ))
                        }
                    });
                }

                ByteCode::DefineGlobal(index) => {
                    let id = self.get_string(index);

                    let value = self.pop();
                    self.globals.insert(id, value);
                }

                ByteCode::SetLocal(index) => {
                    let start = self.func_stack_start();
                    self.stack[start + index as usize] = self.peek();
                }

                ByteCode::GetLocal(index) => {
                    let start = self.func_stack_start();
                    self.stack.push(self.stack[start + index as usize].clone());
                }

                ByteCode::SetGlobal(index) => {
                    let id = self.get_string(index);

                    if self.globals.contains_key(&id) {
                        let val = self.peek();
                        let value = self.globals.get_mut(&id).unwrap();
                        *value = val;
                    } else {
                        return Err(SpruceErr::new(
                            format!("Cannot find global '{id}'"),
                            SpruceErrData::VM,
                        ));
                    }
                }

                ByteCode::GetGlobal(index) => {
                    let id = self.get_string(index);

                    match self.globals.get(&id) {
                        Some(value) => {
                            self.stack.push(value.clone());
                        }
                        None => {
                            return Err(SpruceErr::new(
                                format!("Cannot find global '{id}'"),
                                SpruceErrData::VM,
                            ));
                        }
                    }
                }

                ByteCode::IndexGet => {
                    let index_expr = self.pop();
                    let item = self.pop();

                    self.index_item(item, index_expr)?;
                }

                ByteCode::IndexSet => {
                    let value = self.pop();
                    let index_expr = self.pop();
                    let index_item = self.pop();

                    self.index_item_set(index_item, index_expr, value)?;
                }

                ByteCode::PropertyGet(index) => {
                    let item = self.pop();
                    let property = self.get_string(index);
                    self.get_property(item, property)?;
                }

                ByteCode::PropertySet(index) => {
                    let item = self.pop();
                    let property = self.get_string(index);
                    let value = self.peek();
                    self.set_property(item, property, value)?;
                }

                ByteCode::Jump(index) => self.set_current_ip(index as usize),

                ByteCode::JumpNot(index) => {
                    let value = self.pop();

                    match value {
                        Value::Boolean(b) => {
                            if !b {
                                self.set_current_ip(index as usize);
                            }
                        }
                        _ => {
                            return Err(SpruceErr::new(
                                format!("Cannot compare non-boolean value '{value}'"),
                                SpruceErrData::VM,
                            ))
                        }
                    }
                }

                ByteCode::IntoList(count) => {
                    let end = self.stack.len();
                    let moved_values = self.stack.drain(end - count as usize..).collect();

                    let reference = self.allocate(ObjectData::List(moved_values));
                    self.stack.push(Value::Object(reference));
                }

                ByteCode::IntoTuple(count) => {
                    let end = self.stack.len();
                    let moved_values = self.stack.drain(end - count as usize..).collect();

                    let reference = self.allocate(ObjectData::Tuple(moved_values));
                    self.stack.push(Value::Object(reference));
                }

                ByteCode::Call(arg_count) => {
                    let function = self.pop();
                    self.call(function, arg_count as usize)?;
                }

                ByteCode::This => {
                    let function = self.get_function();
                    if function.receiver.is_none() {
                        return Err(SpruceErr::new(
                            format!(
                                "Function '{}' does not have a receiver to use 'this'",
                                function.identifier.as_ref().unwrap_or(&"Anon".to_string())
                            ),
                            SpruceErrData::VM,
                        ));
                    }

                    let start = self.func_stack_start();
                    let args = self.call_stack.last().unwrap().arg_count as usize;
                    let pos = start + args - 1;

                    self.push(self.stack[pos].clone());
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

    fn get_string(&self, index: u16) -> String {
        let constant = &self.constants[index as usize];
        let Value::Object(obj) = constant else { unreachable!() };

        let obj = obj.upgrade().unwrap();
        let ObjectData::Str(str) = &*obj.data.borrow() else { unreachable!() };

        str.clone()
    }

    fn cleanup_strings(&mut self) {
        _ = self
            .interned_strings
            .retain(|_, v| Rc::strong_count(v) > 0 && Rc::weak_count(v) > 0);
    }

    fn register_function(
        &mut self,
        identifier: &'static str,
        param_count: Option<u8>,
        func: NativeFn,
    ) {
        let func = NativeFunction::alloc(self, identifier, param_count, func);
        self.globals
            .insert(identifier.to_string(), Value::Object(func));
    }

    fn register_functions(&mut self) {
        self.register_function("print", None, &super::nt_print);
        self.register_function("println", None, &super::nt_println);
        self.register_function("len", Some(1), &super::nt_len);
        self.register_function("freeze", Some(1), &super::nt_freeze);
        self.register_function("fields_of", Some(1), &super::nt_fields_of);

        // Debugging functions
        self.register_function("dbg_stack", None, &super::nt_dbg_stack);
        self.register_function("dbg_globals", None, &super::nt_dbg_globals);
    }

    fn register_value_method(
        &mut self,
        identifier: &'static str,
        param_count: Option<u8>,
        func: NativeFn,
    ) {
        let func = NativeFunction::alloc(self, identifier, param_count, func);
        self.value_methods.insert(identifier, Value::Object(func));
    }

    fn register_value_methods(&mut self) {
        self.register_value_method("add", Some(2), &value_methods::nt_binary_add);
        self.register_value_method("sub", Some(2), &value_methods::nt_binary_sub);
        self.register_value_method("mul", Some(2), &value_methods::nt_binary_mul);
        self.register_value_method("div", Some(2), &value_methods::nt_binary_div);
    }

    #[inline]
    fn are_numbers(lhs: &Value, rhs: &Value) -> bool {
        matches!(lhs, Value::Number(_)) && lhs.kind_equals(&rhs)
    }

    #[inline]
    fn are_strings(lhs: &Value, rhs: &Value) -> bool {
        if let Value::Object(lobj) = lhs {
            if let Value::Object(robj) = rhs {
                let left_is = matches!(*lobj.upgrade().unwrap().data.borrow(), ObjectData::Str(_));
                let right_is = matches!(*robj.upgrade().unwrap().data.borrow(), ObjectData::Str(_));

                if left_is && right_is {
                    return true;
                }
            }
        }

        false
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

    fn index_item(&mut self, item: Value, value: Value) -> Result<(), SpruceErr> {
        if let Value::Object(obj) = &item {
            let index = if let Value::Number(n) = value {
                n as usize
            } else {
                return Err(SpruceErr::new(
                    format!("Cannot index with non-number '{value}'"),
                    SpruceErrData::VM,
                ));
            };

            let obj = obj.upgrade().unwrap();
            let data = obj.data.borrow();

            match &*data {
                &ObjectData::Str(ref s) => {
                    let value = if index >= s.len() {
                        Value::None
                    } else {
                        let str = self
                            .allocate_string(format!("{}", s.chars().nth(index).unwrap()), false);
                        Value::Object(str)
                    };
                    self.stack.push(value);
                }

                &ObjectData::List(ref values) => {
                    let value = if index >= values.len() {
                        Value::None
                    } else {
                        values[index].clone()
                    };
                    self.push(value);
                }

                &ObjectData::Tuple(ref values) => {
                    let value = if index >= values.len() {
                        Value::None
                    } else {
                        values[index].clone()
                    };
                    self.push(value);
                }

                _ => {
                    return Err(SpruceErr::new(
                        format!("Cannot index into item '{item}'"),
                        SpruceErrData::VM,
                    ))
                }
            }
        } else {
            return Err(SpruceErr::new(
                format!("Cannot index into item '{item}'"),
                SpruceErrData::VM,
            ));
        }

        Ok(())
    }

    fn index_item_set(
        &mut self,
        index_item: Value,
        index_expr: Value,
        value: Value,
    ) -> Result<(), SpruceErr> {
        if let Value::Object(obj) = &index_item {
            let index = if let Value::Number(n) = index_expr {
                n as usize
            } else {
                return Err(SpruceErr::new(
                    format!("Cannot index with non-number '{value}'"),
                    SpruceErrData::VM,
                ));
            };

            let obj = obj.upgrade().unwrap();
            let mut data = obj.data.borrow_mut();

            match &mut *data {
                &mut ObjectData::List(ref mut values) => {
                    if index < values.len() {
                        values[index] = value;
                    }
                }

                _ => {
                    drop(data);
                    return Err(SpruceErr::new(
                        format!("Cannot index set item '{index_item}'"),
                        SpruceErrData::VM,
                    ));
                }
            }
        } else {
            return Err(SpruceErr::new(
                format!("Cannot index set item '{index_item}'"),
                SpruceErrData::VM,
            ));
        }

        Ok(())
    }

    fn get_property(&mut self, item: Value, property: String) -> Result<(), SpruceErr> {
        match &item {
            Value::Object(obj) => {
                match &*obj.upgrade().unwrap().data.borrow() {
                    &ObjectData::Module(ref module) => {
                        if let Some(value) = module.items.get(&property) {
                            // Assign receiver to function
                            if let Value::Object(obj) = &value {
                                match &mut *obj.upgrade().unwrap().data.borrow_mut() {
                                    ObjectData::Function(function, _) => {
                                        Rc::get_mut(function).unwrap().receiver =
                                            Some(item.clone());
                                    }
                                    _ => {}
                                }
                            }

                            self.push(value.clone());
                        } else {
                            return Err(SpruceErr::new(
                                format!("Unknown property '{property}' on {item}"),
                                SpruceErrData::VM,
                            ));
                        }
                    }

                    &ObjectData::StructDef(ref struct_) => {
                        if let Some(value) = struct_.items.get(&property) {
                            // Assign receiver to function
                            if let Value::Object(obj) = &value {
                                match &mut *obj.upgrade().unwrap().data.borrow_mut() {
                                    ObjectData::Function(function, _) => {
                                        Rc::get_mut(function).unwrap().receiver =
                                            Some(item.clone());
                                    }
                                    _ => {}
                                }
                            }

                            self.push(value.clone());
                        } else {
                            return Err(SpruceErr::new(
                                format!("Unknown property '{property}' on {item}"),
                                SpruceErrData::VM,
                            ));
                        }
                    }

                    &ObjectData::StructInstance(ref struct_) => {
                        if let Some(value) = struct_.values.get(&property) {
                            // Assign receiver to function
                            if let Value::Object(obj) = &value {
                                match &mut *obj.upgrade().unwrap().data.borrow_mut() {
                                    ObjectData::Function(function, _) => {
                                        Rc::get_mut(function).unwrap().receiver =
                                            Some(item.clone());
                                    }
                                    _ => {}
                                }
                            }

                            self.push(value.clone());
                        } else {
                            return Err(SpruceErr::new(
                                format!("Unknown property '{property}' on {item}"),
                                SpruceErrData::VM,
                            ));
                        }
                    }

                    n => {
                        return Err(SpruceErr::new(
                            format!("Cannot access property '{property}' on {n}"),
                            SpruceErrData::VM,
                        ))
                    }
                }
            }

            // Try value method
            _ => {
                if let Some(method) = self.value_methods.get(&property.as_str()) {
                    // We want to make sure they have the same type, as they most likely
                    // will require the same type
                    if !item.kind_equals(&self.peek()) {
                        return Err(SpruceErr::new(
                            format!(
                                "Type mismatch in value method {item:?} and {:?}",
                                self.peek()
                            ),
                            SpruceErrData::VM,
                        ));
                    }

                    // Restore lhs onto the stack
                    // NOTE: Cannot just push, as a 0.div(2) will result in 2 / 0
                    self.stack.insert(self.stack.len() - 1, item);
                    self.stack.push(method.clone());
                } else {
                    return Err(SpruceErr::new(
                        format!("Cannot access property '{property}' on {item}"),
                        SpruceErrData::VM,
                    ));
                }
            }
        }

        Ok(())
    }

    fn set_property(
        &mut self,
        item: Value,
        property: String,
        value: Value,
    ) -> Result<(), SpruceErr> {
        if let Value::Object(obj) = &item {
            match &mut *obj.upgrade().unwrap().data.borrow_mut() {
                &mut ObjectData::StructInstance(ref mut struct_) => {
                    let struct_ = Rc::get_mut(struct_).unwrap();

                    if struct_.values.contains_key(&property) {
                        _ = struct_.values.insert(property, value);
                    } else {
                        return Err(SpruceErr::new(
                            format!(
                                "Unknown field '{property}' on struct '{}'",
                                struct_.struct_name
                            ),
                            SpruceErrData::VM,
                        ));
                    }
                }

                n => {
                    return Err(SpruceErr::new(
                        format!("Cannot access property '{property}' on {n}"),
                        SpruceErrData::VM,
                    ))
                }
            }
        } else {
            return Err(SpruceErr::new(
                format!("Cannot access property '{property}' on {item}"),
                SpruceErrData::VM,
            ));
        }

        Ok(())
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

    fn logical_op(&mut self, op: ByteCode, lhs: Value, rhs: Value) -> Result<(), SpruceErr> {
        self.stack.push(match op {
            ByteCode::Greater => Value::Boolean(lhs > rhs),
            ByteCode::GreaterEq => Value::Boolean(lhs >= rhs),

            ByteCode::Less => Value::Boolean(lhs < rhs),
            ByteCode::LessEq => Value::Boolean(lhs <= rhs),

            ByteCode::Equal => Value::Boolean(lhs == rhs),
            ByteCode::NotEqual => Value::Boolean(lhs != rhs),

            ByteCode::Or => {
                let (lhs, rhs) = match (&lhs, &rhs) {
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => (lhs, rhs),
                    _ => unreachable!(),
                };

                Value::Boolean(*lhs || *rhs)
            }

            ByteCode::And => {
                let (lhs, rhs) = match (&lhs, &rhs) {
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => (lhs, rhs),
                    _ => unreachable!(),
                };

                Value::Boolean(*lhs && *rhs)
            }

            _ => unreachable!(),
        });

        Ok(())
    }

    fn binary_op_strings(&mut self, op: ByteCode, lhs: Value, rhs: Value) -> Result<(), SpruceErr> {
        let Value::Object(robj) = rhs else { unreachable!() };
        let Value::Object(lobj) = lhs else { unreachable!() };

        let robj = robj.upgrade().unwrap();
        let lobj = lobj.upgrade().unwrap();

        let ObjectData::Str(rstr) = &*robj.data.borrow() else { unreachable!() };
        let ObjectData::Str(lstr) = &*lobj.data.borrow() else { unreachable!() };

        match op {
            ByteCode::Add => {
                let obj = self.allocate_string(format!("{lstr}{rstr}"), RUNTIME_INTERNING);
                self.stack.push(Value::Object(obj));
            }

            _ => {
                return Err(SpruceErr::new(
                    format!("Cannot use '{op:?}' on strings"),
                    SpruceErrData::VM,
                ))
            }
        }

        Ok(())
    }

    #[inline]
    fn peek(&self) -> Value {
        self.stack.last().unwrap().clone()
    }

    #[inline]
    fn get_function(&self) -> Rc<Function> {
        let ObjectData::Function(ref function, _) =
            &*self.call_stack.last().unwrap().function.data.borrow() else { unreachable!() };
        Rc::clone(function)
    }

    #[inline]
    fn func_stack_start(&self) -> usize {
        self.call_stack.last().unwrap().stack_start
    }

    #[inline]
    fn set_current_ip(&mut self, ip: usize) {
        self.call_stack.last_mut().unwrap().ip = ip;
    }

    #[inline]
    fn get_next_inst(&mut self) -> ByteCode {
        let function = self.get_function();
        let frame = self.call_stack.last_mut().unwrap();

        if function.code.len() == 0 {
            self.stack.push(Value::None);
            return ByteCode::Return;
        }

        let code = function.code[frame.ip];
        frame.ip += 1;

        code
    }
}
