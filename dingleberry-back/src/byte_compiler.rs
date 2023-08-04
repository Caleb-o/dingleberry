use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use dingleberry_front::{
    ast::{Ast, AstData},
    ast_inner::{
        BinaryOp, ForStatement, FunctionCall, IfStatement, Include, IndexGetter, IndexSetter,
        LogicalOp, PropertyGetter, PropertySetter, VarAssign, VarDeclaration, WhileStatement,
    },
    token::{Token, TokenKind},
};
use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{
    bytecode::ByteCode,
    get_identifier_or_string,
    object::{ClassDef, Module, ObjectData, StructDef},
    symbol_table::{Symbol, SymbolTable},
    type_name_to_int,
    value::Value,
    vm::VM,
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Context {
    None,
    Function(bool),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ContainerContext {
    None,
    Module,
    Struct,
    Class,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub is_static: bool,
    pub identifier: Option<String>,
    pub arg_count: u8,
    pub code: Vec<u8>,

    pub receiver: Option<Value>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.identifier == other.identifier && self.arg_count == other.arg_count
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.identifier.partial_cmp(&other.identifier) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.arg_count.partial_cmp(&other.arg_count) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.receiver.partial_cmp(&other.receiver)
    }
}

impl Function {
    fn new(is_static: bool, identifier: Option<String>, arg_count: u8) -> Rc<Self> {
        Rc::new(Self {
            is_static,
            identifier,
            arg_count,
            code: Vec::new(),
            receiver: None,
        })
    }
}

enum CurrentType {
    Module(Module),
    StructDef(StructDef),
    ClassDef(ClassDef),
}

struct CurrentKind {
    last: Option<Box<CurrentKind>>,
    current: CurrentType,
}

impl CurrentKind {
    fn new_with(current: CurrentType, last: Option<Box<CurrentKind>>) -> Self {
        Self { current, last }
    }
}

pub struct ByteCompiler<'a> {
    current_func: Option<Rc<Function>>,
    current_kind: Option<CurrentKind>,
    symbol_table: SymbolTable,
    ctx: Context,
    container_ctx: ContainerContext,
    vm: &'a mut VM,
    had_error: usize,
}

impl<'a> ByteCompiler<'a> {
    pub fn new(vm: &'a mut VM) -> Self {
        Self {
            current_func: Some(Function::new(true, Some("script".into()), 0)),
            current_kind: None,
            symbol_table: SymbolTable::new(),
            ctx: Context::None,
            container_ctx: ContainerContext::None,
            vm,
            had_error: 0,
        }
    }

    pub fn compile(mut self, root: Box<Ast>) -> Result<Value, SpruceErr> {
        self.visit(&root)?;

        self.write_many(&[ByteCode::None, ByteCode::Return]);

        let ByteCompiler { current_func, .. } = self;
        let current_func = current_func.unwrap();

        let func = self
            .vm
            .allocate(ObjectData::Function(Rc::clone(&current_func), false));

        if self.had_error > 0 {
            return Err(SpruceErr::new(
                format!("Encountered {} error(s) while compiling", self.had_error),
                SpruceErrData::Analyser,
            ));
        }

        Ok(Value::Object(func))
    }

    fn rollback_last_kind(&mut self) -> CurrentType {
        let CurrentKind { last, current } = self.current_kind.take().unwrap();
        if let Some(last) = last {
            self.current_kind = Some(*last);
        }
        current
    }

    fn error(&mut self, err: SpruceErr) {
        self.had_error += 1;
        err.display();
    }

    fn add_constant(&mut self, value: Value) -> u16 {
        self.vm.constants.push(value);
        (self.vm.constants.len() - 1) as u16
    }

    fn find_str_constant(&self, id: &str) -> Option<u16> {
        for (idx, v) in self.vm.constants.iter().enumerate() {
            match v {
                Value::Object(obj) => match &*obj.upgrade().unwrap().data.borrow() {
                    &ObjectData::Str(ref s) => {
                        if *s == id {
                            return Some(idx as u16);
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        None
    }

    fn find_constant(&self, value: &Value) -> Option<u16> {
        for (idx, v) in self.vm.constants.iter().enumerate() {
            if *v == *value {
                return Some(idx as u16);
            }
        }

        None
    }

    fn get_string_or_insert(&mut self, identifier: String) -> u16 {
        let str = self.vm.allocate_string(identifier, true);
        let str = Value::Object(str);

        if let Some(index) = self.find_constant(&str) {
            index
        } else {
            self.add_constant(str)
        }
    }

    fn set_current_struct(&mut self, struct_def: StructDef) {
        let last = self.current_kind.take();
        self.current_kind = Some(CurrentKind::new_with(
            CurrentType::StructDef(struct_def),
            last.and_then(|l| Some(Box::new(l))),
        ));
    }

    fn set_current_class(&mut self, class_def: ClassDef) {
        let last = self.current_kind.take();
        self.current_kind = Some(CurrentKind::new_with(
            CurrentType::ClassDef(class_def),
            last.and_then(|l| Some(Box::new(l))),
        ));
    }

    fn set_current_mod(&mut self, module: Module) {
        let last = self.current_kind.take();
        self.current_kind = Some(CurrentKind::new_with(
            CurrentType::Module(module),
            last.and_then(|l| Some(Box::new(l))),
        ));
    }

    fn get_current_struct(&self) -> &StructDef {
        match &self.current_kind {
            Some(kind) => {
                if let CurrentType::StructDef(struct_def) = &kind.current {
                    return &struct_def;
                }

                unreachable!();
            }
            None => unreachable!(),
        }
    }

    fn get_current_struct_mut(&mut self) -> &mut StructDef {
        match &mut self.current_kind {
            Some(ref mut kind) => {
                if let CurrentType::StructDef(ref mut struct_def) = &mut kind.current {
                    return struct_def;
                }

                unreachable!();
            }
            None => unreachable!(),
        }
    }

    fn get_current_class_mut(&mut self) -> &mut ClassDef {
        match &mut self.current_kind {
            Some(ref mut kind) => {
                if let CurrentType::ClassDef(ref mut class_def) = &mut kind.current {
                    return class_def;
                }

                unreachable!();
            }
            None => unreachable!(),
        }
    }

    #[inline]
    fn open_module(&mut self, identifier: String) {
        self.symbol_table.new_func();
        self.set_current_mod(Module {
            identifier,
            items: HashMap::new(),
        });
    }

    fn close_module(&mut self) -> u16 {
        self.symbol_table.close_func();

        let last_kind = self.rollback_last_kind();
        let CurrentType::Module(mut module) = last_kind else { unreachable!() };
        // Fields cannot be added after it's created, so we can shrink
        module.items.shrink_to_fit();

        let identifier = module.identifier.clone();

        let module = self.vm.allocate(ObjectData::Module(Rc::new(module)));
        self.vm.constants.push(Value::Object(module.clone()));

        if self.current_kind.is_none() {
            self.vm.globals.insert(identifier, Value::Object(module));
        }

        (self.vm.constants.len() - 1) as u16
    }

    #[inline]
    fn open_struct(&mut self, is_static: bool, identifier: String) {
        self.symbol_table.new_func();
        self.set_current_struct(StructDef {
            is_static,
            identifier,
            init_items: None,
            items: HashMap::new(),
        });
    }

    fn close_struct(&mut self) -> u16 {
        self.symbol_table.close_func();

        let last_kind = self.rollback_last_kind();
        let CurrentType::StructDef(mut struct_def) = last_kind else { unreachable!() };

        // Fields cannot be added after it's created, so we can shrink
        struct_def.items.shrink_to_fit();

        let identifier = struct_def.identifier.clone();
        let struct_def = self.vm.allocate(ObjectData::StructDef(Rc::new(struct_def)));
        self.vm.constants.push(Value::Object(struct_def.clone()));

        if self.current_kind.is_none() {
            self.vm
                .globals
                .insert(identifier, Value::Object(struct_def));
        }

        (self.vm.constants.len() - 1) as u16
    }

    #[inline]
    fn open_class(&mut self, is_static: bool, identifier: String) {
        self.symbol_table.new_func();
        self.set_current_class(ClassDef {
            is_static,
            identifier,
            init_items: None,
            super_class: None,
            items: HashMap::new(),
        });
    }

    fn close_class(&mut self, super_class: &Option<Box<Ast>>) -> Result<u16, SpruceErr> {
        self.symbol_table.close_func();

        let last_kind = self.rollback_last_kind();
        let CurrentType::ClassDef(mut class_def) = last_kind else { unreachable!() };

        // Fields cannot be added after it's created, so we can shrink
        class_def.items.shrink_to_fit();

        // This will add the inheritted class at runtime
        if super_class.is_some() {
            self.visit(super_class.as_ref().unwrap())?;
            self.write_op_u16(ByteCode::ConstantShort, self.vm.constants.len() as u16);
            self.write(ByteCode::Inherit);
        }

        let identifier = class_def.identifier.clone();
        let class_def = self.vm.allocate(ObjectData::ClassDef(Rc::new(class_def)));
        self.vm.constants.push(Value::Object(class_def.clone()));

        if self.current_kind.is_none() {
            self.vm.globals.insert(identifier, Value::Object(class_def));
        }

        Ok((self.vm.constants.len() - 1) as u16)
    }

    #[inline]
    fn add_item_to_current(&mut self, identifier: String, value: Value) {
        if let Some(ref mut current_type) = &mut self.current_kind {
            match &mut current_type.current {
                CurrentType::Module(ref mut module) => module.add_item(identifier, value),
                CurrentType::StructDef(ref mut struct_def) => {
                    struct_def.add_item(identifier, value)
                }
                CurrentType::ClassDef(ref mut class_def) => class_def.add_item(identifier, value),
            }
        } else {
            unreachable!();
        }
    }

    #[inline]
    fn open_function(&mut self, is_static: bool, identifier: Option<String>, arg_count: u8) {
        self.current_func = Some(Function::new(is_static, identifier, arg_count));
        self.symbol_table.new_func();
    }

    fn close_function(
        &mut self,
        anonymous: bool,
        has_variadic: bool,
        last_fn: Option<Rc<Function>>,
    ) -> Option<u16> {
        self.symbol_table.close_func();

        let func = self.current_func.take().unwrap();
        self.current_func = last_fn;

        if anonymous || self.current_kind.is_some() {
            let obj = self.vm.allocate(ObjectData::Function(func, has_variadic));
            self.vm.constants.push(Value::Object(obj));
            Some((self.vm.constants.len() - 1) as u16)
        } else {
            let identifier = func.identifier.as_ref().unwrap().clone();
            let obj = self.vm.allocate(ObjectData::Function(func, has_variadic));
            self.vm.globals.insert(identifier, Value::Object(obj));
            None
        }
    }

    fn get_filepath(token: &Token) -> Option<String> {
        token
            .lexeme
            .as_ref()
            .map(|span| (*span.source.file_path).clone())
    }

    #[inline]
    fn func(&mut self) -> &mut Function {
        Rc::get_mut(self.current_func.as_mut().unwrap()).unwrap()
    }

    #[inline]
    fn write(&mut self, op: ByteCode) {
        self.func().code.push(op as u8);
    }

    #[inline]
    fn write_op_u8(&mut self, op: ByteCode, index: u8) {
        self.func().code.extend(&[op as u8, index]);
    }

    #[inline]
    fn write_op_u16(&mut self, op: ByteCode, index: u16) {
        let [l, r] = index.to_le_bytes();
        self.func().code.extend(&[op as u8, l, r]);
    }

    #[inline]
    fn write_many(&mut self, ops: &[ByteCode]) {
        self.func()
            .code
            .extend(ops.iter().map(|o| *o as u8).collect::<Vec<_>>());
    }

    #[inline]
    fn write_op_u16_inplace(&mut self, start: usize, op: ByteCode, value: u16) {
        let [l, r] = value.to_le_bytes();
        self.func().code[start] = op as u8;
        self.func().code[start + 1] = l;
        self.func().code[start + 2] = r;
    }

    #[inline]
    fn inject_symbol(&mut self, identifier: &str, is_mutable: bool) {
        self.symbol_table
            .inject_symbol(identifier.to_string(), is_mutable);
    }

    fn add_symbol(
        &mut self,
        token: &Token,
        is_mutable: bool,
        allow_override: bool,
    ) -> Result<(), SpruceErr> {
        if self.symbol_table.is_global() {
            let slice = token.lexeme.as_ref().unwrap().get_slice();
            let index = if let Some(index) = self.find_str_constant(slice) {
                index
            } else {
                let string = self.vm.allocate_string(slice.to_string(), true);
                let string = Value::Object(string);
                self.add_constant(string)
            };

            _ = self
                .symbol_table
                .add_global_symbol(token, is_mutable, index)
                .map_err(|e| self.error(e));
        } else {
            _ = self
                .symbol_table
                .add_symbol(token, is_mutable, allow_override)
                .map_err(|e| self.error(e));
        }

        Ok(())
    }

    fn visit(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        match &item.data {
            &AstData::Program(ref statements) => self.visit_program(statements),
            &AstData::Include(ref incl) => self.visit_include(incl),
            &AstData::Empty => self.visit_empty(item),

            &AstData::FieldVarDeclarations(ref decls) => self.visit_field_var_declarations(decls),
            &AstData::FieldVarDeclaration => self.visit_field_var_declaration(item),

            &AstData::VarDeclarations(ref decls) => self.visit_var_declarations(decls),
            &AstData::VarDeclaration(ref decl) => self.visit_var_declaration(item, decl),
            &AstData::VarAssign(ref assign) => self.visit_var_assign(item, assign),

            &AstData::Function(ref func) => self.visit_function(item, func),
            &AstData::FunctionCall(ref fncall) => self.visit_function_call(item, fncall),

            &AstData::BinaryOp(ref bin) => self.visit_binary_op(item, bin),
            &AstData::UnaryOp(ref rhs) => self.visit_unary_op(rhs),
            &AstData::LogicalOp(ref logic) => self.visit_logical_op(item, logic),

            &AstData::ForStatement(ref fstmt) => self.visit_for_statement(fstmt),
            &AstData::WhileStatement(ref wstmt) => self.visit_while_statement(wstmt),
            &AstData::LoopStatement(ref body) => self.visit_loop_statement(body),

            &AstData::IndexGetter(ref getter) => self.visit_index_getter(getter),
            &AstData::IndexSetter(ref setter) => self.visit_index_setter(setter),

            &AstData::PropertyGetter(ref getter) => self.visit_property_getter(getter),
            &AstData::PropertySetter(ref setter) => self.visit_property_setter(setter),

            &AstData::Yield(ref maybe_expr) => self.visit_yield(item, maybe_expr),
            &AstData::Resume(ref expr) => self.visit_resume(expr),

            &AstData::TypeOf => self.visit_type_of(item),

            &AstData::Body(ref statements) => self.visit_body(statements, true),
            &AstData::IfStatement(ref statement) => self.visit_if_statement(statement),
            &AstData::This => self.visit_this(item),
            &AstData::Return(ref expr) => self.visit_return_statement(item, expr),
            &AstData::Module(ref items) => self.visit_module(item, items),
            &AstData::StructDef(ref struct_def) => self.visit_struct_def(item, struct_def),
            &AstData::ClassDef(ref class_def) => self.visit_class_def(item, class_def),
            &AstData::Identifier => self.visit_identifier(item),
            &AstData::Literal => self.visit_literal(item),
            &AstData::ArrayLiteral(_) => self.visit_array_literal(item),
            &AstData::TupleLiteral(_) => self.visit_tuple_literal(item),
            &AstData::ExpressionStatement(_, _) => self.visit_expression_statement(item),
            n => todo!("Visit {n:?}"),
        }
    }

    fn visit_identifier(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let maybe_values = self.symbol_table.find_symbol_any(&item.token)?;

        match maybe_values {
            Some(Symbol { index, depth, .. }) => {
                // FIXME: Hack to allow use of fields without 'this.'
                // if self.container_ctx == ContainerContext::Struct {
                //     let identifier = identifier.to_string();
                //     let identifier = self.get_string_or_insert(identifier);

                //     self.func()
                //         .code
                //         .extend([ByteCode::This, ByteCode::PropertyGet(identifier)]);
                // } else
                if depth == 0 {
                    self.write_op_u16(ByteCode::GetGlobal, index);
                } else {
                    self.write_op_u8(ByteCode::GetLocal, index as u8);
                }
            }
            None => {
                let identifier = item.token.lexeme.as_ref().unwrap().get_slice();
                let index = self.get_string_or_insert(identifier.to_string());
                self.write_op_u16(ByteCode::GetGlobal, index);
            }
        }

        Ok(())
    }

    fn visit_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Literal = &item.data else { unreachable!() };

        let value: Value = if item.token.kind == TokenKind::String {
            let slice = item.token.lexeme.as_ref().unwrap().get_slice();
            // Remove quotes
            let string = slice[1..slice.len() - 1].to_string();
            Value::Object(self.vm.allocate_string(string, true))
        } else {
            item.token.clone().into()
        };

        if let Some(index) = self.find_constant(&value) {
            self.write_op_u8(ByteCode::ConstantByte, index as u8);
        } else {
            let index = self.add_constant(value);
            self.write_op_u8(ByteCode::ConstantByte, index as u8);
        }

        Ok(())
    }

    fn visit_tuple_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::TupleLiteral(values) = &item.data else { unreachable!() };

        for value in values {
            self.visit(value)?;
        }

        if values.len() > u16::MAX as usize {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                "More than 65535 values in tuple literal".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.write_op_u16(ByteCode::IntoTuple, values.len() as u16);

        Ok(())
    }

    fn visit_array_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ArrayLiteral(values) = &item.data else { unreachable!() };

        for value in values {
            self.visit(value)?;
        }

        if values.len() > u16::MAX as usize {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                "More than 65535 values in array literal".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.write_op_u16(ByteCode::IntoList, values.len() as u16);

        Ok(())
    }

    fn visit_expression_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ExpressionStatement(is_stmt, expr) = &item.data else { unreachable!() };

        self.visit(expr)?;

        if *is_stmt {
            match expr.data {
                AstData::Body(_) => {}
                _ => self.write(ByteCode::Pop),
            }
        } else {
            if self.ctx == Context::Function(true) {
                self.write_many(&[ByteCode::WrapYielded, ByteCode::Return]);
            } else {
                self.write(ByteCode::Return);
            }
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, item: &Box<Ast>, binary: &BinaryOp) -> Result<(), SpruceErr> {
        let BinaryOp { lhs, rhs } = &binary;

        self.visit(lhs)?;
        self.visit(rhs)?;

        self.write(match item.token.kind {
            TokenKind::Plus => ByteCode::Add,
            TokenKind::Minus => ByteCode::Sub,
            TokenKind::Star => ByteCode::Mul,
            TokenKind::Slash => ByteCode::Div,

            _ => unreachable!(),
        });

        Ok(())
    }

    fn visit_unary_op(&mut self, rhs: &Box<Ast>) -> Result<(), SpruceErr> {
        self.visit(rhs)?;
        self.write(ByteCode::Negate);

        Ok(())
    }

    fn visit_logical_op(&mut self, item: &Box<Ast>, logic: &LogicalOp) -> Result<(), SpruceErr> {
        // FIXME: This is inefficient for the case of OR
        self.visit(&logic.lhs)?;
        self.visit(&logic.rhs)?;

        self.write(match item.token.kind {
            TokenKind::Greater => ByteCode::Greater,
            TokenKind::GreaterEqual => ByteCode::GreaterEq,

            TokenKind::Less => ByteCode::Less,
            TokenKind::LessEqual => ByteCode::LessEq,

            TokenKind::EqualEqual => ByteCode::Equal,
            TokenKind::NotEqual => ByteCode::NotEqual,

            TokenKind::Or => ByteCode::Or,
            TokenKind::And => ByteCode::And,

            _ => unreachable!(),
        });

        Ok(())
    }

    fn visit_parameter(
        &mut self,
        item: &Box<Ast>,
        position: usize,
        parameters: &Vec<Box<Ast>>,
    ) -> Result<bool, SpruceErr> {
        let AstData::Parameter(is_variadic) = &item.data else { unreachable!() };

        _ = self
            .add_symbol(&item.token, false, false)
            .map_err(|e| self.error(e));

        if *is_variadic && position != parameters.len() - 1 {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                format!(
                    "Parameter '{}' is variadic, but is in position {} of {}",
                    item.token.lexeme.as_ref().unwrap().get_slice(),
                    position + 1,
                    parameters.len()
                ),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        Ok(*is_variadic)
    }

    fn visit_function(
        &mut self,
        item: &Box<Ast>,
        func: &dingleberry_front::ast_inner::Function,
    ) -> Result<(), SpruceErr> {
        let dingleberry_front::ast_inner::Function {
            is_static,
            anonymous,
            yields,
            parameters,
            body,
        } = &func;

        let last_ctx = self.ctx;
        self.ctx = Context::Function(*yields);

        let last_function = self.current_func.take();

        let identifier = if item.token.lexeme.is_some() {
            Some(item.token.lexeme.as_ref().unwrap().get_slice().to_string())
        } else {
            None
        };

        if !anonymous {
            _ = self
                .add_symbol(&item.token, false, false)
                .map_err(|e| self.error(e));
        }

        self.open_function(
            *is_static,
            identifier.clone(),
            parameters
                .as_ref()
                .map(|p| p.len() as u8)
                .unwrap_or_default(),
        );

        if *anonymous {
            self.symbol_table.lock();
        }

        let mut has_variadic = false;
        if let Some(parameters) = parameters {
            for (idx, param) in parameters.iter().enumerate() {
                has_variadic = self.visit_parameter(param, idx, parameters)?;
            }
        }

        if self.container_ctx != ContainerContext::None {
            self.inject_symbol("self", false);
        }

        match &body.data {
            AstData::Body(statements) => {
                self.visit_body(statements, false)?;

                if statements.len() > 0 {
                    match &statements.last().unwrap().data {
                        &AstData::Return(_) => {}
                        _ => {
                            if self.ctx == Context::Function(true) {
                                self.write_many(&[
                                    ByteCode::None,
                                    ByteCode::WrapYielded,
                                    ByteCode::Return,
                                ]);
                            } else {
                                self.write_many(&[ByteCode::None, ByteCode::Return]);
                            }
                        }
                    }
                } else {
                    if self.ctx == Context::Function(true) {
                        self.write_many(&[ByteCode::None, ByteCode::WrapYielded, ByteCode::Return]);
                    } else {
                        self.write_many(&[ByteCode::None, ByteCode::Return]);
                    }
                }
            }
            AstData::Return(expr) => self.visit_return_statement(body, expr)?,
            _ => unreachable!(),
        }

        if let Some(index) = self.close_function(*anonymous, has_variadic, last_function) {
            if !anonymous {
                let func = &self.vm.constants[index as usize];
                if self.current_kind.is_some() {
                    self.add_item_to_current(identifier.unwrap().clone(), func.clone());
                }
            } else {
                self.write_op_u8(ByteCode::ConstantByte, index as u8);
            }
        }

        if *anonymous {
            self.symbol_table.unlock();
        }

        self.ctx = last_ctx;
        Ok(())
    }

    fn visit_function_call(
        &mut self,
        item: &Box<Ast>,
        fncall: &FunctionCall,
    ) -> Result<(), SpruceErr> {
        let FunctionCall { lhs, arguments } = &fncall;

        for arg in arguments {
            self.visit(arg)?;
        }

        self.visit(lhs)?;

        let literal_call = match &lhs.data {
            AstData::PropertyGetter(PropertyGetter { lhs, .. }) => {
                if let AstData::Literal = &lhs.data {
                    1
                } else {
                    0
                }
            }
            _ => 0,
        } as u8;

        if arguments.len() > u8::MAX as usize {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                "More than 256 arguments in function call".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.write_op_u8(ByteCode::Call, arguments.len() as u8 + literal_call);

        Ok(())
    }

    fn visit_var_declaration(
        &mut self,
        item: &Box<Ast>,
        decl: &VarDeclaration,
    ) -> Result<(), SpruceErr> {
        let VarDeclaration {
            is_mutable,
            expression,
        } = &decl;

        if let Some(expression) = expression {
            self.visit(expression)?;
        }

        let slice = item.token.lexeme.as_ref().unwrap().get_slice();

        if self.symbol_table.is_global() {
            let index = if let Some(index) = self.find_str_constant(slice) {
                index
            } else {
                let string = self.vm.allocate_string(slice.to_string(), true);
                let string = Value::Object(string);
                self.add_constant(string)
            };

            self.write_op_u16(ByteCode::DefineGlobal, index);

            self.symbol_table
                .add_global_symbol(&item.token, *is_mutable, index)?;
        } else if self.symbol_table.find_local_symbol(slice).is_some() {
            // Add new symbol
            _ = self
                .symbol_table
                .add_symbol(&item.token, *is_mutable, true)
                .map_err(|e| self.error(e));
        } else {
            _ = self
                .symbol_table
                .add_symbol(&item.token, *is_mutable, false)
                .map_err(|e| self.error(e));
        }

        Ok(())
    }

    fn visit_var_declarations(&mut self, decls: &Vec<Box<Ast>>) -> Result<(), SpruceErr> {
        for item in decls {
            let AstData::VarDeclaration(ref decl) = &item.data else { unreachable!() };
            self.visit_var_declaration(item, decl)?;
        }

        Ok(())
    }

    fn visit_field_var_declaration(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let field_name = get_identifier_or_string(&item.token);
        self.add_item_to_current(field_name, Value::None);

        _ = self
            .symbol_table
            .add_symbol(&item.token, false, false)
            .map_err(|e| self.error(e));

        Ok(())
    }

    fn visit_field_var_declarations(&mut self, decls: &Vec<Box<Ast>>) -> Result<(), SpruceErr> {
        for item in decls {
            self.visit_field_var_declaration(item)?;
        }

        Ok(())
    }

    fn visit_var_assign(&mut self, item: &Box<Ast>, assign: &VarAssign) -> Result<(), SpruceErr> {
        let VarAssign { lhs, expression } = &assign;
        self.visit(expression)?;

        if let Some(Symbol {
            identifier,
            mutable,
            index,
            depth,
        }) = self.symbol_table.find_symbol_any(&lhs.token)?
        {
            if !mutable {
                let file_path = Self::get_filepath(&item.token);

                self.error(SpruceErr::new(
                    format!("Cannot mutate immutable identifier '{identifier}'"),
                    SpruceErrData::Compiler {
                        file_path,
                        line: item.token.line,
                        column: item.token.column,
                    },
                ));
            }

            if depth == 0 {
                self.write_op_u16(ByteCode::SetGlobal, index);
            } else {
                self.write_op_u8(ByteCode::SetLocal, index as u8);
            };
        } else {
            let file_path = Self::get_filepath(&item.token);
            let identifier = lhs.token.lexeme.as_ref().unwrap().get_slice();

            self.error(SpruceErr::new(
                format!("Binding '{identifier}' does not exist"),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        Ok(())
    }

    fn visit_if_statement(&mut self, statement: &IfStatement) -> Result<(), SpruceErr> {
        let IfStatement {
            condition,
            true_body,
            false_body,
            ..
        } = statement;

        self.visit(condition)?;
        let jump_loc = self.func().code.len();
        self.write_op_u16(ByteCode::Error, 0);

        self.visit(true_body)?;

        if let Some(false_body) = false_body {
            let true_jump_loc = self.func().code.len();
            self.write_op_u16(ByteCode::Error, 0);

            // Relative jump
            let len = self.func().code.len() as u16;
            self.write_op_u16_inplace(jump_loc, ByteCode::JumpNot, len);

            self.visit(false_body)?;

            let len = self.func().code.len() as u16;
            self.write_op_u16_inplace(true_jump_loc, ByteCode::Jump, len);
        } else {
            let len = self.func().code.len() as u16;
            self.write_op_u16_inplace(jump_loc, ByteCode::JumpNot, len);
        }

        Ok(())
    }

    fn visit_for_statement(&mut self, fstmt: &ForStatement) -> Result<(), SpruceErr> {
        let ForStatement {
            variable,
            expression,
            body,
        } = &fstmt;

        // Infinite loop
        if variable.is_none() && expression.is_none() {
            let here = self.func().code.len() as u16;
            self.visit(body)?;
            self.write_op_u16(ByteCode::Jump, here);
        } else {
            todo!()
        }

        Ok(())
    }

    fn visit_while_statement(&mut self, wstmt: &WhileStatement) -> Result<(), SpruceErr> {
        let WhileStatement { expression, body } = wstmt;

        let before_expr = self.func().code.len();
        self.visit(expression)?;
        let before_loop = self.func().code.len();
        self.write_op_u16(ByteCode::Error, 0);

        self.visit(body)?;
        self.write_op_u16(ByteCode::Jump, before_expr as u16);

        let len = self.func().code.len() as u16;
        self.write_op_u16_inplace(before_loop, ByteCode::JumpNot, len);

        Ok(())
    }

    fn visit_loop_statement(&mut self, inner: &Box<Ast>) -> Result<(), SpruceErr> {
        let before_loop = self.func().code.len();
        self.visit(inner)?;
        self.write_op_u16(ByteCode::Jump, before_loop as u16);

        Ok(())
    }

    fn visit_index_getter(&mut self, getter: &IndexGetter) -> Result<(), SpruceErr> {
        let IndexGetter { expression, index } = &getter;

        self.visit(expression)?;
        self.visit(index)?;

        self.write(ByteCode::IndexGet);

        Ok(())
    }

    fn visit_index_setter(&mut self, setter: &IndexSetter) -> Result<(), SpruceErr> {
        let IndexSetter { expression, rhs } = &setter;
        let AstData::IndexGetter(IndexGetter { expression, index }) = &expression.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(index)?;
        self.visit(rhs)?;

        self.write(ByteCode::IndexSet);

        Ok(())
    }

    fn visit_property_getter(&mut self, getter: &PropertyGetter) -> Result<(), SpruceErr> {
        let PropertyGetter { lhs, property } = &getter;

        self.visit(lhs)?;

        let identifier = get_identifier_or_string(&property.token);
        let identifier = self.get_string_or_insert(identifier);
        self.write_op_u16(ByteCode::PropertyGet, identifier);

        Ok(())
    }

    fn visit_property_setter(&mut self, setter: &PropertySetter) -> Result<(), SpruceErr> {
        let PropertySetter { lhs, expression } = &setter;
        let AstData::PropertyGetter(PropertyGetter { lhs, property }) = &lhs.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(lhs)?;

        let identifier = get_identifier_or_string(&property.token);
        let identifier = self.get_string_or_insert(identifier);
        self.write_op_u16(ByteCode::PropertySet, identifier);

        Ok(())
    }

    fn visit_yield(
        &mut self,
        item: &Box<Ast>,
        maybe_expr: &Option<Box<Ast>>,
    ) -> Result<(), SpruceErr> {
        if self.ctx != Context::Function(true) {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                "Cannot yield outside of a function or inside a function that does not yield"
                    .into(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        if let Some(expr) = maybe_expr {
            self.visit(expr)?;
        } else {
            self.write(ByteCode::None);
        }

        self.write(ByteCode::Yield);
        Ok(())
    }

    fn visit_resume(&mut self, expr: &Box<Ast>) -> Result<(), SpruceErr> {
        self.visit(expr)?;
        self.write(ByteCode::Resume);
        Ok(())
    }

    fn visit_type_of(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let type_name = item.token.lexeme.as_ref().unwrap().get_slice();
        let type_idx = type_name_to_int(type_name);

        if type_idx == u8::MAX {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                format!("Invalid type name '{type_name}' for type value"),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        let value = Value::Number(type_idx as f32);

        if let Some(index) = self.find_constant(&value) {
            self.write_op_u8(ByteCode::ConstantByte, index as u8);
        } else {
            let index = self.add_constant(value);
            self.write_op_u8(ByteCode::ConstantByte, index as u8);
        }

        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        item: &Box<Ast>,
        expr: &Option<Box<Ast>>,
    ) -> Result<(), SpruceErr> {
        if let Some(expr) = expr {
            self.visit(expr)?;
        }

        if self.ctx == Context::Function(true) {
            self.write_many(&[ByteCode::WrapYielded, ByteCode::Return]);
        } else {
            self.write(ByteCode::Return);
        }

        if !matches!(self.ctx, Context::Function(_)) {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                "Cannot return outside function".into(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        Ok(())
    }

    fn visit_body(&mut self, statements: &Vec<Box<Ast>>, new_scope: bool) -> Result<(), SpruceErr> {
        if new_scope {
            self.symbol_table.new_scope();
        }

        for (idx, stmt) in statements.iter().enumerate() {
            self.visit(stmt)?;

            if let AstData::ExpressionStatement(is_stmt, _) = &stmt.data {
                if !is_stmt && idx != statements.len() - 1 {
                    let file_path = Self::get_filepath(&stmt.token);

                    self.error(SpruceErr::new(
                        "Cannot use bare expression unless it is the last expression in the body"
                            .into(),
                        SpruceErrData::Compiler {
                            file_path,
                            line: stmt.token.line,
                            column: stmt.token.column,
                        },
                    ));
                }
            }
        }

        if new_scope {
            self.symbol_table.close_scope();
        }

        Ok(())
    }

    fn visit_include(&mut self, incl: &Include) -> Result<(), SpruceErr> {
        if let Some(module_name) = &incl.module_name {
            let last_item = self.current_kind.take();

            let last_container = self.container_ctx;
            self.container_ctx = ContainerContext::Module;

            self.open_module(module_name.lexeme.as_ref().unwrap().get_slice().to_string());
            self.visit(&incl.root)?;

            _ = self.close_module();
            _ = self
                .add_symbol(&module_name, false, false)
                .map_err(|e| self.error(e));

            self.container_ctx = last_container;
            self.current_kind = last_item;
        } else {
            self.visit(&incl.root)?;
        }

        Ok(())
    }

    fn visit_program(&mut self, statements: &Vec<Box<Ast>>) -> Result<(), SpruceErr> {
        for (idx, stmt) in statements.iter().enumerate() {
            self.visit(stmt)?;

            if let AstData::ExpressionStatement(is_stmt, _) = &stmt.data {
                if !is_stmt && idx != statements.len() - 1 {
                    let file_path = Self::get_filepath(&stmt.token);

                    self.error(SpruceErr::new(
                        "Cannot use bare expression unless it is the last expression in the body"
                            .into(),
                        SpruceErrData::Compiler {
                            file_path,
                            line: stmt.token.line,
                            column: stmt.token.column,
                        },
                    ));
                }
            }
        }

        Ok(())
    }

    fn visit_empty(&mut self, _: &Box<Ast>) -> Result<(), SpruceErr> {
        Ok(())
    }

    fn visit_module(&mut self, item: &Box<Ast>, items: &Vec<Box<Ast>>) -> Result<(), SpruceErr> {
        let last_ctx = self.container_ctx;
        self.container_ctx = ContainerContext::Module;

        let identifier = item.token.lexeme.as_ref().unwrap().get_slice().to_string();
        self.open_module(identifier.clone());

        for item in items {
            self.visit(item)?;
        }

        let module_idx = self.close_module();
        _ = self
            .add_symbol(&item.token, false, false)
            .map_err(|e| self.error(e));

        if self.current_kind.is_some() {
            let module = &self.vm.constants[module_idx as usize];
            self.add_item_to_current(identifier, module.clone());
        } else {
            self.write_op_u8(ByteCode::ConstantByte, module_idx as u8);
        }

        self.container_ctx = last_ctx;
        Ok(())
    }

    fn visit_struct_def(
        &mut self,
        item: &Box<Ast>,
        struct_def: &dingleberry_front::ast_inner::StructDef,
    ) -> Result<(), SpruceErr> {
        let dingleberry_front::ast_inner::StructDef {
            is_static,
            init_fields,
            declarations,
        } = struct_def;

        let last_ctx = self.container_ctx;
        self.container_ctx = ContainerContext::Struct;

        let struct_identifier = item.token.lexeme.as_ref().unwrap().get_slice().to_string();
        self.open_struct(*is_static, struct_identifier.clone());

        for item in declarations {
            self.visit(item)?;

            match &item.data {
                AstData::FieldVarDeclarations(decls) if *is_static => {
                    for decl in decls {
                        let AstData::FieldVarDeclaration = decl.data else { unreachable!() };

                        let file_path = Self::get_filepath(&decl.token);
                        self.error(SpruceErr::new(
                            format!(
                            "Static struct '{struct_identifier}' cannot contain let bindings '{}'",
                            decl.token.lexeme.as_ref().unwrap().get_slice()
                        ),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: decl.token.line,
                                column: decl.token.column,
                            },
                        ));
                    }
                }

                AstData::Function(func) if *is_static => {
                    if !func.is_static {
                        let file_path = Self::get_filepath(&item.token);
                        self.error(SpruceErr::new(
                            format!("Static struct '{struct_identifier}' cannot contain non-static function '{}'", item.token.lexeme.as_ref().unwrap().get_slice()),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: item.token.line,
                                column: item.token.column,
                            },
                        ));
                    }
                }

                AstData::StructDef(struct_def) if *is_static => {
                    if !struct_def.is_static {
                        let file_path = Self::get_filepath(&item.token);
                        self.error(SpruceErr::new(
                            format!("Static struct '{struct_identifier}' cannot contain non-static struct '{}'", item.token.lexeme.as_ref().unwrap().get_slice()),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: item.token.line,
                                column: item.token.column,
                            },
                        ));
                    }
                }

                AstData::ClassDef(class_def) if *is_static => {
                    if !class_def.is_static {
                        let file_path = Self::get_filepath(&item.token);
                        self.error(SpruceErr::new(
                            format!("Static class '{struct_identifier}' cannot contain non-static class '{}'", item.token.lexeme.as_ref().unwrap().get_slice()),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: item.token.line,
                                column: item.token.column,
                            },
                        ));
                    }
                }

                _ => {}
            }
        }

        if let Some(init_fields) = init_fields {
            let mut current_field_names = HashSet::new();
            let mut current_fields = Vec::new();

            for field in init_fields {
                let identifier = get_identifier_or_string(field);
                let file_path = Self::get_filepath(&field);

                current_fields.push(identifier.clone());

                if !current_field_names.insert(identifier.clone()) {
                    self.error(SpruceErr::new(
                        format!("Struct '{struct_identifier}' already contains init field '{identifier}'"),
                        SpruceErrData::Compiler {
                            file_path: file_path.clone(),
                            line: field.line,
                            column: field.column,
                        },
                    ));
                }

                if !self.get_current_struct().items.contains_key(&identifier) {
                    self.error(SpruceErr::new(
                        format!("Struct '{struct_identifier}' does not contain field '{identifier}' to initialise"),
                        SpruceErrData::Compiler {
                            file_path,
                            line: field.line,
                            column: field.column,
                        },
                    ));
                }
            }

            self.get_current_struct_mut().init_items =
                Some(current_fields.into_iter().map(|s| s.to_string()).collect());
        }

        let struct_idx = self.close_struct();
        _ = self
            .add_symbol(&item.token, false, false)
            .map_err(|e| self.error(e));

        match last_ctx {
            ContainerContext::Module | ContainerContext::Struct | ContainerContext::Class => {
                let struct_def = &self.vm.constants[struct_idx as usize];
                self.add_item_to_current(struct_identifier, struct_def.clone());
            }

            _ => self.write_op_u8(ByteCode::ConstantByte, struct_idx as u8),
        }

        self.container_ctx = last_ctx;
        Ok(())
    }

    fn visit_class_def(
        &mut self,
        item: &Box<Ast>,
        class_def: &dingleberry_front::ast_inner::ClassDef,
    ) -> Result<(), SpruceErr> {
        let dingleberry_front::ast_inner::ClassDef {
            is_static,
            init_fields,
            super_class,
            declarations,
        } = class_def;

        let last_ctx = self.container_ctx;
        self.container_ctx = ContainerContext::Class;

        let class_identifier = item.token.lexeme.as_ref().unwrap().get_slice().to_string();
        self.open_class(*is_static, class_identifier.clone());

        for item in declarations {
            self.visit(item)?;

            match &item.data {
                AstData::Function(func) if *is_static => {
                    if !func.is_static {
                        let file_path = Self::get_filepath(&item.token);
                        self.error(SpruceErr::new(
                            format!("Static class '{class_identifier}' cannot contain non-static function '{}'", item.token.lexeme.as_ref().unwrap().get_slice()),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: item.token.line,
                                column: item.token.column,
                            },
                        ));
                    }
                }

                AstData::StructDef(struct_def) if *is_static => {
                    if !struct_def.is_static {
                        let file_path = Self::get_filepath(&item.token);
                        self.error(SpruceErr::new(
                            format!("Static class '{class_identifier}' cannot contain non-static struct '{}'", item.token.lexeme.as_ref().unwrap().get_slice()),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: item.token.line,
                                column: item.token.column,
                            },
                        ));
                    }
                }

                AstData::ClassDef(class_def) if *is_static => {
                    if !class_def.is_static {
                        let file_path = Self::get_filepath(&item.token);
                        self.error(SpruceErr::new(
                            format!("Static class '{class_identifier}' cannot contain non-static class '{}'", item.token.lexeme.as_ref().unwrap().get_slice()),
                            SpruceErrData::Compiler {
                                file_path: file_path.clone(),
                                line: item.token.line,
                                column: item.token.column,
                            },
                        ));
                    }
                }

                _ => {}
            }
        }

        if let Some(init_fields) = init_fields {
            let mut current_field_names = HashSet::new();
            let mut current_fields = Vec::new();

            for field in init_fields {
                let identifier = get_identifier_or_string(field);
                let file_path = Self::get_filepath(&field);

                current_fields.push(identifier.clone());

                if !current_field_names.insert(identifier.clone()) {
                    self.error(SpruceErr::new(
                        format!(
                            "Class '{class_identifier}' already contains init field '{identifier}'"
                        ),
                        SpruceErrData::Compiler {
                            file_path: file_path.clone(),
                            line: field.line,
                            column: field.column,
                        },
                    ));
                }
            }

            self.get_current_class_mut().init_items =
                Some(current_fields.into_iter().map(|s| s.to_string()).collect());
        }

        let class_idx = self.close_class(super_class)?;
        _ = self
            .add_symbol(&item.token, false, false)
            .map_err(|e| self.error(e));

        match last_ctx {
            ContainerContext::Module | ContainerContext::Struct | ContainerContext::Class => {
                let class_def = &self.vm.constants[class_idx as usize];
                self.add_item_to_current(class_identifier, class_def.clone());
            }

            _ => self.write_op_u8(ByteCode::ConstantByte, class_idx as u8),
        }

        self.container_ctx = last_ctx;
        Ok(())
    }

    fn visit_this(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        if self.container_ctx == ContainerContext::None {
            let file_path = Self::get_filepath(&item.token);

            self.error(SpruceErr::new(
                "Cannot use 'this' outside of modules".into(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.write(ByteCode::This);

        Ok(())
    }
}
