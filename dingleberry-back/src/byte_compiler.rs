use std::{collections::HashMap, hash::Hash, rc::Rc};

use dingleberry_front::{
    ast::{Ast, AstData},
    ast_inner::{
        BinaryOp, ForStatement, FunctionCall, IndexGetter, IndexSetter, LogicalOp, PropertyGetter,
        PropertySetter, VarAssign, VarDeclaration,
    },
    token::{Token, TokenKind},
};
use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{
    bytecode::ByteCode,
    object::{Module, ObjectData, StructDef},
    value::Value,
    vm::VM,
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Context {
    None,
    Function,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ContainerContext {
    None,
    Module,
    Struct,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: Option<String>,
    pub arg_count: u8,
    pub code: Vec<ByteCode>,

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
    fn new(identifier: Option<String>, arg_count: u8) -> Rc<Self> {
        Rc::new(Self {
            identifier,
            arg_count,
            code: Vec::new(),
            receiver: None,
        })
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    pub identifier: String,
    pub mutable: bool,
    pub index: u16,
    pub depth: u16,
}

struct SymbolTable {
    depth: u16,
    in_depth: Vec<u16>,
    scope: Vec<Symbol>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            depth: 0,
            in_depth: vec![0],
            scope: Vec::new(),
        }
    }

    #[inline]
    fn is_global(&self) -> bool {
        self.depth == 0
    }

    #[inline]
    fn new_func(&mut self) {
        self.depth += 1;
        self.in_depth.push(0);
    }

    #[inline]
    fn close_func(&mut self) {
        self.depth -= 1;
        self.in_depth.pop();
        self.scope.retain(|sym| sym.depth <= self.depth);
    }

    #[inline]
    fn new_scope(&mut self) {
        self.depth += 1;
    }

    #[inline]
    fn close_scope(&mut self) {
        self.depth -= 1;
    }

    fn find_local_symbol(&self, identifier: &str) -> Option<&Symbol> {
        for sym in self.scope.iter().rev() {
            if identifier == sym.identifier {
                return Some(sym);
            }
        }

        None
    }

    fn find_symbol_any(&self, identifier: &str) -> Option<Symbol> {
        for (index, sym) in self.scope.iter().rev().enumerate() {
            if sym.identifier == identifier {
                return Some(sym.clone());
            }
        }

        None
    }

    #[inline]
    fn add_global_symbol(&mut self, identifier: String, is_mutable: bool, str_idx: u16) {
        self.scope.push(Symbol {
            identifier,
            mutable: is_mutable,
            index: str_idx,
            depth: 0,
        });
        *self.in_depth.last_mut().unwrap() += 1;
    }

    #[inline]
    fn inject_symbol(&mut self, identifier: String, is_mutable: bool) {
        self.scope.push(Symbol {
            identifier,
            mutable: is_mutable,
            index: *self.in_depth.last().unwrap(),
            depth: self.depth,
        });
        *self.in_depth.last_mut().unwrap() += 1;
    }

    fn add_symbol(
        &mut self,
        token: &Token,
        is_mutable: bool,
        allow_override: bool,
    ) -> Result<(), SpruceErr> {
        let identifier = token.clone().lexeme.unwrap().get_slice().to_string();

        if !allow_override && self.find_local_symbol(&identifier).is_some() {
            let file_path = token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                format!("Symbol with name '{identifier}' already exists in scope"),
                SpruceErrData::Compiler {
                    file_path,
                    line: token.line,
                    column: token.column,
                },
            ));
        }

        if *self.in_depth.last().unwrap() == u16::MAX {
            let file_path = token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                format!("Too many symbols in current scope"),
                SpruceErrData::Compiler {
                    file_path,
                    line: token.line,
                    column: token.column,
                },
            ));
        }

        self.scope.push(Symbol {
            identifier,
            mutable: is_mutable,
            index: *self.in_depth.last().unwrap(),
            depth: self.depth,
        });
        *self.in_depth.last_mut().unwrap() += 1;

        Ok(())
    }
}

pub struct ByteCompiler<'a> {
    current_func: Option<Rc<Function>>,
    current_mod: Option<Module>,
    current_struct: Option<StructDef>,
    symbol_table: SymbolTable,
    ctx: Context,
    container_ctx: ContainerContext,
    vm: &'a mut VM,
}

impl<'a> ByteCompiler<'a> {
    pub fn new(vm: &'a mut VM) -> Self {
        Self {
            current_func: Some(Function::new(Some("script".into()), 0)),
            current_mod: None,
            current_struct: None,
            symbol_table: SymbolTable::new(),
            ctx: Context::None,
            container_ctx: ContainerContext::None,
            vm,
        }
    }

    pub fn compile(mut self, root: Box<Ast>) -> Result<Value, SpruceErr> {
        self.visit(&root)?;

        let ByteCompiler { current_func, .. } = self;
        let mut current_func = current_func.unwrap();
        Rc::get_mut(&mut current_func)
            .unwrap()
            .code
            .extend([ByteCode::None, ByteCode::Return]);

        let func = self
            .vm
            .allocate(ObjectData::Function(Rc::clone(&current_func)));

        Ok(Value::Object(func))
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

    #[inline]
    fn open_module(&mut self, identifier: String) {
        self.symbol_table.new_func();

        self.current_mod = Some(Module {
            identifier,
            items: HashMap::new(),
        });
    }

    fn close_module(&mut self, last_mod: Option<Module>) -> u16 {
        self.symbol_table.close_func();

        let module = self.current_mod.take().unwrap();
        let identifier = module.identifier.clone();
        self.current_mod = last_mod;

        let module = self.vm.allocate(ObjectData::Module(Rc::new(module)));
        self.vm.constants.push(Value::Object(module.clone()));

        if self.current_mod.is_none() {
            self.vm.globals.insert(identifier, Value::Object(module));
        }

        (self.vm.constants.len() - 1) as u16
    }

    #[inline]
    fn open_struct(&mut self, identifier: String) {
        self.symbol_table.new_func();

        self.current_struct = Some(StructDef {
            identifier,
            items: HashMap::new(),
        });
    }

    fn close_struct(&mut self, last_struct: Option<StructDef>) -> u16 {
        self.symbol_table.close_func();

        let struct_ = self.current_struct.take().unwrap();
        let identifier = struct_.identifier.clone();
        self.current_struct = last_struct;

        let struct_ = self.vm.allocate(ObjectData::StructDef(Rc::new(struct_)));
        self.vm.constants.push(Value::Object(struct_.clone()));

        if self.current_struct.is_none() {
            self.vm.globals.insert(identifier, Value::Object(struct_));
        }

        (self.vm.constants.len() - 1) as u16
    }

    #[inline]
    fn add_item_to_module(&mut self, identifier: String, value: Value) {
        let module = self.current_mod.as_mut().unwrap();
        module.items.insert(identifier, value);
    }

    #[inline]
    fn add_item_to_struct(&mut self, identifier: String, value: Value) {
        let struct_ = self.current_struct.as_mut().unwrap();
        struct_.items.insert(identifier, value);
    }

    #[inline]
    fn open_function(&mut self, identifier: Option<String>, arg_count: u8) {
        self.current_func = Some(Function::new(identifier, arg_count));
        self.symbol_table.new_func();
    }

    fn close_function(&mut self, anonymous: bool, last_fn: Option<Rc<Function>>) -> Option<u16> {
        self.symbol_table.close_func();
        self.func().code.extend([ByteCode::None, ByteCode::Return]);

        let func = self.current_func.take().unwrap();
        self.current_func = last_fn;

        if anonymous || self.current_mod.is_some() || self.current_struct.is_some() {
            let obj = self.vm.allocate(ObjectData::Function(func));
            self.vm.constants.push(Value::Object(obj));
            Some((self.vm.constants.len() - 1) as u16)
        } else {
            let identifier = func.identifier.as_ref().unwrap().clone();
            let obj = self.vm.allocate(ObjectData::Function(func));
            self.vm.globals.insert(identifier, Value::Object(obj));
            None
        }
    }

    #[inline]
    fn func(&mut self) -> &mut Function {
        Rc::get_mut(self.current_func.as_mut().unwrap()).unwrap()
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

            self.symbol_table
                .add_global_symbol(slice.to_string(), is_mutable, index);
        } else {
            self.symbol_table
                .add_symbol(token, is_mutable, allow_override)?;
        }

        Ok(())
    }

    fn visit(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        match &item.data {
            &AstData::Program(ref statements) => self.visit_program(statements),
            &AstData::Empty => self.visit_empty(item),
            &AstData::VarDeclarations(ref decls) => self.visit_var_declarations(decls),
            &AstData::VarDeclaration(ref decl) => self.visit_var_declaration(item, decl),
            &AstData::VarAssign(ref assign) => self.visit_var_assign(item, assign),
            &AstData::Function(ref func) => self.visit_function(item, func),
            &AstData::BinaryOp(ref bin) => self.visit_binary_op(item, bin),
            &AstData::UnaryOp(ref rhs) => self.visit_unary_op(rhs),
            &AstData::LogicalOp(ref logic) => self.visit_logical_op(item, logic),
            &AstData::FunctionCall(ref fncall) => self.visit_function_call(item, fncall),
            &AstData::ForStatement(ref fstmt) => self.visit_for_statement(fstmt),
            &AstData::IndexGetter(ref getter) => self.visit_index_getter(getter),
            &AstData::IndexSetter(ref setter) => self.visit_index_setter(setter),
            &AstData::PropertyGetter(ref getter) => self.visit_property_getter(getter),
            &AstData::PropertySetter(ref setter) => self.visit_property_setter(setter),
            &AstData::Body(ref statements) => self.visit_body(statements, true),
            &AstData::This => self.visit_this(item),
            &AstData::Return(ref expr) => self.visit_return_statement(item, expr),
            &AstData::Module(ref items) => self.visit_module(item, items),
            &AstData::StructDef(ref items) => self.visit_struct_def(item, items),
            &AstData::Identifier => self.visit_identifier(item),
            &AstData::Literal => self.visit_literal(item),
            &AstData::ArrayLiteral(_) => self.visit_array_literal(item),
            &AstData::TupleLiteral(_) => self.visit_tuple_literal(item),
            &AstData::ExpressionStatement(_, _) => self.visit_expression_statement(item),
            n => todo!("Visit {n:?}"),
        }
    }

    fn visit_identifier(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let identifier = item.token.lexeme.as_ref().unwrap().get_slice();
        let maybe_values = self.symbol_table.find_symbol_any(identifier);

        match maybe_values {
            Some(Symbol { index, depth, .. }) => {
                if depth == 0 {
                    self.func().code.push(ByteCode::GetGlobal(index));
                } else {
                    self.func().code.push(ByteCode::GetLocal(index as u8));
                }
            }
            None => {
                let index = self.get_string_or_insert(identifier.to_string());
                self.func().code.push(ByteCode::GetGlobal(index));
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
            self.func().code.push(ByteCode::ConstantByte(index as u8));
        } else {
            let index = self.add_constant(value);
            self.func().code.push(ByteCode::ConstantByte(index as u8));
        }

        Ok(())
    }

    fn visit_tuple_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::TupleLiteral(values) = &item.data else { unreachable!() };

        for value in values {
            self.visit(value)?;
        }

        if values.len() > u16::MAX as usize {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                "More than 65535 values in tuple literal".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.func()
            .code
            .push(ByteCode::IntoTuple(values.len() as u16));

        Ok(())
    }

    fn visit_array_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ArrayLiteral(values) = &item.data else { unreachable!() };

        for value in values {
            self.visit(value)?;
        }

        if values.len() > u16::MAX as usize {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                "More than 65535 values in array literal".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.func()
            .code
            .push(ByteCode::IntoList(values.len() as u16));

        Ok(())
    }

    fn visit_expression_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ExpressionStatement(_, expr) = &item.data else { unreachable!() };

        self.visit(expr)?;

        match expr.data {
            AstData::Body(_) => {}
            _ => self.func().code.push(ByteCode::Pop),
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, item: &Box<Ast>, binary: &BinaryOp) -> Result<(), SpruceErr> {
        let BinaryOp { lhs, rhs } = &binary;

        self.visit(lhs)?;
        self.visit(rhs)?;

        self.func().code.push(match item.token.kind {
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
        self.func().code.push(ByteCode::Negate);

        Ok(())
    }

    fn visit_logical_op(&mut self, item: &Box<Ast>, logic: &LogicalOp) -> Result<(), SpruceErr> {
        // FIXME: This is inefficient for the case of OR
        self.visit(&logic.lhs)?;
        self.visit(&logic.rhs)?;

        self.func().code.push(match item.token.kind {
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

    fn visit_parameter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        self.add_symbol(&item.token, false, false)
    }

    fn visit_function(
        &mut self,
        item: &Box<Ast>,
        func: &dingleberry_front::ast_inner::Function,
    ) -> Result<(), SpruceErr> {
        let dingleberry_front::ast_inner::Function {
            anonymous,
            parameters,
            body,
        } = &func;

        let last_ctx = self.ctx;
        self.ctx = Context::Function;

        let last_function = self.current_func.take();

        let identifier = if item.token.lexeme.is_some() {
            Some(item.token.lexeme.as_ref().unwrap().get_slice().to_string())
        } else {
            None
        };

        self.add_symbol(&item.token, false, false)?;

        self.open_function(
            identifier.clone(),
            parameters
                .as_ref()
                .map(|p| p.len() as u8)
                .unwrap_or_default(),
        );

        if let Some(parameters) = parameters {
            for param in parameters {
                self.visit_parameter(param)?;
            }
        }

        if self.container_ctx != ContainerContext::None {
            self.inject_symbol("self", false);
        }

        match &body.data {
            AstData::Body(statements) => self.visit_body(statements, false)?,
            AstData::Return(expr) => self.visit_return_statement(body, expr)?,
            _ => unreachable!(),
        }

        if let Some(index) = self.close_function(*anonymous, last_function) {
            if !anonymous {
                let func = &self.vm.constants[index as usize];
                if self.container_ctx == ContainerContext::Module {
                    self.add_item_to_module(identifier.unwrap().clone(), func.clone());
                } else if self.container_ctx == ContainerContext::Struct {
                    self.add_item_to_struct(identifier.unwrap().clone(), func.clone());
                }
            } else if *anonymous {
                self.func().code.push(ByteCode::ConstantByte(index as u8));
            }
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

        if arguments.len() > u8::MAX as usize {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                "More than 256 arguments in function call".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.func().code.push(ByteCode::Call(arguments.len() as u8));

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

            self.func().code.push(ByteCode::DefineGlobal(index));

            self.symbol_table
                .add_global_symbol(slice.to_string(), *is_mutable, index);
        } else if self.symbol_table.find_local_symbol(slice).is_some() {
            // Add new symbol
            self.symbol_table
                .add_symbol(&item.token, *is_mutable, true)?;
        } else {
            self.symbol_table
                .add_symbol(&item.token, *is_mutable, false)?;
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

    fn visit_var_assign(&mut self, item: &Box<Ast>, assign: &VarAssign) -> Result<(), SpruceErr> {
        let VarAssign { lhs, expression } = &assign;
        self.visit(expression)?;

        let identifier = lhs.token.lexeme.as_ref().unwrap().get_slice().to_string();
        if let Some(Symbol {
            identifier,
            mutable,
            index,
            depth,
        }) = self.symbol_table.find_symbol_any(&identifier)
        {
            if !mutable {
                let file_path = item
                    .token
                    .lexeme
                    .as_ref()
                    .map(|span| (*span.source.file_path).clone());

                return Err(SpruceErr::new(
                    format!("Cannot mutate immutable identifier '{identifier}'"),
                    SpruceErrData::Compiler {
                        file_path,
                        line: item.token.line,
                        column: item.token.column,
                    },
                ));
            }

            self.func().code.push(if depth == 1 {
                ByteCode::SetGlobal(index)
            } else {
                ByteCode::SetLocal(index as u8)
            });
        } else {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
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

    fn visit_var_assign_equal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_if_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
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
            self.func().code.push(ByteCode::Jump(here));
        } else {
            todo!()
        }

        Ok(())
    }

    fn visit_index_getter(&mut self, getter: &IndexGetter) -> Result<(), SpruceErr> {
        let IndexGetter { expression, index } = &getter;

        self.visit(expression)?;
        self.visit(index)?;

        self.func().code.push(ByteCode::IndexGet);

        Ok(())
    }

    fn visit_index_setter(&mut self, setter: &IndexSetter) -> Result<(), SpruceErr> {
        let IndexSetter { expression, rhs } = &setter;
        let AstData::IndexGetter(IndexGetter { expression, index }) = &expression.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(index)?;
        self.visit(rhs)?;

        self.func().code.push(ByteCode::IndexSet);

        Ok(())
    }

    fn visit_property_getter(&mut self, getter: &PropertyGetter) -> Result<(), SpruceErr> {
        let PropertyGetter { lhs, property } = &getter;

        self.visit(lhs)?;

        let identifier = property
            .token
            .lexeme
            .as_ref()
            .unwrap()
            .get_slice()
            .to_string();
        let identifier = self.get_string_or_insert(identifier);
        self.func().code.push(ByteCode::PropertyGet(identifier));

        Ok(())
    }

    fn visit_property_setter(&mut self, setter: &PropertySetter) -> Result<(), SpruceErr> {
        let PropertySetter { lhs, expression } = &setter;
        let AstData::PropertyGetter(PropertyGetter { lhs, property }) = &lhs.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(lhs)?;

        let identifier = property
            .token
            .lexeme
            .as_ref()
            .unwrap()
            .get_slice()
            .to_string();

        let identifier = self.get_string_or_insert(identifier);
        self.func().code.push(ByteCode::PropertySet(identifier));

        Ok(())
    }

    fn visit_switch_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_switch_case(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_return_statement(
        &mut self,
        item: &Box<Ast>,
        expr: &Option<Box<Ast>>,
    ) -> Result<(), SpruceErr> {
        if let Some(expr) = expr {
            self.visit(expr)?;
        }

        self.func().code.push(ByteCode::Return);

        if self.ctx != Context::Function {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
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

        for stmt in statements {
            self.visit(stmt)?;
        }

        let locals = *self.symbol_table.in_depth.last().unwrap() as u8;
        if locals > 0 {
            self.func().code.push(if locals > 1 {
                ByteCode::PopN(locals)
            } else {
                ByteCode::Pop
            });
        }

        if new_scope {
            self.symbol_table.close_scope();
        }

        Ok(())
    }

    fn visit_include(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_program(&mut self, statements: &Vec<Box<Ast>>) -> Result<(), SpruceErr> {
        for stmt in statements {
            self.visit(stmt)?;
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
        let last_mod = self.current_mod.take();

        self.open_module(identifier.clone());

        for item in items {
            self.visit(item)?;
        }

        let module_idx = self.close_module(last_mod);

        if self.current_mod.is_some() {
            let module = &self.vm.constants[module_idx as usize];
            self.add_item_to_module(identifier, module.clone());
        } else {
            self.func()
                .code
                .push(ByteCode::ConstantByte(module_idx as u8));
        }

        self.container_ctx = last_ctx;
        Ok(())
    }

    fn visit_struct_def(
        &mut self,
        item: &Box<Ast>,
        items: &Vec<Box<Ast>>,
    ) -> Result<(), SpruceErr> {
        let last_ctx = self.container_ctx;
        self.container_ctx = ContainerContext::Struct;

        let identifier = item.token.lexeme.as_ref().unwrap().get_slice().to_string();
        let last_struct = self.current_struct.take();

        self.open_struct(identifier.clone());

        for item in items {
            self.visit(item)?;
        }

        let struct_idx = self.close_struct(last_struct);

        match last_ctx {
            ContainerContext::Module => {
                let struct_ = &self.vm.constants[struct_idx as usize];
                self.add_item_to_module(identifier, struct_.clone());
            }

            ContainerContext::Struct => {
                let struct_ = &self.vm.constants[struct_idx as usize];
                self.add_item_to_struct(identifier, struct_.clone());
            }

            _ => {
                self.func()
                    .code
                    .push(ByteCode::ConstantByte(struct_idx as u8));
            }
        }

        self.container_ctx = last_ctx;
        Ok(())
    }

    fn visit_this(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        if self.container_ctx == ContainerContext::None {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                "Cannot use 'this' outside of modules".into(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.func().code.push(ByteCode::This);

        Ok(())
    }
}
