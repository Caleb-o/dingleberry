use std::{collections::HashMap, rc::Rc};

use dingleberry_front::{
    ast::{Ast, AstData},
    source::Source,
    token::TokenKind,
};
use dingleberry_shared::{
    error::{SpruceErr, SpruceErrData},
    visitor::Visitor,
};

use crate::{bytecode::ByteCode, object::ObjectData, value::Value, vm::VM};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Context {
    None,
    Function,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: Option<String>,
    pub arg_count: u8,
    pub code: Vec<ByteCode>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.identifier == other.identifier && self.arg_count == other.arg_count
    }
}

impl Function {
    fn new(identifier: Option<String>, arg_count: u8) -> Rc<Self> {
        Rc::new(Self {
            identifier,
            arg_count,
            code: Vec::new(),
        })
    }
}

struct SymbolTable {
    scope: Vec<HashMap<String, (bool, u8)>>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scope: vec![HashMap::new()],
        }
    }

    #[inline]
    fn is_global(&self) -> bool {
        self.scope.len() == 1
    }

    #[inline]
    fn new_scope(&mut self) {
        self.scope.push(HashMap::new());
    }

    #[inline]
    fn current_locals(&self) -> u8 {
        self.scope.last().unwrap().len() as u8
    }

    #[inline]
    fn close_scope(&mut self) {
        _ = self.scope.pop();
    }

    fn find_local_symbol(&self, identifier: &str) -> Option<(bool, u8)> {
        let top_scope = self.scope.last().unwrap();
        for (id, value) in top_scope {
            if *id == identifier {
                return Some(*value);
            }
        }

        None
    }

    fn find_local_symbol_mut_ref(&mut self, identifier: &str) -> Option<&mut (bool, u8)> {
        let top_scope = self.scope.last_mut().unwrap();
        for (id, value) in top_scope {
            if *id == identifier {
                return Some(value);
            }
        }

        None
    }

    fn find_symbol_any(&self, identifier: &str) -> Option<(bool, bool, u8)> {
        for (index, scope) in self.scope.iter().rev().enumerate() {
            for (id, value) in scope {
                if *id == identifier {
                    return Some((self.scope.len() - index == 1, value.0, value.1));
                }
            }
        }

        None
    }

    fn add_symbol(&mut self, identifier: String, is_mutable: bool) -> Result<u8, SpruceErr> {
        let top_scope = self.scope.last_mut().unwrap();

        if top_scope.contains_key(&identifier) {
            return Err(SpruceErr::new(
                format!("Symbol with name '{identifier}' already exists in scope"),
                SpruceErrData::Analyser,
            ));
        }

        if top_scope.len() >= u8::MAX as usize {
            return Err(SpruceErr::new(
                format!("Too many symbols in current scope"),
                SpruceErrData::Analyser,
            ));
        }

        top_scope.insert(identifier, (is_mutable, top_scope.len() as u8));
        Ok((top_scope.len() - 1) as u8)
    }

    fn add_global_symbol(
        &mut self,
        index: u8,
        identifier: String,
        is_mutable: bool,
    ) -> Result<(), SpruceErr> {
        let top_scope = self.scope.last_mut().unwrap();
        top_scope.insert(identifier, (is_mutable, index));
        Ok(())
    }
}

pub struct ByteCompiler<'a> {
    current_func: Option<Rc<Function>>,
    symbol_table: SymbolTable,
    ctx: Context,
    vm: &'a mut VM,
}

impl<'a> ByteCompiler<'a> {
    pub fn new(vm: &'a mut VM) -> Self {
        Self {
            current_func: Some(Function::new(Some("script".into()), 0)),
            symbol_table: SymbolTable::new(),
            ctx: Context::None,
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

    fn add_constant(&mut self, value: Value) -> u8 {
        self.vm.constants.push(value);
        (self.vm.constants.len() - 1) as u8
    }

    fn find_constant(&self, value: &Value) -> Option<u8> {
        for (idx, v) in self.vm.constants.iter().enumerate() {
            if *v == *value {
                return Some(idx as u8);
            }
        }

        None
    }

    fn get_string_or_insert(&mut self, identifier: String) -> u8 {
        let str = self.vm.allocate_string(identifier, true);
        let str = Value::Object(str);

        if let Some(index) = self.find_constant(&str) {
            index
        } else {
            self.add_constant(str)
        }
    }

    #[inline]
    fn open_function(&mut self, identifier: Option<String>, arg_count: u8) {
        self.current_func = Some(Function::new(identifier, arg_count));
        self.symbol_table.new_scope();
    }

    fn close_function(&mut self, anonymous: bool, last_fn: Option<Rc<Function>>) -> Option<u8> {
        self.symbol_table.close_scope();

        self.func().code.extend([ByteCode::None, ByteCode::Return]);

        let func = self.current_func.take().unwrap();
        self.current_func = last_fn;

        if anonymous {
            let obj = self.vm.allocate(ObjectData::Function(func));
            self.vm.constants.push(Value::Object(obj));
            Some((self.vm.constants.len() - 1) as u8)
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
}

impl<'a> Visitor<Box<Ast>, ()> for ByteCompiler<'a> {
    fn visit(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        match &item.data {
            &AstData::Program(_) => self.visit_program(item),
            &AstData::Empty => self.visit_empty(item),
            &AstData::VarDeclarations(_) => self.visit_var_declarations(item),
            &AstData::VarDeclaration { .. } => self.visit_var_declaration(item),
            &AstData::Function { .. } => self.visit_function(item),
            &AstData::BinaryOp { .. } => self.visit_binary_op(item),
            &AstData::UnaryOp { .. } => self.visit_unary_op(item),
            &AstData::FunctionCall { .. } => self.visit_function_call(item),
            &AstData::ForStatement { .. } => self.visit_for_statement(item),
            &AstData::IndexGetter { .. } => self.visit_index_getter(item),
            &AstData::IndexSetter { .. } => self.visit_index_setter(item),
            &AstData::Body(_) => self.visit_body(item, true),
            &AstData::Return(_) => self.visit_return_statement(item),
            &AstData::Identifier => self.visit_identifier(item),
            &AstData::Literal => self.visit_literal(item),
            &AstData::ArrayLiteral(_) => self.visit_array_literal(item),
            &AstData::ExpressionStatement(_, _) => self.visit_expression_statement(item),
            n => todo!("Visit {n:?}"),
        }
    }

    fn visit_identifier(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Identifier = &item.data else { unreachable!() };

        let identifier = item.token.lexeme.as_ref().unwrap().get_slice();
        let maybe_values = self.symbol_table.find_symbol_any(identifier);

        match maybe_values {
            Some((is_global, _, index)) if is_global => {
                self.func().code.push(ByteCode::GetGlobal(index));
            }
            Some((_, _, index)) => {
                self.func().code.push(ByteCode::GetLocal(index));
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
            self.func().code.push(ByteCode::ConstantByte(index));
        } else {
            let index = self.add_constant(value);
            self.func().code.push(ByteCode::ConstantByte(index));
        }

        Ok(())
    }

    fn visit_struct_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_tuple_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_array_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ArrayLiteral(values) = &item.data else { unreachable!() };

        for value in values {
            self.visit(value)?;
        }

        if values.len() > u8::MAX as usize {
            let file_path = item
                .token
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                "More than 256 values in array literal".to_string(),
                SpruceErrData::Compiler {
                    file_path,
                    line: item.token.line,
                    column: item.token.column,
                },
            ));
        }

        self.func()
            .code
            .push(ByteCode::IntoList(values.len() as u8));

        Ok(())
    }

    fn visit_expression_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ExpressionStatement(_, expr) = &item.data else { unreachable!() };

        self.visit(expr)?;
        self.func().code.push(ByteCode::Pop);

        Ok(())
    }

    fn visit_binary_op(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &item.data else { unreachable!() } ;

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

    fn visit_unary_op(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_logical_op(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_parameter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Parameter = &item.data else { unreachable!() };

        let param = item.token.lexeme.as_ref().unwrap().get_slice().to_string();
        _ = self.symbol_table.add_symbol(param, false)?;

        Ok(())
    }

    fn visit_function(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Function { anonymous, parameters, body } = &item.data else { unreachable!() };

        let last_ctx = self.ctx;
        self.ctx = Context::Function;

        let last_function = self.current_func.take();

        let identifier = if item.token.lexeme.is_some() {
            Some(item.token.lexeme.as_ref().unwrap().get_slice().to_string())
        } else {
            None
        };
        self.open_function(
            identifier,
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

        self.visit(body)?;

        if let Some(index) = self.close_function(*anonymous, last_function) {
            self.func().code.push(ByteCode::ConstantByte(index));
        }

        self.ctx = last_ctx;
        Ok(())
    }

    fn visit_function_call(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::FunctionCall { lhs, arguments } = &item.data else { unreachable!() };

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

    fn visit_var_declaration(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclaration { is_mutable, expression } = &item.data else { unreachable!() };

        if let Some(expression) = expression {
            self.visit(expression)?;
        }

        let slice = item.token.lexeme.as_ref().unwrap().get_slice();
        if let Some(sym) = self.symbol_table.find_local_symbol_mut_ref(slice) {
            // Update mutability
            sym.0 = *is_mutable;
        } else if self.symbol_table.is_global() {
            let str = self.vm.allocate_string(slice.to_string(), true);
            let str = Value::Object(str);

            let index = if let Some(index) = self.find_constant(&str) {
                index
            } else {
                self.add_constant(str)
            };

            self.func().code.push(ByteCode::DefineGlobal(index));

            _ = self
                .symbol_table
                .add_global_symbol(index, slice.to_string(), *is_mutable)?;
        } else {
            _ = self
                .symbol_table
                .add_symbol(slice.to_string(), *is_mutable)?;
        }

        Ok(())
    }

    fn visit_var_declarations(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclarations(decls) = &item.data else { unreachable!() };

        for item in decls {
            self.visit_var_declaration(item)?;
        }

        Ok(())
    }

    fn visit_var_assign(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_var_assign_equal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_if_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_for_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ForStatement { variable, expression, body } = &item.data else { unreachable!() };

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

    fn visit_index_getter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::IndexGetter { expression, index } = &item.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(index)?;

        self.func().code.push(ByteCode::IndexGet);

        Ok(())
    }

    fn visit_index_setter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::IndexSetter { expression, rhs } = &item.data else { unreachable!() };
        let AstData::IndexGetter { expression, index } = &expression.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(index)?;
        self.visit(rhs)?;

        self.func().code.push(ByteCode::IndexSet);

        Ok(())
    }

    fn visit_property_getter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_property_setter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_switch_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_switch_case(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_return_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Return(expr) = &item.data else { unreachable!() } ;

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

    fn visit_body(&mut self, item: &Box<Ast>, new_scope: bool) -> Result<(), SpruceErr> {
        let AstData::Body(statements) = &item.data else { unreachable!() };

        if new_scope {
            self.symbol_table.new_scope();
        }

        for stmt in statements {
            self.visit(stmt)?;
        }

        let locals = self.symbol_table.current_locals();
        self.func().code.push(ByteCode::PopN(locals));

        if new_scope {
            self.symbol_table.close_scope();
        }

        Ok(())
    }

    fn visit_include(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_program(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Program(statements) = &item.data else { unreachable!() };

        for stmt in statements {
            self.visit(stmt)?;
        }

        Ok(())
    }

    fn visit_empty(&mut self, _: &Box<Ast>) -> Result<(), SpruceErr> {
        Ok(())
    }
}
