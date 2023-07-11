use std::collections::HashMap;

use dingleberry_front::{
    ast::{Ast, AstData},
    token::TokenKind,
};
use dingleberry_shared::{
    error::{SpruceErr, SpruceErrData},
    visitor::Visitor,
};

use crate::{
    bytecode::ByteCode,
    gc::{ObjectData, Value},
    vm::VM,
};

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: String,
    pub arg_count: u8,
    pub code: Vec<ByteCode>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.identifier == other.identifier && self.arg_count == other.arg_count
    }
}

impl Function {
    fn new(identifier: String, arg_count: u8) -> Box<Self> {
        Box::new(Self {
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
    fn new_scope(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn find_local_symbol<'a>(&self, identifier: &'a str) -> Option<(bool, u8)> {
        let top_scope = self.scope.last().unwrap();
        for (id, value) in top_scope {
            if *id == identifier {
                return Some(*value);
            }
        }

        None
    }

    fn find_symbol_any<'a>(&self, identifier: &'a str) -> Option<(bool, bool, u8)> {
        for (index, scope) in self.scope.iter().rev().enumerate() {
            for (id, value) in scope {
                if *id == identifier {
                    return Some((self.scope.len() - index == 0, value.0, value.1));
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
                SpruceErrData::Compiler,
            ));
        }

        if top_scope.len() >= u8::MAX as usize {
            return Err(SpruceErr::new(
                format!("Too many symbols in current scope"),
                SpruceErrData::Compiler,
            ));
        }

        top_scope.insert(identifier, (is_mutable, top_scope.len() as u8));
        Ok((top_scope.len() - 1) as u8)
    }
}

pub struct ByteCompiler<'a> {
    current_func: Option<Box<Function>>,
    symbol_table: SymbolTable,
    vm: &'a mut VM,
}

impl<'a> ByteCompiler<'a> {
    pub fn new(vm: &'a mut VM) -> Self {
        Self {
            current_func: Some(Function::new("script".into(), 0)),
            symbol_table: SymbolTable::new(),
            vm,
        }
    }

    pub fn compile(mut self, root: Box<Ast>) -> Result<Box<Function>, SpruceErr> {
        self.visit(&root)?;
        let ByteCompiler { current_func, .. } = self;
        Ok(current_func.unwrap())
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

    fn find_global(&self, value: Value) -> Option<u8> {
        for (idx, v) in self.vm.globals.iter().enumerate() {
            if *v == value {
                return Some(idx as u8);
            }
        }

        None
    }

    fn close_function(&mut self) -> usize {
        let func = self.current_func.take().unwrap();
        let obj = self.vm.allocate(ObjectData::Function(func));
        self.vm.globals.push(Value::Object(obj));

        self.vm.globals.len()
    }

    #[inline]
    fn fn_code(&mut self) -> &mut Vec<ByteCode> {
        &mut self.current_func.as_mut().unwrap().code
    }
}

impl<'a> Visitor<Box<Ast>, ()> for ByteCompiler<'a> {
    fn visit(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        match &item.data {
            &AstData::Program(_) => self.visit_program(item),
            &AstData::Empty => self.visit_empty(item),
            _ => todo!("Visit"),
        }
    }

    fn visit_identifier(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Identifier = &item.data else { unreachable!() };
        let maybe_values = self
            .symbol_table
            .find_symbol_any(item.token.lexeme.as_ref().unwrap().get_slice());

        if let None = maybe_values {}

        let (is_global, _, index) = maybe_values.unwrap();

        ByteCode::get_value(is_global, index, self.fn_code());

        Ok(())
    }

    fn visit_literal(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Literal = &item.data else { unreachable!() };

        let value: Value = if item.token.kind == TokenKind::String {
            Value::Object(
                self.vm
                    .allocate_string(item.token.lexeme.as_ref().unwrap().get_slice().to_string()),
            )
        } else {
            item.token.clone().into()
        };

        if let Some(index) = self.find_constant(&value) {
            ByteCode::constant(index as u8, self.fn_code());
        } else {
            let index = self.add_constant(value);
            ByteCode::constant(index as u8, self.fn_code());
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
        todo!()
    }

    fn visit_expression_statement(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_binary_op(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &item.data else { unreachable!() } ;

        Ok(())
    }

    fn visit_unary_op(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_logical_op(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_parameter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_parameter_list(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_function(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_function_call(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_var_declaration(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclaration { is_mutable, expression } = &item.data else { unreachable!() };

        if let Some(expression) = expression {
            self.visit(expression)?;
        }

        let slice = item.token.lexeme.as_ref().unwrap().get_slice();
        if let Some(_) = self.symbol_table.find_local_symbol(slice) {
            return Err(SpruceErr::new(
                format!("Variable '{slice}' has previously been declared"),
                SpruceErrData::Compiler,
            ));
        }

        _ = self
            .symbol_table
            .add_symbol(slice.to_string(), *is_mutable)?;

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
        todo!()
    }

    fn visit_index_getter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_index_setter(&mut self, item: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
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
        todo!()
    }

    fn visit_body(&mut self, item: &Box<Ast>, new_scope: bool) -> Result<(), SpruceErr> {
        let AstData::Body(statements) = &item.data else { unreachable!() };

        for stmt in statements {
            self.visit(stmt)?;
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
