use std::rc::Rc;

use crate::ast_inner::{
    BinaryOp, ClassDef, ForStatement, Function, FunctionCall, IfStatement, Include, IndexGetter,
    IndexSetter, LogicalOp, PropertyGetter, PropertySetter, StructDef, SwitchCase, SwitchStatement,
    VarAssign, VarAssignEqual, VarDeclaration, WhileStatement,
};

use super::token::Token;

#[derive(Debug, Clone)]
pub struct Ast {
    pub token: Token,
    pub data: AstData,
}

#[derive(Debug, Clone)]
pub enum AstData {
    Identifier,
    Literal,
    SymbolLiteral,
    TupleLiteral(Vec<Box<Ast>>),
    ArrayLiteral(Vec<Box<Ast>>),
    ExpressionStatement(bool, Box<Ast>),

    BinaryOp(BinaryOp),
    UnaryOp(Box<Ast>),
    LogicalOp(LogicalOp),

    Parameter(bool),

    Function(Function),
    FunctionCall(FunctionCall),

    VarDeclaration(VarDeclaration),
    VarDeclarations(Vec<Box<Ast>>),

    VarAssign(VarAssign),
    VarAssignEqual(VarAssignEqual),

    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    LoopStatement(Box<Ast>),

    IndexGetter(IndexGetter),
    IndexSetter(IndexSetter),

    PropertyGetter(PropertyGetter),
    PropertySetter(PropertySetter),

    SwitchStatement(SwitchStatement),
    SwitchCase(SwitchCase),

    This,
    Super,

    Yield(Option<Box<Ast>>),
    Resume(Box<Ast>),

    TypeOf,

    Return(Option<Box<Ast>>),
    Body(Vec<Box<Ast>>),
    Include(Include),
    Module(Vec<Box<Ast>>),
    StructDef(StructDef),
    ClassDef(ClassDef),
    Program(Vec<Box<Ast>>),
    Empty,
}

impl Ast {
    pub fn new_empty(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Empty,
        })
    }

    pub fn new_include(token: Token, root: Rc<Box<Ast>>, module_name: Option<Token>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Include(Include { root, module_name }),
        })
    }

    pub fn new_module(token: Token, body: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Module(body),
        })
    }

    pub fn new_struct_def(
        token: Token,
        is_static: bool,
        init_fields: Option<Vec<Token>>,
        declarations: Vec<Box<Ast>>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::StructDef(StructDef {
                is_static,
                init_fields,
                declarations,
            }),
        })
    }

    pub fn new_class_def(
        token: Token,
        is_static: bool,
        init_fields: Option<Vec<Token>>,
        super_class: Option<Box<Ast>>,
        declarations: Vec<Box<Ast>>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::ClassDef(ClassDef {
                is_static,
                init_fields,
                super_class,
                declarations,
            }),
        })
    }

    pub fn new_program(token: Token, body: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Program(body),
        })
    }

    pub fn new_body(token: Token, statements: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Body(statements),
        })
    }

    pub fn new_literal(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Literal,
        })
    }

    pub fn new_symbol(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::SymbolLiteral,
        })
    }

    pub fn new_tuple_literal(token: Token, values: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::TupleLiteral(values),
        })
    }

    pub fn new_array_literal(token: Token, values: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::ArrayLiteral(values),
        })
    }

    pub fn new_identifier(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Identifier,
        })
    }

    pub fn new_var_decl(token: Token, is_mutable: bool, expression: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarDeclaration(VarDeclaration {
                is_mutable,
                expression,
            }),
        })
    }

    pub fn new_var_decls(decls: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token: decls[0].token.clone(),
            data: AstData::VarDeclarations(decls),
        })
    }

    pub fn new_var_assign(token: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarAssign(VarAssign { lhs, expression }),
        })
    }

    pub fn new_var_assign_equal(
        token: Token,
        operator: Token,
        lhs: Box<Ast>,
        expression: Box<Ast>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarAssignEqual(VarAssignEqual {
                operator,
                lhs,
                expression,
            }),
        })
    }

    pub fn new_function(
        token: Token,
        is_static: bool,
        anonymous: bool,
        yields: bool,
        parameters: Option<Vec<Box<Ast>>>,
        body: Box<Ast>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Function(Function {
                is_static,
                anonymous,
                yields,
                parameters,
                body,
            }),
        })
    }

    pub fn new_expr_statement(is_statement: bool, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token: expression.token.clone(),
            data: AstData::ExpressionStatement(is_statement, expression),
        })
    }

    pub fn new_parameter(token: Token, is_variadic: bool) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Parameter(is_variadic),
        })
    }

    pub fn new_this(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::This,
        })
    }

    pub fn new_super(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Super,
        })
    }

    pub fn new_yield(token: Token, maybe_expr: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Yield(maybe_expr),
        })
    }

    pub fn new_resume(token: Token, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Resume(expression),
        })
    }

    pub fn new_type_of(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::TypeOf,
        })
    }

    pub fn new_return(token: Token, expression: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Return(expression),
        })
    }

    pub fn new_if_statement(
        token: Token,
        is_expression: bool,
        condition: Box<Ast>,
        true_body: Box<Ast>,
        false_body: Option<Box<Ast>>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IfStatement(IfStatement {
                is_expression,
                condition,
                true_body,
                false_body,
            }),
        })
    }

    pub fn new_while_statement(token: Token, expression: Box<Ast>, body: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::WhileStatement(WhileStatement { expression, body }),
        })
    }

    pub fn new_for_statement(
        token: Token,
        variable: Option<Box<Ast>>,
        expression: Option<Box<Ast>>,
        body: Box<Ast>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::ForStatement(ForStatement {
                variable,
                expression,
                body,
            }),
        })
    }

    pub fn new_loop_statement(token: Token, body: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::LoopStatement(body),
        })
    }

    pub fn new_function_call(token: Token, lhs: Box<Ast>, arguments: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::FunctionCall(FunctionCall { lhs, arguments }),
        })
    }

    pub fn new_switch_case(token: Token, case: Option<Box<Ast>>, body: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::SwitchCase(SwitchCase { case, body }),
        })
    }

    pub fn new_switch_statement(
        token: Token,
        condition: Box<Ast>,
        cases: Vec<Box<Ast>>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::SwitchStatement(SwitchStatement { condition, cases }),
        })
    }

    pub fn new_binary_op(token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::BinaryOp(BinaryOp { lhs, rhs }),
        })
    }

    pub fn new_unary_op(token: Token, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::UnaryOp(rhs),
        })
    }

    pub fn new_logical_op(token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::LogicalOp(LogicalOp { lhs, rhs }),
        })
    }

    pub fn new_index_getter(token: Token, expression: Box<Ast>, index: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IndexGetter(IndexGetter { expression, index }),
        })
    }

    pub fn new_index_setter(token: Token, expression: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IndexSetter(IndexSetter { expression, rhs }),
        })
    }

    pub fn new_property_getter(token: Token, lhs: Box<Ast>, property: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::PropertyGetter(PropertyGetter { lhs, property }),
        })
    }

    pub fn new_property_setter(token: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::PropertySetter(PropertySetter { lhs, expression }),
        })
    }
}
