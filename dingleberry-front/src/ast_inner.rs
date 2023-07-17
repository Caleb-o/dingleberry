use crate::{ast::Ast, token::Token};

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct LogicalOp {
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub anonymous: bool,
    pub parameters: Option<Vec<Box<Ast>>>,
    pub body: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub lhs: Box<Ast>,
    pub arguments: Vec<Box<Ast>>,
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub is_mutable: bool,
    pub expression: Option<Box<Ast>>,
}

#[derive(Debug, Clone)]
pub struct VarAssign {
    pub lhs: Box<Ast>,
    pub expression: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct VarAssignEqual {
    pub operator: Token,
    pub lhs: Box<Ast>,
    pub expression: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub is_expression: bool,
    pub condition: Box<Ast>,
    pub true_body: Box<Ast>,
    pub false_body: Option<Box<Ast>>,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub variable: Option<Box<Ast>>,
    pub expression: Option<Box<Ast>>,
    pub body: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct IndexGetter {
    pub expression: Box<Ast>,
    pub index: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct IndexSetter {
    pub expression: Box<Ast>,
    pub rhs: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct PropertyGetter {
    pub lhs: Box<Ast>,
    pub property: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct PropertySetter {
    pub lhs: Box<Ast>,
    pub expression: Box<Ast>,
}

#[derive(Debug, Clone)]
pub struct SwitchStatement {
    pub condition: Box<Ast>,
    pub cases: Vec<Box<Ast>>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub case: Option<Box<Ast>>,
    pub body: Box<Ast>,
}
