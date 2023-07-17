use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{
    ast_inner::{BinaryOp, Function},
    source::Source,
};

use super::{
    ast::{Ast, AstData},
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub struct Parser {
    lexer: Lexer,
    source: Rc<Source>,
    current: Token,
    had_error: bool,
    included: HashSet<PathBuf>,
}

impl Parser {
    pub fn new(source: Rc<Source>) -> Parser {
        let mut lexer = Lexer::new(Rc::clone(&source));
        let token = lexer.next();

        Self {
            lexer,
            source,
            current: token,
            had_error: false,
            included: HashSet::new(),
        }
    }

    pub fn run(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let program = self.outer_statements();
        if self.had_error {
            Err(SpruceErr::new(
                "Encountered an error while parsing".into(),
                SpruceErrData::Parser {
                    file_path: (*self.lexer.source.file_path).clone(),
                    line: self.current.line,
                    column: self.current.column,
                },
            ))
        } else {
            program
        }
    }

    #[inline]
    fn get_current(&self) -> Token {
        self.current.clone()
    }

    fn error(&self, message: String) -> SpruceErr {
        SpruceErr::new(
            message,
            SpruceErrData::Parser {
                file_path: (*self.lexer.source.file_path).clone(),
                line: self.current.line,
                column: self.current.column,
            },
        )
    }

    fn consume(&mut self, expected: TokenKind, msg: &str) -> Result<(), SpruceErr> {
        if self.current.kind == expected {
            self.current = self.lexer.next();
            return Ok(());
        }

        Err(self.error(String::from(msg)))
    }

    fn consume_here(&mut self) {
        self.current = self.lexer.next();
    }

    fn is_any_of(&self, kinds: &[TokenKind]) -> bool {
        kinds.iter().any(|k| self.current.kind == *k)
    }

    fn primary(&mut self) -> Result<Box<Ast>, SpruceErr> {
        match self.current.kind {
            TokenKind::Number
            | TokenKind::String
            | TokenKind::None
            | TokenKind::True
            | TokenKind::False => {
                let token = self.get_current();
                self.consume_here();
                Ok(Ast::new_literal(token))
            }

            TokenKind::This => {
                let token = self.get_current();
                self.consume_here();
                Ok(Ast::new_this(token))
            }

            TokenKind::LParen => {
                self.consume_here();
                let expr = self.expression()?;

                if self.current.kind == TokenKind::Comma {
                    let mut exprs = vec![expr];

                    while self.current.kind == TokenKind::Comma {
                        self.consume_here();
                        exprs.push(self.expression()?);
                    }

                    self.consume(TokenKind::RParen, "Expect ')' to close group expression")?;
                    return Ok(Ast::new_tuple_literal(exprs[0].token.clone(), exprs));
                }

                self.consume(TokenKind::RParen, "Expect ')' to close group expression")?;
                Ok(expr)
            }

            TokenKind::LSquare => self.array_literal(),
            TokenKind::LCurly => self.body(),

            TokenKind::If => self.if_expression_statement(true),

            TokenKind::Identifier => {
                let token = self.get_current();
                self.consume_here();
                let identifier = Ast::new_identifier(token);

                // FIXME: Allow struct literals without colliding with blocks
                // Ok(if self.current.kind == TokenKind::LCurly {
                //     self.struct_literal(Some(identifier))?
                // } else {
                //     identifier
                // })
                Ok(identifier)
            }

            TokenKind::Function => self.anon_function(),
            TokenKind::Colon => self.struct_literal(),

            _ => Err(self.error(format!(
                "Unexpected token found {:?} '{}'",
                self.current.kind,
                self.source
                    .maybe_slice_from(self.get_current().lexeme)
                    .unwrap_or(&self.current.kind.to_string()),
            ))),
        }
    }

    fn call(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.primary()?;

        loop {
            node = match self.current.kind {
                TokenKind::LParen => self.function_call(node)?,
                TokenKind::LSquare => self.index(node)?,
                TokenKind::Dot => self.dot_property(node)?,
                _ => break,
            }
        }

        Ok(node)
    }

    fn unary(&mut self) -> Result<Box<Ast>, SpruceErr> {
        loop {
            match self.current.kind {
                TokenKind::Minus | TokenKind::Bang => {
                    let token = self.get_current();
                    self.consume_here();
                    return Ok(Ast::new_unary_op(token, self.expression()?));
                }
                _ => break,
            }
        }

        self.call()
    }

    fn factor(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.unary()?;

        loop {
            if self.is_any_of(&[TokenKind::Star, TokenKind::Slash]) {
                let token = self.get_current();
                self.consume_here();
                node = Ast::new_binary_op(token, node, self.unary()?);
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn term(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.factor()?;

        loop {
            if self.is_any_of(&[TokenKind::Plus, TokenKind::Minus]) {
                let token = self.get_current();
                self.consume_here();
                node = Ast::new_binary_op(token, node, self.factor()?);
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn comparison(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.term()?;

        loop {
            match self.current.kind {
                TokenKind::Greater
                | TokenKind::Less
                | TokenKind::GreaterEqual
                | TokenKind::LessEqual => {
                    let token = self.get_current();
                    self.consume_here();
                    node = Ast::new_logical_op(token, node, self.term()?);
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn equality(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.comparison()?;

        loop {
            match self.current.kind {
                TokenKind::EqualEqual | TokenKind::NotEqual => {
                    let token = self.get_current();
                    self.consume_here();
                    node = Ast::new_logical_op(token, node, self.comparison()?);
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn or(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.equality()?;

        while self.current.kind == TokenKind::Or {
            let token = self.get_current();
            self.consume_here();
            node = Ast::new_logical_op(token, node, self.expression()?);
        }

        Ok(node)
    }

    fn and(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.or()?;

        while self.current.kind == TokenKind::And {
            let token = self.get_current();
            self.consume_here();
            node = Ast::new_logical_op(token, node, self.expression()?);
        }

        Ok(node)
    }

    fn assignment(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.and()?;

        loop {
            node = match self.current.kind {
                TokenKind::Equal => {
                    self.consume_here();
                    match node.data {
                        AstData::Identifier => {
                            Ast::new_var_assign(node.token.clone(), node, self.expression()?)
                        }
                        AstData::IndexGetter { .. } => {
                            Ast::new_index_setter(node.token.clone(), node, self.expression()?)
                        }
                        AstData::PropertyGetter { .. } => {
                            Ast::new_property_setter(node.token.clone(), node, self.expression()?)
                        }
                        _ => {
                            return Err(self.error(format!(
                                "Cannot use '{}':{:?} on lhs of assignment",
                                self.source
                                    .maybe_slice_from(node.token.lexeme)
                                    .unwrap_or(&self.current.kind.to_string()),
                                node.token.kind,
                            )))
                        }
                    }
                }

                TokenKind::PlusEqual
                | TokenKind::MinusEqual
                | TokenKind::StarEqual
                | TokenKind::SlashEqual => {
                    let operator = self.get_current();
                    self.consume_here();
                    match node.data {
                        AstData::Identifier => Ast::new_var_assign_equal(
                            node.token.clone(),
                            operator,
                            node,
                            self.expression()?,
                        ),
                        _ => {
                            return Err(self.error(format!(
                                "Cannot use '{}':{:?} on lhs of assignment operator '{}'",
                                self.source
                                    .maybe_slice_from(node.token.lexeme)
                                    .unwrap_or(&self.current.kind.to_string()),
                                node.token.kind,
                                self.source
                                    .maybe_slice_from(operator.lexeme)
                                    .unwrap_or(&self.current.kind.to_string()),
                            )))
                        }
                    }
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn expression(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.assignment()
    }

    fn expression_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let node = Ast::new_expr_statement(true, self.expression()?);
        self.consume(
            TokenKind::SemiColon,
            "Expect ';' after expression statement",
        )?;
        Ok(node)
    }

    fn array_literal(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let mut values = Vec::new();

        if self.current.kind != TokenKind::RSquare {
            values.push(self.expression()?);

            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                values.push(self.expression()?);
            }
        }

        self.consume(
            TokenKind::RSquare,
            "Expect ']' after array literal arguments",
        )?;

        Ok(Ast::new_array_literal(token, values))
    }

    fn struct_literal(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        self.consume(
            TokenKind::LCurly,
            "Expect '{' after ':' to start struct literal",
        )?;

        let mut values = Vec::new();

        if self.current.kind != TokenKind::RCurly {
            let identifier = self.get_current();
            self.consume(TokenKind::Identifier, "Expect identifier as key in struct")?;
            let mut expr = None;

            if self.current.kind == TokenKind::Colon {
                self.consume_here();
                expr = Some(self.expression()?);
            }

            values.push((identifier, expr));

            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                let identifier = self.get_current();
                self.consume(TokenKind::Identifier, "Expect identifier as key in struct")?;
                let mut expr = None;

                if self.current.kind == TokenKind::Colon {
                    self.consume_here();
                    expr = Some(self.expression()?);
                }

                values.push((identifier, expr));
            }
        }

        self.consume(TokenKind::RCurly, "Expect '}' after struct literal")?;

        Ok(Ast::new_struct_literal(token, values))
    }

    fn function_call(&mut self, lhs: Box<Ast>) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume(TokenKind::LParen, "Expect '(' after function identifier")?;

        // We track arguments, since native functions can have N..Any parameters
        let mut arguments = Vec::new();
        if self.current.kind != TokenKind::RParen {
            arguments.push(self.expression()?);

            while self.current.kind == TokenKind::Comma {
                self.consume(TokenKind::Comma, "Expect ',' after function argument")?;
                arguments.push(self.expression()?);
            }
        }

        self.consume(TokenKind::RParen, "Expect ')' after argument list")?;
        Ok(Ast::new_function_call(token, lhs, arguments))
    }

    fn index(&mut self, expression: Box<Ast>) -> Result<Box<Ast>, SpruceErr> {
        self.consume(TokenKind::LSquare, "Expect '[' after expression to index")?;
        let index = self.expression()?;
        self.consume(TokenKind::RSquare, "Expect ']' after index expression")?;

        Ok(Ast::new_index_getter(
            expression.token.clone(),
            expression,
            index,
        ))
    }

    fn dot_property(&mut self, lhs: Box<Ast>) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expect identifier after '.'")?;
        Ok(Ast::new_property_getter(
            identifier.clone(),
            lhs,
            Ast::new_identifier(identifier),
        ))
    }

    fn maybe_single_statement_block(&mut self) -> Result<Box<Ast>, SpruceErr> {
        match self.current.kind {
            TokenKind::If => self.if_expression_statement(false),
            TokenKind::For => self.for_statement(),
            _ => self.body(),
        }
    }

    fn module_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expect identifier after 'module'")?;
        self.consume(TokenKind::LCurly, "Expect '{' after module identifier")?;

        let mut statements = Vec::new();

        while self.current.kind != TokenKind::EndOfFile && self.current.kind != TokenKind::RCurly {
            statements.push(match self.current.kind {
                TokenKind::Module => self.module_statement()?,
                TokenKind::Function => self.function()?,
                TokenKind::Struct => self.struct_statement()?,
                // TokenKind::Let => self.let_declaration()?,
                _ => {
                    return Err(self.error(
                        "Unknown item in module body. Can only have functions and modules".into(),
                    ))
                }
            });
        }

        self.consume(TokenKind::RCurly, "Expect '}' after module body")?;
        Ok(Ast::new_module(identifier, statements))
    }

    fn struct_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expect identifier after 'struct'")?;
        self.consume(TokenKind::LCurly, "Expect '{' after struct identifier")?;

        let mut statements = Vec::new();

        while self.current.kind != TokenKind::EndOfFile && self.current.kind != TokenKind::RCurly {
            statements.push(match self.current.kind {
                TokenKind::Struct => self.struct_statement()?,
                TokenKind::Function => self.function()?,
                TokenKind::Let => {
                    let let_decl = self.let_declaration()?;
                    self.consume(TokenKind::SemiColon, "Expect semicolon after struct let member")?;
                    let_decl
                },

                n => {
                    return Err(self.error(format!(
                        "Unknown item in struct body '{n:?}'. Can only have structs, functions and let bindings",
                    )))
                }
            });
        }

        self.consume(TokenKind::RCurly, "Expect '}' after struct body")?;
        Ok(Ast::new_struct_def(identifier, statements))
    }

    fn if_expression_statement(&mut self, force_else: bool) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let condition = self.expression()?;
        let true_body = self.maybe_single_statement_block()?;
        let mut false_body = None;

        if self.current.kind == TokenKind::Else {
            self.consume_here();

            false_body = Some(if self.current.kind == TokenKind::If {
                self.if_expression_statement(force_else)?
            } else {
                self.body()?
            });
        } else if force_else {
            return Err(self.error("If expected an else branch".into()));
        }

        Ok(Ast::new_if_statement(
            token, force_else, condition, true_body, false_body,
        ))
    }

    fn for_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let variable = if self.current.kind != TokenKind::LCurly {
            Some(self.let_declaration()?)
        } else {
            None
        };

        let expr = if self.current.kind == TokenKind::In {
            self.consume_here();
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Ast::new_for_statement(
            token,
            variable,
            expr,
            self.maybe_single_statement_block()?,
        ))
    }

    fn switch_case(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        let case = if self.current.kind == TokenKind::Else {
            self.consume_here();
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenKind::Colon, "Expect ':' after switch case value")?;
        Ok(Ast::new_switch_case(
            token,
            case,
            if self.current.kind == TokenKind::LCurly {
                self.body()?
            } else {
                self.expression_statement()?
            },
        ))
    }

    fn switch_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let condition = self.expression()?;
        self.consume(TokenKind::LCurly, "Expect '{' after switch condition")?;

        let mut cases = Vec::new();

        while self.current.kind != TokenKind::RCurly {
            cases.push(self.switch_case()?);
        }

        self.consume(TokenKind::RCurly, "Expect '}' after switch cases")?;
        Ok(Ast::new_switch_statement(token, condition, cases))
    }

    fn return_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();
        let mut expression = None;

        if self.current.kind != TokenKind::SemiColon {
            expression = Some(self.expression()?);
        }

        Ok(Ast::new_return(token, expression))
    }

    fn statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let node = match self.current.kind {
            TokenKind::Function => {
                let func = self.function()?;
                if let AstData::Function(Function { body, .. }) = &func.data {
                    if let AstData::Return(_) = &body.data {
                        self.consume(TokenKind::SemiColon, "Expect ';' after function statement")?;
                    }
                }
                func
            }

            TokenKind::If => self.if_expression_statement(false)?,
            TokenKind::For => self.for_statement()?,
            TokenKind::Switch => self.switch_statement()?,
            TokenKind::Let => self.let_declaration()?,
            TokenKind::Return => self.return_statement()?,
            _ => {
                let node = self.expression()?;
                let is_stmt = match node.data {
                    _ if self.current.kind == TokenKind::SemiColon => {
                        self.consume_here();
                        true
                    }
                    _ => false,
                };
                return Ok(Ast::new_expr_statement(is_stmt, node));
            }
        };

        match node.data {
            AstData::SwitchStatement { .. }
            | AstData::Function { .. }
            | AstData::IfStatement { .. }
            | AstData::Module(_)
            | AstData::ForStatement { .. } => {}

            _ => self.consume(TokenKind::SemiColon, "Expect ';' after statement")?,
        }

        Ok(node)
    }

    fn body(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume(TokenKind::LCurly, "Expect '{' to start block body")?;
        let mut statements = Vec::new();

        while self.current.kind != TokenKind::RCurly {
            statements.push(self.statement()?);
        }

        self.consume(TokenKind::RCurly, "Expect '}' to end block body")?;

        Ok(Ast::new_body(token, statements))
    }

    #[inline]
    fn consume_parameter(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let param_name = self.get_current();
        self.consume(
            TokenKind::Identifier,
            "Expected identifier in parameter list",
        )?;

        Ok(Ast::new_parameter(param_name))
    }

    fn collect_params(
        &mut self,
        left: TokenKind,
        right: TokenKind,
    ) -> Result<Option<Vec<Box<Ast>>>, SpruceErr> {
        let parameters = if self.current.kind == left {
            let mut parameters = Vec::new();
            self.consume(
                left,
                &format!("Expect '{:?}' at the start of parameter list", left),
            )?;

            // Consume paarameter list
            // TODO: Underscore to add unnamed parameter
            if self.current.kind != right {
                if self.current.kind == TokenKind::UnderscoreUnderscore {
                    self.consume_here();
                } else {
                    parameters.push(self.consume_parameter()?);

                    while self.current.kind == TokenKind::Comma {
                        self.consume_here();
                        parameters.push(self.consume_parameter()?);
                    }
                }
            }

            self.consume(
                right,
                &format!("Expect '{:?}' after function parameter list", right),
            )?;
            Some(parameters)
        } else {
            None
        };

        Ok(parameters)
    }

    fn collect_body(&mut self) -> Result<Box<Ast>, SpruceErr> {
        Ok(if self.current.kind == TokenKind::Equal {
            let token = self.get_current();
            self.consume_here();
            Ast::new_return(token, Some(self.expression()?))
        } else {
            self.body()?
        })
    }

    fn anon_function(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let parameters = self.collect_params(TokenKind::LParen, TokenKind::RParen)?;
        let body = self.collect_body()?;

        Ok(Ast::new_function(token, true, parameters, body))
    }

    fn include(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();
        match self.current.kind {
            TokenKind::Identifier | TokenKind::String => {
                // TODO: Check if path is already included, can use std lib token
                let include = if self.current.kind == TokenKind::Identifier {
                    let identifier = self
                        .source
                        .maybe_slice_from(self.get_current().lexeme)
                        .unwrap_or(&self.current.kind.to_string())
                        .to_string();

                    let path = PathBuf::from(identifier);
                    if self.included.contains(&path) {
                        let noop = Ast::new_empty(self.get_current());
                        self.consume_here();
                        return Ok(noop);
                    }
                    self.included.insert(path);

                    let token = self.get_current();
                    self.consume_here();

                    Ast::new_std_include(token)
                } else {
                    let maybe_path = Path::new(&*self.lexer.source.file_path)
                        .parent()
                        .unwrap()
                        .join(
                            self.source
                                .maybe_slice_from(self.get_current().lexeme)
                                .unwrap_or(&self.current.kind.to_string()),
                        );

                    let include_path = fs::canonicalize(&maybe_path);

                    if include_path.is_err() {
                        let path = maybe_path.to_str().unwrap().to_string();
                        return Err(self.error(format!("Include path does not exist '{}'", path)));
                    }

                    let include_path = include_path.unwrap();

                    if self.included.contains(&include_path) {
                        let noop = Ast::new_empty(self.get_current());
                        self.consume_here();
                        return Ok(noop);
                    }

                    let _source = fs::read_to_string(&include_path).unwrap();
                    // // let program = match util::compile_source(
                    // //     include_str.clone(),
                    // //     source,
                    // //     self.args.clone(),
                    // // ) {
                    // //     Ok((_, program)) => program,
                    // //     Err(e) => {
                    // //         return Err(self.error(format!(
                    // //             "Could not parse '{}' because {}",
                    // //             include_str, e.message
                    // //         )))
                    // //     }
                    // // };

                    self.included.insert(include_path);
                    // program

                    // FIXME: Make the above code not trash to allow imports
                    Ast::new_empty(self.get_current())
                };

                self.consume_here();

                Ok(include)
            }
            _ => Err(self.error(format!(
                "Include expected string or identifier, but recieved '{}'",
                self.source
                    .maybe_slice_from(self.get_current().lexeme)
                    .unwrap_or(&self.current.kind.to_string())
            ))),
        }
    }

    fn function(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expected identifier after 'fn'")?;

        let parameters = self.collect_params(TokenKind::LParen, TokenKind::RParen)?;

        let body = self.collect_body()?;

        Ok(Ast::new_function(identifier, false, parameters, body))
    }

    fn collect_var_decl(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let is_mutable = if self.current.kind == TokenKind::Mutable {
            self.consume_here();
            true
        } else {
            false
        };

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expected identifier after 'let'")?;

        // Produce the expression
        let mut expr = None;
        if self.current.kind == TokenKind::Equal {
            self.consume_here();
            expr = Some(self.expression()?);
        }

        Ok(Ast::new_var_decl(identifier, is_mutable, expr))
    }

    fn let_declaration(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let mut decls = vec![self.collect_var_decl()?];

        while self.current.kind == TokenKind::Comma {
            self.consume_here();
            decls.push(self.collect_var_decl()?);
        }

        Ok(Ast::new_var_decls(decls))
    }

    fn outer_statements(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        let mut statements = Vec::new();

        while self.current.kind != TokenKind::EndOfFile {
            match self.current.kind {
                TokenKind::Include => {
                    statements.push(self.include()?);
                    self.consume(TokenKind::SemiColon, "Expect ';' after include statement")?;
                }
                TokenKind::Module => statements.push(self.module_statement()?),
                TokenKind::Struct => statements.push(self.struct_statement()?),
                TokenKind::Function => {
                    let func = self.function()?;
                    if let AstData::Function(Function { body, .. }) = &func.data {
                        if let AstData::Return(_) = &body.data {
                            self.consume(
                                TokenKind::SemiColon,
                                "Expect ';' after function statement",
                            )?;
                        }
                    }
                    statements.push(func);
                }
                TokenKind::Let => {
                    statements.push(self.let_declaration()?);
                    self.consume(TokenKind::SemiColon, "Expect ';' after variable statement")?;
                }

                _ => statements.push(self.statement()?),
            }
        }

        Ok(Ast::new_program(token, statements))
    }
}

#[test]
fn simple_binary_operation() {
    let source = Rc::new(Source::from("1 + 2"));
    let mut parser = Parser::new(source);

    let expr = parser.expression();
    assert!(expr.is_ok());
    let expr = expr.unwrap();

    assert!(matches!(expr.data, AstData::BinaryOp { .. }));

    let AstData::BinaryOp(BinaryOp { lhs, rhs }) = expr.data else { unreachable!() };
    assert!(matches!(lhs.data, AstData::Literal));
    assert!(matches!(rhs.data, AstData::Literal));
}
