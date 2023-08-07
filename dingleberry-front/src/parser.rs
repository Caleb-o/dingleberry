use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::{ast_inner::Function, source::Source};

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
    included: HashMap<Box<PathBuf>, Rc<Box<Ast>>>,
    current_path: Box<PathBuf>,
}

struct ParserState {
    lexer: Lexer,
    token: Token,
    source: Rc<Source>,
    last_path: Box<PathBuf>,
}

impl Parser {
    pub fn new(source: Rc<Source>) -> Parser {
        let mut lexer = Lexer::new(Rc::clone(&source));
        let token = lexer.next();

        let mut included = HashMap::new();

        let current_path = if source.file_path.as_str() != "source" {
            let path = Path::new(&*source.file_path).canonicalize().unwrap();
            let current_path = Box::new(path.clone().parent().unwrap().to_path_buf());
            included.insert(Box::new(path), Rc::new(Ast::new_empty(token.clone())));
            current_path
        } else {
            Box::new(PathBuf::from("source"))
        };

        Self {
            lexer,
            source,
            current: token,
            had_error: false,
            included,
            current_path,
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

    fn set_new_state(&mut self, file_path: &Box<PathBuf>) -> ParserState {
        let last_path = self.current_path.clone();
        self.current_path = Box::new(file_path.parent().unwrap().to_path_buf());

        let content = fs::read_to_string(&**file_path).unwrap();
        let source = Rc::new(Source::new(
            file_path.as_os_str().to_str().unwrap().to_string(),
            content,
        ));

        let last_source = self.source.clone();
        let last_token = self.current.clone();
        let last_lexer = self.lexer.clone();

        // Do stuff
        let mut lexer = Lexer::new(source);
        self.current = lexer.next();
        self.lexer = lexer;

        ParserState {
            lexer: last_lexer,
            token: last_token,
            source: last_source,
            last_path,
        }
    }

    fn restore_state(&mut self, state: ParserState) {
        let ParserState {
            lexer,
            token,
            source,
            last_path,
        } = state;

        self.current_path = last_path;
        self.source = source;
        self.lexer = lexer;
        self.current = token;
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

    fn consume_maybe(&mut self, expected: TokenKind) -> bool {
        if self.current.kind == expected {
            self.current = self.lexer.next();
            return true;
        }

        false
    }

    fn consume_any(&mut self, expected: &[TokenKind], msg: &str) -> Result<(), SpruceErr> {
        for kind in expected {
            if self.current.kind == *kind {
                self.current = self.lexer.next();
                return Ok(());
            }
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

            TokenKind::Super => {
                let token = self.get_current();
                self.consume_here();
                Ok(Ast::new_super(token))
            }

            TokenKind::At => self.type_of_expr(),

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
            TokenKind::If => self.if_expression_statement(true),
            TokenKind::Resume => self.resume_expression(),

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

    fn type_of_expr(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let type_name = self.get_current();
        self.consume_any(
            &[
                TokenKind::Identifier,
                TokenKind::None,
                TokenKind::Module,
                TokenKind::Struct,
            ],
            "Expect identifier type name after '@'",
        )?;

        Ok(Ast::new_type_of(type_name))
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
        self.consume_any(
            &[TokenKind::Identifier, TokenKind::String],
            "Expect identifier or string after '.'",
        )?;
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
            TokenKind::While => self.while_statement(),
            _ => self.body(),
        }
    }

    fn static_declaration(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        match self.current.kind {
            TokenKind::Struct => self.struct_declaration(true),
            TokenKind::Class => self.class_declaration(true),
            TokenKind::Function => self.function_declaration(true),
            _ => {
                return Err(self.error(
                    "Unknown item after static declaration. Can only have functions and struct declarations"
                        .into(),
                ))
            }
        }
    }

    fn module_declaration(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expect identifier after 'module'")?;
        self.consume(TokenKind::LCurly, "Expect '{' after module identifier")?;

        let mut statements = Vec::new();

        while self.current.kind != TokenKind::EndOfFile && self.current.kind != TokenKind::RCurly {
            statements.push(match self.current.kind {
                TokenKind::Module => self.module_declaration()?,
                TokenKind::Function => self.function_declaration(false)?,
                TokenKind::Struct => self.struct_declaration(false)?,
                TokenKind::Class => self.class_declaration(false)?,

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

    fn struct_declaration(&mut self, is_static: bool) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expect identifier after 'struct'")?;

        let init_fields = if self.current.kind == TokenKind::LParen {
            self.consume_here();
            let mut fields = vec![self.get_current()];
            self.consume_any(
                &[TokenKind::Identifier, TokenKind::String],
                "Expected identifier or string in struct init fields",
            )?;

            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                fields.push(self.get_current());
                self.consume(
                    TokenKind::Identifier,
                    "Expected identifier in struct init fields",
                )?;
            }

            self.consume(TokenKind::RParen, "Expect ')' after struct init fields")?;

            Some(fields)
        } else {
            None
        };

        if self.current.kind == TokenKind::SemiColon {
            self.consume_here();

            return Ok(Ast::new_struct_def(
                identifier,
                is_static,
                init_fields,
                Vec::new(),
            ));
        }

        self.consume(TokenKind::LCurly, "Expect '{' after struct identifier")?;

        let mut declarations = Vec::new();

        while self.current.kind != TokenKind::EndOfFile && self.current.kind != TokenKind::RCurly {
            declarations.push(match self.current.kind {
                TokenKind::Static => self.static_declaration()?,

                TokenKind::Struct => self.struct_declaration(false)?,
                TokenKind::Class => self.class_declaration(false)?,
                TokenKind::Function => self.function_declaration(false)?,

                n => {
                    return Err(self.error(format!(
                        "Unknown item in struct body '{n:?}'. Can only have classes, structs, functions and let bindings",
                    )))
                }
            });
        }

        self.consume(TokenKind::RCurly, "Expect '}' after struct body")?;
        Ok(Ast::new_struct_def(
            identifier,
            is_static,
            init_fields,
            declarations,
        ))
    }

    fn class_declaration(&mut self, is_static: bool) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expect identifier after 'class'")?;

        let init_fields = if self.current.kind == TokenKind::LParen {
            self.consume_here();
            let mut fields = vec![self.get_current()];
            self.consume_any(
                &[TokenKind::Identifier, TokenKind::String],
                "Expected identifier or string in class init fields",
            )?;

            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                fields.push(self.get_current());
                self.consume(
                    TokenKind::Identifier,
                    "Expected identifier in class init fields",
                )?;
            }

            self.consume(TokenKind::RParen, "Expect ')' after class init fields")?;

            Some(fields)
        } else {
            None
        };

        let super_class = if self.current.kind == TokenKind::Colon {
            self.consume_here();
            let class_name = self.expression()?;
            Some(class_name)
        } else {
            None
        };

        if self.current.kind == TokenKind::SemiColon {
            self.consume_here();

            return Ok(Ast::new_class_def(
                identifier,
                is_static,
                init_fields,
                super_class,
                Vec::new(),
            ));
        }

        self.consume(TokenKind::LCurly, "Expect '{' after class identifier")?;

        let mut declarations = Vec::new();

        while self.current.kind != TokenKind::EndOfFile && self.current.kind != TokenKind::RCurly {
            declarations.push(match self.current.kind {
                TokenKind::Static => self.static_declaration()?,

                TokenKind::Struct => self.struct_declaration(false)?,
                TokenKind::Class => self.class_declaration(false)?,
                TokenKind::Function => self.function_declaration(false)?,

                n => {
                    return Err(self.error(format!(
                        "Unknown item in class body '{n:?}'. Can only have classes, structs and functions.",
                    )))
                }
            });
        }

        self.consume(TokenKind::RCurly, "Expect '}' after class body")?;
        Ok(Ast::new_class_def(
            identifier,
            is_static,
            init_fields,
            super_class,
            declarations,
        ))
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

    fn while_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let expression = self.expression()?;
        let body = self.maybe_single_statement_block()?;

        Ok(Ast::new_while_statement(token, expression, body))
    }

    fn for_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let variable = if self.current.kind == TokenKind::Let {
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

    fn loop_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        Ok(Ast::new_loop_statement(token, self.body()?))
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

    fn yield_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let maybe_expr = if self.current.kind != TokenKind::SemiColon {
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Ast::new_yield(token, maybe_expr))
    }

    fn resume_expression(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.get_current();
        self.consume_here();

        let expression = self.expression()?;
        Ok(Ast::new_resume(token, expression))
    }

    fn statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let node = match self.current.kind {
            TokenKind::If => self.if_expression_statement(false)?,
            TokenKind::For => self.for_statement()?,
            TokenKind::While => self.while_statement()?,
            TokenKind::Loop => self.loop_statement()?,
            TokenKind::Switch => self.switch_statement()?,
            TokenKind::Let => self.let_declaration()?,
            TokenKind::Return => self.return_statement()?,
            TokenKind::LCurly => self.body()?,

            TokenKind::Yield => self.yield_statement()?,

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
            | AstData::LoopStatement(_)
            | AstData::ForStatement { .. }
            | AstData::Body(_)
            | AstData::WhileStatement { .. } => {}

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
        Ok(if self.current.kind == TokenKind::LSquare {
            self.consume_here();

            let param_name = self.get_current();
            self.consume(
                TokenKind::Identifier,
                "Expected identifier in parameter list",
            )?;

            self.consume(TokenKind::RSquare, "Expect ']' after parameter identifier")?;

            Ast::new_parameter(param_name, true)
        } else {
            let param_name = self.get_current();
            self.consume(
                TokenKind::Identifier,
                "Expected identifier in parameter list",
            )?;

            Ast::new_parameter(param_name, false)
        })
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
        let yields = self.consume_maybe(TokenKind::Yields);
        let body = self.collect_body()?;

        Ok(Ast::new_function(
            token, false, true, yields, parameters, body,
        ))
    }

    fn include(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();
        match self.current.kind {
            TokenKind::String => {
                let token = self.get_current();
                self.consume_here();

                let module_name = if self.current.kind == TokenKind::In {
                    self.consume_here();
                    let token = self.get_current();
                    self.consume(
                        TokenKind::Identifier,
                        "Expect identifier after include 'in'",
                    )?;
                    Some(token)
                } else {
                    None
                };

                let path_token = token.clone();
                let slice = path_token.lexeme.as_ref().unwrap().get_slice();
                let file_name = &slice[1..slice.len() - 1];
                let file_path = PathBuf::from(format!(
                    "{}/{}.dingle",
                    self.current_path.display(),
                    file_name
                ));

                if !Path::exists(&file_path) {
                    return Err(SpruceErr::new(
                        format!("Cannot find file '{file_path:?}'"),
                        SpruceErrData::Parser {
                            file_path: (*self.source.file_path).clone(),
                            line: token.line,
                            column: token.column,
                        },
                    ));
                }

                let boxed_path = Box::new(file_path);
                if let Some(included) = self.included.get(&boxed_path) {
                    return Ok(if module_name.is_some() {
                        Ast::new_include(token, included.clone(), module_name)
                    } else {
                        Ast::new_empty(token)
                    });
                }

                let state = self.set_new_state(&boxed_path);
                let root = Rc::new(self.run()?);

                self.included.insert(boxed_path, root.clone());
                self.restore_state(state);

                Ok(Ast::new_include(token, root, module_name))
            }

            _ => Err(self.error(format!(
                "Include expected string, but recieved '{}'",
                self.source
                    .maybe_slice_from(self.get_current().lexeme)
                    .unwrap_or(&self.current.kind.to_string())
            ))),
        }
    }

    fn function_declaration(&mut self, is_static: bool) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.get_current();
        self.consume(TokenKind::Identifier, "Expected identifier after 'fn'")?;

        let parameters = self.collect_params(TokenKind::LParen, TokenKind::RParen)?;

        let yields = self.consume_maybe(TokenKind::Yields);
        let body = self.collect_body()?;

        Ok(Ast::new_function(
            identifier, is_static, false, yields, parameters, body,
        ))
    }

    fn collect_let_decl(&mut self) -> Result<Box<Ast>, SpruceErr> {
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

        let mut decls = vec![self.collect_let_decl()?];

        while self.current.kind == TokenKind::Comma {
            self.consume_here();
            decls.push(self.collect_let_decl()?);
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

                TokenKind::Static => statements.push(self.static_declaration()?),

                TokenKind::Module => statements.push(self.module_declaration()?),
                TokenKind::Struct => statements.push(self.struct_declaration(false)?),
                TokenKind::Class => statements.push(self.class_declaration(false)?),
                TokenKind::If => statements.push(self.if_expression_statement(false)?),
                TokenKind::For => statements.push(self.for_statement()?),
                TokenKind::Loop => statements.push(self.loop_statement()?),
                TokenKind::Function => {
                    let func = self.function_declaration(false)?;
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
                    self.consume(TokenKind::SemiColon, "Expect ';' after let statement")?;
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

    let AstData::BinaryOp(crate::ast_inner::BinaryOp { lhs, rhs }) = expr.data else { unreachable!() };
    assert!(matches!(lhs.data, AstData::Literal));
    assert!(matches!(rhs.data, AstData::Literal));
}
