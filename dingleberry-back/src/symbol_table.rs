use dingleberry_front::token::Token;
use dingleberry_shared::error::{SpruceErr, SpruceErrData};

use crate::get_identifier_or_string;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub identifier: String,
    pub mutable: bool,
    pub index: u16,
    pub depth: u16,
}

pub struct SymbolTable {
    depth: u16,
    locked_at: u16,
    in_depth: Vec<u16>,
    scope: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            depth: 0,
            locked_at: 0,
            in_depth: vec![0],
            scope: Vec::new(),
        }
    }

    #[inline]
    pub fn lock(&mut self) {
        self.locked_at = self.depth;
    }

    #[inline]
    pub fn unlock(&mut self) {
        self.locked_at = 0;
    }

    #[inline]
    pub fn is_locked(&self) -> bool {
        self.locked_at > 0
    }

    #[inline]
    pub fn is_global(&self) -> bool {
        self.depth == 0
    }

    #[inline]
    pub fn new_func(&mut self) {
        self.depth += 1;
        self.in_depth.push(0);
    }

    #[inline]
    pub fn close_func(&mut self) {
        self.depth -= 1;
        self.in_depth.pop();
        self.scope.retain(|sym| sym.depth <= self.depth);
    }

    #[inline]
    pub fn new_scope(&mut self) {
        self.depth += 1;
    }

    #[inline]
    pub fn close_scope(&mut self) {
        self.depth -= 1;
    }

    pub fn find_local_symbol(&self, identifier: &str) -> Option<&Symbol> {
        for (idx, sym) in self.scope.iter().rev().enumerate() {
            if idx as u16 >= *self.in_depth.last().unwrap() {
                break;
            }
            if identifier == sym.identifier {
                return Some(sym);
            }
        }

        None
    }

    pub fn find_symbol_any(&self, identifier: &Token) -> Result<Option<Symbol>, SpruceErr> {
        let sliced_id = identifier.lexeme.as_ref().unwrap().get_slice();

        for sym in self.scope.iter().rev() {
            if self.locked_at > 0 && sym.depth <= self.locked_at && sym.depth != 0 {
                continue;
            }

            if sym.identifier == sliced_id {
                return Ok(Some(sym.clone()));
            }
        }

        if self.is_locked() {
            let file_path = identifier
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(format!("Cannot find '{sliced_id}' as it is outside of locked bounds. This is usually because an anonymous function is looking for a symbol outside its scope or is not global"), SpruceErrData::Compiler { file_path, line: identifier.line, column: identifier.column }));
        }

        Ok(None)
    }

    #[inline]
    pub fn add_global_symbol(
        &mut self,
        identifier: &Token,
        is_mutable: bool,
        str_idx: u16,
    ) -> Result<(), SpruceErr> {
        let string = identifier.lexeme.as_ref().unwrap().get_slice().to_string();

        if self.find_symbol_any(&identifier)?.is_some() {
            let file_path = identifier
                .lexeme
                .as_ref()
                .map(|span| (*span.source.file_path).clone());

            return Err(SpruceErr::new(
                format!("Symbol with name '{string}' already exists in scope"),
                SpruceErrData::Compiler {
                    file_path,
                    line: identifier.line,
                    column: identifier.column,
                },
            ));
        }

        self.scope.push(Symbol {
            identifier: string,
            mutable: is_mutable,
            index: str_idx,
            depth: 0,
        });
        *self.in_depth.last_mut().unwrap() += 1;

        Ok(())
    }

    #[inline]
    pub fn inject_symbol(&mut self, identifier: String, is_mutable: bool) {
        self.scope.push(Symbol {
            identifier,
            mutable: is_mutable,
            index: *self.in_depth.last().unwrap(),
            depth: self.depth,
        });
        *self.in_depth.last_mut().unwrap() += 1;
    }

    pub fn add_symbol(
        &mut self,
        token: &Token,
        is_mutable: bool,
        allow_override: bool,
    ) -> Result<(), SpruceErr> {
        let identifier = get_identifier_or_string(token);

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
