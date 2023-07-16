use std::{fmt::Display, io};

#[derive(Debug)]
pub enum SpruceErrData {
    Generic,
    Parser {
        file_path: String,
        line: u32,
        column: u16,
    },
    Analyser,
    Compiler {
        file_path: Option<String>,
        line: u32,
        column: u16,
    },
    VM,
}

#[derive(Debug)]
pub struct SpruceErr {
    pub message: String,
    pub payload: SpruceErrData,
}

impl SpruceErr {
    pub fn new(message: String, payload: SpruceErrData) -> Self {
        Self { message, payload }
    }
}

impl From<io::Error> for SpruceErr {
    fn from(value: io::Error) -> Self {
        Self {
            message: value.to_string(),
            payload: SpruceErrData::Generic,
        }
    }
}

impl Display for SpruceErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = write!(f, "[\x1b[31mError\x1b[0m] {} ", self.message);

        match self.payload {
            SpruceErrData::Parser {
                ref file_path,
                line,
                column,
            } => write!(f, "'{}' [{}:{}]", file_path, line, column)?,
            SpruceErrData::Compiler {
                ref file_path,
                line,
                column,
            } => {
                if let Some(file_path) = file_path {
                    write!(f, "'{file_path}' [{line}:{column}]")?
                } else {
                    write!(f, "[{line}:{column}]")?
                }
            }
            _ => {}
        }

        r
    }
}
