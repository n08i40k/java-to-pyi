use lalrpop_util::lalrpop_mod;
use ownable::{IntoOwned, traits::IntoOwned};
use std::borrow::Cow;

use crate::lexer::{LexicalError, Token};

pub mod ast;
mod lexer;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    java
);

#[derive(Debug, Clone, PartialEq, IntoOwned)]
pub struct ErrorLocation {
    raw: usize,

    row: usize,
    column: usize,
}

impl ErrorLocation {
    pub fn new(str: &str, loc: usize) -> Self {
        let row = str[..loc].chars().filter(|&ch| ch == '\n').count() + 1;
        let column = loc - str[..loc].rfind("\n").unwrap_or(0);

        ErrorLocation {
            raw: loc,
            row,
            column,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error<'a> {
    InvalidToken {
        location: ErrorLocation,
    },

    UnrecognizedEof {
        location: ErrorLocation,
        expected: Vec<String>,
    },

    UnrecognizedToken {
        token: (ErrorLocation, Token<'a>, ErrorLocation),
        expected: Vec<String>,
    },

    ExtraToken {
        token: (ErrorLocation, Token<'a>, ErrorLocation),
    },

    User {
        error: LexicalError,
    },
}

impl<'a> IntoOwned for Error<'a> {
    type Owned = Error<'static>;

    fn into_owned(self) -> Self::Owned {
        match self {
            Error::UnrecognizedToken {
                token: (l, t, r),
                expected,
            } => Error::UnrecognizedToken {
                token: (l, t.into_owned(), r),
                expected,
            },
            Error::ExtraToken { token: (l, t, r) } => Error::ExtraToken {
                token: (l, t.into_owned(), r),
            },
            Error::InvalidToken { location } => Error::InvalidToken { location },
            Error::UnrecognizedEof { location, expected } => {
                Error::UnrecognizedEof { location, expected }
            }
            Error::User { error } => Error::User { error },
        }
    }
}

#[derive(Clone, IntoOwned)]
pub struct ErrorCell<'a> {
    pub owner: Cow<'a, str>,
    pub inner: Error<'a>,
}

impl<'a> ErrorCell<'a> {
    fn new(
        owner: &'a str,
        error: lalrpop_util::ParseError<usize, Token<'a>, LexicalError>,
    ) -> Self {
        Self {
            owner: Cow::from(owner),
            inner: match error {
                lalrpop_util::ParseError::InvalidToken { location } => Error::InvalidToken {
                    location: ErrorLocation::new(owner, location),
                },
                lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                    Error::UnrecognizedEof {
                        location: ErrorLocation::new(owner, location),
                        expected,
                    }
                }
                lalrpop_util::ParseError::UnrecognizedToken {
                    token: (l, t, r),
                    expected,
                } => Error::UnrecognizedToken {
                    token: (
                        ErrorLocation::new(owner, l),
                        t,
                        ErrorLocation::new(owner, r),
                    ),
                    expected,
                },
                lalrpop_util::ParseError::ExtraToken { token: (l, t, r) } => Error::ExtraToken {
                    token: (
                        ErrorLocation::new(owner, l),
                        t,
                        ErrorLocation::new(owner, r),
                    ),
                },
                lalrpop_util::ParseError::User { error } => Error::User { error },
            },
        }
    }
}

impl<'a> std::fmt::Display for ErrorCell<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.inner {
            Error::InvalidToken { location } => {
                write!(f, "Invalid token at {}:{}", location.row, location.column)?
            }
            Error::UnrecognizedEof { location, expected } => write!(
                f,
                "Unrecognized end of file at {}:{}, expected '{}'",
                location.row,
                location.column,
                expected.join("', '")
            )?,
            Error::UnrecognizedToken { token, expected } => write!(
                f,
                "Unrecognized token '{}' from {}:{} to {}:{}, expected '{}'",
                token.1,
                token.0.row,
                token.0.column,
                token.2.row,
                token.2.column,
                expected.join("', '")
            )?,
            Error::ExtraToken { token } => write!(
                f,
                "Unexpected token '{}' at the end of file from {}:{} to {}:{}",
                token.1, token.0.row, token.0.column, token.2.row, token.2.column
            )?,
            Error::User { error } => {
                let start = ErrorLocation::new(self.owner.as_ref(), error.span.start);
                let end = ErrorLocation::new(self.owner.as_ref(), error.span.end);

                match &error.kind {
                    lexer::LexicalErrorKind::InvalidToken => write!(
                        f,
                        "Invalid token from {}:{} to {}:{}",
                        start.row, start.column, end.row, end.column
                    )?,
                    lexer::LexicalErrorKind::InvalidInteger(parse_int_error) => write!(
                        f,
                        "Invalid integer '{}' from {}:{} to {}:{}: {}",
                        &self.owner[error.span.start..error.span.end],
                        start.row,
                        start.column,
                        end.row,
                        end.column,
                        &parse_int_error
                    )?,
                }
            }
        };

        Ok(())
    }
}

pub fn parse<'a>(data: &'a str) -> Result<ast::Root, Box<ErrorCell<'a>>> {
    let lexer = lexer::Lexer::new(data);
    let parser = java::RootParser::new();

    parser
        .parse(data, lexer)
        .map_err(|e| Box::new(ErrorCell::new(data, e)))
}
