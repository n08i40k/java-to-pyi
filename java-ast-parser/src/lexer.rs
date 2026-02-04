//! Tokenization for Protocol Buffers sources.
//!
//! # Examples
//! ```rust
//! use protobuf_ast_parser::lexer::{Lexer, Token};
//!
//! let mut lexer = Lexer::new("syntax = \"proto3\";");
//! let first = lexer.next().unwrap().unwrap();
//! assert_eq!(first.1, Token::Syntax);
//! ```

use logos::{Logos, Span};
use ownable::IntoOwned;
use std::{
    borrow::Cow, cell::RefCell, collections::VecDeque, num::ParseIntError, ops::DerefMut, rc::Rc,
};

/// Categories of lexical errors produced by [`Lexer`].
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalErrorKind {
    #[default]
    InvalidToken,
    InvalidInteger(ParseIntError),
}

impl From<ParseIntError> for LexicalErrorKind {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidInteger(value)
    }
}

/// Error emitted when the lexer cannot produce a valid token.
#[derive(Debug, Clone, PartialEq)]
pub struct LexicalError {
    pub kind: LexicalErrorKind,
    pub span: Span,
}

fn string_from_lexer<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Cow<'a, str> {
    let slice = lex.slice();
    Cow::from(&slice[1..slice.len() - 1])
}

/// Token kinds produced by the lexer.
#[derive(Clone, Debug, PartialEq, Logos, IntoOwned)]
#[logos(error = LexicalErrorKind)]
#[logos(skip r"[\s\t\n\f]+")]
#[logos(skip(r"//.*", allow_greedy = true))]
#[logos(skip(
    r"@[A-Za-z0-9.]+(?:\((?:[^()]|\((?:[^()]|\((?:[^()]|\((?:[^()]|\([^()]*\))*\))*\))*\))*\))?"
))]
#[logos(skip r"\/\*[^*]*\*+(?:[^\/*][^*]*\*+)*\/")]
pub enum Token<'a> {
    #[token("=")]
    Eq,

    #[token("!")]
    ExclamationMark,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Percent,

    #[token("^")]
    Caret,

    #[token("&")]
    Ampersand,

    #[token("|")]
    VerticalBar,

    #[token("~")]
    Tilde,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token(",")]
    Comma,

    #[token(".")]
    Period,

    #[token("...")]
    VarArg,

    #[token("(")]
    OpenPth,

    #[token(")")]
    ClosePth,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[token("<")]
    OpenAngle,

    #[token(">")]
    CloseAngle,

    #[token("?")]
    QuestionMark,

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool),

    #[regex(r"-?[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"0x[0-9a-fA-F]{1,16}", |lex| u64::from_str_radix(&lex.slice()[2..], 16).map(|x| x as i64))]
    Integer(i64),

    #[token("abstract")]
    KeywordAbstract,

    #[token("assert")]
    KeywordAssert,

    #[token("break")]
    KeywordBreak,

    #[token("case")]
    KeywordCase,

    #[token("catch")]
    KeywordCatch,

    #[token("class")]
    KeywordClass,

    #[token("continue")]
    KeywordContinue,

    #[token("const")]
    KeywordConst,

    #[token("default")]
    KeywordDefault,

    #[token("do")]
    KeywordDo,

    #[token("else")]
    KeywordElse,

    #[token("enum")]
    KeywordEnum,

    #[token("exports")]
    KeywordExports,

    #[token("extends")]
    KeywordExtends,

    #[token("final")]
    KeywordFinal,

    #[token("finally")]
    KeywordFinally,

    #[token("for")]
    KeywordFor,

    #[token("goto")]
    KeywordGoto,

    #[token("if")]
    KeywordIf,

    #[token("implements")]
    KeywordImplements,

    #[token("import")]
    KeywordImport,

    #[token("instanceof")]
    KeywordInstanceof,

    #[token("interface")]
    KeywordInterface,

    #[token("module")]
    KeywordModule,

    #[token("native")]
    KeywordNative,

    #[token("new")]
    KeywordNew,

    #[token("package")]
    KeywordPackage,

    #[token("private")]
    KeywordPrivate,

    #[token("protected")]
    KeywordProtected,

    #[token("public")]
    KeywordPublic,

    #[token("requires")]
    KeywordRequires,

    #[token("return")]
    KeywordReturn,

    #[token("static")]
    KeywordStatic,

    #[token("strictfp")]
    KeywordStrictfp,

    #[token("super")]
    KeywordSuper,

    #[token("switch")]
    KeywordSwitch,

    #[token("synchronized")]
    KeywordSynchronized,

    #[token("this")]
    KeywordThis,

    #[token("throw")]
    KeywordThrow,

    #[token("throws")]
    KeywordThrows,

    #[token("transient")]
    KeywordTransient,

    #[token("try")]
    KeywordTry,

    #[token("var")]
    KeywordVar,

    #[token("volatile")]
    KeywordVolatile,

    #[token("while")]
    KeywordWhile,

    #[token("void")]
    TypeVoid,

    #[token("boolean")]
    TypeBoolean,

    #[token("char")]
    TypeChar,

    #[token("short")]
    TypeShort,

    #[token("int")]
    TypeInt,

    #[token("long")]
    TypeLong,

    #[token("float")]
    TypeFloat,

    #[token("double")]
    TypeDouble,

    #[regex(
        r#"'(?:\\u[0-9a-fA-F]{4}|\\[0-7]{1,3}|\\[btnfr"'\\]|[^'\\\r\n])'"#,
        string_from_lexer
    )]
    CharLiteral(Cow<'a, str>),

    #[regex(r#""([^"\\]*(?:\\.[^"\\]*)*)""#, string_from_lexer)]
    String(Cow<'a, str>),

    #[regex(r"[\p{L}_][\p{L}\p{Nd}_]*", |x| Cow::from(x.slice()), priority = 0)]
    Ident(Cow<'a, str>),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

enum Scope {
    Root,
    Object { skip_pths: bool, in_body: bool },
    Function,
    EnumVariant,
}

/// Streaming lexer that yields spanned tokens.
pub struct Lexer<'input> {
    inner: logos::SpannedIter<'input, Token<'input>>,
    scope_stack: VecDeque<Rc<RefCell<Scope>>>,
}

impl<'input> Lexer<'input> {
    pub fn new(src: &'input str) -> Self {
        Self {
            inner: Token::lexer(src).spanned(),
            scope_stack: VecDeque::from([Rc::from(RefCell::from(Scope::Root))]),
        }
    }
}

/// LALRPOP-compatible spanned token wrapper.
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (tok, span) = self.inner.next()?;

        let tok = match tok {
            Ok(tok) => tok,
            Err(kind) => {
                return Some(Err(LexicalError { kind, span }));
            }
        };

        let current_scope = self.scope_stack.back().unwrap().clone();

        match current_scope.borrow_mut().deref_mut() {
            Scope::Root => match &tok {
                Token::KeywordClass | Token::KeywordInterface | Token::KeywordEnum => {
                    self.scope_stack
                        .push_back(Rc::from(RefCell::from(Scope::Object {
                            skip_pths: tok == Token::KeywordEnum,
                            in_body: false,
                        })));
                }
                _ => {}
            },
            Scope::Object { skip_pths, in_body } => match &tok {
                Token::KeywordClass | Token::KeywordInterface | Token::KeywordEnum => {
                    self.scope_stack
                        .push_back(Rc::from(RefCell::from(Scope::Object {
                            skip_pths: tok == Token::KeywordEnum,
                            in_body: false,
                        })));
                }
                Token::OpenPth => {
                    if *skip_pths {
                        self.scope_stack
                            .push_back(Rc::from(RefCell::from(Scope::EnumVariant)));
                    }
                }
                Token::Semicolon => {
                    if *skip_pths {
                        *skip_pths = false;
                    }
                }
                Token::OpenBrace => {
                    if !*in_body {
                        *in_body = true;
                        return Some(Ok((span.start, tok, span.end)));
                    }

                    self.scope_stack
                        .push_back(Rc::from(RefCell::from(Scope::Function)));
                }
                Token::CloseBrace => {
                    if *in_body {
                        self.scope_stack.pop_back();
                    }
                }
                Token::Eq => {
                    let mut expr_level = 0;

                    loop {
                        let (tok, span) = self.inner.next()?;

                        let tok = match tok {
                            Ok(tok) => tok,
                            Err(kind) => {
                                return Some(Err(LexicalError { kind, span }));
                            }
                        };

                        match &tok {
                            Token::OpenBrace => {
                                expr_level += 1;
                            }
                            Token::CloseBrace => {
                                if expr_level == 0 {
                                    return Some(Err(LexicalError {
                                        kind: LexicalErrorKind::InvalidToken,
                                        span,
                                    }));
                                }

                                expr_level -= 1;
                            }
                            Token::Semicolon => {
                                if expr_level == 0 {
                                    return Some(Ok((span.start, tok, span.end)));
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            },
            Scope::Function => {
                let mut expr_level = 1;

                let mut _current_spanned_tok = Some((Ok(tok), span));

                loop {
                    let (tok, span) = _current_spanned_tok.take().or_else(|| self.inner.next())?;

                    let tok = match tok {
                        Ok(tok) => tok,
                        Err(kind) => {
                            return Some(Err(LexicalError { kind, span }));
                        }
                    };

                    match &tok {
                        Token::OpenBrace => {
                            expr_level += 1;
                        }
                        Token::CloseBrace => {
                            expr_level -= 1;

                            if expr_level == 0 {
                                self.scope_stack.pop_back();
                                return Some(Ok((span.start, tok, span.end)));
                            }
                        }
                        _ => {}
                    }
                }
            }
            Scope::EnumVariant => {
                let mut expr_level = 1;

                let mut _current_spanned_tok = Some((Ok(tok), span));

                loop {
                    let (tok, span) = _current_spanned_tok.take().or_else(|| self.inner.next())?;

                    let tok = match tok {
                        Ok(tok) => tok,
                        Err(kind) => {
                            return Some(Err(LexicalError { kind, span }));
                        }
                    };

                    match &tok {
                        Token::OpenPth => {
                            expr_level += 1;
                        }
                        Token::ClosePth => {
                            expr_level -= 1;

                            if expr_level == 0 {
                                self.scope_stack.pop_back();
                                return Some(Ok((span.start, tok, span.end)));
                            }
                        }
                        _ => {}
                    }
                }
            }
        };

        Some(Ok((span.start, tok, span.end)))
    }
}
