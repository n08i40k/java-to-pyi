//! Tokenization for Java sources.
//!
//! # Examples
//! ```rust
//! use java_ast_parser::lexer::{Lexer, Token};
//!
//! let mut lexer = Lexer::new("package com.example;");
//! let first = lexer.next().unwrap().unwrap();
//! assert_eq!(first.1, Token::KeywordPackage);
//! ```

use itertools::{Itertools, MultiPeek};
use logos::{Logos, Span, SpannedIter};
use ownable::IntoOwned;
use std::{
    borrow::Cow,
    cell::RefCell,
    collections::VecDeque,
    num::{ParseFloatError, ParseIntError},
    ops::{DerefMut, Range},
    rc::Rc,
};

use crate::java;

/// Categories of lexical errors produced by [`Lexer`].
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalErrorKind {
    #[default]
    InvalidToken,
    InvalidInteger(ParseIntError),
    InvalidFloat(ParseFloatError),
}

impl From<ParseIntError> for LexicalErrorKind {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidInteger(value)
    }
}

impl From<ParseFloatError> for LexicalErrorKind {
    fn from(value: ParseFloatError) -> Self {
        Self::InvalidFloat(value)
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

    #[regex(r"-?[0-9]+\.[0-9]+[fF]", |lex| lex.slice()[..lex.slice().len() - 1].parse())]
    Float(f32),

    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().parse())]
    Double(f64),

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
    #[token("@interface")]
    KeywordInterface,

    #[token("module")]
    KeywordModule,

    #[token("native")]
    KeywordNative,

    #[token("new")]
    KeywordNew,

    #[token("non-sealed")]
    KeywordNonSealed,

    #[token("package")]
    KeywordPackage,

    #[token("permits")]
    KeywordPermits,

    #[token("private")]
    KeywordPrivate,

    #[token("protected")]
    KeywordProtected,

    #[token("public")]
    KeywordPublic,

    #[token("record")]
    KeywordRecord,

    #[token("requires")]
    KeywordRequires,

    #[token("return")]
    KeywordReturn,

    #[token("sealed")]
    KeywordSealed,

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

    #[token("byte")]
    TypeByte,

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

    RecordCtor,

    #[regex(
        r#"'(?:\\u[0-9a-fA-F]{4}|\\[0-7]{1,3}|\\[btnfr"'\\]|[^'\\\r\n])'"#,
        string_from_lexer
    )]
    CharLiteral(Cow<'a, str>),

    #[regex(r#""([^"\\]*(?:\\.[^"\\]*)*)""#, string_from_lexer)]
    String(Cow<'a, str>),

    #[regex(r"[\p{L}_\$][\p{L}\p{Nd}_\$]*", |x| Cow::from(x.slice()), priority = 0)]
    Ident(Cow<'a, str>),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

enum Scope {
    Root,
    Object {
        skip_pths: bool,
        in_body: bool,
        is_record: bool,
        record_name: Option<String>,
    },
    Function,
    EnumVariant,
}

/// Streaming lexer that yields spanned tokens.
pub struct Lexer<'input> {
    input: &'input str,
    inner: MultiPeek<logos::SpannedIter<'input, Token<'input>>>,
    scope_stack: VecDeque<Rc<RefCell<Scope>>>,
}

impl<'input> Lexer<'input> {
    pub fn new(src: &'input str) -> Self {
        Self {
            input: src,
            inner: Token::lexer(src).spanned().multipeek(),
            scope_stack: VecDeque::from([Rc::from(RefCell::from(Scope::Root))]),
        }
    }
}

/// LALRPOP-compatible spanned token wrapper.
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

fn wait_until_bracket_end<'input>(
    mut curr_tok: Option<(Result<Token<'input>, LexicalErrorKind>, Range<usize>)>,
    iter: &mut MultiPeek<logos::SpannedIter<'input, Token<'input>>>,
) -> Option<Spanned<Token<'input>, usize, LexicalError>> {
    let mut expr_level = 1;

    loop {
        let (tok, span) = curr_tok.take().or_else(|| iter.next())?;

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
                    return Some(Ok((span.start, tok, span.end)));
                }
            }
            _ => {}
        }
    }
}

pub struct CommitPeek<'a, 'input> {
    it: &'a mut MultiPeek<SpannedIter<'input, Token<'input>>>,
    peeked: usize,
}

impl<'a, 'input> CommitPeek<'a, 'input> {
    pub fn new(iter: &'a mut MultiPeek<SpannedIter<'input, Token<'input>>>) -> Self {
        Self {
            it: iter,
            peeked: 0,
        }
    }

    pub fn commit_peek(
        &mut self,
    ) -> Option<(Result<Token<'input>, LexicalErrorKind>, Range<usize>)> {
        let mut r = None;

        for _ in 0..(self.peeked.max(1) - 1) {
            r = self.it.next();

            if let Some((Err(_), _)) = &r {
                return r;
            }
        }

        self.peeked = 0;

        r
    }
}

impl<'a, 'input> Iterator for CommitPeek<'a, 'input> {
    type Item = Result<(usize, Token<'input>, usize), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let out = self.it.peek();
        self.peeked += 1;

        out.map(|(tok, span)| {
            tok.clone()
                .map(|x| (span.start, x, span.end))
                .map_err(|kind| LexicalError {
                    kind,
                    span: span.clone(),
                })
        })
    }
}

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
                Token::KeywordClass
                | Token::KeywordInterface
                | Token::KeywordEnum
                | Token::KeywordRecord => {
                    if let Token::KeywordRecord = tok
                        && let Some((Ok(next_tok), _)) = self.inner.peek()
                        && !matches!(next_tok, Token::Ident(_))
                    {
                        return Some(Ok((span.start, tok, span.end)));
                    }

                    self.scope_stack
                        .push_back(Rc::from(RefCell::from(Scope::Object {
                            skip_pths: tok == Token::KeywordEnum,
                            in_body: false,
                            is_record: matches!(tok, Token::KeywordRecord),
                            record_name: None,
                        })));
                }
                _ => {}
            },
            Scope::Object {
                skip_pths,
                in_body,
                is_record,
                record_name,
            } => match &tok {
                Token::KeywordClass
                | Token::KeywordInterface
                | Token::KeywordEnum
                | Token::KeywordRecord => {
                    if let Token::KeywordRecord = tok
                        && let Some((Ok(next_tok), _)) = self.inner.peek()
                        && !matches!(next_tok, Token::Ident(_))
                    {
                        return Some(Ok((span.start, tok, span.end)));
                    }

                    self.scope_stack
                        .push_back(Rc::from(RefCell::from(Scope::Object {
                            skip_pths: tok == Token::KeywordEnum,
                            in_body: false,
                            is_record: matches!(tok, Token::KeywordRecord),
                            record_name: None,
                        })));
                }
                Token::Ident(ident) => {
                    if *is_record {
                        if record_name.is_none() {
                            *record_name = Some(ident.to_string());
                        } else if record_name.as_ref().unwrap().as_str() == ident
                            && matches!(self.inner.peek(), Some((Ok(Token::OpenBrace), _)))
                        {
                            return Some(Ok((span.start, Token::RecordCtor, span.end)));
                        }
                    }
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
                Token::ClosePth => {
                    let Some((Ok(Token::KeywordDefault), _)) = self.inner.peek() else {
                        return Some(Ok((span.start, tok, span.end)));
                    };

                    let mut expr_level = 0;

                    loop {
                        if let Some((Ok(Token::Semicolon), _)) = self.inner.peek() {
                            return Some(Ok((span.start, tok, span.end)));
                        }

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
                            Token::Ident(_) => {
                                if !matches!(self.inner.peek(), Some((Ok(Token::OpenAngle), _))) {
                                    continue;
                                }

                                self.inner.reset_peek();

                                //skip generics
                                let generics_parser = java::TypeGenericsParser::new();
                                let mut commit_iter = CommitPeek::new(&mut self.inner);

                                let result = generics_parser.parse(self.input, &mut commit_iter);

                                if result.is_ok()
                                    || matches!(
                                        result,
                                        Err(lalrpop_util::ParseError::ExtraToken { .. })
                                    )
                                {
                                    commit_iter.commit_peek();
                                }

                                if let Err(lalrpop_util::ParseError::UnrecognizedToken {
                                    expected,
                                    ..
                                }) = result
                                    && expected.is_empty()
                                {
                                    commit_iter.commit_peek();
                                }
                            }
                            Token::OpenBrace => {
                                let _ = wait_until_bracket_end(None, &mut self.inner)?;
                            }
                            Token::OpenPth | Token::OpenBracket => {
                                expr_level += 1;
                            }
                            Token::ClosePth | Token::CloseBracket => {
                                if expr_level == 0 {
                                    return Some(Err(LexicalError {
                                        kind: LexicalErrorKind::InvalidToken,
                                        span,
                                    }));
                                }

                                expr_level -= 1;
                            }
                            Token::Semicolon | Token::Comma => {
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
                self.scope_stack.pop_back();

                return wait_until_bracket_end(Some((Ok(tok), span)), &mut self.inner);
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
