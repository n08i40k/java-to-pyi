use bitflags::bitflags;
use ownable::traits::IntoOwned;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

bitflags! {
    #[derive(Debug, Clone, PartialEq)]
    pub struct Modifiers: u16 {
        const PUBLIC        = 1 << 0;
        const PROTECTED     = 1 << 1;
        const PRIVATE       = 1 << 2;

        const STATIC        = 1 << 3;
        const FINAL         = 1 << 4;
        const ABSTRACT      = 1 << 5;

        const NATIVE        = 1 << 6;
        const SYNCHRONIZED  = 1 << 7;
        const TRANSIENT     = 1 << 8;
        const VOLATILE      = 1 << 9;
        const STRICTFP      = 1 << 10;
    }
}

impl IntoOwned for Modifiers {
    type Owned = Modifiers;

    fn into_owned(self) -> Self::Owned {
        self
    }
}

#[derive(Clone, PartialEq)]
pub enum TypeName {
    Void,
    Boolean,
    Char,
    Short,
    Integer,
    Long,
    Float,
    Double,
    Ident(String),
    ResolvedIdent(ClassCell),
}

impl std::fmt::Debug for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Boolean => write!(f, "Boolean"),
            Self::Char => write!(f, "Char"),
            Self::Short => write!(f, "Short"),
            Self::Integer => write!(f, "Integer"),
            Self::Long => write!(f, "Long"),
            Self::Float => write!(f, "Float"),
            Self::Double => write!(f, "Double"),
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::ResolvedIdent(arg0) => f
                .debug_tuple("ResolvedIdent")
                .field(&arg0.borrow().ident)
                .finish(),
        }
    }
}

impl TypeName {
    pub fn resolved_ident(&self) -> Option<&ClassCell> {
        if let Self::ResolvedIdent(class_cell) = self {
            Some(class_cell)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: TypeName,
    pub generics: Box<[Type]>,
    pub array: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub modifiers: Modifiers,
    pub r#type: Type,
    pub ident: String,
    // TODO: may be initialisers?
}

impl Variable {
    pub fn new_array(modifiers: Modifiers, r#type: Type, idents: Box<[&str]>) -> Box<[Self]> {
        idents
            .into_iter()
            .map(|ident| Self {
                modifiers: modifiers.clone(),
                r#type: r#type.clone(),
                ident: ident.to_string(),
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionGeneric {
    pub ident: String,
    pub extends: Box<[Type]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgument {
    pub r#type: Type,
    pub ident: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub modifiers: Modifiers,
    pub generics: Box<[FunctionGeneric]>,
    pub return_type: Type,
    pub ident: String,
    pub arguments: Box<[FunctionArgument]>,
    // TODO: may be initialisers?
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum ClassEntry {
    Class(Class),
    Variables(Box<[Variable]>),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub modifiers: Modifiers,
    pub ident: String,

    pub extends: Option<Type>,
    pub implements: Box<[Type]>,

    pub variables: Box<[Variable]>,
    pub functions: Box<[Function]>,
    pub classes: Box<[ClassCell]>,
}

// impl std::fmt::Debug for Class {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("Class@{}", &self.ident))
//     }
// }

impl
    From<(
        Modifiers,
        &str,
        Option<Type>,
        Option<Box<[Type]>>,
        Vec<ClassEntry>,
    )> for Class
{
    fn from(
        value: (
            Modifiers,
            &str,
            Option<Type>,
            Option<Box<[Type]>>,
            Vec<ClassEntry>,
        ),
    ) -> Self {
        let (modifiers, ident, extends, implements, entries) = value;

        let mut classes = Vec::new();
        let mut variables = Vec::new();
        let mut functions = Vec::new();

        for entry in entries {
            match entry {
                ClassEntry::Class(c) => classes.push(c),
                ClassEntry::Variables(v) => variables.append(&mut v.into_vec()),
                ClassEntry::Function(f) => functions.push(f),
            }
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            extends,
            implements: implements.unwrap_or(Box::from([])),
            variables: variables.into_boxed_slice(),
            functions: functions.into_boxed_slice(),
            classes: classes
                .into_iter()
                .map(|x| Rc::from(RefCell::from(x)).into())
                .collect::<Box<_>>(),
        }
    }
}

#[allow(clippy::mutable_key_type)]
#[derive(Debug, Clone)]
pub struct ClassCell(Rc<RefCell<Class>>);

impl From<Rc<RefCell<Class>>> for ClassCell {
    fn from(value: Rc<RefCell<Class>>) -> Self {
        Self(value)
    }
}

impl Deref for ClassCell {
    type Target = RefCell<Class>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl std::hash::Hash for ClassCell {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0.deref().as_ptr(), state);
    }
}

impl std::cmp::PartialEq for ClassCell {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl std::cmp::Eq for ClassCell {}

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub package: String,
    pub imports: Box<[String]>,
    pub classes: Box<[ClassCell]>,
}

impl std::hash::Hash for Root {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state);
    }
}

impl std::cmp::Eq for Root {}
