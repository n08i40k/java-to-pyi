use bitflags::bitflags;
use ownable::traits::IntoOwned;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[allow(clippy::mutable_key_type)]
#[derive(Debug, Clone)]
pub struct ObjectCell<T>(Rc<RefCell<T>>);

impl<T> From<T> for ObjectCell<T> {
    fn from(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }
}

impl<T> From<Rc<RefCell<T>>> for ObjectCell<T> {
    fn from(value: Rc<RefCell<T>>) -> Self {
        Self(value)
    }
}

impl<T> Deref for ObjectCell<T> {
    type Target = RefCell<T>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<T> std::hash::Hash for ObjectCell<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0.deref().as_ptr(), state);
    }
}

impl<T> std::cmp::PartialEq for ObjectCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<T> std::cmp::Eq for ObjectCell<T> {}

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

        const DEFAULT       = 1 << 11;
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
    ResolvedClass(ClassCell),
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
            Self::ResolvedClass(arg0) => f
                .debug_tuple("ResolvedIdent")
                .field(&arg0.borrow().ident)
                .finish(),
        }
    }
}

impl TypeName {
    pub fn resolved_class(&self) -> Option<&ClassCell> {
        if let Self::ResolvedClass(class_cell) = self {
            Some(class_cell)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WildcardBoundary {
    None,
    Extends(QualifiedType),
    Super(QualifiedType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeGeneric {
    Type(QualifiedType),
    Wildcard(WildcardBoundary),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: TypeName,
    pub generics: Box<[TypeGeneric]>,
    pub array_depth: usize,
}

pub type QualifiedType = Box<[Type]>;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub modifiers: Modifiers,
    pub r#type: QualifiedType,
    pub ident: String,
    // TODO: may be initialisers?
}

impl Variable {
    pub fn new_array(
        modifiers: Modifiers,
        r#type: QualifiedType,
        idents: Box<[(Cow<'_, str>, usize)]>,
    ) -> Box<[Self]> {
        idents
            .into_iter()
            .map(|(ident, array_depth)| Self {
                modifiers: modifiers.clone(),
                r#type: {
                    let mut clone = r#type.clone();
                    clone.last_mut().unwrap().array_depth += array_depth;
                    clone
                },
                ident: ident.to_string(),
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericDefinition {
    pub ident: String,
    pub extends: Box<[QualifiedType]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgument {
    pub modifiers: Modifiers,
    pub r#type: QualifiedType,
    pub ident: String,
    pub vararg: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub modifiers: Modifiers,
    pub generics: Box<[GenericDefinition]>,
    pub return_type: QualifiedType,
    pub ident: String,
    pub arguments: Box<[FunctionArgument]>,
    // TODO: may be initialisers?
}

#[derive(Debug, Clone)]
pub(super) enum ClassEntry {
    Variables(Box<[Variable]>),
    Function(Function),
    Class(Class),
    Enum(Enum),
    Interface(Interface),
    Skip,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub modifiers: Modifiers,
    pub ident: String,
    pub generics: Box<[GenericDefinition]>,

    pub extends: Option<QualifiedType>,
    pub implements: Box<[QualifiedType]>,

    pub variables: Box<[Variable]>,
    pub functions: Box<[Function]>,

    pub classes: Box<[ClassCell]>,
    pub enums: Box<[EnumCell]>,
    pub interfaces: Box<[InterfaceCell]>,
}

pub type ClassCell = ObjectCell<Class>;

// impl std::fmt::Debug for Class {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("Class@{}", &self.ident))
//     }
// }

impl
    From<(
        Modifiers,
        Cow<'_, str>,
        Option<Box<[GenericDefinition]>>,
        Option<QualifiedType>,
        Option<Box<[QualifiedType]>>,
        Vec<ClassEntry>,
    )> for Class
{
    fn from(
        value: (
            Modifiers,
            Cow<'_, str>,
            Option<Box<[GenericDefinition]>>,
            Option<QualifiedType>,
            Option<Box<[QualifiedType]>>,
            Vec<ClassEntry>,
        ),
    ) -> Self {
        let (modifiers, ident, generics, extends, implements, entries) = value;

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        for entry in entries {
            match entry {
                ClassEntry::Variables(v) => variables.append(&mut v.into_vec()),
                ClassEntry::Function(f) => functions.push(f),
                ClassEntry::Class(c) => classes.push(c),
                ClassEntry::Enum(e) => enums.push(e),
                ClassEntry::Interface(i) => interfaces.push(i),
                ClassEntry::Skip => {}
            }
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            generics: generics.unwrap_or(Box::new([])),
            extends,
            implements: implements.unwrap_or(Box::from([])),
            variables: variables.into_boxed_slice(),
            functions: functions.into_boxed_slice(),
            classes: classes.into_iter().map(ClassCell::from).collect(),
            enums: enums.into_iter().map(EnumCell::from).collect(),
            interfaces: interfaces.into_iter().map(InterfaceCell::from).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum EnumEntry {
    Variables(Box<[Variable]>),
    Function(Function),
    Class(Class),
    Enum(Enum),
    Interface(Interface),
    Skip,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub modifiers: Modifiers,
    pub ident: String,
    pub generics: Box<[GenericDefinition]>,

    pub implements: Box<[QualifiedType]>,

    pub variables: Box<[Variable]>,
    pub functions: Box<[Function]>,

    pub classes: Box<[ClassCell]>,
    pub enums: Box<[EnumCell]>,
    pub interfaces: Box<[InterfaceCell]>,
}

pub type EnumCell = ObjectCell<Enum>;

// impl std::fmt::Debug for Enum {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("Enum@{}", &self.ident))
//     }
// }

impl
    From<(
        Modifiers,
        Cow<'_, str>,
        Option<Box<[GenericDefinition]>>,
        Option<Box<[QualifiedType]>>,
        Option<Box<[EnumEntry]>>,
    )> for Enum
{
    fn from(
        value: (
            Modifiers,
            Cow<'_, str>,
            Option<Box<[GenericDefinition]>>,
            Option<Box<[QualifiedType]>>,
            Option<Box<[EnumEntry]>>,
        ),
    ) -> Self {
        let (modifiers, ident, generics, implements, entries) = value;

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        if let Some(entries) = entries {
            for entry in entries {
                match entry {
                    EnumEntry::Variables(v) => variables.append(&mut v.into_vec()),
                    EnumEntry::Function(f) => functions.push(f),
                    EnumEntry::Class(c) => classes.push(c),
                    EnumEntry::Enum(e) => enums.push(e),
                    EnumEntry::Interface(i) => interfaces.push(i),
                    EnumEntry::Skip => {}
                }
            }
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            generics: generics.unwrap_or(Box::new([])),
            implements: implements.unwrap_or(Box::from([])),
            classes: classes.into_iter().map(ClassCell::from).collect(),
            enums: enums.into_iter().map(EnumCell::from).collect(),
            interfaces: interfaces.into_iter().map(InterfaceCell::from).collect(),
            variables: variables.into_boxed_slice(),
            functions: functions.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum InterfaceEntry {
    Variables(Box<[Variable]>),
    Function(Function),
    Class(Class),
    Enum(Enum),
    Interface(Interface),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub modifiers: Modifiers,
    pub ident: String,
    pub generics: Box<[GenericDefinition]>,

    pub extends: Box<[QualifiedType]>,

    pub variables: Box<[Variable]>,
    pub functions: Box<[Function]>,

    pub classes: Box<[ClassCell]>,
    pub enums: Box<[EnumCell]>,
    pub interfaces: Box<[InterfaceCell]>,
}

pub type InterfaceCell = ObjectCell<Interface>;

impl
    From<(
        Modifiers,
        Cow<'_, str>,
        Option<Box<[GenericDefinition]>>,
        Option<Box<[QualifiedType]>>,
        Vec<InterfaceEntry>,
    )> for Interface
{
    fn from(
        value: (
            Modifiers,
            Cow<'_, str>,
            Option<Box<[GenericDefinition]>>,
            Option<Box<[QualifiedType]>>,
            Vec<InterfaceEntry>,
        ),
    ) -> Self {
        let (modifiers, ident, generics, extends, entries) = value;

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        for entry in entries {
            match entry {
                InterfaceEntry::Variables(v) => variables.extend_from_slice(&v),
                InterfaceEntry::Function(f) => functions.push(f),
                InterfaceEntry::Class(c) => classes.push(c),
                InterfaceEntry::Enum(e) => enums.push(e),
                InterfaceEntry::Interface(i) => interfaces.push(i),
            }
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            extends: extends.unwrap_or(Box::new([])),
            variables: variables.into_boxed_slice(),
            generics: generics.unwrap_or(Box::new([])),
            functions: functions.into_boxed_slice(),
            classes: classes.into_iter().map(ClassCell::from).collect(),
            enums: enums.into_iter().map(EnumCell::from).collect(),
            interfaces: interfaces.into_iter().map(InterfaceCell::from).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum RootEntry {
    Class(Class),
    Enum(Enum),
    Interface(Interface),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub package: String,
    pub imports: Box<[String]>,
    pub classes: Box<[ClassCell]>,
    pub enums: Box<[EnumCell]>,
    pub interfaces: Box<[InterfaceCell]>,
}

impl From<(Cow<'_, str>, Vec<Cow<'_, str>>, Vec<RootEntry>)> for Root {
    fn from(value: (Cow<'_, str>, Vec<Cow<'_, str>>, Vec<RootEntry>)) -> Self {
        let (package, imports, entries) = value;

        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        for entry in entries {
            match entry {
                RootEntry::Class(c) => classes.push(c),
                RootEntry::Enum(e) => enums.push(e),
                RootEntry::Interface(i) => interfaces.push(i),
            }
        }

        Self {
            package: package.to_string(),
            imports: imports.into_iter().map(String::from).collect(),
            classes: classes.into_iter().map(ClassCell::from).collect(),
            enums: enums.into_iter().map(EnumCell::from).collect(),
            interfaces: interfaces.into_iter().map(InterfaceCell::from).collect(),
        }
    }
}

impl std::hash::Hash for Root {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state);
    }
}
