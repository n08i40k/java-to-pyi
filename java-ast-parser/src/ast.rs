use bitflags::bitflags;
use ownable::traits::IntoOwned;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub trait GetIdent {
    fn ident(&self) -> &str;
}

#[allow(clippy::mutable_key_type)]
#[derive(Debug, Clone)]
pub struct ObjectCell<T: GetIdent>(Rc<String>, Rc<RefCell<T>>);

impl<T: GetIdent> GetIdent for ObjectCell<T> {
    fn ident(&self) -> &str {
        self.0.as_str()
    }
}

impl<T: GetIdent> From<T> for ObjectCell<T> {
    fn from(value: T) -> Self {
        Self(
            Rc::new(value.ident().to_string()),
            Rc::new(RefCell::new(value)),
        )
    }
}

impl<T: GetIdent> From<Rc<RefCell<T>>> for ObjectCell<T> {
    fn from(value: Rc<RefCell<T>>) -> Self {
        let ident = Rc::new(value.borrow().ident().to_string());
        Self(ident, value)
    }
}

impl<T: GetIdent> Deref for ObjectCell<T> {
    type Target = RefCell<T>;

    fn deref(&self) -> &Self::Target {
        self.1.deref()
    }
}

impl<T: GetIdent> std::hash::Hash for ObjectCell<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.1.deref().as_ptr(), state);
    }
}

impl<T: GetIdent> std::cmp::PartialEq for ObjectCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.1.as_ptr() == other.1.as_ptr()
    }
}

impl<T: GetIdent> std::cmp::Eq for ObjectCell<T> {}

bitflags! {
    #[derive(Debug, Clone, PartialEq)]
    pub struct Modifiers: u16 {
        const PUBLIC        = 1 << 0;
        const PROTECTED     = 1 << 1;
        const PRIVATE       = 1 << 2;

        const STATIC        = 1 << 3;
        const FINAL         = 1 << 4;
        const SEALED        = 1 << 5;
        const NON_SEALED    = 1 << 6;
        const ABSTRACT      = 1 << 7;

        const NATIVE        = 1 << 8;
        const SYNCHRONIZED  = 1 << 9;
        const TRANSIENT     = 1 << 10;
        const VOLATILE      = 1 << 11;
        const STRICTFP      = 1 << 12;

        const DEFAULT       = 1 << 13;
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
    Byte,
    Char,
    Short,
    Integer,
    Long,
    Float,
    Double,
    Ident(String),
    ResolvedGeneric(String),
    ResolvedClass(ClassCell),
}

impl std::fmt::Debug for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Boolean => write!(f, "Boolean"),
            Self::Byte => write!(f, "Byte"),
            Self::Char => write!(f, "Char"),
            Self::Short => write!(f, "Short"),
            Self::Integer => write!(f, "Integer"),
            Self::Long => write!(f, "Long"),
            Self::Float => write!(f, "Float"),
            Self::Double => write!(f, "Double"),
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::ResolvedGeneric(arg0) => f.debug_tuple("ResolvedGeneric").field(arg0).finish(),
            Self::ResolvedClass(arg0) => f
                .debug_tuple("ResolvedIdent")
                .field(&arg0.borrow().ident)
                .finish(),
        }
    }
}

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Void => write!(f, "void"),
            TypeName::Boolean => write!(f, "boolean"),
            TypeName::Byte => write!(f, "byte"),
            TypeName::Char => write!(f, "char"),
            TypeName::Short => write!(f, "short"),
            TypeName::Integer => write!(f, "int"),
            TypeName::Long => write!(f, "long"),
            TypeName::Float => write!(f, "float"),
            TypeName::Double => write!(f, "double"),
            TypeName::Ident(ident) => write!(f, "{}", ident),
            TypeName::ResolvedGeneric(ident) => write!(f, "{}", ident),
            TypeName::ResolvedClass(class_cell) => write!(f, "{}", class_cell.ident()),
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

impl std::fmt::Display for WildcardBoundary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WildcardBoundary::None => write!(f, "?"),
            WildcardBoundary::Extends(items) => write!(
                f,
                "? extends {}",
                items
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
            WildcardBoundary::Super(items) => write!(
                f,
                "? super {}",
                items
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeGeneric {
    Type(QualifiedType),
    Wildcard(WildcardBoundary),
}

impl std::fmt::Display for TypeGeneric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeGeneric::Type(items) => write!(
                f,
                "{}",
                items
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
            TypeGeneric::Wildcard(wildcard_boundary) => wildcard_boundary.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: TypeName,
    pub generics: Box<[TypeGeneric]>,
    pub array_depth: usize,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.generics.is_empty() {
            write!(f, "{}{}", self.name, "[]".repeat(self.array_depth))
        } else {
            write!(
                f,
                "{}<{}>{}",
                self.name,
                self.generics
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                "[]".repeat(self.array_depth)
            )
        }
    }
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

impl std::fmt::Display for GenericDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.extends.is_empty() {
            write!(f, "{}", self.ident)
        } else {
            write!(
                f,
                "{} extends {}",
                self.ident,
                self.extends
                    .iter()
                    .map(|extend| extend
                        .iter()
                        .map(|part| part.to_string())
                        .collect::<Vec<_>>()
                        .join("."))
                    .collect::<Vec<_>>()
                    .join(".")
            )
        }
    }
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
    pub permits: Box<[QualifiedType]>,

    pub variables: Box<[Variable]>,
    pub functions: Box<[Function]>,

    pub classes: Box<[ClassCell]>,
    pub enums: Box<[EnumCell]>,
    pub interfaces: Box<[InterfaceCell]>,
}

impl GetIdent for Class {
    fn ident(&self) -> &str {
        self.ident.as_str()
    }
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
        Option<Box<[FunctionArgument]>>,
        Option<Box<[QualifiedType]>>,
        Option<Box<[ClassEntry]>>,
    )> for Class
{
    fn from(
        value: (
            Modifiers,
            Cow<'_, str>,
            Option<Box<[GenericDefinition]>>,
            Option<Box<[FunctionArgument]>>,
            Option<Box<[QualifiedType]>>,
            Option<Box<[ClassEntry]>>,
        ),
    ) -> Self {
        let (mut modifiers, ident, generics, args, implements, entries) = value;

        modifiers.insert(Modifiers::FINAL);

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        if let Some(entries) = entries {
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
        }

        functions.push(Function {
            modifiers: Modifiers::PUBLIC | Modifiers::STATIC,
            generics: Box::new([]),
            return_type: QualifiedType::from([Type {
                name: TypeName::Ident(ident.to_string()),
                generics: generics
                    .clone()
                    .unwrap_or_else(|| Box::new([]))
                    .into_iter()
                    .map(|x| {
                        TypeGeneric::Type(QualifiedType::from([Type {
                            name: TypeName::Ident(x.ident),
                            generics: Box::new([]),
                            array_depth: 0,
                        }]))
                    })
                    .collect::<Box<[_]>>(),
                array_depth: 0,
            }]),
            ident: "__ctor".to_string(),
            arguments: args.clone().unwrap_or_else(|| Box::new([])),
        });

        if let Some(args) = args {
            for mut arg in args {
                arg.modifiers.insert(Modifiers::PRIVATE | Modifiers::FINAL);

                if arg.vararg {
                    arg.r#type.last_mut().unwrap().array_depth += 1;
                }

                functions.push(Function {
                    modifiers: Modifiers::PUBLIC,
                    generics: Box::new([]),
                    return_type: arg.r#type.clone(),
                    ident: arg.ident.clone(),
                    arguments: Box::new([]),
                });

                variables.push(Variable {
                    modifiers: arg.modifiers,
                    r#type: arg.r#type,
                    ident: arg.ident,
                });
            }
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            generics: generics.unwrap_or(Box::new([])),
            extends: Some(Box::new([
                Type {
                    name: TypeName::Ident("java".to_string()),
                    generics: Box::new([]),
                    array_depth: 0,
                },
                Type {
                    name: TypeName::Ident("lang".to_string()),
                    generics: Box::new([]),
                    array_depth: 0,
                },
                Type {
                    name: TypeName::Ident("Record".to_string()),
                    generics: Box::new([]),
                    array_depth: 0,
                },
            ])),
            implements: implements.unwrap_or(Box::from([])),
            permits: Box::from([]),
            variables: variables.into_boxed_slice(),
            functions: functions.into_boxed_slice(),
            classes: classes.into_iter().map(ClassCell::from).collect(),
            enums: enums.into_iter().map(EnumCell::from).collect(),
            interfaces: interfaces.into_iter().map(InterfaceCell::from).collect(),
        }
    }
}

impl
    From<(
        Modifiers,
        Cow<'_, str>,
        Option<Box<[GenericDefinition]>>,
        Option<QualifiedType>,
        Option<Box<[QualifiedType]>>,
        Option<Box<[QualifiedType]>>,
        Option<Box<[ClassEntry]>>,
    )> for Class
{
    fn from(
        value: (
            Modifiers,
            Cow<'_, str>,
            Option<Box<[GenericDefinition]>>,
            Option<QualifiedType>,
            Option<Box<[QualifiedType]>>,
            Option<Box<[QualifiedType]>>,
            Option<Box<[ClassEntry]>>,
        ),
    ) -> Self {
        let (modifiers, ident, generics, extends, implements, permits, entries) = value;

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        if let Some(entries) = entries {
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
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            generics: generics.unwrap_or(Box::new([])),
            extends,
            implements: implements.unwrap_or(Box::from([])),
            permits: permits.unwrap_or(Box::from([])),
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

impl GetIdent for Enum {
    fn ident(&self) -> &str {
        self.ident.as_str()
    }
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
    pub permits: Box<[QualifiedType]>,

    pub variables: Box<[Variable]>,
    pub functions: Box<[Function]>,

    pub classes: Box<[ClassCell]>,
    pub enums: Box<[EnumCell]>,
    pub interfaces: Box<[InterfaceCell]>,
}

impl GetIdent for Interface {
    fn ident(&self) -> &str {
        self.ident.as_str()
    }
}

pub type InterfaceCell = ObjectCell<Interface>;

impl
    From<(
        Modifiers,
        Cow<'_, str>,
        Option<Box<[GenericDefinition]>>,
        Option<Box<[QualifiedType]>>,
        Option<Box<[QualifiedType]>>,
        Option<Box<[InterfaceEntry]>>,
    )> for Interface
{
    fn from(
        value: (
            Modifiers,
            Cow<'_, str>,
            Option<Box<[GenericDefinition]>>,
            Option<Box<[QualifiedType]>>,
            Option<Box<[QualifiedType]>>,
            Option<Box<[InterfaceEntry]>>,
        ),
    ) -> Self {
        let (modifiers, ident, generics, extends, permits, entries) = value;

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        if let Some(entries) = entries {
            for entry in entries {
                match entry {
                    InterfaceEntry::Variables(v) => variables.extend_from_slice(&v),
                    InterfaceEntry::Function(f) => functions.push(f),
                    InterfaceEntry::Class(c) => classes.push(c),
                    InterfaceEntry::Enum(e) => enums.push(e),
                    InterfaceEntry::Interface(i) => interfaces.push(i),
                }
            }
        }

        Self {
            modifiers,
            ident: ident.to_string(),
            extends: extends.unwrap_or(Box::new([])),
            permits: permits.unwrap_or(Box::new([])),
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

impl From<(Cow<'_, str>, Vec<Cow<'_, str>>, Option<Box<[RootEntry]>>)> for Root {
    fn from(value: (Cow<'_, str>, Vec<Cow<'_, str>>, Option<Box<[RootEntry]>>)) -> Self {
        let (package, mut imports, entries) = value;

        let mut classes = Vec::new();
        let mut enums = Vec::new();
        let mut interfaces = Vec::new();

        if let Some(entries) = entries {
            for entry in entries {
                match entry {
                    RootEntry::Class(c) => classes.push(c),
                    RootEntry::Enum(e) => enums.push(e),
                    RootEntry::Interface(i) => interfaces.push(i),
                }
            }
        }

        imports.push(Cow::from("java.lang.*"));

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
