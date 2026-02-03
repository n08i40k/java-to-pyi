use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

use java_ast_parser::ast::{
    self, ClassCell, Function, Modifiers, Root, Type, TypeGeneric, TypeName,
};

pub fn generate_pyi_by_package(
    roots: &[Rc<Root>],
    namespace_prefix: Option<&str>,
) -> HashMap<String, String> {
    let class_paths = Rc::new(collect_class_paths(roots, namespace_prefix));

    let mut roots_by_package: HashMap<String, Vec<Rc<Root>>> = HashMap::new();
    for root in roots {
        roots_by_package
            .entry(root.package.clone())
            .or_default()
            .push(root.clone());
    }

    let mut outputs = HashMap::new();
    for (package, package_roots) in roots_by_package {
        let type_params = collect_type_params(&package_roots);
        let mut emitter = PyiEmitter::new(type_params, class_paths.clone());

        emitter.emit_header();

        for root in &package_roots {
            for class_cell in &root.classes {
                emitter.emit_class(class_cell);
            }
        }

        outputs.insert(package, emitter.finish());
    }

    outputs
}

struct PyiEmitter {
    output: String,
    indent: usize,
    type_params: BTreeSet<String>,
    type_renderer: TypeRenderer,
}

impl PyiEmitter {
    fn new(type_params: BTreeSet<String>, class_paths: Rc<HashMap<ClassCell, String>>) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            type_params,
            type_renderer: TypeRenderer::new(class_paths),
        }
    }

    fn emit_header(&mut self) {
        self.line("from __future__ import annotations".to_string());
        self.line("from typing import Any, TypeVar, overload".to_string());
        self.blank_line();

        if !self.type_params.is_empty() {
            let params = self.type_params.iter().cloned().collect::<Vec<_>>();
            for type_param in params {
                self.line(format!("{} = TypeVar(\"{}\")", type_param, type_param));
            }
            self.blank_line();
        }
    }

    fn emit_class(&mut self, class_cell: &ClassCell) {
        let class = class_cell.borrow();
        let class_path = self.type_renderer.class_path(class_cell);
        let rendered_bases = collect_base_types(&class, &self.type_renderer);
        let bases = rendered_bases.bases;
        let bases_suffix = if bases.is_empty() {
            String::new()
        } else {
            format!("({})", bases.join(", "))
        };

        let mut class_line = format!("class {}{}:", class.ident, bases_suffix);
        if rendered_bases.has_unknown {
            class_line.push_str(&format!("  # unknown type used in {}", class_path));
        }
        self.line(class_line);
        self.indent += 1;

        let mut has_members = false;

        for variable in &class.variables {
            has_members = true;
            let rendered = self.type_renderer.render(&variable.r#type);
            let mut line = format!("{}: {}", variable.ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type used in {}.{}",
                    class_path, variable.ident
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&class.functions);
        for function_group in function_groups {
            let use_overload = function_group.len() > 1;
            for function in function_group {
                has_members = true;
                self.emit_function(function, use_overload, &class_path);
            }
        }

        for nested_class in &class.classes {
            has_members = true;
            self.emit_class(nested_class);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_function(&mut self, function: &Function, use_overload: bool, class_path: &str) {
        if use_overload {
            self.line("@overload".to_string());
        }

        let is_static = function.modifiers.intersects(Modifiers::STATIC);
        if is_static {
            self.line("@staticmethod".to_string());
        }

        let mut args = Vec::new();
        if !is_static {
            args.push("self".to_string());
        }

        let mut unknown_paths = BTreeSet::new();
        for argument in &function.arguments {
            let rendered = self.type_renderer.render(&argument.r#type);
            args.push(format!("{}: {}", argument.ident, rendered.text));
            if rendered.has_unknown() {
                unknown_paths.insert(format!(
                    "{}.{}.{}",
                    class_path, function.ident, argument.ident
                ));
            }
        }

        let rendered_return = self.type_renderer.render(&function.return_type);
        if rendered_return.has_unknown() {
            unknown_paths.insert(format!("{}.{}", class_path, function.ident));
        }

        let mut line = format!(
            "def {}({}) -> {}: ...",
            function.ident,
            args.join(", "),
            rendered_return.text
        );

        if !unknown_paths.is_empty() {
            let paths = unknown_paths.into_iter().collect::<Vec<_>>();
            let label = if paths.len() > 1 {
                "unknown types used in"
            } else {
                "unknown type used in"
            };
            line.push_str(&format!("  # {} {}", label, paths.join("; ")));
        }

        self.line(line);
    }

    fn line(&mut self, text: String) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
        self.output.push_str(&text);
        self.output.push('\n');
    }

    fn blank_line(&mut self) {
        self.output.push('\n');
    }

    fn finish(self) -> String {
        self.output
    }
}

fn group_functions<'a>(functions: &'a [Function]) -> Vec<Vec<&'a Function>> {
    let mut order: Vec<String> = Vec::new();
    let mut grouped: HashMap<String, Vec<&Function>> = HashMap::new();

    for function in functions {
        let name = function.ident.clone();
        if !grouped.contains_key(&name) {
            order.push(name.clone());
        }
        grouped.entry(name).or_default().push(function);
    }

    order
        .into_iter()
        .filter_map(|name| grouped.remove(&name))
        .collect()
}

struct RenderedBases {
    bases: Vec<String>,
    has_unknown: bool,
}

fn collect_base_types(class: &ast::Class, type_renderer: &TypeRenderer) -> RenderedBases {
    let mut bases = Vec::new();
    let mut has_unknown = false;

    if let Some(extends) = &class.extends {
        let rendered = type_renderer.render(extends);
        has_unknown |= rendered.has_unknown();
        bases.push(rendered.text);
    }

    for implemented in &class.implements {
        let rendered = type_renderer.render(implemented);
        has_unknown |= rendered.has_unknown();
        bases.push(rendered.text);
    }

    RenderedBases { bases, has_unknown }
}

fn collect_type_params(roots: &[Rc<Root>]) -> BTreeSet<String> {
    let mut type_params = BTreeSet::new();

    fn walk_class(type_params: &mut BTreeSet<String>, class_cell: &ClassCell) {
        let class = class_cell.borrow();

        for function in &class.functions {
            for generic in &function.generics {
                type_params.insert(generic.ident.clone());
            }
        }

        for nested in &class.classes {
            walk_class(type_params, nested);
        }
    }

    for root in roots {
        for class_cell in &root.classes {
            walk_class(&mut type_params, class_cell);
        }
    }

    type_params
}

struct TypeRenderer {
    class_paths: Rc<HashMap<ClassCell, String>>,
}

struct RenderedType {
    text: String,
    unknown_idents: Vec<String>,
}

impl RenderedType {
    fn known(text: String) -> Self {
        Self {
            text,
            unknown_idents: Vec::new(),
        }
    }

    fn unknown(text: String, ident: &str) -> Self {
        Self {
            text,
            unknown_idents: vec![ident.to_string()],
        }
    }

    fn has_unknown(&self) -> bool {
        !self.unknown_idents.is_empty()
    }
}

impl TypeRenderer {
    fn new(class_paths: Rc<HashMap<ClassCell, String>>) -> Self {
        Self { class_paths }
    }

    fn render_generic(&self, ty_gen: &TypeGeneric) -> RenderedType {
        match &ty_gen {
            TypeGeneric::Type(ty) => self.render(ty),
            TypeGeneric::Wildcard(_) => RenderedType {
                text: "Any".to_string(),
                unknown_idents: Vec::new(),
            },
        }
    }

    fn render(&self, ty: &Type) -> RenderedType {
        let mut rendered = self.render_non_array(ty);
        if ty.array {
            rendered.text = format!("list[{}]", rendered.text);
        }
        rendered
    }

    fn render_non_array(&self, ty: &Type) -> RenderedType {
        match &ty.name {
            TypeName::Ident(ident) => {
                let name_key = type_name_key(&ty.name);
                if let Some(collection) = name_key.as_deref().and_then(collection_kind) {
                    return self.render_collection(collection, &ty.generics);
                }

                if let Some(mapped) = map_boxed_type(ident) {
                    return RenderedType::known(mapped);
                }

                RenderedType::unknown("Any".to_string(), ident)
            }
            _ => {
                let name = self.render_type_name(&ty.name);
                if ty.generics.is_empty() {
                    return RenderedType::known(name);
                }

                let mut unknowns = Vec::new();
                let args = ty
                    .generics
                    .iter()
                    .map(|arg| {
                        let rendered = self.render_generic(arg);
                        unknowns.extend(rendered.unknown_idents);
                        rendered.text
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                RenderedType {
                    text: format!("{}[{}]", name, args),
                    unknown_idents: unknowns,
                }
            }
        }
    }

    fn render_type_name(&self, name: &TypeName) -> String {
        match name {
            TypeName::Void => "None".to_string(),
            TypeName::Boolean => "bool".to_string(),
            TypeName::Char => "str".to_string(),
            TypeName::Short | TypeName::Integer | TypeName::Long => "int".to_string(),
            TypeName::Float | TypeName::Double => "float".to_string(),
            TypeName::ResolvedIdent(class_cell) => self
                .class_paths
                .get(class_cell)
                .cloned()
                .unwrap_or_else(|| class_cell.borrow().ident.clone()),
            TypeName::Ident(ident) => map_boxed_type(ident).unwrap_or_else(|| ident.clone()),
        }
    }

    fn render_collection(
        &self,
        collection: CollectionKind,
        generics: &[TypeGeneric],
    ) -> RenderedType {
        let mut unknowns = Vec::new();
        let mut render_arg = |index: usize| -> String {
            let rendered = generics.get(index).map(|arg| self.render_generic(arg));
            match rendered {
                Some(rendered) => {
                    unknowns.extend(rendered.unknown_idents);
                    rendered.text
                }
                None => "Any".to_string(),
            }
        };

        let text = match collection {
            CollectionKind::List => format!("list[{}]", render_arg(0)),
            CollectionKind::Set => format!("set[{}]", render_arg(0)),
            CollectionKind::Map => format!("dict[{}, {}]", render_arg(0), render_arg(1)),
        };

        RenderedType {
            text,
            unknown_idents: unknowns,
        }
    }
}

impl TypeRenderer {
    fn class_path(&self, class_cell: &ClassCell) -> String {
        self.class_paths
            .get(class_cell)
            .cloned()
            .unwrap_or_else(|| class_cell.borrow().ident.clone())
    }
}

#[derive(Clone, Copy)]
enum CollectionKind {
    List,
    Set,
    Map,
}

fn collection_kind(type_name: &str) -> Option<CollectionKind> {
    match type_name {
        "List" | "ArrayList" | "LinkedList" | "Vector" => Some(CollectionKind::List),
        "Set" | "HashSet" | "LinkedHashSet" | "TreeSet" => Some(CollectionKind::Set),
        "Map" | "HashMap" | "LinkedHashMap" | "TreeMap" | "ConcurrentHashMap" => {
            Some(CollectionKind::Map)
        }
        _ => None,
    }
}

fn type_name_key(name: &TypeName) -> Option<String> {
    match name {
        TypeName::Ident(ident) => ident.rsplit('.').next().map(|v| v.to_string()),
        _ => None,
    }
}

fn collect_class_paths(
    roots: &[Rc<Root>],
    namespace_prefix: Option<&str>,
) -> HashMap<ClassCell, String> {
    let mut paths = HashMap::new();

    let prefix = normalize_namespace_prefix(namespace_prefix);

    fn walk_class(
        paths: &mut HashMap<ClassCell, String>,
        class_cell: &ClassCell,
        parent_path: Option<&str>,
    ) {
        let class = class_cell.borrow();
        let class_path = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, class.ident)
        } else {
            class.ident.clone()
        };

        paths.insert(class_cell.clone(), class_path.clone());

        for nested in &class.classes {
            walk_class(paths, nested, Some(&class_path));
        }
    }

    for root in roots {
        let package_prefix = match (prefix.as_deref(), root.package.is_empty()) {
            (Some(prefix), false) => Some(format!("{}.{}", prefix, root.package)),
            (Some(prefix), true) => Some(prefix.to_string()),
            (None, false) => Some(root.package.clone()),
            (None, true) => None,
        };

        for class_cell in &root.classes {
            walk_class(&mut paths, class_cell, package_prefix.as_deref());
        }
    }

    paths
}

fn normalize_namespace_prefix(namespace_prefix: Option<&str>) -> Option<String> {
    let prefix = namespace_prefix?.trim().trim_matches('.');
    if prefix.is_empty() {
        None
    } else {
        Some(prefix.to_string())
    }
}

fn map_boxed_type(ident: &str) -> Option<String> {
    let last_segment = ident.rsplit('.').next().unwrap_or(ident);
    match last_segment {
        "String" => Some("str".to_string()),
        "Object" => Some("Any".to_string()),
        "Integer" | "Long" | "Short" | "Byte" => Some("int".to_string()),
        "Boolean" => Some("bool".to_string()),
        "Float" | "Double" => Some("float".to_string()),
        "Character" => Some("str".to_string()),
        _ => None,
    }
}
