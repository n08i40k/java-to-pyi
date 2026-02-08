use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

use java_ast_parser::ast::{
    self, ClassCell, EnumCell, Function, InterfaceCell, Modifiers, QualifiedType, Root,
    TypeGeneric, TypeName, WildcardBoundary,
};
use crate::status;

trait QualifiedTypeFormat {
    fn fmt(&self) -> String;
}

impl QualifiedTypeFormat for QualifiedType {
    fn fmt(&self) -> String {
        self.iter()
            .map(|x| x.to_string())
            .collect::<Box<[_]>>()
            .join(".")
    }
}

pub fn generate_pyi_by_package(roots: &[Rc<Root>]) -> HashMap<String, String> {
    let definition_paths = Rc::new(collect_definition_paths(roots));
    let class_paths = Rc::new(definition_paths.class_paths.clone());

    let mut roots_by_package: HashMap<String, Vec<Rc<Root>>> = HashMap::new();
    for root in roots {
        roots_by_package
            .entry(root.package.clone())
            .or_default()
            .push(root.clone());
    }

    let mut outputs = HashMap::new();
    let total_packages = roots_by_package.len();
    status::update(&format!("Serializing 0/{}", total_packages));
    for (index, (package, package_roots)) in roots_by_package.into_iter().enumerate() {
        let label = if package.is_empty() {
            "<root>".to_string()
        } else {
            package.clone()
        };
        status::update(&format!(
            "Serializing {}/{}: {}",
            index + 1,
            total_packages,
            label
        ));
        let module_imports = collect_module_imports(&package_roots, &definition_paths);
        let mut emitter = PyiEmitter::new(
            class_paths.clone(),
            definition_paths.clone(),
            module_imports,
        );

        emitter.emit_header();

        let empty_type_params = BTreeSet::new();
        for root in &package_roots {
            for class_cell in &root.classes {
                emitter.emit_class(class_cell, &empty_type_params);
            }
            for interface_cell in &root.interfaces {
                emitter.emit_interface(interface_cell, &empty_type_params);
            }
            for enum_cell in &root.enums {
                emitter.emit_enum(enum_cell, &empty_type_params);
            }
        }

        outputs.insert(package, emitter.finish());
    }

    outputs
}

struct PyiEmitter {
    output: String,
    indent: usize,
    type_renderer: TypeRenderer,
    definition_paths: Rc<DefinitionPaths>,
    module_imports: BTreeSet<String>,
}

impl PyiEmitter {
    fn new(
        class_paths: Rc<HashMap<ClassCell, String>>,
        definition_paths: Rc<DefinitionPaths>,
        module_imports: BTreeSet<String>,
    ) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            type_renderer: TypeRenderer::new(
                class_paths,
                Rc::new(definition_paths.interface_paths.clone()),
            ),
            definition_paths,
            module_imports,
        }
    }

    fn emit_header(&mut self) {
        self.line("from __future__ import annotations".to_string());
        let module_imports = self.module_imports.iter().cloned().collect::<Vec<_>>();
        for module_import in module_imports {
            self.line(format!("import {}", module_import));
        }
        self.line("from typing import Any, overload".to_string());
        self.blank_line();
    }

    fn emit_class(&mut self, class_cell: &ClassCell, outer_type_params: &BTreeSet<String>) {
        let class = class_cell.borrow();
        let class_type_params = extend_type_params(outer_type_params, &class.generics);
        let type_params_suffix = format_type_params(&class.generics);
        let class_path = self.definition_paths.class_path(class_cell);
        let mut rendered_bases =
            collect_class_base_types(&class, &self.type_renderer, &class_type_params);
        if let Some(special_base) = java_stdlib_python_base(&class_path, &class.generics) {
            if !rendered_bases.bases.iter().any(|base| base == &special_base) {
                rendered_bases.bases.insert(0, special_base);
            }
        }
        let bases_suffix = if rendered_bases.bases.is_empty() {
            String::new()
        } else {
            format!("({})", rendered_bases.bases.join(", "))
        };

        let mut line = format!("class {}{}{}:", class.ident, type_params_suffix, bases_suffix);
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                class_path
            ));
        }
        self.line(line);
        self.indent += 1;

        let mut has_members = false;
        let has_explicit_init = class.functions.iter().any(|function| function.ident == "__init__");
        if !has_explicit_init {
            has_members = true;
            self.line("def __init__(self, *args: Any, **kwargs: Any) -> None: ...".to_string());
        }

        for variable in &class.variables {
            has_members = true;
            let rendered = self.type_renderer.render(&variable.r#type, &class_type_params);
            let ident = sanitize_ident(&variable.ident);
            let mut line = format!("{}: {}", ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    class_path,
                    variable.ident
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&class.functions);
        for function_group in function_groups {
            let filtered = function_group
                .into_iter()
                .filter(|function| {
                    function.ident == "__ctor" || !function.modifiers.intersects(Modifiers::STATIC)
                })
                .collect::<Vec<_>>();
            if filtered.is_empty() {
                continue;
            }
            let use_overload = filtered.len() > 1;
            for function in filtered {
                has_members = true;
                let function_type_params =
                    extend_type_params(&class_type_params, &function.generics);
                self.emit_function(
                    function,
                    use_overload,
                    &class_path,
                    &function_type_params,
                );
            }
        }

        for nested_class in &class.classes {
            has_members = true;
            self.emit_class(nested_class, &class_type_params);
        }
        for nested_interface in &class.interfaces {
            has_members = true;
            self.emit_interface(nested_interface, &class_type_params);
        }
        for nested_enum in &class.enums {
            has_members = true;
            self.emit_enum(nested_enum, &class_type_params);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_interface(
        &mut self,
        interface_cell: &InterfaceCell,
        outer_type_params: &BTreeSet<String>,
    ) {
        let interface = interface_cell.borrow();
        let interface_type_params = extend_type_params(outer_type_params, &interface.generics);
        let type_params_suffix = format_type_params(&interface.generics);
        let interface_path = self.definition_paths.interface_path(interface_cell);
        let mut rendered_bases = collect_interface_base_types(
            &interface,
            &self.type_renderer,
            &interface_type_params,
        );
        if let Some(special_base) = java_stdlib_python_base(&interface_path, &interface.generics) {
            if !rendered_bases.bases.iter().any(|base| base == &special_base) {
                rendered_bases.bases.insert(0, special_base);
            }
        }
        let bases_suffix = if rendered_bases.bases.is_empty() {
            String::new()
        } else {
            format!("({})", rendered_bases.bases.join(", "))
        };

        let mut line =
            format!("class {}{}{}:", interface.ident, type_params_suffix, bases_suffix);
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                interface_path
            ));
        }
        self.line(line);
        self.indent += 1;

        let mut has_members = false;

        for variable in &interface.variables {
            has_members = true;
            let rendered = self
                .type_renderer
                .render(&variable.r#type, &interface_type_params);
            let ident = sanitize_ident(&variable.ident);
            let mut line = format!("{}: {}", ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    interface_path,
                    variable.ident
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&interface.functions);
        for function_group in function_groups {
            let filtered = function_group
                .into_iter()
                .filter(|function| {
                    function.ident == "__ctor" || !function.modifiers.intersects(Modifiers::STATIC)
                })
                .collect::<Vec<_>>();
            if filtered.is_empty() {
                continue;
            }
            let use_overload = filtered.len() > 1;
            for function in filtered {
                has_members = true;
                let function_type_params =
                    extend_type_params(&interface_type_params, &function.generics);
                self.emit_function(
                    function,
                    use_overload,
                    &interface_path,
                    &function_type_params,
                );
            }
        }

        for nested_class in &interface.classes {
            has_members = true;
            self.emit_class(nested_class, &interface_type_params);
        }
        for nested_interface in &interface.interfaces {
            has_members = true;
            self.emit_interface(nested_interface, &interface_type_params);
        }
        for nested_enum in &interface.enums {
            has_members = true;
            self.emit_enum(nested_enum, &interface_type_params);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_enum(&mut self, enum_cell: &EnumCell, outer_type_params: &BTreeSet<String>) {
        let r#enum = enum_cell.borrow();
        let enum_type_params = extend_type_params(outer_type_params, &r#enum.generics);
        let type_params_suffix = format_type_params(&r#enum.generics);
        let enum_path = self.definition_paths.enum_path(enum_cell);
        let rendered_bases =
            collect_enum_base_types(&r#enum, &self.type_renderer, &enum_type_params);
        let bases = rendered_bases.bases;
        let bases_suffix = if bases.is_empty() {
            String::new()
        } else {
            format!("({})", bases.join(", "))
        };

        let mut line = format!("class {}{}{}:", r#enum.ident, type_params_suffix, bases_suffix);
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                enum_path
            ));
        }
        self.line(line);
        self.indent += 1;

        let mut has_members = false;

        for variable in &r#enum.variables {
            has_members = true;
            let rendered = self.type_renderer.render(&variable.r#type, &enum_type_params);
            let ident = sanitize_ident(&variable.ident);
            let mut line = format!("{}: {}", ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    enum_path,
                    variable.ident
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&r#enum.functions);
        for function_group in function_groups {
            let filtered = function_group
                .into_iter()
                .filter(|function| {
                    function.ident == "__ctor" || !function.modifiers.intersects(Modifiers::STATIC)
                })
                .collect::<Vec<_>>();
            if filtered.is_empty() {
                continue;
            }
            let use_overload = filtered.len() > 1;
            for function in filtered {
                has_members = true;
                let function_type_params = extend_type_params(&enum_type_params, &function.generics);
                self.emit_function(function, use_overload, &enum_path, &function_type_params);
            }
        }

        for nested_class in &r#enum.classes {
            has_members = true;
            self.emit_class(nested_class, &enum_type_params);
        }
        for nested_interface in &r#enum.interfaces {
            has_members = true;
            self.emit_interface(nested_interface, &enum_type_params);
        }
        for nested_enum in &r#enum.enums {
            has_members = true;
            self.emit_enum(nested_enum, &enum_type_params);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_function(
        &mut self,
        function: &Function,
        use_overload: bool,
        class_path: &str,
        type_params: &BTreeSet<String>,
    ) {
        if use_overload {
            self.line("@overload".to_string());
        }

        let is_static =
            function.ident == "__ctor" || function.modifiers.intersects(Modifiers::STATIC);
        if is_static {
            self.line("@staticmethod".to_string());
        }

        let mut args = Vec::new();
        if !is_static {
            args.push("self".to_string());
        }

        let mut unknown_paths = HashMap::new();
        for argument in &function.arguments {
            let rendered = self.type_renderer.render(&argument.r#type, type_params);
            let arg_prefix = if argument.vararg { "*" } else { "" };
            let ident = sanitize_ident(&argument.ident);
            args.push(format!("{}{}: {}", arg_prefix, ident, rendered.text));
            if rendered.has_unknown() {
                unknown_paths.insert(
                    format!("{}.{}.{}", class_path, function.ident, argument.ident),
                    rendered.unknown,
                );
            }
        }

        let rendered_return = self.type_renderer.render(&function.return_type, type_params);
        if rendered_return.has_unknown() {
            unknown_paths.insert(
                format!("{}.{}", class_path, function.ident),
                rendered_return.unknown,
            );
        }

        let type_params_suffix = format_type_params(&function.generics);
        let mut line = format!(
            "def {}{}({}) -> {}: ...",
            function.ident,
            type_params_suffix,
            args.join(", "),
            rendered_return.text
        );

        if !unknown_paths.is_empty() {
            let paths = unknown_paths.into_iter().collect::<Vec<_>>();
            line.push_str(&format!(
                "  # unknown type(s) used in {}",
                paths
                    .into_iter()
                    .map(|(k, v)| format!("{} -> [{}]", k, v.join(", ")))
                    .collect::<Box<[_]>>()
                    .join("; ")
            ));
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

fn group_functions(functions: &[Function]) -> Vec<Vec<&Function>> {
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

fn sanitize_ident(ident: &str) -> String {
    if is_python_keyword(ident) {
        format!("{}_", ident)
    } else {
        ident.to_string()
    }
}

fn is_python_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "False"
            | "None"
            | "True"
            | "and"
            | "as"
            | "assert"
            | "async"
            | "await"
            | "break"
            | "class"
            | "continue"
            | "def"
            | "del"
            | "elif"
            | "else"
            | "except"
            | "finally"
            | "for"
            | "from"
            | "global"
            | "if"
            | "import"
            | "in"
            | "is"
            | "lambda"
            | "match"
            | "nonlocal"
            | "not"
            | "or"
            | "pass"
            | "raise"
            | "return"
            | "try"
            | "while"
            | "with"
            | "yield"
    )
}

fn collect_module_imports(
    roots: &[Rc<Root>],
    definition_paths: &DefinitionPaths,
) -> BTreeSet<String> {
    let mut modules = BTreeSet::new();

    fn add_module(
        modules: &mut BTreeSet<String>,
        definition_paths: &DefinitionPaths,
        class_cell: &ClassCell,
    ) {
        if let Some(module_path) = definition_paths.class_module(class_cell)
            && !module_path.is_empty()
        {
            modules.insert(module_path.to_string());
        }
    }

    fn add_interface_module(
        modules: &mut BTreeSet<String>,
        definition_paths: &DefinitionPaths,
        interface_cell: &InterfaceCell,
    ) {
        if let Some(module_path) = definition_paths.interface_module(interface_cell)
            && !module_path.is_empty()
        {
            modules.insert(module_path.to_string());
        }
    }

    fn collect_from_generic(
        generic: &TypeGeneric,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
    ) {
        match generic {
            TypeGeneric::Type(r#type) => collect_from_type(r#type, definition_paths, modules),
            TypeGeneric::Wildcard(boundary) => match boundary {
                WildcardBoundary::None => {}
                WildcardBoundary::Extends(bound) | WildcardBoundary::Super(bound) => {
                    collect_from_type(bound, definition_paths, modules);
                }
            },
        }
    }

    fn collect_from_type(
        r#type: &QualifiedType,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
    ) {
        for part in r#type {
            match &part.name {
                TypeName::ResolvedClass(class_cell) => {
                    add_module(modules, definition_paths, class_cell);
                }
                TypeName::ResolvedInterface(interface_cell) => {
                    add_interface_module(modules, definition_paths, interface_cell);
                }
                _ => {}
            }

            for generic in &part.generics {
                collect_from_generic(generic, definition_paths, modules);
            }
        }
    }

    fn collect_from_function(
        function: &Function,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
    ) {
        collect_from_type(&function.return_type, definition_paths, modules);
        for argument in &function.arguments {
            collect_from_type(&argument.r#type, definition_paths, modules);
        }
    }

    fn collect_from_class(
        class_cell: &ClassCell,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
    ) {
        let class = class_cell.borrow();

        if let Some(extends) = &class.extends {
            collect_from_type(extends, definition_paths, modules);
        }
        for implemented in &class.implements {
            collect_from_type(implemented, definition_paths, modules);
        }

        for variable in &class.variables {
            collect_from_type(&variable.r#type, definition_paths, modules);
        }

        for function in &class.functions {
            collect_from_function(function, definition_paths, modules);
        }

        for nested in &class.classes {
            collect_from_class(nested, definition_paths, modules);
        }
        for nested in &class.interfaces {
            collect_from_interface(nested, definition_paths, modules);
        }
        for nested in &class.enums {
            collect_from_enum(nested, definition_paths, modules);
        }
    }

    fn collect_from_interface(
        interface_cell: &InterfaceCell,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
    ) {
        let interface = interface_cell.borrow();

        for extend in &interface.extends {
            collect_from_type(extend, definition_paths, modules);
        }

        for variable in &interface.variables {
            collect_from_type(&variable.r#type, definition_paths, modules);
        }

        for function in &interface.functions {
            collect_from_function(function, definition_paths, modules);
        }

        for nested in &interface.classes {
            collect_from_class(nested, definition_paths, modules);
        }
        for nested in &interface.interfaces {
            collect_from_interface(nested, definition_paths, modules);
        }
        for nested in &interface.enums {
            collect_from_enum(nested, definition_paths, modules);
        }
    }

    fn collect_from_enum(
        enum_cell: &EnumCell,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
    ) {
        let r#enum = enum_cell.borrow();

        for implemented in &r#enum.implements {
            collect_from_type(implemented, definition_paths, modules);
        }

        for variable in &r#enum.variables {
            collect_from_type(&variable.r#type, definition_paths, modules);
        }

        for function in &r#enum.functions {
            collect_from_function(function, definition_paths, modules);
        }

        for nested in &r#enum.classes {
            collect_from_class(nested, definition_paths, modules);
        }
        for nested in &r#enum.interfaces {
            collect_from_interface(nested, definition_paths, modules);
        }
        for nested in &r#enum.enums {
            collect_from_enum(nested, definition_paths, modules);
        }
    }

    for root in roots {
        for class_cell in &root.classes {
            collect_from_class(class_cell, definition_paths, &mut modules);
        }
        for interface_cell in &root.interfaces {
            collect_from_interface(interface_cell, definition_paths, &mut modules);
        }
        for enum_cell in &root.enums {
            collect_from_enum(enum_cell, definition_paths, &mut modules);
        }
    }

    modules
}

struct RenderedBases {
    bases: Vec<String>,
    unknown: Box<[String]>,
}

fn generic_ident_or_any(generics: &[ast::GenericDefinition], index: usize) -> String {
    generics
        .get(index)
        .map(|generic| generic.ident.clone())
        .unwrap_or_else(|| "Any".to_string())
}

fn java_stdlib_python_base(
    definition_path: &str,
    generics: &[ast::GenericDefinition],
) -> Option<String> {
    match definition_path {
        "java.util.Map" => {
            let key = generic_ident_or_any(generics, 0);
            let value = generic_ident_or_any(generics, 1);
            Some(format!("dict[{}, {}]", key, value))
        }
        "java.util.List" => Some(format!("list[{}]", generic_ident_or_any(generics, 0))),
        "java.util.Set" => Some(format!("set[{}]", generic_ident_or_any(generics, 0))),
        "java.lang.Boolean" => Some("bool".to_string()),
        "java.lang.Integer" | "java.lang.Byte" | "java.lang.Long" | "java.lang.Short" => {
            Some("int".to_string())
        }
        "java.lang.Double" | "java.lang.Float" => Some("float".to_string()),
        "java.lang.String" => Some("str".to_string()),
        _ => None,
    }
}

fn collect_class_base_types(
    class: &ast::Class,
    type_renderer: &TypeRenderer,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases = Vec::new();
    let mut unknown = Vec::new();

    if let Some(extends) = &class.extends {
        let rendered = type_renderer.render(extends, type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    for implemented in &class.implements {
        let rendered = type_renderer.render(implemented, type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn collect_interface_base_types(
    interface: &ast::Interface,
    type_renderer: &TypeRenderer,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases = Vec::new();
    let mut unknown = Vec::new();

    for extend in &interface.extends {
        let rendered = type_renderer.render(extend, type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn collect_enum_base_types(
    r#enum: &ast::Enum,
    type_renderer: &TypeRenderer,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases = Vec::new();
    let mut unknown = Vec::new();

    for implemented in &r#enum.implements {
        let rendered = type_renderer.render(implemented, type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn extend_type_params(
    base: &BTreeSet<String>,
    generics: &[ast::GenericDefinition],
) -> BTreeSet<String> {
    let mut combined = base.clone();
    for generic in generics {
        combined.insert(generic.ident.clone());
    }
    combined
}

fn format_type_params(generics: &[ast::GenericDefinition]) -> String {
    if generics.is_empty() {
        return String::new();
    }

    let params = generics
        .iter()
        .map(|generic| generic.ident.clone())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", params)
}

struct TypeRenderer {
    class_paths: Rc<HashMap<ClassCell, String>>,
    interface_paths: Rc<HashMap<InterfaceCell, String>>,
}

struct RenderedType {
    text: String,
    unknown: Box<[String]>,
}

impl RenderedType {
    fn known(text: String) -> Self {
        Self {
            text,
            unknown: Box::from([]),
        }
    }

    fn unknown(qty: &QualifiedType) -> Self {
        Self {
            text: "Any".to_string(),
            unknown: Box::from([qty.fmt()]),
        }
    }

    fn has_unknown(&self) -> bool {
        !self.unknown.is_empty()
    }
}

impl TypeRenderer {
    fn new(
        class_paths: Rc<HashMap<ClassCell, String>>,
        interface_paths: Rc<HashMap<InterfaceCell, String>>,
    ) -> Self {
        Self {
            class_paths,
            interface_paths,
        }
    }

    fn render_generic(&self, ty_gen: &TypeGeneric, type_params: &BTreeSet<String>) -> RenderedType {
        match &ty_gen {
            TypeGeneric::Type(ty) => self.render(ty, type_params),
            TypeGeneric::Wildcard(boundary) => match boundary {
                WildcardBoundary::None => RenderedType::known("Any".to_string()),
                WildcardBoundary::Extends(bound) | WildcardBoundary::Super(bound) => {
                    let rendered = self.render(bound, type_params);
                    RenderedType {
                        text: "Any".to_string(),
                        unknown: rendered.unknown,
                    }
                }
            },
        }
    }

    fn render(&self, qty: &QualifiedType, type_params: &BTreeSet<String>) -> RenderedType {
        let Some(last) = qty.last() else {
            return RenderedType::unknown(qty);
        };

        let mut rendered = self.render_type(qty, type_params);
        if last.array_depth > 0 {
            for _ in 0..last.array_depth {
                rendered.text = format!("list[{}]", rendered.text);
            }
        }

        rendered
    }

    fn render_type(&self, qty: &QualifiedType, type_params: &BTreeSet<String>) -> RenderedType {
        let ty = qty.last().unwrap();

        match &ty.name {
            TypeName::Ident(ident) => {
                if type_params.contains(ident) {
                    RenderedType::known(ident.clone())
                } else {
                    RenderedType::unknown(qty)
                }
            }
            TypeName::ResolvedGeneric(ident) => {
                self.render_named_type(ident.clone(), &ty.generics, type_params)
            }
            _ => {
                let name = self.render_type_name(&ty.name);
                self.render_named_type(name, &ty.generics, type_params)
            }
        }
    }

    fn render_named_type(
        &self,
        base: String,
        generics: &[TypeGeneric],
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        if generics.is_empty() {
            return RenderedType::known(base);
        }

        let mut unknown = Vec::new();
        let args = generics
            .iter()
            .map(|arg| {
                let rendered = self.render_generic(arg, type_params);
                unknown.extend(rendered.unknown);
                rendered.text
            })
            .collect::<Vec<_>>()
            .join(", ");

        RenderedType {
            text: format!("{}[{}]", base, args),
            unknown: unknown.into_boxed_slice(),
        }
    }

    fn render_type_name(&self, name: &TypeName) -> String {
        match name {
            TypeName::Void => "None".to_string(),
            TypeName::Boolean => "bool".to_string(),
            TypeName::Byte => "int".to_string(),
            TypeName::Char => "str".to_string(),
            TypeName::Short | TypeName::Integer | TypeName::Long => "int".to_string(),
            TypeName::Float | TypeName::Double => "float".to_string(),
            TypeName::ResolvedClass(class_cell) => self
                .class_paths
                .get(class_cell)
                .cloned()
                .unwrap_or_else(|| class_cell.borrow().ident.clone()),
            TypeName::ResolvedInterface(interface_cell) => self
                .interface_paths
                .get(interface_cell)
                .cloned()
                .unwrap_or_else(|| interface_cell.borrow().ident.clone()),
            TypeName::ResolvedGeneric(ident) => ident.clone(),
            TypeName::Ident(ident) => ident.clone(),
        }
    }
}

fn collect_definition_paths(roots: &[Rc<Root>]) -> DefinitionPaths {
    let mut paths = DefinitionPaths {
        class_paths: HashMap::new(),
        class_modules: HashMap::new(),
        enum_paths: HashMap::new(),
        interface_paths: HashMap::new(),
        interface_modules: HashMap::new(),
    };

    fn walk_class(
        paths: &mut DefinitionPaths,
        class_cell: &ClassCell,
        parent_path: Option<&str>,
        module_path: Option<&str>,
    ) {
        let class = class_cell.borrow();
        let class_path = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, class.ident)
        } else {
            class.ident.clone()
        };

        paths
            .class_paths
            .insert(class_cell.clone(), class_path.clone());

        if let Some(module_path) = module_path
            && !module_path.is_empty()
        {
            paths
                .class_modules
                .insert(class_cell.clone(), module_path.to_string());
        }

        for nested in &class.classes {
            walk_class(paths, nested, Some(&class_path), module_path);
        }
        for nested in &class.interfaces {
            walk_interface(paths, nested, Some(&class_path), module_path);
        }
        for nested in &class.enums {
            walk_enum(paths, nested, Some(&class_path), module_path);
        }
    }

    fn walk_interface(
        paths: &mut DefinitionPaths,
        interface_cell: &InterfaceCell,
        parent_path: Option<&str>,
        module_path: Option<&str>,
    ) {
        let interface = interface_cell.borrow();
        let interface_path = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, interface.ident)
        } else {
            interface.ident.clone()
        };

        paths
            .interface_paths
            .insert(interface_cell.clone(), interface_path.clone());

        if let Some(module_path) = module_path
            && !module_path.is_empty()
        {
            paths
                .interface_modules
                .insert(interface_cell.clone(), module_path.to_string());
        }

        for nested in &interface.classes {
            walk_class(paths, nested, Some(&interface_path), module_path);
        }
        for nested in &interface.interfaces {
            walk_interface(paths, nested, Some(&interface_path), module_path);
        }
        for nested in &interface.enums {
            walk_enum(paths, nested, Some(&interface_path), module_path);
        }
    }

    fn walk_enum(
        paths: &mut DefinitionPaths,
        enum_cell: &EnumCell,
        parent_path: Option<&str>,
        module_path: Option<&str>,
    ) {
        let r#enum = enum_cell.borrow();
        let enum_path = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, r#enum.ident)
        } else {
            r#enum.ident.clone()
        };

        paths
            .enum_paths
            .insert(enum_cell.clone(), enum_path.clone());

        for nested in &r#enum.classes {
            walk_class(paths, nested, Some(&enum_path), module_path);
        }
        for nested in &r#enum.interfaces {
            walk_interface(paths, nested, Some(&enum_path), module_path);
        }
        for nested in &r#enum.enums {
            walk_enum(paths, nested, Some(&enum_path), module_path);
        }
    }

    for root in roots {
        let package_prefix = root.package.trim().trim_matches('.');
        let package_prefix = if package_prefix.is_empty() {
            None
        } else {
            Some(package_prefix)
        };

        for class_cell in &root.classes {
            walk_class(&mut paths, class_cell, package_prefix, package_prefix);
        }
        for interface_cell in &root.interfaces {
            walk_interface(&mut paths, interface_cell, package_prefix, package_prefix);
        }
        for enum_cell in &root.enums {
            walk_enum(&mut paths, enum_cell, package_prefix, package_prefix);
        }
    }

    paths
}

#[derive(Debug, Clone)]
struct DefinitionPaths {
    class_paths: HashMap<ClassCell, String>,
    class_modules: HashMap<ClassCell, String>,
    enum_paths: HashMap<EnumCell, String>,
    interface_paths: HashMap<InterfaceCell, String>,
    interface_modules: HashMap<InterfaceCell, String>,
}

impl DefinitionPaths {
    fn class_path(&self, class_cell: &ClassCell) -> String {
        self.class_paths
            .get(class_cell)
            .cloned()
            .unwrap_or_else(|| class_cell.borrow().ident.clone())
    }

    fn class_module(&self, class_cell: &ClassCell) -> Option<&str> {
        self.class_modules
            .get(class_cell)
            .map(|module| module.as_str())
    }

    fn interface_module(&self, interface_cell: &InterfaceCell) -> Option<&str> {
        self.interface_modules
            .get(interface_cell)
            .map(|module| module.as_str())
    }

    fn enum_path(&self, enum_cell: &EnumCell) -> String {
        self.enum_paths
            .get(enum_cell)
            .cloned()
            .unwrap_or_else(|| enum_cell.borrow().ident.clone())
    }

    fn interface_path(&self, interface_cell: &InterfaceCell) -> String {
        self.interface_paths
            .get(interface_cell)
            .cloned()
            .unwrap_or_else(|| interface_cell.borrow().ident.clone())
    }
}
