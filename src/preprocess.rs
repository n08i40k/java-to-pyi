use std::{collections::HashMap, fs, ops::Deref, path::Path, rc::Rc};

use java_ast_parser::ast::{self, ClassCell, EnumCell, InterfaceCell, Type, TypeGeneric, TypeName};
use log::warn;

use crate::index_tree::{GlobalIndexTree, ImportedIndexTree, LocalIndexTree, PackageIndexTree};

/// Parse ast and convert it to owned ("disconnect" from string).
pub fn parse_java_ast<P: AsRef<Path>>(
    path: P,
) -> std::result::Result<java_ast_parser::ast::Root, Box<java_ast_parser::ErrorCell<'static>>> {
    let data = fs::read_to_string(path).unwrap();

    java_ast_parser::parse(&data).map_err(|x| Box::new(x.into_owned()))
}

fn resolve_qualified_type(
    generic_names: &[String],
    r#type: &mut ast::QualifiedType,
    scope: Option<&ClassCell>,
    local_index_tree: &LocalIndexTree,
) {
    if r#type.len() == 1
        && let TypeName::Ident(ident) = &r#type[0].name
        && generic_names.contains(ident)
    {
        r#type.last_mut().unwrap().name = ast::TypeName::ResolvedGeneric(ident.clone());
        return;
    }

    if let Some(last) = r#type.last_mut()
        && !last.generics.is_empty()
    {
        for inner in &mut last.generics {
            if let TypeGeneric::Type(inner) = inner {
                resolve_qualified_type(generic_names, inner, scope, local_index_tree);
            }
        }
    }

    if let Some(Type {
        name: TypeName::Ident(ident),
        ..
    }) = r#type.last()
    {
        match ident.as_str() {
            "String" | "Boolean" | "Integer" | "Long" | "Map" | "List" | "Set" | "HashMap"
            | "ArrayList" | "HashSet" | "ImmutableMap" | "ImmutableList" | "ImmutableSet"
            | "IdentityHashMap" | "CharSequence" | "ByteBuffer" | "Object" | "Runnable" => return,
            _ => {}
        }
    }

    let Some(type_cell) = local_index_tree.search(scope, r#type) else {
        if let TypeName::Ident(_) = r#type.last().unwrap().name {
            warn!(
                "Failed to resolve type `{}`.",
                r#type
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            );
        }

        return;
    };

    r#type.last_mut().unwrap().name = ast::TypeName::ResolvedClass(type_cell.clone());
}

/// ChildPtr -> ParentPtr
fn resolve_type_names<'a, T: IntoIterator<Item = &'a (ClassCell, &'a LocalIndexTree)>>(
    iter: T,
) -> HashMap<ClassCell, ClassCell> {
    let mut extends_map: HashMap<ClassCell, ClassCell> = HashMap::new();

    for (class_cell, local_index_tree) in iter {
        let mut class = class_cell.borrow_mut();

        let generics = collect_generic_names(&class.generics);

        if let Some(extends) = &mut class.extends {
            resolve_qualified_type(&generics, extends, Some(class_cell), local_index_tree);
            if let Some(ast::TypeName::ResolvedClass(parent_cell)) = extends.last().map(|t| &t.name)
            {
                extends_map.insert(class_cell.clone(), parent_cell.clone());
            }
        }

        for variable in &mut class.variables {
            resolve_qualified_type(
                &generics,
                &mut variable.r#type,
                Some(class_cell),
                local_index_tree,
            );
        }

        for function in &mut class.functions {
            let mut function_generics = generics.clone();
            function_generics.extend(collect_generic_names(&function.generics));

            if function.ident == "__ctor" && function.return_type.len() == 1 {
                function.return_type.last_mut().unwrap().name =
                    TypeName::ResolvedClass(class_cell.clone());
            } else {
                resolve_qualified_type(
                    &function_generics,
                    &mut function.return_type,
                    Some(class_cell),
                    local_index_tree,
                );
            }

            for argument in &mut function.arguments {
                resolve_qualified_type(
                    &function_generics,
                    &mut argument.r#type,
                    Some(class_cell),
                    local_index_tree,
                );
            }
        }
    }

    extends_map
}

fn resolve_interface_type_names<
    'a,
    T: IntoIterator<Item = &'a (InterfaceCell, &'a LocalIndexTree)>,
>(
    iter: T,
) {
    for (interface_cell, local_index_tree) in iter {
        let mut interface = interface_cell.borrow_mut();
        let generics = collect_generic_names(&interface.generics);

        for extend in &mut interface.extends {
            resolve_qualified_type(&generics, extend, None, local_index_tree);
        }

        for variable in &mut interface.variables {
            resolve_qualified_type(&generics, &mut variable.r#type, None, local_index_tree);
        }

        for function in &mut interface.functions {
            let mut function_generics = generics.clone();
            function_generics.extend(collect_generic_names(&function.generics));

            resolve_qualified_type(
                &function_generics,
                &mut function.return_type,
                None,
                local_index_tree,
            );

            for argument in &mut function.arguments {
                resolve_qualified_type(
                    &function_generics,
                    &mut argument.r#type,
                    None,
                    local_index_tree,
                );
            }
        }
    }
}

fn resolve_enum_type_names<'a, T: IntoIterator<Item = &'a (EnumCell, &'a LocalIndexTree)>>(
    iter: T,
) {
    for (enum_cell, local_index_tree) in iter {
        let mut r#enum = enum_cell.borrow_mut();
        let generics = collect_generic_names(&r#enum.generics);

        for implement in &mut r#enum.implements {
            resolve_qualified_type(&generics, implement, None, local_index_tree);
        }

        for variable in &mut r#enum.variables {
            resolve_qualified_type(&generics, &mut variable.r#type, None, local_index_tree);
        }

        for function in &mut r#enum.functions {
            let mut function_generics = generics.clone();
            function_generics.extend(collect_generic_names(&function.generics));

            resolve_qualified_type(
                &function_generics,
                &mut function.return_type,
                None,
                local_index_tree,
            );

            for argument in &mut function.arguments {
                resolve_qualified_type(
                    &function_generics,
                    &mut argument.r#type,
                    None,
                    local_index_tree,
                );
            }
        }
    }
}

fn collect_scoped_classes<'a, T: IntoIterator<Item = &'a Scope>>(
    scopes: T,
) -> Box<[(ClassCell, &'a LocalIndexTree)]> {
    let mut classes: Vec<(ClassCell, &'a LocalIndexTree)> = Vec::new();

    fn walk_interface<'a>(
        classes: &mut Vec<(ClassCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        interface_cell: &InterfaceCell,
    ) {
        for class in &interface_cell.borrow().classes {
            walk_class(classes, local_index_tree, class);
        }

        for interface in &interface_cell.borrow().interfaces {
            walk_interface(classes, local_index_tree, interface);
        }

        for r#enum in &interface_cell.borrow().enums {
            walk_enum(classes, local_index_tree, r#enum);
        }
    }

    fn walk_class<'a>(
        classes: &mut Vec<(ClassCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        class_cell: &ClassCell,
    ) {
        classes.push((class_cell.clone(), local_index_tree));

        for class in &class_cell.borrow().classes {
            walk_class(classes, local_index_tree, class);
        }

        for interface in &class_cell.borrow().interfaces {
            walk_interface(classes, local_index_tree, interface);
        }

        for r#enum in &class_cell.borrow().enums {
            walk_enum(classes, local_index_tree, r#enum);
        }
    }

    fn walk_enum<'a>(
        classes: &mut Vec<(ClassCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        enum_cell: &EnumCell,
    ) {
        for class in &enum_cell.borrow().classes {
            walk_class(classes, local_index_tree, class);
        }

        for interface in &enum_cell.borrow().interfaces {
            walk_interface(classes, local_index_tree, interface);
        }

        for r#enum in &enum_cell.borrow().enums {
            walk_enum(classes, local_index_tree, r#enum);
        }
    }

    for scope in scopes {
        for interface in &scope.ast.interfaces {
            walk_interface(&mut classes, &scope.local_index_tree, interface);
        }

        for class in &scope.ast.classes {
            walk_class(&mut classes, &scope.local_index_tree, class);
        }

        for r#enum in &scope.ast.enums {
            walk_enum(&mut classes, &scope.local_index_tree, r#enum);
        }
    }

    classes.into_boxed_slice()
}

fn collect_scoped_interfaces<'a, T: IntoIterator<Item = &'a Scope>>(
    scopes: T,
) -> Box<[(InterfaceCell, &'a LocalIndexTree)]> {
    let mut interfaces: Vec<(InterfaceCell, &'a LocalIndexTree)> = Vec::new();

    fn walk_interface<'a>(
        interfaces: &mut Vec<(InterfaceCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        interface_cell: &InterfaceCell,
    ) {
        interfaces.push((interface_cell.clone(), local_index_tree));

        for class in &interface_cell.borrow().classes {
            walk_class(interfaces, local_index_tree, class);
        }

        for interface in &interface_cell.borrow().interfaces {
            walk_interface(interfaces, local_index_tree, interface);
        }

        for r#enum in &interface_cell.borrow().enums {
            walk_enum(interfaces, local_index_tree, r#enum);
        }
    }

    fn walk_class<'a>(
        interfaces: &mut Vec<(InterfaceCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        class_cell: &ClassCell,
    ) {
        for interface in &class_cell.borrow().interfaces {
            walk_interface(interfaces, local_index_tree, interface);
        }

        for class in &class_cell.borrow().classes {
            walk_class(interfaces, local_index_tree, class);
        }

        for r#enum in &class_cell.borrow().enums {
            walk_enum(interfaces, local_index_tree, r#enum);
        }
    }

    fn walk_enum<'a>(
        interfaces: &mut Vec<(InterfaceCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        enum_cell: &EnumCell,
    ) {
        for interface in &enum_cell.borrow().interfaces {
            walk_interface(interfaces, local_index_tree, interface);
        }

        for class in &enum_cell.borrow().classes {
            walk_class(interfaces, local_index_tree, class);
        }

        for r#enum in &enum_cell.borrow().enums {
            walk_enum(interfaces, local_index_tree, r#enum);
        }
    }

    for scope in scopes {
        for interface in &scope.ast.interfaces {
            walk_interface(&mut interfaces, &scope.local_index_tree, interface);
        }

        for class in &scope.ast.classes {
            walk_class(&mut interfaces, &scope.local_index_tree, class);
        }

        for r#enum in &scope.ast.enums {
            walk_enum(&mut interfaces, &scope.local_index_tree, r#enum);
        }
    }

    interfaces.into_boxed_slice()
}

fn collect_scoped_enums<'a, T: IntoIterator<Item = &'a Scope>>(
    scopes: T,
) -> Box<[(EnumCell, &'a LocalIndexTree)]> {
    let mut enums: Vec<(EnumCell, &'a LocalIndexTree)> = Vec::new();

    fn walk_enum<'a>(
        enums: &mut Vec<(EnumCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        enum_cell: &EnumCell,
    ) {
        enums.push((enum_cell.clone(), local_index_tree));

        for class in &enum_cell.borrow().classes {
            walk_class(enums, local_index_tree, class);
        }

        for interface in &enum_cell.borrow().interfaces {
            walk_interface(enums, local_index_tree, interface);
        }

        for r#enum in &enum_cell.borrow().enums {
            walk_enum(enums, local_index_tree, r#enum);
        }
    }

    fn walk_class<'a>(
        enums: &mut Vec<(EnumCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        class_cell: &ClassCell,
    ) {
        for r#enum in &class_cell.borrow().enums {
            walk_enum(enums, local_index_tree, r#enum);
        }

        for class in &class_cell.borrow().classes {
            walk_class(enums, local_index_tree, class);
        }

        for interface in &class_cell.borrow().interfaces {
            walk_interface(enums, local_index_tree, interface);
        }
    }

    fn walk_interface<'a>(
        enums: &mut Vec<(EnumCell, &'a LocalIndexTree)>,
        local_index_tree: &'a LocalIndexTree,
        interface_cell: &InterfaceCell,
    ) {
        for r#enum in &interface_cell.borrow().enums {
            walk_enum(enums, local_index_tree, r#enum);
        }

        for class in &interface_cell.borrow().classes {
            walk_class(enums, local_index_tree, class);
        }

        for interface in &interface_cell.borrow().interfaces {
            walk_interface(enums, local_index_tree, interface);
        }
    }

    for scope in scopes {
        for r#enum in &scope.ast.enums {
            walk_enum(&mut enums, &scope.local_index_tree, r#enum);
        }

        for class in &scope.ast.classes {
            walk_class(&mut enums, &scope.local_index_tree, class);
        }

        for interface in &scope.ast.interfaces {
            walk_interface(&mut enums, &scope.local_index_tree, interface);
        }
    }

    enums.into_boxed_slice()
}

fn collect_generic_names(generics: &[ast::GenericDefinition]) -> Vec<String> {
    generics.iter().map(|g| g.ident.clone()).collect()
}

fn strip_unknown_extends<'a, T: IntoIterator<Item = &'a ClassCell>>(
    classes_iter: T,
    _extends_map: &HashMap<ClassCell, ClassCell>,
) {
    for class_cell in classes_iter {
        let mut class = class_cell.borrow_mut();

        match &mut class.extends {
            None => continue,
            Some(q) => {
                if let ast::TypeName::ResolvedClass(_) = q.last().unwrap().name {
                    continue;
                }
            }
        }

        class.extends = None;
    }
}

#[derive(Debug)]
pub struct Scope {
    pub ast: Rc<ast::Root>,
    pub local_index_tree: LocalIndexTree,
}

impl Scope {
    pub fn from_roots(roots: &[Rc<ast::Root>]) -> Box<[Self]> {
        let package_index_trees = {
            let package_index_trees = roots
                .iter()
                .map(|x| PackageIndexTree::from_ast(x))
                .collect::<Box<[_]>>();

            let mut groups: HashMap<String, PackageIndexTree> = HashMap::new();

            for package_index_tree in package_index_trees {
                if let Some(target_index_tree) = groups.get_mut(package_index_tree.package()) {
                    target_index_tree.merge_with(&package_index_tree);
                } else {
                    groups.insert(package_index_tree.package().to_string(), package_index_tree);
                }
            }

            groups
        };

        let global_index_tree = Rc::new(GlobalIndexTree::from_iter(package_index_trees.values()));

        roots
            .iter()
            .map(|root| {
                let imported_index_tree = ImportedIndexTree::from_imports(
                    root.imports.iter().map(|x| x.as_str()),
                    &global_index_tree,
                );

                let package_index_tree = package_index_trees.get(root.package.as_str()).unwrap();

                let local_index_tree = LocalIndexTree::new(
                    global_index_tree.clone(),
                    imported_index_tree,
                    Clone::clone(package_index_tree.deref()),
                );

                Scope {
                    ast: root.clone(),
                    local_index_tree,
                }
            })
            .collect::<Box<_>>()
    }
}

pub fn preprocess_asts(roots: &[Rc<ast::Root>]) {
    let scopes = Scope::from_roots(roots);

    let scoped_classes = collect_scoped_classes(&scopes);
    let scoped_interfaces = collect_scoped_interfaces(&scopes);
    let scoped_enums = collect_scoped_enums(&scopes);

    let extends_map = resolve_type_names(&scoped_classes);
    resolve_interface_type_names(&scoped_interfaces);
    resolve_enum_type_names(&scoped_enums);

    let classes = scoped_classes
        .into_iter()
        .map(|(x, _)| x)
        .collect::<Box<_>>();

    strip_unknown_extends(&classes, &extends_map);
}
