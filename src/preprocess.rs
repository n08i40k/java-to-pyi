use std::{collections::HashMap, fs, ops::Deref, path::Path, rc::Rc};

use java_ast_parser::ast::{self, ClassCell, InterfaceCell, Type, TypeGeneric, TypeName};
use log::warn;
use topo_sort::{SortResults, TopoSort};

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
    scope: &ClassCell,
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

    let Some(type_cell) = local_index_tree.search(Some(scope), r#type) else {
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
    let extends_map: HashMap<ClassCell, ClassCell> = HashMap::new();

    for (class_cell, local_index_tree) in iter {
        let mut class = class_cell.borrow_mut();

        let generics = class
            .generics
            .iter()
            .map(|g| g.ident.clone())
            .collect::<Box<[_]>>();

        if let Some(extends) = &mut class.extends {
            resolve_qualified_type(&generics, extends, class_cell, local_index_tree);
        }

        for variable in &mut class.variables {
            resolve_qualified_type(
                &generics,
                &mut variable.r#type,
                class_cell,
                local_index_tree,
            );
        }

        for function in &mut class.functions {
            resolve_qualified_type(
                &generics,
                &mut function.return_type,
                class_cell,
                local_index_tree,
            );

            for argument in &mut function.arguments {
                resolve_qualified_type(
                    &generics,
                    &mut argument.r#type,
                    class_cell,
                    local_index_tree,
                );
            }
        }
    }

    extends_map
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
    }

    for scope in scopes {
        for interface in &scope.ast.interfaces {
            walk_interface(&mut classes, &scope.local_index_tree, interface);
        }

        for class in &scope.ast.classes {
            walk_class(&mut classes, &scope.local_index_tree, class);
        }
    }

    classes.into_boxed_slice()
}

fn merge_inherited_members<'a, T: IntoIterator<Item = &'a ClassCell>>(
    classes_iter: T,
    extends_map: &HashMap<ClassCell, ClassCell>,
) {
    for class_cell in classes_iter {
        let mut class = class_cell.borrow_mut();

        if class.extends.is_none() {
            continue;
        }

        let Some(parent_cell) = extends_map.get(class_cell) else {
            continue;
        };

        let parent = parent_cell.borrow();

        class.variables = {
            let mut variables = parent.variables.clone().into_vec();
            variables.extend_from_slice(&class.variables);
            variables.into_boxed_slice()
        };

        class.functions = {
            let mut functions = parent.functions.clone().into_vec();
            functions.extend_from_slice(&class.functions);
            functions.into_boxed_slice()
        };
    }
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

fn topo_sort_extendables<'a, T: IntoIterator<Item = &'a ClassCell>>(
    classes_iter: T,
    classes_iter_len: usize,
) -> Option<Box<[ClassCell]>> {
    let mut topo_sort = TopoSort::with_capacity(classes_iter_len);

    for class_cell in classes_iter {
        if let Some(q) = &class_cell.borrow().extends
            && let ast::TypeName::ResolvedClass(type_cell) = &q.last().unwrap().name
        {
            topo_sort.insert(class_cell.clone(), vec![type_cell.clone()]);
        }
    }

    let SortResults::Full(nodes) = topo_sort.into_vec_nodes() else {
        return None;
    };

    Some(nodes.into_boxed_slice())
}

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

#[derive(Debug)]
pub enum Error {
    CircullarDependency,
}

pub fn preprocess_asts(roots: &[Rc<ast::Root>], inherit_by_merge: bool) -> Result<(), Error> {
    let scopes = Scope::from_roots(roots);

    let scoped_classes = collect_scoped_classes(&scopes);

    let extends_map = resolve_type_names(&scoped_classes);

    let classes = scoped_classes
        .into_iter()
        .map(|(x, _)| x)
        .collect::<Box<_>>();

    strip_unknown_extends(&classes, &extends_map);

    if inherit_by_merge {
        let sorted_classes =
            topo_sort_extendables(&classes, classes.len()).ok_or(Error::CircullarDependency)?;

        merge_inherited_members(&sorted_classes, &extends_map);
    }

    Ok(())
}
