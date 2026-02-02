use std::{
    collections::HashMap,
    fs,
    ops::{Deref, DerefMut},
    path::Path,
    rc::Rc,
};

use java_ast_parser::ast::{self, ClassCell, Modifiers};
use topo_sort::{SortResults, TopoSort};

use crate::index_table::{GlobalIndexTable, ImportedIndexTable, LocalIndexTable, QualifiedIndexTable};

/// Parse ast and convert it to owned ("disconnect" from string).
pub fn parse_java_ast<P: AsRef<Path>>(path: P) -> Option<ast::Root> {
    let data = fs::read_to_string(path).unwrap();

    let mut ast = match java_ast_parser::parse(&data) {
        Ok(ast) => ast,
        Err(e) => {
            match &e {
                lalrpop_util::ParseError::UnrecognizedToken { token, expected: _ } => {
                    let line = data[..token.0].chars().filter(|&ch| ch == '\n').count() + 1;

                    let column = token.0 - data[..token.0].rfind('\n').unwrap_or(0);

                    let position = format!("line {}, column {}", line, column);
                    println!("{}\n{}", e, position);
                }
                _ => {
                    println!("{}", e);
                }
            };

            return None;
        }
    };

    strip_non_public_symbols(&mut ast);

    Some(ast)
}

/// Remove `private`, `protected` and `static` functions and variables from all classes in ast
/// recursively.
/// As we don't see them in Python (afaik).
fn strip_non_public_symbols(ast: &mut ast::Root) {
    fn should_keep_variable(modifiers: &Modifiers) -> bool {
        modifiers.intersects(Modifiers::PUBLIC)
            && !modifiers.intersects(Modifiers::STATIC | Modifiers::FINAL)
    }

    fn should_keep_function(modifiers: &Modifiers) -> bool {
        modifiers.intersects(Modifiers::PUBLIC)
    }

    fn walk_class(class: &mut java_ast_parser::ast::Class) {
        class.variables = class
            .variables
            .clone()
            .into_iter()
            .filter(|v| should_keep_variable(&v.modifiers))
            .collect();

        class.functions = class
            .functions
            .clone()
            .into_iter()
            .filter(|f| should_keep_function(&f.modifiers))
            .collect();

        for class in class.classes.iter_mut() {
            walk_class(class.borrow_mut().deref_mut());
        }
    }

    for class in ast.classes.iter_mut() {
        walk_class(class.borrow_mut().deref_mut());
    }
}

/// ClassPtr -> Local Scope
/// # Examples:
/// ru.n08i40k.MyClass -> None
/// ru.n08i40k.MyClass.InnerClass -> Some("MyClass")
fn build_local_scope_map(ast: &ast::Root) -> HashMap<ClassCell, String> {
    let mut scope_map: HashMap<ClassCell, String> = HashMap::new();

    fn walk_class(
        scope_map: &mut HashMap<ClassCell, String>,
        scope: Option<&str>,
        class_cell: &ClassCell,
    ) {
        let class = class_cell.borrow();

        if let Some(scope) = &scope {
            scope_map.insert(class_cell.clone(), scope.to_string());
        }

        let scope = if let Some(parent) = scope {
            format!("{}.{}", parent, &class.ident)
        } else {
            class.ident.to_string()
        };

        for class_cell in &class.classes {
            walk_class(scope_map, Some(&scope), class_cell);
        }
    }

    for class_cell in &ast.classes {
        walk_class(&mut scope_map, None, class_cell);
    }

    scope_map
}

fn resolve_type_name(
    type_name: &mut ast::TypeName,
    scope: Option<&str>,
    local_index_table: &LocalIndexTable,
) {
    let ast::TypeName::Ident(ident) = type_name else {
        return;
    };

    let Some(type_cell) = local_index_table.search(scope, ident) else {
        return;
    };

    *type_name = ast::TypeName::ResolvedIdent(type_cell.clone());
}

/// ChildPtr -> ParentPtr
fn resolve_type_names<'a, T: IntoIterator<Item = &'a (ClassCell, &'a LocalIndexTable)>>(
    scope_map: &HashMap<ClassCell, String>,
    classes_with_tables: T,
) -> HashMap<ClassCell, ClassCell> {
    let extends_map: HashMap<ClassCell, ClassCell> = HashMap::new();

    for (class_cell, local_index_table) in classes_with_tables {
        let scope = scope_map.get(class_cell).map(|x| x.as_str());

        let mut class = class_cell.borrow_mut();

        if let Some(extends) = &mut class.extends {
            resolve_type_name(&mut extends.name, scope, local_index_table);
        }

        for variable in &mut class.variables {
            resolve_type_name(&mut variable.r#type.name, scope, local_index_table);
        }

        for function in &mut class.functions {
            resolve_type_name(&mut function.return_type.name, scope, local_index_table);
        }
    }

    extends_map
}

fn collect_scoped_classes<'a, T: IntoIterator<Item = &'a Scope>>(
    scopes: T,
) -> Box<[(ClassCell, &'a LocalIndexTable)]> {
    let mut classes: Vec<(ClassCell, &'a LocalIndexTable)> = Vec::new();

    fn walk_class<'a>(
        classes: &mut Vec<(ClassCell, &'a LocalIndexTable)>,
        local_index_table: &'a LocalIndexTable,
        class_cell: &ClassCell,
    ) {
        classes.push((class_cell.clone(), local_index_table));

        for class in &class_cell.borrow().classes {
            walk_class(classes, local_index_table, class);
        }
    }

    for scope in scopes {
        for class in &scope.ast.classes {
            walk_class(&mut classes, &scope.local_index_table, class);
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

        match class.extends {
            None => continue,
            Some(ast::Type {
                name: ast::TypeName::ResolvedIdent(_),
                ..
            }) => continue,
            _ => {}
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
        if let Some(ast::Type {
            name: ast::TypeName::ResolvedIdent(type_cell),
            ..
        }) = &class_cell.borrow().extends
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
    pub local_index_table: LocalIndexTable,
}

impl Scope {
    pub fn from_roots(roots: &[Rc<ast::Root>]) -> Box<[Self]> {
        let qualified_tables_by_root = {
            let mut tables_by_root: HashMap<*const ast::Root, QualifiedIndexTable> =
                HashMap::new();

            for root in roots.iter() {
                tables_by_root.insert(
                    root.deref() as *const ast::Root,
                    QualifiedIndexTable::from_ast(root),
                );
            }

            tables_by_root
        };

        let qualified_tables = qualified_tables_by_root.values().collect::<Box<_>>();

        let global_index_table = Rc::new(GlobalIndexTable::from_iter(
            qualified_tables_by_root.values(),
        ));

        roots
            .iter()
            .map(|root| {
                let imported_index_table = ImportedIndexTable::from_imports(
                    root.imports
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Box<[_]>>()
                        .as_ref(),
                    qualified_tables_by_root.values(),
                );

                let qualified_index_table = qualified_tables
                    .iter()
                    .filter_map(|v| {
                        if v.package != root.package {
                            return None;
                        }

                        Some(v.idt.clone())
                    })
                    .reduce(|left, right| {
                        let mut merged = HashMap::new();

                        merged.extend(left);
                        merged.extend(right);

                        merged
                    })
                    .map(|merged_table| QualifiedIndexTable {
                        package: root.package.clone(),
                        idt: merged_table,
                    })
                    .unwrap();

                let local_index_table = LocalIndexTable::new(
                    global_index_table.clone(),
                    imported_index_table,
                    qualified_index_table,
                );

                Scope {
                    ast: root.clone(),
                    local_index_table,
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

    let classes_with_tables = collect_scoped_classes(&scopes);

    let scope_map = roots
        .iter()
        .map(|x| build_local_scope_map(x.deref()))
        .reduce(|left, right| {
            let mut merged = HashMap::new();

            merged.extend(left);
            merged.extend(right);

            merged
        })
        .unwrap_or(HashMap::new());

    let extends_map = resolve_type_names(&scope_map, &classes_with_tables);

    let classes = classes_with_tables
        .into_iter()
        .map(|(x, _)| x)
        .collect::<Box<_>>();

    strip_unknown_extends(&classes, &extends_map);

    if inherit_by_merge {
        let sorted_classes = topo_sort_extendables(&classes, classes.len())
            .ok_or(Error::CircullarDependency)?;

        merge_inherited_members(&sorted_classes, &extends_map);
    }

    Ok(())
}
