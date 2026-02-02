use itertools::Itertools;
use java_ast_parser::ast::{ClassCell, Root};
use std::{collections::HashMap, rc::Rc};

pub type IndexTable = HashMap<String, ClassCell>;

#[derive(Debug, Clone)]
pub struct QualifiedIndexTable {
    pub package: String,
    pub idt: IndexTable,
}

#[derive(Debug, Clone)]
pub struct GlobalIndexTable(IndexTable);

impl<'a> FromIterator<&'a QualifiedIndexTable> for GlobalIndexTable {
    fn from_iter<IT: IntoIterator<Item = &'a QualifiedIndexTable>>(iter: IT) -> Self {
        let mut index_table = IndexTable::new();

        for qualified_table in iter {
            for (key, class_cell) in &qualified_table.idt {
                index_table.insert(
                    format!("{}.{}", qualified_table.package, key),
                    class_cell.clone(),
                );
            }
        }

        Self(index_table)
    }
}

impl GlobalIndexTable {
    pub fn search(&self, query: &str) -> Option<ClassCell> {
        self.0.get(query).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct ImportedIndexTable(IndexTable);

impl From<IndexTable> for ImportedIndexTable {
    fn from(value: IndexTable) -> Self {
        Self(value)
    }
}

impl ImportedIndexTable {
    pub fn from_imports<'a, I>(
        imports: &[impl AsRef<str>],
        qualified_index_tables: I,
    ) -> Self
    where
        I: IntoIterator<Item = &'a QualifiedIndexTable>,
    {
        let mut imported_table = IndexTable::new();

        for qualified_table in qualified_index_tables {
            let qualified_keys = qualified_table
                .idt
                .keys()
                .map(|x| x.split('.').collect::<Box<[_]>>())
                .collect::<Box<[_]>>();

            for import_path in imports {
                let import_path = import_path.as_ref();
                let package = qualified_table.package.as_str();

                if !import_path.starts_with(package) {
                    continue;
                }

                let Some((_, import_suffix)) = import_path.split_at_checked(package.len() + 1)
                else {
                    continue;
                };

                if import_suffix == "*" {
                    for (key, class_cell) in &qualified_table.idt {
                        imported_table.insert(key.to_string(), class_cell.clone());
                    }
                    break;
                }

                for qualified_key in &qualified_keys {
                    let mut segments = import_suffix.split('.').zip_longest(qualified_key.iter());

                    let mut should_insert = false;
                    let mut prev_ident: Option<&str> = None;

                    while let Some(segment) = segments.next() {
                        should_insert = false;

                        match segment {
                            itertools::EitherOrBoth::Both(import_segment, ident) => {
                                if import_segment == "*" {
                                    let remaining_idents = segments
                                        .filter_map(|segment| match segment {
                                            itertools::EitherOrBoth::Both(_, ident) => Some(ident),
                                            itertools::EitherOrBoth::Left(_) => None,
                                            itertools::EitherOrBoth::Right(ident) => Some(ident),
                                        })
                                        .join(".");

                                    let import_key = if remaining_idents.is_empty() {
                                        ident.to_string()
                                    } else {
                                        format!("{}.{}", ident, remaining_idents)
                                    };

                                    let class_cell = qualified_table
                                        .idt
                                        .get(&qualified_key.join("."))
                                        .unwrap();

                                    imported_table.insert(import_key, class_cell.clone());

                                    break;
                                }

                                if import_segment != *ident {
                                    break;
                                }

                                should_insert = true;
                                prev_ident = Some(ident);
                            }
                            itertools::EitherOrBoth::Left(_) => break,
                            itertools::EitherOrBoth::Right(ident) => {
                                let Some(prev_ident) = prev_ident else {
                                    break;
                                };

                                let remaining_idents = segments
                                    .filter_map(|segment| match segment {
                                        itertools::EitherOrBoth::Both(_, ident) => Some(ident),
                                        itertools::EitherOrBoth::Left(_) => None,
                                        itertools::EitherOrBoth::Right(ident) => Some(ident),
                                    })
                                    .join(".");

                                let scoped_key = format!("{}.{}", prev_ident, ident);

                                let import_key = if remaining_idents.is_empty() {
                                    scoped_key
                                } else {
                                    format!("{}.{}", scoped_key, remaining_idents)
                                };

                                let class_cell = qualified_table
                                    .idt
                                    .get(&qualified_key.join("."))
                                    .unwrap();

                                imported_table.insert(import_key, class_cell.clone());

                                break;
                            }
                        }
                    }

                    if should_insert {
                        imported_table.insert(
                            qualified_key.last().unwrap().to_string(),
                            qualified_table
                                .idt
                                .get(&qualified_key.join("."))
                                .unwrap()
                                .clone(),
                        );
                    }
                }
            }
        }

        Self::from(imported_table)
    }

    pub fn search(&self, query: &str) -> Option<ClassCell> {
        self.0.get(query).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct LocalIndexTable {
    global: Rc<GlobalIndexTable>,
    imported: ImportedIndexTable,
    local: QualifiedIndexTable,
}

impl LocalIndexTable {
    pub fn new(
        global: Rc<GlobalIndexTable>,
        imported: ImportedIndexTable,
        local: QualifiedIndexTable,
    ) -> Self {
        Self {
            global,
            imported,
            local,
        }
    }
    pub fn search_global(&self, query: &str) -> Option<ClassCell> {
        self.global.search(query)
    }

    pub fn search_imported(&self, query: &str) -> Option<ClassCell> {
        self.imported.search(query)
    }

    pub fn search_local(&self, scope: Option<&str>, query: &str) -> Option<ClassCell> {
        self.local
            .idt
            .keys()
            .find_map(|qualified_key| {
                if let Some(scope) = scope
                    && !qualified_key.starts_with(scope)
                {
                    return None;
                }

                let (_, remaining_key) =
                    qualified_key.split_at_checked(scope.map(|x| x.len() + 1).unwrap_or(0))?;

                // dbg!(&rem, &query);
                if remaining_key == query {
                    self.local.idt.get(qualified_key).cloned()
                } else {
                    None
                }
            })
            .or_else(|| {
                let scope = scope?;

                if !scope.contains('.') {
                    // println!("------- LOWER -------");
                    return self.search_local(None, query);
                }

                // println!("------- LOWER -------");

                self.search_local(Some(scope.rsplit_once('.').unwrap().0), query)
            })
    }

    pub fn search(&self, scope: Option<&str>, query: &str) -> Option<ClassCell> {
        self.search_local(scope, query)
            .or_else(|| self.search_imported(query))
            .or_else(|| self.search_global(query))
    }
}

impl QualifiedIndexTable {
    pub fn from_ast(ast: &Root) -> Self {
        let mut index_table = IndexTable::new();

        fn walk_class(
            index_table: &mut IndexTable,
            parent_path: Option<&str>,
            class_cell: &ClassCell,
        ) {
            let class = class_cell.borrow();

            let qualified_name = if let Some(parent_path) = parent_path {
                format!("{}.{}", parent_path, class.ident)
            } else {
                class.ident.to_string()
            };

            for class in &class.classes {
                walk_class(index_table, Some(&qualified_name), class);
            }

            index_table.insert(qualified_name, class_cell.clone());
        }

        for class in &ast.classes {
            walk_class(&mut index_table, None, class);
        }

        Self {
            package: ast.package.to_string(),
            idt: index_table,
        }
    }
}
