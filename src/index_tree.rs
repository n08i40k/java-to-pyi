use java_ast_parser::ast::{self, ClassCell, EnumCell, GetIdent, InterfaceCell, Root};
use orx_tree::{Bfs, Dyn, DynTree, NodeIdx, NodeRef};
use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum TreeNode {
    Root,
    Package(Rc<String>),
    Class(ClassCell),
    Enum(EnumCell),
    Interface(InterfaceCell),
}

impl TreeNode {
    pub fn ident(&self) -> Option<&'_ str> {
        match self {
            TreeNode::Root => None,
            TreeNode::Package(ident) => Some(ident.as_str()),
            TreeNode::Class(cell) => Some(cell.ident()),
            TreeNode::Enum(cell) => Some(cell.ident()),
            TreeNode::Interface(cell) => Some(cell.ident()),
        }
    }
}

impl std::hash::Hash for TreeNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(ident) = self.ident() {
            ident.hash(state);
        } else {
            0.hash(state);
        }
    }
}

impl std::cmp::PartialEq for TreeNode {
    fn eq(&self, other: &Self) -> bool {
        match (self.ident(), other.ident()) {
            (None, None) => true,
            (Some(l), Some(r)) => l == r,
            _ => false,
        }
    }
}

impl std::cmp::Eq for TreeNode {}

impl From<&str> for TreeNode {
    fn from(value: &str) -> Self {
        Self::Package(Rc::from(value.to_string()))
    }
}

impl From<&ClassCell> for TreeNode {
    fn from(value: &ClassCell) -> Self {
        Self::Class(value.clone())
    }
}

impl From<&InterfaceCell> for TreeNode {
    fn from(value: &InterfaceCell) -> Self {
        Self::Interface(value.clone())
    }
}

impl From<&EnumCell> for TreeNode {
    fn from(value: &EnumCell) -> Self {
        Self::Enum(value.clone())
    }
}
pub type IndexTree = DynTree<TreeNode>;

#[derive(Debug, Clone)]
pub struct PackageIndexTree {
    package: String,
    inner: IndexTree,
}

impl Deref for PackageIndexTree {
    type Target = DynTree<TreeNode>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for PackageIndexTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn merge_index_trees(
    target_tree: &mut DynTree<TreeNode>,
    target_node_idx: NodeIdx<Dyn<TreeNode>>,
    source_tree: &DynTree<TreeNode>,
    source_node_idx: NodeIdx<Dyn<TreeNode>>,
) {
    for source_child in source_tree.node(source_node_idx).children() {
        let same_target_idx =
            target_tree
                .node(target_node_idx)
                .children()
                .find_map(|target_child| {
                    if target_child.data() == source_child.data() {
                        Some(target_child.idx())
                    } else {
                        None
                    }
                });

        if let Some(same_target_idx) = same_target_idx {
            merge_index_trees(
                target_tree,
                same_target_idx,
                source_tree,
                source_child.idx(),
            );
        } else {
            target_tree
                .node_mut(target_node_idx)
                .push_child_tree(source_child.as_cloned_subtree());
        }
    }
}

impl PackageIndexTree {
    pub fn from_ast(ast: &Root) -> Self {
        fn walk_interface(
            tree: &mut DynTree<TreeNode>,
            parent_idx: NodeIdx<Dyn<TreeNode>>,
            self_cell: &InterfaceCell,
        ) {
            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            let self_ref = self_cell.borrow();

            for class_cell in &self_ref.classes {
                walk_class(tree, self_idx, class_cell);
            }

            for interface_cell in &self_ref.interfaces {
                walk_interface(tree, self_idx, interface_cell);
            }

            for enum_cell in &self_ref.enums {
                walk_enum(tree, self_idx, enum_cell);
            }
        }

        fn walk_class(
            tree: &mut DynTree<TreeNode>,
            parent_idx: NodeIdx<Dyn<TreeNode>>,
            self_cell: &ClassCell,
        ) {
            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            let self_ref = self_cell.borrow();

            for class_cell in &self_ref.classes {
                walk_class(tree, self_idx, class_cell);
            }

            for interface_cell in &self_ref.interfaces {
                walk_interface(tree, self_idx, interface_cell);
            }

            for enum_cell in &self_ref.enums {
                walk_enum(tree, self_idx, enum_cell);
            }
        }

        fn walk_enum(
            tree: &mut DynTree<TreeNode>,
            parent_idx: NodeIdx<Dyn<TreeNode>>,
            self_cell: &EnumCell,
        ) {
            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            let self_ref = self_cell.borrow();

            for class_cell in &self_ref.classes {
                walk_class(tree, self_idx, class_cell);
            }

            for interface_cell in &self_ref.interfaces {
                walk_interface(tree, self_idx, interface_cell);
            }

            for enum_cell in &self_ref.enums {
                walk_enum(tree, self_idx, enum_cell);
            }
        }

        let mut tree = DynTree::new(TreeNode::Root);
        let root_idx = tree.root().idx();

        for interface_cell in &ast.interfaces {
            walk_interface(&mut tree, root_idx, interface_cell);
        }

        for class_cell in &ast.classes {
            walk_class(&mut tree, root_idx, class_cell);
        }

        for enum_cell in &ast.enums {
            walk_enum(&mut tree, root_idx, enum_cell);
        }

        Self {
            package: ast.package.to_string(),
            inner: tree,
        }
    }

    pub fn package(&self) -> &str {
        &self.package
    }

    pub fn merge_with(&mut self, other: &Self) {
        let self_idx = self.inner.root().idx();
        let other_idx = other.inner.root().idx();

        merge_index_trees(&mut self.inner, self_idx, &other.inner, other_idx);
    }
}

#[derive(Debug, Clone)]
pub struct GlobalIndexTree(IndexTree);

impl Deref for GlobalIndexTree {
    type Target = DynTree<TreeNode>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GlobalIndexTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> FromIterator<&'a PackageIndexTree> for GlobalIndexTree {
    fn from_iter<IT: IntoIterator<Item = &'a PackageIndexTree>>(iter: IT) -> Self {
        let mut tree = IndexTree::new(TreeNode::Root);
        let root_idx = tree.root().idx();

        for package_index_tree in iter {
            let package_idx = {
                let mut current_idx = root_idx;

                for node in package_index_tree.package().split('.').map(TreeNode::from) {
                    let node_idx = tree.node(current_idx).children().find_map(|x| {
                        if x.data() == &node {
                            Some(x.idx())
                        } else {
                            None
                        }
                    });

                    current_idx =
                        node_idx.unwrap_or_else(|| tree.node_mut(current_idx).push_child(node));
                }

                current_idx
            };

            let mut package_mut = tree.node_mut(package_idx);

            for child in package_index_tree.root().children() {
                package_mut.push_child_tree(child.as_cloned_subtree());
            }
        }

        Self(tree)
    }
}

impl GlobalIndexTree {
    pub fn search(&self, query: &ast::QualifiedType) -> Option<ClassCell> {
        let mut current_idx = self.0.root().idx();

        for query_part in query {
            let ast::TypeName::Ident(ident) = &query_part.name else {
                return None;
            };

            let node_idx = self.0.node(current_idx).children().find_map(|x| {
                if x.data().ident().is_some_and(|x| x == ident) {
                    Some(x.idx())
                } else {
                    None
                }
            })?;

            current_idx = node_idx;
        }

        if let TreeNode::Class(class_cell) = self.0.node(current_idx).data() {
            Some(class_cell.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportedIndexTree(IndexTree);

impl From<IndexTree> for ImportedIndexTree {
    fn from(value: IndexTree) -> Self {
        Self(value)
    }
}

impl ImportedIndexTree {
    pub fn from_imports<'a, I>(import_iter: I, global_index_tree: &GlobalIndexTree) -> Self
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut tree = IndexTree::new(TreeNode::Root);

        for import in import_iter {
            let mut current_idx = global_index_tree.root().idx();
            let mut terminated = false;

            for import_part in import.split('.') {
                if import_part == "*" {
                    let mut root_mut = tree.root_mut();

                    for child in global_index_tree.node(current_idx).children() {
                        root_mut.push_child_tree(child.as_cloned_subtree());
                    }
                }

                let Some(node_idx) = global_index_tree
                    .node(current_idx)
                    .children()
                    .find_map(|x| {
                        if x.data().ident().is_some_and(|x| x == import_part) {
                            Some(x.idx())
                        } else {
                            None
                        }
                    })
                else {
                    terminated = true;
                    break;
                };

                current_idx = node_idx;
            }

            if terminated {
                continue;
            }

            tree.root_mut()
                .push_child_tree(global_index_tree.node(current_idx).as_cloned_subtree());
        }

        Self::from(tree)
    }

    pub fn search(&self, query: &ast::QualifiedType) -> Option<ClassCell> {
        let mut current_idx = self.0.root().idx();

        for query_part in query {
            let ast::TypeName::Ident(ident) = &query_part.name else {
                return None;
            };

            let node_idx = self.0.node(current_idx).children().find_map(|x| {
                if x.data().ident().is_some_and(|x| x == ident) {
                    Some(x.idx())
                } else {
                    None
                }
            })?;

            current_idx = node_idx;
        }

        if let TreeNode::Class(class_cell) = self.0.node(current_idx).data() {
            Some(class_cell.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalIndexTree {
    global: Rc<GlobalIndexTree>,
    imported: ImportedIndexTree,
    local: IndexTree,
    reverse_local: HashMap<ClassCell, NodeIdx<Dyn<TreeNode>>>,
}

impl LocalIndexTree {
    pub fn new(global: Rc<GlobalIndexTree>, imported: ImportedIndexTree, local: IndexTree) -> Self {
        let mut reverse_local = HashMap::new();

        for idx in local.root().indices::<Bfs>() {
            let TreeNode::Class(class_cell) = local.node(idx).data() else {
                continue;
            };

            reverse_local.insert(class_cell.clone(), idx);
        }

        Self {
            global,
            imported,
            local,
            reverse_local,
        }
    }

    pub fn search_global(&self, query: &ast::QualifiedType) -> Option<ClassCell> {
        self.global.search(query)
    }

    pub fn search_imported(&self, query: &ast::QualifiedType) -> Option<ClassCell> {
        self.imported.search(query)
    }

    pub fn search_local(
        &self,
        scope: Option<&ClassCell>,
        query: &ast::QualifiedType,
    ) -> Option<ClassCell> {
        let root_idx = scope
            .and_then(|x| self.reverse_local.get(x))
            .cloned()
            .unwrap_or_else(|| self.local.root().idx());

        let try_parent = || {
            scope?;

            if let Some(parent_node) = self.local.node(root_idx).parent() {
                let scope = if let TreeNode::Class(class_cell) = parent_node.data() {
                    Some(class_cell)
                } else {
                    None
                };

                self.search_local(scope, query)
            } else {
                None
            }
        };

        let mut current_idx = root_idx;

        for query_part in query.iter() {
            let ast::TypeName::Ident(ident) = &query_part.name else {
                return None;
            };

            let Some(node_idx) = self.local.node(current_idx).children().find_map(|x| {
                if x.data().ident().is_some_and(|x| x == ident) {
                    Some(x.idx())
                } else {
                    None
                }
            }) else {
                return try_parent();
            };

            current_idx = node_idx;
        }

        if let TreeNode::Class(class_cell) = self.local.node(current_idx).data() {
            Some(class_cell.clone())
        } else {
            try_parent()
        }
    }

    pub fn search(
        &self,
        scope: Option<&ClassCell>,
        query: &ast::QualifiedType,
    ) -> Option<ClassCell> {
        self.search_local(scope, query)
            .or_else(|| self.search_imported(query))
            .or_else(|| self.search_global(query))
    }
}
