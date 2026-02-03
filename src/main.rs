use std::{fs, path::PathBuf, rc::Rc};

use crate::preprocess::{parse_java_ast, preprocess_asts};
use crate::pyi::generate_pyi_by_package;

mod index_table;
mod preprocess;
mod pyi;

fn main() {
    let asts = [
        parse_java_ast("java/example1.java").unwrap(),
        parse_java_ast("java/example2.java").unwrap(),
        parse_java_ast("java/example3.java").unwrap(),
    ]
    .into_iter()
    .map(Rc::new)
    .collect::<Box<_>>();

    preprocess_asts(&asts, true).unwrap();

    let namespace_prefix = Some("javai");
    let outputs = generate_pyi_by_package(&asts, namespace_prefix);
    let out_dir = PathBuf::from("out");

    for (package, contents) in outputs {
        let file_path = package_to_path(&out_dir, &package, namespace_prefix);
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&file_path, contents).unwrap();
        println!("wrote {}", file_path.display());
    }
}

fn package_to_path(
    out_dir: &PathBuf,
    package: &str,
    namespace_prefix: Option<&str>,
) -> PathBuf {
    let mut path = out_dir.clone();

    if let Some(prefix) = namespace_prefix {
        for part in prefix.trim_matches('.').split('.') {
            if !part.is_empty() {
                path.push(part);
            }
        }
    }

    if package.is_empty() {
        path.push("__init__.pyi");
        return path;
    }

    for part in package.split('.') {
        path.push(part);
    }

    path.push("__init__.pyi");
    path
}
