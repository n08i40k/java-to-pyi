#![allow(clippy::mutable_key_type)]

use std::{
    env, fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::preprocess::{parse_java_ast, preprocess_asts};
use crate::pyi::generate_pyi_by_package;

mod index_tree;
mod preprocess;
mod pyi;

fn main() {
    if std::env::var("RUST_LOG").is_err() {
        unsafe {
            std::env::set_var("RUST_LOG", "info");
        }
    }

    env_logger::init();

    let options = match parse_args(env::args().collect()) {
        Ok(options) => options,
        Err(message) => {
            if message != "help requested" {
                eprintln!("{}", message);
                eprintln!();
            }
            eprintln!("{}", usage());
            return;
        }
    };

    let files = match collect_java_files(&options.inputs) {
        Ok(files) => files,
        Err(message) => {
            eprintln!("{}", message);
            return;
        }
    };

    if files.is_empty() {
        eprintln!("no .java files found in provided inputs");
        return;
    }

    let mut asts = Vec::new();
    for file in files {
        match parse_java_ast(&file) {
            Ok(ast) => asts.push(Rc::new(ast)),
            Err(e) => match &e.inner {
                java_ast_parser::Error::UnrecognizedEof { .. } => {}
                _ => {
                    eprintln!("failed to parse {}\n{}", file.display(), e);
                    return;
                }
            },
        }
    }

    if asts.is_empty() {
        eprintln!("no parsable .java files found");
        return;
    }

    preprocess_asts(&asts);

    let outputs = generate_pyi_by_package(&asts);

    for (package, contents) in outputs {
        let file_path = package_to_path(&options.out_dir, &package);
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&file_path, contents).unwrap();
        ensure_parent_inits(&file_path, &options.out_dir).unwrap();
        println!("wrote {}", file_path.display());
    }
}

fn package_to_path(out_dir: &Path, package: &str) -> PathBuf {
    let mut path = PathBuf::from(out_dir);

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

fn ensure_parent_inits(file_path: &Path, out_dir: &Path) -> std::io::Result<()> {
    let mut current = file_path.parent();
    while let Some(dir) = current {
        if dir == out_dir {
            break;
        }

        let init_path = dir.join("__init__.py");
        if init_path != file_path && !init_path.exists() {
            fs::write(&init_path, "")?;
        }

        current = dir.parent();
    }

    Ok(())
}

struct CliOptions {
    inputs: Vec<PathBuf>,
    out_dir: PathBuf,
}

fn parse_args(args: Vec<String>) -> Result<CliOptions, String> {
    let mut inputs = Vec::new();
    let mut out_dir = PathBuf::from("out");
    let mut iter = args.into_iter();
    let _program = iter.next();

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-i" | "--input" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --input".to_string())?;
                inputs.push(PathBuf::from(value));
            }
            "-o" | "--out" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --out".to_string())?;
                out_dir = PathBuf::from(value);
            }
            "-h" | "--help" => {
                return Err(String::from("help requested"));
            }
            _ => {
                if arg.starts_with('-') {
                    return Err(format!("unknown option: {}", arg));
                }
                inputs.push(PathBuf::from(arg));
            }
        }
    }

    if inputs.is_empty() {
        return Err(String::from("no inputs provided"));
    }

    Ok(CliOptions { inputs, out_dir })
}

fn usage() -> String {
    [
        "Usage:",
        "  java-to-pyi -i <path> [-i <path> ...] [--out <dir>]",
        "",
        "Options:",
        "  -i, --input <path>      Input file or directory (recurses for .java)",
        "  -o, --out <dir>         Output directory (default: out)",
        "  -h, --help              Show this help",
    ]
    .join("\n")
}

fn collect_java_files(inputs: &[PathBuf]) -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();

    for input in inputs {
        let metadata = fs::metadata(input)
            .map_err(|err| format!("failed to read {}: {}", input.display(), err))?;

        if metadata.is_file() {
            if is_java_file(input) {
                files.push(input.clone());
            }
        } else if metadata.is_dir() {
            collect_java_files_in_dir(input, &mut files)?;
        }
    }

    Ok(files)
}

fn collect_java_files_in_dir(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    for entry in
        fs::read_dir(dir).map_err(|err| format!("failed to read {}: {}", dir.display(), err))?
    {
        let entry = entry.map_err(|err| format!("failed to read {}: {}", dir.display(), err))?;
        let path = entry.path();
        let metadata = entry
            .metadata()
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;

        if metadata.is_dir() {
            collect_java_files_in_dir(&path, files)?;
        } else if metadata.is_file() && is_java_file(&path) {
            files.push(path);
        }
    }

    Ok(())
}

fn is_java_file(path: &Path) -> bool {
    path.extension().and_then(|ext| ext.to_str()) == Some("java")
}
