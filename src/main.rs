use std::rc::Rc;

use crate::preprocess::{parse_java_ast, preprocess_asts};

mod index_table;
mod preprocess;

fn main() {
    let asts = [
        parse_java_ast("java/example1.java").unwrap(),
        parse_java_ast("java/example2.java").unwrap(),
    ]
    .into_iter()
    .map(Rc::new)
    .collect::<Box<_>>();

    preprocess_asts(&asts, true).unwrap();

    dbg!(asts);
}
