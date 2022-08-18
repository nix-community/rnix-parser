use std::{ffi::OsStr, fmt::Write, fs, path::PathBuf};

use expect_test::expect_file;
use rowan::ast::AstNode;

use crate::{
    ast::{self, HasEntry},
    tokenize, Root, SyntaxKind,
};

#[test]
fn interpolation() {
    let root = ast::Root::parse(include_str!("../test_data/parser/success/interpolation.nix"))
        .ok()
        .unwrap();
    let let_in = ast::LetIn::try_from(root.expr().unwrap()).unwrap();
    let set = ast::AttrSet::try_from(let_in.body().unwrap()).unwrap();
    let entry = set.entries().nth(1).unwrap();
    let attrpath_value = ast::AttrpathValue::try_from(entry).unwrap();
    let value = ast::Str::try_from(attrpath_value.value().unwrap()).unwrap();

    match &*value.normalized_parts() {
    &[
        ast::InterpolPart::Literal(ref s1),
        ast::InterpolPart::Interpolation(_),
        ast::InterpolPart::Literal(ref s2),
        ast::InterpolPart::Interpolation(_),
        ast::InterpolPart::Literal(ref s3)
    ]
    if s1 == "The set's x value is: "
        && s2 == "\n\nThis line shall have no indention\n  This line shall be indented by 2\n\n\n"
        && s3 == "\n" => (),
    parts => panic!("did not match: {:#?}", parts)
}
}

#[test]
fn inherit() {
    let root =
        ast::Root::parse(include_str!("../test_data/parser/success/inherit.nix")).ok().unwrap();
    let let_in = ast::LetIn::try_from(root.expr().unwrap()).unwrap();
    let set = ast::AttrSet::try_from(let_in.body().unwrap()).unwrap();
    let inherit = set.inherits().nth(1).unwrap();

    let from = inherit.from().unwrap().expr().unwrap();
    let ident: ast::Ident = ast::Ident::try_from(from).unwrap();
    assert_eq!(ident.syntax().text(), "set");
    let mut children = inherit.idents();
    assert_eq!(children.next().unwrap().syntax().text(), "z");
    assert_eq!(children.next().unwrap().syntax().text(), "a");
    assert!(children.next().is_none());
}

#[test]
fn math() {
    let root = ast::Root::parse(include_str!("../test_data/parser/success/math.nix")).ok().unwrap();
    let op1 = ast::BinOp::try_from(root.expr().unwrap()).unwrap();
    let op2 = ast::BinOp::try_from(op1.lhs().unwrap()).unwrap();
    assert_eq!(op1.operator().unwrap(), ast::BinOpKind::Add);

    let lhs = ast::Literal::try_from(op2.lhs().unwrap()).unwrap();
    assert_eq!(lhs.syntax().text(), "1");

    let rhs = ast::BinOp::try_from(op2.rhs().unwrap()).unwrap();
    assert_eq!(rhs.operator().unwrap(), ast::BinOpKind::Mul);
}

#[test]
fn t_macro() {
    assert_eq!(T![@], SyntaxKind::TOKEN_AT);
    assert!(matches!(SyntaxKind::TOKEN_L_PAREN, T!['(']));
}

fn dir_tests<F>(dir: &str, get_actual: F)
where
    F: Fn(String) -> String,
{
    let base_path: PathBuf = [env!("CARGO_MANIFEST_DIR"), "test_data", dir].iter().collect();
    let success_path = base_path.join("success");
    let error_path = base_path.join("error");

    let entries = success_path.read_dir().unwrap().chain(error_path.read_dir().unwrap());

    for entry in entries {
        let path = entry.unwrap().path();

        if path.extension() != Some(OsStr::new("nix")) {
            continue;
        }

        println!("testing: {}", path.display());

        let mut code = fs::read_to_string(&path).unwrap();
        if code.ends_with('\n') {
            code.truncate(code.len() - 1);
        }

        let actual = get_actual(code);
        expect_file![path.with_extension("expect")].assert_eq(&actual);
    }
}

#[test]
fn parser_dir_tests() {
    dir_tests("parser", |code| {
        let parse = Root::parse(&code);

        let mut actual = String::new();
        for error in parse.errors() {
            writeln!(actual, "error: {}", error).unwrap();
        }
        writeln!(actual, "{:#?}", parse.syntax()).unwrap();

        actual
    })
}

#[test]
fn tokenizer_dir_tests() {
    dir_tests("tokenizer", |code| {
        let mut actual = String::new();
        for (kind, str) in tokenize(&code) {
            writeln!(actual, "{:?}, \"{}\"", kind, str).unwrap();
        }
        actual
    })
}
