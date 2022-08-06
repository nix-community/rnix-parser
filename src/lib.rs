#[macro_use]
mod macros;
pub mod ast;
mod kinds;
pub mod parser;
mod token_set;
pub mod tokenizer;

use std::marker::PhantomData;

pub use self::{kinds::SyntaxKind, tokenizer::tokenize};

use ast::AstNode;
use parser::ParseError;
use rowan::GreenNode;
pub use rowan::{NodeOrToken, TextRange, TextSize, TokenAtOffset, WalkEvent};
pub(crate) use token_set::TokenSet;

use self::tokenizer::Tokenizer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NixLanguage {}

impl rowan::Language for NixLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        let discriminant: u16 = raw.0;
        assert!(discriminant <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(discriminant) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<NixLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NixLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<NixLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<NixLanguage>;

pub use ast::Root;

impl Root {
    pub fn parse(s: &str) -> Parse<Root> {
        let (green, errors) = parser::parse(Tokenizer::new(s));
        Parse { green, errors, _ty: PhantomData }
    }
}

/// The result of a parse
#[derive(Clone)]
pub struct Parse<T> {
    green: GreenNode,
    errors: Vec<ParseError>,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Parse<T> {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
}

impl<T: AstNode> Parse<T> {
    pub fn tree(&self) -> T {
        T::cast(self.syntax()).unwrap()
    }

    /// Return all errors in the tree, if any
    pub fn errors(&self) -> &[ParseError] {
        &*self.errors
    }

    /// Either return the first error in the tree, or if there are none return self
    pub fn ok(self) -> Result<T, ParseError> {
        if let Some(err) = self.errors().first() {
            return Err(err.clone());
        }
        Ok(self.tree())
    }
}

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```ignore
/// match_ast! {
///     match node {
///         ast::Apply(it) => { ... },
///         ast::Assert(it) => { ... },
///         ast::IfElse(it) => { ... },
///         _ => None,
///     }
/// }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

#[cfg(test)]
mod tests {
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

        match &*value.parts() {
            &[
                ast::StrPart::Literal(ref s1),
                ast::StrPart::Interpolation(_),
                ast::StrPart::Literal(ref s2),
                ast::StrPart::Interpolation(_),
                ast::StrPart::Literal(ref s3)
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
        let root =
            ast::Root::parse(include_str!("../test_data/parser/success/math.nix")).ok().unwrap();
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
        assert!(matches!(SyntaxKind::TOKEN_L_PAREN, T!["("]));
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
}
