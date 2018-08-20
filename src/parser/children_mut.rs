//! Convenience functions for returning all the children of an AST node.
//! Useful for traversing without looking up each and every single possible
//! type.

// This file is copy-pasted by generate-mut.sh into children-mut.rs.
// Mark all lines that should be dropped in that file with a *trailing* // IMMUT.
// Optionally, you can also choose to start/end while sections with // <<IMMUT and // IMMUT>>
// Mark all lines that should only be in that file with a *leading* // MUT.

use super::*;

/// A child: Either metadata, or another node
pub enum ChildMut<'a> {
    Meta(&'a mut Meta),
    Node(NodeId)
}
impl<'a> From<&'a mut Meta> for ChildMut<'a> {
    fn from(meta: &'a mut Meta) -> Self {
        ChildMut::Meta(meta)
    }
}
impl<'a> From<&'a mut NodeId> for ChildMut<'static> {
    fn from(node: &'a mut NodeId) -> Self {
        ChildMut::Node(*node)
    }
}

impl super::ASTType {
    ///// Return an iterator over all the children of this node
    //pub fn children(&mut self) -> ChildrenIter {
    //    ChildrenIter {
    //        node: self,
    //        index: 0
    //    }
    //}

    /// Index the children of this tree. Note: The intended
    /// usage is only for the `children` function. Which node
    /// an index actually refers to is unspecified, so don't
    /// rely on it.
    pub fn child_mut(&mut self, mut index: usize) -> Option<ChildMut> {
        macro_rules! index {
            () => {{
                #[allow(unused_assignments)]
                match index.checked_sub(1) {
                    Some(new) => { index = new; false },
                    None => true
                }
            }};
            (maybe $node:expr) => {{
                if index!() {
                    return Some($node.into());
                }
            }};
            (set $set:expr) => {{
                for entry in $set {
                    match entry {
                        SetEntry::Error(_err) => (),
                        SetEntry::Assign(Attribute(attr), assign, value, semi) => {
                            for (ident, dot) in attr {
                                index!(maybe ident);
                                if let Some(dot) = dot {
                                    index!(maybe dot);
                                }
                            }
                            index!(maybe assign);
                            index!(maybe value);
                            index!(maybe semi);
                        },
                        SetEntry::Inherit(inherit, from, idents, semi) => {
                            index!(maybe inherit);
                            if let Some(Parens(open, from, close)) = from {
                                index!(maybe open);
                                index!(maybe from);
                                index!(maybe close);
                            }
                            for (meta, _ident) in idents {
                                index!(maybe meta);
                            }
                            index!(maybe semi);
                        }
                    }
                }
            }};
            ($($node:expr),+) => {{
                $(
                    index!(maybe $node);
                )+
            }};
        }

        match self {
            ASTType::Interpol { meta, multiline: _, parts } => {
                index!(maybe meta);
                for part in parts {
                    match part {
                        Interpol::AST(node, close) => {
                            index!(maybe node);
                            index!(maybe close);
                        },
                        Interpol::Literal { original: _, content: _ } => ()
                    }
                }
            },
            ASTType::Lambda(arg, colon, body) => {
                match arg {
                    LambdaArg::Ident(meta, _name) => {
                        index!(maybe meta);
                    },
                    LambdaArg::Pattern { args: Brackets(open, args, close), bind, ellipsis } => {
                        index!(maybe open);
                        for entry in args {
                            if let Some((meta, default)) = &mut entry.default {
                                index!(maybe meta);
                                index!(maybe default);
                            }
                        }
                        index!(maybe close);
                        if let Some(bind) = bind {
                            index!(maybe &mut bind.at);
                            index!(maybe &mut bind.ident);
                        }
                        if let Some(ellipsis) = ellipsis {
                            index!(maybe ellipsis);
                        }
                    }
                }
                index!(maybe colon);
                index!(maybe body);
            },
            ASTType::List(open, items, close) => {
                index!(maybe open);
                for item in items {
                    index!(maybe item);
                }
                index!(maybe close);
            },
            ASTType::Parens(Parens(open, inner, close)) => index!(open, inner, close),
            ASTType::Set { recursive, values: Brackets(open, values, close) } => {
                if let Some(recursive) = recursive {
                    index!(maybe recursive);
                }
                index!(maybe open);
                index!(set values);
                index!(maybe close);
            },
            ASTType::Value(meta, _)
                | ASTType::Var(meta, _) => index!(meta),
            ASTType::Assert(assert, condition, semi, rest) => index!(assert, condition, semi, rest),
            ASTType::IfElse { if_meta, condition, then_meta, then_body, else_meta, else_body } =>
                index!(if_meta, condition, then_meta, then_body, else_meta, else_body),
            ASTType::Import(meta, path) => index!(meta, path),
            ASTType::Let(let_, Brackets(open, values, close)) => {
                index!(maybe let_);
                index!(maybe open);
                index!(set values);
                index!(maybe close);
            },
            ASTType::LetIn(let_, values, in_, body) => {
                index!(maybe let_);
                index!(set values);
                index!(maybe in_);
                index!(body);
            },
            ASTType::With(with, namespace, in_, body) => index!(with, namespace, in_, body),
            ASTType::Apply(f, arg) => index!(f, arg),
            ASTType::Dynamic { meta, ast, close } => index!(meta, ast, close),
            ASTType::IndexSet(set, dot, attr) => index!(set, dot, attr),
            ASTType::Unary(meta, _op, value) => index!(meta, value),
            ASTType::OrDefault { set, dot, attr, or, default } => index!(set, dot, attr, or, default),
            ASTType::Operation(one, _op, two) => index!(one, two)
        }
        None
    }
}
