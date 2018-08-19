use super::*;

/// An iterator over children, created using the `children` function on ASTType
pub struct ChildrenIter<'a> {
    node: &'a ASTType,
    index: usize
}
impl<'a> Iterator for ChildrenIter<'a> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let child = self.node.child(self.index);
        if child.is_some() {
            self.index += 1;
        }
        child
    }
}

impl super::ASTType {
    /// Return an iterator over all the children of this node
    pub fn children(&self) -> ChildrenIter {
        ChildrenIter {
            node: self,
            index: 0
        }
    }

    /// Index the children of this tree. Note: The intended
    /// usage is only for the `children` function. Which node
    /// an index actually refers to is unspecified, so don't
    /// rely on it.
    pub fn child(&self, mut index: usize) -> Option<NodeId> {
        macro_rules! index {
            () => {{
                #[allow(unused_assignments)]
                match index.checked_sub(1) {
                    Some(new) => { index = new; false },
                    None => true
                }
            }};
            ($($node:expr),+) => {{
                return [$(*$node),+].get(index).cloned();
            }};
            (set $set:expr) => {{
                for entry in $set {
                    match entry {
                        SetEntry::Error(_err) => (),
                        SetEntry::Assign(Attribute(attr), _assign, value, _semi) => {
                            for (node, _dot) in attr {
                                if index!() {
                                    return Some(*node);
                                }
                            }
                            if index!() {
                                return Some(*value);
                            }
                        },
                        SetEntry::Inherit(_inherit, from, _idents, _semi) => {
                            if let Some(Parens(_open, from, _close)) = from {
                                return Some(*from);
                            }
                        }
                    }
                }
            }};
        }

        match self {
            ASTType::Interpol { meta: _, multiline: _, parts } => {
                for part in parts {
                    match part {
                        Interpol::AST(node, _meta) => if index!() {
                            return Some(*node);
                        },
                        Interpol::Literal { original: _, content: _ } => ()
                    }
                }
            },
            ASTType::Lambda(arg, _colon, body) => {
                match arg {
                    LambdaArg::Ident(_meta, _name) => (),
                    LambdaArg::Pattern { args: Brackets(_open, args, _close), bind: _, ellipsis: _ } => {
                        for entry in args {
                            if let Some((_meta, node)) = &entry.default {
                                if index!() {
                                    return Some(*node);
                                }
                            }
                        }
                    }
                }
                if index!() {
                    return Some(*body);
                }
            },
            ASTType::List(_open, items, _close) => {
                return items.get(index).cloned();
            },
            ASTType::Parens(Parens(_open, inner, _close)) => index!(inner),
            ASTType::Set { recursive: _, values: Brackets(_open, values, _close) } => index!(set values),
            ASTType::Value(_, _)
                | ASTType::Var(_, _) => (),
            ASTType::Assert(_assert, condition, _semi, rest) => index!(condition, rest),
            ASTType::IfElse { if_meta: _, condition, then_meta: _, then_body, else_meta: _, else_body } =>
                index!(condition, then_body, else_body),
            ASTType::Import(_meta, path) => index!(path),
            ASTType::Let(_let, Brackets(_open, values, _close)) => index!(set values),
            ASTType::LetIn(_let, values, _in, body) => {
                index!(set values);
                index!(body);
            },
            ASTType::With(_with, namespace, _in, body) => index!(namespace, body),
            ASTType::Apply(f, arg) => index!(f, arg),
            ASTType::Dynamic { meta: _, ast, close: _ } => index!(ast),
            ASTType::IndexSet(set, _dot, attr) => index!(set, attr),
            ASTType::Unary(_meta, _op, value) => index!(value),
            ASTType::OrDefault { set, dot: _, attr, or: _, default } => index!(set, attr, default),
            ASTType::Operation(one, _op, two) => index!(one, two)
        }
        None
    }
}
