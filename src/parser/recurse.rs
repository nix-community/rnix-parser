macro_rules! recurse {
    ($var:expr, $ast:ident, $f:expr) => {
        macro_rules! recurse_inner {
            (tuple, $tuple:expr) => {{
                $tuple.0.recurse($f)?;
                $tuple.1.recurse($f)?;
            }};
            (triple, $triple:expr) => {{
                $triple.0.recurse($f)?;
                $triple.1.recurse($f)?;
                $triple.2.recurse($f)?;
            }};
            (set, $values:expr) => {{
                for entry in $values {
                    match entry {
                        SetEntry::Assign(_key, value) => value.recurse($f)?,
                        SetEntry::Inherit(Some(from), _keys) => from.recurse($f)?,
                        SetEntry::Inherit(None, _keys) => ()
                    }
                }
            }};
        }
        match $var {
            $ast::Interpol { multiline: _, parts } => {
                for part in parts {
                    match part {
                        // Yes, could be a `if let`, but I want errors if I
                        // ever add more to this enum
                        Interpol::AST(ast) => ast.recurse($f)?,
                        Interpol::Literal(_) => (),
                    }
                }
            },
            $ast::Lambda(arg, body) => {
                match arg {
                    LambdaArg::Ident(_) => (),
                    LambdaArg::Pattern { args, bind: _, exact: _ } => {
                        for PatEntry(_arg, default) in args {
                            if let Some(default) = default {
                                default.recurse($f)?;
                            }
                        }
                    }
                }
                body.recurse($f)?;
            },
            $ast::List(items) => {
                for item in items {
                    item.recurse($f)?;
                }
            },
            $ast::Set { recursive: _, values } => recurse_inner!(set, values),
            $ast::Value(_) | $ast::Var(_) => (),
            $ast::Assert(inner) => recurse_inner!(tuple, inner),
            $ast::IfElse(inner) => recurse_inner!(triple, inner),
            $ast::Import(inner) => inner.recurse($f)?,
            $ast::Let(values) => recurse_inner!(set, values),
            $ast::LetIn(values, body) => {
                recurse_inner!(set, values);
                body.recurse($f)?;
            },
            $ast::With(inner) => recurse_inner!(tuple, inner),

            $ast::Apply(inner) => recurse_inner!(tuple, inner),
            $ast::Concat(inner) => recurse_inner!(tuple, inner),
            $ast::Dynamic(inner) => inner.recurse($f)?,
            $ast::IndexSet(inner) => recurse_inner!(tuple, inner),
            $ast::Invert(inner) => inner.recurse($f)?,
            $ast::IsSet(inner) => recurse_inner!(tuple, inner),
            $ast::Merge(inner) => recurse_inner!(tuple, inner),
            $ast::Negate(inner) => inner.recurse($f)?,
            $ast::OrDefault(inner) => recurse_inner!(triple, inner),

            $ast::Add(inner) => recurse_inner!(tuple, inner),
            $ast::Sub(inner) => recurse_inner!(tuple, inner),
            $ast::Mul(inner) => recurse_inner!(tuple, inner),
            $ast::Div(inner) => recurse_inner!(tuple, inner),

            $ast::And(inner) => recurse_inner!(tuple, inner),
            $ast::Equal(inner) => recurse_inner!(tuple, inner),
            $ast::Implication(inner) => recurse_inner!(tuple, inner),
            $ast::Less(inner) => recurse_inner!(tuple, inner),
            $ast::LessOrEq(inner) => recurse_inner!(tuple, inner),
            $ast::More(inner) => recurse_inner!(tuple, inner),
            $ast::MoreOrEq(inner) => recurse_inner!(tuple, inner),
            $ast::NotEqual(inner) => recurse_inner!(tuple, inner),
            $ast::Or(inner) => recurse_inner!(tuple, inner)
        }
    }
}

impl super::AST {
    /// Recurse the AST and run a function on each node
    pub fn recurse<T, F>(&self, f: &mut F) -> Result<(), T>
        where F: FnMut(&Self) -> Result<(), T>
    {
        use super::*;
        (*f)(self)?;

        recurse!(&self.1, ASTType, f);
        Ok(())
    }
}
impl super::nometa::AST {
    /// Recurse the AST and run a function on each node
    pub fn recurse<T, F>(&self, f: &mut F) -> Result<(), T>
        where F: FnMut(&Self) -> Result<(), T>
    {
        use super::nometa::*;
        (*f)(self)?;

        recurse!(self, AST, f);
        Ok(())
    }
}
