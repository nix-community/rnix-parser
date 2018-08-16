// TODO: Use optional macro args if they become a thing

/// Technical detail, don't mind this macro
// #[macro_export]
macro_rules! nix_inner {
    (op ($($val1:tt)*) ($op:expr) ($($val2:tt)*)) => {{
        AST::Operation(Box::new((
            nix_inner!(parse $($val1)*),
            $op,
            nix_inner!(parse $($val2)*)
        )))
    }};
    (entry($vec:expr)) => {};
    (entry($vec:expr) $ident:ident = ($($val:tt)*); $($remaining:tt)*) => {
        $vec.push(SetEntry::Assign(vec![AST::Var(String::from(stringify!($ident)))], nix_inner!(parse $($val)*)));
        nix_inner!(entry($vec) $($remaining)*);
    };
    (entry($vec:expr) $(($($ident:tt)*)).* = ($($val:tt)*); $($remaining:tt)*) => {
        $vec.push(SetEntry::Assign(
            vec![$(nix_inner!(parse $($ident)*)),*],
            nix_inner!(parse $($val)*)
        ));
        nix_inner!(entry($vec) $($remaining)*);
    };
    (entry($vec:expr) inherit ($($from:tt)*) $($var:ident)*; $($remaining:tt)*) => {
        $vec.push(SetEntry::Inherit(
            Some(nix_inner!(parse $($from)*)),
            vec![$(String::from(stringify!($var))),*]
        ));
        nix_inner!(entry($vec) $($remaining)*);
    };
    (entry($vec:expr) inherit $($var:ident)*; $($remaining:tt)*) => {{
        $vec.push(SetEntry::Inherit(None, vec![$(String::from(stringify!($var))),*]));
        nix_inner!(entry($vec) $($remaining)*);
    }};
    (set (rec: $recursive:expr) { $($inner:tt)* }) => {{
        AST::Set {
            recursive: $recursive,
            values: {{
                let mut vec = Vec::new();
                nix_inner!(entry(vec) $($inner)*);
                vec
            }}
        }
    }};
    (pat_default) => {{ None }};
    (pat_default $($stuff:tt)+) => {{ Some(nix_inner!(parse $($stuff)+)) }};
    (pat_bind) => {{ None }};
    (pat_bind $name:ident) => {{ Some(String::from(stringify!($name))) }};
    (pat_exact) => {{ true }};
    (pat_exact $value:expr) => {{ $value }};
    (parse { $($token:tt)* }) => {{ nix_inner!(set (rec: false) { $($token)* }) }};
    (parse rec { $($token:tt)* }) => {{ nix_inner!(set (rec: true) { $($token)* }) }};
    (parse let {
        $($ident:ident = ($($val:tt)*);)*
    } in $($remaining:tt)*) => {{
        AST::LetIn(
            vec![$(SetEntry::Assign(vec![AST::Var(String::from(stringify!($ident)))], nix_inner!(parse $($val)*))),*],
            Box::new(nix_inner!(parse $($remaining)*))
        )
    }};
    (parse let {
        $($ident:ident = ($($val:tt)*);)*
    }) => {{
        AST::Let(vec![$(SetEntry::Assign(vec![AST::Var(String::from(stringify!($ident)))], nix_inner!(parse $($val)*))),*])
    }};
    (parse with ($($namespace:tt)*); $($remaining:tt)*) => {{
        AST::With(Box::new((
            nix_inner!(parse $($namespace)*),
            nix_inner!(parse $($remaining)*)
        )))
    }};
    (parse import ($($path:tt)*)) => {{
        AST::Import(Box::new(nix_inner!(parse $($path)*)))
    }};
    (parse if ($($cond:tt)*) then ($($body:tt)*) else $($otherwise:tt)*) => {{
        AST::IfElse(Box::new((
            nix_inner!(parse $($cond)*),
            nix_inner!(parse $($body)*),
            nix_inner!(parse $($otherwise)*)
        )))
    }};
    (parse assert ($($cond:tt)*); $($remaining:tt)*) => {{
        AST::Assert(Box::new((
            nix_inner!(parse $($cond)*),
            nix_inner!(parse $($remaining)*)
        )))
    }};
    (parse $fn:ident: $($body:tt)*) => {{
        AST::Lambda(LambdaArg::Ident(String::from(stringify!($fn))), Box::new(nix_inner!(parse $($body)*)))
    }};
    (parse $($bind:ident @)* {
        $(( exact = $optional:expr ))*
        $($arg:ident $(? ($($default:tt)*))*),*
    }: $($body:tt)*) => {{
        AST::Lambda(
            LambdaArg::Pattern {
                args: vec![
                    $(PatEntry(String::from(stringify!($arg)), nix_inner!(pat_default $($($default)*)*))),*
                ],
                bind: nix_inner!(pat_bind $($bind)*),
                exact: nix_inner!(pat_exact $($optional)*)
            },
            Box::new(nix_inner!(parse $($body)*))
        )
    }};
    (parse ($($val1:tt)*) + ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Add) ($($val2)*))
    }};
    (parse ($($val1:tt)*) - ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Sub) ($($val2)*))
    }};
    (parse ($($val1:tt)*) * ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Mul) ($($val2)*))
    }};
    (parse ($($val1:tt)*) / ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Div) ($($val2)*))
    }};
    (parse ($($val1:tt)*) ++ ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Concat) ($($val2)*))
    }};
    (parse ($($val1:tt)*) == ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Equal) ($($val2)*))
    }};
    (parse ($($val1:tt)*) -> ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Implication) ($($val2)*))
    }};
    (parse ($($val1:tt)*) != ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::NotEqual) ($($val2)*))
    }};
    (parse ($($val1:tt)*) <= ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::LessOrEq) ($($val2)*))
    }};
    (parse ($($val1:tt)*) < ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Less) ($($val2)*))
    }};
    (parse ($($val1:tt)*) >= ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::MoreOrEq) ($($val2)*))
    }};
    (parse ($($val1:tt)*) > ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::More) ($($val2)*))
    }};
    (parse ($($val1:tt)*) merge ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::Merge) ($($val2)*))
    }};
    (parse ($($val1:tt)*) ? ($($val2:tt)*)) => {{
        nix_inner!(op ($($val1)*) (Operator::IsSet) ($($val2)*))
    }};
    (parse ($($set:tt)*).($($index:tt)*) or ($($default:tt)*)) => {{
        AST::OrDefault(Box::new((
            nix_inner!(parse $($set)*),
            nix_inner!(parse $($index)*),
            nix_inner!(parse $($default)*)
        )))
    }};
    (parse ($($fn:tt)*) ($($arg:tt)*)) => {{
        AST::Apply(Box::new((nix_inner!(parse $($fn)*), nix_inner!(parse $($arg)*))))
    }};
    (parse ($($set:tt)*).$field:ident) => {{
        AST::IndexSet(Box::new((nix_inner!(parse $($set)*), AST::Var(String::from(stringify!($field))))))
    }};
    (parse [$(($($item:tt)*))*]) => {{
        AST::List(vec![$(nix_inner!(parse $($item)*)),*])
    }};
    (parse -$($val:tt)*) => {{
        AST::Negate(Box::new((nix_inner!(parse $($val)*))))
    }};
    (parse !$($val:tt)*) => {{
        AST::Invert(Box::new((nix_inner!(parse $($val)*))))
    }};
    (parse dyn {$($val:tt)*}) => {{
        AST::Dynamic(Box::new((nix_inner!(parse $($val)*))))
    }};
    (parse true) => {{ AST::Value(Value::Bool(true)) }};
    (parse false) => {{ AST::Value(Value::Bool(false)) }};
    (parse null) => {{ AST::Value(Value::Null) }};
    (parse $val:ident) => {{
        AST::Var(String::from(stringify!($val)))
    }};
    (parse ./$val:expr) => {{
        AST::Value(Value::Path(Anchor::Relative, String::from(concat!("./", $val))))
    }};
    (parse raw $ast:expr) => {{ $ast }};
    (parse multiline $val:expr) => {{ AST::Value(Value::Str {
        multiline: true,
        content: $val.into()
    }) }};
    (parse $val:expr) => {{ AST::Value(Value::from($val)) }};
}
/// A macro that turns Nix-like code into a Nix AST.
/// Useful for unit testing, where typing the AST manually can get annoying.
// #[macro_export]
// Can't export because `use crate::` isn't valid from outside, and
// I can't use `use rnix::` because that's not valid from inside.
macro_rules! nix {
    ($($tokens:tt)*) => {{
        #[allow(unused_imports)]
        use crate::{
            parser::nometa::*,
            value::{Anchor, Value}
        };
        nix_inner!(parse $($tokens)*)
    }}
}

#[cfg(test)]
#[test]
fn test_macro() {
    use crate::parser::nometa::*;
    assert_eq!(
        nix!({
            string = ("Hello World");
            number = ((3) * ((4) + (2)));
        }),
        AST::Set {
            recursive: false,
            values: vec![
                SetEntry::Assign(vec![AST::Var("string".into())], AST::Value("Hello World".into())),
                SetEntry::Assign(vec![AST::Var("number".into())], AST::Operation(Box::new((
                    AST::Value(3.into()),
                    Operator::Mul,
                    AST::Operation(Box::new((
                        AST::Value(4.into()),
                        Operator::Add,
                        AST::Value(2.into()),
                    )))
                )))),
            ]
        }
    );
}
