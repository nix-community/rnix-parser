// TODO: Use optional macro args if they become a thing

#[macro_export]
macro_rules! nix_inner {
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
    (set (rec: $recursive:expr) {}) => {{ AST::EmptySet }};
    (set (rec: $recursive:expr) { $($inner:tt)+ }) => {{
        AST::Set {
            recursive: $recursive,
            values: {{
                let mut vec = Vec::new();
                nix_inner!(entry(vec) $($inner)+);
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
        AST::Lambda(FnArg::Ident(String::from(stringify!($fn))), Box::new(nix_inner!(parse $($body)*)))
    }};
    (parse $($bind:ident @)* {
        $(( exact = $optional:expr ))*
        $($arg:ident $(? ($($default:tt)*))*),*
    }: $($body:tt)*) => {{
        AST::Lambda(
            FnArg::Pattern {
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
        AST::Add(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) - ($($val2:tt)*)) => {{
        AST::Sub(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) * ($($val2:tt)*)) => {{
        AST::Mul(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) / ($($val2:tt)*)) => {{
        AST::Div(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) ++ ($($val2:tt)*)) => {{
        AST::Concat(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) == ($($val2:tt)*)) => {{
        AST::Equal(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) != ($($val2:tt)*)) => {{
        AST::NotEqual(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) <= ($($val2:tt)*)) => {{
        AST::LessOrEq(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) < ($($val2:tt)*)) => {{
        AST::Less(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) >= ($($val2:tt)*)) => {{
        AST::MoreOrEq(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) > ($($val2:tt)*)) => {{
        AST::More(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) merge ($($val2:tt)*)) => {{
        AST::Merge(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) ? ($($val2:tt)*)) => {{
        AST::IsSet(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
    }};
    (parse ($($val1:tt)*) or ($($val2:tt)*)) => {{
        AST::OrDefault(Box::new((nix_inner!(parse $($val1)*), nix_inner!(parse $($val2)*))))
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
    (parse $val:expr) => {{ AST::Value(Value::from($val)) }};
}
#[macro_export]
macro_rules! nix {
    ($($tokens:tt)*) => {{
        #[allow(unused_imports)]
        use crate::{
            nometa::*,
            value::{Anchor, Value}
        };
        nix_inner!(parse $($tokens)*)
    }}
}

#[cfg(test)]
#[test]
fn test_macro() {
    use crate::nometa::*;
    assert_eq!(
        nix!({
            string = ("Hello World");
            number = ((3) * ((4) + (2)));
        }),
        AST::Set {
            recursive: false,
            values: vec![
                SetEntry::Assign(vec![AST::Var("string".into())], AST::Value("Hello World".into())),
                SetEntry::Assign(vec![AST::Var("number".into())], AST::Mul(Box::new((
                    AST::Value(3.into()),
                    AST::Add(Box::new((
                        AST::Value(4.into()),
                        AST::Value(2.into()),
                    )))
                )))),
            ]
        }
    );
}
