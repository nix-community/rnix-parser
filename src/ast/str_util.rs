use crate::kinds::SyntaxKind::*;
use rowan::{ast::AstNode as OtherAstNode, NodeOrToken};

use crate::ast;

use super::support::children_tokens_u;

impl ast::Str {
    pub fn parts(&self) -> Vec<StrPart> {
        let multiline = children_tokens_u(self).next().map_or(false, |t| t.text() == "''");
        let mut is_first_literal = true;
        let mut at_start_of_line = true;
        let mut min_indent = 1000000;
        let mut cur_indent = 0;
        let mut n = 0;
        let mut first_is_literal = false;

        if multiline {
            for child in self.syntax().children_with_tokens() {
                match child {
                    NodeOrToken::Node(_) => {
                        if at_start_of_line {
                            at_start_of_line = false;
                            min_indent = min_indent.min(cur_indent);
                        }
                        n += 1;
                    }
                    NodeOrToken::Token(token) => {
                        if token.kind() == TOKEN_STRING_CONTENT {
                            let mut token_text = token.text();

                            if n == 0 {
                                first_is_literal = true;
                            }

                            if is_first_literal && first_is_literal {
                                is_first_literal = false;
                                if let Some(p) = token_text.find('\n') {
                                    if token_text[0..p].chars().all(|c| c == ' ') {
                                        token_text = &token_text[p + 1..]
                                    }
                                }
                            }

                            for c in token_text.chars() {
                                if at_start_of_line {
                                    if c == ' ' {
                                        cur_indent += 1;
                                    } else if c == '\n' {
                                        cur_indent = 0;
                                    } else {
                                        at_start_of_line = false;
                                        min_indent = min_indent.min(cur_indent);
                                    }
                                } else if c == '\n' {
                                    at_start_of_line = true;
                                    cur_indent = 0;
                                }
                            }

                            n += 1;
                        }
                    }
                }
            }
        }

        let mut parts = Vec::new();
        let mut cur_dropped = 0;
        let mut i = 0;
        is_first_literal = true;
        at_start_of_line = true;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    at_start_of_line = false;
                    cur_dropped = 0;
                    parts.push(StrPart::Interpolation(
                        ast::StrInterpol::cast(node.clone()).unwrap(),
                    ));
                    i += 1;
                }
                NodeOrToken::Token(token) => {
                    if token.kind() == TOKEN_STRING_CONTENT {
                        let mut token_text = token.text();

                        if multiline {
                            if is_first_literal && first_is_literal {
                                is_first_literal = false;
                                if let Some(p) = token_text.find('\n') {
                                    if token_text[0..p].chars().all(|c| c == ' ') {
                                        token_text = &token_text[p + 1..];
                                        if token_text.is_empty() {
                                            i += 1;
                                            continue;
                                        }
                                    }
                                }
                            }

                            let mut str = String::new();
                            for c in token_text.chars() {
                                if at_start_of_line {
                                    if c == ' ' {
                                        if cur_dropped >= min_indent {
                                            str.push(c);
                                        }
                                        cur_dropped += 1;
                                    } else if c == '\n' {
                                        cur_dropped = 0;
                                        str.push(c);
                                    } else {
                                        at_start_of_line = false;
                                        cur_dropped = 0;
                                        str.push(c);
                                    }
                                } else {
                                    str.push(c);
                                    if c == '\n' {
                                        at_start_of_line = true;
                                    }
                                }
                            }

                            if i == n - 1 {
                                if let Some(p) = str.rfind('\n') {
                                    if str[p + 1..].chars().all(|c| c == ' ') {
                                        str.truncate(p + 1);
                                    }
                                }
                            }

                            parts.push(StrPart::Literal(unescape(&str, multiline)));
                            i += 1;
                        } else {
                            parts.push(StrPart::Literal(unescape(token_text, multiline)));
                        }
                    }
                }
            }
        }

        parts
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrPart {
    Literal(String),
    Interpolation(ast::StrInterpol),
}

/// Interpret escape sequences in the nix string and return the converted value
pub fn unescape(input: &str, multiline: bool) -> String {
    let mut output = String::new();
    let mut input = input.chars().peekable();
    loop {
        match input.next() {
            None => break,
            Some('"') if !multiline => break,
            Some('\\') if !multiline => match input.next() {
                None => break,
                Some('n') => output.push('\n'),
                Some('r') => output.push('\r'),
                Some('t') => output.push('\t'),
                Some(c) => output.push(c),
            },
            Some('\'') if multiline => match input.next() {
                None => {
                    output.push('\'');
                }
                Some('\'') => match input.peek() {
                    Some('\'') => {
                        input.next().unwrap();
                        output.push_str("''");
                    }
                    Some('$') => {
                        input.next().unwrap();
                        output.push('$');
                    }
                    Some('\\') => {
                        input.next().unwrap();
                        match input.next() {
                            None => break,
                            Some('n') => output.push('\n'),
                            Some('r') => output.push('\r'),
                            Some('t') => output.push('\t'),
                            Some(c) => output.push(c),
                        }
                    }
                    _ => break,
                },
                Some(c) => {
                    output.push('\'');
                    output.push(c);
                }
            },
            Some(c) => output.push(c),
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use crate::Root;

    use super::*;

    #[test]
    fn string_unescapes() {
        assert_eq!(unescape(r#"Hello\n\"World\" :D"#, false), "Hello\n\"World\" :D");
        assert_eq!(unescape(r#"\"Hello\""#, false), "\"Hello\"");

        assert_eq!(unescape(r#"Hello''\n'''World''' :D"#, true), "Hello\n''World'' :D");
        assert_eq!(unescape(r#""Hello""#, true), "\"Hello\"");
    }
    #[test]
    fn parts_leading_ws() {
        let inp = "''\n  hello\n  world''";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Str(str) => {
                assert_eq!(str.parts(), vec![StrPart::Literal("hello\nworld".to_string())])
            }
            _ => unreachable!(),
        }
    }
    #[test]
    fn parts_trailing_ws_single_line() {
        let inp = "''hello ''";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Str(str) => {
                assert_eq!(str.parts(), vec![StrPart::Literal("hello ".to_string())])
            }
            _ => unreachable!(),
        }
    }
    #[test]
    fn parts_trailing_ws_multiline() {
        let inp = "''hello\n ''";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Str(str) => {
                assert_eq!(str.parts(), vec![StrPart::Literal("hello\n".to_string())])
            }
            _ => unreachable!(),
        }
    }
    #[test]
    fn parts() {
        use crate::{NixLanguage, SyntaxNode};
        use rowan::{GreenNodeBuilder, Language};

        fn string_node(content: &str) -> ast::Str {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(NixLanguage::kind_to_raw(NODE_STRING));
            builder.token(NixLanguage::kind_to_raw(TOKEN_STRING_START), "''");
            builder.token(NixLanguage::kind_to_raw(TOKEN_STRING_CONTENT), content);
            builder.token(NixLanguage::kind_to_raw(TOKEN_STRING_END), "''");
            builder.finish_node();

            ast::Str::cast(SyntaxNode::new_root(builder.finish())).unwrap()
        }

        let txtin = r#"
                        |trailing-whitespace
                              |trailing-whitespace
                    This is a multiline string :D
                      indented by two
                    \'\'\'\'\
                    ''${ interpolation was escaped }
                    two single quotes: '''
                    three single quotes: ''''
                "#
        .replace("|trailing-whitespace", "");

        if let [StrPart::Literal(lit)] = &ast::Str::parts(&string_node(txtin.as_str()))[..] {
            assert_eq!(lit,
                // Get the below with nix repl
                "    \n          \nThis is a multiline string :D\n  indented by two\n\\'\\'\\'\\'\\\n${ interpolation was escaped }\ntwo single quotes: ''\nthree single quotes: '''\n"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn parts_ast() {
        fn assert_eq_ast_ctn(it: &mut dyn Iterator<Item = StrPart>, x: &str) {
            let tmp = it.next().expect("unexpected EOF");
            if let StrPart::Interpolation(astn) = tmp {
                assert_eq!(astn.expr().unwrap().syntax().to_string(), x);
            } else {
                unreachable!("unexpected literal {:?}", tmp);
            }
        }

        let inp = r#"''

        This version of Nixpkgs requires Nix >= ${requiredVersion}, please upgrade:

        - If you are running NixOS, `nixos-rebuild' can be used to upgrade your system.

        - Alternatively, with Nix > 2.0 `nix upgrade-nix' can be used to imperatively
          upgrade Nix. You may use `nix-env --version' to check which version you have.

        - If you installed Nix using the install script (https://nixos.org/nix/install),
          it is safe to upgrade by running it again:

              curl -L https://nixos.org/nix/install | sh

        For more information, please see the NixOS release notes at
        https://nixos.org/nixos/manual or locally at
        ${toString ./nixos/doc/manual/release-notes}.

        If you need further help, see https://nixos.org/nixos/support.html
      ''"#;
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Str(s) => {
                let mut it = s.parts().into_iter();
                assert_eq!(
                    it.next().unwrap(),
                    StrPart::Literal("\nThis version of Nixpkgs requires Nix >= ".to_string())
                );
                assert_eq_ast_ctn(&mut it, "requiredVersion");
                assert_eq!(it.next().unwrap(), StrPart::Literal(
                        ", please upgrade:\n\n- If you are running NixOS, `nixos-rebuild' can be used to upgrade your system.\n\n- Alternatively, with Nix > 2.0 `nix upgrade-nix' can be used to imperatively\n  upgrade Nix. You may use `nix-env --version' to check which version you have.\n\n- If you installed Nix using the install script (https://nixos.org/nix/install),\n  it is safe to upgrade by running it again:\n\n      curl -L https://nixos.org/nix/install | sh\n\nFor more information, please see the NixOS release notes at\nhttps://nixos.org/nixos/manual or locally at\n".to_string()
                    ));
                assert_eq_ast_ctn(&mut it, "toString ./nixos/doc/manual/release-notes");
                assert_eq!(
                    it.next().unwrap(),
                    StrPart::Literal(
                        ".\n\nIf you need further help, see https://nixos.org/nixos/support.html\n"
                            .to_string()
                    )
                );
            }
            _ => unreachable!(),
        }
    }
}
