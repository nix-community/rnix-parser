use crate::kinds::SyntaxKind::*;
use rowan::{ast::AstNode as OtherAstNode, NodeOrToken};

use crate::ast;

use super::{support::children_tokens_u, AstToken, InterpolPart, StrContent};

impl ast::Str {
    pub fn parts(&self) -> impl Iterator<Item = InterpolPart<StrContent>> {
        self.syntax().children_with_tokens().filter_map(|child| match child {
            NodeOrToken::Token(token) if token.kind() == TOKEN_STRING_CONTENT => {
                Some(InterpolPart::Literal(StrContent::cast(token).unwrap()))
            }
            NodeOrToken::Token(token) => {
                assert!(token.kind() == TOKEN_STRING_START || token.kind() == TOKEN_STRING_END);
                None
            }
            NodeOrToken::Node(node) => {
                assert_eq!(node.kind(), NODE_INTERPOL);
                Some(InterpolPart::Interpolation(ast::Interpol::cast(node.clone()).unwrap()))
            }
        })
    }

    pub fn normalized_parts(&self) -> Vec<InterpolPart<String>> {
        let mut parsed_parts = Vec::new();
        let mut literals = 0;
        let mut common = std::usize::MAX;
        let multiline = children_tokens_u(self).next().map_or(false, |t| t.text() == "''");
        let mut last_was_ast = false;

        let mut parts = self.parts().peekable();

        while let Some(part) = parts.next() {
            match part {
                InterpolPart::Literal(literal) => {
                    let text: &str = literal.syntax().text();

                    let line_count = text.lines().count();
                    let next_is_ast = matches!(parts.peek(), Some(InterpolPart::Interpolation(_)));
                    for (i, line) in text.lines().enumerate().skip(if last_was_ast { 1 } else { 0 })
                    {
                        let indent: usize = indention(line).count();
                        if (i != line_count - 1 || !next_is_ast) && indent == line.chars().count() {
                            // line is empty and not the start of an
                            // interpolation, ignore indention
                            continue;
                        }
                        common = common.min(indent);
                    }
                    parsed_parts.push(InterpolPart::Literal(text.to_string()));
                    literals += 1;
                }
                InterpolPart::Interpolation(interpol) => {
                    parsed_parts.push(InterpolPart::Interpolation(interpol));
                    last_was_ast = true;
                }
            }
        }

        let mut i = 0;
        for part in parsed_parts.iter_mut() {
            if let InterpolPart::Literal(ref mut text) = part {
                if multiline {
                    *text = remove_indent(text, i == 0, common);
                    // If the last literal is of the form `X\nY`, exclude Y if it consists solely of spaces
                    if i == literals - 1 {
                        match text.rfind('\n') {
                            Some(p) if text[p + 1..].chars().all(|c| c == ' ') => {
                                text.truncate(p + 1);
                            }
                            _ => {}
                        }
                    }
                }
                *text = unescape(text, multiline);
                i += 1;
            }
        }

        parsed_parts
    }
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
                None => break,
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

pub(crate) fn indention(s: &str) -> impl Iterator<Item = char> + '_ {
    s.chars().take_while(|&c| c != '\n' && c.is_whitespace())
}

/// Remove common indention in string
pub fn remove_common_indent(input: &str) -> String {
    let mut common = std::usize::MAX;
    for line in input.lines() {
        let indent = indention(line).count();
        if line.chars().count() == indent {
            // line is empty, ignore indention
            continue;
        }
        common = common.min(indent);
    }

    remove_indent(input, true, common)
}

/// Remove a specified max value of indention from each line in a string after
/// a specified starting point
pub fn remove_indent(input: &str, initial: bool, indent: usize) -> String {
    let mut output = String::new();
    let mut start = 0;
    if initial {
        // If the first line is whitespace, ignore it completely
        let iter = input.chars().take_while(|&c| c != '\n');
        if iter.clone().all(char::is_whitespace) {
            start += iter.map(char::len_utf8).sum::<usize>() + /* newline */ 1;
            if start >= input.len() {
                // There's nothing after this whitespace line
                return output;
            }
        } else {
            // Otherwise, skip like normal
            start += indention(input).take(indent).map(char::len_utf8).sum::<usize>();
        }
    }
    loop {
        start += indention(&input[start..]).take(indent).map(char::len_utf8).sum::<usize>();
        let end = input[start..].find('\n').map(|i| start + i + 1);
        {
            let end = end.unwrap_or(input.len());
            output.push_str(&input[start..end]);
        }
        start = match end {
            Some(end) => end,
            None => break,
        };
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
    fn string_remove_common_indent() {
        assert_eq!(remove_common_indent("\n  \n    \n \n "), "\n\n\n");
        assert_eq!(remove_common_indent("\n  \n    \n a\n"), " \n   \na\n");
        assert_eq!(remove_common_indent("  \n    \n a\n"), "   \na\n");
    }
    #[test]
    fn parts_trailing_ws_single_line() {
        let inp = "''hello ''";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Str(str) => {
                assert_eq!(
                    str.normalized_parts(),
                    vec![InterpolPart::Literal("hello ".to_string())]
                )
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
                assert_eq!(
                    str.normalized_parts(),
                    vec![InterpolPart::Literal("hello\n".to_string())]
                )
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

        if let [InterpolPart::Literal(lit)] =
            &ast::Str::normalized_parts(&string_node(txtin.as_str()))[..]
        {
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
        fn assert_eq_ast_ctn(it: &mut dyn Iterator<Item = InterpolPart<String>>, x: &str) {
            let tmp = it.next().expect("unexpected EOF");
            if let InterpolPart::Interpolation(astn) = tmp {
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
                let mut it = s.normalized_parts().into_iter();
                assert_eq!(
                    it.next().unwrap(),
                    InterpolPart::Literal("\nThis version of Nixpkgs requires Nix >= ".to_string())
                );
                assert_eq_ast_ctn(&mut it, "requiredVersion");
                assert_eq!(it.next().unwrap(), InterpolPart::Literal(
                        ", please upgrade:\n\n- If you are running NixOS, `nixos-rebuild' can be used to upgrade your system.\n\n- Alternatively, with Nix > 2.0 `nix upgrade-nix' can be used to imperatively\n  upgrade Nix. You may use `nix-env --version' to check which version you have.\n\n- If you installed Nix using the install script (https://nixos.org/nix/install),\n  it is safe to upgrade by running it again:\n\n      curl -L https://nixos.org/nix/install | sh\n\nFor more information, please see the NixOS release notes at\nhttps://nixos.org/nixos/manual or locally at\n".to_string()
                    ));
                assert_eq_ast_ctn(&mut it, "toString ./nixos/doc/manual/release-notes");
                assert_eq!(
                    it.next().unwrap(),
                    InterpolPart::Literal(
                        ".\n\nIf you need further help, see https://nixos.org/nixos/support.html\n"
                            .to_string()
                    )
                );
            }
            _ => unreachable!(),
        }
    }
}
