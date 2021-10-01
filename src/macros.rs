#[macro_export]
#[rustfmt::skip]
macro_rules! T {
    (assert)  => ($crate::SyntaxKind::TOKEN_ASSERT);
    (else)    => ($crate::SyntaxKind::TOKEN_ELSE);
    (if)      => ($crate::SyntaxKind::TOKEN_IF);
    (in)      => ($crate::SyntaxKind::TOKEN_IN);
    (inherit) => ($crate::SyntaxKind::TOKEN_INHERIT);
    (let)     => ($crate::SyntaxKind::TOKEN_LET);
    (rec)     => ($crate::SyntaxKind::TOKEN_REC);
    (then)    => ($crate::SyntaxKind::TOKEN_THEN);
    (with)    => ($crate::SyntaxKind::TOKEN_WITH);
    (ident)   => ($crate::SyntaxKind::TOKEN_IDENT);

    ("{")     => ($crate::SyntaxKind::TOKEN_CURLY_B_OPEN);
    ("}")     => ($crate::SyntaxKind::TOKEN_CURLY_B_CLOSE);
    ("[")     => ($crate::SyntaxKind::TOKEN_SQUARE_B_OPEN);
    ("]")     => ($crate::SyntaxKind::TOKEN_SQUARE_B_CLOSE);
    ("(")     => ($crate::SyntaxKind::TOKEN_PAREN_OPEN);
    (")")     => ($crate::SyntaxKind::TOKEN_PAREN_CLOSE);

    (=)       => ($crate::SyntaxKind::TOKEN_ASSIGN);
    (@)       => ($crate::SyntaxKind::TOKEN_AT);
    (:)       => ($crate::SyntaxKind::TOKEN_COLON);
    (,)       => ($crate::SyntaxKind::TOKEN_COMMA);
    (.)       => ($crate::SyntaxKind::TOKEN_DOT);
    (...)     => ($crate::SyntaxKind::TOKEN_ELLIPSIS);
    (?)       => ($crate::SyntaxKind::TOKEN_QUESTION);
    (;)       => ($crate::SyntaxKind::TOKEN_SEMICOLON);
    (++)      => ($crate::SyntaxKind::TOKEN_CONCAT);
    (!)       => ($crate::SyntaxKind::TOKEN_INVERT);
    ("//")    => ($crate::SyntaxKind::TOKEN_UPDATE);

    (+)       => ($crate::SyntaxKind::TOKEN_ADD);
    (-)       => ($crate::SyntaxKind::TOKEN_SUB);
    (*)       => ($crate::SyntaxKind::TOKEN_MUL);
    (/)       => ($crate::SyntaxKind::TOKEN_DIV);

    (&&)      => ($crate::SyntaxKind::TOKEN_AND);
    (==)      => ($crate::SyntaxKind::TOKEN_EQUAL);
    (=>)      => ($crate::SyntaxKind::TOKEN_IMPLICATION);
    (<)       => ($crate::SyntaxKind::TOKEN_LESS);
    (<=)      => ($crate::SyntaxKind::TOKEN_LESS_OR_EQ);
    (>)       => ($crate::SyntaxKind::TOKEN_MORE);
    (>=)      => ($crate::SyntaxKind::TOKEN_MORE_OR_EQ);
    (!=)      => ($crate::SyntaxKind::TOKEN_NOT_EQUAL);
    (||)      => ($crate::SyntaxKind::TOKEN_OR);
}