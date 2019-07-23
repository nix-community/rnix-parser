#[macro_export]
#[rustfmt::skip]
macro_rules! T {
    (assert)  => ($crate::tokenizer::tokens::TOKEN_ASSERT);
    (else)    => ($crate::tokenizer::tokens::TOKEN_ELSE);
    (if)      => ($crate::tokenizer::tokens::TOKEN_IF);
    (in)      => ($crate::tokenizer::tokens::TOKEN_IN);
    (inherit) => ($crate::tokenizer::tokens::TOKEN_INHERIT);
    (let)     => ($crate::tokenizer::tokens::TOKEN_LET);
    (rec)     => ($crate::tokenizer::tokens::TOKEN_REC);
    (then)    => ($crate::tokenizer::tokens::TOKEN_THEN);
    (with)    => ($crate::tokenizer::tokens::TOKEN_WITH);

    ("{")     => ($crate::tokenizer::tokens::TOKEN_CURLY_B_OPEN);
    ("}")     => ($crate::tokenizer::tokens::TOKEN_CURLY_B_CLOSE);
    ("[")     => ($crate::tokenizer::tokens::TOKEN_SQUARE_B_OPEN);
    ("]")     => ($crate::tokenizer::tokens::TOKEN_SQUARE_B_CLOSE);
    ("(")     => ($crate::tokenizer::tokens::TOKEN_PAREN_OPEN);
    (")")     => ($crate::tokenizer::tokens::TOKEN_PAREN_CLOSE);

    (=)       => ($crate::tokenizer::tokens::TOKEN_ASSIGN);
    (@)       => ($crate::tokenizer::tokens::TOKEN_AT);
    (:)       => ($crate::tokenizer::tokens::TOKEN_COLON);
    (,)       => ($crate::tokenizer::tokens::TOKEN_COMMA);
    (.)       => ($crate::tokenizer::tokens::TOKEN_DOT);
    (...)     => ($crate::tokenizer::tokens::TOKEN_ELLIPSIS);
    (?)       => ($crate::tokenizer::tokens::TOKEN_QUESTION);
    (;)       => ($crate::tokenizer::tokens::TOKEN_SEMICOLON);
    (++)      => ($crate::tokenizer::tokens::TOKEN_CONCAT);
    (!)       => ($crate::tokenizer::tokens::TOKEN_INVERT);
    ("//")    => ($crate::tokenizer::tokens::TOKEN_MERGE);

    (+)       => ($crate::tokenizer::tokens::TOKEN_ADD);
    (-)       => ($crate::tokenizer::tokens::TOKEN_SUB);
    (*)       => ($crate::tokenizer::tokens::TOKEN_MUL);
    (/)       => ($crate::tokenizer::tokens::TOKEN_DIV);

    (&&)      => ($crate::tokenizer::tokens::TOKEN_AND);
    (==)      => ($crate::tokenizer::tokens::TOKEN_EQUAL);
    (=>)      => ($crate::tokenizer::tokens::TOKEN_IMPLICATION);
    (<)       => ($crate::tokenizer::tokens::TOKEN_LESS);
    (<=)      => ($crate::tokenizer::tokens::TOKEN_LESS_OR_EQ);
    (>)       => ($crate::tokenizer::tokens::TOKEN_MORE);
    (>=)      => ($crate::tokenizer::tokens::TOKEN_MORE_OR_EQ);
    (!=)      => ($crate::tokenizer::tokens::TOKEN_NOT_EQUAL);
    (||)      => ($crate::tokenizer::tokens::TOKEN_OR);
}
