use rowan_tools::define_language;

define_language! {
    language NixLanguage;

    #[allow(non_camel_case_types)]
    enum SyntaxKind {
        // Internals
        TOKEN_COMMENT,
        TOKEN_ERROR,
        TOKEN_WHITESPACE,

        // Keywords
        TOKEN_ASSERT,
        TOKEN_ELSE,
        TOKEN_IF,
        TOKEN_IN,
        TOKEN_INHERIT,
        TOKEN_LET,
        TOKEN_REC,
        TOKEN_THEN,
        TOKEN_WITH,

        // Symbols
        TOKEN_CURLY_B_OPEN,
        TOKEN_CURLY_B_CLOSE,
        TOKEN_SQUARE_B_OPEN,
        TOKEN_SQUARE_B_CLOSE,
        TOKEN_ASSIGN,
        TOKEN_AT,
        TOKEN_COLON,
        TOKEN_COMMA,
        TOKEN_DOT,
        TOKEN_ELLIPSIS,
        TOKEN_QUESTION,
        TOKEN_SEMICOLON,

        // Operators
        TOKEN_PAREN_OPEN,
        TOKEN_PAREN_CLOSE,
        TOKEN_CONCAT,
        TOKEN_INVERT,
        TOKEN_UPDATE,

        TOKEN_ADD,
        TOKEN_SUB,
        TOKEN_MUL,
        TOKEN_DIV,

        TOKEN_AND,
        TOKEN_EQUAL,
        TOKEN_IMPLICATION,
        TOKEN_LESS,
        TOKEN_LESS_OR_EQ,
        TOKEN_MORE,
        TOKEN_MORE_OR_EQ,
        TOKEN_NOT_EQUAL,
        TOKEN_OR,

        // Identifiers and values
        TOKEN_DYNAMIC_END,
        TOKEN_DYNAMIC_START,
        TOKEN_FLOAT,
        TOKEN_IDENT,
        TOKEN_INTEGER,
        TOKEN_INTERPOL_END,
        TOKEN_INTERPOL_START,
        TOKEN_PATH,
        TOKEN_URI,
        TOKEN_STRING_CONTENT,
        TOKEN_STRING_END,
        TOKEN_STRING_START,

        NODE_APPLY,
        NODE_ASSERT,
        NODE_KEY,
        NODE_DYNAMIC,
        NODE_ERROR,
        NODE_IDENT,
        NODE_IF_ELSE,
        NODE_SELECT,
        NODE_INHERIT,
        NODE_INHERIT_FROM,
        NODE_STRING,
        NODE_STRING_INTERPOL,
        NODE_LAMBDA,
        // An old let { x = 92; body = x; } syntax
        NODE_LEGACY_LET,
        NODE_LET_IN,
        NODE_LIST,
        NODE_BIN_OP,
        NODE_OR_DEFAULT,
        NODE_PAREN,
        NODE_PATTERN,
        NODE_PAT_BIND,
        NODE_PAT_ENTRY,
        NODE_ROOT,
        NODE_ATTR_SET,
        NODE_KEY_VALUE,
        NODE_UNARY_OP,
        NODE_LITERAL,
        NODE_WITH,

        #[doc(hidden)]
        __LAST,
    }
}
use SyntaxKind::*;

impl SyntaxKind {
    /// Returns true if this token is a literal, such as an integer or a string
    pub fn is_literal(self) -> bool {
        match self {
            TOKEN_FLOAT | TOKEN_INTEGER | TOKEN_PATH | TOKEN_URI => true,
            _ => false,
        }
    }
    /// Returns true if this token should be used as a function argument.
    /// ```ignore
    /// Example:
    /// add 1 2 + 3
    /// ^   ^ ^ ^
    /// |   | | +- false
    /// |   | +- true
    /// |   +- true
    /// +- true
    /// ```
    pub fn is_fn_arg(self) -> bool {
        match self {
            TOKEN_REC | TOKEN_CURLY_B_OPEN | TOKEN_SQUARE_B_OPEN | TOKEN_PAREN_OPEN
                | TOKEN_STRING_START | TOKEN_IDENT => true,
            _ => self.is_literal(),
        }
    }
    /// Returns true if this token is a comment, whitespace, or similar, and
    /// should be skipped over by the parser.
    pub fn is_trivia(self) -> bool {
        match self {
            TOKEN_COMMENT | TOKEN_ERROR | TOKEN_WHITESPACE => true,
            _ => false,
        }
    }
}

impl From<rowan_tools::lexer::Error> for SyntaxKind {
    fn from(_error: rowan_tools::lexer::Error) -> Self {
        Self::TOKEN_ERROR
    }
}
