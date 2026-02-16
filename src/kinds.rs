#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
#[non_exhaustive]
pub enum SyntaxKind {
    TOKEN_ADD,
    TOKEN_AND_AND,
    TOKEN_ASSIGN,
    TOKEN_ASSERT,
    TOKEN_AT,
    TOKEN_COLON,
    TOKEN_COMMA,
    TOKEN_COMMENT,
    TOKEN_CONCAT,
    TOKEN_CUR_POS,
    TOKEN_DIV,
    TOKEN_DOT,
    TOKEN_ELLIPSIS,
    TOKEN_ELSE,
    TOKEN_EQUAL,
    TOKEN_ERROR,
    TOKEN_FLOAT,
    TOKEN_IDENT,
    TOKEN_IF,
    TOKEN_IMPLICATION,
    TOKEN_IN,
    TOKEN_INHERIT,
    TOKEN_INTEGER,
    TOKEN_INTERPOL_END,
    TOKEN_INTERPOL_START,
    TOKEN_INVERT,
    TOKEN_L_BRACE,
    TOKEN_L_BRACK,
    TOKEN_L_PAREN,
    TOKEN_LESS,
    TOKEN_LESS_OR_EQ,
    TOKEN_LET,
    TOKEN_MORE,
    TOKEN_MORE_OR_EQ,
    TOKEN_MUL,
    TOKEN_NOT_EQUAL,
    TOKEN_OR,
    TOKEN_OR_OR,
    TOKEN_PATH_ABS,
    TOKEN_PATH_HOME,
    TOKEN_PATH_REL,
    TOKEN_PATH_SEARCH,
    TOKEN_PIPE_LEFT,
    TOKEN_PIPE_RIGHT,
    TOKEN_QUESTION,
    TOKEN_R_BRACE,
    TOKEN_R_BRACK,
    TOKEN_R_PAREN,
    TOKEN_REC,
    TOKEN_SEMICOLON,
    TOKEN_STRING_CONTENT,
    TOKEN_STRING_END,
    TOKEN_STRING_START,
    TOKEN_SUB,
    TOKEN_THEN,
    TOKEN_UPDATE,
    TOKEN_URI,
    TOKEN_WHITESPACE,
    TOKEN_WITH,

    NODE_APPLY,
    NODE_ASSERT,
    NODE_ATTR_SET,
    NODE_ATTRPATH,
    NODE_ATTRPATH_VALUE,
    NODE_BIN_OP,
    NODE_CUR_POS,
    NODE_DYNAMIC,
    NODE_ERROR,
    // Attrpath existence check: foo ? bar.${baz}."bux"
    NODE_HAS_ATTR,
    NODE_IDENT,
    NODE_IDENT_PARAM,
    NODE_IF_ELSE,
    NODE_INHERIT,
    NODE_INHERIT_FROM,
    NODE_INTERPOL,
    NODE_LAMBDA,
    // An old let { x = 92; body = x; } syntax
    NODE_LEGACY_LET,
    NODE_LET_IN,
    NODE_LIST,
    NODE_LITERAL,
    NODE_PAREN,
    NODE_PAT_BIND,
    NODE_PAT_ENTRY,
    NODE_PATTERN,
    NODE_PATH_ABS,
    NODE_PATH_HOME,
    NODE_PATH_REL,
    NODE_PATH_SEARCH,
    NODE_ROOT,
    NODE_SELECT,
    NODE_STRING,
    NODE_UNARY_OP,
    NODE_WITH,
}
use SyntaxKind::*;

/// Maximum valid discriminant value for SyntaxKind
pub(crate) const SYNTAX_KIND_MAX: u16 = NODE_WITH as u16;

/// Error returned when parsing a string into a `SyntaxKind` fails.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseSyntaxKindError;

impl std::fmt::Display for ParseSyntaxKindError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid SyntaxKind")
    }
}

impl std::error::Error for ParseSyntaxKindError {}

impl std::str::FromStr for SyntaxKind {
    type Err = ParseSyntaxKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "TOKEN_ADD" => Ok(TOKEN_ADD),
            "TOKEN_AND_AND" => Ok(TOKEN_AND_AND),
            "TOKEN_ASSIGN" => Ok(TOKEN_ASSIGN),
            "TOKEN_ASSERT" => Ok(TOKEN_ASSERT),
            "TOKEN_AT" => Ok(TOKEN_AT),
            "TOKEN_COLON" => Ok(TOKEN_COLON),
            "TOKEN_COMMA" => Ok(TOKEN_COMMA),
            "TOKEN_COMMENT" => Ok(TOKEN_COMMENT),
            "TOKEN_CONCAT" => Ok(TOKEN_CONCAT),
            "TOKEN_CUR_POS" => Ok(TOKEN_CUR_POS),
            "TOKEN_DIV" => Ok(TOKEN_DIV),
            "TOKEN_DOT" => Ok(TOKEN_DOT),
            "TOKEN_ELLIPSIS" => Ok(TOKEN_ELLIPSIS),
            "TOKEN_ELSE" => Ok(TOKEN_ELSE),
            "TOKEN_EQUAL" => Ok(TOKEN_EQUAL),
            "TOKEN_ERROR" => Ok(TOKEN_ERROR),
            "TOKEN_FLOAT" => Ok(TOKEN_FLOAT),
            "TOKEN_IDENT" => Ok(TOKEN_IDENT),
            "TOKEN_IF" => Ok(TOKEN_IF),
            "TOKEN_IMPLICATION" => Ok(TOKEN_IMPLICATION),
            "TOKEN_IN" => Ok(TOKEN_IN),
            "TOKEN_INHERIT" => Ok(TOKEN_INHERIT),
            "TOKEN_INTEGER" => Ok(TOKEN_INTEGER),
            "TOKEN_INTERPOL_END" => Ok(TOKEN_INTERPOL_END),
            "TOKEN_INTERPOL_START" => Ok(TOKEN_INTERPOL_START),
            "TOKEN_INVERT" => Ok(TOKEN_INVERT),
            "TOKEN_L_BRACE" => Ok(TOKEN_L_BRACE),
            "TOKEN_L_BRACK" => Ok(TOKEN_L_BRACK),
            "TOKEN_L_PAREN" => Ok(TOKEN_L_PAREN),
            "TOKEN_LESS" => Ok(TOKEN_LESS),
            "TOKEN_LESS_OR_EQ" => Ok(TOKEN_LESS_OR_EQ),
            "TOKEN_LET" => Ok(TOKEN_LET),
            "TOKEN_MORE" => Ok(TOKEN_MORE),
            "TOKEN_MORE_OR_EQ" => Ok(TOKEN_MORE_OR_EQ),
            "TOKEN_MUL" => Ok(TOKEN_MUL),
            "TOKEN_NOT_EQUAL" => Ok(TOKEN_NOT_EQUAL),
            "TOKEN_OR" => Ok(TOKEN_OR),
            "TOKEN_OR_OR" => Ok(TOKEN_OR_OR),
            "TOKEN_PATH_ABS" => Ok(TOKEN_PATH_ABS),
            "TOKEN_PATH_HOME" => Ok(TOKEN_PATH_HOME),
            "TOKEN_PATH_REL" => Ok(TOKEN_PATH_REL),
            "TOKEN_PATH_SEARCH" => Ok(TOKEN_PATH_SEARCH),
            "TOKEN_PIPE_LEFT" => Ok(TOKEN_PIPE_LEFT),
            "TOKEN_PIPE_RIGHT" => Ok(TOKEN_PIPE_RIGHT),
            "TOKEN_QUESTION" => Ok(TOKEN_QUESTION),
            "TOKEN_R_BRACE" => Ok(TOKEN_R_BRACE),
            "TOKEN_R_BRACK" => Ok(TOKEN_R_BRACK),
            "TOKEN_R_PAREN" => Ok(TOKEN_R_PAREN),
            "TOKEN_REC" => Ok(TOKEN_REC),
            "TOKEN_SEMICOLON" => Ok(TOKEN_SEMICOLON),
            "TOKEN_STRING_CONTENT" => Ok(TOKEN_STRING_CONTENT),
            "TOKEN_STRING_END" => Ok(TOKEN_STRING_END),
            "TOKEN_STRING_START" => Ok(TOKEN_STRING_START),
            "TOKEN_SUB" => Ok(TOKEN_SUB),
            "TOKEN_THEN" => Ok(TOKEN_THEN),
            "TOKEN_UPDATE" => Ok(TOKEN_UPDATE),
            "TOKEN_URI" => Ok(TOKEN_URI),
            "TOKEN_WHITESPACE" => Ok(TOKEN_WHITESPACE),
            "TOKEN_WITH" => Ok(TOKEN_WITH),

            "NODE_APPLY" => Ok(NODE_APPLY),
            "NODE_ASSERT" => Ok(NODE_ASSERT),
            "NODE_ATTR_SET" => Ok(NODE_ATTR_SET),
            "NODE_ATTRPATH" => Ok(NODE_ATTRPATH),
            "NODE_ATTRPATH_VALUE" => Ok(NODE_ATTRPATH_VALUE),
            "NODE_BIN_OP" => Ok(NODE_BIN_OP),
            "NODE_CUR_POS" => Ok(NODE_CUR_POS),
            "NODE_DYNAMIC" => Ok(NODE_DYNAMIC),
            "NODE_ERROR" => Ok(NODE_ERROR),
            "NODE_HAS_ATTR" => Ok(NODE_HAS_ATTR),
            "NODE_IDENT" => Ok(NODE_IDENT),
            "NODE_IDENT_PARAM" => Ok(NODE_IDENT_PARAM),
            "NODE_IF_ELSE" => Ok(NODE_IF_ELSE),
            "NODE_INHERIT" => Ok(NODE_INHERIT),
            "NODE_INHERIT_FROM" => Ok(NODE_INHERIT_FROM),
            "NODE_INTERPOL" => Ok(NODE_INTERPOL),
            "NODE_LAMBDA" => Ok(NODE_LAMBDA),
            "NODE_LEGACY_LET" => Ok(NODE_LEGACY_LET),
            "NODE_LET_IN" => Ok(NODE_LET_IN),
            "NODE_LIST" => Ok(NODE_LIST),
            "NODE_LITERAL" => Ok(NODE_LITERAL),
            "NODE_PAREN" => Ok(NODE_PAREN),
            "NODE_PAT_BIND" => Ok(NODE_PAT_BIND),
            "NODE_PAT_ENTRY" => Ok(NODE_PAT_ENTRY),
            "NODE_PATTERN" => Ok(NODE_PATTERN),
            "NODE_PATH_ABS" => Ok(NODE_PATH_ABS),
            "NODE_PATH_HOME" => Ok(NODE_PATH_HOME),
            "NODE_PATH_REL" => Ok(NODE_PATH_REL),
            "NODE_PATH_SEARCH" => Ok(NODE_PATH_SEARCH),
            "NODE_ROOT" => Ok(NODE_ROOT),
            "NODE_SELECT" => Ok(NODE_SELECT),
            "NODE_STRING" => Ok(NODE_STRING),
            "NODE_UNARY_OP" => Ok(NODE_UNARY_OP),
            "NODE_WITH" => Ok(NODE_WITH),

            _ => Err(ParseSyntaxKindError),
        }
    }
}

impl SyntaxKind {
    /// Returns true if this token is a literal, such as an integer or a string
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            TOKEN_FLOAT
                | TOKEN_INTEGER
                | TOKEN_PATH_ABS
                | TOKEN_PATH_REL
                | TOKEN_PATH_HOME
                | TOKEN_PATH_SEARCH
                | TOKEN_URI
        )
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
            TOKEN_REC | TOKEN_L_BRACE | TOKEN_L_BRACK | TOKEN_L_PAREN | TOKEN_STRING_START
            | TOKEN_IDENT | TOKEN_CUR_POS => true,
            _ => self.is_literal(),
        }
    }
    /// Returns true if this token is a comment, whitespace, or similar, and
    /// should be skipped over by the parser.
    pub fn is_trivia(self) -> bool {
        matches!(self, TOKEN_COMMENT | TOKEN_WHITESPACE)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn from_str_valid() {
        assert_eq!(SyntaxKind::from_str("TOKEN_COMMENT").unwrap(), TOKEN_COMMENT);
        assert_eq!(SyntaxKind::from_str("NODE_ROOT").unwrap(), NODE_ROOT);
        assert_eq!(SyntaxKind::from_str("NODE_HAS_ATTR").unwrap(), NODE_HAS_ATTR);
    }

    #[test]
    fn from_str_invalid() {
        assert!(SyntaxKind::from_str("INVALID").is_err());
        assert!(SyntaxKind::from_str("").is_err());
        assert!(SyntaxKind::from_str("token_comment").is_err()); // case sensitive
    }
}
