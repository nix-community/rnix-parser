use crate::SyntaxKind;

#[derive(Clone, Copy)]
pub struct TokenSet(u128);

impl TokenSet {
    pub const EMPTY: TokenSet = TokenSet(0);

    pub const fn new(kinds: &[SyntaxKind]) -> TokenSet {
        let mut res = 0u128;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1;
        }
        TokenSet(res)
    }

    pub const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub const fn contains(&self, kind: SyntaxKind) -> bool {
        self.0 & mask(kind) != 0
    }
}

const fn mask(kind: SyntaxKind) -> u128 {
    1u128 << (kind as usize)
}

impl FromIterator<SyntaxKind> for TokenSet {
    fn from_iter<T: IntoIterator<Item = SyntaxKind>>(iter: T) -> Self {
        TokenSet(iter.into_iter().fold(0u128, |res, x| res | mask(x)))
    }
}

#[test]
fn smoke() {
    use crate::SyntaxKind::*;
    let ts = TokenSet::new(&[TOKEN_ASSIGN, TOKEN_INTEGER]);
    ts.contains(TOKEN_INTEGER);
    ts.contains(TOKEN_ASSIGN);
}
