use std::ops;

use crate::SyntaxKind;

/// A bit-set of `SyntaxKind`s
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenSet(u128);

impl TokenSet {
    pub(crate) const EMPTY: TokenSet = TokenSet(0);

    pub(crate) const fn new(kind: SyntaxKind) -> TokenSet {
        TokenSet(mask(kind))
    }

    pub(crate) const fn from_slice(kinds: &[SyntaxKind]) -> TokenSet {
        let mut res = 0u128;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1
        }
        TokenSet(res)
    }

    pub(crate) const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub(crate) const fn contains(&self, kind: SyntaxKind) -> bool {
        self.0 & mask(kind) != 0
    }
}

const fn mask(kind: SyntaxKind) -> u128 {
    1u128 << (kind as usize)
}

impl ops::BitOr for SyntaxKind {
    type Output = TokenSet;

    fn bitor(self, rhs: Self) -> Self::Output {
        TokenSet(mask(self) | mask(rhs))
    }
}

impl ops::BitOr<SyntaxKind> for TokenSet {
    type Output = TokenSet;

    fn bitor(self, rhs: SyntaxKind) -> Self::Output {
        self.union(TokenSet(mask(rhs)))
    }
}

impl ops::BitOr<TokenSet> for SyntaxKind {
    type Output = TokenSet;

    fn bitor(self, rhs: TokenSet) -> Self::Output {
        TokenSet(mask(self)).union(rhs)
    }
}

impl ops::BitOr<TokenSet> for TokenSet {
    type Output = TokenSet;

    fn bitor(self, rhs: TokenSet) -> Self::Output {
        self.union(rhs)
    }
}

impl ops::BitOr<SyntaxKind> for () {
    type Output = TokenSet;

    fn bitor(self, rhs: SyntaxKind) -> Self::Output {
        TokenSet::new(rhs)
    }
}

impl ops::BitOr<()> for SyntaxKind {
    type Output = TokenSet;

    fn bitor(self, (): ()) -> Self::Output {
        TokenSet::new(self)
    }
}