use std::ops::{Index, IndexMut};

/// An AST node ID for lookups in the arena
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeId(pub usize);

/// Arenas are efficient ways of storing trees: All nodes are actually on a
/// flat surface, but have IDs to other nodes.
#[derive(Debug)]
pub enum Arena<'a, T: 'a> {
    Owned(Vec<Option<T>>),
    Borrowed(&'a mut Vec<Option<T>>)
}
impl<'a, T> Default for Arena<'a, T> {
    fn default() -> Self {
        Arena::Owned(Vec::new())
    }
}
impl<'a, T> Arena<'a, T> {
    /// Create a new arena instance
    pub fn new() -> Self {
        Self::default()
    }
    /// Create a new arena instance that reuses an existing vector
    pub fn with_arena(arena: &'a mut Vec<Option<T>>) -> Self {
        Arena::Borrowed(arena)
    }
    /// Create a new arena reference that shares the inner vector. This
    /// reference will make the existing instance invalid for use while the
    /// reference is active.
    pub fn reference<'b>(&'b mut self) -> Arena<'b, T> {
        Arena::with_arena(self.get_mut())
    }

    /// Return the arena instance
    ///
    /// # Panics
    /// Panics if this arena instance was created with Borrowed
    pub fn into_inner(self) -> Vec<Option<T>> {
        match self {
            Arena::Owned(inner) => inner,
            Arena::Borrowed(_) => panic!("can't move out of borrowed arena")
        }
    }
    /// Return a slice pointing to the inner arena representation
    pub fn get_ref(&self) -> &[Option<T>] {
        match self {
            Arena::Owned(inner) => inner,
            Arena::Borrowed(inner) => inner
        }
    }
    fn get_mut(&mut self) -> &mut Vec<Option<T>> {
        match self {
            Arena::Owned(inner) => inner,
            Arena::Borrowed(inner) => inner
        }
    }
    /// Place an element into the arena
    pub fn insert(&mut self, elem: T) -> NodeId {
        let inner = self.get_mut();
        inner.push(Some(elem));
        NodeId(inner.len() - 1)
    }
    /// Move out of an element in the arena. This will make following indexing
    /// calls on that index panic. This will not deallocate any space. This is
    /// due to the fact that all indexes need to stay the same. We could use a
    /// different data type instead of a tree to prevent this, but that instead
    /// means lookup will be slightly slower.
    pub fn take(&mut self, index: NodeId) -> T {
        self.get_mut()[index.0].take().expect("this index has been taken")
    }
}
impl<'a, T> Index<NodeId> for Arena<'a, T> {
    type Output = T;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.get_ref()[index.0].as_ref().expect("this index has been taken")
    }
}
impl<'a, T> IndexMut<NodeId> for Arena<'a, T> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.get_mut()[index.0].as_mut().expect("this index has been taken")
    }
}
