use arena::{Arena, NodeId};
use std::marker::PhantomData;

/// A single node in the tree. Has a reference to the first child and the next
/// sibling.
#[derive(Clone, Copy, Debug, Default)]
pub struct Node {
    pub child: Option<NodeId>,
    pub sibling: Option<NodeId>
}
impl Node {
    /// Convenience function for creating a node with children and no siblings
    pub fn with_child<T: Into<Option<NodeId>>>(child: T) -> Self {
        Self {
            child: child.into(),
            ..Default::default()
        }
    }
}

/// An easy way to create a list of nodes. The first node will point to the
/// next, etc...
#[derive(Clone, Copy)]
pub struct NodeList<T: AsMut<Node>> {
    start: Option<NodeId>,
    cursor: Option<NodeId>,
    __marker: PhantomData<*const T>
}
// For some reason, a NodeList can't derive Default unless the generics
// parameter is also a defaultable type. This of course makes no sense, since
// PhantomData has a Default.
impl<T: AsMut<Node>> Default for NodeList<T> {
    fn default() -> Self {
        Self {
            start: None,
            cursor: None,
            __marker: PhantomData::default()
        }
    }
}
impl<T: AsMut<Node>> NodeList<T> {
    /// Create a new instance
    pub fn new() -> Self {
        Self::default()
    }
    /// Push a new node to this list
    pub fn push(&mut self, node: NodeId, arena: &mut Arena<T>) {
        self.push_all(&[node], arena);
    }
    /// Push a slice of nodes to this list. Each node will get linked to the
    /// next one. If one of the nodes already have siblings, they'll be added
    /// as well.
    pub fn push_all(&mut self, nodes: &[NodeId], arena: &mut Arena<T>) {
        if nodes.is_empty() {
            return;
        }
        if let Some(ref mut cursor) = self.cursor {
            for &src in nodes {
                let mut dest = arena[*cursor].as_mut();
                while let Some(sibling) = dest.sibling {
                    dest = arena[sibling].as_mut();
                }
                dest.sibling = Some(src);
                *cursor = src;
            }
        } else {
            assert_eq!(self.start, None);
            self.start = Some(nodes[0]);
            self.cursor = self.start;
            return self.push_all(&nodes[1..], arena)
        }
    }
    /// Return the first node in the list
    pub fn node(&self) -> Option<NodeId> {
        self.start
    }
}

/// An iterator over a node's siblings. Keeps going to the next sibling until
/// it reaches the end.
pub struct NodeIter<'a, 'b: 'a, T: 'b + AsRef<Node>> {
    pub arena: &'a Arena<'b, T>,
    pub cursor: Option<NodeId>
}
impl<'a, 'b, T: AsRef<Node>> Iterator for NodeIter<'a, 'b, T> {
    type Item = NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        let current = self.cursor?;
        self.cursor = self.arena[current].as_ref().sibling;
        Some(current)
    }
}
