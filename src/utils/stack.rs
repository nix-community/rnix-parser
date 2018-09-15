#[derive(Debug)]
pub struct Stack<I> {
    // TODO: const generics
    buffer: [Option<I>; 2]
}
impl<I> Stack<I> {
    pub fn new(buffer: [Option<I>; 2]) -> Self {
        Self { buffer }
    }

    #[inline(always)]
    pub fn first_free_index(&self) -> usize {
        self.buffer.iter().position(Option::is_none).unwrap_or(self.buffer.len())
    }
    #[inline(always)]
    pub fn first_free(&mut self) -> &mut Option<I> {
        debug_assert!(self.buffer.iter().any(Option::is_none));
        let pos = self.first_free_index();
        &mut self.buffer[pos]
    }
    #[inline(always)]
    pub fn peek(&self) -> Option<&I> {
        let pos = self.first_free_index().saturating_sub(1);
        self.buffer[pos].as_ref()
    }
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.first_free_index() == 0
    }
    #[inline(always)]
    pub fn push(&mut self, val: I) {
        *self.first_free() = Some(val);
    }
    #[inline(always)]
    pub fn pop(&mut self) -> Option<I> {
        let pos = self.first_free_index().saturating_sub(1);
        self.buffer[pos].take()
    }
}
