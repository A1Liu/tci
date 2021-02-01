pub struct StackLL<'a, E> {
    pub parent: Option<&'a StackLL<'a, E>>,
    pub item: E,
}

impl<'a, E> StackLL<'a, E> {
    pub fn new(item: E) -> Self {
        Self { parent: None, item }
    }

    pub fn get(&self) -> &E {
        &self.item
    }

    pub fn child<'b>(&'b self, item: E) -> StackLL<'b, E>
    where
        'a: 'b,
    {
        StackLL {
            parent: Some(self),
            item,
        }
    }
}

pub struct StackLLIter<'a, 'b, E>
where
    'b: 'a,
{
    pub ll: Option<&'a StackLL<'b, E>>,
}

impl<'a, 'b, E> Iterator for StackLLIter<'a, 'b, E> {
    type Item = &'a E;
    fn next(&mut self) -> Option<&'a E> {
        let ll = self.ll?;
        let item = &ll.item;
        self.ll = ll.parent;
        return Some(item);
    }
}

impl<'a, 'b, E> IntoIterator for &'a StackLL<'b, E> {
    type Item = &'a E;
    type IntoIter = StackLLIter<'a, 'b, E>;

    fn into_iter(self) -> Self::IntoIter {
        StackLLIter { ll: Some(self) }
    }
}
