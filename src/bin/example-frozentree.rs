//! This example demonstrates a "FrozenTree" where nodes contain safe back pointers to their parents.
//!
//! To build a `FrozenTree`, one starts with a `Tree` and add the various nodes of the tree.
//! When all nodes are added, the `Tree` is pinned to stack by using it as source in a call to the `stack_let` macro.
//! During this operation, it becomes a `FrozenTree` where each node creates a backlink to its parent, at the cost of the resulting
//! becoming "frozen", i.e. nodes can't be added/removed anymore.
pub mod frozen_tree {

    use stackpin::FromUnpinned;
    use stackpin::PinStack;
    use std::ptr::NonNull;

    pub struct Tree<T> {
        root: Node<T>,
    }

    pub struct Node<T> {
        frozen: FrozenNode<T>,
    }

    pub struct FrozenTree<T> {
        root: FrozenNode<T>,
        _pinned: std::marker::PhantomPinned,
    }

    pub struct FrozenNode<T> {
        data: T,
        parent: Option<NonNull<FrozenNode<T>>>,
        children: Vec<FrozenNode<T>>,
    }

    impl<T> FrozenNode<T> {
        fn on_pin(&mut self) {
            let r = &*self;
            let parent_ptr = NonNull::from(r);
            for child in self.children.iter_mut() {
                child.on_pin();
                child.parent = Some(parent_ptr);
            }
        }

        /// # Safety
        ///
        /// This method performs type punning from a `&FrozenNode<T>` to a `&Node<T>`
        ///
        /// Since Node is `#[repr(transparent)]`, the pointer cast is legal.
        ///
        /// However, unsafety could arise if this is used to get a Node from a
        /// `FrozenTree` and add new children to that node.
        ///
        /// Since this returns a `&Node`, on which adding children is not possible, this is perfectly safe.
        ///
        /// This method is meant to be used to extract the `Node` representation of a child node in a `Tree`
        /// (not `FrozenTree`).
        fn as_node(&self) -> &Node<T> {
            // safety: compatible representation, pointer is not wild since it comes from `self`
            unsafe { &*(self as *const FrozenNode<T> as *const Node<T>) }
        }

        /// # Safety
        ///
        /// This method performs type punning from a `&mut FrozenNode<T>` to a `&mut Node<T>`
        ///
        /// Since Node is `#[repr(transparent)]`, the pointer cast is legal.
        ///
        /// However, unsafety could arise if this is used to get a Node from a
        /// `FrozenTree` and add new children to that node.
        ///
        /// This method is meant to be used to extract the `Node` representation of a child node in a `Tree`
        /// (not `FrozenTree`).
        unsafe fn as_node_mut(&mut self) -> &mut Node<T> {
            &mut *(self as *mut FrozenNode<T> as *mut Node<T>)
        }
    }

    unsafe impl<T> FromUnpinned<Tree<T>> for FrozenTree<T> {
        type PinData = ();

        unsafe fn from_unpinned(tree: Tree<T>) -> (Self, ()) {
            (
                // Just move the existing root to the `FrozenTree`.
                Self {
                    root: tree.root.frozen,
                    _pinned: std::marker::PhantomPinned,
                },
                (),
            )
        }

        unsafe fn on_pin(&mut self, _data: ()) {
            // recursively "fix up" the back reference of each node so that it points to its parent node
            self.root.on_pin()
        }
    }

    impl<T> Node<T> {
        pub fn add_child(&mut self, data: T) -> &Self {
            self.frozen.children.push(FrozenNode::new(data));
            self.frozen.children.last().unwrap().as_node()
        }

        pub fn add_child_mut(&mut self, data: T) -> &mut Self {
            self.frozen.children.push(FrozenNode::new(data));
            // safety: we are a `Node`, contained in a `Tree` (not `FrozenTree`)
            unsafe { self.frozen.children.last_mut().unwrap().as_node_mut() }
        }
    }

    impl<T> Tree<T> {
        pub fn new(root_data: T) -> Self {
            Self {
                root: Node {
                    frozen: FrozenNode::new(root_data),
                },
            }
        }

        pub fn root(&self) -> &Node<T> {
            &self.root
        }

        pub fn root_mut(&mut self) -> &mut Node<T> {
            &mut self.root
        }
    }

    impl<T> FrozenNode<T> {
        fn new(data: T) -> Self {
            Self {
                data,
                children: Vec::new(),
                parent: None,
            }
        }

        pub fn data(&self) -> &T {
            &self.data
        }

        pub fn data_mut(&mut self) -> &mut T {
            &mut self.data
        }

        pub fn pin_data_mut<'a>(pin: &'a mut PinStack<'_, Self>) -> &'a mut T {
            // safety: one cannot move the node itself even when moving its data.
            unsafe { &mut pin.as_mut().get_unchecked_mut().data }
        }

        pub fn iter_children(&self) -> impl DoubleEndedIterator<Item = &Self> {
            self.children.iter()
        }

        pub fn iter_children_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Self> {
            self.children.iter_mut()
        }

        pub fn parent(&self) -> Option<&Self> {
            // safety: parent has been set in the on_pin method and cannot be a wild pointer
            self.parent.map(|p| unsafe { &*p.as_ptr() })
        }
    }

    impl<T> FrozenTree<T> {
        pub fn root(&self) -> &FrozenNode<T> {
            &self.root
        }

        pub fn root_mut(&mut self) -> &mut FrozenNode<T> {
            &mut self.root
        }

        pub fn iter_depth_first(&self) -> impl Iterator<Item = &FrozenNode<T>> {
            DepthFirstIterator {
                visit_stack: std::iter::once(&self.root).collect(),
            }
        }

        pub fn iter_depth_first_pointer(&self) -> impl Iterator<Item = &FrozenNode<T>> {
            // Should theorically never happen since FrozenNode contains a Vec
            assert!(std::mem::size_of::<FrozenNode<T>>() > 0);
            PointerDepthFirstIterator {
                current_node: Some(self.root()),
            }
        }

        pub fn iter_breadth_first(&self) -> impl Iterator<Item = &FrozenNode<T>> {
            BreadthFirstIterator {
                visit_stack: std::iter::once(&self.root).collect(),
            }
        }
    }

    use std::collections::VecDeque;

    struct DepthFirstIterator<'a, T> {
        visit_stack: VecDeque<&'a FrozenNode<T>>,
    }

    impl<'a, T: 'a> Iterator for DepthFirstIterator<'a, T> {
        type Item = &'a FrozenNode<T>;

        fn next(&mut self) -> Option<&'a FrozenNode<T>> {
            self.visit_stack.pop_back().map(|node| {
                self.visit_stack.extend(node.iter_children().rev());
                node
            })
        }
    }

    struct BreadthFirstIterator<'a, T> {
        visit_stack: VecDeque<&'a FrozenNode<T>>,
    }

    impl<'a, T: 'a> Iterator for BreadthFirstIterator<'a, T> {
        type Item = &'a FrozenNode<T>;

        fn next(&mut self) -> Option<&'a FrozenNode<T>> {
            self.visit_stack.pop_front().map(|node| {
                self.visit_stack.extend(node.iter_children());
                node
            })
        }
    }

    struct PointerDepthFirstIterator<'a, T> {
        current_node: Option<&'a FrozenNode<T>>,
    }

    impl<'a, T: 'a> Iterator for PointerDepthFirstIterator<'a, T> {
        type Item = &'a FrozenNode<T>;

        fn next(&mut self) -> Option<&'a FrozenNode<T>> {
            let current_node = self.current_node?;
            let next_node = current_node
                .children
                .first()
                .or_else(|| Self::get_sibling(current_node));
            std::mem::replace(&mut self.current_node, next_node)
        }
    }

    impl<'a, T: 'a> PointerDepthFirstIterator<'a, T> {
        fn get_sibling(mut current_node: &'a FrozenNode<T>) -> Option<&'a FrozenNode<T>> {
            loop {
                let parent = current_node.parent()?;
                // compute address in parent
                let self_pointer = current_node as *const FrozenNode<T>;
                let children_base = parent.children.as_ptr();
                let index = Self::ptr_distance_from(children_base, self_pointer);
                if let Some(node) = parent.children.get(index + 1) {
                    return Some(node);
                }
                current_node = parent;
            }
        }

        fn ptr_distance_from(base: *const FrozenNode<T>, offset: *const FrozenNode<T>) -> usize {
            let base = base as usize;
            let offset = offset as usize;
            let distance = offset - base;
            distance / std::mem::size_of::<FrozenNode<T>>()
        }
    }
}

use stackpin::stack_let;

use frozen_tree::*;

fn main() {
    let mut tree = Tree::new("root");
    let a = tree.root_mut().add_child_mut("A");
    let aa = a.add_child_mut("AA");
    aa.add_child("AAA");
    a.add_child("AB");
    let b = tree.root_mut().add_child_mut("B");
    b.add_child("BA");
    let bb = b.add_child_mut("BB");
    bb.add_child_mut("BBA");
    stack_let!(tree: FrozenTree<_> = tree);
    for child in tree.root().iter_children() {
        assert!(child.parent().unwrap().data() == tree.root().data())
    }
    assert_eq!(
        tree.iter_depth_first()
            .map(|node| *node.data())
            .collect::<Vec<_>>(),
        vec!["root", "A", "AA", "AAA", "AB", "B", "BA", "BB", "BBA"]
    );

    assert_eq!(
        tree.iter_depth_first_pointer()
            .map(|node| *node.data())
            .collect::<Vec<_>>(),
        vec!["root", "A", "AA", "AAA", "AB", "B", "BA", "BB", "BBA"]
    );

    assert_eq!(
        tree.iter_breadth_first()
            .map(|node| *node.data())
            .collect::<Vec<_>>(),
        vec!["root", "A", "B", "AA", "AB", "BA", "BB", "AAA", "BBA"]
    );
}
