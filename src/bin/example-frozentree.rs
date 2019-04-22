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

        unsafe fn as_node(&self) -> &Node<T> {
            &*(self as *const FrozenNode<T> as *const Node<T>)
        }

        unsafe fn as_node_mut(&mut self) -> &mut Node<T> {
            &mut *(self as *mut FrozenNode<T> as *mut Node<T>)
        }
    }

    unsafe impl<T> FromUnpinned<Tree<T>> for FrozenTree<T> {
        type PinData = ();

        unsafe fn from_unpinned(tree: Tree<T>) -> (Self, ()) {
            (
                Self {
                    root: tree.root.frozen,
                    _pinned: std::marker::PhantomPinned,
                },
                (),
            )
        }

        unsafe fn on_pin(&mut self, _data: ()) {
            self.root.on_pin()
        }
    }

    impl<T> Node<T> {
        pub fn add_child(&mut self, data: T) -> &Self {
            self.frozen.children.push(FrozenNode::new(data));
            unsafe { self.frozen.children.last().unwrap().as_node() }
        }

        pub fn add_child_mut(&mut self, data: T) -> &mut Self {
            self.frozen.children.push(FrozenNode::new(data));
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
            // should be safe because one cannot move the node itself even when moving its data.
            unsafe { &mut pin.as_mut().get_unchecked_mut().data }
        }

        pub fn iter_children(&self) -> impl DoubleEndedIterator<Item = &Self> {
            self.children.iter()
        }

        pub fn iter_children_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Self> {
            self.children.iter_mut()
        }

        pub fn parent(&self) -> Option<&Self> {
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
        tree.iter_breadth_first()
            .map(|node| *node.data())
            .collect::<Vec<_>>(),
        vec!["root", "A", "B", "AA", "AB", "BA", "BB", "AAA", "BBA"]
    );
}