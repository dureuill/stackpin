//! The `stackpin` crate exposes a [`StackPinned`] type that allows to represent data that should be [pinned](https://doc.rust-lang.org/nightly/std/pin/index.html) to the stack
//! at the point of declaration.
//! The crate exposes a trait, [`FromUnpinned`], as well as macros (notably [`stack_let`]) that make safely creating [`StackPinned`] instances easier.
//! The crate also exposes the [`PinStack`] type alias for `Pin<StackPinned<T>>`.
//!
//! This crate was inspired from the [pin-utils] crate, the main differences being:
//! * [pin-utils] provides a macro to return a `Pin<&mut T>` instance,
//!   with a "mutable reference" semantics that includes reborrow. The `stackpin` crate promotes a
//!   "root handle" semantics that guarantees that a function consuming a [`PinStack<T>`] consumes
//!   the *only* handle to `T`, and not a borrow.
//! * The syntax for the `stack_let(mut id = expr)` macro attempts to mimic a regular `let mut id = expr` statement.
//! * The provided [`FromUnpinned`] trait and [`Unpinned`] struct aims at separating unmovable types
//! from the data that can be used to construct them. `stackpin` aims at promoting a model where
//! all unmovable types can solely be constructed 
//! * The [`StackPinned<T>`] type expresses strong guarantee about the fact that the destructor for
//! `T` will be run.
//! * The `stackpin` crate solely focuses on stack pinning. The [pin-utils] crate also provides
//! other utilities such as pin projection.
//!
//! # Stack pinnable types
//!
//! A type T that wants to benefit from the guarantees provided by [`StackPinned`] should enforce a couple of requirements:
//! 1. The type should be [`!Unpin`](https://doc.rust-lang.org/nightly/std/pin/index.html#unpin). This is necessary to enforce the "drop will be run" guarantee
//! 2. The type should **not** provide any safe way to construct an instance of itself.
//!
//! For example, let's consider the following `Unmovable` struct:
//! ```
//! use std::marker::PhantomPinned;
//! struct Unmovable {
//!     s: String,
//!     _pinned: PhantomPinned
//! }
//! ```
//!
//! It is important to note that this struct is **not** unmovable by itself, as there are no such types in Rust.
//! Instead, we are going to enforce this through privacy: since the fields of the struct are private, no instance can be created
//! from outside the module.
//! Similarly, no public "constructor" function `pub fn new() -> Unmovable` should be provided.
//!
//! So, how will clients consume `Unmovable` instances?
//!
//! The recommended solution using `stackpin` is to implement `FromUnpinned<Data>` for `Unmovable`, where `Data` is the
//! type that would normally serve as parameters in a "constructor" function.
//! ```
//! # use std::marker::PhantomPinned;
//! # struct Unmovable {
//! #     s: String,
//! #     _pinned: PhantomPinned
//! # }
//! use stackpin::FromUnpinned;
//! impl FromUnpinned<String> for Unmovable {
//!     // This associated type can be used to retain information between the creation of the instance and its pinning.
//!     // This allows for some sort of "two-steps initialization" without having to store the initialization part in the
//!     // type itself.
//!     // Here, we don't need it, so we just set it to `()`.
//!     type PinData = ();
//!
//!     // Simply builds the Unmovable from the String.
//!     // The implementation of this function is not allowed to consider that the type won't ever move **yet**.
//!     // (in particular, the `Self` instance is returned by this function)
//!     unsafe fn from_unpinned(s: String) -> (Self, ()) {
//!         (
//!             Self {
//!                 s,
//!                 _pinned: PhantomPinned,
//!             },
//!             (),
//!         )
//!     }
//!
//!     // Performs a second initialization step on an instance that is already guaranteed to never move again.
//!     // This allows to e.g. set self borrow with the guarantee that they will remain valid
//!     unsafe fn on_pin(&mut self, _data: ()) {
//!         // do nothing
//!     }
//! }
//! ```
//! With `FromUnpinned<Data>` implemented for `T`, one can now add a "constructor method" that would return an
//! `Unpinned<Data, T>`. The `Unpinned<U, T>` struct is a simple helper struct around `U` that maintains the destination
//! type `T`. This is used by, e.g., the [`stack_let`] macro to infer the type of `T` that the user may want to produce.
//!
//! ```
//! # use std::marker::PhantomPinned;
//! # struct Unmovable {
//! #     s: String,
//! #     _pinned: PhantomPinned
//! # }
//! # use stackpin::Unpinned;
//! # use stackpin::FromUnpinned;
//! # impl FromUnpinned<String> for Unmovable {
//! #     type PinData = ();
//! #     unsafe fn from_unpinned(s: String) -> (Self, ()) {
//! #         (
//! #             Self {
//! #                 s,
//! #                 _pinned: PhantomPinned,
//! #             },
//! #             (),
//! #         )
//! #     }
//! #     unsafe fn on_pin(&mut self, _data: ()) {
//! #         // do nothing
//! #     }
//! # }
//! impl Unmovable {
//!     fn new_unpinned<T: Into<String>>(s: T) -> Unpinned<String, Unmovable> {
//!        Unpinned::new(s.into())
//!     }
//! }
//! ```
//!
//! Then, a user of the `Unmovable` struct can simply build an instance by using the [`stack_let`] macro:
//! ```
//! # use std::marker::PhantomPinned;
//! # struct Unmovable {
//! #     s: String,
//! #     _pinned: PhantomPinned
//! # }
//! # use stackpin::Unpinned;
//! # use stackpin::FromUnpinned;
//! # impl FromUnpinned<String> for Unmovable {
//! #     type PinData = ();
//! #     unsafe fn from_unpinned(s: String) -> (Self, ()) {
//! #         (
//! #             Self {
//! #                 s,
//! #                 _pinned: PhantomPinned,
//! #             },
//! #             (),
//! #         )
//! #     }
//! #     unsafe fn on_pin(&mut self, _data: ()) {
//! #         // do nothing
//! #     }
//! # }
//! # impl Unmovable {
//! #     fn new_unpinned<T: Into<String>>(s: T) -> Unpinned<String, Unmovable> {
//! #        Unpinned::new(s.into())
//! #     }
//! # }
//! use stackpin::stack_let;
//! // ...
//! stack_let!(unmovable = Unmovable::new_unpinned("Intel the Beagle")); // this creates the unmovable instance on the stack and binds `unmovable` with a `PinStack<Unmovable>`
//! // ...
//! ```
//!
//! [pin-utils]: https://docs.rs/pin-utils
//! [`StackPinned`]: struct.StackPinned
//! [`StackPinned<T>`]: struct.StackPinned
//! [`FromUnpinned`]: trait.FromUnpinned
//! [`stack_let`]: macro.stack_let
//! [`PinStack`]: type.PinStack
//! [`PinStack<T>`]: type.PinStack
//! [`Unpinned`]: struct.Unpinned

use std::ops::Deref;
use std::ops::DerefMut;
use std::pin::Pin;

/// Struct that represents data that is pinned to the stack, at the point of declaration.
///
/// Because this property cannot be guaranteed by safe rust, constructing an instance of a
/// `StackPinned` directly is `unsafe`.
/// Rather, one should use the one of the creation macros that return a `PinStack<T>` instance,
/// such as pin_stack!, into_pin_stack! or stack_let!.
///
/// In particular, one should note the following about StackPinned instance:
/// * It is impossible to safely pass a `StackPinned` instance to a function
/// * It is impossible to safely return a `StackPinned` instance from a function
/// * It is impossible to store a `StackPinned` instance inside of a struct
///
/// Instead, one should replace `StackPinned<T>` with `PinStack<T>` in each of these situations.
///
/// A PinStack<T> instance does have its benefits:
/// * The underlying `T` instance is guaranteed to never move. This is useful for `T` types whose instances should never move.
/// * The destructor of `T` is guaranteed to run when the T leaves the stack frame it was allocated on,
///   even if one uses `::std::mem::forget` on the `PinStack<T>` instance.
#[repr(transparent)]
pub struct StackPinned<'pin, T>(&'pin mut T);

impl<'pin, T> StackPinned<'pin, T> {
    /// # Safety
    /// Currently the only way to build a safe `StackPinned<T>` instance is to use the `pin_stack!`, `into_pin_stack!` or `stack_let!`
    /// macros that will return a `PinStack<T>` instance.
    #[inline(always)]
    pub unsafe fn new(t: &'pin mut T) -> Self {
        Self(t)
    }
}

impl<'pin, T> Deref for StackPinned<'pin, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'pin, T> DerefMut for StackPinned<'pin, T> {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.0
    }
}

/// Trait to build `StackPinned` values from unpinned types.
///
/// Implementers of `FromUnpinned<Source>` indicate that they can be built from a `Source` instance,
/// to the condition that they will be pinned afterwards.
pub trait FromUnpinned<Source>
where
    Self: Sized,
{
    /// This associated type can be used to retain information between the creation of the instance and its pinning.
    /// This allows for some sort of "two-steps initialization" without having to store the initialization part in the
    /// type itself.
    type PinData;

    /// Performs a first initialization step, resulting in the creation of the `Self` instance.
    ///
    /// # Safety
    ///
    /// * This function is used by the construction macros, it is never safe to call directly.
    /// * Implementers of this function  are **not** allowed to consider that the type won't ever move **yet**.
    ///   (in particular, the `Self` instance is returned by this function)
    unsafe fn from_unpinned(src: Source) -> (Self, Self::PinData);

    /// Performs a second initialization step, resulting in the pinning of the `Self` instance.
    ///
    /// # Safety
    ///
    /// * This function is used by the construction macros, it is never safe to call directly.
    /// * Implementers of this function **are** allowed to consider that the type won't move ever again.
    ///   You can for instance set autoborrows safely in this function.
    /// * For convenience, a naked mutable borrow is given. Implementers of this function are **not** allowed
    ///   to move out of this mutable borrow.
    unsafe fn on_pin(&mut self, pin_data: Self::PinData);
}

/// A helper struct around `U` that memorizes the `T` destination type.
pub struct Unpinned<U, T: FromUnpinned<U>> {
    u: U,
    t: std::marker::PhantomData<T>,
}

impl<U, T: FromUnpinned<U>> FromUnpinned<Unpinned<U, T>> for T {
    type PinData = <T as FromUnpinned<U>>::PinData;

    unsafe fn from_unpinned(src: Unpinned<U, T>) -> (Self, Self::PinData) {
        <T as FromUnpinned<U>>::from_unpinned(src.u)
    }

    unsafe fn on_pin(&mut self, pin_data: Self::PinData) {
        <T as FromUnpinned<U>>::on_pin(self, pin_data)
    }
}

impl<U, T: FromUnpinned<U>> Unpinned<U, T> {
    pub fn new(u: U) -> Self {
        Self {
            u,
            t: std::marker::PhantomData,
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! internal_pin_stack {
    ($id:ident) => {
        // Shadow the original binding so that it can't directly be accessed ever again.
        let $id: $crate::PinStack<_> = unsafe {
            let $id = $crate::StackPinned::new(&mut $id);

            std::pin::Pin::new_unchecked($id)
        };
    };
    (mut $id:ident) => {
        // Shadow the original binding so that it can't directly be accessed ever again.
        let mut $id: $crate::PinStack<_> = unsafe {
            let $id = $crate::StackPinned::new(&mut $id);

            std::pin::Pin::new_unchecked($id)
        };
    };
}

/// `pin_stack!(id)` pins the instance `id: T` to the stack and rebinds `id` to a `PinStack<T>` instance pointing to the older binding.
/// Prefer `stack_let!(id = id)`.
///
/// The `id` instance will be moved to the stack in the process.
///
/// To bind `id` mutably, use `pin_stack!(mut id)`.
/// Adapted from https://docs.rs/pin-utils/0.1.0-alpha.4/src/pin_utils/stack_pin.rs.html#12-23
#[macro_export]
macro_rules! pin_stack {
    ($id:ident) => {
        // immediately move the value so we can be sure of its location
        let mut $id = $id;

        $crate::internal_pin_stack!($id);
    };
    (mut $id:ident) => {
        // immediately move the value so we can be sure of its location
        let mut $id = $id;

        $crate::internal_pin_stack!(mut $id);
    };
}

#[doc(hidden)]
pub unsafe fn write_pinned<Source, Dest>(source: Source, pdest: *mut Dest)
where
    Dest: FromUnpinned<Source>,
{
    let (dest, data) = FromUnpinned::<Source>::from_unpinned(source);
    std::ptr::write(pdest, dest);
    FromUnpinned::<Source>::on_pin(&mut *pdest, data);
}

#[doc(hidden)]
pub unsafe fn correct_type_uninitialized<Source, Dest>(_: &Unpinned<Source, Dest>) -> Dest
where
    Dest: FromUnpinned<Source>,
{
    std::mem::uninitialized()
}

/// `into_pin_stack!(id : T)` rebinds `id : U` to a `PinStack<T>` if `T: FromUnpinned<U>`.
/// Prefer `stack_let!(id = id)`.
///
/// To bind `id` mutably, use `into_pin_stack!(mut id: T)`
#[macro_export]
macro_rules! into_pin_stack {
    ($id:ident : $type:ty) => {
        let source = $id;
        let mut $id: $type = unsafe { std::mem::uninitialized() };

        unsafe {
            $crate::write_pinned(source, &mut $id as *mut _);
        }

        $crate::internal_pin_stack!($id);
    };
    (mut $id:ident :  $type: ty) => {
        let source = $id;
        let mut $id: $type = unsafe { std::mem::uninitialized() };

        unsafe {
            $crate::write_pinned(source, &mut $id as *mut _);
        }

        $crate::internal_pin_stack!(mut $id);
    };
}

/// `stack_let!(id = expr)` binds a `PinStack<T>` to `id` if `expr` is an expression of type `U` where `T : FromUnpinned<U>`.
///
/// If `expr` is of type `Unpinned<U, T>` for some `U`, then no type annotation is necessary.
/// If `expr` is of type `U` where `T: FromUnpinned<U>`, use `stack_let!(id : T = expr)`.
///
/// To bind `id` mutably, use `stack_let!(mut id = expr)`.
#[macro_export]
macro_rules! stack_let {
    ($id: ident = $expr: expr) => {
        let _stack_let_expr = $expr;
        let mut $id = unsafe { $crate::correct_type_uninitialized(&_stack_let_expr) };
        unsafe {
            $crate::write_pinned(_stack_let_expr, &mut $id as *mut _);
        }

        $crate::internal_pin_stack!($id);
    };
    (mut $id: ident = $expr: expr) => {
        let _stack_let_expr = $expr;
        let mut $id: $type = unsafe { $crate::correct_type_uninitialized(&_stack_let_expr) };
        unsafe {
            $crate::write_pinned(_stack_let_expr, &mut $id as *mut _);
        }

        $crate::internal_pin_stack!(mut $id);
    };
    ($id: ident : $type:ty = $expr: expr) => {
        let _stack_let_expr = $expr;
        let mut $id: $type = unsafe { ::std::mem::uninitialized() };
        unsafe {
            $crate::write_pinned(_stack_let_expr, &mut $id as *mut _);
        }

        $crate::internal_pin_stack!($id);
    };
    (mut $id: ident : $type:ty = $expr: expr) => {
        let _stack_let_expr = $expr;
        let mut $id: $type = unsafe { ::std::mem::uninitialized() };
        unsafe {
            $crate::write_pinned(_stack_let_expr, &mut $id as *mut _);
        }

        $crate::internal_pin_stack!(mut $id);
    };
}

/// Short-hand for `Pin<StackPinned<T>>`
pub type PinStack<'a, T> = Pin<StackPinned<'a, T>>;

#[cfg(test)]
mod tests {
    use super::FromUnpinned;
    use super::PinStack;
    use super::Unpinned;
    use std::marker::PhantomPinned;
    use std::ptr::NonNull;

    struct NUnpin {
        x: u64,
        _pinned: PhantomPinned,
    }

    struct Unpin {
        x: u64,
    }

    struct Unmovable {
        data: String,
        slice: NonNull<String>,
        _pin: PhantomPinned,
    }

    impl Unmovable {
        fn slice(&self) -> &str {
            unsafe { self.slice.as_ref() }
        }

        fn slice_mut<'a>(this: &'a mut PinStack<Unmovable>) -> &'a mut str {
            unsafe { this.as_mut().get_unchecked_mut().slice.as_mut() }
        }
    }

    impl Unmovable {
        fn new_unpinned(src: String) -> Unpinned<String, Unmovable> {
            Unpinned::new(src)
        }
    }

    impl FromUnpinned<String> for Unmovable {
        type PinData = ();
        unsafe fn from_unpinned(src: String) -> (Self, Self::PinData) {
            (
                Self {
                    data: src,
                    slice: NonNull::dangling(),
                    _pin: PhantomPinned,
                },
                (),
            )
        }

        unsafe fn on_pin(&mut self, _pin_data: Self::PinData) {
            self.slice = NonNull::from(&self.data);
        }
    }

    #[test]
    fn it_works() {
        struct Unit;
        let _unit = Unit;
        pin_stack!(_unit);
    }

    #[test]
    fn transparent_unpin() {
        let unpin = Unpin { x: 0 };
        pin_stack!(mut unpin);
        unpin.x = 42;
        assert_eq!(unpin.x, 42);
    }

    #[test]
    fn pin_block_mutation() {
        let nunpin = NUnpin {
            x: 42,
            _pinned: PhantomPinned,
        };
        pin_stack!(nunpin);
        assert_eq!(nunpin.x, 42);
    }

    #[test]
    fn simple_function_call() {
        fn f(p: PinStack<NUnpin>) {
            assert_eq!(p.x, 42);
        }
        fn g(p: &mut PinStack<Unpin>) {
            p.x = 12;
        }
        let nunpin = NUnpin {
            x: 42,
            _pinned: PhantomPinned,
        };
        pin_stack!(nunpin);
        f(nunpin);

        let unpin = Unpin { x: 0 };
        pin_stack!(mut unpin);
        g(&mut unpin);
        assert_eq!(unpin.x, 12);
    }

    #[test]
    fn stack_unmovable() {
        let test_str = "Intel the Beagle is the greatest dog in existence";
        let unmovable: Unpinned<String, Unmovable> = Unpinned::new(String::from(test_str));

        into_pin_stack!(unmovable: Unmovable);
        assert_eq!(test_str, unmovable.slice());
    }

    #[test]
    fn let_stack_unmovable() {
        let test_str = "Intel the Beagle is the greatest dog in existence";
        stack_let!(mut unmovable: Unmovable = Unmovable::new_unpinned(String::from(test_str)));
        let slice = Unmovable::slice_mut(&mut unmovable);
        slice.make_ascii_uppercase();
        assert_eq!(test_str.to_ascii_uppercase(), Unmovable::slice(&unmovable));
    }
}
