//! The `stackpin` crate exposes a [`StackPinned`] type that allows to represent [`!Unpin`] data that should be [pinned](https://doc.rust-lang.org/std/pin/index.html) to the stack
//! at the point of declaration.
//! The crate exposes a trait, [`FromUnpinned`], as well as a [`stack_let`] macro that makes safely creating [`StackPinned`] instances easier.
//! The crate also exposes the [`PinStack`] type alias for `Pin<StackPinned<T>>`.
//!
//! This crate was inspired from the [pin-utils] crate, the main differences being:
//! * [pin-utils] provides a macro to return a `Pin<&mut T>` instance,
//!   with a "mutable reference" semantics that includes reborrow. The `stackpin` crate promotes a
//!   "root handle" semantics that guarantees that a function consuming a [`PinStack<T>`] consumes
//!   the *only* handle to `T`, and not a reborrowed reference.
//! * The syntax for the `stack_let!(mut id : ty = expr)` macro attempts to mimic a regular `let mut id : ty = expr` statement.
//! * The provided [`FromUnpinned`] trait and [`Unpinned`] struct aim at separating unmovable types
//! from the data that can be used to construct them. `stackpin` aims at promoting a model where
//! all unmovable types are only accessible once pinned.
//! * The [`StackPinned<T>`] type expresses strong guarantee about the fact that the destructor for
//! `T` will be run.
//! * The `stackpin` crate solely focuses on stack pinning. The [pin-utils] crate also provides
//! other utilities such as pin projection.
//!
//! # Stack pinnable types
//!
//! A type T that wants to benefit from the guarantees provided by [`StackPinned`] should be
//! [`!Unpin`]. This is necessary to enforce the "drop will be run" guarantee.
//!
//! Additionally, the `stackpin` crate promotes an idiom where "unmovable" types are strictly
//! separated from movable types, and are preferably only accessible through `PinStack`.
//!
//! For example, let's consider the following `Unmovable` struct (from the [documentation for the
//! `pin` module](https://doc.rust-lang.org/std/pin/index.html)):
//! ```
//! use std::marker::PhantomPinned;
//! use std::ptr::NonNull;
//! struct Unmovable {
//!     // Owned data
//!     s: String,
//!     // Self referential pointer meant to point to `s`
//!     slice: NonNull<String>,
//!     // Obligatory marker that makes this struct `!Unpin`.
//!     // Without this, implementing `FromUnpinned` for `Unmovable` would not be safe.
//!     _pinned: PhantomPinned,
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
//! # use std::ptr::NonNull;
//! # struct Unmovable {
//! #     s: String,
//! #     slice: NonNull<String>,
//! #     _pinned: PhantomPinned,
//! # }
//! use stackpin::FromUnpinned;
//! // An `Unmovable` can be created from a `String`
//! unsafe impl FromUnpinned<String> for Unmovable {
//!     // This associated type can be used to retain information between the creation of the instance and its pinning.
//!     // This allows for some sort of "two-steps initialization" without having to store the initialization part in the
//!     // type itself.
//!     // Here, we don't need it, so we just set it to `()`.
//!     type PinData = ();
//!
//!     // Simply builds the Unmovable from the String.
//!     // The implementation of this function is not allowed to consider that the type won't ever move **yet**.
//!     // (in particular, the `Self` instance is returned by this function)
//!     // Note, however, that safe users of FromUnpinned will:
//!     // * Not do anything to with the returned `Self` instance between the call to
//!     //   `from_unpinned` and the call to `on_pin`.
//!     // * Not panic between calling the two functions
//!     // * Always call the second function if the first has been called.
//!     unsafe fn from_unpinned(s: String) -> (Self, ()) {
//!         (
//!             Self {
//!                 s,
//!                 // We will "fix" this dangling pointer once the data will be pinned
//!                 // and guaranteed not to move anymore.
//!                 slice: NonNull::dangling(),
//!                 _pinned: PhantomPinned,
//!             },
//!             (),
//!         )
//!     }
//!
//!     // Performs a second initialization step on an instance that is already guaranteed to never move again.
//!     // This allows to e.g. set self borrow with the guarantee that they will remain valid.
//!     unsafe fn on_pin(&mut self, _data: ()) {
//!         // Data will never move again, set the pointer to our own internal String whose address
//!         // will never change anymore
//!         self.slice = NonNull::from(&self.s);
//!     }
//! }
//! ```
//! With `FromUnpinned<Data>` implemented for `T`, one can now add a "constructor method" that would return an
//! `Unpinned<Data, T>`. The `Unpinned<U, T>` struct is a simple helper struct around `U` that maintains the destination
//! type `T`. This is used by the [`stack_let`] macro to infer the type of `T` that the user may want to produce.
//!
//! ```
//! # use std::marker::PhantomPinned;
//! # use std::ptr::NonNull;
//! # struct Unmovable {
//! #     s: String,
//! #     slice: NonNull<String>,
//! #     _pinned: PhantomPinned,
//! # }
//! # use stackpin::Unpinned;
//! # use stackpin::FromUnpinned;
//! # unsafe impl FromUnpinned<String> for Unmovable {
//! #     type PinData = ();
//! #     unsafe fn from_unpinned(s: String) -> (Self, ()) {
//! #         (
//! #             Self {
//! #                 s,
//! #                 slice: NonNull::dangling(),
//! #                 _pinned: PhantomPinned,
//! #             },
//! #             (),
//! #         )
//! #     }
//! #     unsafe fn on_pin(&mut self, _data: ()) {
//! #         self.slice = NonNull::from(&self.s);
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
//! # use std::ptr::NonNull;
//! # struct Unmovable {
//! #     s: String,
//! #     slice: NonNull<String>,
//! #     _pinned: PhantomPinned,
//! # }
//! # use stackpin::Unpinned;
//! # use stackpin::FromUnpinned;
//! # unsafe impl FromUnpinned<String> for Unmovable {
//! #     type PinData = ();
//! #     unsafe fn from_unpinned(s: String) -> (Self, ()) {
//! #         (
//! #             Self {
//! #                 s,
//! #                 slice: NonNull::dangling(),
//! #                 _pinned: PhantomPinned,
//! #             },
//! #             (),
//! #         )
//! #     }
//! #     unsafe fn on_pin(&mut self, _data: ()) {
//! #         self.slice = NonNull::from(&self.s);
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
//! [`StackPinned`]: struct.StackPinned.html
//! [`StackPinned<T>`]: struct.StackPinned.html
//! [`FromUnpinned`]: trait.FromUnpinned.html
//! [`stack_let`]: macro.stack_let.html
//! [`PinStack`]: type.PinStack.html
//! [`PinStack<T>`]: type.PinStack.html
//! [`Unpinned`]: struct.Unpinned.html
//! [`!Unpin`]: https://doc.rust-lang.org/std/pin/index.html#unpin

use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::pin::Pin;

/// Struct that represents data that is pinned to the stack, at the point of declaration.
///
/// Because this property cannot be guaranteed by safe rust, constructing an instance of a
/// [`StackPinned`] directly is `unsafe`.
/// Rather, one should use the [`stack_let`] macro that returns a [`PinStack`] instance.
///
/// In particular, one should note the following about [`StackPinned`] instance:
/// * It is impossible to safely pass a [`StackPinned`] instance to a function
/// * It is impossible to safely return a [`StackPinned`] instance from a function
/// * It is impossible to safely store a [`StackPinned`] instance inside of a struct
///
/// Instead, one should replace [`StackPinned<T>`] with [`PinStack<T>`] in each of these situations.
///
/// A [`PinStack<T>`] instance does have its benefits:
/// * The underlying `T` instance is guaranteed to never move for `T: !Unpin` once pinned.
///   This is useful for `T` types whose instances should never move.
/// * For `T: !Unpin`, the destructor of `T` is guaranteed to run when the T leaves the stack frame it was allocated on,
///   even if one uses [`std::mem::forget`](https://doc.rust-lang.org/std/mem/fn.forget.html) on
///   the [`PinStack<T>`] instance.
///
/// [`StackPinned`]: struct.StackPinned.html
/// [`StackPinned<T>`]: struct.StackPinned.html
/// [`PinStack`]: type.PinStack.html
/// [`PinStack<T>`]: type.PinStack.html
/// [`stack_let`]: macro.stack_let.html
#[repr(transparent)]
pub struct StackPinned<'pin, T>(&'pin mut T);

impl<'pin, T> StackPinned<'pin, T> {
    /// # Safety
    /// Currently the only way to build a safe [`StackPinned<T>`] instance is to use the
    /// [`stack_let`] macro that will return a [`PinStack<T>`] instance.
    ///
    /// [`StackPinned<T>`]: struct.StackPinned.html
    /// [`PinStack<T>`]: type.PinStack.html
    /// [`stack_let`]: macro.stack_let.html
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

/// Trait to build [`StackPinned`] values from unpinned types.
///
/// Implementers of `FromUnpinned<Source>` indicate that they can be built from a `Source` instance,
/// to the condition that they will be pinned afterwards.
///
/// # Safety
///
/// This trait both exposes unsafe functions **and** is unsafe to implement.
/// * Unsafe functions are exposed because the functions have the preconditions of having to be
/// called from the [`stack_let`] macro.
/// * The trait itself is unsafe to implement because implementers must provide implementations of
/// the functions that must upheld invariants that cannot be checked by the compiler. See the
/// documentation of each function for information on the invariants.
///
/// [`stack_let`]: macro.stack_let.html
/// [`StackPinned`]: struct.StackPinned.html
pub unsafe trait FromUnpinned<Source>
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
    /// * This function is used by the construction macro, it is never safe to call directly.
    /// * Implementers of this function  are **not** allowed to consider that the type won't ever move **yet**.
    ///   (in particular, the `Self` instance is returned by this function). The type should be
    ///   movable at this point.
    unsafe fn from_unpinned(src: Source) -> (Self, Self::PinData);

    /// Performs a second initialization step, resulting in the pinning of the `Self` instance.
    ///
    /// # Safety
    ///
    /// * This function is used by the construction macro, it is never safe to call directly.
    /// * Implementers of this function **are** allowed to consider that the type won't move ever again.
    ///   You can for instance set autoborrows safely in this function.
    /// * For convenience, a naked mutable borrow is directly given.
    ///   Implementers of this function are **not** allowed to move out of this mutable borrow.
    unsafe fn on_pin(&mut self, pin_data: Self::PinData);
}

/// A helper struct around `U` that remembers the `T` destination type.
///
/// This struct is typically used to build [`PinStack`] values using the [`stack_let`] macro
/// without having to specify the destination type.
///
/// # Example
///
/// ```
/// # use std::marker::PhantomPinned;
/// # use std::ptr::NonNull;
/// # struct Unmovable {
/// #     s: String,
/// #     slice: NonNull<String>,
/// #     _pinned: PhantomPinned,
/// # }
/// # use stackpin::Unpinned;
/// # use stackpin::FromUnpinned;
/// # unsafe impl FromUnpinned<String> for Unmovable {
/// #     type PinData = ();
/// #     unsafe fn from_unpinned(s: String) -> (Self, ()) {
/// #         (
/// #             Self {
/// #                 s,
/// #                 slice: NonNull::dangling(),
/// #                 _pinned: PhantomPinned,
/// #             },
/// #             (),
/// #         )
/// #     }
/// #     unsafe fn on_pin(&mut self, _data: ()) {
/// #         self.slice = NonNull::from(&self.s);
/// #     }
/// # }
/// use stackpin::stack_let;
/// // Without `Unpinned`
/// fn new_string(s : impl Into<String>) -> String { s.into() }
/// stack_let!(unmovable : Unmovable = new_string("toto"));
/// // With `Unpinned`
/// fn new_unpinned(s : impl Into<String>) -> Unpinned<String, Unmovable> { Unpinned::new(s.into()) }
/// stack_let!(unmovable = new_unpinned("toto"));
/// ```
///
/// [`stack_let`]: macro.stack_let.html
/// [`PinStack`]: type.PinStack.html
pub struct Unpinned<U, T: FromUnpinned<U>> {
    u: U,
    t: std::marker::PhantomData<T>,
}

unsafe impl<U, T: FromUnpinned<U>> FromUnpinned<Unpinned<U, T>> for T {
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
        Self { u, t: PhantomData }
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
pub unsafe fn from_unpinned<Source, Dest>(
    source: Unpinned<Source, Dest>,
) -> (Dest, Dest::PinData, PhantomData<Unpinned<Source, Dest>>)
where
    Dest: FromUnpinned<Source>,
{
    let (dest, data) = FromUnpinned::from_unpinned(source);
    (dest, data, PhantomData)
}

#[doc(hidden)]
pub unsafe fn from_source<Dest, Source>(
    source: Source,
) -> (Dest, Dest::PinData, PhantomData<Source>)
where
    Dest: FromUnpinned<Source>,
{
    let (dest, data) = FromUnpinned::from_unpinned(source);
    (dest, data, PhantomData)
}

#[doc(hidden)]
pub unsafe fn on_pin<Source, Dest>(
    pdest: *mut Dest,
    data: Dest::PinData,
    _source: PhantomData<Source>,
) where
    Dest: FromUnpinned<Source>,
{
    FromUnpinned::<Source>::on_pin(&mut *pdest, data);
}

/// `stack_let!(id = expr)` binds a [`PinStack<T>`] to `id` if `expr` is an expression of type `U` where [`T: FromUnpinned<U>`].
///
/// If `expr` is of type [`Unpinned<U, T>`] for some `U`, then no type annotation is necessary.
/// If `expr` is of type `U` where [`T: FromUnpinned<U>`], use `stack_let!(id : T = expr)`.
///
/// To bind `id` mutably, use `stack_let!(mut id = expr)`.
///
/// [`PinStack<T>`]: type.PinStack.html
/// [`T: FromUnpinned<U>`]: trait.FromUnpinned.html
/// [`Unpinned<U, T>`]: struct.Unpinned.html
#[macro_export]
macro_rules! stack_let {
    ($id: ident = $expr: expr) => {
        let (mut $id, _stack_data, _stack_phantom) = unsafe { $crate::from_unpinned($expr) };
        unsafe { $crate::on_pin(&mut $id as *mut _, _stack_data, _stack_phantom) }

        $crate::internal_pin_stack!($id);
    };
    (mut $id: ident = $expr: expr) => {
        let (mut $id, _stack_data, _stack_phantom) = unsafe { $crate::from_unpinned($expr) };
        unsafe { $crate::on_pin(&mut $id as *mut _, _stack_data, _stack_phantom) }

        $crate::internal_pin_stack!(mut $id);
    };
    ($id: ident : $type:ty = $expr: expr) => {
        let (mut $id, _stack_data, _stack_phantom) =
            unsafe { $crate::from_source::<$type, _>($expr) };
        unsafe { $crate::on_pin(&mut $id as *mut _, _stack_data, _stack_phantom) }

        $crate::internal_pin_stack!($id);
    };
    (mut $id: ident : $type:ty = $expr: expr) => {
        let (mut $id, _stack_data, _stack_phantom) =
            unsafe { $crate::from_source::<$type, _>($expr) };
        unsafe { $crate::on_pin(&mut $id as *mut _, _stack_data, _stack_phantom) }

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

    unsafe impl FromUnpinned<String> for Unmovable {
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
    fn let_stack_unmovable() {
        let test_str = "Intel the Beagle is the greatest dog in existence";
        stack_let!(mut unmovable = Unmovable::new_unpinned(String::from(test_str)));
        let slice = Unmovable::slice_mut(&mut unmovable);
        slice.make_ascii_uppercase();
        assert_eq!(test_str.to_ascii_uppercase(), Unmovable::slice(&unmovable));
    }
}
