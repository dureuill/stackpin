use std::ops::Deref;
use std::ops::DerefMut;

/// Struct that represents data that is pinned to the stack, at the point of declaration.
///
/// Because this property cannot be guaranteed by safe rust, constructing an instance of a
/// `StackPinned` directly is `unsafe`.
/// Rather, one should use the pin_stack! macro, that returns a `Pin<StackPinned<T>>` instance.
///
/// In particular, one should note the following about StackPinned instance:
/// * It is impossible to safely pass a `StackPinned` instance to a function
/// * It is impossible to safely return a `StackPinned` instance from a function
/// * It is impossible to store a `StackPinned` instance inside of a struct
///
/// Instead, one should replace `StackPinned<T>` with `Pin<StackPinned<T>>` in each of these situations.
///
/// A Pin<StackPinned<T>> instance does have its benefits:
/// * The underlying `StackPinned<T>` is guaranteed to never move. This is useful for `T` that shouldn't move.
/// * The destructor of `T` is guaranteed to run.
#[repr(transparent)]
pub struct StackPinned<'pin, T>(&'pin mut T);

impl<'pin, T> StackPinned<'pin, T> {
    /// # Safety
    /// Currently the only way to build a safe `StackPinned<T>` instance is to use the `pin_stack!`
    /// macro on a `T` instance that will return a `Pin<StackPinned<T>>` instance.
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

pub trait FromUnpinned<Source>
where
    Self: Sized,
{
    type PinData;
    unsafe fn from_unpinned(src: Source) -> (Self, Self::PinData);
    unsafe fn on_pin(&mut self, pin_data: Self::PinData);
}

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
        let $id: std::pin::Pin<$crate::StackPinned<_>> = unsafe {
            // Move the value into a fresh StackPinned
            let $id = $crate::StackPinned::new(&mut $id);

            std::pin::Pin::new_unchecked($id)
        };
    };
    (mut $id:ident) => {
        // Shadow the original binding so that it can't directly be accessed ever again.
        let mut $id: std::pin::Pin<$crate::StackPinned<_>> = unsafe {
            // Move the value into a fresh StackPinned
            let $id = $crate::StackPinned::new(&mut $id);

            std::pin::Pin::new_unchecked($id)
        };
    };
}

// Adapted from https://docs.rs/pin-utils/0.1.0-alpha.4/src/pin_utils/stack_pin.rs.html#12-23
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

#[macro_export]
macro_rules! into_pin_stack {
    ($id:ident : $type:ty) => {
        let source = $id;
        let mut $id: $type = unsafe { std::mem::uninitialized() };

        unsafe {
            $crate::write_pinned(source, &mut $id as *mut _);
        }

        internal_pin_stack!($id);
    };
    (mut $id:ident :  $type: ty) => {
        let source = $id;
        let mut $id: $type = unsafe { std::mem::uninitialized() };

        unsafe {
            $crate::write_pinned(source, &mut $id as *mut _);
        }

        internal_pin_stack!(mut $id);
    };
}

#[cfg(test)]
mod tests {
    use super::FromUnpinned;
    use super::StackPinned;
    use super::Unpinned;
    use std::marker::PhantomPinned;
    use std::pin::Pin;
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
        fn f(p: Pin<StackPinned<NUnpin>>) {
            assert_eq!(p.x, 42);
        }
        fn g(p: &mut Pin<StackPinned<Unpin>>) {
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
        let test_str = "Intel the Beagle is the greated dog in existence";
        let unmovable: Unpinned<String, Unmovable> = Unpinned::new(String::from(test_str));

        into_pin_stack!(unmovable: Unmovable);
        assert_eq!(test_str, unmovable.slice());
    }
}
