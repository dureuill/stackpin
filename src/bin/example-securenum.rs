mod secret {
    use stackpin::FromUnpinned;
    use std::marker::PhantomPinned;
    use std::pin::Pin;

    #[repr(transparent)]
    struct SecretU64(u64, PhantomPinned);

    // let's assume that the following is a function that prevents the compiler from not performing
    // the write
    #[inline(never)]
    fn secure_erase(x: &mut u64) {
        *x = 0
    }

    unsafe impl<'a> FromUnpinned<&'a mut u64> for SecretU64 {
        type PinData = &'a mut u64;

        unsafe fn from_unpinned(src: &'a mut u64) -> (Self, &'a mut u64) {
            (Self(0, PhantomPinned), src)
        }

        unsafe fn on_pin(&mut self, data: &'a mut u64) {
            self.0 = *data;
            // erase data from source reference
            secure_erase(data);
        }
    }

    impl SecretU64 {
        pub fn reveal(this: Pin<&Self>) -> u64 {
            this.0
        }

        pub fn secret_add(this: Pin<&mut Self>, a: u64) {
            // safety: not moving out of self
            unsafe { this.get_unchecked_mut().0 += a }
        }
    }
}

fn main() {}
