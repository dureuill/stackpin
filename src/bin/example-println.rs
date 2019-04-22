use stackpin::stack_let;
use stackpin::FromUnpinned;
use stackpin::Unpinned;
use std::marker::PhantomPinned;

struct Unmovable {
    s: String,
    _pinned: PhantomPinned,
}

unsafe impl FromUnpinned<String> for Unmovable {
    type PinData = ();

    unsafe fn from_unpinned(s: String) -> (Self, ()) {
        (
            Self {
                s,
                _pinned: PhantomPinned,
            },
            (),
        )
    }

    unsafe fn on_pin(&mut self, _data: ()) {
        // do nothing
    }
}

impl Drop for Unmovable {
    fn drop(&mut self) {
        println!("Dropped {}", self.s)
    }
}

impl Unmovable {
    fn new_unpinned<T: Into<String>>(s: T) -> Unpinned<String, Unmovable> {
        Unpinned::new(s.into())
    }
}

fn main() {
    stack_let!(unmovable = Unmovable::new_unpinned("Beagle"));
    ::std::mem::forget(unmovable); // Cannot forget the unmovable itself

    {
        stack_let!(unmovable = Unmovable::new_unpinned("Totoro"));
        {
            ::std::mem::forget(unmovable); // Cannot drop earlier
        }
        println!("Totoro not dropped yet")
    }
}
