use stackpin::stack_let;
use stackpin::InitPinned;
use stackpin::InitData;
use stackpin::UnsafeInitToken;
use std::marker::PhantomPinned;

struct Unmovable {
    s: String,
    _pinned: PhantomPinned,
    _token: UnsafeInitToken,
}

unsafe impl InitPinned for Unmovable {
    type InitData = String;
    type PinData = ();

    unsafe fn init(data: Self::InitData, _token: UnsafeInitToken) -> (Self, ()) {
        (
            Self {
                s: data,
                _pinned: PhantomPinned,
                _token,
            },
            (),
        )
    }

    unsafe fn on_pin(&mut self, _data: ()) {
        // do nothing
    }

    fn unsafe_init_token(&self) -> &UnsafeInitToken {
        &self._token
    }
}

impl Drop for Unmovable {
    fn drop(&mut self) {
        println!("Dropped {}", self.s)
    }
}

impl Unmovable {
    fn new_unpinned<T: Into<String>>(s: T) -> InitData<Self> {
        InitData(s.into())
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
