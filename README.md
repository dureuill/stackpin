stackpin
========

This crate exposes a `StackPinned` type that allows to represent `!Unpin` data that should be pinned to the stack at the point of declaration.

To do so, this crate provides a `FromUnpinned` trait and a `stack_let!` macro that enable safe construction of `Pin<StackPinned>` objects (aliased to `PinStack` for short).

Getting instances pinned at the point of declaration is as easy as:

```rust
stack_let!(unmovable = Unmovable::new_unpinned("Intel the Beagle")); // this creates the unmovable instance on the stack and binds `unmovable` with a `PinStack<Unmovable>`
```

For `Unmovable` a struct implementing the `FromUnpinned<String>` trait.

See the [crate documentation](https://doc.rs/stackpin) for details, or look directly [at the examples](https://github.com/dureuill/stackpin/tree/master/src/bin).