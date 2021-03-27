pub use deanonymize_macro::deanonymize;

pub trait Foo {
    type Bar;
}

struct Baz(<Baz as Foo>::Bar);

impl Foo for Baz {
    type Bar = [u8; 10];
}