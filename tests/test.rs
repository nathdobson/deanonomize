#![allow(dead_code, unused_imports, unused_variables, incomplete_features)]
#![feature(unboxed_closures, const_fn, const_generics, const_evaluatable_checked, specialization)]

use std::future::Future;
use deanonymize::deanonymize;
use std::task::Wake;
use std::sync::Arc;
use std::task::Waker;
use std::task::Context;

#[deanonymize(Foo)]
async fn foo() {}

mod with_arg {
    use deanonymize::deanonymize;

    #[deanonymize(WithArg)]
    pub async fn with_arg(x: usize) -> usize { x }
}

mod with_type_arg {
    use deanonymize::deanonymize;

    #[deanonymize(WithTypeArg)]
    pub async fn with_type_arg<T>(x: T) -> T { x }
}

mod with_life_arg {
    use deanonymize::deanonymize;

    #[deanonymize(WithLifeArg)]
    pub async fn with_life_arg<'a>(x: &'a usize) -> &'a usize { x }
}

mod with_life_and_type_arg {
    use deanonymize::deanonymize;

    #[deanonymize(WithLifeAndTypeArg)]
    pub async fn with_life_and_type_arg<'a, T: 'a + Send>(x: &'a T) -> &'a T { x }
}

mod all {
    use deanonymize::deanonymize;

    #[deanonymize(WithArg)]
    pub async fn with_arg(x: usize) -> usize { x }

    #[deanonymize(WithTypeArg)]
    pub async fn with_type_arg<T>(x: T) -> T { x }

    #[deanonymize(WithLifeArg)]
    pub async fn with_life_arg<'a>(x: &'a usize) -> &'a usize { x }

    #[deanonymize(WithLifeAndTypeArg)]
    pub async fn with_life_and_type_arg<'a, T: 'a + Send>(x: &'a T) -> &'a T { x }
}


// pub async fn foo2<T>(x: T) -> T {
//     with_type_arg::with_type_arg::<T>(x).await
// }

trait Arrayify<T> {
    type Imp;
}

impl<T> Arrayify<T> for () {
    default type Imp = ();
}

impl<T> Arrayify<T> for () where [u8; std::mem::size_of::<T>()]: Sized {
    type Imp = [u8; std::mem::size_of::<T>()];
}

struct Bar<T: Sized> {
    array: <() as Arrayify<T>>::Imp,
}

#[test]
fn test() {
    let mut fut = Box::pin(async {
        assert_eq!(foo().await, ());
        assert_eq!(with_arg::with_arg(1).await, 1);
        assert_eq!(with_type_arg::with_type_arg(3u8).await, 3u8);
        assert_eq!(with_life_and_type_arg::with_life_and_type_arg(&3u8).await, &3u8);
    });
    struct Noop;
    impl Wake for Noop {
        fn wake(self: Arc<Self>) {}
    }
    let waker = Waker::from(Arc::new(Noop));
    assert!(fut.as_mut().poll(&mut Context::from_waker(&waker)).is_ready());
}