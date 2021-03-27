#![allow(dead_code, unused_imports, unused_variables, incomplete_features)]
#![feature(unboxed_closures, const_fn, const_generics, const_evaluatable_checked, specialization, generic_associated_types)]

use std::future::Future;
use std::task::Wake;
use std::sync::Arc;
use std::task::Waker;
use std::task::Context;
use deanonymize::deanonymize;

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
    pub async fn with_type_arg<T: Send>(x: T) -> T { x }
}

mod with_life_arg {
    use deanonymize::deanonymize;

    #[deanonymize(WithLifeArg)]
    pub async fn with_life_arg<'a>(x: &'a usize) -> &'a usize { x }
}

mod with_life_and_type_arg {
    use deanonymize::deanonymize;

    #[deanonymize(WithLifeAndTypeArg)]
    pub async fn with_life_and_type_arg<'a, T: 'a + Send + Sync>(x: &'a T) -> &'a T { x }
}

mod with_where {
    use deanonymize::deanonymize;

    #[deanonymize(WithWhere)]
    pub async fn with_where<'a, T>(x: &'a T) -> &'a T where T: 'a + Send + Sync { x }
}

mod picard {
    use deanonymize::deanonymize;
    use std::marker::PhantomData;

    #[deanonymize]
    pub trait Picard<'b, S> where S: 'b + Send + Sync {
        #[deanonymize(Engage)]
        async fn engage<'a, T: 'a + Send + Sync>(&'a self, x: &'a T) -> &'a T;
    }

    #[deanonymize]
    impl<'b, S: 'b + Send + Sync> Picard<'b, S> for Box<()> {
        #[deanonymize(Engage)]
        async fn engage<'a, T: 'a + Send + Sync>(&'a self, x: &'a T) -> &'a T {
            x
        }
    }
}

mod inherent {
    use deanonymize::deanonymize;
    use std::marker::PhantomData;
    use std::mem;

    pub struct Blaz<'b, S: 'b>(PhantomData<&'b S>);

    #[deanonymize]
    impl<'b, S: 'b + Send + Sync> Blaz<'b, S> {
        #[deanonymize(Engage)]
        async fn engage<'a, T: 'a + Send + Sync>(&'a self, x: &'a T) -> &'a T where S: 'a {
            mem::drop(self);
            x
        }
    }
}

mod all {
    use deanonymize::deanonymize;

    #[deanonymize(WithArg)]
    pub async fn with_arg(x: usize) -> usize { x }

    #[deanonymize(WithTypeArg)]
    pub async fn with_type_arg<T: Send>(x: T) -> T { x }

    #[deanonymize(WithLifeArg)]
    pub async fn with_life_arg<'a>(x: &'a usize) -> &'a usize { x }

    #[deanonymize(WithLifeAndTypeArg)]
    pub async fn with_life_and_type_arg<'a, T: 'a + Send + Sync>(x: &'a T) -> &'a T { x }
}

#[test]
fn test() {
    use picard::Picard;

    let mut fut = Box::pin(async {
        assert_eq!(foo().await, ());
        assert_eq!(with_arg::with_arg(1).await, 1);
        assert_eq!(with_type_arg::with_type_arg(3u8).await, 3u8);
        assert_eq!(with_life_and_type_arg::with_life_and_type_arg(&3u8).await, &3u8);
        <Box::<()> as Picard<()>>::engage(&Box::new(()),&123).await;
    });
    struct Noop;
    impl Wake for Noop {
        fn wake(self: Arc<Self>) {}
    }
    let waker = Waker::from(Arc::new(Noop));
    assert!(fut.as_mut().poll(&mut Context::from_waker(&waker)).is_ready());
}