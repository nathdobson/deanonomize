#![feature(unboxed_closures, const_fn)]
//!
//! This crate provides an attribute macro to make `async fn` work in traits. There are no heap
//! allocations or dynamic dispatch. However this crate relies on unsafe code and incomplete features,
//! so you probably shouldn't use it. See [async-trait](https://docs.rs/async-trait/0.1.48/async_trait/)
//! for a reasonable solution that uses heap allocations and dynamic dispatch.
//!
//! ```
//! #![allow(incomplete_features)]
//! #![feature(const_generics, specialization, generic_associated_types, const_evaluatable_checked)]
//! use deanonymize::deanonymize;
//! # use std::fmt::Display;
//!
//! #[deanonymize]
//! pub trait Println {
//!     // The return type of println will be an associated type called `PrintlnFut`.
//!     #[deanonymize(PrintlnFut)]
//!     async fn println<'a, T: Display + Send + Sync>(&'a mut self, x: &'a T) -> ();
//! }
//!
//! pub struct Stdout;
//!
//! #[deanonymize]
//! impl Println for Stdout {
//!     #[deanonymize(PrintlnFut)]
//!     async fn println<'a, T: Display + Send + Sync>(&'a mut self, x: &'a T) -> () {
//!         println!("{}", x);
//!     }
//! }
//!
//! ```
//!
//! # Implementation notes
//! This implementation uses a combination of terrifying hacks and incomplete features in Rust.
//! To understand these hacks, it's worth considering a sequence of use cases for async traits
//! and what is necessary to implement them.
//! ## A minimal trait
//! Say we want to implement the following:
//! ```ignore
//! use futures::future::ready;
//! trait Snoot {
//!     async fn boop() -> usize;
//! }
//! struct Pupper;
//! impl Snoot for Pupper {
//!     async fn boop() -> usize { ready(42).await }
//! }
//! ```
//! This is actually straightforward with stable rust:
//! ```
//! # use std::future::Future;
//! use futures::future::{ready, Ready};
//! trait Snoot {
//!     // The type of the returned `Future` must be included as an associated type because
//!     // `impl Future` isn't supported in traits.
//!     type BoopFut: Future<Output=usize> + Send;
//!     // The `async fn` is de-sugared to a normal `fn` returning a `Future`.
//!     fn boop() -> Self::BoopFut;
//! }
//! struct Pupper;
//! impl Snoot for Pupper {
//!     // `ready` returns a `Ready` instead of an `impl Future`.
//!     type BoopFut = Ready<usize>;
//!     fn boop() -> Self::BoopFut { ready(42) }
//! }
//! ```
//! ## Adding lifetimes
//! Things get awkward if we add any references:
//! ```ignore
//! use futures::future::ready;
//! trait Snoot {
//!     async fn boop(&mut self) -> &mut Self;
//! }
//! struct Pupper;
//! impl Snoot for Pupper {
//!     async fn boop(&mut self) -> &mut Self { ready(self).await }
//! }
//! ```
//! The type of the returned `Future` now has a lifetime parameter, so we need
//! [Generic Associated Types](https://rust-lang.github.io/rfcs/1598-generic_associated_types.html)
//! to define the associated type for the `Future`.
//! ```
//! #![feature(generic_associated_types)]
//! #![allow(incomplete_features)]
//! # use std::future::Future;
//! use futures::future::{ready, Ready};
//! trait Snoot {
//!     fn boop<'a>(&'a mut self) -> Self::BoopFut<'a>;
//!     // The type of the `Future` is allowed to depend on a lifetime 'a that `Self` outlives. If
//!     // `Self` did not outlive `'a`, then `&'a mut Self` would be invalid.
//!     type BoopFut<'a>: Future<Output=&'a mut Self> + Send where Self: 'a;
//! }
//! struct Pupper;
//! impl Snoot for Pupper {
//!     type BoopFut<'a> = Ready<&'a mut Self>;
//!     fn boop<'a>(&'a mut self) -> Self::BoopFut<'a> { ready(self) }
//! }
//! ```
//! ## Including `impl Future`s.
//! So far these cases have relied on the fact that there is a named future. However `async fn`
//! returns an `impl Future` that has no name. So what should `BoopFut` be assigned to?
//! ```ignore
//! async fn bork() { println!("Bork"); }
//! trait Snoot {
//!     async fn boop();
//! }
//! struct Pupper;
//! impl Snoot for Pupper {
//!     async fn boop() { bork().await }
//! }
//! ```
//! The answer is to create a type that pretends to be a wrapper around the `impl Future` without
//! actually containing it. This wrapper type must have a compatible layout, variance, and autotraits.
//!
//! ```
//! #![feature(unboxed_closures, const_fn)]
//! # use std::future::Future;
//! # use std::mem::{size_of, MaybeUninit};
//! # use std::mem;
//! # use std::task::{Context, Poll};
//! # use std::pin::Pin;
//! async fn bork() { println!("Bork"); }
//! // Here is the critical insight: by passing the `async fn` to another function, that function
//! // may access the `impl Future` type as `Fn::Output`.
//! const fn size_of_future<A, F: Fn<A> + Copy>(f: F) -> usize { size_of::<F::Output>() }
//! // Use repr C to force the field to be placed at 0 offset (I hope).
//! // 16 bytes ought to enough for anybody.
//! #[repr(C, align(16))]
//! struct BorkWrapper {
//!     // Use MaybeUninit because the `[u8;_]` array is never actually constructed. Hopefully
//!     // this actually addresses the issues with aliasing.
//!     inner: MaybeUninit<[u8; size_of_future(bork)]>
//! };
//!
//! // The `poll` implementation just defers to the inner `poll`.
//! unsafe fn cast<T, A, F: Fn<A>>(x:Pin<&mut T>, f:F) -> Pin<&mut F::Output> { mem::transmute(x) }
//! impl Future for BorkWrapper {
//!     type Output = ();
//!     fn poll(self:Pin<&mut Self>,cx: &mut Context<'_>) -> Poll<Self::Output> {
//!         unsafe { cast(self, bork).poll(cx) }
//!     }
//! }
//!
//! // The `drop` implementation just defers to the inner `poll`.
//! unsafe fn cast_raw<T, A, F: Fn<A>>(x:*mut T, f:F) -> *mut F::Output { mem::transmute(x) }
//! impl Drop for BorkWrapper {
//!     fn drop(&mut self) { unsafe { std::ptr::drop_in_place(cast_raw(self, bork)) }}
//! }
//!
//! // The only autotrait we need is Send. This assert should be sufficient to
//! // make the wrapper `Send`.
//! fn assert_send() -> impl Send { bork() }
//! unsafe impl Send for BorkWrapper {}
//!
//! trait Snoot {
//!     type BoopFut: Future<Output=()> + Send;
//!     fn boop() -> Self::BoopFut;
//! }
//!
//! struct Pupper;
//! const fn align_of_future<A, F: Fn<A> + Copy>(f: F) -> usize { mem::align_of::<F::Output>() }
//! impl Snoot for Pupper {
//!     type BoopFut = BorkWrapper;
//!     fn boop() -> BorkWrapper {
//!         // Verify that the alignment is compatible.
//!         assert!(align_of_future(bork) <= mem::align_of::<BorkWrapper>());
//!         unsafe { BorkWrapper { inner: mem::transmute(bork()) } }
//!     }
//! }
//! ```
//!
//! ## Adding generic type parameters
//! We're almost done. There's just one little surprise when we add futures with type parameters:
//! ```ignore
//! async fn bork<T>(x: T) -> T { x }
//! trait Snoot {
//!     async fn boop<T>(x: T) -> T;
//! }
//! struct Pupper;
//! impl Snoot for Pupper {
//!     async fn boop<T>(x: T) { bork(x).await }
//! }
//! ```
//! We try to create the wrapper type, and add features when the compiler complains:
//! ```ignore
//! #![allow(incomplete_features)]
//! #![feature(unboxed_closures, const_fn, const_generics, const_evaluatable_checked)]
//! # use std::future::Future;
//! # use std::mem::{size_of, MaybeUninit};
//! # use std::mem;
//! # use std::task::{Context, Poll};
//! # use std::pin::Pin;
//! # use std::marker::PhantomData;
//! async fn bork<T>(x: T) -> T { x }
//! const fn size_of_future<A, F: Fn<A> + Copy>(f: F) -> usize { size_of::<F::Output>() }
//! #[repr(C, align(16))]
//! struct BorkWrapper<T> {
//!     inner: MaybeUninit<[u8; size_of_future(bork::<T>)]>,
//!     // Use *mut T for invariance.
//!     phantom: PhantomData<*mut T>,
//! };
//! ```
//! Sadly this doesn't quite work:
//! ```text,ignore
//! error: unconstrained generic constant
//!   --> src/lib.rs:170:12
//!    |
//! 15 |     inner: MaybeUninit<[u8; size_of_future(bork::<T>)]>,
//!    |            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//!    |
//!    = help: try adding a `where` bound using this expression: `where [(); size_of_future(bork::<T>)]:`
//! ```
//! We're using a const expression as a parameter to a type. Const expressions can fail during evaluation,
//! but types should not fail during monomorphization. The bound the compiler is asking for
//! ensures the expression does not fail, which ensures it is safe to use in a type.
//! But we don't want to expose this bound to users. It would trickle up through APIs until the compiler
//! could evaluate the bound with a concrete parameter.
//! The last trick here is to use specialization: we create a specialized implementation
//! when the bound is satisfied. There will be a default implementation when the bound is not
//! satisfied, but the default will never be used because we know that `size_of_future` cannot fail.
//!
//! ```
//! #![allow(incomplete_features)]
//! #![feature(unboxed_closures, const_fn, const_generics, const_evaluatable_checked, specialization)]
//! # use std::future::Future;
//! # use std::mem::{size_of, MaybeUninit};
//! # use std::mem;
//! # use std::task::{Context, Poll};
//! # use std::pin::Pin;
//! # use std::marker::PhantomData;
//! async fn bork<T>(x: T) -> T { x }
//! const fn size_of_future<A, F: Fn<A> + Copy>(f: F) -> usize { size_of::<F::Output>() }
//! trait Storage { type Array; }
//! #[repr(C, align(16))]
//! struct BorkWrapper<T> {
//!     inner: MaybeUninit<<BorkWrapper<T> as Storage>::Array>,
//!     // Use *mut T for invariance.
//!     phantom: PhantomData<*mut T>,
//! };
//! impl<T> Storage for BorkWrapper<T> {
//!     default type Array = ();
//! }
//! impl<T> Storage for BorkWrapper<T> where [(); size_of_future(bork::<T>)]: Sized {
//!     type Array = [u8; size_of_future(bork::<T>)];
//! }
//! ```
//! ## Wrapping up
//! This is a rough outline of the methods used in this crate. It relies on interactions between
//! three incomplete (i.e. broken) features: generic_associated_types, const_evaluatable_checked,
//! and specialization. The compiler will crash. The linker will be missing symbols. There will be
//! undefined behavior. Type errors will be appear as "unconstrained generic constant".
//! You may even summon Cthulhu. But at least you got rid of that pesky `Box`.

pub use deanonymize_macro::deanonymize;
use std::pin::Pin;
use std::mem;
use std::mem::{MaybeUninit, size_of, align_of};

#[doc(hidden)]
pub trait Storage {
    type Array: Send + Sync;
}

#[doc(hidden)]
pub const fn size_of_future<A, F: Fn<A> + Copy>(_: F) -> usize
    where F::Output: Send {
    ::std::mem::size_of::<F::Output>()
}

#[doc(hidden)]
pub const fn align_of_future<A, F: Fn<A> + Copy>(_: F) -> usize {
    ::std::mem::align_of::<F::Output>()
}

#[doc(hidden)]
pub unsafe fn cast_to_future_raw_mut<T, A, F: Fn<A> + Copy>(x: *mut T, _: F) -> *mut F::Output {
    x as *mut _ as *mut F::Output
}

#[doc(hidden)]
pub unsafe fn cast_to_future_pin_mut<T, A, F: Fn<A> + Copy>(x: Pin<&mut T>, _: F) -> Pin<&mut F::Output> {
    Pin::new_unchecked(&mut *(x.get_unchecked_mut() as *mut _ as *mut F::Output))
}

#[doc(hidden)]
pub unsafe fn force_transmute<T1, T2>(x: T1) -> T2 {
    assert!(size_of::<T1>() <= size_of::<T2>());
    assert!(align_of::<T1>() <= align_of::<T2>());
    let x = MaybeUninit::new(x);
    let result = mem::transmute_copy(&*x.as_ptr());
    mem::forget(x);
    result
}