#![allow(dead_code, unused_imports, unused_variables)]
extern crate proc_macro;

use proc_macro::{TokenStream};

use syn::{parse_macro_input, DeriveInput, Item, Ident, ReturnType, Type, FnArg, Visibility, GenericParam};
use quote::quote;
use proc_macro2::Span;
use syn::parse_quote;
use syn::spanned::Spanned;
use quote::format_ident;

/// Example of user-defined [procedural macro attribute][1].
///
/// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#attribute-macros
#[proc_macro_attribute]
pub fn deanonymize(args: TokenStream, input: TokenStream) -> TokenStream {
    let future_name = parse_macro_input!(args as Ident);
    let input = parse_macro_input!(input as Item);
    let input = match &input {
        Item::Fn(f) => f,
        _ => panic!("Expected fn"),
    };
    let vis = &input.vis;
    let fun_name = &input.sig.ident;
    let mod_name = format_ident!("__deanonymize_internal__{}", fun_name);
    let gen_params = &input.sig.generics;
    let gen_idents: Vec<_> =
        gen_params.params.iter().map(|gen_param| match gen_param {
            GenericParam::Type(type_param) => {
                let ident = &type_param.ident;
                quote!(#ident)
            }
            GenericParam::Lifetime(life_param) => {
                let lifetime = &life_param.lifetime;
                quote!(#lifetime)
            }
            GenericParam::Const(const_param) => {
                let ident = &const_param.ident;
                quote!(#ident)
            }
        }).collect();
    let gen_idents_no_lifetimes: Vec<_> =
        gen_params.params.iter().filter_map(|gen_param| match gen_param {
            GenericParam::Type(type_param) => {
                let ident = &type_param.ident;
                Some(quote!(#ident))
            }
            GenericParam::Lifetime(life_param) => {
                let lifetime = &life_param.lifetime;
                None
            }
            GenericParam::Const(const_param) => {
                let ident = &const_param.ident;
                Some(quote!(#ident))
            }
        }).collect();
    let phantom_args: Vec<_> =
        gen_params.params.iter().map(|gen_param| match gen_param {
            GenericParam::Type(type_param) => {
                let ident = &type_param.ident;
                quote!(::std::cell::Cell<#ident>)
            }
            GenericParam::Lifetime(life_param) => {
                let lifetime = &life_param.lifetime;
                quote!(& #lifetime mut ())
            }
            GenericParam::Const(const_param) => {
                let ident = &const_param.ident;
                quote!([(); #ident])
            }
        }).collect();
    let gen_idents_turbofish =
        if gen_idents.is_empty() { quote!() } else { quote!(::< #( #gen_idents ),* >) };
    let gen_idents_no_lifetimes_turbofish =
        if gen_idents_no_lifetimes.is_empty() { quote!() } else { quote!(::< #( #gen_idents_no_lifetimes ),* >) };
    let gen_idents_brackets =
        if gen_idents.is_empty() { quote!() } else { quote!(< #( #gen_idents ),* >) };
    let phantom_args = quote!(< ( #( #phantom_args ),* ) >);
    let input_types: Vec<_> =
        input.sig.inputs.iter().map(|input| match input {
            FnArg::Receiver(_) => panic!("Cannot use self"),
            FnArg::Typed(ty) => {
                &*ty.ty
            }
        }).collect();


    let input_names: Vec<Ident> =
        input_types.iter().enumerate().map(|(index, ty)| {
            Ident::new(&format!("__deanonymize_internal__{}", index), ty.span())
        }).collect();
    let unit = parse_quote!(());
    let output = match &input.sig.output {
        ReturnType::Default => {
            &unit
        }
        ReturnType::Type(_, ty) => &**ty,
    };
    let imp_name = Ident::new(&format!("__deanonymize_internal__{}", fun_name), fun_name.span());
    let mut imp = input.clone();
    imp.vis = parse_quote!(pub);
    imp.sig.ident = imp_name.clone();

    println!("{:#?}", input);

    let default_imp =
        if gen_idents_no_lifetimes.is_empty() {
            quote!()
        } else {
            quote! {
                impl #gen_params __deanonymize_internal__Storage #gen_idents_brackets for () {
                    default type Array = ();
                }
            }
        };

    let tokens = quote! {
        #[allow(non_snake_case)]
        mod #mod_name {
            #[allow(non_snake_case)]
            #imp

            pub const fn __deanonymize_internal__size_of_future<A, F: Fn<A> + Copy>(f:F) -> usize{
                ::std::mem::size_of::<F::Output>()
            }

            pub trait __deanonymize_internal__Storage #gen_params{
                type Array;
            }

            #default_imp

            impl #gen_params __deanonymize_internal__Storage #gen_idents_brackets for ()
            where [u8; __deanonymize_internal__size_of_future(#imp_name #gen_idents_no_lifetimes_turbofish)]: Sized {
                default type Array = [u8; __deanonymize_internal__size_of_future(#imp_name #gen_idents_no_lifetimes_turbofish)];
            }
        }

        #[repr(align(8))]
        pub struct #future_name #gen_params {
            inner: ::std::mem::MaybeUninit<<() as #mod_name::__deanonymize_internal__Storage #gen_idents_brackets>::Array>,
            phantom: ::std::marker::PhantomData #phantom_args,
        }

        impl #gen_params ::std::future::Future for #future_name #gen_idents_brackets
        {
            type Output = #output;
            fn poll(self: ::std::pin::Pin<&mut Self>, cx: &mut ::std::task::Context) -> ::std::task::Poll<Self::Output>{
                unsafe {
                    const fn __deanonymize_internal__cast_to_future<A, F: Fn<A> + Copy>(x:*mut (), f:F) -> *mut F::Output{
                        x as *mut F::Output
                    }
                    let raw_ptr = &mut self.get_unchecked_mut().inner as *mut _ as *mut ();
                    ::std::pin::Pin::new_unchecked(&mut *__deanonymize_internal__cast_to_future(raw_ptr, #mod_name::#imp_name #gen_idents_no_lifetimes_turbofish)).poll(cx)
                }
            }
        }

        #[allow(non_snake_case)]
        #vis fn #fun_name #gen_params(#( #input_names : #input_types ),*) -> #future_name #gen_idents_brackets
        {
            fn __deanonymize_internal__align_of_future<A, F: Fn<A> + Copy>(f:F) -> usize {
                ::std::mem::align_of::<F::Output>()
            }
            assert!(::std::mem::align_of::<#future_name #gen_idents_no_lifetimes_turbofish>() >= __deanonymize_internal__align_of_future(#mod_name::#imp_name #gen_idents_no_lifetimes_turbofish));
            unsafe {
                let imp = ::std::mem::MaybeUninit::new(#mod_name::#imp_name #gen_idents_no_lifetimes_turbofish(#(#input_names),*));
                let inner: ::std::mem::MaybeUninit<#future_name #gen_idents_brackets> = ::std::mem::transmute_copy(&imp);
                inner.as_ptr().read()
            }
        }
    };

    tokens.into()
}
