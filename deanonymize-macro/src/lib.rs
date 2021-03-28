#![feature(drain_filter)]
#![feature(default_free_fn)]
#![allow(dead_code, unused_imports, unused_variables)]


mod generics;
mod signature;

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Group};

use syn::{parse_macro_input, DeriveInput, Item, Ident, ReturnType, Type, FnArg, Visibility, GenericParam, TraitItem, ItemFn, ItemTrait, Attribute, Generics, Signature, ItemImpl, ImplItem, AttributeArgs, WhereClause, PathArguments, AngleBracketedGenericArguments, Expr, PatType, ImplItemType, ItemStruct, Fields, AttrStyle, FieldsNamed, VisPublic, Receiver, ImplItemMethod};
use quote::quote;
use proc_macro2::{Span, TokenTree};
use syn::parse_quote;
use syn::spanned::Spanned;
use quote::format_ident;
use itertools::Itertools;
use std::{mem, iter};
use syn::punctuated::Punctuated;
use syn::token::{Comma, Impl, For};
use syn::token::Brace;
use syn::Field;
use std::default::default;
use crate::signature::SignatureDerivations;
use crate::generics::GenericsDerivations;

#[proc_macro_attribute]
pub fn deanonymize(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Item);
    match &input {
        Item::Fn(f) => deanonymize_fn(args, f),
        Item::Trait(t) => deanonymize_trait(args, t),
        Item::Impl(i) => deanonymize_impl(args, i),
        _ => panic!("Expected fn or trait"),
    }
}


fn deanonymize_fn(args: TokenStream, input: &ItemFn) -> TokenStream {
    let future_name = parse_macro_input!(args as Ident);
    let vis = &input.vis;

    let mut output = vec![];

    let SignatureDerivations {
        fun_name,
        input_names_noself,
        input_names_self,
        input_self,
        input_noself,
        generics: GenericsDerivations {
            gen_params,
            gen_idents_turbofish,
            gen_idents_no_lifetimes,
            gen_idents_no_lifetimes_turbofish,
            gen_idents_brackets,
            gen_where,
            phantom_args
        },
    } = SignatureDerivations::new(&input.sig, None);
    let predicates = &gen_where.as_ref().map_or(vec![], |x| x.predicates.iter().cloned().collect());

    let unit = parse_quote!(());
    let output_ty = match &input.sig.output {
        ReturnType::Default => {
            &unit
        }
        ReturnType::Type(_, ty) => &**ty,
    };
    let imp_name = Ident::new(&format!("__deanonymize_internal__{}", fun_name), fun_name.span());
    {
        let mut imp = input.clone();
        imp.attrs.push(parse_quote!(#[allow(non_snake_case)]));
        imp.vis = parse_quote!(pub);
        imp.sig.ident = imp_name.clone();
        output.push(Item::Fn(imp));
    }
    let fut_type: Type = parse_quote!(#future_name #gen_idents_brackets);

    if !gen_idents_no_lifetimes.is_empty() {
        output.push(Item::Impl(ItemImpl {
            attrs: vec![],
            defaultness: None,
            unsafety: None,
            impl_token: parse_quote!(impl),
            generics: input.sig.generics.clone(),
            trait_: Some((None, parse_quote!(::deanonymize::Storage), parse_quote!(for))),
            self_ty: Box::new(fut_type.clone()),
            brace_token: Brace { span: Span::call_site() },
            items: vec![
                parse_quote!(default type Array = ();)
            ],
        }));
    };

    {
        let mut generics = input.sig.generics.clone();
        if generics.where_clause.is_none() {
            generics.where_clause = Some(WhereClause {
                where_token: parse_quote!(where),
                predicates: Punctuated::new(),
            });
        }
        generics.where_clause.as_mut().unwrap().predicates.push(
            parse_quote!([u8; ::deanonymize::size_of_future(#imp_name #gen_idents_no_lifetimes_turbofish)]: Sized)
        );
        output.push(Item::Impl(ItemImpl {
            attrs: vec![],
            defaultness: None,
            unsafety: None,
            impl_token: parse_quote!(impl),
            generics,
            trait_: Some((None, parse_quote!(::deanonymize::Storage), For::default())),
            self_ty: Box::new(fut_type.clone()),
            brace_token: Brace { span: Span::call_site() },
            items: vec![
                parse_quote!(default type Array = [u8; ::deanonymize::size_of_future(#imp_name #gen_idents_no_lifetimes_turbofish)];)
            ],
        }));
    }

    {
        output.push(Item::Struct(ItemStruct {
            attrs: vec![
                parse_quote!(#[allow(non_camel_case_types)]),
                parse_quote!(#[repr(align(16))])
            ],
            vis: parse_quote!(pub),
            struct_token: parse_quote!(struct),
            ident: future_name.clone(),
            generics: input.sig.generics.clone(),
            fields: Fields::Named(FieldsNamed {
                brace_token: Brace::default(),
                named: vec![
                    Field {
                        attrs: vec![],
                        vis: Visibility::Inherited,
                        ident: parse_quote!(inner),
                        colon_token: None,
                        ty: parse_quote!(::std::mem::MaybeUninit<<Self as ::deanonymize::Storage>::Array>),
                    },
                    Field {
                        attrs: vec![],
                        vis: Visibility::Inherited,
                        ident: parse_quote!(phantom),
                        colon_token: None,
                        ty: parse_quote!(::std::marker::PhantomData #phantom_args),
                    }
                ].into_iter().collect(),
            }),
            semi_token: None,
        }));
    };

    {
        output.push(Item::Impl(ItemImpl {
            attrs: vec![],
            defaultness: None,
            unsafety: None,
            impl_token: Impl::default(),
            generics: input.sig.generics.clone(),
            trait_: Some((None, parse_quote!(::std::future::Future), For::default())),
            self_ty: Box::new(fut_type.clone()),
            brace_token: Brace::default(),
            items: vec![
                parse_quote!(
                    type Output = #output_ty;
                ),
                parse_quote!(
                    fn poll(self: ::std::pin::Pin<&mut Self>, cx: &mut ::std::task::Context) -> ::std::task::Poll<Self::Output>{
                        unsafe {
                            ::deanonymize::cast_to_future_pin_mut(self, #imp_name #gen_idents_no_lifetimes_turbofish).poll(cx)
                        }
                    }
                )
            ],
        }));
    };

    output.push(Item::Impl(ItemImpl {
        attrs: vec![],
        defaultness: None,
        unsafety: None,
        impl_token: Impl::default(),
        generics: input.sig.generics.clone(),
        trait_: Some((None, parse_quote!(::std::ops::Drop), For::default())),
        self_ty: Box::new(fut_type.clone()),
        brace_token: Brace::default(),
        items: vec![
            parse_quote!(
                fn drop(&mut self) {
                    unsafe {
                        ::std::ptr::drop_in_place(
                            ::deanonymize::cast_to_future_raw_mut(self,
                                #imp_name #gen_idents_no_lifetimes_turbofish))
                    }
                }
            )
        ],
    }));

    output.push(Item::Impl(ItemImpl {
        attrs: vec![],
        defaultness: None,
        unsafety: Some(default()),
        impl_token: default(),
        generics: input.sig.generics.clone(),
        trait_: Some((None, parse_quote!(::std::marker::Send), default())),
        self_ty: Box::new(fut_type.clone()),
        brace_token: default(),
        items: vec![],
    }));

    output.push(Item::Fn(ItemFn {
        attrs: vec![parse_quote!(#[allow(non_snake_case)])],
        vis: vis.clone(),
        sig: Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: default(),
            ident: fun_name,
            generics: input.sig.generics.clone(),
            paren_token: default(),
            inputs: input_self,
            variadic: None,
            output: ReturnType::Type(default(), Box::new(fut_type.clone())),
        },
        block: Box::new(parse_quote!({
            unsafe {
                ::deanonymize::force_transmute(#imp_name #gen_idents_no_lifetimes_turbofish(#(#input_names_noself),*))
            }
        })),
    }));

    let tokens = quote! { #( #output )* };

    tokens.into()
}

fn remove_tag(attrs: &mut Vec<Attribute>) -> Option<Ident> {
    if let Some(attr) = attrs.drain_filter(
        |attr| {
            attr.path.segments.iter().exactly_one()
                .ok().expect("unexpected path to deanonymize")
                .ident.to_string() == "deanonymize"
        }
    ).exactly_one().ok() {
        let target = attr.tokens.into_iter().exactly_one().unwrap();
        let target = match target {
            TokenTree::Group(target) => target.stream().into_iter().exactly_one().unwrap(),
            _ => panic!("Parameter must be ident {:?}", target)
        };
        match target {
            TokenTree::Ident(i) => Some(i),
            _ => panic!(),
        }
    } else {
        None
    }
}

fn deanonymize_trait(_args: TokenStream, input: &ItemTrait) -> TokenStream {
    let mut output = input.clone();
    let mut new_items: Vec<TraitItem> = vec![];
    for item in output.items.iter_mut() {
        match item {
            TraitItem::Method(method) => {
                if let Some(target) = remove_tag(&mut method.attrs) {
                    let GenericsDerivations {
                        gen_params,
                        gen_idents_turbofish,
                        ..
                    } = GenericsDerivations::new(&method.sig.generics);
                    assert!(method.sig.asyncness.take().is_some());
                    let yielded =
                        mem::replace(
                            &mut method.sig.output,
                            parse_quote!(-> Self :: #target #gen_idents_turbofish),
                        );
                    let yielded: Type = match yielded {
                        ReturnType::Default => parse_quote!(()),
                        ReturnType::Type(_, x) => *x,
                    };
                    new_items.push(parse_quote!(type #target #gen_params : ::std::future::Future<Output= #yielded> + Send;));
                }
            }
            _ => {}
        }
    }
    output.items.extend(new_items);
    let tokens = quote!(#output);
    tokens.into()
}

fn combine_generics(g1: &Generics, g2: &Generics) -> Generics {
    let mut combined_params = g1.params.clone();
    combined_params.extend(g2.params.iter().cloned());
    let mut predicates = vec![];
    if let Some(w1) = &g1.where_clause {
        predicates.extend(w1.predicates.iter().cloned());
    }
    if let Some(w2) = &g2.where_clause {
        predicates.extend(w2.predicates.iter().cloned());
    }
    let where_clause = if predicates.is_empty() {
        None
    } else {
        Some(parse_quote!(where #(#predicates),* ))
    };
    Generics {
        lt_token: g1.lt_token.clone(),
        params: combined_params,
        gt_token: g2.gt_token.clone(),
        where_clause,
    }
}

fn replace_self(stream: &TokenStream2, body: &TokenTree) -> TokenStream2 {
    stream.clone().into_iter().map(|tt| match &tt {
        TokenTree::Group(g) =>
            TokenTree::Group(Group::new(g.delimiter(), replace_self(&g.stream(), body))),
        TokenTree::Ident(id) => {
            if id.to_string() == "self" {
                body.clone()
            } else {
                tt
            }
        }
        TokenTree::Punct(p) => tt,
        TokenTree::Literal(l) => tt,
    }).collect()
}

fn deanonymize_impl(_args: TokenStream, imp: &ItemImpl) -> TokenStream {
    let mut items: Vec<Item> = vec![];
    let mut impl_items: Vec<ImplItem> = vec![];
    for item in imp.items.iter() {
        match item {
            ImplItem::Method(method) => {
                let mut method_clone = method.clone();
                if let Some(target) = remove_tag(&mut method_clone.attrs) {
                    let SignatureDerivations {
                        fun_name,
                        input_self,
                        input_noself,
                        input_names_self,
                        input_names_noself,
                        generics: GenericsDerivations {
                            gen_params: gen_params_method,
                            gen_where: gen_where_method,
                            ..
                        },
                        ..
                    } = SignatureDerivations::new(&method.sig, Some(&imp.self_ty));

                    let combined = combine_generics(&imp.generics, &method.sig.generics);
                    let GenericsDerivations {
                        gen_params,
                        gen_idents_turbofish,
                        gen_idents_no_lifetimes,
                        gen_idents_no_lifetimes_turbofish,
                        gen_idents_brackets,
                        gen_where,
                        phantom_args
                    } = GenericsDerivations::new(&combined);
                    assert!(method.sig.asyncness.is_some());
                    let yielded = &method.sig.output;

                    let trait_name = match imp.trait_.as_ref() {
                        None => None,
                        Some(t) => Some(t.1.segments.iter().next().unwrap().ident.clone())
                    };
                    let struct_name: Ident = match &*imp.self_ty {
                        Type::Path(x) => {
                            Ident::new(&x.path.segments.iter().map(|x| x.ident.clone()).join("__"), method.span())
                        }
                        _ => panic!("Expected impl for path"),
                    };

                    let imp_name;
                    let fut_name;
                    match &trait_name {
                        Some(trait_name) => {
                            imp_name = format_ident!("__deanonymize_internal__{}__{}__{}",trait_name,struct_name, method.sig.ident);
                            fut_name = format_ident!("__deanonymize_internal__{}__{}__{}",trait_name,struct_name, target);
                        }
                        None => {
                            imp_name = format_ident!("__deanonymize_internal__{}__{}",struct_name, method.sig.ident);
                            fut_name = target.clone();
                        }
                    }
                    let fut_type: Type = parse_quote!(#fut_name #gen_idents_brackets);

                    let imp_body = method_clone.block;
                    let self_name = &input_names_noself[0];
                    let imp_body = replace_self(&quote!(#imp_body), &parse_quote!(#self_name));
                    let ret = &method_clone.sig.output;
                    items.push(Item::Fn(ItemFn {
                        attrs: vec![parse_quote!(#[deanonymize(#fut_name)])],
                        vis: Visibility::Inherited,
                        sig: Signature {
                            constness: None,
                            asyncness: Some(default()),
                            unsafety: None,
                            abi: None,
                            fn_token: default(),
                            ident: imp_name.clone(),
                            generics: gen_params.clone(),
                            paren_token: default(),
                            inputs: input_noself,
                            variadic: None,
                            output: ret.clone(),
                        },
                        block: parse_quote!(#imp_body),
                    }));
                    if trait_name.is_some() {
                        impl_items.push(ImplItem::Type(ImplItemType {
                            attrs: vec![],
                            vis: Visibility::Inherited,
                            defaultness: None,
                            type_token: default(),
                            ident: target,
                            generics: gen_params_method.clone(),
                            eq_token: default(),
                            ty: fut_type.clone(),
                            semi_token: default(),
                        }));
                    }
                    impl_items.push(ImplItem::Method(ImplItemMethod{
                        attrs: vec![parse_quote!(#[allow(non_snake_case)])],
                        vis: Visibility::Inherited,
                        defaultness: None,
                        sig: Signature {
                            constness: None,
                            asyncness: None,
                            unsafety: None,
                            abi: None,
                            fn_token: method.sig.fn_token.clone(),
                            ident: fun_name.clone(),
                            generics: gen_params_method.clone(),
                            paren_token: method.sig.paren_token.clone(),
                            inputs: input_self,
                            variadic: None,
                            output: ReturnType::Type(default(), Box::new(fut_type.clone())),
                        },
                        block: parse_quote!({
                            #imp_name(#( #input_names_self ),*)
                        })
                    }));
                } else {
                    impl_items.push(ImplItem::Method(method_clone));
                }
            }
            _ => {
                impl_items.push(item.clone());
            }
        }
    }
    items.push(Item::Impl(ItemImpl {
        attrs: vec![],
        defaultness: None,
        unsafety: None,
        impl_token: imp.impl_token,
        generics: imp.generics.clone(),
        trait_: imp.trait_.clone(),
        self_ty: imp.self_ty.clone(),
        brace_token: Default::default(),
        items: impl_items,
    }));
    let tokens = quote!(#(#items)*);
    tokens.into()
}