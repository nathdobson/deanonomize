
#![feature(drain_filter)]
#![allow(dead_code, unused_imports, unused_variables)]


extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Group};

use syn::{parse_macro_input, DeriveInput, Item, Ident, ReturnType, Type, FnArg, Visibility, GenericParam, TraitItem, ItemFn, ItemTrait, Attribute, Generics, Signature, ItemImpl, ImplItem, AttributeArgs, WhereClause, PathArguments, AngleBracketedGenericArguments};
use quote::quote;
use proc_macro2::{Span, TokenTree};
use syn::parse_quote;
use syn::spanned::Spanned;
use quote::format_ident;
use itertools::Itertools;
use std::mem;
use syn::punctuated::Punctuated;

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

struct SignatureExtra {
    fun_name: Ident,
    mod_name: Ident,
    input_types: Vec<Type>,
    input_names: Vec<TokenStream2>,
    input_names_selfy: Vec<TokenStream2>,
    input_selfy: TokenStream2,
    input_not_selfy: TokenStream2,
    generics: GenericsExtra,
}

struct GenericsExtra {
    gen_params: Generics,
    gen_idents_turbofish: AngleBracketedGenericArguments,
    gen_idents_no_lifetimes: Vec<TokenStream2>,
    gen_idents_no_lifetimes_turbofish: AngleBracketedGenericArguments,
    gen_idents_brackets: AngleBracketedGenericArguments,
    gen_where: Option<WhereClause>,
    phantom_args: AngleBracketedGenericArguments,
}

impl SignatureExtra {
    fn new(sig: &Signature, self_type: Option<&Type>) -> Self {
        let fun_name = sig.ident.clone();
        let mod_name = format_ident!("__deanonymize_internal__{}", fun_name);
        let generics = GenericsExtra::new(&sig.generics);
        let mut self_arg = None;
        let input_types: Vec<_> =
            sig.inputs.iter().map(|input| match input {
                FnArg::Receiver(r) => {
                    self_arg = Some(r);
                    let self_type = self_type.unwrap().clone();
                    match r.reference.as_ref() {
                        None => self_type,
                        Some((and, l)) => {
                            match l {
                                None => {
                                    if let Some(mu) = r.mutability {
                                        parse_quote!(#and #mu #self_type)
                                    } else {
                                        parse_quote!(#and #self_type)
                                    }
                                }
                                Some(l) => {
                                    if let Some(mu) = r.mutability {
                                        parse_quote!(#and #l #mu #self_type)
                                    } else {
                                        parse_quote!(#and #l #self_type)
                                    }
                                }
                            }
                        }
                    }
                }
                FnArg::Typed(ty) => {
                    (*ty.ty).clone()
                }
            }).collect();
        let input_names: Vec<TokenStream2> =
            input_types.iter().enumerate().map(|(index, ty)| {
                let id = Ident::new(&format!("__deanonymize_internal__{}", index), ty.span());
                quote!(#id)
            }).collect();
        let mut input_names_selfy = input_names.clone();
        let input_selfy;
        let input_not_selfy;
        if let Some(self_arg) = self_arg {
            let n0 = &input_names[0];
            let t0 = &input_types[0];
            let input_names = &input_names[1..];
            let input_types = &input_types[1..];
            input_selfy = quote!( #self_arg , #( #input_names : #input_types ),*);
            input_names_selfy[0] = parse_quote!(self);
            let inputs: Vec<_> = sig.inputs.iter().skip(1).collect();
            input_not_selfy = quote!(#n0: #t0 #(, #inputs )*);
        } else {
            input_selfy = quote!( #( #input_names : #input_types ),*);
            let inputs = &sig.inputs;
            input_not_selfy = quote!(#inputs);
        };

        SignatureExtra {
            fun_name,
            mod_name,
            generics,
            input_types,
            input_names,
            input_names_selfy,
            input_selfy,
            input_not_selfy,
        }
    }
}

impl GenericsExtra {
    fn new(gen_params: &Generics) -> Self {
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
                    quote!(*mut #ident)
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
        let gen_idents_turbofish = parse_quote!(::< #( #gen_idents ),* >);
        let gen_idents_no_lifetimes_turbofish =
            parse_quote!(::< #( #gen_idents_no_lifetimes ),* >);
        let gen_idents_brackets = parse_quote!(< #( #gen_idents ),* >);
        let phantom_args = parse_quote!(< ( #( #phantom_args ),* ) >);
        let gen_where = gen_params.where_clause.clone();
        GenericsExtra {
            gen_params: gen_params.clone(),
            gen_idents_turbofish,
            gen_idents_no_lifetimes,
            gen_idents_no_lifetimes_turbofish,
            gen_idents_brackets,
            gen_where,
            phantom_args,
        }
    }
}

fn deanonymize_fn(args: TokenStream, input: &ItemFn) -> TokenStream {
    let future_name = parse_macro_input!(args as Ident);
    let vis = &input.vis;


    let SignatureExtra {
        fun_name,
        mod_name,
        input_types,
        input_names,
        input_names_selfy,
        input_selfy,
        input_not_selfy,
        generics: GenericsExtra {
            gen_params,
            gen_idents_turbofish,
            gen_idents_no_lifetimes,
            gen_idents_no_lifetimes_turbofish,
            gen_idents_brackets,
            gen_where,
            phantom_args
        },
    } = SignatureExtra::new(&input.sig, None);
    let predicates = &gen_where.as_ref().map_or(vec![], |x| x.predicates.iter().cloned().collect());

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

    let default_imp =
        if gen_idents_no_lifetimes.is_empty() {
            quote!()
        } else {
            quote! {
                impl #gen_params ::deanonymize::Storage  for #future_name #gen_idents_brackets #gen_where{
                    default type Array = ();
                }
            }
        };

    let tokens = quote! {
        #[allow(non_snake_case)]
        #imp

        #default_imp

        impl #gen_params ::deanonymize::Storage for #future_name #gen_idents_brackets
        where
            #( #predicates,)*
            [u8; ::deanonymize::size_of_future(#imp_name #gen_idents_no_lifetimes_turbofish)]: Sized {
            default type Array = [u8; ::deanonymize::size_of_future(#imp_name #gen_idents_no_lifetimes_turbofish)];
        }

        // TODO: derive alignment?
        #[allow(non_camel_case_types)]
        #[repr(align(16))]
        pub struct #future_name #gen_params #gen_where{
            inner: ::std::mem::MaybeUninit<<Self as ::deanonymize::Storage>::Array>,
            phantom: ::std::marker::PhantomData #phantom_args,
        }

        impl #gen_params ::std::future::Future for #future_name #gen_idents_brackets #gen_where
        {
            type Output = #output;
            fn poll(self: ::std::pin::Pin<&mut Self>, cx: &mut ::std::task::Context) -> ::std::task::Poll<Self::Output>{
                unsafe {
                    ::deanonymize::cast_to_future_pin_mut(self, #imp_name #gen_idents_no_lifetimes_turbofish).poll(cx)
                }
            }
        }

        impl #gen_params ::std::ops::Drop for #future_name #gen_idents_brackets #gen_where
        {
            fn drop(&mut self) {
                unsafe {
                    ::std::ptr::drop_in_place(
                        ::deanonymize::cast_to_future_raw_mut(self,
                            #imp_name #gen_idents_no_lifetimes_turbofish))
                }
            }
        }

        unsafe impl #gen_params Send for #future_name #gen_idents_brackets #gen_where {}

        #[allow(non_snake_case)]
        #vis fn #fun_name #gen_params(#( #input_names : #input_types ),*) -> #future_name #gen_idents_brackets
        #gen_where {
            unsafe {
                ::deanonymize::force_transmute(#imp_name #gen_idents_no_lifetimes_turbofish(#(#input_names),*))
            }
        }
    };

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
                    let GenericsExtra {
                        gen_params,
                        gen_idents_turbofish,
                        ..
                    } = GenericsExtra::new(&method.sig.generics);
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

fn deanonymize_impl(_args: TokenStream, input: &ItemImpl) -> TokenStream {
    let mut output = input.clone();
    let mut extras: Vec<TokenStream2> = vec![];
    let mut extra_items: Vec<ImplItem> = vec![];
    for item in output.items.iter_mut() {
        match item {
            ImplItem::Method(method) => {
                let imp = method.clone();
                if let Some(target) = remove_tag(&mut method.attrs) {
                    let GenericsExtra {
                        ..
                    } = GenericsExtra::new(&output.generics);

                    let SignatureExtra {
                        fun_name,
                        input_selfy,
                        input_not_selfy,
                        input_names_selfy,
                        input_names,
                        generics: GenericsExtra {
                            gen_params: gen_params_method,
                            gen_where: gen_where_method,
                            ..
                        },
                        ..
                    } = SignatureExtra::new(&method.sig, Some(&output.self_ty));

                    let combined = combine_generics(&output.generics, &method.sig.generics);
                    let GenericsExtra {
                        gen_params,
                        gen_idents_turbofish,
                        gen_idents_no_lifetimes,
                        gen_idents_no_lifetimes_turbofish,
                        gen_idents_brackets,
                        gen_where,
                        phantom_args
                    } = GenericsExtra::new(&combined);
                    assert!(method.sig.asyncness.take().is_some());
                    let yielded =
                        mem::replace(
                            &mut method.sig.output,
                            parse_quote!(-> Self :: #target #gen_idents_turbofish),
                        );

                    let trait_name = match output.trait_.as_ref() {
                        None => None,
                        Some(t) => Some(t.1.segments.iter().next().unwrap().ident.clone())
                    };
                    let struct_name: Ident = match &*output.self_ty {
                        Type::Path(x) => {
                            Ident::new(&x.path.segments.iter().map(|x| x.ident.clone()).join("__"), output.self_ty.span())
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

                    let imp_body = imp.block;
                    let self_name = &input_names[0];
                    let imp_body = replace_self(&quote!(#imp_body), &parse_quote!(#self_name));
                    let ret = &imp.sig.output;
                    extras.push(quote!(
                        #[deanonymize(#fut_name)]
                        async fn #imp_name #gen_params(#input_not_selfy) #ret #gen_where #imp_body
                    ));
                    if trait_name.is_some() {
                        extra_items.push(parse_quote!(
                            type #target #gen_params_method = #fut_name #gen_idents_brackets;
                        ));
                    }
                    *method = parse_quote!(
                        #[allow(non_snake_case)]
                        fn #fun_name #gen_params_method(#input_selfy) -> #fut_name #gen_idents_brackets #gen_where_method {
                            #imp_name(#( #input_names_selfy ),*)
                        }
                    );
                }
            }
            _ => {}
        }
    }
    output.items.extend(extra_items);
    let tokens = quote!(
        #(#extras)*
        #output
    );
    tokens.into()
}