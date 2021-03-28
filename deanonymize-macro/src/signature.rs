use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{FnArg, Type, Signature, Receiver};
use syn::token::Comma;
use crate::generics::GenericsDerivations;
use syn::parse_quote;
use quote::format_ident;
use std::iter;
use itertools::Itertools;
use syn::spanned::Spanned;

pub struct SignatureDerivations {
    pub fun_name: Ident,
    pub input_self: Punctuated<FnArg, Comma>,
    pub input_noself: Punctuated<FnArg, Comma>,
    pub input_names_noself: Vec<Ident>,
    pub input_names_self: Vec<Ident>,
    pub generics: GenericsDerivations,
}

fn type_of_receiver(r: &Receiver, self_type: Type) -> Type {
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

fn type_of_fn_arg(fn_arg: &FnArg, self_type: Option<&Type>) -> Type {
    match fn_arg {
        FnArg::Receiver(r) => {
            let self_type = self_type.unwrap().clone();
            type_of_receiver(r, self_type)
        }
        FnArg::Typed(ty) => {
            (*ty.ty).clone()
        }
    }
}

impl SignatureDerivations {
    pub fn new(sig: &Signature, self_type: Option<&Type>) -> Self {
        let fun_name = sig.ident.clone();
        let generics = GenericsDerivations::new(&sig.generics);
        let input_types: Vec<_> =
            sig.inputs.iter().map(|input| type_of_fn_arg(input, self_type)).collect();
        let receiver: Option<_> = sig.inputs.iter().take(1).filter_map(|fn_arg| match fn_arg {
            FnArg::Receiver(r) => Some(r),
            FnArg::Typed(_) => None
        }).exactly_one().ok();
        let input_names_noself: Vec<Ident> =
            sig.inputs.iter().enumerate().map(|(index, arg)| {
                Ident::new(&format!("__deanonymize_internal__{}", index), arg.span())
            }).collect();
        let mut input_names_self = input_names_noself.clone();
        let input_self: Punctuated<FnArg, Comma>;
        let mut input_noself: Punctuated<FnArg, Comma>;
        if let Some(receiver) = receiver {
            let n0 = &input_names_noself[0];
            let t0 = &input_types[0];
            let input_names = &input_names_noself[1..];
            let input_types = &input_types[1..];
            input_self =
                iter::once(FnArg::Receiver(receiver.clone()))
                    .chain(
                        input_names.iter()
                            .zip(input_types.iter())
                            .map(|(name, typ)| parse_quote!(#name : #typ))
                    ).collect();
            input_names_self[0] = format_ident!("self");
            input_noself = sig.inputs.clone();
            input_noself[0] = parse_quote!(#n0 : #t0);
        } else {
            input_self =
                input_names_noself.iter()
                    .zip(input_types.iter())
                    .map::<FnArg, _>(|(name, typ)| parse_quote!(#name : #typ))
                    .collect();
            input_noself = sig.inputs.clone();
        };

        SignatureDerivations {
            fun_name,
            generics,
            input_names_noself,
            input_names_self,
            input_self,
            input_noself,
        }
    }
}