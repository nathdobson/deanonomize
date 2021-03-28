use syn::{Generics, AngleBracketedGenericArguments, WhereClause, GenericParam};
use proc_macro2::Ident;
use quote::quote;
use syn::parse_quote;

pub struct GenericsDerivations {
    pub gen_params: Generics,
    pub gen_idents_turbofish: AngleBracketedGenericArguments,
    pub gen_idents_no_lifetimes: Vec<Ident>,
    pub gen_idents_no_lifetimes_turbofish: AngleBracketedGenericArguments,
    pub gen_idents_brackets: AngleBracketedGenericArguments,
    pub gen_where: Option<WhereClause>,
    pub phantom_args: AngleBracketedGenericArguments,
}

impl GenericsDerivations {
    pub fn new(gen_params: &Generics) -> Self {
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
        let gen_idents_no_lifetimes: Vec<Ident> =
            gen_params.params.iter().filter_map(|gen_param| match gen_param {
                GenericParam::Type(type_param) => {
                    let ident = &type_param.ident;
                    Some(ident.clone())
                }
                GenericParam::Lifetime(life_param) => {
                    let lifetime = &life_param.lifetime;
                    None
                }
                GenericParam::Const(const_param) => {
                    let ident = &const_param.ident;
                    Some(ident.clone())
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
        GenericsDerivations {
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