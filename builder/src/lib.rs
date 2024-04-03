use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput, Expr,
    ExprAssign, Fields, Ident, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let idents_and_types = builder_fields(&input.data);

    let fields_with_value_types = idents_and_types.iter().map(|field_meta| {
        let ident = &field_meta.ident;
        let ty = &field_meta.ty;

        if get_inner_type_if_optional(ty).is_some() {
            quote_spanned! {
                field_meta.span.span() =>
                    #ident: #ty
            }
        } else {
            if let Some(_) = field_meta
                .first_attr
                .as_ref()
                .and_then(get_repeated_field_builder_ident)
            {
                let inner_vec_type =
                    get_inner_type_if_vec(ty).expect("Expected a vector");
                quote_spanned! {
                    field_meta.span.span() =>
                        #ident: std::vec::Vec<#inner_vec_type>
                }
            } else {
                quote_spanned! {
                    field_meta.span.span() =>
                        #ident: std::option::Option<#ty>
                }
            }
        }
    });
    let fields_with_none = idents_and_types.iter().map(|field_meta| {
        let ident = &field_meta.ident;
        if let Some(_) = field_meta
            .first_attr
            .as_ref()
            .and_then(get_repeated_field_builder_ident)
        {
            quote_spanned! {
                field_meta.span.span() =>
                    #ident: std::vec::Vec::new()
            }
        } else {
            quote_spanned! {
                field_meta.span.span() =>
                    #ident: std::option::Option::None
            }
        }
    });
    let setter_fns_for_fields = idents_and_types.iter().map(|field_meta| {
        let ident = &field_meta.ident;
        let ty = &field_meta.ty;

        if let Some(inner_type) = get_inner_type_if_optional(ty) {
            quote_spanned! {
                field_meta.span.span() =>
                    fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
            }
        } else {
            if let Some(fn_name_of_repeated_vec_field) = field_meta
                .first_attr
                .as_ref()
                .and_then(get_repeated_field_builder_ident).as_ref().and_then(|path| path.get_ident())
            {
                let inner_vec_type = get_inner_type_if_vec(ty).expect("Expected a vector");
                quote_spanned! {
                    field_meta.span.span() =>
                        fn #fn_name_of_repeated_vec_field(&mut self, #ident: #inner_vec_type) -> &mut Self {
                            self.#ident.push(#ident);
                            self
                        }
                }
            } else {
                quote_spanned! {
                    field_meta.span.span() =>
                        fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                }
            }
        }
    });
    let all_fields_is_some = idents_and_types.iter().map(|field_meta| {
        let ident = &field_meta.ident;
        if let Some(_) = field_meta
            .first_attr
            .as_ref()
            .and_then(get_repeated_field_builder_ident)
        {
            quote_spanned! {
                field_meta.span.span() =>
                    true
            }
        } else {
            let is_optional_field =
                get_inner_type_if_optional(&field_meta.ty).is_some();
            quote_spanned! {
                field_meta.span.span() =>
                    (self.#ident.is_some() | #is_optional_field)
            }
        }
    });
    let all_fields_are_set = quote! {
        # ( #all_fields_is_some )&*
    };

    let field_names_being_set = idents_and_types.iter().map(|field_meta| {
        let ident = &field_meta.ident;
        if get_inner_type_if_optional(&field_meta.ty).is_some() {
            quote_spanned! {
                field_meta.span.span() => #ident: self.#ident.take()
            }
        } else {
            if let Some(_) = field_meta
                .first_attr
                .as_ref()
                .and_then(get_repeated_field_builder_ident) {
                    quote_spanned! {
                        field_meta.span.span() => #ident: self.#ident.drain(..).collect()
                    }
                } else {
                    quote_spanned! {
                        field_meta.span.span() => #ident: self.#ident.take().unwrap()
                    }
                }

        }
    });

    let builder_name =
        Ident::new(&format!("{}Builder", name), Span::call_site());

    let expanded = quote! {

        pub struct #builder_name {
            #( #fields_with_value_types, )*
        }

        impl #builder_name {
            #( #setter_fns_for_fields )*

            pub fn build(&mut self)
            -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                if !(#all_fields_are_set) {
                    std::result::Result::Err("All fields are not set".into())
                } else {
                    std::result::Result::Ok(
                        #name {
                            #( #field_names_being_set, )*
                        }
                    )
                }
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #( #fields_with_none, )*
                }
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

struct FieldMeta {
    ident: Option<Ident>,
    ty: Type,
    span: Span,
    first_attr: Option<Attribute>,
}

fn builder_fields(data: &Data) -> Vec<FieldMeta> {
    match *data {
        Data::Struct(ref data_struct) => match data_struct.fields {
            Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| FieldMeta {
                    ident: f.ident.clone(),
                    ty: f.ty.clone(),
                    span: f.span(),
                    first_attr: f.attrs.first().cloned(),
                })
                .collect(),

            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn get_inner_type_if_optional(ty: &Type) -> Option<syn::Type> {
    match ty {
        Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments, .. },
        }) => match segments.first() {
            Some(syn::PathSegment {
                ident,
                arguments:
                    syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments { args, .. },
                    ),
            }) => {
                if ident.to_string() == "Option".to_string() {
                    match args.first() {
                        Some(syn::GenericArgument::Type(ty)) => {
                            Some(ty.clone())
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn get_inner_type_if_vec(ty: &Type) -> Option<syn::Type> {
    match ty {
        Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments, .. },
        }) => match segments.first() {
            Some(syn::PathSegment {
                ident,
                arguments:
                    syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments { args, .. },
                    ),
            }) => {
                if ident.to_string() == "Vec".to_string() {
                    match args.first() {
                        Some(syn::GenericArgument::Type(ty)) => {
                            Some(ty.clone())
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn get_repeated_field_builder_ident(attr: &Attribute) -> Option<syn::Path> {
    if attr.path().is_ident("builder") {
        if let Ok(expr) = attr.parse_args::<Expr>() {
            return match expr {
                Expr::Assign(ExprAssign { left, right, .. }) => {
                    if let Expr::Path(p) = *left {
                        if p.path.is_ident("each") {
                            if let Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Str(s),
                                ..
                            }) = *right
                            {
                                return s.parse().ok();
                            }
                        }
                    }
                    None
                }
                _ => None,
            };
        }
    }
    None
}
