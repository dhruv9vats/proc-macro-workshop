use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    spanned::Spanned, Attribute, Data, DeriveInput, Error, Expr, ExprAssign,
    Fields, Ident, Result, Type,
};

pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
    let idents_and_types = builder_fields(&input.data)?;
    let num_fields = idents_and_types.len();
    let mut fields_with_value_types =
        Vec::<TokenStream>::with_capacity(num_fields);
    let mut fields_with_default_values =
        Vec::<TokenStream>::with_capacity(num_fields);
    let mut setter_fns_for_fields =
        Vec::<TokenStream>::with_capacity(num_fields);
    let mut fields_validity = Vec::<TokenStream>::with_capacity(num_fields);
    let mut fields_assigners = Vec::<TokenStream>::with_capacity(num_fields);

    idents_and_types
        .iter()
        .map(|field_meta| -> Result<()> {
            let ident = &field_meta.ident;
            let ty = &field_meta.ty;

            if let Some(inner_optional_type) = inner_type_if_optional(ty) {
                // If the field type is Option<Type>
                fields_with_value_types.push(quote_spanned! {
                    field_meta.span.span() =>
                        #ident: std::option::Option<#inner_optional_type>
                });
                fields_with_default_values.push(quote_spanned! {
                    field_meta.span.span() =>
                        #ident: std::option::Option::None
                });
                setter_fns_for_fields.push(quote_spanned! {
                    field_meta.span.span() =>
                        fn #ident(&mut self, #ident: #inner_optional_type)
                        -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                });
                fields_validity.push(quote_spanned! {
                    // Unset optional fields are allowed
                    field_meta.span.span() => true
                });
                fields_assigners.push(quote_spanned! {
                    field_meta.span.span() => #ident: self.#ident.take()
                });
            } else if let Some(fn_name_of_repeated_vec_field) = field_meta
                .first_attr
                .and_then(get_repeated_field_builder_fn_ident)
            {
                // [builder(each = "...")] is specified

                if let Some(inner_vec_type) = inner_type_if_vec(ty) {
                    let fn_name = fn_name_of_repeated_vec_field?;
                    // If the field type is Vec<Type>, with [builder(each = "...")] specified
                    fields_with_value_types.push(quote_spanned! {
                        field_meta.span.span() =>
                            #ident: std::vec::Vec<#inner_vec_type>
                    });
                    fields_with_default_values.push(quote_spanned! {
                        field_meta.span.span() =>
                                #ident: std::vec::Vec::new()
                    });
                    setter_fns_for_fields.push(quote_spanned! {
                        field_meta.span.span() =>
                        fn #fn_name(&mut self, #ident: #inner_vec_type)
                        -> &mut Self {
                            self.#ident.push(#ident);
                            self
                        }
                    });
                    fields_validity.push(quote_spanned! {
                        // No values for Vec fields is allowed
                        field_meta.span.span() => true
                    });
                    fields_assigners.push(quote_spanned! {
                        field_meta.span.span() =>
                            #ident: self.#ident.drain(..).collect()
                    });
                } else {
                    return Err(Error::new_spanned(
                    &ident,
                    "Must be a vector if builder(each = \"...\") is specified.",
                ));
                }
            } else {
                fields_with_value_types.push(quote_spanned! {
                    field_meta.span.span() => #ident: std::option::Option<#ty>
                });
                fields_with_default_values.push(quote_spanned! {
                    field_meta.span.span() =>
                        #ident: std::option::Option::None
                });
                setter_fns_for_fields.push(quote_spanned! {
                    field_meta.span.span() =>
                        fn #ident(&mut self, #ident: #ty)
                        -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                });
                fields_validity.push(quote_spanned! {
                    field_meta.span.span() => self.#ident.is_some()
                });
                fields_assigners.push(quote_spanned! {
                    field_meta.span.span() => #ident: self.#ident.take().unwrap()
                });
            }
            Ok(())
        })
        .collect::<Result<()>>()?;

    let all_fields_are_valid = quote! {
        # ( #fields_validity )&*
    };

    let name = input.ident;
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
                if !(#all_fields_are_valid) {
                    std::result::Result::Err("All fields are not set".into())
                } else {
                    std::result::Result::Ok(
                        #name {
                            #( #fields_assigners, )*
                        }
                    )
                }
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #( #fields_with_default_values, )*
                }
            }
        }
    };

    Ok(expanded.into())
}

struct FieldMeta<'a> {
    ident: Option<&'a Ident>,
    ty: &'a Type,
    span: Span,
    first_attr: Option<&'a Attribute>,
}

fn builder_fields(data: &Data) -> Result<Vec<FieldMeta>> {
    match *data {
        Data::Struct(ref data_struct) => match data_struct.fields {
            Fields::Named(ref fields) => Ok(fields
                .named
                .iter()
                .map(|f| FieldMeta {
                    ident: f.ident.as_ref(),
                    ty: &f.ty,
                    span: f.span(),
                    first_attr: f.attrs.first(),
                })
                .collect()),

            _ => Err(Error::new_spanned(
                &data_struct.fields,
                "Only named fields are supported",
            )),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

/// Returns the type the `Option` is wrapping around, if the supplied
/// type is `Option<Type>`
fn inner_type_if_optional(ty: &Type) -> Option<&syn::Type> {
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
                        Some(syn::GenericArgument::Type(ty)) => Some(ty),
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

fn inner_type_if_vec(ty: &Type) -> Option<&syn::Type> {
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
                        Some(syn::GenericArgument::Type(ty)) => Some(ty),
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

fn get_repeated_field_builder_fn_ident(
    attr: &Attribute,
) -> Option<syn::Result<syn::Path>> {
    if attr.path().is_ident("builder") {
        if let Ok(expr) = attr.parse_args::<Expr>() {
            return match expr {
                Expr::Assign(ExprAssign { left, right, .. }) => {
                    if let Expr::Path(p) = &*left {
                        if p.path.is_ident("each") {
                            if let Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Str(s),
                                ..
                            }) = &*right
                            {
                                return Some(s.parse());
                            }
                        }
                    }
                    Some(Err(syn::Error::new_spanned(
                        left,
                        "expected \"each\" in `builder(each = \"...\")`",
                    )))
                }
                _ => Some(Err(syn::Error::new_spanned(
                    expr,
                    "expected `builder(each = \"...\")`",
                ))),
            };
        }
    }
    None
}
