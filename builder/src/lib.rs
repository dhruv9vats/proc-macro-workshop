use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields, Ident, Type,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let idents_and_types = builder_fields(&input.data);

    let fields_with_value_types =
        idents_and_types.iter().map(|(ident, ty, spanned)| {
            quote_spanned! { spanned.span() => #ident: Option<#ty> }
        });
    let fields_with_none =
        idents_and_types.iter().map(|(ident, _ty, spanned)| {
            quote_spanned! { spanned.span() => #ident: None }
        });
    let setter_fns_for_fields =
        idents_and_types.iter().map(|(ident, ty, spanned)| {
            quote_spanned! { spanned.span() =>
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
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

fn builder_fields(data: &Data) -> Vec<(Option<Ident>, Type, impl Spanned)> {
    match *data {
        Data::Struct(ref data_struct) => match data_struct.fields {
            Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| (f.ident.clone(), f.ty.clone(), f.span()))
                .collect(),

            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
