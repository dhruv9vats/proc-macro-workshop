mod builder;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    builder::expand(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
