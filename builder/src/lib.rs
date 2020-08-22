// TODO: The code passes the tests but is extremly ugly. Refactor.

use {
    quote::quote,
    syn::{
        AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, FieldsNamed,
        GenericArgument, Ident, PathArguments, Type, TypePath,
    },
};

struct Fields {
    fields: Vec<Field>,
}

impl Fields {
    fn idents(&self) -> impl Iterator<Item = &Ident> {
        self.fields.iter().map(|field| &field.ident)
    }

    fn types(&self) -> impl Iterator<Item = &Type> {
        self.fields.iter().map(|field| &field.ty)
    }
}

struct Field {
    ident: Ident,
    ty: Type,
    attrs: Vec<Attribute>,
}

enum OuterTypeIdentLevel {
    External,
    Internal,
}

impl Field {
    fn outer_type_ident(ty: &Type, level: OuterTypeIdentLevel) -> Option<Ident> {
        match level {
            OuterTypeIdentLevel::External => match ty {
                Type::Path(TypePath { qself: _, path }) => {
                    path.segments.first().map(|seg| seg.ident.clone().into())
                }
                _ => None,
            },
            OuterTypeIdentLevel::Internal => match ty {
                Type::Path(TypePath { qself: _, path }) => {
                    match &path.segments.first()?.arguments {
                        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            colon2_token: _,
                            lt_token: _,
                            args,
                            gt_token: _,
                        }) => match args.first()? {
                            GenericArgument::Type(ty) => {
                                Self::outer_type_ident(ty, OuterTypeIdentLevel::External)
                            }
                            _ => None,
                        },
                        _ => None,
                    }
                }
                _ => None,
            },
        }
    }

    fn is_option(&self) -> bool {
        Self::outer_type_ident(&self.ty, OuterTypeIdentLevel::External)
            .map(|x| x == "Option")
            .unwrap_or(false)
    }

    fn collection_inner_type(&self) -> Option<&Type> {
        let is_vec = (Self::outer_type_ident(
            &self.ty,
            if self.is_option() {
                OuterTypeIdentLevel::Internal
            } else {
                OuterTypeIdentLevel::External
            },
        ))
        .map(|x| x == "Vec")
        .unwrap_or(false);

        if !is_vec {
            return None;
        }

        match &self.ty {
            Type::Path(TypePath { qself: _, path }) => match &path.segments.first()?.arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: _,
                    lt_token: _,
                    gt_token: _,
                    args,
                }) => match args.first()? {
                    GenericArgument::Type(ty) => Some(ty),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn collection_attr_name(&self) -> Result<Option<Ident>, syn::Error> {
        let is_vec = Self::outer_type_ident(
            &self.ty,
            if self.is_option() {
                OuterTypeIdentLevel::Internal
            } else {
                OuterTypeIdentLevel::External
            },
        )
        .map(|x| x == "Vec")
        .unwrap_or(false);

        if !is_vec {
            return Ok(None);
        }

        let attr = self.attrs.first();
        match attr {
            Some(attr) => {
                let is_builder = attr
                    .path
                    .segments
                    .first()
                    .map(|x| x.ident == "builder")
                    .unwrap_or(false);
                if !is_builder {
                    return Ok(None);
                }
                match &attr.tokens.clone().into_iter().nth(0) {
                    Some(proc_macro2::TokenTree::Group(masiero)) => {
                        let mut stream = masiero.stream().into_iter();
                        match (stream.nth(0), stream.nth(1)) {
                            (
                                Some(proc_macro2::TokenTree::Ident(ident)),
                                Some(proc_macro2::TokenTree::Literal(literal)),
                            ) => {
                                if ident == "each" {
                                    let literal = literal.to_string();
                                    if let Some(ident_from_literal) = literal.split('"').nth(1) {
                                        Ok(Some(Ident::new(
                                            ident_from_literal,
                                            proc_macro::Span::call_site().into(),
                                        )))
                                    } else {
                                        Ok(None)
                                    }
                                } else {
                                    Err(syn::Error::new(
                                        ident.span(),
                                        "expected `builder(each = \"...\")`",
                                    ))
                                }
                            }
                            _ => Ok(None),
                        }
                    }
                    _ => Ok(None),
                }
            }
            None => Ok(None),
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = Ident::new(
        &format!("{}Builder", name),
        proc_macro::Span::call_site().into(),
    );

    let struct_fields = Fields {
        fields: match input.data {
            Data::Struct(DataStruct {
                fields,
                semi_token: _,
                struct_token: _,
            }) => match fields {
                syn::Fields::Named(FieldsNamed {
                    named,
                    brace_token: _,
                }) => named
                    .into_iter()
                    .map(|field| Field {
                        ident: field.ident.expect("Struct field must have an identifier."),
                        ty: field.ty,
                        attrs: field.attrs,
                    })
                    .collect::<Vec<_>>(),
                _ => unimplemented!("Only named fields are supported at the moment."),
            },
            _ => unimplemented!("Only data structs are supported at the moment."),
        },
    };

    let (builder_methods, actual_struct_fields_val, required_field_not_specified_checks) = {
        let mut builder_methods = Vec::new();
        let mut actual_struct_fields_val = Vec::new();
        let mut required_field_not_specified_checks = Vec::new();

        for field in &struct_fields.fields {
            let ident = &field.ident;
            let ty = &field.ty;
            let is_option = field.is_option();
            let collection_attr_name = field.collection_attr_name();

            builder_methods.push(if is_option {
                // TODO: unwrap and panic
                let ty = match ty {
                    Type::Path(TypePath { qself: _, path }) => path
                        .segments
                        .first()
                        .map(|seg| match &seg.arguments {
                            PathArguments::AngleBracketed(
                                AngleBracketedGenericArguments {
                                    colon2_token: _,
                                    lt_token: _,
                                    gt_token: _,
                                    args,
                                },
                            ) => args.first().unwrap(),
                            _ => panic!(),
                        })
                        .unwrap(),
                    _ => panic!(),
                };

                let non_extra_method = quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(std::option::Option::Some(#ident));
                        self
                    }
                };

                match collection_attr_name {
                    Ok(Some(collection_attr_name)) => {
                        let inner_type = field.collection_inner_type().unwrap();
                        let extra_method = quote! {
                            fn #collection_attr_name(&mut self, #collection_attr_name: #inner_type) -> &mut Self {
                                match &mut self.#ident {
                                    std::option::Option::Some(std::option::Option::Some(v)) => {
                                        v.push(#collection_attr_name);
                                    }
                                    std::option::Option::Some(std::option::Option::None) | std::option::Option::None => {
                                        self.#ident = std::option::Option::Some(std::option::Option::Some(vec![#collection_attr_name]));
                                    }
                                };
                                self
                            }
                        };
                        if collection_attr_name == *ident {
                            extra_method
                        } else {
                            quote! {
                                #non_extra_method

                                #extra_method
                            }
                        }
                    }
                    Ok(None) => non_extra_method,
                    Err(err) => return err.to_compile_error().into()
                }
            } else {
                let non_extra_method = quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                };

                match collection_attr_name {
                    Ok(Some(collection_attr_name)) => {
                        let inner_type = field.collection_inner_type().unwrap();
                        let extra_method = quote! {
                            fn #collection_attr_name(&mut self, #collection_attr_name: #inner_type) -> &mut Self {
                                match &mut self.#ident {
                                    std::option::Option::Some(v) => {
                                        v.push(#collection_attr_name);
                                    }
                                    std::option::Option::None => {
                                        self.#ident = std::option::Option::Some(vec![#collection_attr_name]);
                                    }
                                }
                                self
                            }
                        };
                        if collection_attr_name == *ident {
                            extra_method
                        } else {
                            quote! {
                                #non_extra_method

                                #extra_method
                            }
                        }
                    }
                    Ok(None) => non_extra_method,
                    Err(err) => return err.to_compile_error().into()
                }
            });

            actual_struct_fields_val.push(if is_option {
                quote! {
                    self.#ident.as_ref().map(|x| x.clone()).unwrap_or(None)
                }
            } else {
                quote! {
                    self.#ident.as_ref().unwrap().clone()
                }
            });

            if !is_option {
                required_field_not_specified_checks.push(quote! {
                if self.#ident.is_none() {
                    return std::result::Result::Err(std::format!("Field {} is missing", std::stringify!(#ident)).into());
                }
            });
            }
        }

        (
            builder_methods,
            actual_struct_fields_val,
            required_field_not_specified_checks,
        )
    };
    let idents = struct_fields.idents().collect::<Vec<_>>();
    let types = struct_fields.types();

    let quoted = quote! {
        pub struct #builder_name {
            #( #idents: std::option::Option<#types> ),*
        }

        impl #builder_name {
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #( #required_field_not_specified_checks )*

                std::result::Result::Ok(#name {
                    #( #idents: #actual_struct_fields_val ),*
                })
            }

            #( #builder_methods )*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #( #idents: std::option::Option::None ),*
                }
            }
        }
    };
    quoted.into()
}
