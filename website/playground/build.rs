use std::{
    collections::{HashSet, VecDeque},
    env, fs, io,
    path::PathBuf,
};

use quote::{format_ident, quote};
use schemars::{
    gen::{SchemaGenerator, SchemaSettings},
    schema::{InstanceType, RootSchema, Schema, SchemaObject, SingleOrVec},
};
use serde_json::Value;

use rome_js_factory::{
    make,
    syntax::{
        JsAnyModuleItem, JsAnyObjectMemberName, JsAnyStatement, TsAnyName, TsAnyTypeMember, TsType,
        T,
    },
};
use rome_js_formatter::{context::JsFormatContext, format_node};
use rome_rowan::{AstNode, AstSeparatedList};
use rome_service::workspace::*;

/// Manages a queue of type definitions that need to be generated
#[derive(Default)]
struct ModuleQueue<'a> {
    /// Set of type names that have already been emitted
    visited: HashSet<&'a str>,
    /// Queue of type names and definitions that need to be generated
    queue: VecDeque<(&'a str, &'a SchemaObject)>,
}

impl<'a> ModuleQueue<'a> {
    /// Add a type definition to the queue if it hasn't been emitted already
    fn push_back(&mut self, item: (&'a str, &'a SchemaObject)) {
        if self.visited.insert(item.0) {
            self.queue.push_back(item);
        }
    }

    /// Pull a type name and definition from the queue
    fn pop_front(&mut self) -> Option<(&'a str, &'a SchemaObject)> {
        self.queue.pop_front()
    }
}

/// Generate a [TsType] node from the `instance_type` of a [SchemaObject]
fn instance_type<'a>(
    queue: &mut ModuleQueue<'a>,
    root_schema: &'a RootSchema,
    schema: &'a SchemaObject,
    ty: InstanceType,
) -> TsType {
    match ty {
        // If the instance type is an object, generate a TS object type with the corresponding properties
        InstanceType::Object => {
            let object = schema.object.as_deref().unwrap();
            TsType::from(make::ts_object_type(
                make::token(T!['{']),
                make::ts_type_member_list(object.properties.iter().map(|(property, schema)| {
                    let (ts_type, optional) = schema_type(queue, root_schema, schema);
                    assert!(!optional, "optional nested types are not supported");

                    TsAnyTypeMember::from(
                        make::ts_property_signature_type_member(JsAnyObjectMemberName::from(
                            make::js_literal_member_name(make::ident(property)),
                        ))
                        .with_type_annotation(make::ts_type_annotation(make::token(T![:]), ts_type))
                        .build(),
                    )
                })),
                make::token(T!['}']),
            ))
        }
        // If the instance type is an array, generate a TS array type with the corresponding item type
        InstanceType::Array => {
            let array = schema.array.as_deref().unwrap();
            let items = array.items.as_ref().unwrap();
            match items {
                SingleOrVec::Single(schema) => {
                    let (ts_type, optional) = schema_type(queue, root_schema, schema);
                    assert!(!optional, "optional nested types are not supported");

                    TsType::from(make::ts_array_type(
                        ts_type,
                        make::token(T!['[']),
                        make::token(T![']']),
                    ))
                }
                SingleOrVec::Vec(_) => unimplemented!(),
            }
        }

        // Map native types to the corresponding TS type
        InstanceType::Null => TsType::from(make::ts_null_literal_type(make::token(T![null]))),
        InstanceType::Boolean => TsType::from(make::ts_boolean_type(make::token(T![boolean]))),
        InstanceType::String => TsType::from(make::ts_string_type(make::token(T![string]))),
        InstanceType::Number | InstanceType::Integer => {
            TsType::from(make::ts_number_type(make::token(T![number])))
        }
    }
}

/// Generate a literal [TsType] from a `serde_json` [Value]
fn value_type(value: &Value) -> TsType {
    match value {
        Value::Null => TsType::from(make::ts_null_literal_type(make::token(T![null]))),
        Value::Bool(true) => TsType::from(make::ts_boolean_literal_type(make::token(T![true]))),
        Value::Bool(false) => TsType::from(make::ts_boolean_literal_type(make::token(T![false]))),
        Value::Number(value) => TsType::from(
            make::ts_number_literal_type(make::js_number_literal(value.as_f64().unwrap())).build(),
        ),
        Value::String(value) => {
            TsType::from(make::ts_string_literal_type(make::js_string_literal(value)))
        }
        Value::Array(_) => unimplemented!(),
        Value::Object(_) => unimplemented!(),
    }
}

/// Generate a union [TsType] node from a list of [TsType]s,
/// flattening any nested union type the iterator may emit
fn make_union_type(items: impl IntoIterator<Item = TsType>) -> TsType {
    let mut result = Vec::new();

    for item in items {
        if let TsType::TsUnionType(union_type) = item {
            for item in union_type.types().iter() {
                result.push(item.unwrap());
            }
        } else {
            result.push(item);
        }
    }

    let separators = (0..result.len().saturating_sub(1)).map(|_| make::token(T![|]));
    TsType::from(make::ts_union_type(make::ts_union_type_variant_list(result, separators)).build())
}

/// Generate a [TsType] node from a [SchemaObject], returning the generated
/// TypeScript type along with a boolean flag indicating whether the type is
/// considered "optional" in the schema
fn schema_object_type<'a>(
    queue: &mut ModuleQueue<'a>,
    root_schema: &'a RootSchema,
    schema: &'a SchemaObject,
) -> (TsType, bool) {
    // Start by detecting enum types by inspecting the `enum_values` field, i
    // the field is set return a union type generated from the literal enum values
    let ts_type = schema
        .enum_values
        .as_deref()
        .map(|enum_values| make_union_type(enum_values.iter().map(value_type)))
        // If the type isn't an enum, inspect its `instance_type` field, if the
        // field is set return a type annotation for the corresponding type
        .or_else(|| {
            Some(match schema.instance_type.as_ref()? {
                SingleOrVec::Single(ty) => instance_type(queue, root_schema, schema, **ty),
                SingleOrVec::Vec(types) => make_union_type(
                    types
                        .iter()
                        .map(|ty| instance_type(queue, root_schema, schema, *ty)),
                ),
            })
        })
        // Otherwise inspect the `reference` field of the schema, if its set return
        // a TS reference type and add the corresponding type to the queue
        .or_else(|| {
            let reference = schema.reference.as_deref()?;
            let key = reference.trim_start_matches("#/components/schemas/");
            match root_schema.definitions.get(key) {
                Some(Schema::Bool(_)) => unimplemented!(),
                Some(Schema::Object(schema)) => queue.push_back((key, schema)),
                None => panic!("definition for type {key:?} not found"),
            }

            Some(TsType::from(
                make::ts_reference_type(TsAnyName::from(make::js_reference_identifier(
                    make::ident(key),
                )))
                .build(),
            ))
        })
        // Finally try to inspect the subschemas for this type
        .or_else(|| {
            let subschemas = schema.subschemas.as_deref()?;
            // First try to inspect the `all_of` list of subschemas, if it's
            // set generate an intersection type from it
            subschemas
                .all_of
                .as_deref()
                .map(|all_of| {
                    TsType::from(
                        make::ts_intersection_type(make::ts_intersection_type_element_list(
                            all_of.iter().map(|ty| {
                                let (ts_type, optional) = schema_type(queue, root_schema, ty);
                                assert!(!optional, "optional nested types are not supported");
                                ts_type
                            }),
                            (0..all_of.len().saturating_sub(1)).map(|_| make::token(T![&])),
                        ))
                        .build(),
                    )
                })
                // Otherwise try to inspect the `any_of` list of subschemas, and
                // generate the corresponding union type for it
                .or_else(|| {
                    let any_of = subschemas
                        .any_of
                        .as_deref()
                        .or(subschemas.one_of.as_deref())?;

                    Some(make_union_type(any_of.iter().map(|ty| {
                        let (ts_type, optional) = schema_type(queue, root_schema, ty);
                        assert!(!optional, "optional nested types are not supported");
                        ts_type
                    })))
                })
        })
        .unwrap_or_else(|| panic!("unimplemented schema {schema:#?}"));

    // Types are considered "optional" in the serialization protocol if they
    // have the `nullable` OpenAPI extension property, or if they have a default value
    let is_nullable = matches!(schema.extensions.get("nullable"), Some(Value::Bool(true)));
    let has_defaults = schema
        .metadata
        .as_ref()
        .map(|metadata| metadata.default.is_some())
        .unwrap_or(false);

    (ts_type, is_nullable || has_defaults)
}

/// Generate a [TsType] node from a [Schema], returning the generated type
/// along with a boolean flag indicating whether the type is considered
/// "optional" in the schema
fn schema_type<'a>(
    queue: &mut ModuleQueue<'a>,
    root_schema: &'a RootSchema,
    schema: &'a Schema,
) -> (TsType, bool) {
    match schema {
        // Types defined as `true` in the schema always pass validation,
        // map them to the `any` type
        Schema::Bool(true) => (TsType::from(make::ts_any_type(make::token(T![any]))), true),
        // Types defined as `false` in the schema never pass validation,
        // map them to the `never` type
        Schema::Bool(false) => (
            TsType::from(make::ts_never_type(make::token(T![never]))),
            false,
        ),
        Schema::Object(schema_object) => schema_object_type(queue, root_schema, schema_object),
    }
}

/// Generate and emit all the types defined in `root_schema` into the `module`
fn generate_type<'a>(
    module: &mut Vec<JsAnyModuleItem>,
    queue: &mut ModuleQueue<'a>,
    root_schema: &'a RootSchema,
) {
    // Read the root type of the schema and push it to the queue
    let root_name = root_schema
        .schema
        .metadata
        .as_deref()
        .and_then(|metadata| metadata.title.as_deref())
        .unwrap();

    queue.push_back((root_name, &root_schema.schema));

    while let Some((name, schema)) = queue.pop_front() {
        // Detect if the type being emitted is an object, emit it as an
        // interface definition if that's the case
        let is_interface = schema
            .instance_type
            .as_ref()
            .map(|instance_type| {
                if let SingleOrVec::Single(instance_type) = instance_type {
                    matches!(**instance_type, InstanceType::Object)
                } else {
                    false
                }
            })
            .unwrap_or_else(|| schema.object.is_some());

        if is_interface {
            let mut members = Vec::new();

            // Create a property signature member in the interface for each
            // property of the corresponding schema object
            let object = schema.object.as_deref().unwrap();
            for (property, schema) in &object.properties {
                let (ts_type, optional) = schema_type(queue, root_schema, schema);

                let mut builder =
                    make::ts_property_signature_type_member(JsAnyObjectMemberName::from(
                        make::js_literal_member_name(make::ident(property)),
                    ))
                    .with_type_annotation(make::ts_type_annotation(make::token(T![:]), ts_type));

                if optional {
                    builder = builder.with_optional_token(make::token(T![?]));
                }

                members.push(TsAnyTypeMember::from(builder.build()));
            }

            module.push(JsAnyModuleItem::JsAnyStatement(JsAnyStatement::from(
                make::ts_interface_declaration(
                    make::token(T![interface]),
                    make::ts_identifier_binding(make::ident(name)),
                    make::token(T!['{']),
                    make::ts_type_member_list(members),
                    make::token(T!['}']),
                )
                .build(),
            )));
        } else {
            // If the schema for this type is not an object, emit it as a type alias
            let (ts_type, optional) = schema_object_type(queue, root_schema, schema);
            assert!(!optional, "optional nested types are not supported");

            module.push(JsAnyModuleItem::JsAnyStatement(JsAnyStatement::from(
                make::ts_type_alias_declaration(
                    make::token(T![type]),
                    make::ts_identifier_binding(make::ident(name)),
                    make::token(T![=]),
                    ts_type,
                )
                .build(),
            )));
        }
    }
}

/// Helper macro for generated an OpenAPI schema for a type implementing JsonSchema
macro_rules! openapi_schema_for {
    ($ty:ty) => {
        SchemaGenerator::from(SchemaSettings::openapi3()).root_schema_for::<$ty>()
    };
}

/// Generate type definitions for all the data structures of the `Workspace`
/// protocol, returning the generated TypeScript code and the list of all the
/// emitted type names
fn write_protocol() -> (String, Vec<String>) {
    let schemas = [
        openapi_schema_for!(SupportsFeatureParams),
        openapi_schema_for!(UpdateSettingsParams),
        openapi_schema_for!(OpenFileParams),
        openapi_schema_for!(GetSyntaxTreeParams),
        openapi_schema_for!(GetControlFlowGraphParams),
        openapi_schema_for!(GetFormatterIRParams),
        openapi_schema_for!(ChangeFileParams),
        openapi_schema_for!(CloseFileParams),
        openapi_schema_for!(PullDiagnosticsParams),
        openapi_schema_for!(PullActionsParams),
        openapi_schema_for!(FormatFileParams),
        openapi_schema_for!(FormatRangeParams),
        openapi_schema_for!(FormatOnTypeParams),
        openapi_schema_for!(FixFileParams),
        openapi_schema_for!(RenameParams),
        openapi_schema_for!(GetSyntaxTreeResult),
        openapi_schema_for!(PullDiagnosticsResult),
        openapi_schema_for!(PullActionsResult),
        openapi_schema_for!(FixFileResult),
        openapi_schema_for!(RenameResult),
    ];

    let mut items = Vec::new();
    let mut queue = ModuleQueue::default();

    for schema in &schemas {
        generate_type(&mut items, &mut queue, schema);
    }

    let module = make::js_module(
        make::js_directive_list(None),
        make::js_module_item_list(items),
        make::eof(),
    )
    .build();

    // Wasm-bindgen will paste the generated TS code as-is into the final .d.ts file,
    // ensure it looks good by running it through the formatter
    let formatted = format_node(JsFormatContext::default(), module.syntax()).unwrap();
    let printed = formatted.print();

    let types = queue.visited.iter().map(|name| name.to_string()).collect();
    (printed.into_code(), types)
}

fn main() -> io::Result<()> {
    let (definitions, types) = write_protocol();

    // Generate wasm-bindgen extern type imports for all the types defined in the TS code
    let types = types.into_iter().map(|name| {
        let ident = format_ident!("I{name}");
        quote! {
            #[wasm_bindgen(typescript_type = #name)]
            #[allow(non_camel_case_types)]
            pub type #ident;
        }
    });

    let tokens = quote! {
        #[wasm_bindgen(typescript_custom_section)]
        const TS_TYPEDEFS: &'static str = #definitions;

        #[wasm_bindgen]
        extern "C" {
            #( #types )*
        }
    };

    let out_dir = env::var("OUT_DIR").unwrap();
    fs::write(
        PathBuf::from(out_dir).join("ts_types.rs"),
        tokens.to_string(),
    )?;

    Ok(())
}
