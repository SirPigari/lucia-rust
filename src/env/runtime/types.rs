use crate::env::runtime::value::Value;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::functions::Function;
use crate::env::runtime::utils::get_inner_type;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
pub use imagnum::{
    Int, Float
};

pub const VALID_TYPES: &[&str] = &[
    "void", "any", "int", "float", "bool", "str", "map", "list", "function", "generator", "bytes", "tuple", "object", "auto", "type",
];

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
pub enum Type {
    Simple {
        name: String,
        ref_level: usize,
        is_maybe_type: bool,
    },
    Function {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
    Generator {
        parameter_types: Vec<Type>,
        yield_type: Box<Type>,
    },
    Indexed {
        base_type: Box<Type>,
        elements: Vec<Type>,
    },
    Union(Vec<Type>),
    Enum {
        name: String,
        variants: Vec<(String, Statement, usize)>,
        generics: Vec<String>,
        wheres: Vec<(String, Value)>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Statement, Vec<String>)>,
        methods: Vec<(String, Function)>,
        generics: Vec<String>,
        wheres: Vec<(String, Value)>,
    },
    Impl {
        implementations: Vec<(String, Box<Type>, Vec<String>)>,
    },
    Alias {
        name: String,
        base_type: Box<Type>,
        conditions: Vec<Statement>,
        variables: Vec<String>,
    },
    Unwrap(Vec<Value>),
}

impl Type {
    pub fn display(&self) -> String {
        match self {
            Type::Simple { name, ref_level, is_maybe_type } => {
                let mut result = "<type '".to_string();
                if *ref_level > 0 {
                    for _ in 0..*ref_level {
                        result.push('&');
                    }
                }
                if *is_maybe_type {
                    result.push('?');
                }
                result.push_str(name);
                result.push_str("'>");
                result
            },
            Type::Function { parameter_types, return_type } => {
                let params = parameter_types.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", ");
                format!("<type 'function({}) -> {}'>", params, return_type.display_simple())
            },
            Type::Generator { parameter_types, yield_type } => {
                let params = parameter_types.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", ");
                format!("<type 'generator({}) -> {}'>", params, yield_type.display_simple())
            },
            Type::Indexed { base_type, elements } => format!("<type '{}[{}]'>", base_type.display_simple(), elements.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", ")),
            Type::Union(types) => format!("<union type '{}'>", types.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(" | ")),
            Type::Enum { name, variants, .. } => {
                let variants_str = if variants.is_empty() {
                    "".to_string()
                } else {
                    if variants.len() == 1 {
                        let (variant_name, _, _) = &variants[0];
                        return format!("<enum '{}' with single variant '{}'>", name, variant_name);
                    }
                    let display_variants: Vec<String> = if variants.len() > 7 {
                        variants.iter().take(5).map(|(name, _, _)| format!("{}", name)).collect()
                    } else {
                        variants.iter().map(|(name, _, _)| format!("{}", name)).collect()
                    };
                    let mut result = display_variants.join(", ");
                    if variants.len() > 7 {
                        result.push_str(", ...");
                    }
                    format!(" with variants: {}", result)
                };
                format!("<enum '{}'{}>", name, variants_str)
            },
            Type::Struct { name, fields, .. } => {
                let fields_str = if fields.is_empty() {
                    "".to_string()
                } else {
                    if fields.len() == 1 {
                        let (field_name, _, _) = &fields[0];
                        return format!("<struct '{}' with single field '{}'>", name, field_name);
                    }
                    let display_fields: Vec<String> = if fields.len() > 7 {
                        fields.iter()
                            .take(5)
                            .map(|(k, _, _)| format!("{}", k))
                            .collect()
                    } else {
                        fields.iter()
                            .map(|(k, _, _)| format!("{}", k))
                            .collect()
                    };
                    let mut result = display_fields.join(", ");
                    if fields.len() > 7 {
                        result.push_str(", ...");
                    }
                    format!(" with fields: {}", result)
                };
                format!("<struct '{}'{}>", name, fields_str)
            },
            Type::Alias { name, base_type, .. } => format!("<type '{}' as '{}'>", base_type.display_simple(), name),
            Type::Impl { implementations } => {
                let methods_str = implementations.iter().map(|(name, func, _)| func.display_simple().replacen("function", &name, 1)).collect::<Vec<_>>().join(" + ");
                format!("<impl {}>", methods_str)
            },
            Type::Unwrap(values) => format!("<unwrap type '{}'>", values.iter().map(|v| v.get_type().display_simple()).collect::<Vec<_>>().join(", ")),
        }
    }

    pub fn display_simple(&self) -> String {
        match self {
            Type::Simple { name, ref_level, is_maybe_type } => {
                let mut result = String::new();
                if *ref_level > 0 {
                    for _ in 0..*ref_level {
                        result.push('&');
                    }
                }
                if *is_maybe_type {
                    result.push('?');
                }
                result.push_str(name);
                result
            },
            Type::Function { parameter_types, return_type } => {
                let params = parameter_types.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", ");
                format!("function({}) -> {}", params, return_type.display_simple())
            },
            Type::Generator { parameter_types, yield_type } => {
                let params = parameter_types.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", ");
                format!("generator({}) -> {}", params, yield_type.display_simple())
            },
            Type::Indexed { base_type, elements } => {
                let elements_str = elements.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", ");
                format!("{}[{}]", base_type.display_simple(), elements_str)
            },
            Type::Union(types) => types.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(" | "),
            Type::Alias { name, .. } => name.to_string(),
            Type::Enum { name, .. } => name.to_string(),
            Type::Struct { name, .. } => name.to_string(),
            Type::Impl { implementations } => {
                let methods_str = implementations
                    .iter()
                    .map(|(name, func, mods)| {
                        mods.iter()
                            .map(|s| s.as_str())
                            .collect::<Vec<_>>()
                            .join(" + ") + " " + &func.display_simple().replacen("function", &name, 1)
                    })
                    .collect::<Vec<_>>()
                    .join(" + ");
                format!("impl {}", methods_str)
            },
            _ => self.display(),
        }
    }

    pub fn set_reference(&mut self, r: usize) -> &mut Self {
        if let Type::Simple { ref_level, .. } = self {
            *ref_level = r;
        }
        self
    }

    pub fn set_maybe_type(&mut self, m: bool) -> &mut Self {
        if let Type::Simple { is_maybe_type, .. } = self {
            *is_maybe_type = m;
        }
        self
    }

    pub fn unmut(&mut self) -> Self {
        std::mem::replace(self, Type::Simple {
            name: "void".to_owned(),
            ref_level: 0,
            is_maybe_type: false,
        })
    }

    pub fn new_simple(name: &str) -> Self {
        let mut ref_level = 0;
        let mut maybe_type = false;
        let mut base_name = name.to_owned();
        while base_name.starts_with('&') || base_name.starts_with('?') {
            if base_name.starts_with('&') {
                ref_level += 1;
                base_name = base_name[1..].to_owned();
            } else if base_name.starts_with('?') {
                maybe_type = true;
                base_name = base_name[1..].to_owned();
            }
        }
        Type::Simple {
            name: base_name,
            ref_level,
            is_maybe_type: maybe_type,
        }
    }

    pub fn new_union(types: Vec<Type>) -> Self {
        Type::Union(types)
    }

    pub fn base_type(&self) -> Type {
        match get_inner_type(self) {
            Ok((_, Type::Indexed { base_type, .. })) => *base_type.clone(),
            Ok((_, Type::Alias { base_type, .. })) => *base_type.clone(),
            _ => self.clone(),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Simple { name: n1, ref_level: r1, is_maybe_type: m1 },
             Simple { name: n2, ref_level: r2, is_maybe_type: m2 }) => {
                n1 == n2 && r1 == r2 && m1 == m2
            },

            (Function { parameter_types: p1, return_type: r1 },
             Function { parameter_types: p2, return_type: r2 }) =>
                p1 == p2 && r1 == r2,

            (Generator { parameter_types: p1, yield_type: y1 },
             Generator { parameter_types: p2, yield_type: y2 }) =>
                p1 == p2 && y1 == y2,

            (Indexed { base_type: b1, elements: e1 },
             Indexed { base_type: b2, elements: e2 }) =>
                b1 == b2 && e1 == e2,

            (Union(u1), Union(u2)) =>
                u1 == u2,

            (Enum { name: n1, variants: v1, generics: g1, wheres: c1, .. },
             Enum { name: n2, variants: v2, generics: g2, wheres: c2, .. }) =>
                n1 == n2 && v1 == v2 && g1 == g2 && c1 == c2,

            (Struct { name: n1, fields: f1, generics: g1, wheres: c1, .. },
             Struct { name: n2, fields: f2, generics: g2, wheres: c2, .. }) =>
                n1 == n2 && f1 == f2 && g1 == g2 && c1 == c2,

            (Alias { name: n1, base_type: b1, variables: c1, .. },
             Alias { name: n2, base_type: b2, variables: c2, .. }) =>
                n1 == n2 && b1 == b2 && c1 == c2,

            (Impl { implementations: m1, .. },
             Impl { implementations: m2, .. }) =>
                m1 == m2,

            (Unwrap(v1), Unwrap(v2)) =>
                v1 == v2,

            _ => false,
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        return None;
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Type::*;
        std::mem::discriminant(self).hash(state);
        match self {
            Simple { name, ref_level, is_maybe_type } => {
                name.hash(state);
                ref_level.hash(state);
                is_maybe_type.hash(state);
            }
            Function { parameter_types, return_type } => {
                parameter_types.hash(state);
                return_type.hash(state);
            }
            Generator { parameter_types, yield_type } => {
                parameter_types.hash(state);
                yield_type.hash(state);
            }
            Indexed { base_type, elements } => {
                base_type.hash(state);
                elements.hash(state);
            }
            Union(u) => {
                u.hash(state);
            }
            Enum { name, generics, wheres, .. } => {
                name.hash(state);
                generics.hash(state);
                wheres.hash(state);
            }
            Struct { name, generics, wheres, .. } => {
                name.hash(state);
                generics.hash(state);
                wheres.hash(state);
            }
            Alias { name, base_type, .. } => {
                name.hash(state);
                base_type.hash(state);
            }
            Impl { implementations, .. } => {
                implementations.hash(state);
            }
            Unwrap(values) => {
                values.hash(state);
            }
        }
    }
}