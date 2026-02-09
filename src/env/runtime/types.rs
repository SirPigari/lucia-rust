use crate::env::runtime::value::Value;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::functions::Function;
use crate::env::runtime::utils::get_inner_type;
use core::fmt;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
pub use imagnum::{
    Int, Float
};

pub const VALID_TYPES: &[&str] = &[
    "void", "any", "int", "float", "bool", "str", "map", "list", "function", "generator", "bytes", "tuple", "auto", "type",
];

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode, Hash, PartialEq, Eq)]
pub enum SimpleType {
    Void,
    Any,
    Int,
    Float,
    Bool,
    Str,
    Map,
    List,
    Function,
    Generator,
    Bytes,
    Tuple,
    Auto,
    Type,
}


#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
pub enum Type {
    Simple {
        ty: SimpleType,
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
    Reference {
        base_type: Box<Type>,
        ref_level: usize,
    },
    Maybe {
        base_type: Box<Type>,
    },
    Unwrap(Vec<Value>),
}

impl Type {
    pub fn display(&self) -> String {
        match self {
            Type::Simple { ty } => {
                format!("<type '{}'>", ty.to_string())
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
                let fields = fields.iter().filter(|(_, _, m)| !m.contains(&"private".to_string())).collect::<Vec<_>>();
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
            Type::Reference { base_type, ref_level } => {
                let refs = "&".repeat(*ref_level);
                format!("<type '{}{}'>", refs, base_type.display_simple())
            },
            Type::Maybe { base_type } => format!("<type '?{}'>", base_type.display_simple()),
            Type::Unwrap(values) => format!("<unwrap type '{}'>", values.iter().map(|v| v.get_type().display_simple()).collect::<Vec<_>>().join(", ")),
        }
    }

    pub fn display_simple(&self) -> String {
        match self {
            Type::Simple { ty } => {
                ty.to_string()
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
            Type::Reference { base_type, ref_level } => {
                let refs = "&".repeat(*ref_level);
                format!("{}{}", refs, base_type.display_simple())
            },
            Type::Maybe { base_type } => format!("?{}", base_type.display_simple()),
            _ => self.display(),
        }
    }

    pub fn make_reference(&self, level: usize) -> Self {
        if level == 0 {
            return self.clone();
        }
        Type::Reference {
            base_type: Box::new(self.clone()),
            ref_level: level,
        }
    }

    pub fn make_maybe_type(&self) -> Self {
        Type::Maybe {
            base_type: Box::new(self.clone()),
        }
    }

    pub fn new_simple(name: &str) -> Self {
        let mut base = name;

        while let Some(c) = base.chars().next() {
            match c {
                '&' | '?' => {
                    base = &base[1..];
                }
                _ => break,
            }
        }

        let mut t = Type::Simple {
            ty: SimpleType::from_str(base).unwrap_or(SimpleType::Any),
        };

        let rest = name[..name.len() - base.len()].as_bytes();
        let mut i = rest.len();

        let mut ref_acc = 0;

        while i > 0 {
            i -= 1;
            match rest[i] {
                b'&' => {
                    ref_acc += 1;
                }
                b'?' => {
                    if ref_acc > 0 {
                        t = t.make_reference(ref_acc);
                        ref_acc = 0;
                    }
                    t = t.make_maybe_type();
                }
                _ => unreachable!(),
            }
        }

        if ref_acc > 0 {
            t = t.make_reference(ref_acc);
        }

        t
    }

    pub fn new_list(element_type: Type) -> Self {
        Type::Indexed {
            base_type: Box::new(Type::Simple {
                ty: SimpleType::List,
            }),
            elements: vec![element_type],
        }
    }

    pub fn new_union(types: Vec<Type>) -> Self {
        Type::Union(types)
    }

    pub fn base_type(&self) -> Type {
        match get_inner_type(self) {
            Ok((_, Type::Indexed { base_type, .. })) => *base_type.clone(),
            Ok((_, Type::Alias { base_type, .. })) => *base_type.clone(),
            Ok((_, Type::Reference { base_type, .. })) => *base_type.clone(),
            Ok((_, Type::Maybe { base_type })) => *base_type.clone(),
            _ => self.clone(),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Simple { ty: n1 },
             Simple { ty: n2 }) => {
                n1 == n2
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

            (Reference { base_type: b1, ref_level: r1 },
             Reference { base_type: b2, ref_level: r2 }) =>
                b1 == b2 && r1 == r2,

            (Maybe { base_type: b1 },
             Maybe { base_type: b2 }) =>
                b1 == b2,

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
            Simple { ty } => {
                ty.hash(state);
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
            Reference { base_type, ref_level } => {
                base_type.hash(state);
                ref_level.hash(state);
            }
            Maybe { base_type } => {
                base_type.hash(state);
            }
            Unwrap(values) => {
                values.hash(state);
            }
        }
    }
}

impl fmt::Display for SimpleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            SimpleType::Void => "void",
            SimpleType::Any => "any",
            SimpleType::Int => "int",
            SimpleType::Float => "float",
            SimpleType::Bool => "bool",
            SimpleType::Str => "str",
            SimpleType::Map => "map",
            SimpleType::List => "list",
            SimpleType::Function => "function",
            SimpleType::Generator => "generator",
            SimpleType::Bytes => "bytes",
            SimpleType::Tuple => "tuple",
            SimpleType::Auto => "auto",
            SimpleType::Type => "type",
        };
        write!(f, "{}", s)
    }
}

impl SimpleType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "void" => Some(SimpleType::Void),
            "any" => Some(SimpleType::Any),
            "int" => Some(SimpleType::Int),
            "float" => Some(SimpleType::Float),
            "bool" => Some(SimpleType::Bool),
            "str" => Some(SimpleType::Str),
            "map" => Some(SimpleType::Map),
            "list" => Some(SimpleType::List),
            "function" => Some(SimpleType::Function),
            "generator" => Some(SimpleType::Generator),
            "bytes" => Some(SimpleType::Bytes),
            "tuple" => Some(SimpleType::Tuple),
            "auto" => Some(SimpleType::Auto),
            "type" => Some(SimpleType::Type),
            _ => None,
        }
    }
    
    pub fn can_be_uninitialized(&self) -> bool {
        match self {
            SimpleType::Int
            | SimpleType::Float
            | SimpleType::Bool
            | SimpleType::Str
            | SimpleType::Map
            | SimpleType::List
            | SimpleType::Function
            | SimpleType::Bytes
            | SimpleType::Tuple
            | SimpleType::Any
            | SimpleType::Void => true,
            _ => false,
        }
    }

    pub fn can_be_uninitialized_str(s: &str) -> bool {
        match SimpleType::from_str(s) {
            Some(ty) => ty.can_be_uninitialized(),
            None => false,
        }
    }
}