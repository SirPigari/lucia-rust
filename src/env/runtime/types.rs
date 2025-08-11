use crate::env::runtime::value::Value;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
pub use imagnum::{
    Int, Float
};

pub const VALID_TYPES: &[&str] = &[
    "void", "any", "int", "float", "bool", "str", "map", "list", "function", "generator", "error", "bytes", "tuple", "object", "auto", "type",
];

#[derive(Debug, Clone)]
pub enum Type {
    Simple {
        name: String,
        is_reference: bool,
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
        variants: Vec<Value>,
        generics: Vec<Value>,
        conditions: Vec<Value>,
        variables: Vec<Value>,
    },
    EnumInstance {
        name: String,
        variant: String,
        value: Box<Type>,
    },
    Alias {
        name: String,
        base_type: Box<Type>,
        conditions: Vec<Value>,
        variables: Vec<Value>,
    }
}

impl Type {
    pub fn display(&self) -> String {
        match self {
            Type::Simple { name, is_reference, is_maybe_type } => {
                let mut result = "<type '".to_string();
                if *is_reference {
                    result.push('&');
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
            Type::Enum { name, .. } => format!("<enum '{}'>", name),
            Type::EnumInstance { name, variant, value } => format!("<type '{}::{}({})'>", name, variant, value.display_simple()),
            Type::Alias { name, base_type, .. } => format!("<type '{} as {}'>", name, base_type.display_simple()),
        }
    }

    pub fn display_simple(&self) -> String {
        match self {
            Type::Simple { name, is_reference, is_maybe_type } => {
                let mut result = String::new();
                if *is_reference {
                    result.push('&');
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
            _ => self.display(),
        }
    }

    pub fn set_reference(&mut self, r: bool) -> &mut Self {
        if let Type::Simple { is_reference, .. } = self {
            *is_reference = r;
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
            name: "void".to_string(),
            is_reference: false,
            is_maybe_type: false,
        })
    }

    pub fn new_simple(name: &str) -> Self {
        Type::Simple {
            name: name.to_string(),
            is_reference: false,
            is_maybe_type: false,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Simple { name: n1, is_reference: r1, is_maybe_type: m1 },
             Simple { name: n2, is_reference: r2, is_maybe_type: m2 }) =>
                n1 == n2 && r1 == r2 && m1 == m2,

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

            (Enum { name: n1, variants: v1, generics: g1, conditions: c1, .. },
             Enum { name: n2, variants: v2, generics: g2, conditions: c2, .. }) =>
                n1 == n2 && v1 == v2 && g1 == g2 && c1 == c2,

            (EnumInstance { name: n1, variant: v1, value: val1 },
             EnumInstance { name: n2, variant: v2, value: val2 }) =>
                n1 == n2 && v1 == v2 && val1 == val2,

            (Alias { name: n1, base_type: b1, conditions: c1, .. },
             Alias { name: n2, base_type: b2, conditions: c2, .. }) =>
                n1 == n2 && b1 == b2 && c1 == c2,

            _ => false
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Type::*;
        match (self, other) {
            (Simple { name: n1, is_reference: r1, is_maybe_type: m1 },
             Simple { name: n2, is_reference: r2, is_maybe_type: m2 }) =>
                (n1, r1, m1).partial_cmp(&(n2, r2, m2)),

            (Function { parameter_types: p1, return_type: r1 },
             Function { parameter_types: p2, return_type: r2 }) =>
                (p1, r1).partial_cmp(&(p2, r2)),

            (Generator { parameter_types: p1, yield_type: y1 },
             Generator { parameter_types: p2, yield_type: y2 }) =>
                (p1, y1).partial_cmp(&(p2, y2)),

            (Indexed { base_type: b1, elements: e1 },
             Indexed { base_type: b2, elements: e2 }) =>
                (b1, e1).partial_cmp(&(b2, e2)),

            (Union(u1), Union(u2)) =>
                u1.partial_cmp(u2),

            (Enum { name: n1, variants: v1, generics: g1, conditions: c1, .. },
             Enum { name: n2, variants: v2, generics: g2, conditions: c2, .. }) =>
                (n1, v1, g1, c1).partial_cmp(&(n2, v2, g2, c2)),

            (EnumInstance { name: n1, variant: v1, value: val1 },
             EnumInstance { name: n2, variant: v2, value: val2 }) =>
                (n1, v1, val1).partial_cmp(&(n2, v2, val2)),

            (Alias { name: n1, base_type: b1, conditions: c1, .. },
             Alias { name: n2, base_type: b2, conditions: c2, .. }) =>
                (n1, b1, c1).partial_cmp(&(n2, b2, c2)),

            _ => None
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Type::*;
        std::mem::discriminant(self).hash(state);
        match self {
            Simple { name, is_reference, is_maybe_type } => {
                name.hash(state);
                is_reference.hash(state);
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
            Enum { name, variants, generics, conditions, .. } => {
                name.hash(state);
                variants.hash(state);
                generics.hash(state);
                conditions.hash(state);
            }
            EnumInstance { name, variant, value } => {
                name.hash(state);
                variant.hash(state);
                value.hash(state);
            }
            Alias { name, base_type, conditions, .. } => {
                name.hash(state);
                base_type.hash(state);
                conditions.hash(state);
            }
        }
    }
}