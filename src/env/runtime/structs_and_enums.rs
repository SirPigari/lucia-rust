use crate::env::runtime::types::Type;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::utils::{format_value, get_variant_name};
use crate::env::runtime::functions::Function;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Enum {
    pub ty: Type,
    pub variant: (usize, Box<Value>),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Struct {
    pub ty: Type,
    pub fields: HashMap<String, (Box<Value>, Type)>,
}

impl Enum {
    pub fn new(ty: Type, variant: (usize, Value)) -> Self {
        Self { ty, variant: (variant.0, Box::new(variant.1)) }
    }

    pub fn display(&self) -> String {
        format!("{}.{}{}", self.ty.display_simple(), get_variant_name(&self.ty, self.variant.0).unwrap_or(self.variant.0.to_string()), if matches!(*self.variant.1, Value::Null) { "".to_string() } else { format!("({})", format_value(&self.variant.1)) })
    }

    pub fn is_variant(&self, name: &str) -> bool {
        if let Type::Enum { variants, .. } = &self.ty {
            return variants
                .iter()
                .enumerate()
                .find(|(idx, (v_name, _, _))| v_name == name && *idx == self.variant.0)
                .is_some();
        }
        false
    }

    pub fn get_type(&self) -> Type {
        self.ty.clone()
    }

    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.variant.1.get_size()
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self { variant: (_, v), ty: _ } => !matches!(**v, Value::Null),
        }
    }

    pub fn ptr(&self) -> *const Self {
        self as *const Self
    }

    pub fn help_string(&self) -> String {
        let mut result = format!("Enum: {}\n", self.ty.display_simple());
        if let Type::Enum { variants, .. } = &self.ty {
            let (variant_name, _, _) = &variants.iter().find(|(_, _, idx)| *idx == self.variant.0).unwrap();
            result.push_str(&format!("Variant: {}\n", variant_name));
            result.push_str(&format!("Value: {}\n", format_value(&self.variant.1)));
        }
        result
    }
}

impl PartialOrd for Enum {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.variant.partial_cmp(&other.variant)
    }
}

impl PartialOrd for Struct {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mut self_vec: Vec<_> = self.fields.iter().collect();
        let mut other_vec: Vec<_> = other.fields.iter().collect();
        self_vec.sort_by(|a, b| a.0.cmp(b.0));
        other_vec.sort_by(|a, b| a.0.cmp(b.0));
        self_vec.partial_cmp(&other_vec)
    }
}

impl Struct {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            fields: HashMap::default(),
        }
    }

    pub fn new_with_fields(ty: Type, fields: HashMap<String, (Box<Value>, Type)>) -> Self {
        Self { ty, fields }
    }

    pub fn new_as_null(ty: Type) -> Self {
        let type_fields = if let Type::Struct { fields, .. } = &ty {
            fields
        } else {
            &vec![]
        };
        let mut fields = vec![];
        for field in type_fields {
            fields.push((field.0.clone(), (Box::new(Value::Null), Type::new_simple("any"))));
        }
        Self {
            ty,
            fields: HashMap::from_iter(fields),
        }
    }

    pub fn get_field_mods(&self, field: &str) -> Option<(bool, bool, bool)> {
        if let Type::Struct { fields: ty_fields, .. } = &self.ty {
            if let Some((_, _, mods)) = ty_fields.iter().find(|(name, _, _)| name == field) {
                let mut is_public = false;
                let mut is_static = false;
                let mut is_final = false;
                for m in mods {
                    match m.as_str() {
                        "public" => is_public = true,
                        "static" => is_static = true,
                        "final" => is_final = true,
                        "private" => is_public = false,
                        "non-static" => is_static = false,
                        "mutable" => is_final = false,
                        _ => {}
                    }
                }
                return Some((is_public, is_static, is_final));
            }
        }
        None
    }

    pub fn get_field(&self, field: &str) -> Option<&Value> {
        match self.fields.get(field) {
            Some((v, _)) => Some(&**v),
            None => None,
        }
    }

    pub fn set_field(&mut self, field: String, value: Value) -> Option<Box<Value>> {
        self.fields.insert(field, (Box::new(value.clone()), value.get_type())).map(|(v, _)| v)
    }

    pub fn get_fields(&self) -> &HashMap<String, (Box<Value>, Type)> {
        &self.fields
    }

    pub fn display(&self) -> String {
        if self.fields.is_empty() {
            format!("{} {{}}", self.ty.display_simple())
        } else {
            format!("{} {{ {} }}", self.ty.display_simple(), self.fields.iter()
                .map(|(k, (v, _))| format!("{} = {}", k, format_value(&v)))
                .collect::<Vec<_>>().join(", "))
        }
    }

    pub fn get_type(&self) -> Type {
        self.ty.clone()
    }

    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.fields.iter().map(|(k, (v, _))| k.len() + v.get_size()).sum::<usize>()
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self { fields, .. } if fields.is_empty() => false,
            _ => true,
        }
    }

    pub fn ptr(&self) -> *const Self {
        self as *const Self
    }

    pub fn name(&self) -> &str {
        if let Type::Struct { name, .. } = &self.ty {
            name
        } else {
            "<struct>"
        }
    }

    pub fn get_properties(&self) -> HashMap<String, Variable> {
        let mut map: HashMap<String, Variable> = HashMap::default();
        for field in self.fields.iter() {
            let (is_public, is_static, is_final) = self.get_field_mods(&field.0).unwrap_or((true, false, false));
            map.insert(field.0.clone(), Variable::new_pt(
                field.0.clone(),
                *(field.1.0).clone(),
                (field.1.1).clone(),
                is_static,
                is_public,
                is_final,
            ));
        }
        map
    }

    pub fn get(&self, field: &str) -> Option<&Value> {
        self.fields.get(field).map(|(v, _)| v.as_ref())
    }
    pub fn set(&mut self, field: String, value: Value) -> Option<Box<Value>> {
        self.fields.insert(field, (Box::new(value.clone()), value.get_type())).map(|(v, _)| v)
    }
    pub fn remove(&mut self, field: &str) -> Option<Box<Value>> {
        self.fields.remove(field).map(|(v, _)| v)
    }
    pub fn fields(&self) -> &HashMap<String, (Box<Value>, Type)> {
        &self.fields
    }
    pub fn get_methods(&self) -> HashMap<String, Function> {
        let mut methods = HashMap::new();
        if let Type::Struct { methods: ty_methods, .. } = &self.ty {
            for (name, method) in ty_methods {
                methods.insert(name.clone(), method.clone());
            }
        }
        methods
    }

    pub fn help_string(&self) -> String {
        let mut result = format!("Struct: {}\n", self.ty.display_simple());
        result.push_str("Fields:\n");
        for (field_name, (field_value, field_type)) in &self.fields {
            result.push_str(&format!("  {}: {} = {}\n", field_name, field_type.display_simple(), format_value(field_value)));
        }
        result.push_str("Methods:\n");
        for (name, method) in self.get_methods() {
            result.push_str(&format!("  {} - {}\n", name, method.help_string()));
        }
        result
    }
}
