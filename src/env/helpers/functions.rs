use std::fmt;
use std::sync::Arc;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use crate::env::helpers::utils::Statement; // Adjust imports based on actual module structure
use std::collections::HashMap;


use crate::env::helpers::utils::{Value, Error}; // Adjust imports based on actual module structure

// -------------------------------
// Parameter and Metadata
// -------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ParameterKind {
    Positional,
    Variadic,
    KeywordVariadic
}

#[derive(Clone, Debug, PartialEq, Hash, PartialOrd)]
pub struct Parameter {
    pub name: String,
    pub ty: String,
    pub default: Option<Value>,
    pub kind: ParameterKind,
}


#[derive(Clone, Debug, PartialOrd, PartialEq, Hash)]
pub struct FunctionMetadata {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: String,
    pub is_public: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub is_native: bool,
    pub state: Option<String>,
}

// -------------------------------
// Callable Trait
// -------------------------------

pub trait Callable: Send + Sync {
    fn call(&self, args: &HashMap<String, Value>) -> Value;
    fn metadata(&self) -> &FunctionMetadata;
}

pub trait NativeCallable: Send + Sync {
    fn call(&self, _args: &HashMap<String, Value>) -> Value;
}

impl<F> NativeCallable for F
where
    F: Fn(&HashMap<String, Value>) -> Value + Send + Sync + 'static,
{
    fn call(&self, _args: &HashMap<String, Value>) -> Value {
        (self)(_args)
    }
}

// -------------------------------
// Native Function
// -------------------------------

#[derive(Clone)]
pub struct NativeFunction {
    func: Arc<dyn NativeCallable>,
    meta: FunctionMetadata,
}

impl NativeFunction {
    pub fn new<F>(
        name: &str,
        func: F,
        parameters: Vec<Parameter>,
        return_type: &str,
        is_public: bool,
        is_static: bool,
        is_final: bool,
        state: Option<String>,
    ) -> Self
    where
        F: NativeCallable + 'static,
    {
        Self {
            func: Arc::new(func),
            meta: FunctionMetadata {
                name: name.to_string(),
                parameters,
                return_type: return_type.to_string(),
                is_public,
                is_static,
                is_final,
                is_native: true,
                state,
            },
        }
    }
}


impl Callable for NativeFunction {
    fn call(&self, args: &HashMap<String, Value>) -> Value {
        self.func.call(args)
    }

    fn metadata(&self) -> &FunctionMetadata {
        &self.meta
    }
}


// -------------------------------
// User-defined Function
// -------------------------------

#[derive(Clone, PartialEq, Hash, PartialOrd)]
pub struct UserFunction {
    pub meta: FunctionMetadata,
    pub body: Vec<Statement>,
}

impl Callable for UserFunction {
    fn call(&self, _args: &std::collections::HashMap<String, Value>) -> Value {
        // TODO: actual interpretation logic here
        println!(
            "Calling user-defined function '{}'",
            self.meta.name
        );

        Value::Null
    }

    fn metadata(&self) -> &FunctionMetadata {
        &self.meta
    }
}


// -------------------------------
// Function Enum
// -------------------------------

#[derive(Clone)]
pub enum Function {
    Native(Arc<NativeFunction>),
    Custom( Arc<UserFunction>),
}

impl Function {
    pub fn call(&self, args: &HashMap<String, Value>) -> Value {
        match self {
            Function::Native(f) => f.call(args),
            Function::Custom(f) => f.call(args),
        }
    }

    pub fn metadata(&self) -> &FunctionMetadata {
        match self {
            Function::Native(f) => f.metadata(),
            Function::Custom(f) => f.metadata(),
        }
    }

    pub fn get_name(&self) -> &str {
        self.metadata().name.as_str()
    }

    pub fn get_return_type(&self) -> &str {
        self.metadata().return_type.as_str()
    }

    pub fn get_parameters(&self) -> &[Parameter] {
        &self.metadata().parameters
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let meta = self.metadata();
        f.debug_struct("Function")
            .field("name", &meta.name)
            .field("parameters", &meta.parameters)
            .field("return_type", &meta.return_type)
            .field("is_public", &meta.is_public)
            .field("is_static", &meta.is_static)
            .field("is_final", &meta.is_final)
            .field("is_native", &meta.is_native)
            .field("state", &meta.state)
            .finish()
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.metadata() == other.metadata()
    }
}

impl Eq for Function {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.metadata().hash(state);
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.metadata().partial_cmp(other.metadata())
    }
}

impl Parameter {
    pub fn positional(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.to_string(),
            default: None,
            kind: ParameterKind::Positional,
        }
    }
    pub fn positional_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.to_string(),
            default: Some(default),
            kind: ParameterKind::Positional,
        }
    }

    pub fn variadic(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.to_string(),
            default: None,
            kind: ParameterKind::Variadic,
        }
    }

    pub fn variadic_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.to_string(),
            default: Some(default),
            kind: ParameterKind::Variadic,
        }
    }

    pub fn keyword_variadic(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.to_string(),
            default: None,
            kind: ParameterKind::KeywordVariadic,
        }
    }

    pub fn keyword_variadic_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.to_string(),
            default: Some(default),
            kind: ParameterKind::KeywordVariadic,
        }
    }
}
