use std::fmt;
use std::sync::Arc;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use crate::env::runtime::value::Value;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::types::Type;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::utils::format_value;
use std::collections::HashMap;
use crate::interpreter::Interpreter;
use parking_lot::Mutex;
use serde::ser::{Serialize, Serializer};
use serde::de::{Deserialize, Deserializer};
use rustc_hash::FxHashMap;
use crate::env::runtime::internal_structs::EffectFlags;
use bincode::{
    enc::{Encode, Encoder},
    de::{BorrowDecode, Decode, Decoder},
    error::{EncodeError, DecodeError},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ParameterKind {
    Positional,
    Variadic,
    KeywordVariadic,
    Instance,
}

#[derive(Clone, Debug, PartialEq, Hash, PartialOrd)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
    pub default: Option<Value>,
    pub kind: ParameterKind,
    pub mods: Vec<String>,
}


#[derive(Clone, Debug, PartialOrd, PartialEq, Hash)]
pub struct FunctionMetadata {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub is_public: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub is_native: bool,
    pub state: Option<String>,
    pub effects: EffectFlags,
}

impl std::default::Default for FunctionMetadata {
    fn default() -> Self {
        Self {
            name: String::new(),
            parameters: Vec::new(),
            return_type: Type::new_simple("any"),
            is_public: false,
            is_static: false,
            is_final: false,
            is_native: false,
            state: None,
            effects: EffectFlags::empty(),
        }
    }
}

pub trait Callable: Send + Sync {
    fn call(&self, args: &HashMap<String, Value>) -> Value;
    fn call_shared(&self, _args: &HashMap<String, Value>, _interpreter: &mut Interpreter) -> Value {
        Value::Error("RuntimeError", "This should not be called directly", None)
    }
    #[allow(dead_code)]
    fn call_method(&self, _value: &mut Value, _args: &HashMap<String, Value>) -> Value {
        Value::Error("RuntimeError", "This should not be called directly", None)
    }
    fn metadata(&self) -> &FunctionMetadata;
}

pub trait NativeCallable: Send + Sync {
    fn call(&self, _args: &HashMap<String, Value>) -> Value;
}

pub trait SharedNativeCallable: Send + Sync {
    fn call(&self, _args: &HashMap<String, Value>, _interpreter: &mut Interpreter) -> Value;
}

impl<F> NativeCallable for F
where
    F: Fn(&HashMap<String, Value>) -> Value + Send + Sync + 'static,
{
    fn call(&self, _args: &HashMap<String, Value>) -> Value {
        (self)(_args)
    }
}

impl<F> SharedNativeCallable for F
where
    F: Fn(&HashMap<String, Value>, &mut Interpreter) -> Value + Send + Sync + 'static,
{
    fn call(&self, _args: &HashMap<String, Value>, _interpreter: &mut Interpreter) -> Value {
        (self)(_args, _interpreter)
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub func: Arc<dyn NativeCallable>,
    pub meta: FunctionMetadata,
}

#[derive(Clone)]
pub struct SharedNativeFunction {
    pub func: Arc<dyn SharedNativeCallable>,
    pub meta: FunctionMetadata,
}

impl SharedNativeFunction {
    pub fn new<F>(
        name: &str,
        func: F,
        parameters: Vec<Parameter>,
        return_type: &str,
        is_public: bool,
        is_static: bool,
        is_final: bool,
        state: Option<String>,
        effects: EffectFlags,
    ) -> Self
    where
        F: SharedNativeCallable + 'static,
    {
        Self {
            func: Arc::new(func),
            meta: FunctionMetadata {
                name: name.to_string(),
                parameters,
                return_type: Type::new_simple(return_type),
                is_public,
                is_static,
                is_final,
                is_native: true,
                state,
                effects,
            },
        }
    }

    pub fn new_pt<F>(
        name: &str,
        func: F,
        parameters: Vec<Parameter>,
        return_type: &Type,
        is_public: bool,
        is_static: bool,
        is_final: bool,
        state: Option<String>,
        effects: EffectFlags,
    ) -> Self
    where
        F: SharedNativeCallable + 'static,
    {
        Self {
            func: Arc::new(func),
            meta: FunctionMetadata {
                name: name.to_string(),
                parameters,
                return_type: return_type.clone(),
                is_public,
                is_static,
                is_final,
                is_native: true,
                state,
                effects,
            },
        }
    }
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl Callable for SharedNativeFunction {
    fn call(&self, _args: &HashMap<String, Value>) -> Value {
        Value::Error("RuntimeError", "This should not be called directly", None)
    }

    fn call_shared(&self, args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
        self.func.call(args, interpreter)
    }

    fn metadata(&self) -> &FunctionMetadata {
        &self.meta
    }
}

impl NativeFunction {
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.meta.parameters.len() * std::mem::size_of::<Parameter>()
    }
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
        effects: EffectFlags,
    ) -> Self
    where
        F: NativeCallable + 'static,
    {
        Self {
            func: Arc::new(func),
            meta: FunctionMetadata {
                name: name.to_string(),
                parameters,
                return_type: Type::new_simple(return_type),
                is_public,
                is_static,
                is_final,
                is_native: true,
                state,
                effects,
            },
        }
    }

    pub fn new_pt<F>(
        name: &str,
        func: F,
        parameters: Vec<Parameter>,
        return_type: &Type,
        is_public: bool,
        is_static: bool,
        is_final: bool,
        state: Option<String>,
        effects: EffectFlags,
    ) -> Self
    where
        F: NativeCallable + 'static,
    {
        Self {
            func: Arc::new(func),
            meta: FunctionMetadata {
                name: name.to_string(),
                parameters,
                return_type: return_type.clone(),
                is_public,
                is_static,
                is_final,
                is_native: true,
                state,
                effects,
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

#[derive(Clone, PartialEq)]
pub struct UserFunction {
    pub meta: FunctionMetadata,
    pub body: Vec<Statement>,
}

impl UserFunction {
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.body.len() * std::mem::size_of::<Statement>()
    }
}

impl Callable for UserFunction {
    fn call(&self, _args: &std::collections::HashMap<String, Value>) -> Value {
        Value::Error("RuntimeError", "This should not be called directly", None)
    }

    fn metadata(&self) -> &FunctionMetadata {
        &self.meta
    }
}

pub trait NativeMethodCallable: Send + Sync {
    fn call(&self, _value: &mut Value, _args: &HashMap<String, Value>) -> Value;
}

impl<F> NativeMethodCallable for F
where
    F: Fn(&mut Value, &HashMap<String, Value>) -> Value + Send + Sync + 'static,
{
    fn call(&self, _value: &mut Value, _args: &HashMap<String, Value>) -> Value {
        (self)(_value, _args)
    }
}

#[derive(Clone)]
pub struct NativeMethod {
    pub func: Arc<dyn NativeMethodCallable>,
    pub meta: FunctionMetadata,
}

impl NativeMethod {
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl Callable for NativeMethod {
    fn call(&self, _args: &std::collections::HashMap<String, Value>) -> Value {
        Value::Error("RuntimeError", "This should not be called directly", None)
    }

    fn call_method(&self, value: &mut Value, args: &HashMap<String, Value>) -> Value {
        self.func.call(value, args)
    }

    fn metadata(&self) -> &FunctionMetadata {
        &self.meta
    }
}

// interpreter shared state to support private calls in methods
#[derive(Clone)]
pub struct UserFunctionMethod {
    pub meta: FunctionMetadata,
    pub body: Vec<Statement>,
    pub interpreter: Arc<Mutex<Interpreter>>,
}

impl UserFunctionMethod {
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.body.len() * std::mem::size_of::<Statement>()
    }
}

impl Hash for UserFunctionMethod {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.meta.hash(state);
    }
}

impl PartialEq for UserFunctionMethod {
    fn eq(&self, other: &Self) -> bool {
        self.meta == other.meta
    }
}

impl PartialOrd for UserFunctionMethod {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.meta.partial_cmp(&other.meta)
    }
}


impl UserFunctionMethod {
    pub fn new_from_func_with_interpreter(func: Arc<UserFunction>, interpreter: Arc<Mutex<Interpreter>>) -> Self {
        Self {
            meta: func.metadata().clone(),
            body: func.body.clone(),
            interpreter,
        }
    }

    pub fn get_function(&self) -> Arc<UserFunction> {
        Arc::new(UserFunction {
            meta: self.meta.clone(),
            body: self.body.clone(),
        })
    }

    pub fn get_name(&self) -> &str {
        &self.meta.name
    }

    pub fn get_interpreter(&self) -> Arc<Mutex<Interpreter>> {
        Arc::clone(&self.interpreter)
    }
}

impl Callable for UserFunctionMethod {
    fn call(&self, _args: &std::collections::HashMap<String, Value>) -> Value {
        Value::Error("RuntimeError", "This should not be called directly", None)
    }

    fn metadata(&self) -> &FunctionMetadata {
        &self.meta
    }
}

#[derive(Clone)]
pub enum Function {
    Native(Arc<NativeFunction>),
    Custom(Arc<UserFunction>),
    Lambda(Arc<UserFunction>, FxHashMap<String, Variable>),
    SharedNative(Arc<SharedNativeFunction>),
    NativeMethod(Arc<NativeMethod>),
    CustomMethod(Arc<UserFunctionMethod>),
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str("Function(opaque)")
    }
}

impl<'de> Deserialize<'de> for Function {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let _: String = Deserialize::deserialize(deserializer)?;
        let func = Arc::new(NativeFunction {
            func: Arc::new(PlaceholderNativeCallable) as Arc<dyn NativeCallable>,
            meta: FunctionMetadata::default(),
        });
        Ok(Function::Native(func))
    }
}

impl Encode for Function {
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        0xFFu8.encode(encoder)
    }
}


impl<C> Decode<C> for Function {
    fn decode<D: Decoder>(decoder: &mut D) -> Result<Self, DecodeError> {
        let _: u8 = Decode::decode(decoder)?;
        let func = Arc::new(NativeFunction {
            func: Arc::new(PlaceholderNativeCallable) as Arc<dyn NativeCallable>,
            meta: FunctionMetadata::default(),
        });
        Ok(Function::Native(func))
    }
}

impl<'de, C> BorrowDecode<'de, C> for Function {
    fn borrow_decode<D: Decoder>(decoder: &mut D) -> Result<Self, DecodeError> {
        Decode::decode(decoder)
    }
}

struct PlaceholderNativeCallable;
impl NativeCallable for PlaceholderNativeCallable {
    fn call(&self, _args: &HashMap<String, Value>) -> Value {
        Value::Error("RuntimeError", "This is a placeholder function", None)
    }
}

impl Function {
    pub fn call(&self, args: &HashMap<String, Value>) -> Value {
        match self {
            Function::Native(f) => f.call(args),
            Function::Custom(f) => f.call(args),
            Function::Lambda(f, _) => f.call(args),
            Function::NativeMethod(f) => f.call(args),
            Function::CustomMethod(f) => f.call(args),
            Function::SharedNative(f) => f.call(args),
        }
    }

    pub fn call_shared(&self, args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
        match self {
            Function::Native(f) => f.call_shared(args, interpreter),
            Function::Custom(f) => f.call_shared(args, interpreter),
            Function::Lambda(f, _) => f.call_shared(args, interpreter),
            Function::NativeMethod(f) => f.call_shared(args, interpreter),
            Function::CustomMethod(f) => f.call_shared(args, interpreter),
            Function::SharedNative(f) => f.call_shared(args, interpreter),
        }
    }

    pub fn get_body(&self) -> Vec<Statement> {
        match self {
            Function::Native(_) => vec![],
            Function::Custom(f) => f.body.clone(),
            Function::Lambda(f, _) => f.body.clone(),
            Function::SharedNative(_) => vec![],
            Function::NativeMethod(_) => vec![],
            Function::CustomMethod(f) => f.body.clone(),
        }
    }

    pub fn get_type(&self) -> Type {
        Type::Function {
            parameter_types: self.get_parameter_types(),
            return_type: Box::new(self.get_return_type()),
        }
    }

    pub fn ptr(&self) -> *const () {
        match self {
            Function::Native(arc) => Arc::as_ptr(arc) as *const (),
            Function::Custom(arc) => Arc::as_ptr(arc) as *const (),
            Function::Lambda(arc, _) => Arc::as_ptr(arc) as *const (),
            Function::NativeMethod(arc) => Arc::as_ptr(arc) as *const (),
            Function::CustomMethod(arc) => Arc::as_ptr(arc) as *const (),
            Function::SharedNative(arc) => Arc::as_ptr(arc) as *const (),
        }
    }
    
    pub fn metadata(&self) -> &FunctionMetadata {
        match self {
            Function::Native(f) => f.metadata(),
            Function::Custom(f) => f.metadata(),
            Function::Lambda(f, _) => f.metadata(),
            Function::NativeMethod(f) => f.metadata(),
            Function::CustomMethod(f) => f.metadata(),
            Function::SharedNative(f) => f.metadata(),
        }
    }

    pub fn metadata_mut(&mut self) -> &mut FunctionMetadata {
        match self {
            Function::Native(f) => &mut Arc::make_mut(f).meta,
            Function::Custom(f) => &mut Arc::make_mut(f).meta,
            Function::Lambda(f, _) => &mut Arc::make_mut(f).meta,
            Function::NativeMethod(f) => &mut Arc::make_mut(f).meta,
            Function::CustomMethod(f) => &mut Arc::make_mut(f).meta,
            Function::SharedNative(f) => &mut Arc::make_mut(f).meta,
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Function::Native(f) => f.get_size(),
            Function::Custom(f) => f.get_size(),
            Function::Lambda(f, _) => f.get_size(),
            Function::NativeMethod(f) => f.get_size(),
            Function::CustomMethod(f) => f.get_size(),
            Function::SharedNative(f) => f.get_size(),
        }
    }

    pub fn get_name(&self) -> &str {
        &self.metadata().name
    }

    pub fn get_return_type(&self) -> Type {
        self.metadata().return_type.clone()
    }

    pub fn get_parameter_types(&self) -> Vec<Type> {
        self.metadata().parameters.iter().map(|p| p.ty.clone()).collect()
    }

    pub fn get_parameters(&self) -> &[Parameter] {
        &self.metadata().parameters
    }

    pub fn set_parameters(&mut self, parameters: Vec<Parameter>) {
        self.metadata_mut().parameters = parameters;
    }

    pub fn is_native(&self) -> bool {
        match self {
            Function::Native(_) => true,
            Function::Custom(_) => false,
            Function::Lambda(_, _) => false,
            Function::NativeMethod(_) => true,
            Function::CustomMethod(_) => false,
            Function::SharedNative(_) => true,
        }
    }

    pub fn is_natively_callable(&self) -> bool {
        match self {
            Function::Native(_) => true,
            Function::Custom(_) => false,
            Function::Lambda(_, _) => false,
            Function::NativeMethod(_) => true,
            Function::CustomMethod(_) => false,
            Function::SharedNative(_) => false,
        }
    }

    pub fn is_static(&self) -> bool {
        self.metadata().is_static
    }

    pub fn is_final(&self) -> bool {
        self.metadata().is_final
    }

    pub fn help_string(&self) -> String {
        format!("Function: {}\nParameters: {}\nReturn Type: {}\nPublic: {}\nStatic: {}\nFinal: {}\nNative: {}\nState: {}\nEffects: {:?}",
            self.get_name(),
            if self.get_parameters().is_empty() { "None".to_string() } else { self.get_parameters().iter().map(|p| if let Some(default) = &p.default { format!("{}: {} = {}", p.name, p.ty.display_simple(), format_value(default)) } else { format!("{}: {}", p.name, p.ty.display_simple()) }).collect::<Vec<_>>().join(", ") },
            self.get_return_type().display_simple(),
            self.metadata().is_public,
            self.metadata().is_static,
            self.metadata().is_final,
            self.metadata().is_native,
            self.metadata().state.as_deref().unwrap_or("None"),
            self.metadata().effects.get_names().iter().map(|s| s.to_lowercase()).collect::<Vec<_>>().join(", ")
        )
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

#[allow(dead_code)]
impl Parameter {
    pub fn set_mods(mut self, mods: Vec<String>) -> Self {
        self.mods = mods;
        self
    }

    pub fn positional(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: None,
            kind: ParameterKind::Positional,
            mods: vec![],
        }
    }
    
    // what is pt? parameter type? maybe idk
    pub fn positional_pt(name: &str, ty: &Type) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.clone(),
            default: None,
            kind: ParameterKind::Positional,
            mods: vec![],
        }
    }

    pub fn positional_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: Some(default),
            kind: ParameterKind::Positional,
            mods: vec![],
        }
    }
    
    pub fn positional_optional_pt(name: &str, ty: &Type, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.clone(),
            default: Some(default),
            kind: ParameterKind::Positional,
            mods: vec![],
        }
    }

    pub fn variadic(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: None,
            kind: ParameterKind::Variadic,
            mods: vec![],
        }
    }

    pub fn variadic_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: Some(default),
            kind: ParameterKind::Variadic,
            mods: vec![],
        }
    }

    pub fn keyword_variadic(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: None,
            kind: ParameterKind::KeywordVariadic,
            mods: vec![],
        }
    }

    pub fn keyword_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: Some(default),
            kind: ParameterKind::KeywordVariadic,
            mods: vec![],
        }
    }

    pub fn keyword_variadic_optional(name: &str, ty: &str, default: Value) -> Self {
        Self {
            name: name.to_string(),
            ty: Type::new_simple(ty),
            default: Some(default),
            kind: ParameterKind::KeywordVariadic,
            mods: vec![],
        }
    }

    pub fn is_positional(&self) -> bool {
        self.kind == ParameterKind::Positional
    }

    pub fn is_positional_optional(&self) -> bool {
        self.default.is_some()
    }

    pub fn instance(name: &str, ty: &Type, mods: Vec<String>) -> Self {
        Self {
            name: name.to_string(),
            ty: ty.clone(),
            default: None,
            kind: ParameterKind::Instance,
            mods,
        }
    }
}
