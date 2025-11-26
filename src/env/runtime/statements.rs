use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Int};
use crate::env::runtime::internal_structs::{PathElement, EffectFlags};
use std::collections::HashMap;
use crate::env::runtime::tokens::Location;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use once_cell::sync::Lazy;
use std::sync::{Arc, Mutex};

pub static LOC_TABLE: Lazy<Arc<Mutex<Vec<Location>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

pub fn alloc_loc(loc: Location) -> AstNodeId {
    let mut table = LOC_TABLE.lock().unwrap();
    table.push(loc);
    table.len() - 1
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub enum ThrowNode {
    Tuple(Statement),
    Message {
        message: Statement,
        from: Option<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub enum MatchCase {
    Pattern {
        pattern: PathElement,
        body: Vec<Statement>,
        guard: Option<Statement>,
    },
    Literal {
        patterns: Vec<Statement>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub enum RangeModeType {
    Value,
    Length,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub enum IterableNode {
    List {
        elements: Vec<Statement>,
    },
    Tuple {
        elements: Vec<Statement>,
    },
    ListCompletion {
        seed: Vec<Statement>,
        end: Box<Statement>,
        pattern_flag: bool,
        range_mode: RangeModeType,
        is_infinite: bool,
    },
    ListComprehension {
        for_clauses: Vec<Value>,
        map_expression: Box<Statement>,
    },
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub enum Node {
    If {
        condition: Box<Statement>,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    For {
        iterable: Box<Statement>,
        body: Vec<Statement>, 
        variable: PathElement,
    },
    While {
        condition: Box<Statement>,
        body: Vec<Statement>,
    },
    TryCatch {
        body: Vec<Statement>,
        catch_body: Option<Vec<Statement>>,
        exception_vars: Option<Vec<String>>,
    },
    Throw {
        node: Box<ThrowNode>,
    },
    Forget {
        node: Box<Statement>,
    },
    Continue, Break,
    Defer {
        body: Vec<Statement>,
    },
    Scope {
        body: Vec<Statement>,
        name: Option<String>,
        locals: Vec<String>,
        is_local: bool,
    },
    Match {
        condition: Box<Statement>,
        cases: Vec<MatchCase>,
    },
    Group {
        body: Vec<Statement>,
    },

    FunctionDeclaration {
        name: String,
        pos_args: Vec<Value>, 
        named_args: Vec<(String, Value)>, 
        body: Vec<Statement>, 
        modifiers: Vec<String>, 
        return_type: Box<Statement>,
        effect_flags: EffectFlags,
    },
    GeneratorDeclaration {
        name: String,
        pos_args: Vec<Value>, 
        named_args: Vec<(String, Value)>, 
        body: Vec<Statement>, 
        modifiers: Vec<String>, 
        return_type: Box<Statement>,
        effect_flags: EffectFlags,
    },
    Return {
        value: Box<Statement>,
    },

    Import {
        name: String, 
        alias: Option<String>, 
        named_imports: Vec<Value>, 
        modifiers: Vec<String>,
        import_all: bool, 
        module_path_opt: Option<Box<Statement>>,
    },
    Export {
        names: Vec<String>, 
        aliases: Vec<String>, 
        modifiers_list: Vec<Vec<String>>, 
    },

    VariableDeclaration {
        name: String,
        val_stmt: Box<Statement>, 
        var_type: Box<Statement>, 
        modifiers: Vec<String>,
        is_default: bool,
    },
    Variable {
        name: String,
    },
    Assignment {
        left: Box<Statement>,
        right: Box<Statement>,
    },
    UnpackAssignment {
        targets: Vec<Statement>,
        stmt: Box<Statement>,
    },

    Number {
        value: String,
    },
    String {
        value: String,
        mods: Vec<String>,
    },
    Boolean {
        value: Option<bool>, // Some(true), Some(false), None for null
    },

    Map {
        keys_stmts: Vec<Statement>,
        values_stmts: Vec<Statement>,
    },
    Iterable {
        node: IterableNode,
    },
    Value {
        value: Value,
    },

    Operation {
        left: Box<Statement>,
        operator: String,
        right: Box<Statement>,
    },

    Null,
}

pub type AstNodeId = usize;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub struct Statement {
    pub node: Node,
    pub loc: Option<AstNodeId>,  // Index in AST_LOCATIONS_TABLE
}

impl Node {
    pub fn name(&self) -> String {
        match &self {
            Node::If { .. } => "if".to_string(),
            Node::For { .. } => "for".to_string(),
            Node::While { .. } => "while".to_string(),
            Node::TryCatch { .. } => "try-catch".to_string(),
            Node::Throw { .. } => "throw".to_string(),
            Node::Forget { .. } => "forget".to_string(),
            Node::Continue => "continue".to_string(),
            Node::Break => "break".to_string(),
            Node::Defer { .. } => "defer".to_string(),
            Node::Scope { .. } => "scope".to_string(),
            Node::Match { .. } => "match".to_string(),
            Node::Group { .. } => "group".to_string(),
            Node::FunctionDeclaration { .. } => "function declaration".to_string(),
            Node::GeneratorDeclaration { .. } => "generator declaration".to_string(),
            Node::Return { .. } => "return".to_string(),
            Node::Import { .. } => "import".to_string(),
            Node::Export { .. } => "export".to_string(),
            Node::VariableDeclaration { .. } => "variable declaration".to_string(),
            Node::Variable { .. } => "variable".to_string(),
            Node::Assignment { .. } => "assignment".to_string(),
            Node::UnpackAssignment { .. } => "unpack assignment".to_string(),
            Node::Number { .. } => "number".to_string(),
            Node::String { .. } => "string".to_string(),
            Node::Boolean { .. } => "boolean".to_string(),
            Node::Map { .. } => "map".to_string(),
            Node::Iterable { .. } => "iterable".to_string(),
            Node::Value { .. } => "value".to_string(),
            Node::Null => "none".to_string(),
        }
    }
}

impl Statement {
    #[allow(non_upper_case_globals)]
    pub const Null: Statement = Statement {
        node: Node::Null,
        loc_id: None,
    };

    // pub fn convert_to_map(&self) -> Value {
    //     match self {
    //         Statement::Statement { keys, values, loc } => {
    //             Value::Map {
    //                 keys: {
    //                     let mut new_keys = keys.clone();
    //                     new_keys.push(Value::String("_loc".to_string()));
    //                     new_keys
    //                 },
    //                 values: {
    //                     let mut new_values = values.clone();
    //                     new_values.push(
    //                         Value::Map {
    //                             keys: vec![
    //                                 Value::String("_file".to_string()),
    //                                 Value::String("_line_string".to_string()),
    //                                 Value::String("_line_number".to_string()),
    //                                 Value::String("_range".to_string())
    //                             ],
    //                             values: vec![
    //                                 Value::String(loc.as_ref().map_or("".to_string(), |l| l.file.clone())),
    //                                 Value::String(loc.as_ref().map_or("".to_string(), |l| l.line_string.clone())),
    //                                 Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.line_number as i64))),
    //                                 Value::Tuple (
    //                                     vec![
    //                                         Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.0 as i64))),
    //                                         Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.1 as i64))),
    //                                     ]
    //                                 )
    //                             ],
    //                         }
    //                     );
    //                     new_values
    //                 },
    //             }
    //         },
    //         Statement::Null => {
    //             Value::Map {
    //                 keys: vec![
    //                     Value::String("_loc".to_string()),
    //                 ],
    //                 values: vec![
    //                     Value::Map {
    //                         keys: vec![
    //                             Value::String("_file".to_string()),
    //                             Value::String("_line_string".to_string()),
    //                             Value::String("_line_number".to_string()),
    //                             Value::String("_range".to_string())
    //                         ],
    //                         values: vec![
    //                             Value::String("".to_string()),
    //                             Value::String("".to_string()),
    //                             Value::Int(0.into()),
    //                             Value::Tuple(
    //                                 vec![Value::Int(0.into()), Value::Int(0.into())],
    //                             )
    //                         ],
    //                     }
    //                 ],
    //             }
    //         },
    //     }
    // }
    // pub fn make_value(val: Value) -> Statement {
    //     Statement::Statement {
    //         keys: vec!["type".into(), "value".into()],
    //         values: vec!["VALUE".into(), val],
    //         loc: None,
    //     }
    // }
    // pub fn convert_to_hashmap(&self) -> HashMap<Value, Value> {
    //     match self {
    //         Statement::Statement { keys, values, loc } => {
    //             let mut map = HashMap::default();
    //             for (key, value) in keys.iter().zip(values.iter()) {
    //                 map.insert(key.clone(), value.clone());
    //             }
    //             map.insert(Value::String("_loc".to_string()), Value::Map {
    //                 keys: vec![
    //                     Value::String("_file".to_string()),
    //                     Value::String("_line_string".to_string()),
    //                     Value::String("_line_number".to_string()),
    //                     Value::String("_range".to_string())
    //                 ],
    //                 values: vec![
    //                     Value::String(loc.as_ref().map_or("".to_string(), |l| l.file.clone())),
    //                     Value::String(loc.as_ref().map_or("".to_string(), |l| l.line_string.clone())),
    //                     Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.line_number as i64))),
    //                     Value::Tuple(
    //                         vec![
    //                             Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.0 as i64))),
    //                             Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.1 as i64))),
    //                         ]
    //                     )
    //                 ],
    //             });
    //             map
    //         },
    //         Statement::Null => {
    //             let mut map = HashMap::default();
    //             map.insert(Value::String("_loc".to_string()), Value::Map {
    //                 keys: vec![
    //                     Value::String("_file".to_string()),
    //                     Value::String("_line_string".to_string()),
    //                     Value::String("_line_number".to_string()),
    //                     Value::String("_range".to_string())
    //                 ],
    //                 values: vec![
    //                     Value::String("".to_string()),
    //                     Value::String("".to_string()),
    //                     Value::Int(0.into()),
    //                     Value::Tuple(vec![Value::Int(0.into()), Value::Int(0.into())]),
    //                 ],
    //             });
    //             map
    //         },
    //     }
    // }
    // pub fn convert_from_hashmap(map: &HashMap<Value, Value>) -> Statement {
    //     if map.is_empty() {
    //         return Statement::Null;
    //     }

    //     let mut keys = Vec::new();
    //     let mut values = Vec::new();
    //     let mut loc: Option<Location> = None;

    //     for (k, v) in map {
    //         if let Value::String(s) = k {
    //             if s == "_loc" {
    //                 if let Value::Map { keys: loc_keys, values: loc_values } = v {
    //                     let mut file = "".to_string();
    //                     let mut line_string = "".to_string();
    //                     let mut line_number = 0;
    //                     let mut range = (0, 0);
    //                     let mut lucia_source_loc = "".to_string();

    //                     for (lk, lv) in loc_keys.iter().zip(loc_values.iter()) {
    //                         match (lk, lv) {
    //                             (Value::String(s), Value::String(val)) if s == "_file" => {
    //                                 file = val.clone();
    //                             }
    //                             (Value::String(s), Value::String(val)) if s == "_line_string" => {
    //                                 line_string = val.clone();
    //                             }
    //                             (Value::String(s), Value::Int(n)) if s == "_line_number" => {
    //                                 line_number = n.to_string().parse().unwrap_or(0);
    //                             }
    //                             (Value::String(s), Value::Tuple(vals)) if s == "_range" && vals.len() == 2 => {
    //                                 if let (Value::Int(start), Value::Int(end)) = (&vals[0], &vals[1]) {
    //                                     range = (
    //                                         start.to_string().parse().unwrap_or(0),
    //                                         end.to_string().parse().unwrap_or(0),
    //                                     );
    //                                 }
    //                             }
    //                             (Value::String(s), Value::String(val)) if s == "_lucia_source_loc" => {
    //                                 lucia_source_loc = val.clone();
    //                             }
    //                             _ => {}
    //                         }
    //                     }

    //                     loc = Some(Location {
    //                         file,
    //                         line_string,
    //                         line_number,
    //                         range,
    //                         lucia_source_loc
    //                     });
    //                 }
    //             } else {
    //                 keys.push(k.clone());
    //                 values.push(v.clone());
    //             }
    //         } else {
    //             keys.push(k.clone());
    //             values.push(v.clone());
    //         }
    //     }

    //     if keys.is_empty() && values.is_empty() {
    //         Statement::Null
    //     } else {
    //         Statement::Statement { keys, values, loc }
    //     }
    // }
    // pub fn get_value(&self, key: &str) -> Option<Value> {
    //     match self {
    //         Statement::Statement { keys, values, loc } => {
    //             for (k, v) in keys.iter().zip(values.iter()) {
    //                 if let Value::String(s) = k {
    //                     if s == key {
    //                         return Some(v.clone());
    //                     }
    //                 }
    //             }
    //             match key {
    //                 "_loc" => Some(Value::Map {
    //                     keys: vec![
    //                         Value::String("_file".to_string()),
    //                         Value::String("_line_string".to_string()),
    //                         Value::String("_line_number".to_string()),
    //                         Value::String("_range".to_string())
    //                     ],
    //                     values: vec![
    //                         Value::String(loc.as_ref().map_or("".to_string(), |l| l.file.clone())),
    //                         Value::String(loc.as_ref().map_or("".to_string(), |l| l.line_string.clone())),
    //                         Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.line_number as i64))),
    //                         Value::Tuple(vec![
    //                             Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.0 as i64))),
    //                             Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.1 as i64))),
    //                         ])
    //                     ]
    //                 }),
    //                 _ => None,
    //             }
    //         }
    //         Statement::Null => None,
    //     }
    // }
    pub fn get_type(&self) -> String {
        match &self.node {
            Node::If { .. } => "IF".to_string(),
            Node::For { .. } => "FOR".to_string(),
            Node::While { .. } => "WHILE".to_string(),
            Node::TryCatch { .. } => "TRY_CATCH".to_string(),
            Node::Throw { .. } => "THROW".to_string(),
            Node::Forget { .. } => "FORGET".to_string(),
            Node::Continue => "CONTINUE".to_string(),
            Node::Break => "BREAK".to_string(),
            Node::Defer { .. } => "DEFER".to_string(),
            Node::Scope { .. } => "SCOPE".to_string(),
            Node::Match { .. } => "MATCH".to_string(),
            Node::Group { .. } => "GROUP".to_string(),
            Node::FunctionDeclaration { .. } => "FUNCTION_DECLARATION".to_string(),
            Node::GeneratorDeclaration { .. } => "GENERATOR_DECLARATION".to_string(),
            Node::Return { .. } => "RETURN".to_string(),
            Node::Import { .. } => "IMPORT".to_string(),
            Node::Export { .. } => "EXPORT".to_string(),
            Node::VariableDeclaration { .. } => "VARIABLE_DECLARATION".to_string(),
            Node::Variable { .. } => "VARIABLE".to_string(),
            Node::Assignment { .. } => "ASSIGNMENT".to_string(),
            Node::UnpackAssignment { .. } => "UNPACK_ASSIGNMENT".to_string(),
            Node::Number { .. } => "NUMBER".to_string(),
            Node::String { .. } => "STRING".to_string(),
            Node::Boolean { .. } => "BOOLEAN".to_string(),
            Node::Map { .. } => "MAP".to_string(),
            Node::Iterable { .. } => "ITERABLE".to_string(),
            Node::Value { .. } => "VALUE".to_string(),
            Node::Null => "NULL".to_string(),
        }
    }

    pub fn format_for_debug(&self) -> String {
        let mut buffer = format!("{{\"type\": \"{}\"", self.get_type());
        buffer.push_sr(match &self.node {
            Node::If { condition, else_body, body } => format!("\"condition\": {}, \"body\": [{}], \"else_body\": [{}]",
                buffer,
                condition.format_for_debug(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                else_body.as_ref().map_or("".to_string(), |eb| eb.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "))
            ),
        });
        buffer.push('}');
        buffer
    }

    // pub fn get_name(&self) -> String {
    //     let map = self.convert_to_hashmap();
    //     match map.get(&Value::String("name".to_string())) {
    //         Some(Value::String(s)) => s.clone(),
    //         _ => "".to_string(),
    //     }
    // }
    // pub fn is_empty(&self) -> bool {
    //     match self {
    //         Statement::Statement { keys, values, .. } => keys.is_empty() && values.is_empty(),
    //         Statement::Null => true,
    //     }
    // }
    // pub fn is_equal_to_statement(&self, other: &Statement) -> bool {
    //     match (self, other) {
    //         (Statement::Statement { keys: keys_a, values: values_a, .. },
    //          Statement::Statement { keys: keys_b, values: values_b, .. }) => {
    //             if keys_a.len() != keys_b.len() || values_a.len() != values_b.len() {
    //                 return false;
    //             }
    //             for (k_a, k_b) in keys_a.iter().zip(keys_b.iter()) {
    //                 if k_a != k_b {
    //                     return false;
    //                 }
    //             }
    //             for (v_a, v_b) in values_a.iter().zip(values_b.iter()) {
    //                 if v_a != v_b {
    //                     return false;
    //                 }
    //             }
    //             true
    //         },
    //         (Statement::Null, Statement::Null) => true,
    //         _ => false,
    //     }
    // }
    // pub fn from_hashmap_values(values: &HashMap<Value, Value>) -> Statement {
    //     Statement::convert_from_hashmap(values)
    // }
}
