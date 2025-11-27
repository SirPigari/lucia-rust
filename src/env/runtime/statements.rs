use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Int};
use crate::env::runtime::internal_structs::{PathElement, EffectFlags};
use std::collections::HashMap;
use crate::env::runtime::tokens::Location;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use once_cell::sync::Lazy;
use std::sync::{Arc, Mutex};
use std::fmt;

pub static LOC_TABLE: Lazy<Arc<Mutex<Vec<Location>>>> =
    Lazy::new(|| Arc::new(Mutex::new(Vec::new())));

pub fn alloc_loc(loc: Option<Location>) -> Option<AstNodeId> {
    match loc {
        None => return None,
        Some(l) => {
            let mut table = LOC_TABLE.lock().unwrap();
            table.push(loc);
            Some(table.len() - 1)
        }
    }
}

pub fn get_loc(id: usize) -> Location {
    let table = LOC_TABLE.lock().unwrap();
    table[id].clone()
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

impl fmt::Display for RangeModeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RangeModeType::Value => write!(f, "value"),
            RangeModeType::Length => write!(f, "lenght"),
        }
    }
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
pub enum AccessType {
    Range {
        start: Statement,
        end: Statement,
    },
    Single {
        index: Statement
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
pub enum TypeNode {
    Simple {
        base: String,
        ptr_level: usize,
        is_maybe: bool,  
    },
    Union {
        types: Vec<TypeNode>,
    },
    Function {
        parameters_types: Vec<TypeNode>,
        return_type: Box<TypeNode>,
    },
    Generator {
        parameters_types: Vec<TypeNode>,
        yield_type: Box<TypeNode>,
    },
    Generics {
        base_type: Box<TypeNode>,
        generics_types: Vec<TypeNode>,
    },
    Impl {
        impls: Vec<(String, Vec<TypeNode>, Vec<String>, Box<TypeNode>)>, // (trait_name, args, modifiers, return_type)
        joins: Vec<String>,
    },
    Variadics {
        base: Box<TypeNode>
    }
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
    UnaryOperation {
        operator: String,
        operand: Box<Statement>,
    },
    PrefixOperation {
        operator: String,
        operand: Box<Statement>,
    },
    Pipeline {
        initial_value: Box<Statement>,
        arguments: Vec<Statement>,
    },

    Call {
        name: String,
        pos_args: Vec<Statement>,
        named_args: HashMap<String, Statement>,
    },
    MethodCall {
        object: Box<Statement>,
        method_name: String,
        pos_args: Vec<Statement>,
        named_args: HashMap<String, Statement>,
    },
    PropertyAccess {
        object: Box<Statement>,
        property_name: String,
    },
    IndexAccess {
        object: Box<Statement>,
        access: Box<AccessType>,
    },

    Type {
        node: TypeNode,
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
            Node::Operation { .. } => "operation".to_string(),
            Node::UnaryOperation { .. } => "unary operation".to_string(),
            Node::PrefixOperation { .. } => "prefix operation".to_string(),
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
            Node::Operation { .. } => "OPERATION".to_string(),
            Node::UnaryOperation { .. } => "UNARY_OPERATION".to_string(),
            Node::PrefixOperation { .. } => "PREFIX_OPERATION".to_string(),
            Node::Null => "NULL".to_string(),
        }
    }

    pub fn format_for_debug(&self) -> String {
        let mut buffer = format!("{{\"type\": \"{}\"", self.get_type());
        buffer.push_sr(match &self.node {
            Node::If { condition, else_body, body } => format!("\"condition\": {}, \"body\": [{}], \"else_body\": [{}]",
                condition.format_for_debug(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                else_body.as_ref().map_or("".to_string(), |eb| eb.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "))
            ),
            Node::For { iterable, body, variable } => format!("\"iterable\": {}, \"variable\": \"{}\", \"body\": [{}]",
                iterable.format_for_debug(),
                variable,
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::While { condition, body } => format!("\"condition\": {}, \"body\": [{}]",
                condition.format_for_debug(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::TryCatch { body, catch_body, exception_vars } => format!("\"body\": [{}], \"catch_body\": [{}], \"exception_vars\": [{}]",
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                catch_body.as_ref().map_or("".to_string(), |cb| cb.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")),
                exception_vars.as_ref().map_or("".to_string(), |ev| ev.join(", "))
            ),
            Node::Throw { node } => match &**node {
                ThrowNode::Tuple(stmt) => format!("\"value\": {}", stmt.format_for_debug()),
                ThrowNode::Message { message, from } => format!("\"message\": {}, \"from\": {}",
                    message.format_for_debug(),
                    from.as_ref().map_or("null".to_string(), |f| f.format_for_debug())
                ),
            },
            Node::Forget { node } => format!("\"value\": {}", node.format_for_debug()),
            Node::Continue | Node::Break => "".to_string(),
            Node::Defer { body } => format!("\"body\": [{}]",
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Scope { body, name, locals, is_local } => format!("\"name\": \"{}\", \"is_local\": {}, \"locals\": [{}], \"body\": [{}]",
                name.as_ref().map_or("".to_string(), |n| n.clone()),
                is_local,
                locals.join(", "),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Match { condition, cases } => format!("\"condition\": {}, \"cases\": [{}]",
                condition.format_for_debug(),
                (cases.iter().map(|case| match case {
                    MatchCase::Pattern { pattern, body, guard } => format!("{{\"pattern\": \"{}\", \"guard\": {}, \"body\": [{}]}}",
                        pattern,
                        guard.as_ref().map_or("null".to_string(), |g| g.format_for_debug()),
                        body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                    ),
                    MatchCase::Literal { patterns, body } => format!("{{\"patterns\": [{}], \"body\": [{}]}}",
                        patterns.iter().map(|p| p.format_for_debug()).collect::<Vec<String>>().join(", "),
                        body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                    ),
                }).collect::<Vec<String>>().join(", "))
            ),
            Node::Group { body } => format!("\"body\": [{}]",
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::FunctionDeclaration { name, pos_args, named_args, body, modifiers, return_type, effect_flags } => format!("\"name\": \"{}\", \"pos_args\": [{}], \"named_args\": [{}], \"modifiers\": [{}], \"return_type\": {}, \"effect_flags\": \"{:?}\", \"body\": [{}]",
                name,
                pos_args.iter().map(|v| v.format_for_debug()).collect::<Vec<String>>().join(", "),
                named_args.iter().map(|(n, v)| format!("{{\"{}\": {}}}", n, v.format_for_debug())).collect::<Vec<String>>().join(", "),
                modifiers.join(", "),
                return_type.format_for_debug(),
                effect_flags.display(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::GeneratorDeclaration { name, pos_args, named_args, body, modifiers, return_type, effect_flags } => format!("\"name\": \"{}\", \"pos_args\": [{}], \"named_args\": [{}], \"modifiers\": [{}], \"return_type\": {}, \"effect_flags\": \"{:?}\", \"body\": [{}]",
                name,
                pos_args.iter().map(|v| v.format_for_debug()).collect::<Vec<String>>().join(", "),
                named_args.iter().map(|(n, v)| format!("{{\"{}\": {}}}", n, v.format_for_debug())).collect::<Vec<String>>().join(", "),
                modifiers.join(", "),
                return_type.format_for_debug(),
                effect_flags.display(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Return { value } => format!("\"value\": {}", value.format_for_debug()),
            Node::Import { name, alias, named_imports, modifiers, import_all, module_path_opt } => format!("\"name\": \"{}\", \"alias\": {}, \"named_imports\": [{}], \"modifiers\": [{}], \"import_all\": {}, \"module_path_opt\": {}",
                name,
                alias.as_ref().map_or("null".to_string(), |a| format!("\"{}\"", a)),
                named_imports.iter().map(|v| v.format_for_debug()).collect::<Vec<String>>().join(", "),
                modifiers.join(", "),
                import_all,
                module_path_opt.as_ref().map_or("null".to_string(), |mp| mp.format_for_debug())
            ),
            Node::Export { names, aliases, modifiers_list } => format!("\"names\": [{}, \"aliases\": [{}], \"modifiers_list\": [[{}]]",
                names.iter().map(|n| format!("\"{}\"", n)).collect::<Vec<String>>().join(", "),
                aliases.iter().map(|a| format!("\"{}\"", a)).collect::<Vec<String>>().join(", "),
                modifiers_list.iter().map(|mods| mods.join(", ")).collect::<Vec<String>>().join("], [")
            ),
            Node::VariableDeclaration { name, val_stmt, var_type, modifiers, is_default } => format!("\"name\": \"{}\", \"var_type\": {}, \"val_stmt\": {}, \"modifiers\": [{}], \"is_default\": {}",
                name,
                var_type.format_for_debug(),
                val_stmt.format_for_debug(),
                modifiers.join(", "),
                is_default
            ),
            Node::Variable { name } => format!("\"name\": \"{}\"", name),
            Node::Assignment { left, right } => format!("\"left\": {}, \"right\": {}",
                left.format_for_debug(),
                right.format_for_debug()
            ),
            Node::UnpackAssignment { targets, stmt } => format!("\"targets\": [{}], \"stmt\": {}",
                targets.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                stmt.format_for_debug()
            ),
            Node::Number { value } => format!("\"value\": \"{}\"", value),
            Node::String { value, mods } => format!("\"value\": \"{}\", \"mods\": [{}]",
                value,
                mods.join(", ")
            ),
            Node::Boolean { value } => format!("\"value\": {}", value.map_or("null".to_string(), |v| v.to_string())),
            Node::Map { keys_stmts, values_stmts } => format!("\"keys_stmts\": [{}], \"values_stmts\": [{}]",
                keys_stmts.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                values_stmts.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Iterable { node } => match node {
                IterableNode::List { elements } => format!("\"type\": \"list\", \"elements\": [{}]",
                    elements.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                ),
                IterableNode::Tuple { elements } => format!("\"type\": \"tuple\", \"elements\": [{}]",
                    elements.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                ),
                IterableNode::ListCompletion { seed, end, pattern_flag, range_mode, is_infinite } => format!("\"type\": \"list_completion\", \"seed\": [{}], \"end\": {}, \"pattern_flag\": {}, \"range_mode\": \"{:?}\", \"is_infinite\": {}",
                    seed.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                    end.format_for_debug(),
                    pattern_flag,
                    range_mode,
                    is_infinite
                ),
                IterableNode::ListComprehension { for_clauses, map_expression } => format!("\"type\": \"list_comprehension\", \"for_clauses\": [{}], \"map_expression\": {}",
                    for_clauses.iter().map(|v| v.format_for_debug()).collect::<Vec<String>>().join(", "),
                    map_expression.format_for_debug()
                ),
            },
            Node::Value { value } => format!("\"value\": {}", value.format_for_debug()),
            Node::Operation { left, operator, right } => format!("\"left\": {}, \"operator\": \"{}\", \"right\": {}",
                left.format_for_debug(),
                operator,
                right.format_for_debug()
            ),
            Node::UnaryOperation { operator, operand } => format!("\"operator\": \"{}\", \"operand\": {}",
                operator,
                operand.format_for_debug()
            ),
            Node::PrefixOperation { operator, operand } => format!("\"operator\": \"{}\", \"operand\": {}",
                operator,
                operand.format_for_debug()
            ),
            Node::Value { value } => format!("\"value\": {}", value.format_for_debug()),
            Node::Null => "".to_string(),
        });
        buffer.push('}');
        buffer
    }
    pub fn make_value(v: Value) -> Self {
        Statement {
            node: Node::Value { value: v },
            loc: None,
        }
    }
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
