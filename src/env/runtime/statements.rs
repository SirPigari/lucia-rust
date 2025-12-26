use crate::env::runtime::value::Value;
use crate::env::runtime::internal_structs::{PathElement, EffectFlags};
use crate::env::runtime::utils::{format_value, unescape_string_literal};
use std::collections::HashMap;
use crate::env::runtime::tokens::Location;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::fmt;

pub static LOC_TABLE: Lazy<RwLock<Vec<Location>>> =
    Lazy::new(|| RwLock::new(Vec::with_capacity(1024)));

#[inline]
pub fn alloc_loc(loc: Option<Location>) -> Option<AstNodeId> {
    let l = loc?;
    let mut table = LOC_TABLE.write();
    let id = table.len();
    table.push(l);
    Some(id)
}

#[inline]
pub fn get_loc(id: usize) -> Location {
    let table = LOC_TABLE.read();
    table[id].clone()
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum ThrowNode {
    Tuple(Statement),
    Message {
        message: Statement,
        from: Option<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
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
        for_clauses: Vec<(PathElement, Statement, Vec<Statement>)>, // (variables, iterable, filters)
        map_expression: Box<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum AccessType {
    Range {
        start: Statement,
        end: Statement,
    },
    Single {
        index: Statement
    },
    ToEnd {
        start: Statement,
    },
    ToStart {
        end: Statement,
    },
    Full,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum TypeNode {
    Simple {
        base: String,
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
    },
    Reference {
        base_type: Box<TypeNode>,
        ref_level: usize,
    },
    Maybe {
        base_type: Box<TypeNode>,
    },
}

impl TypeNode {
    pub fn format_for_debug(&self) -> String {
        match self {
            TypeNode::Simple { base } => format!("\"type_type\": \"simple\", \"base\": \"{}\"",
                base,
            ),
            TypeNode::Union { types } => format!("{{\"type_type\": \"union\", \"types\": [{}]}}",
                types.iter().map(|t| t.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            TypeNode::Function { parameters_types, return_type } => format!("{{\"type_type\": \"function\", \"parameters_types\": [{}], \"return_type\": {}}}",
                parameters_types.iter().map(|t| t.format_for_debug()).collect::<Vec<String>>().join(", "),
                return_type.format_for_debug()
            ),
            TypeNode::Generator { parameters_types, yield_type } => format!("{{\"type_type\": \"generator\", \"parameters_types\": [{}], \"yield_type\": {}}}",
                parameters_types.iter().map(|t| t.format_for_debug()).collect::<Vec<String>>().join(", "),
                yield_type.format_for_debug()
            ),
            TypeNode::Generics { base_type, generics_types } => format!("{{\"type_type\": \"generics\", \"base_type\": {}, \"generics_types\": [{}]}}",
                base_type.format_for_debug(),
                generics_types.iter().map(|t| t.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            TypeNode::Impl { impls, joins } => format!("{{\"type_type\": \"impl\", \"impls\": [{}], \"joins\": [{}]}}",
                impls.iter().map(|(trait_name, args, modifiers, return_type)| format!("{{\"trait_name\": \"{}\", \"args\": [{}], \"modifiers\": [{}], \"return_type\": {}}}",
                    trait_name,
                    args.iter().map(|t| t.format_for_debug()).collect::<Vec<String>>().join(", "),
                    modifiers.join(", "),
                    return_type.format_for_debug()
                )).collect::<Vec<String>>().join(", "),
                joins.iter().map(|j| format!("\"{}\"", j)).collect::<Vec<String>>().join(", ")
            ),
            TypeNode::Reference { base_type, ref_level } => format!("{{\"type_type\": \"reference\", \"base_type\": {}, \"ref_level\": {}}}",
                base_type.format_for_debug(),
                ref_level
            ),
            TypeNode::Maybe { base_type } => format!("{{\"type_type\": \"maybe\", \"base_type\": {}}}",
                base_type.format_for_debug()
            ),  
            TypeNode::Variadics { base } => format!("{{\"type_type\": \"variadics\", \"base\": {}}}",
                base.format_for_debug()
            ),
        }
    } 
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum TypeDeclNode {
    Alias {
        base_type: Box<Statement>,
        conditions: Vec<Statement>,
        variables: Vec<String>,
    },
    Enum {
        variants: Vec<(String, Statement, usize)>, // (variant_name, variant_type, variant_index)
        wheres: Vec<(String, Statement)>,
        generics: Vec<String>,
    },
    Struct {
        fields: Vec<(String, Statement, Vec<String>)>, // (field_name, field_type, modifiers)
        wheres: Vec<(String, Statement)>,
        generics: Vec<String>,
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct ParamAST {
    pub name: String,
    pub ty: Option<Statement>,
    pub default: Option<Statement>,
    pub modifiers: Vec<String>,
    pub is_variadic: bool,
}

impl ParamAST {
    pub fn format_for_debug(&self) -> String {
        format!("{{\"name\": \"{}\", \"ty\": {}, \"default\": {}, \"modifiers\": [{}], \"is_variadic\": {}}}",
            self.name,
            self.ty.as_ref().map_or("null".to_string(), |t| t.format_for_debug()),
            self.default.as_ref().map_or("null".to_string(), |d| d.format_for_debug()),
            self.modifiers.join(", "),
            self.is_variadic
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum PtrNode {
    PointerRef {
        ref_level: usize,
    },
    PointerDeref {
        deref_level: usize,
    },
    PointerAssign {
        target: Box<Statement>,
        assign_level: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum ForLoopNode {
    Standard {
        initializer: Box<Statement>,
        condition: Box<Statement>,
        update: Box<Statement>,
    },
    ForIn {
        variable: PathElement,
        iterable: Box<Statement>,
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum Node {
    If {
        condition: Box<Statement>,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    For {
        node: ForLoopNode,
        body: Vec<Statement>,
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
        args: Vec<ParamAST>,
        body: Vec<Statement>,
        modifiers: Vec<String>,
        return_type: Box<Statement>,
        effect_flags: EffectFlags,
    },
    GeneratorDeclaration {
        name: String,
        args: Vec<ParamAST>,
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
        mods: Vec<char>,
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
    PostfixOperation {
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
    TypeConvert {
        node: Box<Statement>,
        target_type: Box<Statement>,
    },
    TypeDeclaration {
        name: String,
        modifiers: Vec<String>,
        node: TypeDeclNode,
    },

    StructCreation {
        name: String, 
        fields_ast: HashMap<String, Statement>, 
        is_null: bool
    },
    StructMethods {
        struct_name: String,
        methods_ast: Vec<Statement>,
    },

    Pointer {
        ptr_node: PtrNode,
        value: Box<Statement>,
    },
    Null,
}

pub type AstNodeId = usize;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
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
            Node::PostfixOperation { .. } => "postfix_operation".to_string(),
            Node::Pipeline { .. } => "pipeline".to_string(),
            Node::Call { .. } => "call".to_string(),
            Node::MethodCall { .. } => "method call".to_string(),
            Node::PropertyAccess { .. } => "property access".to_string(),
            Node::IndexAccess { .. } => "index access".to_string(),
            Node::Type { .. } => "type".to_string(),
            Node::TypeConvert { .. } => "type conversion".to_string(),
            Node::TypeDeclaration { .. } => "type declaration".to_string(),
            Node::StructCreation { .. } => "struct creation".to_string(),
            Node::StructMethods { .. } => "struct methods".to_string(),
            Node::Pointer { ptr_node, .. } => match ptr_node {
                PtrNode::PointerRef { .. } => "pointer reference".to_string(),
                PtrNode::PointerDeref { .. } => "pointer dereference".to_string(),
                PtrNode::PointerAssign { .. } => "pointer assignment".to_string(),
            },
            Node::Null => "none".to_string(),
        }
    }
}

impl Statement {
    // backwards compatibility
    #[allow(non_upper_case_globals)]
    pub const Null: Statement = Statement {
        node: Node::Null,
        loc: None,
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
            Node::PostfixOperation { .. } => "POSTFIX_OPERATION".to_string(),
            Node::Pipeline { .. } => "PIPELINE".to_string(),
            Node::Call { .. } => "CALL".to_string(),
            Node::MethodCall { .. } => "METHOD_CALL".to_string(),
            Node::PropertyAccess { .. } => "PROPERTY_ACCESS".to_string(),
            Node::IndexAccess { .. } => "INDEX_ACCESS".to_string(),
            Node::Type { .. } => "TYPE".to_string(),
            Node::TypeConvert { .. } => "TYPE_CONVERT".to_string(),
            Node::TypeDeclaration { .. } => "TYPE_DECLARATION".to_string(),
            Node::StructCreation { .. } => "STRUCT_CREATION".to_string(),
            Node::StructMethods { .. } => "STRUCT_METHODS".to_string(),
            Node::Pointer { .. } => "POINTER".to_string(),
            Node::Null => "NULL".to_string(),
        }
    }

    pub fn format_for_debug(&self) -> String {
        let mut buffer = format!("{{\"type\": \"{}\"", self.get_type());
        buffer.push_str(&match &self.node {
            Node::If { condition, else_body, body } => format!(", \"condition\": {}, \"body\": [{}], \"else_body\": [{}]",
                condition.format_for_debug(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                else_body.as_ref().map_or("".to_string(), |eb| eb.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "))
            ),
            Node::For { node, body } => match node {
                ForLoopNode::Standard { initializer, condition, update } => format!(", \"initializer\": {}, \"condition\": {}, \"update\": {}, \"body\": [{}]",
                    initializer.format_for_debug(),
                    condition.format_for_debug(),
                    update.format_for_debug(),
                    body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                ),
                ForLoopNode::ForIn { variable, iterable } => format!(", \"variable\": \"{}\", \"iterable\": {}, \"body\": [{}]",
                    variable.display(),
                    iterable.format_for_debug(),
                    body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                ),
            },
            Node::While { condition, body } => format!(", \"condition\": {}, \"body\": [{}]",
                condition.format_for_debug(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::TryCatch { body, catch_body, exception_vars } => format!(", \"body\": [{}], \"catch_body\": [{}], \"exception_vars\": [{}]",
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                catch_body.as_ref().map_or("".to_string(), |cb| cb.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")),
                exception_vars.as_ref().map_or("".to_string(), |ev| ev.join(", "))
            ),
            Node::Throw { node } => match &**node {
                ThrowNode::Tuple(stmt) => format!(", \"value\": {}", stmt.format_for_debug()),
                ThrowNode::Message { message, from } => format!(", \"message\": {}, \"from\": {}",
                    message.format_for_debug(),
                    from.as_ref().map_or("null".to_string(), |f| f.format_for_debug())
                ),
            },
            Node::Forget { node } => format!(", \"value\": {}", node.format_for_debug()),
            Node::Continue | Node::Break => "".to_string(),
            Node::Defer { body } => format!(", \"body\": [{}]",
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Scope { body, name, locals, is_local } => format!(", \"name\": \"{}\", \"is_local\": {}, \"locals\": [{}], \"body\": [{}]",
                name.as_ref().map_or("".to_string(), |n| n.clone()),
                is_local,
                locals.join(", "),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Match { condition, cases } => format!(", \"condition\": {}, \"cases\": [{}]",
                condition.format_for_debug(),
                (cases.iter().map(|case| match case {
                    MatchCase::Pattern { pattern, body, guard } => format!("{{\"pattern\": \"{}\", \"guard\": {}, \"body\": [{}]}}",
                        pattern.display(),
                        guard.as_ref().map_or("null".to_string(), |g| g.format_for_debug()),
                        body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                    ),
                    MatchCase::Literal { patterns, body } => format!("{{\"patterns\": [{}], \"body\": [{}]}}",
                        patterns.iter().map(|p| p.format_for_debug()).collect::<Vec<String>>().join(", "),
                        body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                    ),
                }).collect::<Vec<String>>().join(", "))
            ),
            Node::Group { body } => format!(", \"body\": [{}]",
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::FunctionDeclaration { name, args, body, modifiers, return_type, effect_flags } => format!(", \"name\": \"{}\", \"args\": [{}], \"modifiers\": [{}], \"return_type\": {}, \"effect_flags\": \"{}\", \"body\": [{}]",
                name,
                args.iter().map(|v| v.format_for_debug()).collect::<Vec<String>>().join(", "),
                modifiers.into_iter().map(|m| format!("{:?}", m)).collect::<Vec<String>>().join(", "),
                return_type.format_for_debug(),
                effect_flags.display(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::GeneratorDeclaration { name, args, body, modifiers, return_type, effect_flags } => format!(", \"name\": \"{}\", \"args\": [{}], \"modifiers\": [{}], \"return_type\": {}, \"effect_flags\": \"{}\", \"body\": [{}]",
                name,
                args.iter().map(|v| v.format_for_debug()).collect::<Vec<String>>().join(", "),
                modifiers.into_iter().map(|m| format!("{:?}", m)).collect::<Vec<String>>().join(", "),
                return_type.format_for_debug(),
                effect_flags.display(),
                body.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Return { value } => format!(", \"value\": {}", value.format_for_debug()),
            Node::Import { name, alias, named_imports, modifiers, import_all, module_path_opt } => format!(", \"name\": \"{}\", \"alias\": {}, \"named_imports\": [{}], \"modifiers\": [{}], \"import_all\": {}, \"module_path_opt\": {}",
                name,
                alias.as_ref().map_or("null".to_string(), |a| format!("\"{}\"", a)),
                named_imports.iter().map(|v| format_value(v)).collect::<Vec<String>>().join(", "),
                modifiers.join(", "),
                import_all,
                module_path_opt.as_ref().map_or("null".to_string(), |mp| mp.format_for_debug())
            ),
            Node::Export { names, aliases, modifiers_list } => format!(", \"names\": [{}, \"aliases\": [{}], \"modifiers_list\": [[{}]]",
                names.iter().map(|n| format!("\"{}\"", n)).collect::<Vec<String>>().join(", "),
                aliases.iter().map(|a| format!("\"{}\"", a)).collect::<Vec<String>>().join(", "),
                modifiers_list.iter().map(|mods| mods.join(", ")).collect::<Vec<String>>().join("], [")
            ),
            Node::VariableDeclaration { name, val_stmt, var_type, modifiers, is_default } => format!(", \"name\": \"{}\", \"var_type\": {}, \"val_stmt\": {}, \"modifiers\": [{}], \"is_default\": {}",
                name,
                var_type.format_for_debug(),
                val_stmt.format_for_debug(),
                modifiers.join(", "),
                is_default
            ),
            Node::Variable { name } => format!(", \"name\": \"{}\"", name),
            Node::Assignment { left, right } => format!(", \"left\": {}, \"right\": {}",
                left.format_for_debug(),
                right.format_for_debug()
            ),
            Node::UnpackAssignment { targets, stmt } => format!(", \"targets\": [{}], \"stmt\": {}",
                targets.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                stmt.format_for_debug()
            ),
            Node::Number { value } => format!(", \"value\": \"{}\"", value),
            Node::String { value, mods } => format!(", value: \"{}\", \"mods\": [{}]",
                unescape_string_literal(value).unwrap_or(value.clone()),
                mods.iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Node::Boolean { value } => format!("\"value\": {}", value.map_or("null".to_string(), |v| v.to_string())),
            Node::Map { keys_stmts, values_stmts } => format!("\"keys_stmts\": [{}], \"values_stmts\": [{}]",
                keys_stmts.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                values_stmts.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Iterable { node } => match node {
                IterableNode::List { elements } => format!(", \"iterable_type\": \"list\", \"elements\": [{}]",
                    elements.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                ),
                IterableNode::Tuple { elements } => format!(", \"iterable_type\": \"tuple\", \"elements\": [{}]",
                    elements.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
                ),
                IterableNode::ListCompletion { seed, end, pattern_flag, range_mode, is_infinite } => format!(", \"iterable_type\": \"list_completion\", \"seed\": [{}], \"end\": {}, \"pattern_flag\": {}, \"range_mode\": \"{:?}\", \"is_infinite\": {}",
                    seed.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                    end.format_for_debug(),
                    pattern_flag,
                    range_mode,
                    is_infinite
                ),
                IterableNode::ListComprehension { for_clauses, map_expression } => format!(", \"iterable_type\": \"list_comprehension\", \"for_clauses\": [{}], \"map_expression\": {}",
                    for_clauses.iter().map(|(pe, stmt, vs)| format!("({}, {}, [{}])", pe.display(), stmt.format_for_debug(), vs.into_iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "))).collect::<Vec<String>>().join(", "),
                    map_expression.format_for_debug()
                ),
            },
            Node::Value { value } => format!(", \"value\": {}", format_value(value)),
            Node::Operation { left, operator, right } => format!(", \"left\": {}, \"operator\": \"{}\", \"right\": {}",
                left.format_for_debug(),
                operator,
                right.format_for_debug()
            ),
            Node::UnaryOperation { operator, operand } => format!(", \"operator\": \"{}\", \"operand\": {}",
                operator,
                operand.format_for_debug()
            ),
            Node::PrefixOperation { operator, operand } => format!(", \"operator\": \"{}\", \"operand\": {}",
                operator,
                operand.format_for_debug()
            ),
            Node::PostfixOperation { operator, operand } => format!(", \"operator\": \"{}\", \"operand\": {}",
                operator,
                operand.format_for_debug()
            ),
            Node::Pipeline { initial_value, arguments } => format!(", \"initial_value\": {}, \"arguments\": [{}]",
                initial_value.format_for_debug(),
                arguments.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Call { name, pos_args, named_args } => format!(", \"name\": \"{}\", \"pos_args\": [{}], \"named_args\": {{{}}}",
                name,
                pos_args.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                named_args.iter().map(|(k, v)| format!("\"{}\": {}", k, v.format_for_debug())).collect::<Vec<String>>().join(", ")
            ),
            Node::MethodCall { object, method_name, pos_args, named_args } => format!(", \"object\": {}, \"method_name\": \"{}\", \"pos_args\": [{}], \"named_args\": {{{}}}",
                object.format_for_debug(),
                method_name,
                pos_args.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", "),
                named_args.iter().map(|(k, v)| format!("\"{}\": {}", k, v.format_for_debug())).collect::<Vec<String>>().join(", ")
            ),
            Node::PropertyAccess { object, property_name } => format!(", \"object\": {}, \"property_name\": \"{}\"",
                object.format_for_debug(),
                property_name
            ),
            Node::IndexAccess { object, access } => format!(", \"object\": {}, {}",
                object.format_for_debug(),
                match &**access {
                    AccessType::Range { start, end } => format!("\"access_type\": \"range\", \"start\": {}, \"end\": {}",
                        start.format_for_debug(),
                        end.format_for_debug()
                    ),
                    AccessType::Single { index } => format!("\"access_type\": \"single\", \"index\": {}",
                        index.format_for_debug()
                    ),
                    AccessType::ToEnd { start } => format!("\"access_type\": \"to_end\", \"start\": {}",
                        start.format_for_debug()
                    ),
                    AccessType::ToStart { end } => format!("\"access_type\": \"to_start\", \"end\": {}",
                        end.format_for_debug()
                    ),
                    AccessType::Full => "\"access_type\": \"full\"".to_string(),
                }
            ),
            Node::Type { node } => format!(", {}", node.format_for_debug()),
            Node::TypeConvert { node, target_type } => format!(", \"node\": {}, \"target_type\": {}",
                node.format_for_debug(),
                target_type.format_for_debug()
            ),
            Node::TypeDeclaration { name, modifiers, node } => format!(", \"name\": \"{}\", \"modifiers\": [{}], {}",
                name,
                modifiers.join(", "),
                match node {
                    TypeDeclNode::Alias { base_type, conditions, variables } => format!("\"decl_type\": \"alias\", \"base_type\": {}, \"conditions\": [{}], \"variables\": [{}]",
                        base_type.format_for_debug(),
                        conditions.iter().map(|c| c.format_for_debug()).collect::<Vec<String>>().join(", "),
                        variables.join(", ")
                    ),
                    TypeDeclNode::Enum { variants, wheres, generics } => format!("\"decl_type\": \"enum\", \"variants\": [{}], \"wheres\": [{}], \"generics\": [{}]",
                        variants.iter().map(|(variant_name, variant_type, variant_index)| format!("{{\"variant_name\": \"{}\", \"variant_type\": {}, \"variant_index\": {}}}",
                            variant_name,
                            variant_type.format_for_debug(),
                            variant_index
                        )).collect::<Vec<String>>().join(", "),
                        wheres.iter().map(|(name, stmt)| format!("{{\"name\": \"{}\", \"stmt\": {}}}",
                            name,
                            stmt.format_for_debug()
                        )).collect::<Vec<String>>().join(", "),
                        generics.join(", ")
                    ),
                    TypeDeclNode::Struct { fields, wheres, generics } => format!("\"decl_type\": \"struct\", \"fields\": [{}], \"wheres\": [{}], \"generics\": [{}]",
                        fields.iter().map(|(field_name, field_type, modifiers)| format!("{{\"field_name\": \"{}\", \"field_type\": {}, \"modifiers\": [{}]}}",
                            field_name,
                            field_type.format_for_debug(),
                            modifiers.join(", ")
                        )).collect::<Vec<String>>().join(", "),
                        wheres.iter().map(|(name, stmt)| format!("{{\"name\": \"{}\", \"stmt\": {}}}",
                            name,
                            stmt.format_for_debug()
                        )).collect::<Vec<String>>().join(", "),
                        generics.join(", ")
                    ),
                },
            ),
            Node::StructCreation { name, fields_ast, is_null } => format!(", \"name\": \"{}\", \"is_null\": {}, \"fields_ast\": {{{}}}",
                name,
                is_null,
                fields_ast.iter().map(|(k, v)| format!("\"{}\": {}", k, v.format_for_debug())).collect::<Vec<String>>().join(", ")
            ),
            Node::StructMethods { struct_name, methods_ast } => format!(", \"struct_name\": \"{}\", \"methods_ast\": [{}]",
                struct_name,
                methods_ast.iter().map(|s| s.format_for_debug()).collect::<Vec<String>>().join(", ")
            ),
            Node::Pointer { ptr_node, value } => format!(", \"ptr_node\": {}, \"value\": {}",
                match ptr_node {
                    PtrNode::PointerRef { ref_level } => format!("{{\"ptr_type\": \"ref\", \"ref_level\": {}}}", ref_level),
                    PtrNode::PointerDeref { deref_level } => format!("{{\"ptr_type\": \"deref\", \"deref_level\": {}}}", deref_level),
                    PtrNode::PointerAssign { target, assign_level } => format!("{{\"ptr_type\": \"assign\", \"target\": {}, \"assign_level\": {}}}",
                        target.format_for_debug(),
                        assign_level
                    ),
                },
                value.format_for_debug()
            ),
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
}
