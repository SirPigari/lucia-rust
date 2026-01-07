use std::collections::HashMap;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::value::Value;
use crate::env::runtime::types::Int;
use crate::env::runtime::statements::Statement;
use crate::env::libs::collections::deprecated_stuff::add_deprecated_functions;
use std::collections::VecDeque;
use std::sync::Arc;
use parking_lot::Mutex;
use crate::env::runtime::types::Type;
use crate::env::runtime::structs_and_enums::Struct;
use once_cell::sync::Lazy;

// This module provides many collection types.
// Lucia version 2.0.0, module: collections@2.0.0

// TODO: finish this module

use crate::{make_native_fn_pt, make_native_static_fn_pt, insert_native_var};

static VEC_DEQUE_IDS: Lazy<Arc<Mutex<HashMap<usize, VecDeque<Value>>>>> = Lazy::new(|| Arc::new(Mutex::new(HashMap::new())));

fn add_vecdeque_type(map: &mut HashMap<String, Variable>) {
    let mut vec_deque_type = Type::Struct {
        name: "VecDeque".to_string(),
        fields: vec![
            ("id".to_string(), Statement::make_value(Value::Type(Type::new_simple("int"))), vec!["private".to_string()]),
        ],
        methods: vec![],
        generics: vec!["T".to_string()],
        wheres: Vec::new(),
    };

    
    let len = ("len".to_string(),
        make_native_fn_pt!("len", move |args: &HashMap<String, Value>| -> Value {
            let self_struct = match args.get("self").unwrap() {
                Value::Struct(s) => s,
                _ => panic!("Expected struct instance"),
            };
            let id_value = self_struct.get_field("id").unwrap();
            let id = match &*id_value {
                Value::Int(i) => i.to_usize().unwrap(),
                _ => panic!("Expected int for id field"),
            };
            let vec_deque = VEC_DEQUE_IDS.lock();
            let deque = vec_deque.get(&id).unwrap();
            Value::Int(Int::from(deque.len() as i64))
        }, vec![Parameter::instance("self", &vec_deque_type, vec![])], &Type::new_simple("int"), EffectFlags::PURE)
    );
    let op_len = ("op_len".to_string(),
        len.1.clone()
    );
    let display = ("display".to_string(),
        make_native_fn_pt!("display", move |args: &HashMap<String, Value>| -> Value {
            let self_struct = match args.get("self").unwrap() {
                Value::Struct(s) => s,
                _ => panic!("Expected struct instance"),
            };
            let id_value = self_struct.get_field("id").unwrap();
            let id = match &*id_value {
                Value::Int(i) => i.to_usize().unwrap(),
                _ => panic!("Expected int for id field"),
            };
            let vec_deque = VEC_DEQUE_IDS.lock();
            let deque = vec_deque.get(&id).unwrap();
            let display_str = format!("{:?}", deque);
            Value::String(display_str)
        }, vec![Parameter::instance("self", &vec_deque_type, vec![])], &Type::new_simple("str"), EffectFlags::PURE)
    );
    let op_display = ("op_display".to_string(),
        display.1.clone()
    );

    if let Type::Struct { methods, .. } = &mut vec_deque_type {
        *methods = vec![
            len,
            op_len,
            display,
            op_display,
        ];
    }

    let vec_deque_type_clone = vec_deque_type.clone();
    let new = ("new".to_string(),
        make_native_static_fn_pt!("new", move |_args: &HashMap<String, Value>| -> Value {
            let id = VEC_DEQUE_IDS.lock().len() as usize + 1;
            VEC_DEQUE_IDS.lock().insert(id, VecDeque::new());
            Value::Struct(Box::new(Struct::new_with_fields(vec_deque_type_clone.clone(), HashMap::from([("id".to_string(), (Box::new(Value::Int(Int::from(id))), Type::new_simple("int")))]))))
        }, vec![], &vec_deque_type, EffectFlags::PURE)
    );

    let vec_deque_type_clone = vec_deque_type.clone();
    let from = ("from".to_string(),
        make_native_static_fn_pt!("from", move |args: &HashMap<String, Value>| -> Value {
            let list_value = args.get("l").unwrap();
            let list = match list_value {
                Value::List(l) => l,
                _ => panic!("Expected list"),
            };
            let id = VEC_DEQUE_IDS.lock().len() as usize + 1;
            let mut deque = VecDeque::new();
            for item in list {
                deque.push_back(item.clone());
            }
            VEC_DEQUE_IDS.lock().insert(id, deque);
            Value::Struct(Box::new(Struct::new_with_fields(vec_deque_type_clone.clone(), HashMap::from([("id".to_string(), (Box::new(Value::Int(Int::from(id))), Type::new_simple("int")))]))))
        }, vec![Parameter::positional("l", "list")], &vec_deque_type, EffectFlags::PURE)
    );

    if let Type::Struct { methods, .. } = &mut vec_deque_type {
        *methods = vec![
            new,
            from,
        ];
    }

    insert_native_var!(map, "VecDeque", Value::Type(vec_deque_type), "type");
}

fn add_collection_types(map: &mut HashMap<String, Variable>) {
    // VecDeque
    add_vecdeque_type(map);
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    // will get removed in future
    add_deprecated_functions(&mut map);
    add_collection_types(&mut map);

    map
}
