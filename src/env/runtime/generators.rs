use crate::env::runtime::value::Value;
use crate::env::runtime::statements::Statement;
use crate::interpreter::Interpreter;
use crate::env::runtime::internal_structs::State;
use crate::env::runtime::utils::{to_static, check_pattern};
use crate::env::runtime::types::{Int, Type};
use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::Function;

use std::cmp::Ordering;
use std::fmt;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

fn add(a: &Value, b: &Value) -> Option<Value> {
    match (a, b) {
        (Value::Int(a_i), Value::Int(b_i)) => {
            let sum = (a_i.clone() + b_i.clone()).ok()?;
            Some(Value::Int(sum))
        }
        (Value::Float(a_f), Value::Float(b_f)) => {
            let sum = (a_f.clone() + b_f.clone()).ok()?;
            Some(Value::Float(sum))
        }
        (Value::Int(a_i), Value::Float(b_f)) => {
            let a_f = a_i.to_float().ok()?;
            let sum = (a_f.clone() + b_f.clone()).ok()?;
            Some(Value::Float(sum))
        }
        (Value::Float(a_f), Value::Int(b_i)) => {
            let b_f = b_i.to_float().ok()?;
            let sum = (a_f.clone() + b_f).ok()?;
            Some(Value::Float(sum))
        }
        _ => None,
    }
}

fn add_int(a: &Int, b: &Int) -> Option<Int> {
    (a.clone() + b.clone()).ok()
}

fn cmp_int(a: &Int, b: &Int) -> Option<Ordering> {
    a.partial_cmp(b)
}

fn cmp(a: &Value, b: &Value) -> Option<Ordering> {
    match (a, b) {
        (Value::Int(a_i), Value::Int(b_i)) => a_i.partial_cmp(b_i),
        (Value::Float(a_f), Value::Float(b_f)) => a_f.partial_cmp(b_f),
        (Value::Int(a_i), Value::Float(b_f)) => {
            let a_f = a_i.to_float().ok()?;
            a_f.partial_cmp(b_f)
        }
        (Value::Float(a_f), Value::Int(b_i)) => {
            let b_f = b_i.to_float().ok()?;
            a_f.partial_cmp(&b_f)
        }
        _ => None,
    }
}

pub trait GeneratorIterator: Iterator<Item = Value> + Send + Sync {
    fn clone_box(&self) -> Box<dyn GeneratorIterator>;
    fn is_done(&self) -> bool;
    fn is_infinite(&self) -> bool {
        false
    }
    fn get_size(&self) -> usize;
}

impl Clone for Box<dyn GeneratorIterator> {
    fn clone(&self) -> Box<dyn GeneratorIterator> {
        self.as_ref().clone_box()
    }
}

#[derive(Clone)]
pub struct Generator {
    inner: Arc<Mutex<GeneratorInner>>,
}

impl PartialEq for Generator {
    fn eq(&self, other: &Self) -> bool {
        let a = self.inner.lock().unwrap();
        let b = other.inner.lock().unwrap();
        *a == *b
    }
}

impl PartialOrd for Generator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let a = self.inner.lock().unwrap();
        let b = other.inner.lock().unwrap();
        a.partial_cmp(&b)
    }
}

impl Generator {
    pub fn new(name: String, kind: GeneratorType, is_static: bool) -> Self {
        Self {
            inner: Arc::new(Mutex::new(GeneratorInner {
                name: Some(name),
                kind,
                is_static,
                has_iterated: false,
            })),
        }
    }

    pub fn new_anonymous(kind: GeneratorType, is_static: bool) -> Self {
        Self {
            inner: Arc::new(Mutex::new(GeneratorInner {
                name: None,
                kind,
                is_static,
                has_iterated: false,
            })),
        }
    }

    pub fn name(&self) -> Option<&'static str> {
        let name = self.inner.lock().unwrap().name.clone()?;
        Some(to_static(name))
    }

    pub fn ptr(&self) -> *const Self {
        self as *const Self
    }

    pub fn make_iter(&self) -> Box<dyn Iterator<Item = Value> + Send + Sync> {
        let mut inner = self.inner.lock().unwrap();

        if inner.is_static {
            if inner.has_iterated {
                return Box::new(std::iter::once(Value::Error(
                    "StaticGeneratorError",
                    "Cannot iterate a static generator more than once",
                    None,
                )));
            }
            inner.has_iterated = true;
        }

        match &inner.kind {
            GeneratorType::Native(native) => native.iter.clone(),
            GeneratorType::Custom(custom) => Box::new(custom.clone()),
        }
    }

    pub fn next(&self) -> Option<Value> {
        let mut inner = self.inner.lock().unwrap();
        if inner.is_static {
            if inner.has_iterated {
                return Some(Value::Error(
                    "StaticGeneratorError",
                    "Cannot iterate a static generator more than once",
                    None,
                ));
            }
            inner.has_iterated = true;
        }

        match &mut inner.kind {
            GeneratorType::Native(native) => native.iter.next(),
            GeneratorType::Custom(custom) => custom.next(),
        }
    }

    pub fn peek(&self) -> Option<Value> {
        let inner = self.inner.lock().unwrap();
        if inner.is_static {
            if inner.has_iterated {
                return Some(Value::Error(
                    "StaticGeneratorError",
                    "Cannot iterate a static generator more than once",
                    None,
                ));
            }
        }

        let clone = inner.kind.clone();
        match clone {
            GeneratorType::Native(mut native) => native.iter.next(),
            GeneratorType::Custom(mut custom) => custom.next(),
        }
    }

    pub fn take(&self, count: usize) -> Vec<Value> {
        let mut inner = self.inner.lock().unwrap();

        if inner.is_static {
            if inner.has_iterated {
                return vec![Value::Error(
                    "StaticGeneratorError",
                    "Cannot iterate a static generator more than once",
                    None,
                )];
            }
            inner.has_iterated = true;
        }

        match &mut inner.kind {
            GeneratorType::Native(native) => native.iter.by_ref().take(count).collect(),
            GeneratorType::Custom(custom) => custom.by_ref().take(count).collect(),
        }
    }

    pub fn is_done(&self) -> bool {
        match &self.inner.lock().unwrap().kind {
            GeneratorType::Native(native) => native.iter.is_done(),
            GeneratorType::Custom(custom) => custom.done,
        }
    }

    pub fn get_size(&self) -> usize {
        match &self.inner.lock().unwrap().kind {
            GeneratorType::Native(native) => native.get_size(),
            GeneratorType::Custom(custom) => custom.get_size(),
        }
    }

    pub fn is_infinite(&self) -> bool {
        match &self.inner.lock().unwrap().kind {
            GeneratorType::Native(native) => native.iter.is_infinite(),
            GeneratorType::Custom(custom) => custom.body.is_empty(),
        }
    }

    pub fn to_vec(&self) -> Vec<Value> {
        let mut inner = self.inner.lock().unwrap();

        if inner.is_static {
            if inner.has_iterated {
                return vec![Value::Error(
                    "StaticGeneratorError",
                    "Cannot iterate a static generator more than once",
                    None,
                )];
            }
            inner.has_iterated = true;
        }

        match &inner.kind {
            GeneratorType::Native(native) => native.iter.clone().collect(),
            GeneratorType::Custom(custom) => custom.clone().collect(),
        }
    }

    pub fn get_yield_type(&self) -> Option<Type> {
        match &self.inner.lock().unwrap().kind {
            GeneratorType::Native(_) => None, // Native generators do not have a yield type
            GeneratorType::Custom(custom) => Some(custom.ret_type.clone()),
        }
    }

    pub fn get_parameter_types(&self) -> Option<Vec<Type>> {
        match &self.inner.lock().unwrap().kind {
            GeneratorType::Native(_) => None, // Native generators do not have parameter types
            GeneratorType::Custom(_) => None, // TODO: Implement this
        }
    }

    pub fn help_string(&self) -> String {
        format!("Generator: {}\nParameters: {}\nReturn Type: {}\nIs done: {}\nIterated: {}\nIs static: {}\nIs infinite: {}",
            self.name().unwrap_or("<anonymous>"),
            match self.get_parameter_types() {
                Some(params) => params.iter().map(|t| t.display_simple()).collect::<Vec<_>>().join(", "),
                None => "unknown".to_string(),
            },
            match self.get_yield_type() {
                Some(ty) => ty.display_simple(),
                None => "unknown".to_string(),
            },
            self.is_done(),
            self.inner.lock().unwrap().has_iterated,
            self.inner.lock().unwrap().is_static,
            self.is_infinite()
        )
    }
}

impl fmt::Debug for Generator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = self.inner.lock().unwrap();
        f.debug_struct("Generator")
            .field("name", &inner.name)
            .field("kind", &inner.kind)
            .field("is_static", &inner.is_static)
            .finish()
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
struct GeneratorInner {
    name: Option<String>,
    kind: GeneratorType,
    is_static: bool,
    has_iterated: bool,
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum GeneratorType {
    Native(NativeGenerator),
    Custom(CustomGenerator),
}

impl fmt::Debug for GeneratorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratorType::Native(_) => f.write_str("NativeGenerator"),
            GeneratorType::Custom(_) => f.write_str("CustomGenerator"),
        }
    }
}

pub struct NativeGenerator {
    pub iter: Box<dyn GeneratorIterator>,
    pub iteration: usize,
}

impl NativeGenerator {
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + (*self.iter).get_size()
    }
}

impl Clone for NativeGenerator {
    fn clone(&self) -> Self {
        Self {
            iter: self.iter.clone(),
            iteration: self.iteration,
        }
    }
}

impl PartialEq for NativeGenerator {
    fn eq(&self, other: &Self) -> bool {
        self.iteration == other.iteration
    }
}

impl PartialOrd for NativeGenerator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iteration.partial_cmp(&other.iteration)
    }
}

#[derive(Clone)]
pub struct CustomGenerator {
    pub body: Vec<Statement>,
    pub interpreter: Box<Interpreter>,
    pub ret_type: Type,
    pub iteration: usize,
    pub index: usize,
    pub done: bool,
    pub loop_stack: Vec<Frame>,
}

impl CustomGenerator {
    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.body.len() * std::mem::size_of::<Statement>()
    }
}

impl PartialEq for CustomGenerator {
    fn eq(&self, other: &Self) -> bool {
        self.iteration == other.iteration && self.body == other.body
    }
}

impl PartialOrd for CustomGenerator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iteration.partial_cmp(&other.iteration)
    }
}

#[derive(Clone)]
enum LoopType {
    While {
        condition: Value,
        body: Vec<Value>,
    },
    For {
        var: Value,
        iterable: Vec<Value>,
        body: Vec<Value>,
        index: usize,
        body_pc: usize,
        saved_var: Vec<Variable>,
    },
}

#[derive(Clone)]
pub struct Frame {
    loop_type: LoopType,
    while_pc: usize,
}

impl Iterator for CustomGenerator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        self.interpreter.is_returning = false;
        self.interpreter.return_value = Value::Null;

        loop {
            while let Some(frame) = self.loop_stack.last_mut() {
                match &mut frame.loop_type {
                    LoopType::While { condition, body } => {
                        let pc = frame.while_pc;

                        if pc >= body.len() {
                            frame.while_pc = 0;
                            self.loop_stack.pop();
                            self.index += 1;
                            continue;
                        }

                        if !self.interpreter.evaluate(&condition.convert_to_statement()).is_truthy() {
                            self.loop_stack.pop();
                            self.index += 1;
                            continue;
                        }

                        let stmt = body[pc].clone();
                        let result = self.interpreter.evaluate(&stmt.convert_to_statement());

                        if self.interpreter.err.is_some() {
                            self.done = true;
                            let err = self.interpreter.err.take().unwrap();
                            return Some(Value::Error(
                                to_static(err.error_type),
                                to_static(err.msg),
                                err.ref_err.map(|boxed| *boxed),
                            ));
                        }

                        match self.interpreter.state {
                            State::Break => {
                                self.interpreter.state = State::Normal;
                                self.loop_stack.pop();
                                self.index += 1;
                                continue;
                            }
                            State::Continue => {
                                self.interpreter.state = State::Normal;
                                frame.while_pc = body.len();
                                continue;
                            }
                            _ => {}
                        }

                        frame.while_pc += 1;

                        if self.interpreter.is_returning {
                            return Some(result);
                        }

                        continue;
                    }

                    LoopType::For { var, iterable, body, index, body_pc, saved_var } => {
                        if *index >= iterable.len() {
                            for prev in saved_var.drain(..) {
                                self.interpreter.variables.insert(prev.get_name().to_string(), prev);
                            }
                            self.loop_stack.pop();
                            self.index += 1;
                            continue;
                        }

                        let item = iterable[*index].clone();

                        if saved_var.is_empty() {
                            if let Ok((_, bindings)) = check_pattern(&item, var) {
                                for name in bindings.keys() {
                                    if let Some(existing) = self.interpreter.variables.get(name) {
                                        saved_var.push(existing.clone());
                                    }
                                }
                            }
                        }

                        let bindings = match check_pattern(&item, var) {
                            Ok((true, vars)) => vars,
                            Ok((false, _)) => {
                                self.interpreter.raise("ValueError", "Item did not match for-loop variable pattern");
                                self.done = true;
                                return Some(Value::Null);
                            }
                            Err((etype, emsg)) => {
                                self.interpreter.raise(&etype, &emsg);
                                self.done = true;
                                return Some(Value::Null);
                            }
                        };

                        for (name, val) in &bindings {
                            self.interpreter.variables.insert(name.clone(), Variable::new(
                                name.clone(),
                                val.clone(),
                                val.type_name(),
                                false, true, true,
                            ));
                        }

                        let body_clone = body.clone();

                        if *body_pc >= body_clone.len() {
                            *body_pc = 0;
                            *index += 1;
                            continue;
                        }

                        let stmt = body_clone[*body_pc].clone();
                        let result = self.interpreter.evaluate(&stmt.convert_to_statement());

                        if self.interpreter.err.is_some() {
                            self.done = true;
                            let err = self.interpreter.err.take().unwrap();
                            return Some(Value::Error(
                                to_static(err.error_type),
                                to_static(err.msg),
                                err.ref_err.map(|boxed| *boxed),
                            ));
                        }

                        match self.interpreter.state {
                            State::Break => {
                                self.interpreter.state = State::Normal;
                                for prev in saved_var.drain(..) {
                                    self.interpreter.variables.insert(prev.get_name().to_string(), prev);
                                }
                                self.loop_stack.pop();
                                self.index += 1;
                                continue;
                            }
                            State::Continue => {
                                self.interpreter.state = State::Normal;
                                *body_pc = body_clone.len();
                                continue;
                            }
                            _ => {}
                        }

                        *body_pc += 1;

                        if self.interpreter.is_returning {
                            return Some(result);
                        }

                        continue;
                    }
                }
            }

            if self.index >= self.body.len() {
                self.done = true;
                return None;
            }

            let statement = self.body[self.index].clone();

            match statement.get_type().as_str() {
                "WHILE" => {
                    let data = statement.convert_to_hashmap();
                    let condition = data.get(&Value::String("condition".to_string())).cloned().unwrap_or(Value::Null);
                    let body = data.get(&Value::String("body".to_string()))
                        .and_then(|v| match v {
                            Value::List(l) => Some(l.clone()),
                            _ => None,
                        }).unwrap_or(vec![]);

                    self.loop_stack.push(Frame {
                        loop_type: LoopType::While { condition, body },
                        while_pc: 0,
                    });
                }

                "FOR" => {
                    let data = statement.convert_to_hashmap();

                    let iterable_val = data.get(&Value::String("iterable".to_string()))
                        .map(|v| self.interpreter.evaluate(&v.convert_to_statement()))
                        .unwrap_or(Value::Null);

                    if !iterable_val.is_iterable() {
                        self.done = true;
                        return Some(self.interpreter.raise("TypeError", "Expected iterable in for loop"));
                    }

                    let iterable_vec = iterable_val.iterable_to_vec();

                    let variable = match data.get(&Value::String("variable".to_string())) {
                        Some(v) => v.clone(),
                        None => Value::Null,
                    };

                    let body = data.get(&Value::String("body".to_string()))
                        .and_then(|v| match v {
                            Value::List(l) => Some(l.clone()),
                            _ => None,
                        }).unwrap_or(vec![]);

                    self.loop_stack.push(Frame {
                        loop_type: LoopType::For {
                            var: variable,
                            iterable: iterable_vec,
                            body,
                            index: 0,
                            body_pc: 0,
                            saved_var: Vec::new(),
                        },
                        while_pc: 0,
                    });
                }

                _ => {
                    let result = self.interpreter.evaluate(&statement);

                    if self.interpreter.err.is_some() {
                        self.done = true;
                        let err = self.interpreter.err.take().unwrap();
                        return Some(Value::Error(
                            to_static(err.error_type),
                            to_static(err.msg),
                            err.ref_err.map(|boxed| *boxed),
                        ));
                    }

                    self.index += 1;

                    if self.interpreter.is_returning {
                        self.iteration += 1;
                        return Some(result);
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct RangeValueIter {
    current: Value,
    end: Value,
    step: Value,
    done: bool,
}

impl RangeValueIter {
    pub fn new(start: &Value, end: &Value, step: &Value) -> Self {
        Self {
            current: start.clone(),
            end: end.clone(),
            step: step.clone(),
            done: false,
        }
    }
}

#[derive(Clone)]
pub struct RangeLengthIter {
    current: Int,
    end: Int,
    step: usize,
    index: Int,
    done: bool,
}

impl RangeLengthIter {
    pub fn new(start: Int, end: Int, step: usize) -> Self {
        Self {
            current: start,
            end,
            step: if step == 0 { 1 } else { step },
            index: Int::from(0),
            done: false,
        }
    }
}

#[derive(Clone)]
pub struct InfRangeIter {
    current: Value,
    step: Value,
}

impl InfRangeIter {
    pub fn new(start: Value, step: Value) -> Self {
        Self { current: start, step }
    }
}

#[derive(Clone)]
pub struct VecIter {
    pub vec: Vec<Value>,
    pub index: usize,
    pub done: bool,
}

impl VecIter {
    pub fn new(vec: &[Value]) -> Self {
        Self {
            vec: vec.to_vec(),
            index: 0,
            done: false,
        }
    }
}

#[derive(Clone)]
pub struct EnumerateIter {
    pub generator: Box<Generator>,
    pub index: usize,
    pub done: bool,
}

impl EnumerateIter {
    pub fn new(generator: &Generator) -> Self {
        Self {
            generator: Box::new(generator.clone()),
            index: 0,
            done: false,
        }
    }
}

#[derive(Clone)]
pub struct FilterIter {
    pub generator: Box<Generator>,
    pub filter_func: Function,
    pub interpreter: Arc<Mutex<Interpreter>>,
    pub done: bool,
}

impl FilterIter {
    pub fn new(generator: &Generator, filter_func: Function, interpreter: &Interpreter) -> Self {
        Self {
            generator: Box::new(generator.clone()),
            filter_func,
            interpreter: Arc::new(Mutex::new(interpreter.clone())),
            done: false,
        }
    }
}

#[derive(Clone)]
pub struct MapIter {
    pub generator: Box<Generator>,
    pub map_func: Function,
    pub interpreter: Arc<Mutex<Interpreter>>,
    pub done: bool,
}

impl MapIter {
    pub fn new(generator: &Generator, map_func: Function, interpreter: &Interpreter) -> Self {
        Self {
            generator: Box::new(generator.clone()),
            map_func,
            interpreter: Arc::new(Mutex::new(interpreter.clone())),
            done: false,
        }
    }
}

impl Iterator for RangeValueIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let cmp_result = cmp(&self.current, &self.end)?;
        let step_positive = match &self.step {
            Value::Int(i) => *i > 0.into(),
            Value::Float(f) => *f > 0.0.into(),
            _ => true,
        };

        let passed_end = if step_positive {
            cmp_result == Ordering::Greater
        } else {
            cmp_result == Ordering::Less
        };

        if passed_end {
            self.done = true;
            return None;
        }

        let val = self.current.clone();

        let next = add(&self.current, &self.step).unwrap_or(self.current.clone());

        let cmp_next = cmp(&next, &self.end)?;
        let next_passed_end = if step_positive {
            cmp_next == Ordering::Greater
        } else {
            cmp_next == Ordering::Less
        };

        if next_passed_end {
            self.done = true;
        }

        self.current = next;

        Some(val)
    }
}

impl Iterator for RangeLengthIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let val = self.current.clone();

        self.index = add_int(&self.index, &Int::from(1)).unwrap_or(self.index.clone());

        if let Some(cmp_result) = cmp_int(&self.index, &self.end) {
            if cmp_result == Ordering::Equal {
                self.done = true;
            }
        } else {
            self.done = true;
        }

        self.current = add_int(&self.current, &Int::from(self.step as i64)).unwrap_or(self.current.clone());

        Some(Value::Int(val))
    }
}

impl Iterator for InfRangeIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.current.clone();
        self.current = add(&self.current, &self.step).unwrap_or(self.current.clone());
        Some(val)
    }
}

impl Iterator for EnumerateIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let val = self.generator.next()?;

        let result = Value::Tuple(vec![Value::Int(Int::from(self.index)), val]);

        self.index += 1;

        if self.generator.is_done() {
            self.done = true;
        }

        Some(result)
    }
}

impl Iterator for VecIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done || self.index >= self.vec.len() {
            return None;
        }

        let val = self.vec[self.index].clone();
        self.index += 1;

        if self.index >= self.vec.len() {
            self.done = true;
        }

        Some(val)
    }
}

impl Iterator for FilterIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let mut interpreter = self.interpreter.lock().unwrap();

        while let Some(val) = self.generator.next() {
            let result = interpreter.call_function(
                &self.filter_func,
                vec![val.clone()],
                HashMap::new(),
                None,
            );

            if interpreter.err.is_some() {
                self.done = true;
                return Some(Value::Error(
                    "FilterError",
                    "Error during filter function evaluation",
                    None,
                ));
            }

            if result.is_truthy() {
                return Some(val);
            }
        }

        self.done = true;
        None
    }
}

impl Iterator for MapIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let mut interpreter = self.interpreter.lock().unwrap();

        if let Some(val) = self.generator.next() {
            let result = interpreter.call_function(
                &self.map_func,
                vec![val.clone()],
                HashMap::new(),
                None,
            );

            if interpreter.err.is_some() {
                self.done = true;
                return Some(Value::Error(
                    "MapError",
                    "Error during map function evaluation",
                    None,
                ));
            }

            return Some(result);
        }

        self.done = true;
        None
    }
}

impl GeneratorIterator for RangeValueIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }
    fn is_done(&self) -> bool {
        self.done
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.current.get_size() + self.end.get_size() + self.step.get_size()
    }
}

impl GeneratorIterator for RangeLengthIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }
    fn is_done(&self) -> bool {
        self.done
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl GeneratorIterator for InfRangeIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }

    fn is_done(&self) -> bool {
        false
    }

    fn is_infinite(&self) -> bool {
        true
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl GeneratorIterator for EnumerateIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }

    fn is_done(&self) -> bool {
        self.done
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl GeneratorIterator for VecIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }

    fn is_done(&self) -> bool {
        self.done
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl GeneratorIterator for FilterIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }

    fn is_done(&self) -> bool {
        self.done
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl GeneratorIterator for MapIter {
    fn clone_box(&self) -> Box<dyn GeneratorIterator> {
        Box::new(self.clone())
    }

    fn is_done(&self) -> bool {
        self.done
    }

    fn get_size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}
