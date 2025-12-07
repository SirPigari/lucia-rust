use std::sync::Arc;
use libloading::{Library, Symbol};
#[cfg(unix)]
use libloading::os::unix::{Library as UnixLibrary};
use libffi::middle::{Cif, CodePtr, Type, Arg};
use std::ffi::c_void;

use crate::env::runtime::value::Value;
use crate::env::runtime::utils::format_value;

use once_cell::sync::Lazy;
use parking_lot::Mutex;

static STORED_ARGS: Lazy<Mutex<Vec<StoredValue>>> = Lazy::new(|| Mutex::new(Vec::new()));

#[derive(Debug)]
enum StoredValue {
    Int(i64),
    Boolean(u8),
    Ptr(*const std::ffi::c_void),
    ByteVec(Vec<u8>),
    Float64(f64),
    Float32(f32),
    Null,
}

pub unsafe fn get_list(ptr: *const c_void, elem_type: ValueType, len: usize) -> Value {
    let mut result = Vec::new();
    match elem_type {
        ValueType::Int => {
            let int_ptr = ptr as *const i64;
            for i in 0..len {
                let val = unsafe { *int_ptr.add(i) };
                result.push(Value::Int(val.into()));
            }
        }
        ValueType::Float64 => {
            let float_ptr = ptr as *const f64;
            for i in 0..len {
                let val = unsafe { *float_ptr.add(i) };
                result.push(Value::Float(val.into()));
            }
        }
        ValueType::Float32 => {
            let float_ptr = ptr as *const f32;
            for i in 0..len {
                let val = unsafe { *float_ptr.add(i) };
                result.push(Value::Float((val as f64).into()));
            }
        }
        ValueType::Boolean => {
            let bool_ptr = ptr as *const u8;
            for i in 0..len {
                let val = unsafe { *bool_ptr.add(i) } != 0;
                result.push(Value::Boolean(val));
            }
        }
        ValueType::Ptr => {
            let ptr_ptr = ptr as *const *const c_void;
            for i in 0..len {
                let val = unsafe { *ptr_ptr.add(i) };
                result.push(Value::Pointer(Arc::new(Mutex::new((Value::Int((val as usize as i64).into()), 1)))));
            }
        }
        ValueType::Void => {
            for _ in 0..len {
                result.push(Value::Null);
            }
        }
        ValueType::Byte => {
            let byte_ptr = ptr as *const u8;
            for i in 0..len {
                let val = unsafe { *byte_ptr.add(i) };
                result.push(Value::Int((val as i64).into()));
            }
        }
    }
    Value::List(result)
}

unsafe impl Send for StoredValue {}
unsafe impl Sync for StoredValue {}

#[derive(Clone, Debug)]
pub enum ValueType {
    Int,
    Float32,
    Float64,
    Byte,
    Boolean,
    Ptr,
    Void,
}

pub struct LuciaLib {
    lib: Arc<Library>,
}

impl LuciaLib {
    pub unsafe fn load<P: AsRef<std::ffi::OsStr> + libloading::AsFilename>(path: P) -> Result<Self, String> {
        unsafe { Library::new(path) }
            .map(|lib| LuciaLib { lib: Arc::new(lib) })
            .map_err(|e| e.to_string())
    }

    #[cfg(unix)]
    pub unsafe fn load_flags<P: AsRef<std::ffi::OsStr> + libloading::AsFilename>(path: P, flags: i32) -> Result<Self, String> {
        unsafe { UnixLibrary::open(Some(path.as_ref()), flags) }
            .map(|lib| LuciaLib { lib: Arc::new(Library::from(lib)) })
            .map_err(|e| e.to_string())
    }

    pub unsafe fn get_function(
        &self,
        name: &str,
        arg_types: Vec<ValueType>,
        ret_type: ValueType,
    ) -> Result<LuciaFfiFn, String> {
        let symbol: Symbol<*const ()> = unsafe { self
            .lib
            .get(name.as_bytes())
            .map_err(|e| e.to_string())? };

        Ok(LuciaFfiFn {
            func_ptr: CodePtr::from_ptr(*symbol as *const c_void),
            arg_types,
            ret_type,
            _lib: self.lib.clone(),
        })
    }
}

pub struct LuciaFfiFn {
    func_ptr: CodePtr,
    arg_types: Vec<ValueType>,
    ret_type: ValueType,
    _lib: Arc<Library>,
}

impl LuciaFfiFn {
    pub unsafe fn call(&self, args: &[Value], stdcall: bool) -> Result<Value, String> {
        if stdcall {
            return Err("Stdcall is not supported in this implementation".to_string());
        }
        let ffi_arg_types: Vec<Type> = self.arg_types.iter().map(|t| match t {
            ValueType::Int => Type::i64(),
            ValueType::Float64 => Type::f64(),
            ValueType::Float32 => Type::f32(),
            ValueType::Boolean => Type::u8(),
            ValueType::Ptr => Type::pointer(),
            ValueType::Void => Type::void(),
            ValueType::Byte => Type::u8(),
        }).collect();

        let ffi_ret_type = match self.ret_type {
            ValueType::Int => Type::i64(),
            ValueType::Float64 => Type::f64(),
            ValueType::Float32 => Type::f32(),
            ValueType::Boolean => Type::u8(),
            ValueType::Byte => Type::u8(),
            ValueType::Ptr => Type::pointer(),
            ValueType::Void => Type::void(),
        };

        let cif = Cif::new(ffi_arg_types.clone(), ffi_ret_type);

        let mut stored_args = STORED_ARGS.lock();
        stored_args.clear();

        let ffi_args: Result<Vec<Arg>, String> = args.iter().zip(self.arg_types.iter()).enumerate().map(|(index, (v, t))| {
            match (v, t) {
                (Value::Int(i), ValueType::Int) => {
                    let val = i.to_i64().unwrap_or(0);
                    stored_args.push(StoredValue::Int(val));
                    if let StoredValue::Int(stored_val) = stored_args.last().unwrap() {
                        Ok(Arg::new(stored_val))
                    } else {
                        unreachable!()
                    }
                }
                (Value::Float(f), ValueType::Float64) => {
                    let val_f64 = match f.to_f64() {
                        Ok(v) => v,
                        Err(_) => f.to_str().parse::<f64>().unwrap_or(0.0),
                    };
                    stored_args.push(StoredValue::Float64(val_f64));
                    if let StoredValue::Float64(stored_val) = stored_args.last().unwrap() {
                        Ok(Arg::new(stored_val))
                    } else {
                        unreachable!()
                    }
                }
                (Value::Float(f), ValueType::Float32) => {
                    let val_f32 = match f.to_f64() {
                        Ok(v) => v as f32,
                        Err(_) => f.to_str().parse::<f32>().unwrap_or(0.0),
                    };
                    stored_args.push(StoredValue::Float32(val_f32));
                    if let StoredValue::Float32(stored_val) = stored_args.last().unwrap() {
                        Ok(Arg::new(stored_val))
                    } else {
                        unreachable!()
                    }
                }
                (Value::Boolean(b), ValueType::Boolean) => {
                    stored_args.push(StoredValue::Boolean(*b as u8));
                    if let StoredValue::Boolean(stored_val) = stored_args.last().unwrap() {
                        Ok(Arg::new(stored_val))
                    } else {
                        unreachable!()
                    }
                }
                (Value::Int(i), ValueType::Ptr) => {
                    let raw_ptr = i.to_i64().unwrap_or(0) as *const std::ffi::c_void;
                    stored_args.push(StoredValue::Ptr(raw_ptr));
                    if let StoredValue::Ptr(stored_val) = stored_args.last().unwrap() {
                        Ok(Arg::new(stored_val))
                    } else {
                        unreachable!()
                    }
                }
                (Value::Pointer(ptr_arc), ValueType::Ptr) => {
                    let val_ref = ptr_arc.lock();

                    if let (Value::Int(raw_val), _) = &*val_ref {
                        let raw_usize = raw_val.to_i64().unwrap_or(0) as usize;
                        if raw_usize == 0 {
                            return Err("Pointer is null".into());
                        }

                        let raw_ptr = raw_usize as *const std::ffi::c_void;
                        stored_args.push(StoredValue::Ptr(raw_ptr));
                        if let StoredValue::Ptr(stored_val) = stored_args.last().unwrap() {
                            Ok(Arg::new(stored_val))
                        } else {
                            unreachable!()
                        }
                    } else {
                        Err("Expected Value::Int inside pointer Arc".into())
                    }
                }
                (Value::Int(b), ValueType::Byte) => {
                    let val = b.to_i64().unwrap_or(0) as u8;

                    if let Some(StoredValue::ByteVec(vec)) = stored_args.last_mut() {
                        vec.push(val);
                        let val_ref = &vec[vec.len() - 1];
                        Ok(Arg::new(val_ref))
                    } else {
                        stored_args.push(StoredValue::ByteVec(vec![val]));
                        let val_ref = match stored_args.last().unwrap() {
                            StoredValue::ByteVec(vec) => &vec[0],
                            _ => unreachable!(),
                        };
                        Ok(Arg::new(val_ref))
                    }
                }
                (Value::Null, ValueType::Void) => Ok(Arg::new(&StoredValue::Null)),
                _ => Err(format!("Unsupported Value/ValueType combination for ffi call argument #{}, expected: {:?}, got: {}", 
                    index + 1,
                    t,
                    format_value(v))),
            }
        }).collect();


        let ffi_args = ffi_args?;

        let result = match self.ret_type {
            ValueType::Int => {
                let ret: i64 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Int(ret.into())
            }
            ValueType::Float64 => {
                let ret: f64 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Float(ret.into())
            }
            ValueType::Float32 => {
                let ret: f32 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Float((ret as f64).into())
            }
            ValueType::Byte => {
                let ret: u8 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Int((ret as i64).into())
            }
            ValueType::Boolean => {
                let ret: u8 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Boolean(ret != 0)
            }
            ValueType::Ptr => {
                let ret: *const () = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Pointer(Arc::new(Mutex::new((Value::Int((ret as usize as i64).into()), 1))))
            }
            ValueType::Void => {
                unsafe { cif.call::<()>(self.func_ptr, &ffi_args) };
                Value::Null
            }
        };

        Ok(result)
    }
}
