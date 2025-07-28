use std::sync::Arc;
use libloading::{Library, Symbol};
use libffi::middle::{Cif, CodePtr, Type, Arg};
use std::ffi::c_void;

use crate::env::runtime::value::Value;

#[derive(Clone, Debug)]
pub enum ValueType {
    Int,
    Float,
    Boolean,
    Ptr,
}

pub struct LuciaLib {
    lib: Arc<Library>,
}

impl LuciaLib {
    pub unsafe fn load<P: AsRef<std::ffi::OsStr>>(path: P) -> Result<Self, String> {
        unsafe { Library::new(path) }
            .map(|lib| LuciaLib { lib: Arc::new(lib) })
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
            ValueType::Float => Type::f64(),
            ValueType::Boolean => Type::u8(),
            ValueType::Ptr => Type::pointer(),
        }).collect();

        let ffi_ret_type = match self.ret_type {
            ValueType::Int => Type::i64(),
            ValueType::Float => Type::f64(),
            ValueType::Boolean => Type::u8(),
            ValueType::Ptr => Type::pointer(),
        };

        let cif = Cif::new(ffi_arg_types.clone(), ffi_ret_type);

        let mut ptrs: Vec<*const c_void> = vec![];

        let ffi_args: Result<Vec<Arg>, String> = args.iter().zip(self.arg_types.iter()).map(|(v, t)| {
            match (v, t) {
                (Value::Int(i), ValueType::Int) => Ok(Arg::new(&i.to_i64().unwrap_or(0))),
                (Value::Float(f), ValueType::Float) => Ok(Arg::new(&f.to_f64().unwrap_or(0.0))),
                (Value::Boolean(b), ValueType::Boolean) => Ok(Arg::new(b)),
                (Value::Int(i), ValueType::Ptr) => {
                    let raw_ptr = i.to_i64().unwrap_or(0) as *const c_void;
                    Ok(Arg::new(&raw_ptr))
                }
                (Value::Pointer(ptr_arc), ValueType::Ptr) => {
                    let val_ref = ptr_arc.as_ref();

                    if let Value::Int(raw_val) = val_ref {
                        let raw_usize = raw_val.to_i64().unwrap_or(0) as usize;
                        if raw_usize == 0 {
                            return Err("Pointer is null".into());
                        }

                        let raw_ptr = raw_usize as *const c_void;

                        ptrs.push(raw_ptr);

                        Ok(Arg::new(ptrs.last().unwrap()))
                    } else {
                        Err("Expected Value::Int inside pointer Arc".into())
                    }
                }
                _ => Err("Unsupported Value/ValueType combination for ffi call".to_string()),
            }
        }).collect();

        let ffi_args = ffi_args?;

        let result = match self.ret_type {
            ValueType::Int => {
                let ret: i64 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Int(ret.into())
            }
            ValueType::Float => {
                let ret: f64 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Float(ret.into())
            }
            ValueType::Boolean => {
                let ret: u8 = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Boolean(ret != 0)
            }
            ValueType::Ptr => {
                let ret: *const () = unsafe { cif.call(self.func_ptr, &ffi_args) };
                Value::Pointer(Arc::new(Value::Int((ret as usize as i64).into())))
            }
        };

        Ok(result)
    }
}
