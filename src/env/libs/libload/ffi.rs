use std::sync::Arc;
use libloading::{Library, Symbol};
#[cfg(unix)]
use libloading::os::unix::Library as UnixLibrary;
use libffi::middle::{Cif, CodePtr, Type, Arg};
use std::ffi::c_void;

use crate::env::runtime::value::Value;

use once_cell::sync::Lazy;
use parking_lot::Mutex;

static STORED_ARGS: Lazy<Mutex<Vec<StoredValue>>> = Lazy::new(|| Mutex::new(Vec::with_capacity(16)));

#[derive(Debug)]
enum StoredValue {
    Char(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    LongLong(i64),
    UChar(u8),
    UShort(u16),
    UInt(u32),
    ULong(u64),
    ULongLong(u64),
    Float(f32),
    Double(f64),
    Bool(u8),
    Ptr(*const c_void),
    Null,
}

unsafe impl Send for StoredValue {}
unsafe impl Sync for StoredValue {}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum ValueType {
    Char,          // i8
    Short,         // i16
    Int,           // i32
    Long,          // i64
    LongLong,      // i64
    UChar,         // u8
    UShort,        // u16
    UInt,          // u32
    ULong,         // u64
    ULongLong,     // u64
    Float,         // f32
    Double,        // f64
    Bool,          // u8
    Ptr,           // pointer
    Void,          // void
}

impl ValueType {
    #[inline(always)]
    fn to_ffi_type(&self) -> Type {
        match self {
            ValueType::Char => Type::i8(),
            ValueType::Short => Type::i16(),
            ValueType::Int => Type::i32(),
            ValueType::Long => Type::i64(),
            ValueType::LongLong => Type::i64(),
            ValueType::UChar => Type::u8(),
            ValueType::UShort => Type::u16(),
            ValueType::UInt => Type::u32(),
            ValueType::ULong => Type::u64(),
            ValueType::ULongLong => Type::u64(),
            ValueType::Float => Type::f32(),
            ValueType::Double => Type::f64(),
            ValueType::Bool => Type::u8(),
            ValueType::Ptr => Type::pointer(),
            ValueType::Void => Type::void(),
        }
    }
}

pub unsafe fn get_list(ptr: *const c_void, elem_type: ValueType, len: usize) -> Value {
    let mut result = Vec::with_capacity(len);
    
    match elem_type {
        ValueType::Char => {
            let p = ptr as *const i8;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::Short => {
            let p = ptr as *const i16;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::Int => {
            let p = ptr as *const i32;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::Long | ValueType::LongLong => {
            let p = ptr as *const i64;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) }.into()));
            }
        }
        ValueType::UChar => {
            let p = ptr as *const u8;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::UShort => {
            let p = ptr as *const u16;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::UInt => {
            let p = ptr as *const u32;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::ULong | ValueType::ULongLong => {
            let p = ptr as *const u64;
            for i in 0..len {
                result.push(Value::Int(unsafe { *p.add(i) as i64 }.into()));
            }
        }
        ValueType::Float => {
            let p = ptr as *const f32;
            for i in 0..len {
                result.push(Value::Float(unsafe { *p.add(i) as f64 }.into()));
            }
        }
        ValueType::Double => {
            let p = ptr as *const f64;
            for i in 0..len {
                result.push(Value::Float(unsafe { *p.add(i) }.into()));
            }
        }
        ValueType::Bool => {
            let p = ptr as *const u8;
            for i in 0..len {
                result.push(Value::Boolean(unsafe { *p.add(i) } != 0));
            }
        }
        ValueType::Ptr => {
            let p = ptr as *const *const c_void;
            for i in 0..len {
                let val = unsafe { *p.add(i) };
                result.push(Value::Pointer(Arc::new(Mutex::new((
                    Value::Int((val as usize as i64).into()),
                    1,
                )))));
            }
        }
        ValueType::Void => {
            result.resize(len, Value::Null);
        }
    }
    
    Value::List(result)
}

pub struct LuciaLib {
    lib: Arc<Library>,
}

impl LuciaLib {
    #[inline]
    pub unsafe fn load<P: AsRef<std::ffi::OsStr> + libloading::AsFilename>(path: P) -> Result<Self, String> {
        unsafe { Library::new(path) }
            .map(|lib| LuciaLib { lib: Arc::new(lib) })
            .map_err(|e| e.to_string())
    }

    #[cfg(unix)]
    #[inline]
    pub unsafe fn load_flags<P: AsRef<std::ffi::OsStr> + libloading::AsFilename>(
        path: P,
        flags: i32,
    ) -> Result<Self, String> {
        unsafe { UnixLibrary::open(Some(path.as_ref()), flags) }
            .map(|lib| LuciaLib { lib: Arc::new(Library::from(lib)) })
            .map_err(|e| e.to_string())
    }

    #[inline]
    pub unsafe fn get_function(
        &self,
        name: &str,
        arg_types: Vec<ValueType>,
        ret_type: ValueType,
    ) -> Result<LuciaFfiFn, String> {
        let symbol: Symbol<*const ()> = unsafe {
            self.lib
                .get(name.as_bytes())
                .map_err(|e| e.to_string())?
        };

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
    #[inline]
    pub unsafe fn call(&self, args: &[Value], stdcall: bool) -> Result<Value, String> {
        if stdcall {
            return Err("Stdcall is not supported in this implementation".to_string());
        }

        let ffi_arg_types: Vec<Type> = self.arg_types.iter().map(|t| t.to_ffi_type()).collect();
        let ffi_ret_type = self.ret_type.to_ffi_type();

        let cif = Cif::new(ffi_arg_types, ffi_ret_type);

        let mut stored_args = STORED_ARGS.lock();
        stored_args.clear();
        stored_args.reserve(args.len());
        
        let ffi_args: Result<Vec<Arg>, String> = args
            .iter()
            .zip(self.arg_types.iter())
            .enumerate()
            .map(|(idx, (v, t))| self.convert_arg(v, t, idx, &mut stored_args))
            .collect();

        let ffi_args = ffi_args?;

        let result = self.call_and_convert(cif, &ffi_args);

        Ok(result)
    }

    #[inline(always)]
    fn convert_arg(
        &self,
        v: &Value,
        t: &ValueType,
        idx: usize,
        stored: &mut Vec<StoredValue>,
    ) -> Result<Arg, String> {
        macro_rules! store_int {
            ($variant:ident, $rust_type:ty) => {{
                let val = match v {
                    Value::Int(i) => TryInto::<$rust_type>::try_into(i.to_i128().unwrap_or(0)).unwrap_or(0),
                    _ => return Err(format!("Expected Int for {:?} at arg #{}, got {}", t, idx + 1, v.get_type().display_simple())),
                };
                stored.push(StoredValue::$variant(val));
                if let StoredValue::$variant(stored_val) = stored.last().unwrap() {
                    Ok(Arg::new(stored_val))
                } else {
                    unreachable!()
                }
            }};
        }

        match t {
            ValueType::Char => store_int!(Char, i8),
            ValueType::Short => store_int!(Short, i16),
            ValueType::Int => store_int!(Int, i32),
            ValueType::Long => store_int!(Long, i64),
            ValueType::LongLong => store_int!(LongLong, i64),
            ValueType::UChar => store_int!(UChar, u8),
            ValueType::UShort => store_int!(UShort, u16),
            ValueType::UInt => store_int!(UInt, u32),
            ValueType::ULong => store_int!(ULong, u64),
            ValueType::ULongLong => store_int!(ULongLong, u64),
            
            ValueType::Float => {
                let val = match v {
                    Value::Float(f) => match f.to_f64() {
                        Ok(v) => v as f32,
                        Err(_) => f.to_str().parse::<f32>().unwrap_or(0.0),
                    },
                    _ => return Err(format!("Expected Float for Float at arg #{}, got {}", idx + 1, v.get_type().display_simple())),
                };
                stored.push(StoredValue::Float(val));
                if let StoredValue::Float(stored_val) = stored.last().unwrap() {
                    Ok(Arg::new(stored_val))
                } else {
                    unreachable!()
                }
            }
            
            ValueType::Double => {
                let val = match v {
                    Value::Float(f) => match f.to_f64() {
                        Ok(v) => v,
                        Err(_) => f.to_str().parse::<f64>().unwrap_or(0.0),
                    },
                    _ => return Err(format!("Expected Float for Double at arg #{}, got {}", idx + 1, v.get_type().display_simple())),
                };
                stored.push(StoredValue::Double(val));
                if let StoredValue::Double(stored_val) = stored.last().unwrap() {
                    Ok(Arg::new(stored_val))
                } else {
                    unreachable!()
                }
            }
            
            ValueType::Bool => {
                let val = match v {
                    Value::Boolean(b) => *b as u8,
                    _ => return Err(format!("Expected Boolean for Bool at arg #{}, got {}", idx + 1, v.get_type().display_simple())),
                };
                stored.push(StoredValue::Bool(val));
                if let StoredValue::Bool(stored_val) = stored.last().unwrap() {
                    Ok(Arg::new(stored_val))
                } else {
                    unreachable!()
                }
            }
            
            ValueType::Ptr => match v {
                Value::Int(i) => {
                    let raw_ptr = i.to_i64().unwrap_or(0) as *const c_void;
                    stored.push(StoredValue::Ptr(raw_ptr));
                    if let StoredValue::Ptr(stored_val) = stored.last().unwrap() {
                        Ok(Arg::new(stored_val))
                    } else {
                        unreachable!()
                    }
                }
                Value::Pointer(ptr_arc) => {
                    let val_ref = ptr_arc.lock();
                    if let (Value::Int(raw_val), _) = &*val_ref {
                        let raw_usize = raw_val.to_i64().unwrap_or(0) as usize;
                        if raw_usize == 0 {
                            return Err("Pointer is null".into());
                        }
                        let raw_ptr = raw_usize as *const c_void;
                        stored.push(StoredValue::Ptr(raw_ptr));
                        if let StoredValue::Ptr(stored_val) = stored.last().unwrap() {
                            Ok(Arg::new(stored_val))
                        } else {
                            unreachable!()
                        }
                    } else {
                        Err("Expected Value::Int inside pointer Arc".into())
                    }
                }
                _ => Err(format!("Expected Int or Pointer for Ptr at arg #{}, got {}", idx + 1, v.get_type().display_simple())),
            },
            
            ValueType::Void => Ok(Arg::new(&StoredValue::Null)),
        }
    }

    #[inline(always)]
    fn call_and_convert(&self, cif: Cif, ffi_args: &[Arg]) -> Value {
        unsafe {
            match self.ret_type {
                ValueType::Char => Value::Int((cif.call::<i8>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::Short => Value::Int((cif.call::<i16>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::Int => Value::Int((cif.call::<i32>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::Long => Value::Int(cif.call::<i64>(self.func_ptr, ffi_args).into()),
                ValueType::LongLong => Value::Int(cif.call::<i64>(self.func_ptr, ffi_args).into()),
                ValueType::UChar => Value::Int((cif.call::<u8>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::UShort => Value::Int((cif.call::<u16>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::UInt => Value::Int((cif.call::<u32>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::ULong => Value::Int((cif.call::<u64>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::ULongLong => Value::Int((cif.call::<u64>(self.func_ptr, ffi_args) as i64).into()),
                ValueType::Float => Value::Float((cif.call::<f32>(self.func_ptr, ffi_args) as f64).into()),
                ValueType::Double => Value::Float(cif.call::<f64>(self.func_ptr, ffi_args).into()),
                ValueType::Bool => Value::Boolean(cif.call::<u8>(self.func_ptr, ffi_args) != 0),
                ValueType::Ptr => {
                    let ret: *const () = cif.call(self.func_ptr, ffi_args);
                    Value::Pointer(Arc::new(Mutex::new((
                        Value::Int((ret as usize as i64).into()),
                        1,
                    ))))
                }
                ValueType::Void => {
                    cif.call::<()>(self.func_ptr, ffi_args);
                    Value::Null
                }
            }
        }
    }
}