#ifndef LUCIA_H
#define LUCIA_H

/*
    Lucia Programming Language
        - by Markofwitch

    C Header File for C99 and later
    GPL-3.0 License - Copyright (c) 2025 Markofwitch
*/


// Lucia API is thread-safe
// All memory allocated by the API must be freed by the user using the provided free functions
// NEVER use free() on API-allocated memory - there is no guarantee it will work.
//
// Ownership rules:
//  - Owned (must free using lucia_free_*): LuciaValue, LuciaError, LuciaConfig, LuciaResult
//  - Borrowed (do NOT free): VALUE_POINTER inside LuciaValue (managed by Arc), strings returned by 
//        - lucia_value_string_ptr()
//        - lucia_value_debug()
//        - lucia_value_display()
//        - lucia_error_type()
//        - lucia_error_message()
//        - lucia_error_help()
//        - lucia_error_location()
//        - and any pointers returned by list/map get functions (managed by the backing LuciaValue)
//  - Stack-safe / static: BuildInfo itself can be returned by value or live on stack, strings inside are static
//
// IMPORTANT: VALUE_POINTER values ARE automatically freed by lucia_free_value() - the Arc reference count is decremented.
//            Do NOT manually free the pointer inside VALUE_POINTER values.
// IMPORTANT: Anything marked as "borrowed" must NOT be freed by the user.

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

#define LUCIA_API_VERSION 0x1200 // 0x(major)(minor)(02patch) - https://semver.org/
#define LUCIA_API_VERSION_STRING "1.2.0"

#define LUCIA_VERSION_MAJOR ((LUCIA_API_VERSION >> 12) & 0xF)
#define LUCIA_VERSION_MINOR (((LUCIA_API_VERSION) >> 8)  & 0xF)
#define LUCIA_VERSION_PATCH ((LUCIA_API_VERSION) & 0xFF)

// just to be sure its uint8_t
typedef uint8_t CBool;

#ifndef __bool_true_false_are_defined 
    #define false 0
    #define true 1
#endif

// LuciaValue NULL constant
#define LUCIA_NULL (LuciaValue){0}

typedef struct LuciaConfig {
    CBool moded;                             // whether to run in moded mode (aka allows modified stdlib or version)
    CBool debug;                             // if true, prints debug info to stdout
    const char* debug_mode;                  // debug mode level: "minimal", "normal", "full"
    CBool supports_color;                    // whether terminal supports color output
    CBool use_lucia_traceback;               // whether to use lucia traceback formatting
    CBool warnings;                          // whether to show warnings
    CBool allow_fetch;                       // whether to allow fetch API
    CBool allow_unsafe;                      // whether to allow unsafe operations
    CBool allow_inline_config;               // whether to allow inline config in code
    CBool disable_runtime_type_checking;     // whether to disable runtime type checking
    const char* home_dir;                    // path to home directory, or NULL for default
    const char* libs_paths[16];              // array of paths to search for libraries, limit to 16 for simplicity. NULL entries are ignored
    size_t stack_size;                       // stack size in bytes
    const char* version;                     // lucia version string
} LuciaConfig;

typedef uint8_t LuciaValueType;
enum LuciaValueType {
    VALUE_NULL = 0,
    VALUE_INT = 1,
    VALUE_FLOAT = 2,
    VALUE_STRING = 3,
    VALUE_BOOLEAN = 4,
    VALUE_LIST = 5,
    VALUE_MAP = 6,
    VALUE_BYTES = 7,
    VALUE_NATIVE = 8,
    VALUE_POINTER = 9,
    VALUE_UNSUPPORTED = 255,
};

typedef struct LuciaValue LuciaValue;

typedef union ValueData {
    int64_t int_v;              // integer value, if bigger than i64, return i64::MAX
    double float_v;             // float value, if bigger than f64, return f64::MAX
    CBool bool_v;               // boolean value
    const char* string_v;       // pointer to string data (UTF-8), null-terminated
    const uint8_t* bytes_ptr;   // pointer to bytes data
    const LuciaValue* list_ptr; // pointer to list data
    const LuciaValue* map_ptr;  // flattened array: [key0, value0, key1, value1, ...]
    void* native_func;          // pointer to native function (LuciaNativeFunc), void* to avoid including function pointer type in the union
    void* pointer;              // (advanced use) pointer to rusts Arc<Mutex<(Value, usize)>> where the Value is the rusts Value and usize is the depth
} ValueData;

struct LuciaValue {
    LuciaValueType tag;
    ValueData data;
    size_t length;
};

// value constructors
LuciaValue lucia_value_null(void);
LuciaValue lucia_value_int(int64_t v);
LuciaValue lucia_value_float(double v);
LuciaValue lucia_value_bool(CBool v);
LuciaValue lucia_value_string(const char* utf8); // owns result. Copies utf8 internally. Must lucia_free_value().
LuciaValue lucia_value_bytes(const uint8_t* data, size_t len); // owns result. Copies data internally. Must lucia_free_value().

// owns result. Copies the items array internally (shallow copy of LuciaValue structs). Must lucia_free_value().
LuciaValue lucia_value_list(const LuciaValue* items, size_t len);

// owns result. len = number of key-value pairs. entries must be [k0,v0,k1,v1,...] (len*2 elements). Must lucia_free_value().
LuciaValue lucia_value_map(const LuciaValue* entries, size_t len);

// utils
int lucia_value_cmp(LuciaValue a, LuciaValue b);                                 // compares two values. returns -1, 0, or 1
uint64_t lucia_value_hash(LuciaValue v);                                         // stable hash for use in hash maps
LuciaValueType lucia_value_get_type(LuciaValue v);                               // returns the type tag
const char* lucia_value_type_name(LuciaValueType t);                             // borrowed static string.
const char* lucia_value_string_ptr(LuciaValue v);                                // borrowed pointer into the LuciaValue's string.
CBool lucia_value_is_null(LuciaValue v);                                         // returns 1 if value is null, 0 otherwise

// cloning
LuciaValue lucia_value_clone(LuciaValue v);                                      // shallow clones a LuciaValue. Must lucia_free_value() the result.
LuciaValue lucia_value_deep_clone(LuciaValue v);                                 // deep clones a LuciaValue. Must lucia_free_value() the result.
CBool lucia_value_string_clone(LuciaValue v, const char** out, size_t* out_len); // owns *out. Allocates a new copy. Caller must free *out.

// validating
CBool lucia_value_is_valid(LuciaValue v);
CBool lucia_value_is_valid_ptr(const LuciaValue* v);
CBool lucia_value_is_truthy(LuciaValue v);

// borrowed from internal thread-local buffer. Valid until next call to lucia_value_debug.
const char* lucia_value_debug(LuciaValue v);

// borrowed from separate internal thread-local buffer. Valid until next call to lucia_value_display.
const char* lucia_value_display(LuciaValue v);

// map/list utils - all return borrowed pointers into the backing LuciaValue.
size_t lucia_list_len(LuciaValue list);
const LuciaValue* lucia_list_get(LuciaValue list, size_t index);                          // returns NULL if not a list or out of bounds
CBool lucia_list_try_get(LuciaValue list, size_t index, const LuciaValue** out);          // returns 1 on success, 0 on failure
size_t lucia_map_len(LuciaValue map);                                                   // returns number of key-value pairs
const LuciaValue* lucia_map_get(LuciaValue map, const LuciaValue* key);                   // returns NULL if not found
CBool lucia_map_try_get(LuciaValue map, const LuciaValue* key, const LuciaValue** out);   // returns 1 if found, 0 if not
const LuciaValue* lucia_map_get_cstr(LuciaValue map, const char* key);                    // key is converted to a string value internally (temporary, freed automatically). Returns borrowed pointer or NULL.
CBool lucia_map_try_get_cstr(LuciaValue map, const char* key, const LuciaValue** out);    // same as above but returns bool

// helpers to convert LuciaValue to native types (all borrowed)
int64_t value_as_int(LuciaValue v);
double value_as_float(LuciaValue v);
CBool value_as_bool(LuciaValue v);
const char* value_as_string(LuciaValue v);
const uint8_t* value_as_bytes_ptr(LuciaValue v);
size_t value_as_bytes_len(LuciaValue v);
const LuciaValue* value_as_list_ptr(LuciaValue v);
size_t value_as_list_len(LuciaValue v);
const LuciaValue* value_as_map_ptr(LuciaValue v);
size_t value_as_map_len(LuciaValue v);
void* value_as_pointer(LuciaValue v);

// try_value_as_* functions return true on success, false on failure
CBool try_value_as_int(LuciaValue v, int64_t* out);
CBool try_value_as_float(LuciaValue v, double* out);
CBool try_value_as_bool(LuciaValue v, CBool* out);
CBool try_value_as_string(LuciaValue v, const char** out, size_t* out_len);
CBool try_value_as_bytes(LuciaValue v, const uint8_t** out_ptr, size_t* out_len);
CBool try_value_as_list(LuciaValue v, const LuciaValue** out_ptr, size_t* out_len);
CBool try_value_as_map(LuciaValue v, const LuciaValue** out_ptr, size_t* out_len);
CBool try_value_as_pointer(LuciaValue v, void** out);

// internal arguments struct for value_as with args
typedef struct lucia__ValueAsArgs {
    CBool force;
    CBool cast;
} lucia__ValueAsArgs;

// internal function for value_as with args
CBool lucia__value_as_args(LuciaValue v, LuciaValueType t, void* out, lucia__ValueAsArgs args);

// value_as(LuciaValue v, LuciaValueType t, void* out, CBool .force = false, CBool .cast = false)
#define value_as(v, t, out, ...) \
    lucia__value_as_args(v, t, out, (lucia__ValueAsArgs){ __VA_ARGS__ })

typedef struct LuciaError {
    const char* err_type;
    const char* err_msg;
    const char* help_msg;
    uint32_t line_num;
    const char* line_text;
    size_t column;
} LuciaError;

typedef uint8_t LuciaResultTag;
enum LuciaResultTag {
    LUCIA_RESULT_OK = 1,         // contains LuciaValue
    LUCIA_RESULT_ERROR = 2,      // contains LuciaError
    LUCIA_RESULT_CONFIG_ERR = 3, // means config or code was invalid, empty
    LUCIA_RESULT_PANIC = 4,      // contains panic message
};

typedef union LuciaResultData {
    LuciaValue value;
    LuciaError error;
    const char* panic_msg;
} LuciaResultData;

typedef struct LuciaResult {
    LuciaResultTag tag;
    LuciaResultData data;
} LuciaResult;

CBool lucia_result_is_ok(const LuciaResult* res);
CBool lucia_result_is_error(const LuciaResult* res);
const LuciaValue* lucia_result_value(const LuciaResult* res);  // returns NULL if not value
const LuciaError* lucia_result_error(const LuciaResult* res);  // returns NULL if not error
CBool lucia_result_try_as_value(const LuciaResult* res, const LuciaValue** out); // returns true on value, false on error
CBool lucia_result_try_as_error(const LuciaResult* res, const LuciaError** out); // returns true on error, false on value
const char* lucia_result_display(const LuciaResult res); // borrowed pointer to a human-readable display string for the result. Valid until next call to lucia_result_display.
LuciaResult lucia_new_result_value(LuciaValue v);   // creates a LuciaResult with the given value. Must lucia_free_result() the result.
LuciaResult lucia_new_result_error(const char* err_type, const char* err_msg); // creates a LuciaResult with the given error. Must lucia_free_result() the result.

// error helpers - all take borrowed pointers to LuciaError
const char* lucia_error_display(const LuciaError* err); // borrowed pointer to a human-readable display string for the error. Valid until next call to lucia_error_display.
void lucia_error_print(const LuciaError* err, FILE* out); // prints formatted error to FILE*. No allocation.
const char* lucia_error_type(const LuciaError* err);
const char* lucia_error_message(const LuciaError* err);
const char* lucia_error_help(const LuciaError* err);
const char* lucia_error_location(const LuciaError* err); // borrowed from internal thread-local buffer. Valid until next call to lucia_error_location.

typedef struct LuciaVariables {
    const char** keys;        // array of keys (borrowed pointers into the backing LuciaValue)
    const LuciaValue* values; // array of values (borrowed pointers into the backing LuciaValue)
    size_t len;               // number of variables
    size_t capacity;          // capacity of the keys and values arrays
    CBool include_default;    // whether the variables include the default builtins
} LuciaVariables;

typedef struct LuciaArgs {
    const LuciaValue* values; // array of values
    size_t len;               // number of arguments
} LuciaArgs;

// Users has to validate LuciaArgs passed to the native function
// native functions are represented with lucia type 'native public mutable function[*any] -> any'
typedef LuciaResult (*LuciaNativeFunc)(const LuciaArgs* args);

// creates a new LuciaVariables with given capacity. User must free using lucia_free_vars().
LuciaVariables* lucia_variables_new(size_t capacity);
// creates a new LuciaVariables with the default variables of lucia (builtins). User must free using lucia_free_vars().
LuciaVariables* lucia_variables_new_default(void);
// inserts a key-value pair into the variables. Key is a borrowed pointer (must be valid until vars is freed), value is owned (will be freed when vars is freed).
void lucia_variables_insert(LuciaVariables* vars, const char* key, LuciaValue value);
// inserts a function into the variables. Key is a borrowed pointer (must be valid until vars is freed), func is a function pointer to a native function.
void lucia_variables_insert_function(LuciaVariables* vars, const char* key, LuciaNativeFunc func);
// clears all variables from the struct, freeing owned values but not keys. vars is still valid for use after this.
void lucia_variables_clear(LuciaVariables* vars);
 // removes a variable by key, freeing the owned value but not the key. Does nothing if key not found.
void lucia_variables_remove(LuciaVariables* vars, const char* key);
// gets a variable by key. Returns true if found, false if not found. *out is a borrowed pointer into the backing LuciaValue.
CBool lucia_variables_get(const LuciaVariables* vars, const char* key, const LuciaValue** out);
// if key not found, returns default_value (owned by caller, will not be freed by API)
LuciaValue lucia_variables_get_or_default(const LuciaVariables* vars, const char* key, LuciaValue default_value);
// frees the variables and all owned values. Does not free keys since they are borrowed pointers.
void lucia_variables_free(LuciaVariables* vars);

// gets an argument by key. Returns true if found, false if not found. *out is a borrowed pointer into the backing LuciaValue.
CBool lucia_args_get(const LuciaArgs* args, const LuciaValue** out, size_t index);
// if index is out of bounds, returns default_value (owned by caller, will not be freed by API)
LuciaValue lucia_args_get_or_default(const LuciaArgs* args, size_t index, LuciaValue default_value);
// gets the number of arguments in the LuciaArgs struct
size_t lucia_args_len(const LuciaArgs* args);

// BuildInfo is static, doesnt need freeing
typedef struct BuildInfo {
    const char* name;           // "lucia"
    const char* version;        // "2.x.y" (semver)
    const char* uuid;           // unique build uuid
    const char* rustc_version;  // rustc version string
    const char* rustc_channel;  // "stable", "beta", "nightly"
    const char* target;         // compilation target triple
    const char* repository;     // repository URL ("https://github.com/SirPigari/lucia-rust")
    const char* git_hash;       // git commit hash
    const char* file_hash;      // source file hash
    const char* profile;        // build profile ("debug" or "release")
    const char* build_date;     // build date in ISO 8601 (RFC 3339) format
} BuildInfo;

// functions to free allocated memory
void lucia_free_value(LuciaValue v);  // recursively frees LuciaValue
void lucia_free_error(LuciaError err);
void lucia_free_result(LuciaResult res);
void lucia_free_config(LuciaConfig cfg);

// Gets build info embedded in the binary
// Same as 'lucia --build-info' command
// Returns pointer to static data
const BuildInfo* lucia_get_build_info(void);

// Gets the default configuration
// User has to free the returned LuciaConfig struct after use
LuciaConfig lucia_default_config(void);

// Interrupts the currently running interpretation (if any). Safe to call from another thread.
void lucia_interrupt_current(void);
 // interrupts with a custom message that can be retrieved
void lucia_interrupt_current_with_message(const char* msg);

// Interprets Lucia code given as a string
// Returns a LuciaResult struct
// User has to free the returned LuciaResult struct after use
LuciaResult lucia_interpret(const char* code, const LuciaConfig* config);

// Same as lucia_interpret but with argv
LuciaResult lucia_interpret_with_argv(const char* code, const char** argv, size_t argc, const LuciaConfig* config);

// Interprets Lucia code given as a string
// Returns a LuciaResult struct
// User has to free the returned LuciaResult struct after use
// Modifies vars with the final variables after execution. User has to free vars after use.
LuciaResult lucia_interpret_with_vars(const char* code, const LuciaConfig* config, LuciaVariables* vars);

// Same as lucia_interpret_with_vars but with argv
LuciaResult lucia_interpret_with_vars_and_argv(const char* code, const LuciaConfig* config, LuciaVariables* vars, const char** argv, size_t argc);


#endif // LUCIA_H
