#ifndef LUCIA_H
#define LUCIA_H

/*
    Lucia Programming Language
        - by Markofwitch

    C Header File
*/


#include <stdint.h>
#include <stddef.h>

// alias for u8 as boolean
typedef uint8_t CBool;

typedef struct LuciaColorScheme {
    const char* exception;
    const char* warning;
    const char* help;
    const char* debug;
    const char* input_arrows;
    const char* note;
    const char* output_text;
    const char* info;
} LuciaColorScheme;

typedef struct LuciaConfig {
    CBool moded;
    CBool debug;
    const char* debug_mode;
    CBool supports_color;
    CBool use_lucia_traceback;
    CBool warnings;
    CBool allow_fetch;
    CBool allow_unsafe;
    CBool allow_inline_config;
    CBool disable_runtime_type_checking;
    const char* home_dir;
    size_t stack_size;
    const char* version;
    LuciaColorScheme color_scheme;
} LuciaConfig;

typedef enum LuciaValueTag {
    VALUE_INT = 1,
    VALUE_FLOAT = 2,
    VALUE_STRING = 3,
    VALUE_BOOLEAN = 4,
    VALUE_NULL = 5,
    VALUE_LIST = 6,
    VALUE_MAP = 7,
    VALUE_BYTES = 8,
    VALUE_POINTER = 9,
    VALUE_UNSUPPORTED = 255,
} LuciaValueTag;

typedef struct LuciaValue LuciaValue;

typedef union ValueData {
    int64_t int_v;
    double float_v;
    CBool bool_v;
    const char* string_v;
    const uint8_t* bytes_ptr;
    const LuciaValue* list_ptr;
    const LuciaValue* map_ptr; // flattened key,value,key,value
    void* pointer;
} ValueData;

struct LuciaValue {
    LuciaValueTag tag;
    ValueData data;
    size_t length; // used for lists, bytes, maps
};

// helpers to convert LuciaValue to native types
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

typedef struct LuciaError {
    const char* err_type;
    const char* err_msg;
    const char* help_msg;
    uint32_t line_num;
    const char* line_text;
    size_t column;
} LuciaError;


// Result type
// RESULT_OK: contains LuciaValue
// RESULT_ERROR: contains LuciaError
// RESULT_CONFIG_ERR: means config or code was invalid, empty
// RESULT_PANIC: contains panic message
typedef enum LuciaResultTag {
    LUCIA_RESULT_OK = 1,
    LUCIA_RESULT_ERROR = 2,
    LUCIA_RESULT_CONFIG_ERR = 3,
    LUCIA_RESULT_PANIC = 4,
} LuciaResultTag;

typedef union LuciaResultData {
    LuciaValue value;
    LuciaError error;
    const char* panic_msg;
} LuciaResultData;

typedef struct LuciaResult {
    LuciaResultTag tag;
    LuciaResultData data;
} LuciaResult;

typedef struct BuildInfo {
    const char* name;
    const char* version;
    const char* uuid;
    const char* rustc_version;
    const char* rustc_channel;
    const char* target;
    const char* repository;
    const char* git_hash;
    const char* file_hash;
    const char* profile;
    const char* build_date;
} BuildInfo;

// functions to free allocated memory
void lucia_free_value(LuciaValue v);
void lucia_free_error(LuciaError err);
void lucia_free_config(LuciaConfig cfg);
void lucia_free_result(LuciaResult res);
void lucia_free_build_info(BuildInfo info);

// Gets build info embedded in the binary
// Same as 'lucia -i' command
// User has to free the returned BuildInfo struct after use
BuildInfo lucia_get_build_info(void);

// Gets the default configuration
// User has to free the returned LuciaConfig struct after use
LuciaConfig lucia_default_config(void);

// Interprets Lucia code given as a string
// Returns a LuciaResult struct
// User has to free the returned LuciaResult struct after use
LuciaResult lucia_interpret(const char* code, const LuciaConfig* config);

// Same as lucia_interpret but with argv
LuciaResult lucia_interpret_with_argv(const char* code, const char** argv, size_t argc, const LuciaConfig* config);

#endif // LUCIA_H
