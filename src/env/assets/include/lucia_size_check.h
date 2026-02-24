#ifndef LUCIA_SIZE_CHECK_H
#define LUCIA_SIZE_CHECK_H

#include "lucia.h"

// Static asserts to check sizes of types at compile time

void lucia_print_size_checks(FILE* out);

// #ifdef _MSC_VER
//     #error "Fuck you Microslop"
// #endif

#define ValueAsArgs lucia__ValueAsArgs
#define InterruptArgs lucia__InterruptArgs

#ifndef LUCIA_STATIC_ASSERT
    #if (defined(__cplusplus) && __cplusplus >= 201103L) || (defined(_MSC_VER) && _MSC_VER >= 1600)
        #define LUCIA_STATIC_ASSERT(expr, msg) static_assert(expr, msg)
    #elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
        #define LUCIA_STATIC_ASSERT(expr, msg) _Static_assert(expr, msg)
    #else
        #define A(x, y) x##y
        #define B(x, y) A(x, y)
        #define LUCIA_STATIC_ASSERT(expr, msg) typedef char B(static_assertion_, __LINE__)[(expr)?1:-1]
    #endif
#endif

#ifndef LUCIA_ALIGNOF
    #if defined(__cplusplus) && __cplusplus >= 201103L
        #define alignof(type) ::alignof(type)
    #elif defined(_MSC_VER)
        #if _MSC_VER >= 1920
            #define alignof(type) __alignof(type)
        #else
            #define alignof(type) __alignof(type)
        #endif
    #elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
        #include <stdalign.h>
        #define alignof(type) _Alignof(type)
    #elif defined(__GNUC__) || defined(__clang__)
        #define alignof(type) __alignof__(type)
    #else
        #include <stddef.h>
        #define alignof(type) offsetof(struct { char c; type member; }, member)
    #endif
#else
    #define alignof(type) LUCIA_ALIGNOF(type)
#endif

LUCIA_STATIC_ASSERT(sizeof(CBool) == 1, "Size of CBool was expected to be 1 byte");
LUCIA_STATIC_ASSERT(alignof(CBool) == 1, "Alignment of CBool was expected to be 1");
LUCIA_STATIC_ASSERT(sizeof(LuciaValueType) == 1, "Size of LuciaValueType was expected to be 1 byte");
LUCIA_STATIC_ASSERT(alignof(LuciaValueType) == 1, "Alignment of LuciaValueType was expected to be 1");
LUCIA_STATIC_ASSERT(sizeof(LuciaResultTag) == 1, "Size of LuciaResultTag was expected to be 1 byte");
LUCIA_STATIC_ASSERT(alignof(LuciaResultTag) == 1, "Alignment of LuciaResultTag was expected to be 1");
LUCIA_STATIC_ASSERT(sizeof(ValueData) == 8, "Size of ValueData was expected to be 8 bytes");
LUCIA_STATIC_ASSERT(alignof(ValueData) == 8, "Alignment of ValueData was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaResultData) == 48, "Size of LuciaResultData was expected to be 48 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaResultData) == 8, "Alignment of LuciaResultData was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(BuildInfo) == 88, "Size of BuildInfo was expected to be 88 bytes");
LUCIA_STATIC_ASSERT(alignof(BuildInfo) == 8, "Alignment of BuildInfo was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaConfig) == 176, "Size of LuciaConfig was expected to be 176 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaConfig) == 8, "Alignment of LuciaConfig was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaValue) == 24, "Size of LuciaValue was expected to be 24 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaValue) == 8, "Alignment of LuciaValue was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaError) == 48, "Size of LuciaError was expected to be 48 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaError) == 8, "Alignment of LuciaError was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaResult) == 56, "Size of LuciaResult was expected to be 56 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaResult) == 8, "Alignment of LuciaResult was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaVariables) == 40, "Size of LuciaVariables was expected to be 40 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaVariables) == 8, "Alignment of LuciaVariables was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(LuciaArgs) == 16, "Size of LuciaArgs was expected to be 16 bytes");
LUCIA_STATIC_ASSERT(alignof(LuciaArgs) == 8, "Alignment of LuciaArgs was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(ValueAsArgs) == 2, "Size of ValueAsArgs was expected to be 2 bytes");
LUCIA_STATIC_ASSERT(alignof(ValueAsArgs) == 1, "Alignment of ValueAsArgs was expected to be 1");
LUCIA_STATIC_ASSERT(sizeof(InterruptArgs) == 32, "Size of InterruptArgs was expected to be 32 bytes");
LUCIA_STATIC_ASSERT(alignof(InterruptArgs) == 8, "Alignment of InterruptArgs was expected to be 8");
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->int_v) == 8, "ValueData.int_v was expected to be 8 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->int_v) == 8, "ValueData.int_v alignment was expected to be 8");
#endif
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->float_v) == 8, "ValueData.float_v was expected to be 8 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->float_v) == 8, "ValueData.float_v alignment was expected to be 8");
#endif
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->bool_v) == 1, "ValueData.bool_v was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->bool_v) == 1, "ValueData.bool_v alignment was expected to be 1");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->string_v) == 8, "ValueData.string_v expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->string_v) == 8, "ValueData.string_v alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->string_v) == 4, "ValueData.string_v expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->string_v) == 4, "ValueData.string_v alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->bytes_ptr) == 8, "ValueData.bytes_ptr expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->bytes_ptr) == 8, "ValueData.bytes_ptr alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->bytes_ptr) == 4, "ValueData.bytes_ptr expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->bytes_ptr) == 4, "ValueData.bytes_ptr alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->list_ptr) == 8, "ValueData.list_ptr expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->list_ptr) == 8, "ValueData.list_ptr alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->list_ptr) == 4, "ValueData.list_ptr expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->list_ptr) == 4, "ValueData.list_ptr alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->map_ptr) == 8, "ValueData.map_ptr expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->map_ptr) == 8, "ValueData.map_ptr alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->map_ptr) == 4, "ValueData.map_ptr expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->map_ptr) == 4, "ValueData.map_ptr alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->pointer) == 8, "ValueData.pointer expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->pointer) == 8, "ValueData.pointer alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((ValueData*)0)->pointer) == 4, "ValueData.pointer expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueData*)0)->pointer) == 4, "ValueData.pointer alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaResultData*)0)->value) == 24, "LuciaResultData.value was expected to be 24 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaResultData*)0)->value) == 8, "LuciaResultData.value alignment was expected to be 8");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaResultData*)0)->error) == 48, "LuciaResultData.error was expected to be 48 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaResultData*)0)->error) == 8, "LuciaResultData.error alignment was expected to be 8");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaResultData*)0)->panic_msg) == 8, "LuciaResultData.panic_msg expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaResultData*)0)->panic_msg) == 8, "LuciaResultData.panic_msg alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaResultData*)0)->panic_msg) == 4, "LuciaResultData.panic_msg expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaResultData*)0)->panic_msg) == 4, "LuciaResultData.panic_msg alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->name) == 8, "BuildInfo.name expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->name) == 8, "BuildInfo.name alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->name) == 4, "BuildInfo.name expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->name) == 4, "BuildInfo.name alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->version) == 8, "BuildInfo.version expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->version) == 8, "BuildInfo.version alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->version) == 4, "BuildInfo.version expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->version) == 4, "BuildInfo.version alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->uuid) == 8, "BuildInfo.uuid expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->uuid) == 8, "BuildInfo.uuid alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->uuid) == 4, "BuildInfo.uuid expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->uuid) == 4, "BuildInfo.uuid alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->rustc_version) == 8, "BuildInfo.rustc_version expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->rustc_version) == 8, "BuildInfo.rustc_version alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->rustc_version) == 4, "BuildInfo.rustc_version expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->rustc_version) == 4, "BuildInfo.rustc_version alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->rustc_channel) == 8, "BuildInfo.rustc_channel expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->rustc_channel) == 8, "BuildInfo.rustc_channel alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->rustc_channel) == 4, "BuildInfo.rustc_channel expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->rustc_channel) == 4, "BuildInfo.rustc_channel alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->target) == 8, "BuildInfo.target expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->target) == 8, "BuildInfo.target alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->target) == 4, "BuildInfo.target expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->target) == 4, "BuildInfo.target alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->repository) == 8, "BuildInfo.repository expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->repository) == 8, "BuildInfo.repository alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->repository) == 4, "BuildInfo.repository expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->repository) == 4, "BuildInfo.repository alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->git_hash) == 8, "BuildInfo.git_hash expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->git_hash) == 8, "BuildInfo.git_hash alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->git_hash) == 4, "BuildInfo.git_hash expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->git_hash) == 4, "BuildInfo.git_hash alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->file_hash) == 8, "BuildInfo.file_hash expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->file_hash) == 8, "BuildInfo.file_hash alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->file_hash) == 4, "BuildInfo.file_hash expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->file_hash) == 4, "BuildInfo.file_hash alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->profile) == 8, "BuildInfo.profile expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->profile) == 8, "BuildInfo.profile alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->profile) == 4, "BuildInfo.profile expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->profile) == 4, "BuildInfo.profile alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->build_date) == 8, "BuildInfo.build_date expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->build_date) == 8, "BuildInfo.build_date alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((BuildInfo*)0)->build_date) == 4, "BuildInfo.build_date expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((BuildInfo*)0)->build_date) == 4, "BuildInfo.build_date alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->moded) == 1, "LuciaConfig.moded was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->moded) == 1, "LuciaConfig.moded alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->debug) == 1, "LuciaConfig.debug was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->debug) == 1, "LuciaConfig.debug alignment was expected to be 1");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->debug_mode) == 8, "LuciaConfig.debug_mode expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->debug_mode) == 8, "LuciaConfig.debug_mode alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->debug_mode) == 4, "LuciaConfig.debug_mode expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->debug_mode) == 4, "LuciaConfig.debug_mode alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->supports_color) == 1, "LuciaConfig.supports_color was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->supports_color) == 1, "LuciaConfig.supports_color alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->use_lucia_traceback) == 1, "LuciaConfig.use_lucia_traceback was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->use_lucia_traceback) == 1, "LuciaConfig.use_lucia_traceback alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->warnings) == 1, "LuciaConfig.warnings was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->warnings) == 1, "LuciaConfig.warnings alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->allow_fetch) == 1, "LuciaConfig.allow_fetch was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->allow_fetch) == 1, "LuciaConfig.allow_fetch alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->allow_unsafe) == 1, "LuciaConfig.allow_unsafe was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->allow_unsafe) == 1, "LuciaConfig.allow_unsafe alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->allow_inline_config) == 1, "LuciaConfig.allow_inline_config was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->allow_inline_config) == 1, "LuciaConfig.allow_inline_config alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->disable_runtime_type_checking) == 1, "LuciaConfig.disable_runtime_type_checking was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->disable_runtime_type_checking) == 1, "LuciaConfig.disable_runtime_type_checking alignment was expected to be 1");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->home_dir) == 8, "LuciaConfig.home_dir expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->home_dir) == 8, "LuciaConfig.home_dir alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->home_dir) == 4, "LuciaConfig.home_dir expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->home_dir) == 4, "LuciaConfig.home_dir alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->libs_paths) == 128, "LuciaConfig.libs_paths was expected to be 128 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->libs_paths) == 8, "LuciaConfig.libs_paths alignment was expected to be 8");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->stack_size) == 8, "LuciaConfig.stack_size expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->stack_size) == 8, "LuciaConfig.stack_size alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->stack_size) == 4, "LuciaConfig.stack_size expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->stack_size) == 4, "LuciaConfig.stack_size alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->version) == 8, "LuciaConfig.version expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->version) == 8, "LuciaConfig.version alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaConfig*)0)->version) == 4, "LuciaConfig.version expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaConfig*)0)->version) == 4, "LuciaConfig.version alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaValue*)0)->tag) == 1, "LuciaValue.tag was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaValue*)0)->tag) == 1, "LuciaValue.tag alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaValue*)0)->data) == 8, "LuciaValue.data was expected to be 8 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaValue*)0)->data) == 8, "LuciaValue.data alignment was expected to be 8");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaValue*)0)->length) == 8, "LuciaValue.length expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaValue*)0)->length) == 8, "LuciaValue.length alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaValue*)0)->length) == 4, "LuciaValue.length expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaValue*)0)->length) == 4, "LuciaValue.length alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->err_type) == 8, "LuciaError.err_type expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->err_type) == 8, "LuciaError.err_type alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->err_type) == 4, "LuciaError.err_type expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->err_type) == 4, "LuciaError.err_type alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->err_msg) == 8, "LuciaError.err_msg expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->err_msg) == 8, "LuciaError.err_msg alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->err_msg) == 4, "LuciaError.err_msg expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->err_msg) == 4, "LuciaError.err_msg alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->help_msg) == 8, "LuciaError.help_msg expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->help_msg) == 8, "LuciaError.help_msg alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->help_msg) == 4, "LuciaError.help_msg expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->help_msg) == 4, "LuciaError.help_msg alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->line_num) == 4, "LuciaError.line_num was expected to be 4 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->line_num) == 4, "LuciaError.line_num alignment was expected to be 4");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->line_text) == 8, "LuciaError.line_text expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->line_text) == 8, "LuciaError.line_text alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->line_text) == 4, "LuciaError.line_text expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->line_text) == 4, "LuciaError.line_text alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->column) == 8, "LuciaError.column expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->column) == 8, "LuciaError.column alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaError*)0)->column) == 4, "LuciaError.column expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaError*)0)->column) == 4, "LuciaError.column alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaResult*)0)->tag) == 1, "LuciaResult.tag was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaResult*)0)->tag) == 1, "LuciaResult.tag alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaResult*)0)->data) == 48, "LuciaResult.data was expected to be 48 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaResult*)0)->data) == 8, "LuciaResult.data alignment was expected to be 8");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->keys) == 8, "LuciaVariables.keys expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->keys) == 8, "LuciaVariables.keys alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->keys) == 4, "LuciaVariables.keys expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->keys) == 4, "LuciaVariables.keys alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->values) == 8, "LuciaVariables.values expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->values) == 8, "LuciaVariables.values alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->values) == 4, "LuciaVariables.values expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->values) == 4, "LuciaVariables.values alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->len) == 8, "LuciaVariables.len expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->len) == 8, "LuciaVariables.len alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->len) == 4, "LuciaVariables.len expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->len) == 4, "LuciaVariables.len alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->capacity) == 8, "LuciaVariables.capacity expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->capacity) == 8, "LuciaVariables.capacity alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->capacity) == 4, "LuciaVariables.capacity expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->capacity) == 4, "LuciaVariables.capacity alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((LuciaVariables*)0)->include_default) == 1, "LuciaVariables.include_default was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaVariables*)0)->include_default) == 1, "LuciaVariables.include_default alignment was expected to be 1");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaArgs*)0)->values) == 8, "LuciaArgs.values expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaArgs*)0)->values) == 8, "LuciaArgs.values alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaArgs*)0)->values) == 4, "LuciaArgs.values expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaArgs*)0)->values) == 4, "LuciaArgs.values alignment expected to be 4 on 32-bit");
#endif
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((LuciaArgs*)0)->len) == 8, "LuciaArgs.len expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaArgs*)0)->len) == 8, "LuciaArgs.len alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((LuciaArgs*)0)->len) == 4, "LuciaArgs.len expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((LuciaArgs*)0)->len) == 4, "LuciaArgs.len alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((ValueAsArgs*)0)->force) == 1, "ValueAsArgs.force was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueAsArgs*)0)->force) == 1, "ValueAsArgs.force alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((ValueAsArgs*)0)->cast) == 1, "ValueAsArgs.cast was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((ValueAsArgs*)0)->cast) == 1, "ValueAsArgs.cast alignment was expected to be 1");
#endif
#if UINTPTR_MAX == 0xffffffffffffffff
LUCIA_STATIC_ASSERT(sizeof(((InterruptArgs*)0)->msg) == 8, "InterruptArgs.msg expected to be 8 bytes on 64-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((InterruptArgs*)0)->msg) == 8, "InterruptArgs.msg alignment expected to be 8 on 64-bit");
#endif
#else
LUCIA_STATIC_ASSERT(sizeof(((InterruptArgs*)0)->msg) == 4, "InterruptArgs.msg expected to be 4 bytes on 32-bit");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((InterruptArgs*)0)->msg) == 4, "InterruptArgs.msg alignment expected to be 4 on 32-bit");
#endif
#endif
LUCIA_STATIC_ASSERT(sizeof(((InterruptArgs*)0)->all) == 1, "InterruptArgs.all was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((InterruptArgs*)0)->all) == 1, "InterruptArgs.all alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((InterruptArgs*)0)->last_thread) == 1, "InterruptArgs.last_thread was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((InterruptArgs*)0)->last_thread) == 1, "InterruptArgs.last_thread alignment was expected to be 1");
#endif
LUCIA_STATIC_ASSERT(sizeof(((InterruptArgs*)0)->thread) == 8, "InterruptArgs.thread was expected to be 8 bytes");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((InterruptArgs*)0)->thread) == 8, "InterruptArgs.thread alignment was expected to be 8");
#endif
LUCIA_STATIC_ASSERT(sizeof(((InterruptArgs*)0)->cancel) == 1, "InterruptArgs.cancel was expected to be 1 byte");
#ifndef _MSC_VER
LUCIA_STATIC_ASSERT(alignof(((InterruptArgs*)0)->cancel) == 1, "InterruptArgs.cancel alignment was expected to be 1");
#endif

#undef ValueAsArgs
#endif // LUCIA_SIZE_CHECK_H