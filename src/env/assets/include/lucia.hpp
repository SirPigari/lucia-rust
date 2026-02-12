#ifndef LUCIA_HPP
#define LUCIA_HPP

/*
    Lucia Programming Language
        - by Markofwitch

    C++ Header File for C++11 and later
    GPL-3.0 License - Copyright (c) 2026 Markofwitch
*/

#include <string>
#include <vector>
#include <utility>
#include <stdexcept>
#include <cstdint>
#include <ostream>

#define LUCIA_API_VERSION 0x1000 // 0x(major)(minor)(02patch) - https://semver.org/
#define LUCIA_API_VERSION_STRING "1.0.0"

#define LUCIA_VERSION_MAJOR ((LUCIA_API_VERSION >> 12) & 0xF)
#define LUCIA_VERSION_MINOR (((LUCIA_API_VERSION) >> 8)  & 0xF)
#define LUCIA_VERSION_PATCH ((LUCIA_API_VERSION) & 0xFF)

typedef uint8_t CBool;
enum LuciaValueType : uint8_t {
    VALUE_NULL = 0,
    VALUE_INT = 1,
    VALUE_FLOAT = 2,
    VALUE_STRING = 3,
    VALUE_BOOLEAN = 4,
    VALUE_LIST = 5,
    VALUE_MAP = 6,
    VALUE_BYTES = 7,
    VALUE_POINTER = 8,
    VALUE_UNSUPPORTED = 255,
};

enum LuciaResultTag : uint8_t {
    LUCIA_RESULT_OK = 1,
    LUCIA_RESULT_ERROR = 2,
    LUCIA_RESULT_CONFIG_ERR = 3,
    LUCIA_RESULT_PANIC = 4,
    LUCIA_RESULT_NONE = 0,
};

extern "C" {
    #include <stdint.h>
    #include <stddef.h>
    #include <stdio.h>
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
        const char* libs_paths[16];
        size_t stack_size;
        const char* version;
    } LuciaConfig;
    typedef struct LuciaValue LuciaValue;
    typedef union ValueData {
        int64_t int_v;
        double float_v;
        CBool bool_v;
        const char* string_v;
        const uint8_t* bytes_ptr;
        const LuciaValue* list_ptr;
        const LuciaValue* map_ptr;
        void* pointer;
    } ValueData;
    struct LuciaValue {
        LuciaValueType tag;
        ValueData data;
        size_t length;
    };
    LuciaValue lucia_value_null(void);
    LuciaValue lucia_value_int(int64_t v);
    LuciaValue lucia_value_float(double v);
    LuciaValue lucia_value_bool(CBool v);
    LuciaValue lucia_value_string(const char* utf8);
    LuciaValue lucia_value_bytes(const uint8_t* data, size_t len);
    LuciaValue lucia_value_list(const LuciaValue* items, size_t len);
    LuciaValue lucia_value_map(const LuciaValue* entries, size_t len);
    int lucia_value_cmp(LuciaValue a, LuciaValue b);
    uint64_t lucia_value_hash(LuciaValue v);
    LuciaValueType lucia_value_get_type(LuciaValue v);
    const char* lucia_value_type_name(LuciaValueType t);
    const char* lucia_value_string_ptr(LuciaValue v);
    CBool lucia_value_string_clone(LuciaValue v, const char** out, size_t* out_len);
    LuciaValue lucia_value_clone(LuciaValue v);
    LuciaValue lucia_value_deep_clone(LuciaValue v);
    CBool lucia_value_is_valid(LuciaValue v);
    CBool lucia_value_is_valid_ptr(const LuciaValue* v);
    CBool lucia_value_is_truthy(LuciaValue v);
    CBool lucia_value_is_null(LuciaValue v);
    const char* lucia_value_debug(LuciaValue v);
    const char* lucia_value_display(LuciaValue v);
    uint32_t lucia_list_len(LuciaValue list);
    const LuciaValue* lucia_list_get(LuciaValue list, size_t index);
    CBool lucia_list_try_get(LuciaValue list, size_t index, const LuciaValue** out);
    uint32_t lucia_map_len(LuciaValue map);
    const LuciaValue* lucia_map_get(LuciaValue map, const LuciaValue* key);
    CBool lucia_map_try_get(LuciaValue map, const LuciaValue** out, const LuciaValue* key);
    const LuciaValue* lucia_map_get_cstr(LuciaValue map, const char* key);
    CBool lucia_map_try_get_cstr(LuciaValue map, const char** out, const char* key);
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
    CBool try_value_as_int(LuciaValue v, int64_t* out);
    CBool try_value_as_float(LuciaValue v, double* out);
    CBool try_value_as_bool(LuciaValue v, CBool* out);
    CBool try_value_as_string(LuciaValue v, const char** out, size_t* out_len);
    CBool try_value_as_bytes(LuciaValue v, const uint8_t** out_ptr, size_t* out_len);
    CBool try_value_as_list(LuciaValue v, const LuciaValue** out_ptr, size_t* out_len);
    CBool try_value_as_map(LuciaValue v, const LuciaValue** out_ptr, size_t* out_len);
    CBool try_value_as_pointer(LuciaValue v, void** out);
    typedef struct lucia__ValueAsArgs {
        CBool force;
        CBool cast;
    } lucia__ValueAsArgs;
    CBool lucia__value_as_args(LuciaValue v, LuciaValueType t, void* out, lucia__ValueAsArgs args);
    typedef struct LuciaError {
        const char* err_type;
        const char* err_msg;
        const char* help_msg;
        uint32_t line_num;
        const char* line_text;
        size_t column;
    } LuciaError;
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
    const LuciaValue* lucia_result_value(const LuciaResult* res);
    const LuciaError* lucia_result_error(const LuciaResult* res);
    CBool lucia_result_try_as_value(const LuciaResult* res, const LuciaValue** out);
    CBool lucia_result_try_as_error(const LuciaResult* res, const LuciaError** out);
    void lucia_error_print(const LuciaError* err, FILE* out);
    const char* lucia_error_type(const LuciaError* err);
    const char* lucia_error_message(const LuciaError* err);
    const char* lucia_error_help(const LuciaError* err);
    const char* lucia_error_location(const LuciaError* err);
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
    void lucia_free_value(LuciaValue v);
    void lucia_free_error(LuciaError err);
    void lucia_free_result(LuciaResult res);
    void lucia_free_config(LuciaConfig cfg);
    const BuildInfo* lucia_get_build_info(void);
    LuciaConfig lucia_default_config(void);
    LuciaResult lucia_interpret(const char* code, const LuciaConfig* config);
    LuciaResult lucia_interpret_with_argv(const char* code, const char** argv, size_t argc, const LuciaConfig* config);
}

#define LUCIA_NULL lucia_value_null()

namespace lucia {
    class Value {
    public:
        LuciaValue v;
        bool owned;

        Value() : v(lucia_value_null()), owned(true) {}
        explicit Value(LuciaValue val, bool take_ownership = true) : v(val), owned(take_ownership) {}
        ~Value() { if (owned) lucia_free_value(v); }

        Value(Value&& other) noexcept : v(other.v), owned(other.owned) { other.owned = false; }
        Value& operator=(Value&& other) noexcept {
            if (this != &other) {
                if (owned) lucia_free_value(v);
                v = other.v;
                owned = other.owned;
                other.owned = false;
            }
            return *this;
        }

        Value(const Value&) = delete;
        Value& operator=(const Value&) = delete;

        std::string debug() const { 
            const char* tmp = lucia_value_debug(v);
            return tmp ? std::string(tmp) : std::string{};
        }
        std::string display() const { 
            const char* tmp = lucia_value_display(v);
            return tmp ? std::string(tmp) : std::string{};
        }

        LuciaValueType type() const { return lucia_value_get_type(v); }

        int64_t as_int() const { return value_as_int(v); }
        double as_float() const { return value_as_float(v); }
        bool as_bool() const { return value_as_bool(v); }
        std::string as_string() const { 
            const char* s = value_as_string(v);
            return s ? std::string(s) : std::string{};
        }
        std::vector<uint8_t> as_bytes() const {
            if (v.tag != VALUE_BYTES) return {};
            const uint8_t* p = value_as_bytes_ptr(v);
            size_t len = value_as_bytes_len(v);
            return p ? std::vector<uint8_t>(p, p + len) : std::vector<uint8_t>{};
        }
        std::vector<Value> as_list() const {
            if (type() != VALUE_LIST) return {};
            const LuciaValue* p = value_as_list_ptr(v);
            size_t len = value_as_list_len(v);
            std::vector<Value> result;
            result.reserve(len);
            for (size_t i = 0; i < len; ++i) result.emplace_back(p[i], false); // false = non-owning
            return result;
        }

        std::vector<std::pair<Value, Value>> as_map() const {
            if (type() != VALUE_MAP) return {};
            const LuciaValue* p = value_as_map_ptr(v);
            size_t len = value_as_map_len(v) / 2;
            std::vector<std::pair<Value, Value>> result;
            for (size_t i = 0; i < len; ++i)
                result.emplace_back(Value(p[i*2], false), Value(p[i*2+1], false));
            return result;
        }

        void* as_pointer() const { return value_as_pointer(v); }

        Value operator[](size_t index) const {
            const LuciaValue* ptr = lucia_list_get(v, index);
            if (!ptr) throw std::out_of_range("Index out of bounds or not a list");
            return Value(*ptr, false);
        }

        Value at(size_t index) const { return (*this)[index]; }

        Value operator[](const std::string& key) const {
            const LuciaValue* ptr = lucia_map_get_cstr(v, key.c_str());
            if (!ptr) throw std::out_of_range("Key not found or not a map");
            return Value(*ptr, false);
        }

        Value at(const std::string& key) const { return (*this)[key]; }

        size_t size() const {
            if (v.tag == VALUE_LIST) return lucia_list_len(v);
            if (v.tag == VALUE_MAP) return lucia_map_len(v);
            return 0;
        }

        bool is_null() const { return lucia_value_is_null(v); }

        bool is_valid() const { return lucia_value_is_valid(v); }
        bool is_truthy() const { return lucia_value_is_truthy(v); }
        static bool is_valid(const Value& val) { return lucia_value_is_valid(val.v); }

        Value clone() const { return Value(lucia_value_clone(v), true); }
        Value deep_clone() const { return Value(lucia_value_deep_clone(v), true); }
    };

    class Error {
    public:
        LuciaError e;
        bool owned;

        explicit Error(LuciaError err, bool take_ownership = true) : e(err), owned(take_ownership) {}
        ~Error() { if (owned) lucia_free_error(e); }

        Error(Error&& other) noexcept : e(other.e), owned(other.owned) { other.owned = false; }
        Error& operator=(Error&& other) noexcept {
            if (this != &other) {
                if (owned) lucia_free_error(e);
                e = other.e;
                owned = other.owned;
                other.owned = false;
            }
            return *this;
        }

        Error(const Error&) = delete;
        Error& operator=(const Error&) = delete;

        std::string type() const { return e.err_type ? std::string(e.err_type) : std::string{}; }
        std::string message() const { return e.err_msg ? std::string(e.err_msg) : std::string{}; }
        std::string help() const { return e.help_msg ? std::string(e.help_msg) : std::string{}; }
        std::string location() const {
            const char* tmp = lucia_error_location(&e);
            return tmp ? std::string(tmp) : std::string{};
        }
    };

    class Result {
    public:
        LuciaResult r;

        explicit Result(LuciaResult res) : r(res) {}
        ~Result() { lucia_free_result(r); }

        Result(Result&& other) noexcept : r(other.r) { other.r.tag = LUCIA_RESULT_NONE; }
        Result& operator=(Result&& other) noexcept {
            if (this != &other) {
                lucia_free_result(r);
                r = other.r;
                other.r.tag = LUCIA_RESULT_NONE;
            }
            return *this;
        }

        Result(const Result&) = delete;
        Result& operator=(const Result&) = delete;

        bool is_ok() const { return lucia_result_is_ok(&r); }
        bool is_error() const { return lucia_result_is_error(&r); }

        Value value() const {
            const LuciaValue* v = lucia_result_value(&r);
            if (!v) throw std::runtime_error("Result does not contain a value");
            return Value(*v, false);
        }

        Error error() const {
            const LuciaError* e = lucia_result_error(&r);
            if (!e) throw std::runtime_error("Result does not contain an error");
            return Error(*e, false);
        }
    };

    class Config {
    public:
        LuciaConfig cfg;

        Config() : cfg(lucia_default_config()) {}
        ~Config() { lucia_free_config(cfg); }

        Config(Config&& other) noexcept : cfg(other.cfg) { other.cfg = {}; }
        Config& operator=(Config&& other) noexcept {
            if (this != &other) {
                lucia_free_config(cfg);
                cfg = other.cfg;
                other.cfg = {};
            }
            return *this;
        }

        Config(const Config&) = delete;
        Config& operator=(const Config&) = delete;
    };

    inline Result interpret(const std::string& code, const Config& config) {
        return Result(lucia_interpret(code.c_str(), &config.cfg));
    }

    inline Result interpret_with_argv(const std::string& code, const std::vector<std::string>& argv, const Config& config) {
        std::vector<const char*> c_argv;
        c_argv.reserve(argv.size());
        for (const auto& s : argv) c_argv.push_back(s.c_str());
        return Result(lucia_interpret_with_argv(code.c_str(), c_argv.data(), c_argv.size(), &config.cfg));
    }

    struct BuildInfo {
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

        static BuildInfo get() {
            const ::BuildInfo* b = lucia_get_build_info();
            if (!b) throw std::runtime_error("lucia_get_build_info() returned null");
            BuildInfo info;
            info.name = b->name;
            info.version = b->version;
            info.uuid = b->uuid;
            info.rustc_version = b->rustc_version;
            info.rustc_channel = b->rustc_channel;
            info.target = b->target;
            info.repository = b->repository;
            info.git_hash = b->git_hash;
            info.file_hash = b->file_hash;
            info.profile = b->profile;
            info.build_date = b->build_date;
            return info;
        }
    };

    inline bool value_as(LuciaValue v, LuciaValueType t, void* out,
                        bool force = 0, bool cast = 0) {
        lucia__ValueAsArgs args;
        args.force = (CBool)force;
        args.cast  = (CBool)cast;
        return (bool)(lucia__value_as_args(v, t, out, args));
    }

    inline std::ostream& operator<<(std::ostream& os, const Value& val) {
        os << val.display();
        return os;
    }

    inline std::ostream& operator<<(std::ostream& os, const Error& err) {
        os << "[" << err.type() << "] " << err.message();
        return os;
    }

    inline std::ostream& operator<<(std::ostream& os, const Result& res) {
        if (res.is_ok()) os << res.value();
        else if (res.is_error()) os << res.error();
        else os << "[Unknown Result]";
        return os;
    }

    inline std::ostream& operator<<(std::ostream& os, const Config&) {
        os << "[Lucia Config]";
        return os;
    }

    inline std::ostream& operator<<(std::ostream& os, const BuildInfo& info) {
        os << info.name << " v" << info.version << " (" << info.build_date << ")";
        return os;
    }
} // namespace lucia

// because it didnt work inside the namespace
inline std::ostream& operator<<(std::ostream& os, LuciaValueType t) {
    os << lucia_value_type_name(t);
    return os;
}

#endif // LUCIA_HPP