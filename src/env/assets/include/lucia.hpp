#ifndef LUCIA_HPP
#define LUCIA_HPP

/*
    Lucia Programming Language
        - by Markofwitch

    C++ Header File for C++11 and later
    GPL-3.0 License - Copyright (c) 2026 Markofwitch
*/

// WARNING: Not tested yet

#include <string>
#include <vector>
#include <utility>
#include <stdexcept>
#include <cstdint>

extern "C" {
    #include "lucia.h"
}

namespace lucia {
    class Value {
    public:
        LuciaValue v;

        Value() : v(lucia_value_null()) {}
        explicit Value(LuciaValue val) : v(val) {}
        ~Value() { lucia_free_value(v); }

        Value(Value&& other) noexcept : v(other.v) { other.v = LUCIA_NULL; }
        Value& operator=(Value&& other) noexcept {
            if (this != &other) {
                lucia_free_value(v);
                v = other.v;
                other.v = LUCIA_NULL;
            }
            return *this;
        }

        Value(const Value&) = delete;
        Value& operator=(const Value&) = delete;

        // safe debug/display
        std::string debug() const { 
            const char* tmp = lucia_value_debug(v);
            return tmp ? std::string(tmp) : std::string{};
        }
        std::string display() const { 
            const char* tmp = lucia_value_display(v);
            return tmp ? std::string(tmp) : std::string{};
        }

        LuciaValueType type() const { return lucia_value_get_type(v); }

        // conversions
        int64_t as_int() const { return value_as_int(v); }
        double as_float() const { return value_as_float(v); }
        bool as_bool() const { return value_as_bool(v); }
        std::string as_string() const { 
            const char* s = value_as_string(v);
            return s ? std::string(s) : std::string{};
        }
        std::vector<uint8_t> as_bytes() const {
            const uint8_t* p = value_as_bytes_ptr(v);
            size_t len = value_as_bytes_len(v);
            return p ? std::vector<uint8_t>(p, p + len) : std::vector<uint8_t>{};
        }
        std::vector<Value> as_list() const {
            const LuciaValue* p = value_as_list_ptr(v);
            size_t len = value_as_list_len(v);
            std::vector<Value> result;
            result.reserve(len);
            for (size_t i = 0; i < len; ++i) result.emplace_back(p[i]);
            return result;
        }

        std::vector<std::pair<Value, Value>> as_map() const {
            const LuciaValue* p = value_as_map_ptr(v);
            size_t len = value_as_map_len(v);
            std::vector<std::pair<Value, Value>> result;
            for (size_t i = 0; i < len; ++i)
                result.emplace_back(p[i*2], p[i*2+1]);
            return result;
        }

        void* as_pointer() const { return value_as_pointer(v); }

        Value operator[](size_t index) const {
            const LuciaValue* ptr = lucia_list_get(v, index);
            if (!ptr) throw std::out_of_range("Index out of bounds or not a list");
            return Value(*ptr);
        }

        Value at(size_t index) const { return (*this)[index]; }

        Value operator[](const std::string& key) const {
            const LuciaValue* ptr = lucia_map_get_cstr(v, key.c_str());
            if (!ptr) throw std::out_of_range("Key not found or not a map");
            return Value(*ptr);
        }

        Value at(const std::string& key) const { return (*this)[key]; }

        size_t size() const {
            if (v.tag == VALUE_LIST) return lucia_list_len(v);
            if (v.tag == VALUE_MAP) return lucia_map_len(v);
            return 0;
        }

        bool is_null() const { return lucia_value_is_null(v); }
    };

    class Error {
    public:
        LuciaError e;

        explicit Error(LuciaError err) : e(err) {}
        ~Error() { lucia_free_error(e); }

        Error(Error&& other) noexcept : e(other.e) { other.e = {}; }
        Error& operator=(Error&& other) noexcept {
            if (this != &other) {
                lucia_free_error(e);
                e = other.e;
                other.e = {};
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

        Result(Result&& other) noexcept : r(other.r) { other.r.tag = 0; }
        Result& operator=(Result&& other) noexcept {
            if (this != &other) {
                lucia_free_result(r);
                r = other.r;
                other.r.tag = 0;
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
            return Value(*v);
        }

        Error error() const {
            const LuciaError* e = lucia_result_error(&r);
            if (!e) throw std::runtime_error("Result does not contain an error");
            return Error(*e);
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
            return *b;
        }
    };

} // namespace lucia

#endif // LUCIA_HPP
