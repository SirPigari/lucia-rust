#include "lucia.hpp"
#include <iostream>
#include <vector>
#include <string>

int main() {
    std::cout << "=== Lucia C++ API Test ===\n\n";

    lucia::Config cfg;
    std::cout << "Created default config\n\n";

    const std::string code_simple = R"(
s: int = 0
for i in [0..10]:
    s += i^2
end
println(s)
return 42
)";
    lucia::Result res_simple = lucia::interpret(code_simple, cfg);

    if (res_simple.is_ok()) {
        lucia::Value val = res_simple.value();
        std::cout << "[Simple] Return value debug: " << val.debug() << "\n";
        std::cout << "[Simple] Return value display: " << val.display() << "\n";
        std::cout << "[Simple] As int: " << val.as_int() << "\n\n";
    } else {
        lucia::Error err = res_simple.error();
        std::cerr << "[Simple] Error: " << err.message() << "\n";
    }

    const std::string code_list = R"(
lst: list = [1, 2, 3, 4, 5]
return lst
)";
    lucia::Result res_list = lucia::interpret(code_list, cfg);

    if (res_list.is_ok()) {
        lucia::Value val = res_list.value();
        std::cout << "[List] Type: " << val.type() << ", size: " << val.size() << "\n";
        std::vector<lucia::Value> list = val.as_list();
        std::cout << "[List] Elements: ";
        for (size_t i = 0; i < list.size(); ++i) {
            std::cout << list[i].as_int() << " ";
        }
        std::cout << "\n\n";
    }

    const std::string code_map = R"(
m: map = { "a": 10, "b": 20, "c": 30 }
return m
)";
    lucia::Result res_map = lucia::interpret(code_map, cfg);

    if (res_map.is_ok()) {
        lucia::Value val = res_map.value();
        std::cout << "[Map] Type: " << val.type() << ", size: " << val.size() << "\n";
        auto map = val.as_map();
        for (const auto& pair : map) {
            const auto& k = pair.first;
            const auto& v = pair.second;
            std::cout << "[Map] " << k.as_string() << " => " << v.as_int() << "\n";
        }
        std::cout << "[Map] b via operator[]: " << val["b"].as_int() << "\n\n";
    }

    const std::string code_string = R"(
return "hello lucia"
)";
    lucia::Result res_string = lucia::interpret(code_string, cfg);
    if (res_string.is_ok()) {
        lucia::Value val = res_string.value();
        std::cout << "[String] as_string(): " << val.as_string() << "\n";
        std::cout << "[String] debug(): " << val.debug() << "\n\n";
    }

    const std::string code_bool_float = R"(
return (true, 3.1415)
)";
    lucia::Result res_bf = lucia::interpret(code_bool_float, cfg);
    if (res_bf.is_ok()) {
        lucia::Value val = res_bf.value();
        std::cout << "[Bool/Float] debug(): " << val.debug() << "\n";
        if (val.type() == VALUE_LIST) {
            auto list = val.as_list();
            std::cout << "[Bool/Float] bool: " << list[0].as_bool() 
                      << ", float: " << list[1].as_float() << "\n\n";
        }
    }

    const std::string code_pointer = R"(
ptr: &void = &null
return ptr
)";
    lucia::Result res_ptr = lucia::interpret(code_pointer, cfg);
    if (res_ptr.is_ok()) {
        lucia::Value val = res_ptr.value();
        std::cout << "[Pointer] is_null(): " << val.is_null() << "\n\n";
    }

    const std::string code_error = R"(
a: int = "not an int"
return a
)";
    lucia::Result res_err = lucia::interpret(code_error, cfg);
    if (res_err.is_error()) {
        lucia::Error err = res_err.error();
        std::cerr << "[Error] type   : " << err.type() << "\n";
        std::cerr << "[Error] message: " << err.message() << "\n";
        std::cerr << "[Error] help   : " << err.help() << "\n";
        std::cerr << "[Error] location: " << err.location() << "\n\n";
    }

    const std::string code_argv = R"(
return argv[0], argv[1]
)";
    std::vector<std::string> argv = { "foo", "bar" };
    lucia::Result res_argv = lucia::interpret_with_argv(code_argv, argv, cfg);
    if (res_argv.is_ok()) {
        lucia::Value val = res_argv.value();
        if (val.type() == VALUE_LIST) {
            auto list = val.as_list();
            std::cout << "[ARGV] 0: " << list[0].as_string() << ", 1: " << list[1].as_string() << "\n\n";
        }
    }

    auto info = lucia::BuildInfo::get();
    std::cout << "[BuildInfo] name: " << info.name << "\n";
    std::cout << "[BuildInfo] version: " << info.version << "\n";
    std::cout << "[BuildInfo] rustc_version: " << info.rustc_version << "\n";
    std::cout << "[BuildInfo] git_hash: " << info.git_hash << "\n";
    std::cout << "[BuildInfo] build_date: " << info.build_date << "\n";

    // TODO: fix
    // auto val = lucia::Value::from_string("hello");
    // {
    //     auto clone = val.clone();
    //     std::cout << "\n[Clone] " << clone.debug();
    // }
    // if (val.is_valid()) {
    //     printf(" (valid)\n");
    // } else {
    //     printf(" (invalid)\n");
    //     return 1;
    // }


    std::cout << "\n=== End of Test ===\n";
    return 0;
}
