#include "lucia.h"
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    printf("=== Lucia C API Test ===\n\n");

    LuciaConfig cfg = lucia_default_config();
    printf("Created default config\n\n");

    const char* code_simple = 
        "s: int = 0\n"
        "for i in [0..10]:\n"
        "    s += i^2\n"
        "end\n"
        "println(s)\n"
        "return 42\n";

    LuciaResult res_simple = lucia_interpret(code_simple, &cfg);
    if (lucia_result_is_ok(&res_simple)) {
        const LuciaValue* val = lucia_result_value(&res_simple);
        printf("[Simple] debug: %s\n", lucia_value_debug(*val));
        printf("[Simple] display: %s\n", lucia_value_display(*val));
        printf("[Simple] as int: %ld\n\n", value_as_int(*val));
    } else {
        const LuciaError* err = lucia_result_error(&res_simple);
        printf("[Simple] Error: %s\n", lucia_error_message(err));
    }
    lucia_free_result(res_simple);

    const char* code_list =
        "lst: list = [1, 2, 3, 4, 5]\n"
        "return lst\n";

    LuciaResult res_list = lucia_interpret(code_list, &cfg);
    if (lucia_result_is_ok(&res_list)) {
        const LuciaValue* val = lucia_result_value(&res_list);
        if (val->tag == VALUE_LIST) {
            printf("[List] size: %zu\n", val->length);
            for (size_t i = 0; i < val->length; ++i) {
                const LuciaValue* item = lucia_list_get(*val, i);
                printf("%ld ", value_as_int(*item));
            }
            printf("\n\n");
        }
    }
    lucia_free_result(res_list);

    const char* code_map =
        "m: map = { \"a\": 10, \"b\": 20, \"c\": 30 }\n"
        "return m\n";

    LuciaResult res_map = lucia_interpret(code_map, &cfg);
    if (lucia_result_is_ok(&res_map)) {
        const LuciaValue* val = lucia_result_value(&res_map);
        printf("[Map] size: %zu\n", val->length);
        const LuciaValue* entries = val->data.map_ptr;
        for (size_t i = 0; i < (val->length/2); ++i) {
            const LuciaValue* key = &entries[i*2];
            const LuciaValue* value = &entries[i*2 + 1];
            printf("%s => %ld\n", lucia_value_string_ptr(*key), value_as_int(*value));
        }
        const LuciaValue* b_val = lucia_map_get_cstr(*val, "b");
        if (b_val) {
            printf("[Map] b via key: %ld\n\n", value_as_int(*b_val));
        }
    }
    lucia_free_result(res_map);

    const char* code_string =
        "return \"hello lucia\"\n";

    LuciaResult res_string = lucia_interpret(code_string, &cfg);
    if (lucia_result_is_ok(&res_string)) {
        const LuciaValue* val = lucia_result_value(&res_string);
        printf("[String] as_string: %s\n", value_as_string(*val));
        printf("[String] debug: %s\n\n", lucia_value_debug(*val));
    }
    lucia_free_result(res_string);

    const char* code_bool_float =
        "return (true, 3.1415)\n";

    LuciaResult res_bf = lucia_interpret(code_bool_float, &cfg);
    if (lucia_result_is_ok(&res_bf)) {
        const LuciaValue* val = lucia_result_value(&res_bf);
        printf("[Bool/Float] debug: %s\n", lucia_value_debug(*val));
        if (val->tag == VALUE_LIST) {
            const LuciaValue* list = val->data.list_ptr;
            printf("[Bool/Float] bool: %d, float: %f\n\n",
                   value_as_bool(list[0]), value_as_float(list[1]));
        }
    }
    lucia_free_result(res_bf);

    const char* code_pointer =
        "ptr: &void = &null\n"
        "return ptr\n";

    LuciaResult res_ptr = lucia_interpret(code_pointer, &cfg);
    if (lucia_result_is_ok(&res_ptr)) {
        const LuciaValue* val = lucia_result_value(&res_ptr);
        printf("[Pointer] is_null: %d\n\n", lucia_value_is_null(*val));
    }
    lucia_free_result(res_ptr);

    const char* code_error =
        "a: int = \"not an int\"\n"
        "return a\n";

    LuciaResult res_err = lucia_interpret(code_error, &cfg);
    if (lucia_result_is_error(&res_err)) {
        const LuciaError* err = lucia_result_error(&res_err);
        printf("[Error] type    : %s\n", lucia_error_type(err));
        printf("[Error] message : %s\n", lucia_error_message(err));
        printf("[Error] help    : %s\n", lucia_error_help(err));
        printf("[Error] location: %s\n\n", lucia_error_location(err));
    }
    lucia_free_result(res_err);

    const char* code_argv =
        "return argv[0], argv[1]\n";
    const char* argv_arr[] = { "foo", "bar" };
    LuciaResult res_argv = lucia_interpret_with_argv(code_argv, argv_arr, 2, &cfg);
    if (lucia_result_is_ok(&res_argv)) {
        const LuciaValue* val = lucia_result_value(&res_argv);
        if (val->tag == VALUE_LIST) {
            const LuciaValue* list = val->data.list_ptr;
            printf("[ARGV] 0: %s, 1: %s\n\n", value_as_string(list[0]), value_as_string(list[1]));
        }
    }
    lucia_free_result(res_argv);

    const BuildInfo* info = lucia_get_build_info();
    printf("[BuildInfo] name        : %s\n", info->name);
    printf("[BuildInfo] version     : %s\n", info->version);
    printf("[BuildInfo] rustc_version: %s\n", info->rustc_version);
    printf("[BuildInfo] git_hash    : %s\n", info->git_hash);
    printf("[BuildInfo] build_date  : %s\n", info->build_date);

    printf("\n=== End of Test ===\n");

    lucia_free_config(cfg);
    return 0;
}
