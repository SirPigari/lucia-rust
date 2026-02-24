# Embedding

Lucia can be embedded with `liblucia`.

## Basic code execution

Here is a simple example to print `Hello World` from C:

```c
#include <stdio.h>
#include <lucia.h>

int main() {
    LuciaConfig config = lucia_default_config();
    printf("Lucia version: %s\n", config.version);

    const char* code = "println(\"Hello World\") 42";
    LuciaResult result = lucia_interpret(code, &config);

    if (lucia_result_is_ok(result)) {
        printf("Script executed successfully.\n");
        int64_t i;
        if (!try_value_as_int(result.data.value, &i)) {
            printf("Return value is not an integer\n");
        } else {
            printf("Return value as int: %lld\n", i);
        }
    } else if (lucia_result_is_error(result)) {
        printf("Error: %s\n", result.data.error.err_msg);
    } else {
        printf("Panic: %s\n", result.data.panic.panic_msg);
    }

    lucia_free_result(result);
    lucia_free_config(config);

    return 0;
}
```

You must link to `liblucia` and you must have [lucia.h](../assets/include/lucia.h)

There is a static and a dynamic version of `liblucia` located in the [bin](../bin/) directory

[bin/](../bin/)

- [lucia-standalone](../bin/lucia-standalone)
- [lucia](../bin/lucia)
- [lucia.so](../bin/lucia.so)
- [lucia.a](../bin/lucia.a)

> [!NOTE]  
> For the static library you must link with math library using `-lm`

Example compiling with GCC:

```console
gcc main.c -o main -llucia
```

The stupid command that took me 2 hours to figure on windows MSVC:

```vc
cl main.c /MD /I .\src\env\assets\include /Fe:main.exe /link /LIBPATH:.\src\env\bin ^
lucia.lib ws2_32.lib user32.lib kernel32.lib ntdll.lib msvcrt.lib legacy_stdio_definitions.lib ^
userenv.lib pathcch.lib powrprof.lib gdi32.lib shell32.lib bcrypt.lib advapi32.lib
```

It has to link with a billion stupid libs because microslop cant choose between MSVCRT and UCRT

## Injecting variables and retrieving them

```c
LuciaConfig config = lucia_default_config();
LuciaVariables* vars = lucia_variables_new_default(); // default builtins

lucia_variables_insert(vars, "a", lucia_value_int(35));
lucia_variables_insert(vars, "b", lucia_value_string("hello"));

LuciaResult res = lucia_interpret_with_vars("println(b) c := 34 + a", &config, vars);
if (lucia_result_is_error(&res)) {
    LuciaError err = *lucia_result_error(&res);
    lucia_error_print(&err, stderr)
    return 1;
}
lucia_free_result(res);

// retrieve result
LuciaValue c = lucia_variables_get_or_default(vars, "c", LUCIA_NULL);
int64_t i;
if (try_value_as_int(c, &i))
    printf("c = %lld\n", i); // prints 69

lucia_variables_free(vars);
lucia_free_config(config);
```

If you don't want the default builtins, you can use `lucia_variables_new(size_t capacity)` instead. Be warned that builtins include types.

### Injecting native functions

```c
#include <lucia.h>
#include <stdio.h>

LuciaResult b_func(const LuciaArgs* args) {
    const LuciaValue* a;
    if (!lucia_args_get(args, &a, 0))
        return lucia_new_result_error("TypeError", "Missing argument 'a'");
    
    int64_t i;
    if (!try_value_as_int(*a, &i))
        return lucia_new_result_error("TypeError", "Expected 'a' to be an int");
    
    return lucia_new_result_value(lucia_value_int(i + 34));
}

int main() {
    LuciaConfig config = lucia_default_config();
    config.allow_unsafe = true; // allow unsafe operations (calling to native functions is unsafe)
    LuciaVariables* vars = lucia_variables_new_default();

    lucia_variables_insert(vars, "a", lucia_value_int(35));

    // inject native function
    lucia_variables_insert_function(vars, "b", b_func);

    LuciaResult res = lucia_interpret_with_vars("c := b(a)", &config, vars);
    if (lucia_result_is_error(&res)) {
        const LuciaError* err = lucia_result_error(&res);
        lucia_error_print(err, stderr);
        return 1;
    }
    lucia_free_result(res);

    const LuciaValue c = lucia_variables_get_or_default(vars, "c", LUCIA_NULL);
    int64_t i;
    if (try_value_as_int(c, &i))
        printf("c = %lld\n", i);
    else 
        printf("c is not an int\n");

    lucia_variables_free(vars);
    lucia_free_config(config);
}
```

For calling native C code, we need to enable `allow_unsafe` in config.  

The function you insert into the variables is always of Lucia type `native public mutable function[*any] -> any`.  

Since i wanted a simple API i choose to convert the variadic args into an array that you get. Thats why you need to validate the arguments yourself and make sure they exist and are of the right type. Because Lucia doesnt support kwargs this will work always (i havent found any function that would not be able to be made native).

### Interrupting

If you want to interrupt the currently running interpreter, call `lucia_interrupt_current()`. You can also provide an optional message with `lucia_interrupt_current_with_message(const char* msg)`. The message will be available to you in the LuciaResult returned by the interpreter.

```c
#include <lucia.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

typedef struct {
    LuciaConfig config;
    const char* code;
} ThreadData;

void* interpret_thread(void* arg) {
    ThreadData* data = (ThreadData*)arg;
    LuciaResult res = lucia_interpret(data->code, &data->config);
    fprintf(stderr, "%s\n", lucia_result_display(res));
    lucia_free_result(res);
    return NULL;
}

int main() {
    LuciaConfig config = lucia_default_config();

    // yes sleep is in milliseconds.
    ThreadData data = {config, "import time for _ in [0..5]: time.sleep(1000) end return 420"};

    pthread_t thread;
    pthread_create(&thread, NULL, interpret_thread, &data);

    sleep(5); // wait 5 seconds
    lucia_interrupt_current_with_message("Interrupted by user"); // interrupt the currently running interpreter

    pthread_join(thread, NULL);

    lucia_free_config(config);
    return 0;
}
```

This example should output someting like:

```
lucia git:(main) ✗ gcc test.c -o test src/env/bin/liblucia.a -Isrc/env/assets/include -lm -Wall -Werror -std=c99
lucia git:(main) ✗ time ./test                                                                                  
Interrupted by user
./test  0.01s user 0.01s system 0% cpu 5.020 total
```

It should print `Interrupted by user` after 5 seconds, which is the message we provided to `lucia_interrupt_current_with_message`. The interpreter will stop immediately and return a result with tag `LUCIA_RESULT_INTERRUPT` and the interrupt message.

Im aware there is an issue with multiple interpreters running at the same time and interrupting the wrong one. I will fix that in the future but for now just dont do that.

If no interpreter is running when you call `lucia_interrupt_current()` or `lucia_interrupt_current_with_message()`, it will do nothing.

The future fix may look like:

```c
lucia_interrupt_thread(pthread_self(), "Interrupted by user");
```

## ABI validation

If you want to be sure all the ABI is correct, include [lucia_size_check.h](../assets/include/lucia_size_check.h).

```c
#include <lucia.h>
#include <lucia_size_check.h>

int main() {
    // ...
    return 0
}
```

It uses static assert for the checks so if any fails you will not be able to compile.  
It is compile time only so you don't pay any runtime cost.

> [!WARNING]
> `lucia_size_check.h` is currently experimental because im not sure how much it works on 32bit machines or on MSVC (fuck you microslop)
