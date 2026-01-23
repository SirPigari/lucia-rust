# Embedding

Lucia can be embedded with `liblucia`.

Here is a simple example to print `Hello World` from C:

```c
#include <stdio.h>
#include <lucia.h>

int main() {
    LuciaConfig config = lucia_default_config();
    printf("Lucia version: %s\n", config.version);

    const char* code = "println(\"Hello World\") 42";
    LuciaResult result = lucia_interpret(code, &config);

    if (result.tag == LUCIA_RESULT_OK) {
        printf("Script executed successfully.\n");
        int64_t i;
        if (!try_value_as_int(result.data.value, &i)) {
            printf("Return value is not an integer\n");
        } else {
            printf("Return value as int: %lld\n", i);
        }
    } else if (result.tag == LUCIA_RESULT_ERROR) {
        printf("Error: %s\n", result.data.error.err_msg);
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

It has to link with a billion stupid libs because microslop cannot decide what is where.

I don't know what else to say its really self documented.

Just copy this and put any code into the `code` variable

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

> [!NOTE]
> `lucia_size_check.h` is currently experimental because im not sure how much it works on 32bit machines or on MSVC (fuck you microslop)
