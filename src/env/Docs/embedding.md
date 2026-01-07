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

> NOTE:  
> For the static library you must link with math library using `-lm`

Example compiling with GCC on MSVC Windows:

```console
gcc main.c -o main -llucia
```

I don't know what else to say its really self documented.

Just copy this and put any code into the `code` variable
