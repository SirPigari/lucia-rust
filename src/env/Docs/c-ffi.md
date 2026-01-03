# C FFI

Lets say you want to call a function from C

For simplicity, lets make the C file just a `add` function and a `display` function.

```c
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

void display(const char* text) {
    printf("%s\n", text);
} 
```

and then the header would be:

```h
int add(int a, int b);
void display(const char* text)
```

Okay you have that and you compiled into a test.so.

For calling dynamically, we use the `libload` standard library.

Here is how you would do that:

```lucia
import libload

lib: &int = libload.load_lib(__dir__+"test.so")

add_c: &int = libload.get_fn(lib, "add", ["int", "int"], "int")
display_c: &int = libload.get_fn(lib, "print", ["str"], "void")

fun add(a: int, b: int) -> int:
    libload.call_fn(add_c, [a, b])
end

fun display(text: str) -> void:
    _text: &int = libload.create_str_ptr(text)
    libload.call_fn(display_c, [_text])
end

display(add(34, 35) as str)
```

`lib` holds a pointer to the loaded library.

The `__dir__` points to the directory the file is located in.

`add_c` and `display_c` are pointers to the functions from the library.

To use these functions you have to make the wrappers.

But what if there is a bunch of functions that are just int -> int? Exactly for this case there is a `#link` directive.

```lucia
#link "test.h" "test.so"

display(add(34, 35) as str)
```

## Structs

Now lets say you want to use a structure.

You have a file:

```c
typedef struct {
    int x;
    int y;
} Vector2;

Vector2 add_vec2(Vector2 a, Vector2 b) {
    return (Vector2){ .x = a.x + b.x, .y = a.y + b.y };
}
```

Unfortunately, `libload` does not support structures. The `#link` wont help here.

For this case, we have to create a wrapper file.

```c
#include "test.c"
#include <stdio.h>
#include <stdlib.h>

static char buffer[64];  // enough to store one Vector2 as string

const char* add_vec2_wrapper(int a_x, int a_y, int b_x, int b_y) {
    Vector2 va = (Vector2){ .x = a_x, .y = a_y};
    Vector2 vb = (Vector2){ .x = b_x, .y = b_y};

    Vector2 result = add_vec2(va, vb);

    snprintf(buffer, sizeof(buffer), "%d|%d", result.x, result.y);

    return buffer;
}
```

Yes, we could shift the y and add it to x and return it as single int and then parse it in lucia but that would require i64 for return and i32 for fields.

Now we write the bindings in Lucia:

```lucia
import libload

lib: &int = libload.load_lib(__dir__+"wrapper.so")

add_vec2_c: &int = libload.get_fn(lib, "add_vec2_wrapper", ["int", "int", "int", "int"], "str")

typedef struct Vector2 = {
    x: int,
    y: int,
}

fun add_vec2(a: Vector2, b: Vector2) -> Vector2:
    result_str: str = libload.parse_str_ptr(libload.call_fn(add_vec2_c, [a.x, a.y, b.x, b.y]))
    parts: list[str] = result_str.split("|")
    return Vector2 {
        x = parts[0] as int,
        y = parts[1] as int,
    }
end

v1: Vector2 = Vector2 { x = 10, y = 20 }
v2: Vector2 = Vector2 { x = 5, y = 7 }
result: Vector2 = add_vec2(v1, v2)
println(result)
```

Output:

```output
Vector2 { y = 27, x = 15 }
```

I think its pretty self explanatory.

Supported types by `libload`:

| Libload Type            | C Type            | Parsing                      |
| ----------------------- | ----------------- | ---------------------------- |
| `"int"`                 | `int`             | `x as int`                   |
| `"float"`,`"float32"`   | `float`           | `x as float`                 |
| `"double"`, `"float64"` | `double`          | `x as float`                 |
| `"bool"`                | `bool`, `_Bool`   | `x as bool`                  |
| `"void"`                | `void`            | `x as void`                  |
| `"byte"`                | `uint8_t`, `char` | `x as int`                   |
| `"str"`, `"ptr"`        | `char*`           | `libload.parse_str_ptr(x)`   |
| `"any"`                 | `void*`           | none                         |
| `"array"`               | `T[]`             | `libload.parse_array_ptr(x)` |
