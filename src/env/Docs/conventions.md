# Conventions

## Conventions in Lucia

Lucia has a set of **conventions**, which are like rules but you don't have to follow them.  
They exist just for you and the people reading your code to faster understand your code.  

This file assumes that you read and understood the [language syntax rules and specifications](./language-syntax.md).  
If you don't please go read it first.  

To refer to this file use the identifier `LC2C` (Lucia 2.0.0 Convention)

## Whitespace

Lucia uses `4` space indentation, not `2`.
Spaces are preferred more than `TAB`

After each `:` used for a `code block` should be a newline.  
Max line length is 100 characters

There should always be a blank line before a function/generator/type/macro definition and between logical blocks (if, while, for, ...)  

No trailing spaces

And always a newline at the end of a file  

## Names

### Variable names

Variable names should be [snake_case](https://en.wikipedia.org/wiki/Snake_case)  

Example:  

```lucia
letter_count: int
// instead of LetterCount or letterCount or letter count or anything else
// also you CANNOT use spaces in variable names
```

Emoji names for variables are allowed in Lucia, but for readability and clarity they aren't recommended  

Boolean values should be prefixed with one of these:  

- `is_`
- `has_`
- `can_`
- `was_`
- `do_`
- `should_`

And other prefixes that make sense  

Example:

```lucia
is_alphabetical: bool
```

### Constants (finals)

Constants should be in [SCREAMING_SNAKE_CASE](https://en.wikipedia.org/?title=SCREAMING_SNAKE_CASE&redirect=no)  

Example:  

```lucia
final VERSION: str = "2.0.0"
final APPLICATION_NAME: str = "Lucia"
```

### Modules

Modules should be in [snake_case](https://en.wikipedia.org/wiki/Snake_case) 

Example:  

```lucia
// standard modules
import math
// custom module
import text_utils  // text_utils.lc or text_utils/__init__.lc
```

### Type names

Type names should be in [PascalCase (UpperCamelCase)](https://en.wikipedia.org/wiki/Camel_case)  

Example:

```lucia
typedef IntAlias = int
// instead of intAlias or int_alias or anything else
typedef struct Point = {
    x: int,
    y: int
}
```

All types should be in [PascalCase (UpperCamelCase)](https://en.wikipedia.org/wiki/Camel_case), except standard types or pure aliases to std types.

```lucia
// std/types.lc
typedef ptr = &any
```

### Function/Generator/Macro names

Function/Generator/Macro names should be in [snake_case](https://en.wikipedia.org/wiki/Snake_case) too  

and functions that do single actions should be named by this table:  

| Action        | Name (prefix) |
|---------------|---------------|
| print         | `print_`      |
| sort          | `sort_`       |
| getter (bool) | `is_`         |
| getter (data) | `get_`        |
| setter (data) | `set_`        |
| conversion    | `to_`, `as_`  |

And other reasonable prefixes.

Example:

```lucia
// instead of printData or PrintData or printdata or anything else
fun print_data(data: list) -> void:
    print(data)
end
```

### Methods

Methods should be also in [snake_case](https://en.wikipedia.org/wiki/Snake_case), although in the early versions of Lucia [PascalCase](https://en.wikipedia.org/wiki/Camel_case) was used  

Example:

```lucia
10.to_string()
```

### Generic names

Generics should be named with single uppercase letter.

Example:

```lucia
typedef enum Option<T> = {
    Some(T)
    None
}
```

## Ifs

Use `if` for big if statements like  

```lucia
if (x > 69):
    y := x - 3
    print(y)
    for i in [0..y]:
        z *= i
    end
end
```

And use `if-then` for small if statements like  

```lucia
y := if (x > 69) then x - 3 else x
```

Also use `-li` suffix only where it's cleaner, otherwise always use `if-then`  

```lucia
// x - 3 otherwise null
y: ?int = (x > 69)-li x - 3
```

## Macros

Use only necessary macro modifiers, don't spam them  

```lucia
#macro #unsafe #group #mangle #contextual is_ptr!($a):
    $a is &any
#endmacro
```

In this case is really needed only `#unsafe`, because group and mangle are defaulty on and contextual is not used here.  

### Macro bracket types

We use macro bracket types to differentiate between macros and macro uses

There are 4 macro bracket types

- `()`
- `[]`
- `{}`
- `<>`

`()` are used for normal macros. Just regular macros nothing special

`[]` are used for collection types and array operations

`{}` are used to signal to user that they can put a large code chunk into it, so it doesn't take a expression, but statements

`<>` are used for foreign code, like sql

`()` Are preferred.  

Examples:  

```lucia
#macro log!($x):
    print(f"[LOG] $x$")
#endmacro
```

```lucia
#macro vec![$els...]:
    Vec.from([$els])
#endmacro
```

```lucia
#macro execute!{$code}:
    $code
#endmacro
```

```lucia
#macro sql!<$code>:
    sql.exec($code)
#endmacro
```

## Chaining

Lucia allows you to chain methods, pipes and operators  
You should always use 4 space indentation for chaining
Always put `.` or `|>` or operator at the next line not at the end of previous line

### Method chaining

A line shouldn't be longer than 100 characters

You should rewrite code like:  

```lucia
a := items.into_gen().map((x) => x + 1).filter((x) => (x % 2 == 0)).collect_into(float)
```

into  

```lucia
a := items
    .into_gen()
    .map((x) => x + 1)
    .filter((x) => (x % 2 == 0))
    .collect_into(float)
```

Very similar to rust.

### Pipe chaining

You should rewrite code like:  

```lucia
a := 10 |> add(_, 20) |> multiply(50) |> sum(_, ~items) |> sub(10, _) |> print_and_ret()
```

into  

```lucia
a := 10
    |> add(_, 20)
    |> multiply(50)
    |> sum(_, ~items)
    |> sub(10, _)
    |> print_and_ret()
```

### Operator chaining

Well basically the same.
Just put on a new line if char count above 100.

## File structure

At the very top of a file there should be:

1. Imports (`import module_name`)
2. Includes (`#include "module"`)
3. [File comment](#file-comment)
4. Constants (finals)

Example:  

```lucia
import math
import os
#include <std/assert>

// Dev - This file does things
// This file is for doing stuff with other stuff
// stuff_module@0.1.69

final VERSION: str = "0.1.69"
```

Imports/includes don't need to be sorted, but if you want then alphabetically is preferred.
Imports and includes should **always** be on top of the file, never inline.  

Each thing is optional and you can leave out anything you want.

### File comment

File comment is a comment that says what the file is supposed to do and the package it is in.

Structure:  

```lucia
// my_file.lc - small description
// Longer description if needed
// AuthorName package_name@version
```

### Section headers

Section headers are used to section a file into multiple sections

Structure:

```lucia
// ========================
// Section name
// ========================
```

Note that the amount of `=` is dependent on the length of the section name

### Comments

Comments are used to say stuff for others people reading your horrible code.  
The Lucia interpreter ignores ALL comments.  

Comment types:  

- `//` normal single line comments
- `///` documentation comments
- `/* */` multiline comments
- `<# #>` inline comments

Examples:  

```lucia
// this is a comment
print("hello world")
```

```lucia
/// this function does stuff
/// @param a: int - how many times to do a thing
fun thingy_maker(a: int):
    /* code */
end
```

```lucia
/*
This is a multiline comment
This is second line of a multiline comment
*/
```

```lucia
a := 10 <# 10 is the base number #> + 20
```

Doc comments are for:

- Function
- Generators
- Macros
- Types
- Constants
- Modules
- Public variables
- Struct method implementations (`for`)
- Generics

Doc comment tags are:

- `@param` - @param name: T - desc
- `@return` - @return T - desc
- `@generic` - @generic name - desc
- `@example` - @example desc - newline code newline
- `@see` - @see reference
- `@deprecated` - @deprecated reason
- `@since` - @since version
- `@author` - @author name
- `@version` - @version version
- `@throws` - @throws ExceptionType - desc
- `@note` - @note desc
- `@warning` - @warning desc
- `@bug` - @bug desc
- `@todo` - @todo desc
- `@link` - @link URL desc
- `@license` - @license license_name

### File extension

File extension `.lc` is preferred (instead of `.lucia`).

## Error handling

Function that may fail should be marked with `?` after the parameters  

Example:  

```lucia
fun may_fail()? -> int
    /* 
    code that may result in a exception being thrown
    and not being captured by the function itself
    */
end
```

This does not change how the function works, its only for the static analysis to give you a warning if not handled properly.  

### `??` operator

Don't use this operator for handling errors because it is **deprecated** and is not clear. Use `? or` instead

```lucia
// bad
a := may_fail() ?? 10
// good
a := may_fail()? or 10
```

## Strings

For strings use `"` and not `'`. Although Python prefers `'`, Lucia prefers `"`.

There are also no `"""`, but when you try to execute it works because it gets parsed as `"" " string " ""`. Essentially creating 3 strings.

### F-strings

Always use f-strings. There is no reason to concat a string using `+`.
Keep them simple and not nested too deep.

## Testing

For tests use `std/assert` macros.  
Test function should start with `test_`  

```lucia
#include <std/assert>

fun test_addition() -> void:
    assert_eq!(1 + 3, 4)
end
```

## Nesting

Never nest too deep.  
Max nested depth should be `3`  
If you don't know how to *not* nest something, i really recommend you watching [Why You Shouldn't Nest Your Code](https://www.youtube.com/watch?v=CFRhGnuXG-4)

## Structs/Enums

In structs/enums you should always put the `=` after the name.  
There should never be a trailing comma after the fields of struct or enum.  
In fact there should **never** be a trailing comma.  

Example:  

```lucia
                  // V -- this one
typedef struct Point = {
    x: int,
    y: int
}
```
