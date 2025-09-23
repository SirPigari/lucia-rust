# Conventions

## Conventions in Lucia

Lucia has a set of `conventions`, which are like rules but you don't have to follow them.  
They exist just for you and the people reading your code to faster understand your code.  

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

### Type names

Type names should be in [PascalCase (UpperCamelCase)](https://en.wikipedia.org/wiki/Camel_case)  

Example:

```lucia
typedef IntAlias = int
// instead of intAlias or int_alias or anything else
typedef struct Point = {
    x: int,
    y: int,
}
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

And other reasonable prefixes.

Example:

```lucia
// instead of printData or PrintData or printdata or anything else
fun print_data(data: list) -> void:
    print(data)
end
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
