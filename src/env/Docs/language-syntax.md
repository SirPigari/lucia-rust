# Syntax

---------------

## Content

---------------

- [Syntax](#syntax)
  - [Content](#content)
  - [Getting started](#getting-started)
    - [1. Install Lucia](#1-install-lucia)
    - [2. Create a "Hello, World!" Program](#2-create-a-hello-world-program)
    - [3. Run the Program](#3-run-the-program)
  - [Variables \& Types](#variables--types)
    - [Types](#types)
      - [Simple types](#simple-types)
      - [Reference types](#reference-types)
      - [Maybe types](#maybe-types)
      - [Union types](#union-types)
  - [Numbers](#numbers)
    - [Integer, Float, and Decimal Usage](#integer-float-and-decimal-usage)
    - [Complex Numbers](#complex-numbers)
    - [Base Notations](#base-notations)
    - [Integer Multiplication Shortcut](#integer-multiplication-shortcut)
  - [Strings](#strings)
    - [F-Strings](#f-strings)
    - [Raw string](#raw-string)
    - [Byte string](#byte-string)
    - [String prefixes](#string-prefixes)
  - [Tuples](#tuples)
  - [Lists](#lists)
    - [Pattern recognition](#pattern-recognition)
  - [Maps](#maps)
  - [Functions](#functions)
    - [Modifiers](#modifiers)
    - [Return values](#return-values)
    - [Parameters](#parameters)
    - [Default parameters](#default-parameters)
    - [Mutable parameters](#mutable-parameters)
    - [Recursion](#recursion)
    - [Functions as values](#functions-as-values)
    - [Variadic functions](#variadic-functions)
    - [Argument unpacking](#argument-unpacking)
    - [Control flow and unreachable code](#control-flow-and-unreachable-code)
    - [Function types](#function-types)
      - [Multiple parameters](#multiple-parameters)
      - [No parameters](#no-parameters)
      - [Return type rules](#return-type-rules)
      - [Function types as values](#function-types-as-values)
      - [Function types and unions](#function-types-and-unions)
      - [Variadic function types](#variadic-function-types)
    - [Built-in functions](#built-in-functions)
    - [Effects](#effects)
      - [Declaring effects](#declaring-effects)
      - [Unsafe effect](#unsafe-effect)
      - [Failing functions](#failing-functions)
      - [IO, state, and pure](#io-state-and-pure)
      - [Default effect](#default-effect)
      - [Effect detection](#effect-detection)
      - [FFI and `all`](#ffi-and-all)
      - [Effect table](#effect-table)
  - [Generators](#generators)
    - [Declaring generators](#declaring-generators)
    - [Using generators](#using-generators)
    - [Ranges as generators](#ranges-as-generators)
    - [Static generators](#static-generators)
    - [Generator methods](#generator-methods)
    - [Generator transformations](#generator-transformations)
    - [Standard for-loop generators](#standard-for-loop-generators)
    - [Generator types](#generator-types)
  - [References](#references)
    - [Declaring references](#declaring-references)
    - [Modifying via references](#modifying-via-references)
    - [Nested references](#nested-references)
  - [Operators](#operators)
    - [Arithmetic Operators](#arithmetic-operators)
    - [Bitwise Operators](#bitwise-operators)
    - [Comparison Operators](#comparison-operators)
    - [Logical Operators](#logical-operators)
    - [Membership Operators](#membership-operators)
    - [Type Checking Operators](#type-checking-operators)
    - [Pipeline Operator](#pipeline-operator)
    - [Increment / Decrement Operators](#increment--decrement-operators)
    - [Block Syntax](#block-syntax)
    - [Example of a Block](#example-of-a-block)
    - [Why Use Blocks?](#why-use-blocks)
  - [Indentation](#indentation)
    - [Example](#example)
    - [Summary](#summary)
  - [If Statements](#if-statements)
    - [1. Basic `if`](#1-basic-if)
    - [2. `if` with `else`](#2-if-with-else)
    - [3. `if` with `else if`](#3-if-with-else-if)
    - [4. Multiple `else if` with `else`](#4-multiple-else-if-with-else)
    - [5. Nested / Separate `if` statements](#5-nested--separate-if-statements)
    - [6. `if` as an expression](#6-if-as-an-expression)
    - [7. Nested expressions](#7-nested-expressions)
    - [8. Inline functions using `if`](#8-inline-functions-using-if)
    - [9. `else if` in expressions](#9-else-if-in-expressions)
    - [10. `-li` operator](#10--li-operator)
  - [For Loops](#for-loops)
    - [Basic `for` Loop](#basic-for-loop)
    - [Multiple Statements in Body](#multiple-statements-in-body)
    - [Pattern-Based `for` Loop](#pattern-based-for-loop)
    - [C-Style `for` Loop](#c-style-for-loop)
    - [For Loops Notes](#for-loops-notes)
  - [While Loops](#while-loops)
    - [Basic `while` Loop](#basic-while-loop)
    - [Controlled with Flags](#controlled-with-flags)
    - [Using `break`](#using-break)
  - [Forget](#forget)
    - [Forget a slice of a list](#forget-a-slice-of-a-list)
    - [Forget the entire variable](#forget-the-entire-variable)
    - [Forgeting mutliple variables](#forgeting-mutliple-variables)
  - [Try and catch](#try-and-catch)
    - [Basic `try` / `catch`](#basic-try--catch)
    - [Catching specific elements](#catching-specific-elements)
    - [Optional `try` without catch](#optional-try-without-catch)
    - [Suppressing errors with `?`](#suppressing-errors-with-)
  - [Throw](#throw)
    - [Throwing a tuple](#throwing-a-tuple)
  - [Comments](#comments)
    - [Single-line Comment](#single-line-comment)
    - [Multi-line Comment](#multi-line-comment)
    - [In-line Comment](#in-line-comment)
  - [Lambda Functions](#lambda-functions)
    - [Examples](#examples)
  - [Where and then](#where-and-then)
    - [1. Where — Local Bindings](#1-where--local-bindings)
    - [2. `then` — Chained Transformations](#2-then--chained-transformations)
    - [3. Combining `where` and `then`](#3-combining-where-and-then)
  - [Tuple Unpacking / Multiple Assignment](#tuple-unpacking--multiple-assignment)
    - [1. Basic Swapping / Multiple Assignment](#1-basic-swapping--multiple-assignment)
    - [2. Mixed Expressions](#2-mixed-expressions)
    - [3. `final` and `mutable` Modifiers](#3-final-and-mutable-modifiers)
    - [4. Sequence / String Unpacking](#4-sequence--string-unpacking)
    - [5. Single-element Tuples](#5-single-element-tuples)
    - [6. List Unpacking](#6-list-unpacking)
  - [Defer](#defer)
  - [Scopes](#scopes)
  - [Groups](#groups)
  - [Importing Modules](#importing-modules)
  - [Match](#match)
    - [Matching Literals](#matching-literals)
    - [Capturing Values](#capturing-values)
    - [Matching Against a Variable](#matching-against-a-variable)
    - [Guards and Continue](#guards-and-continue)
    - [Breaking Out Early](#breaking-out-early)
    - [Union Matches](#union-matches)
    - [Combined Example](#combined-example)
  - [Indexing and Slicing](#indexing-and-slicing)
    - [Basic Indexing](#basic-indexing)
    - [Slicing](#slicing)
    - [Index Assignment](#index-assignment)
    - [Nested Indexing](#nested-indexing)
    - [Map Indexing](#map-indexing)
  - [Enums](#enums)
    - [Basic Enum](#basic-enum)
    - [Enum with Data](#enum-with-data)
    - [Pattern Matching](#pattern-matching)
    - [Discriminants](#discriminants)
  - [Structs](#structs)
    - [Defining and Instantiating](#defining-and-instantiating)
    - [Methods and Static Functions](#methods-and-static-functions)
    - [Mutable Fields](#mutable-fields)
    - [Instance Methods](#instance-methods)
    - [Field Validation](#field-validation)
    - [Equality](#equality)
    - [Overriding Methods](#overriding-methods)
    - [Casting](#casting)
  - [Interfaces on structs](#interfaces-on-structs)
    - [Basic Interface](#basic-interface)
    - [Structs Implementing Interfaces](#structs-implementing-interfaces)
    - [Type Safety](#type-safety)
    - [Passing Interfaces to Functions](#passing-interfaces-to-functions)
  - [Operator Interfaces (`impl add`, `impl sub`, etc.)](#operator-interfaces-impl-add-impl-sub-etc)
  - [Static Factory Interface (`impl static new[] -> any`)](#static-factory-interface-impl-static-new---any)
  - [Generics](#generics)
    - [Generic Structs](#generic-structs)
    - [Generic Enums](#generic-enums)
      - [Single type parameter](#single-type-parameter)
      - [Two type parameters](#two-type-parameters)
    - [Generic Constraints](#generic-constraints)
  - [Preprocessor](#preprocessor)
    - [Including files](#including-files)
    - [Defines and aliases](#defines-and-aliases)
    - [Conditional code](#conditional-code)
    - [Precompile blocks](#precompile-blocks)
    - [Token injection](#token-injection)
    - [Preprocessor errors](#preprocessor-errors)
    - [Link preprocessor (FFI)](#link-preprocessor-ffi)
  - [Macros](#macros)
    - [Defining a macro](#defining-a-macro)
    - [Hygiene and name mangling](#hygiene-and-name-mangling)
    - [Grouping behavior](#grouping-behavior)
    - [Bracket types](#bracket-types)
    - [Variadic macros](#variadic-macros)
    - [Token inspection](#token-inspection)
    - [Deprecated macros](#deprecated-macros)
    - [Standard library and macros](#standard-library-and-macros)

---------------

## Getting started

---------------

To write your first program in Lucia, follow the steps below:

### 1\. Install Lucia

First, you need to install the Lucia language. Follow the [installation guide](installation-guide.md) to set it up on your system.

You can also customize your Lucia preferences by modifying the [config.json](../config.json) file. For details on how to configure the file, refer to the [Guide to `config.json`](config-guide.md).

### 2\. Create a "Hello, World!" Program

Create a new file named `hello.lc` and write the following code:

```lucia
println("Hello, World!")
```

### 3\. Run the Program

Once the program is saved, you can run it by executing the following command in your terminal:

```bash
lucia hello.lc
```

Output:

```output
Hello, World!
```

## Variables & Types

In Lucia, you define variables like:

```template
name: type = value
```

If you don't want to specify the type yourself (eg its big, or you dont know, or youre just lazy) you can use the dick operator `:=`

```template
name := value
```

This will always work no matter the value

Accessing variables is simple

```template
name
```

And writing to already declared variable is also very simple

```template
name = new_value
```

Variables can have name in any sequence of characters that does not have a space between, contains a special character (e.g.: `(`, `|`, `[`, ...) or is a keyword.

```lucia
🔥 := "fire emoji"
変数: str = "变量"
```

Just make sure `new_value` is allowed by the type of the variable and `name` was defined before.

### Types

#### Simple types

Simple types are types that consist of a single token:

| Name           | Description                                                                                                                                   |
| -------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| **auto**:      | Same as the dick operator                                                                                                                     |
| **any**:       | Allows any value.                                                                                                                             |
| **void**:      | Represents the absence of value. Has only one value possible and that is `null`                                                               |
| **bool**:      | Boolean value (`true` or `false`)                                                                                                             |
| **int**:       | Whole numbers (42, 13, -5, 69). Not limited by any size, backed by [BigInt](https://docs.rs/num-bigint/latest/num_bigint/)                    |
| **float**:     | Decimal numbers (3.14, 6.9, -0.001). Not limited by precision nor size, backed by [BigDecimal](https://docs.rs/bigdecimal/latest/bigdecimal/) |
| **str**:       | Text value ("Hello world"), refer to [Strings](#strings)                                                                                      |
| **tuple**:     | [Tuples](#tuples)                                                                                                                             |
| **list**:      | [Lists](#lists)                                                                                                                               |
| **map**:       | [Maps](#maps)                                                                                                                                 |
| **bytes**:     | A sequence of bytes                                                                                                                           |
| **type**:      | Refering to the type, type itself is a type. (`int` is type)                                                                                  |
| **function**:  | [Function](#function-types)                                                                                                                   |
| **generator**: | [Generator](#generator-types)                                                                                                                 |
| **module**:    | Refering to imported module (after `import math`, the `math` is now of this type)                                                             |

#### Reference types

A reference type is a type that has a pointer as its value

```template
p: &T = &v
```

For example

```lucia
p: &int = &42
```

For now, references are only allowed if `allow_unsafe` is true in the config. This will be removed in the future.

See [References](#references)

#### Maybe types

A maybe type is a type that can be either T or null.

```template
p: ?T = v
b: ?T = null
```

For example:

```lucia
p: ?int = 42
b: ?int = null
```

#### Union types

A union type allows a variable to hold a value of one of several specified types

```template
u: M | N = m
u = n
```

For example:

```lucia
u: int | str = 10
u = "hello"
```

> NOTE  
> All types last even after assignment.
> For example:
>
> ```lucia
> p: ?int = 42
> p = null // still valid
> ```

## Numbers

Lucia supports integers, floats, complex numbers, and very good numeric notations.

### Integer, Float, and Decimal Usage

```lucia
a: int = 42
b: float = 3.14
c: float = .69   // same as 0.69

println(a)  // 42
println(b)  // 3.14
println(c)  // 0.69
```

- `_` can separate digits: `1_000_000 = 1000000`
- Decimal without leading zero is allowed: `.123 = 0.123`
- Recurring decimals: `0.4(9) = 0.5`

### Complex Numbers

[Complex numbers](https://en.wikipedia.org/wiki/Complex_numbers) are of type `float`.

```lucia
x: complex = 3 + 4i
y: complex = 2 - 3i
z: complex = 1i

println(x) // 3 + 4i
println(y) // 2 - 3i
println(z) // 0 + 1i
```

`i` is the imaginary unit, not as other languages tend to use `j`. WHY IN THE HELL WOULD YOU USE J WHAT DOES THAT HAVE TO DO WITH ANYTHING  
Supports arithmetic:

```lucia
sum: float = (3 + 2i) + (1 + 4i)     // 4 + 6i
prod: float = (1 + 2i) * (3 + 4i)    // -5 + 10i
quot: float = (3 + 2i) / (1 - 1i)    // 0.5 + 2.5i
abs_val: float = |3 + 4i|              // 5.0
conj_val: float = math.conj(3 + 4i)  // 3 - 4i
```

### Base Notations

```lucia
hex: int = 0xFF      // 255, hexadecimal
oct: int = 0o17      // 15, octal
bin: int = 0b1010    // 10, binary

custom: int = 36#z   // 35, base 36
```

- Use `base#digits` for **custom bases** (1–62).

### Integer Multiplication Shortcut

```lucia
result: int = 2(3)    // 6, equivalent to 2 * 3
```

- Parentheses after an integer multiply it directly by the number inside.

## Strings

A string is basically just a text. They are encoded using UTF-8.

You can define a string like:

```lucia
"hello"
```

and assign it to a variable

```lucia
s: str = "hello"
```

Strings primarely use `"` for quotes, but you can also use `'`.
> NOTE  
> `'` are planned to be used for characters instead of strings. This is only planned

### F-Strings

Lucia supports f-strings for string interpolation, allowing you to embed expressions directly within strings using curly braces `{}`.  
F-strings have the prefix `f`.

```lucia
name: str = "World"
f"Hello {name}!"
```

```lucia
age: int = 42
f"My dad is {age} years old!"
```

F-strings also supports formatters

```lucia
n := 69
s: str = f"{n} in hex is {n :: x}"
```

Lucia uses `::` as the separator for formatters in F-strings

Here is a table for reference:

| Syntax          | Description                                           | Example        |
| --------------- | ----------------------------------------------------- | -------------- |
| `?`             | Debug / raw representation, ignores normal formatting | `{value :: ?}` |
| `<`             | Left align within width                               | `{x :: <10}`   |
| `>`             | Right align within width (default)                    | `{x :: >10}`   |
| `^`             | Center align within width                             | `{x :: ^10}`   |
| `<fill><align>` | Custom fill character with alignment                  | `{x :: _>10}`  |
| `<width>`       | Minimum field width                                   | `{x :: 8}`     |
| `+`             | Always show sign for numeric values                   | `{n :: +}`     |
| `-`             | Negative numbers only (default behavior)              | `{n :: -}`     |
| `b`             | Binary integer format                                 | `{n :: b}`     |
| `o`             | Octal integer format                                  | `{n :: o}`     |
| `x`             | Hexadecimal lowercase                                 | `{n :: x}`     |
| `X`             | Hexadecimal uppercase                                 | `{n :: X}`     |
| `.<digits>f`    | Fixed-point float format                              | `{n :: .3f}`   |
| `.<digits>F`    | Fixed-point float, uppercase                          | `{n :: .3F}`   |
| `.<digits>e`    | Scientific notation lowercase                         | `{n :: .2e}`   |
| `.<digits>E`    | Scientific notation uppercase                         | `{n :: .2E}`   |
| `u` / `U`       | Convert output to uppercase                           | `{s :: u}`     |
| `l` / `L`       | Convert output to lowercase                           | `{s :: l}`     |

### Raw string

A raw string is a string that does not need escaping for special characters.  
Raw strings use the prefix `r`.

```lucia
r: str = r"\"
```

Normally you would have to write `\\` to escape it but in raw strings you don't have to.

### Byte string

A byte string looks like a string but turns into `bytes` at runtime.  
Byte strings have the prefix `b`.

```lucia
b: bytes = b"these are bytes"
```

Very simple

### String prefixes

There are only the above mentioned prefixes (`f`, `r`, `b`).
You can use more of them at once:

```lucia
fr"\ {1 + 1}"
bf"{1 + 1}"  // order matters, you must do bf not fb
br"\"
```

## Tuples

Tuples are ordered sequences of elements. They can hold elements of multiple types but they always have the same size.

```lucia
(1, 2)
```

They can hold any type combination

```lucia
(1, 2.0, "hello")
```

Their type is typed like:

```lucia
t: (int, float, str) = (1, 2.0, "hello")
```

Tuples are mainly used as a return value

## Lists

Lists are ordered, mutable sequences of elements. They can hold elements of multiple types, and their size can change dynamically.

Basic syntax:

```lucia
[1, 2, 3]
[1, 2.0, "hello"]
```

Typed lists:

```lucia
t: list[int, float, str] = [1, 2.0, "hello"]
```

### Pattern recognition

The pattern recognition allows you to **define a sequence by providing a few starting values**, and Lucia will automatically continue the sequence according to the pattern you provide.

> Fun fact:  
> It is called `pattern reg` because i misspelled the word **recognition** as **regocnition** and didnt notice so it stuck and now its called that.

Rules / behavior not obvious from the examples:

1. **Step / difference inference**

   - For arithmetic sequences, the difference between consecutive elements is inferred from the initial seed values.

     ```lucia
     [2, 4, 6...12]  // difference is 2
     ```

   - For geometric sequences, the ratio between consecutive elements is inferred.

     ```lucia
     [3, 9, 27...243]  // ratio is 3
     ```

2. **Multiple seeds**

   - You can provide 2 or more seed values. The sequence generator will use the pattern formed by the seeds.
   - If the seeds do **not** form a consistent pattern, a `RangeError` or `PatternError` is thrown.

3. **Inclusive end / length**

   - You can optionally provide the last value or the number of elements with `...; N`.

     ```lucia
     [1, 2, 4...; 5]  // continue pattern until 5 elements
     ```

4. **Overflow / limits**

   - The sequence can fail if it grows beyond the `f64` limit.

5. **Type inference**

   - The type of the resulting list is inferred from the seeds and must be compatible with all subsequent generated values.

6. **Errors / edge cases**

   - Seed values with inconsistent steps: `[1, 3, 6...; 5]` → RangeError
   - Sequence exceeding numeric limits: `[1, 2...; 1e308]` → PatternError

## Maps

Maps are a key-pair value structure, backed by a hashmap.

```lucia
{
    "a": 10,
    "b": 69,
}
```

They can be typed like

```lucia
t: map[str, int] = {"a": 10, "b": 69}
```

If you want to type them explicitely they can only hold a single type for key and a single type for value. Otherwise you can just type `map`.

```lucia
t: map = {"a": 10, 3: 69}
```

## Functions

Functions are declared using the `fun` keyword.

Basic syntax:

```template
modifiers fun name(parameters...) -> return_type:
    body
end
```

Example:

```lucia
fun add(a: int, b: int) -> int:
    return a + b
end
```

### Modifiers

Supported visibility modifiers:

- `private` (default)
- `public`

Supported function modifiers:

- `static`
  Has no effect on functions outside of struct methods.

- `final`
  Prevents the function from being overridden.

- `mutable` (default)
  Allows the function to be overridden.

Example:

```lucia
public static final fun foo() -> void:
    println("hello")
end
```

### Return values

- Functions without an explicit return type default to `void`
- `null` is the default return value
- `any` is the default return type

Example:

```lucia
fun test() end
```

Equivalent to:

```lucia
fun test() -> any:
    return null
end
```

### Parameters

Parameters are declared as:

```template
name: type
```

Example:

```lucia
fun greet(name: str) -> void:
    println("Hello", name)
end
```

### Default parameters

Parameters may have default values.

- Default values are evaluated at **definition time**
- Default values may be **any expression**
- Parameters with defaults must come **after** non-default parameters

Example:

```lucia
fun repeat(s: str, times: int = 3) -> void:
    println(s * times)
end
```

### Mutable parameters

Parameters can be marked as `mutable`.

- Mutation is **local to the function**
- Nothing is passed by reference unless it is a pointer
- Literals are allowed

Example:

```lucia
fun inc(mutable x: int) -> int:
    x += 1
    return x
end
```

### Recursion

Functions may call themselves.

- [Tail Call Optimization (TCO)](https://en.wikipedia.org/wiki/Tail_call) is supported

Example:

```lucia
fun sum(n: int) -> int:
    if (n <= 0):
        return 0
    end
    return n + sum(n - 1)
end
```

### Functions as values

Functions are first-class values.

Function types are written as:

```template
function[T] -> R
```

Example:

```lucia
fun apply(f: function[int] -> int, v: int) -> int:
    return f(v)
end
```

When printed:

- Simple types use the internal `format_value`
- Structs use the `.op_display()` method

### Variadic functions

Functions may accept a variable number of arguments.

- Variadic arguments are collected into a `list`

Example:

```lucia
fun sum_all(*values: list[int]) -> int:
    return sum(values)
end
```

Call:

```lucia
sum_all(1, 2, 3, 4)
```

### Argument unpacking

The `~` operator unpacks arguments.

- Allowed **only in function calls**
- Expands **only positional arguments**

Example:

```lucia
args := [1, 2, 3]
sum_all(~args)
```

### Control flow and unreachable code

- `return` exits the function immediately
- Unreachable code is allowed
- If the experimental static checker is enabled, unreachable code emits a warning

Example:

```lucia
fun example(x: int) -> int:
    if (x < 0):
        return -1
    end
    return x
    throw "Unreachable"
end
```

### Function types

Functions have explicit, first-class types.

The general syntax for a function type is:

```template
function[ArgType1, ArgType2, ...] -> ReturnType
```

Example:

```lucia
f: function[int] -> int
```

This describes a function that:

- Takes one argument of type `int`
- Returns an `int`

#### Multiple parameters

Functions with multiple parameters list them in order:

```lucia
f: function[int, str] -> bool
```

Equivalent to a function declared as:

```lucia
fun f(a: int, b: str) -> bool: ...
```

#### No parameters

Functions that take no parameters use an empty parameter list:

```lucia
f: function[] -> void
```

Example:

```lucia
fun hello() -> void:
    println("hello")
end
```

#### Return type rules

- The return type is always explicit in the function type
- `void` means the function returns `null`
- `any` may be used as a return type

Example:

```lucia
f: function[int] -> any
```

#### Function types as values

Function types can be:

- Stored in variables
- Passed as parameters
- Returned from other functions
- Stored in containers

Example:

```lucia
fun apply(f: function[int] -> int, x: int) -> int:
    return f(x)
end
```

#### Function types and unions

Function types may appear inside unions and maybe types:

```lucia
f: (function[int] -> int) | null
g: ?(function[str] -> void)
```

#### Variadic function types

Variadic parameters are represented explicitly as lists in function types:

```lucia
f: function[list[int]] -> int
```

Corresponding declaration:

```lucia
fun f(*values: list[int]) -> int: ...
```

### Built-in functions

Built in functions are functions that you don't need to import from anywhere.

| Name           | Signature                                                                                                                                                                                                                                                                                     | Effects | Description                                                                                    |
| -------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------- | ---------------------------------------------------------------------------------------------- |
| `print`        | `print(...args: any, end: str = "", sep: str = " ") -> void`                                                                                                                                                                                                                                  | `io`    | Prints values without a trailing newline. Values are formatted using `display` when available. |
| `println`      | `println(...args: any, sep: str = " ") -> void`                                                                                                                                                                                                                                               | `io`    | Prints values followed by a newline.                                                           |
| `styled_print` | `styled_print(...args: any, sep: str = " ", end: str = "\n\\x1b[0m", fg_color: str = "reset", bg_color: str = "reset", bold: bool = false, italic: bool = false, underline: bool = false, blink: bool = false, reverse: bool = false, strikethrough: bool = false, link: any = null) -> void` | `io`    | Prints styled text using ANSI escape codes.                                                    |
| `styledstr`    | `styledstr(...args: any, sep: str = " ", end: str = "\\x1b[0m", fg_color: str = "reset", bg_color: str = "reset", bold: bool = false, italic: bool = false, underline: bool = false, blink: bool = false, reverse: bool = false, strikethrough: bool = false, link: any = null) -> str`       | `pure`  | Returns a styled string without printing it.                                                   |
| `input`        | `input(prompt: str = "", multiline: ?str = null, err: bool = false) -> str`                                                                                                                                                                                                                   | `io`    | Reads user input. Can optionally return errors instead of empty strings.                       |
| `len`          | `len(v: any) -> int`                                                                                                                                                                                                                                                                          | `pure`  | Returns the length of lists, tuples, strings, maps, bytes, or calls `op_len` on structs.       |
| `help`         | `help(value: any = null) -> void`                                                                                                                                                                                                                                                             | `io`    | Displays help for a value or shows the general Lucia help message.                             |
| `type_of`      | `type_of(obj: any) -> type`                                                                                                                                                                                                                                                                   | `pure`  | Returns the runtime type of a value.                                                           |
| `size_of`      | `size_of(obj: any) -> int`                                                                                                                                                                                                                                                                    | `pure`  | Returns the memory size of a value in bytes.                                                   |
| `sum`          | `sum(...args: any, start: any = 0) -> float`                                                                                                                                                                                                                                                  | `pure`  | Recursively sums numeric values in lists or tuples.                                            |
| `ord`          | `ord(s: str) -> int`                                                                                                                                                                                                                                                                          | `pure`  | Returns the Unicode code point of the first character in a string.                             |
| `char`         | `char(i: int) -> str`                                                                                                                                                                                                                                                                         | `pure`  | Converts a Unicode code point into a character.                                                |
| `array`        | `array(size: int, initial_value: any = null) -> list`                                                                                                                                                                                                                                         | `pure`  | Creates a list of fixed size filled with an initial value.                                     |
| `range`        | `range(a: int \| float = 0, b: int \| float, step: int \| float = 1, as: type = generator) -> list\|tuple\|generator`                                                                                                                                                                         | `pure`  | Generates a numeric range as a list, tuple, or generator.                                      |
| `complex`      | `complex(real: int \| float, imaginary: int \| float) -> float`                                                                                                                                                                                                                               | `pure`  | Creates a complex number stored internally as a float type.                                    |

### Effects

Effects are **optional annotations** that describe what a function *might* do.
They are **not required**, **not widely used**, and exist mostly as **documentation hints**.

They can also help tooling and control-flow analysis, but normal code does not depend on them.

#### Declaring effects

If a function has **no return type**:

```lucia
fun log_message() ![io]:
    println("hello")
end
```

If a function **has a return type**, effects use angle brackets:

```lucia
fun print_data(data: str) -> void <![io]>:
    println(data)
end
```

You can omit effects entirely:

```lucia
fun normal_function(x: int) -> int:
    return x * 2
end
```

#### Unsafe effect

```lucia
fun unsafe_operation() ![unsafe]:
end
```

This is the only effect that is actually enforced.

```lucia
#config allow_unsafe = false
unsafe_operation()   // error
```

```lucia
#config allow_unsafe = true
unsafe_operation()   // ok
```

#### Failing functions

`fail` is intended for **control-flow analysis only**.

```lucia
fun may_fail()?:
end
```

This is equivalent to:

```lucia
fun may_fail() ![fail]:
end
```

It indicates that execution may not continue past the call.
It does **not** force error handling.

#### IO, state, and pure

These effects exist mainly to make intent clearer:

```lucia
fun read_input() -> str <![io]>:
end

fun mutate_state() -> void <![state]>:
end

fun add(a: int, b: int) -> int <![pure]>:
    return a + b
end
```

They are **not required**, and skipping them is normal.

#### Default effect

If no effects are written, the function is treated as `unknown`.

```lucia
fun wrapper():
    mutate_state()
end
```

This is the default and most common case.

#### Effect detection

Effects can be:

- written manually, or
- inferred at **call sites**

Currently:

- detection happens only when calling functions
- future static analysis will improve this

#### FFI and `all`

```lucia
fun foreign_call() ![all]:
end
```

`all` is mainly intended for:

- FFI
- external code
- cases where effects are unclear or irrelevant

#### Effect table

| Effect    | Meaning                |
| --------- | ---------------------- |
| `pure`    | No side effects        |
| `io`      | Input/output           |
| `state`   | Modifies state         |
| `unsafe`  | Unsafe operations      |
| `fail`    | May abort control flow |
| `all`     | Any side effect        |
| `unknown` | No effect information  |

## Generators

Generators are functions or expressions that produce a sequence of values lazily.
They are first-class values in Lucia and can be iterated over or collected.

### Declaring generators

Use `gen` instead of `fun` to declare a generator:

```lucia
gen tribonacci(n: int) -> int:
    (a, b, c) := (0, 1, 1)
    for _ in [0..n]:
        return a  // yields a value
        (a, b, c) = (b, c, a + b + c)
    end
    break // optional: exit generator
end
```

- `return` inside a generator yields a value.
- `break` exits the generator early (optional).

### Using generators

Generators are iterated using:

```lucia
g: generator = tribonacci(10)
g.collect()   // returns all yielded values as a list
g.next()      // returns the next value
g.peek()      // returns the next value without consuming it
g.is_done()   // checks if the generator is finished
```

Example:

```lucia
tribonacci_gen := tribonacci(10)
assert_eq!(tribonacci_gen.collect(), [0,1,1,2,4,7,13,24,44,81,149])
```

### Ranges as generators

- `[start..end]` produces a generator for integers in the range.
- `[start..;]` produces an **infinite generator** (must `break` manually or use stopping conditions).
- `[start, step..end]` produces a stepped generator.

Example:

```lucia
g := [0..5]           // yields 0,1,2,3,4,5
t := [0,3,6..; 5]     // yields 0,3,6,9,12 as floats
```

### Static generators

- `static gen` produces a generator that **can only be iterated once**.
- Collecting or calling `next()` after the first iteration raises `StaticGeneratorError`.
- `peek()` works only for the next value of the first iteration.

Example:

```lucia
static gen static_gen():
    for i in [0..5]:
        return i
    end
end

k := static_gen()
assert_eq!(k.next(), 0)
assert_err!(k.next()) // cannot iterate again
```

### Generator methods

| Method               | Description                                                                    |
| -------------------- | ------------------------------------------------------------------------------ |
| `collect()`          | Returns a list of all remaining values                                         |
| `collect_into(Type)` | Returns a list of remaining values cast into `Type`                            |
| `next()`             | Returns the next value (raises StopIteration if done)                          |
| `peek()`             | Returns the next value without consuming it (static generator only works once) |
| `is_done()`          | Checks if generator has finished iterating                                     |

### Generator transformations

Generators support functional transformations:

- `map((x) => ...)` — applies a function to each value.
- `filter((x) => ...)` — keeps only values where the predicate is true.
- `take(n)` — consumes only the first `n` values.

Example:

```lucia
t := [1..10]
assert_eq!(t.map((x) => x*2).collect(), [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
assert_eq!(t.filter((x) => x % 2 == 0).collect(), [2, 4, 6, 8, 10])
assert_eq!(t.take(5).collect(), [1, 2, 3, 4, 5])
```

### Standard for-loop generators

Generators can also be created using a **C-style for-loop**:

```lucia
gen for_standard_gen(n: int) -> int:
    for (i: int = 0, i < n, i += 2):
        return i
    end
end

s := for_standard_gen(10)
assert_eq!(s.collect(), [0, 2, 4, 6, 8])
```

- `for` loop parameters: `(init, condition, step)`
- `return` yields values from the loop.

### Generator types

- Generators have the type:

```lucia
generator[T]
```

Example:

```lucia
t: generator[int] = tribonacci(10)
g: generator[float] = [0, 3, 6..;5]
```

- Type indicates the type of values yielded.
- Generators may be stored in variables, passed as parameters, and returned from functions.

## References

References in Lucia are **pointers** to values.
They allow indirect access and mutation of the referenced value. References are **unsafe**, so you must enable `allow_unsafe` in the config.

```lucia
#config allow_unsafe = true
```

### Declaring references

Use `&` to create a reference:

```lucia
x: int = 10
p: &int = &x
```

- `p` now points to `x`.
- Access the value with `*p`.

```lucia
assert_eq!(*p, 10)
```

### Modifying via references

You can mutate the referenced value using `*`:

```lucia
*p = 20
assert_eq!(x, 20)
```

- References can be passed to functions:

```lucia
fun set_ptr(p: &int, v: int) -> void:
    *p = v
end

set_ptr(p, 42)
assert_eq!(*p, 42)
```

### Nested references

References can point to other references:

```lucia
pp: &&int = &&30
```

- You can flatten nested references using `++`:

```lucia
flattened: int = ++pp
assert_eq!(flattened, 30)
```

- `++` works recursively for arbitrarily nested references:

```lucia
pp: &&&&&&&&&&int = &(&(&(&(&(&(&(&10)))))))
assert_eq!(++pp, 10)
```

## Operators

### Arithmetic Operators

| Operator       | Description                         | Example       | Result      |
| -------------- | ----------------------------------- | ------------- | ----------- |
| `+`            | Addition / string concatenation     | `1 + 2`       | `3`         |
| `-`            | Subtraction                         | `5.0 - 3`     | `2.0`       |
| `*`            | Multiplication / string repetition  | `"ha" * 3`    | `"hahaha"`  |
| `/`            | Division                            | `6 / 2`       | `3`         |
| `^`            | Power                               | `2 ^ 3`       | `8`         |
| `!`            | Factorial (postfix, chainable)      | `5!`          | `120`       |
| `!!`, `!!!`, … | Multi-factorial (double, triple, …) | `7!!`, `6!!!` | `105`, `18` |
| `\|x\|`        | Absolute value (nestable)           | `\| -1 \|`    | `1`         |

> Notes:
>
> - Factorials follow [**standard factorial rules**](https://en.wikipedia.org/wiki/Factorial)
> - `x!!` = multiply every second number down: `7!! = 7*5*3*1 = 105`
> - `x!!!` = multiply every third number down: `6!!! = 6*3*1 = 18`
> - Absolute values can be nested: `|-|-5|| = 5`
> - Strings can be repeated with `*`

### Bitwise Operators

| Operator | Description                 | Example       | Result |
| -------- | --------------------------- | ------------- | ------ |
| `lshift` | Left shift                  | `8 lshift 2`  | `32`   |
| `rshift` | Right shift                 | `32 rshift 1` | `16`   |
| `<<`     | Left shift (symbolic)       | `16 << 4`     | `256`  |
| `>>`     | Right shift (symbolic)      | `64 >> 3`     | `8`    |
| `bnot`   | Bitwise NOT                 | `bnot 0`      | `-1`   |
| `band`   | Bitwise AND                 | `5 band 3`    | `1`    |
| `bor`    | Bitwise OR                  | `12 bor 10`   | `14`   |
| `\|`     | Bitwise OR (outside abs)    | `6 \| 1`      | `7`    |
| `xor`    | Bitwise XOR (also boolean)  | `5 xor 3`     | `6`    |
| `xnor`   | Bitwise XNOR (also boolean) | `5 xnor 3`    | `-7`   |

### Comparison Operators

| Operator | Description      | Example  |
| -------- | ---------------- | -------- |
| `==`     | Equals           | `1 == 1` |
| `!=`     | Not equals       | `2 != 3` |
| `>`      | Greater than     | `3 > 2`  |
| `<`      | Less than        | `2 < 4`  |
| `>=`     | Greater or equal | `4 >= 4` |
| `<=`     | Less or equal    | `5 <= 5` |

### Logical Operators

| Operator | Description      | Example            |
| -------- | ---------------- | ------------------ |
| `&&`     | And              | `true && false`    |
| `\|\|`   | Or               | `false \|\| true`  |
| `!`      | Not              | `!false`           |
| `xor`    | Exclusive or     | `true xor false`   |
| `xnor`   | Exclusive nor    | `false xnor false` |
| `and`    | Alias for `&&`   | `true and false`   |
| `or`     | Alias for `\|\|` | `false or true`    |
| `nein`   | Not equals alias | `1 nein 2`         |

### Membership Operators

| Operator | Description       | Example        |
| -------- | ----------------- | -------------- |
| `in`     | Checks membership | `1 in [1,2,3]` |

### Type Checking Operators

| Operator | Description | Example      |
| -------- | ----------- | ------------ |
| `is`     | Checks type | `1 is int`   |
| `isnt`   | Checks type | `2 isnt str` |

### Pipeline Operator

| Syntax                       | Description                       | Example                                  | Result |
| ---------------------------- | --------------------------------- | ---------------------------------------- | ------ |
| `value \|> fun(...)`         | Pass value into a function        | `5 \|> add(3, _)`                        | `8`    |
| `value \|> fun(_, x, _)`     | Use `_` multiple times for piping | `5 \|> add(3, _) \|> add(_, _)`          | `16`   |
| `value \|> fun(...) \|> ...` | Chain multiple pipeline calls     | `1 \|> add(2) \|> add(3, _) \|> is_10()` | `true` |

> Notes:
>
> - `_` = placeholder for the piped value
> - Pipelines can chain multiple times

### Increment / Decrement Operators

| Operator | Description       | Example | Result         |
| -------- | ----------------- | ------- | -------------- |
| `++i`    | Prefix increment  | `++i`   | `i+1`          |
| `i++`    | Postfix increment | `i++`   | `i` then `i+1` |
| `--i`    | Prefix decrement  | `--i`   | `i-1`          |
| `i--`    | Postfix decrement | `i--`   | `i` then `i-1` |

### Block Syntax

Blocks in Lucia start with a statement that is followed by a `:`, and the body of the block is indented (optional, but recommended for readability). The block is terminated using the `end` keyword.

### Example of a Block

Here’s an example of a block used with an `if` statement:

```lucia
age: int = 18

if (age >= 18):
    println("You are an adult.")
end
```

The code within the block (in this case, the `println()` function) is indented, and the block is terminated with `end`.

### Why Use Blocks?

Blocks help organize code into logical groups. By using the `end` keyword, Lucia ensures that the boundaries of each block are clearly defined, which reduces errors and improves readability.

## Indentation

Unlike Python, Lucia does not rely on indentation to define blocks. Blocks are explicitly marked using the `:` after a statement and are terminated with the `end` keyword. While indentation is not required, it is recommended for better readability.

### Example

Both of the following examples are valid in Lucia:

```lucia
age: int = 18

if (age >= 18):
    print("You are an adult.")  
end
```

```lucia
age: int = 18

if (age >= 18): 
print("You are an adult.")  
end
```

Since blocks in Lucia are clearly defined by `:` and `end`, indentation does not affect how the code runs. However, using consistent indentation is recommended to improve readability.

### Summary

- A block in Lucia is created by using a statement followed by a colon (`:`) and indented code.
- Blocks are terminated with the `end` keyword.
- Indentation is important in Lucia, and the code inside a block must be consistently indented.
- Blocks improve code organization and readability, ensuring that related code is grouped together.

## If Statements

The `if` statement in Lucia allows conditional execution of code blocks. It can be simple, chained with `else if`, or have a final `else` block.

### 1\. Basic `if`

Executes a block if the condition is truthy:

```lucia
temperature: int = 30

if temperature > 25:
    println("It's hot today!")
end
```

Output:

```output
It's hot today!
```

### 2\. `if` with `else`

```lucia
temperature: int = 20

if temperature > 25:
    println("It's hot today!")
else:
    println("The weather is mild.")
end
```

Output:

```output
The weather is mild.
```

### 3\. `if` with `else if`

```lucia
score: int = 75

if score >= 90:
    println("Grade: A")
else if score >= 80:
    println("Grade: B")
else if score >= 70:
    println("Grade: C")
else:
    println("Grade: F")
end
```

Output:

```output
Grade: C
```

### 4\. Multiple `else if` with `else`

```lucia
day: int = 3

if day == 1:
    println("Monday")
else if day == 2:
    println("Tuesday")
else if day == 3:
    println("Wednesday")
else:
    println("Another day")
end
```

Output:

```output
Wednesday
```

### 5\. Nested / Separate `if` statements

```lucia
temperature: int = 28
humidity: int = 60

if temperature > 25:
    println("It's hot today!")
end

if humidity > 50:
    println("It's also humid.")
end
```

Output:

```output
It's hot today!
It's also humid.
```

### 6\. `if` as an expression

```lucia
state: bool = true
message: str = if state then "System is online" else "System is offline"
println(message)
```

Output:

```output
System is online
```

- Can be used with **any type**, including unions:

```lucia
flag: bool = false
result: int | str = if flag then 1 else "No value"
println(result)
```

Output:

```output
No value
```

### 7\. Nested expressions

```lucia
age: int = 17
status: str = if age >= 18 then "adult" else if age >= 13 then "teen" else "child"
println(status)
```

Output:

```output
teen
```

### 8\. Inline functions using `if`

```lucia
check_sign: function = (x: int) => if x > 0 then "positive" else "non-positive"
println(check_sign(5))   // positive
println(check_sign(-2))  // non-positive
```

### 9\. `else if` in expressions

```lucia
score: int = 88
grade: str = if score >= 90 then "A" else if score >= 80 then "B" else "C"
println(grade)
```

Output:

```output
B
```

### 10\. `-li` operator

The `-li` operator comes from the czech **-li** suffix for creating conditional verbs

[WordReference Formus Thread](https://forum.wordreference.com/threads/li-chce%C5%A1-li.2747732/) about **-li** suffix

```lucia
state: bool = true
message: ?str = null

fun print_status(state: bool) -> str:
    if state:
        println("System online")
        return "System online"
    else:
        println("System offline")
        return "System offline"
    end
end

// Executes only if `state` is true
state-li message = print_status(state)
println(message)  // System online

// Executes block only if `!state` is true
state = false
(!state)-li:
    print_status(state)
    message = "Manually set offline"
end
println(message)  // Manually set offline
```

## For Loops

### Basic `for` Loop

Iterates over a range:

```lucia
outputs: list = []

for (i in [0..10]):
    outputs = outputs + [i.to_string()]
end

assert_eq!(outputs, (["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]))
```

### Multiple Statements in Body

```lucia
outputs = []

for (i in [0..10]):
    outputs = outputs + [(i % 2).to_string()]
    outputs = outputs + [f"hello number {i}"]
end
```

### Pattern-Based `for` Loop

Destructuring of complex structures:

```lucia
pairs: list[list[int, tuple[int, int]]] = [[1, (2, 3)], [4, (5, 6)], [7, (8, 9)]]
sums: list[int] = []

for ([a, (b, c)] in pairs):
    sums = sums.append(a + b + c)
end

assert_eq!(sums, ([6, 15, 24]))
```

### C-Style `for` Loop

Classic style with init, condition, step:

```lucia
output: str = "Hello,World!"

for (i: int = 0, i < 5, i++):
    output = output + i.to_string()
end

assert_eq!(output, "Hello,World!01234")
```

### For Loops Notes

- Loop variables **don't overwrite outer variables**:

```lucia
i: int = 100
j: int = 69

for (i in [0..3]):
    j += i
end

assert_eq!(i, 100)
assert_eq!(j, 75)
```

- Use `break` to exit early, `continue` to skip iteration:

```lucia
h: list = []
for (i in [0..10]):
    if (i == 5):
        continue
    end
    h = h.append(i)
end

assert_eq!(h, ([0, 1, 2, 3, 4, 6, 7, 8, 9, 10]))
```

- Strings can be iterated character by character:

```lucia
output: str = ""
for (ch in "Hello, World!"):
    if (ch == ' '):
        continue
    end
    output = output + ch.to_string()
end

assert_eq!(output, "Hello,World!")
```

## While Loops

### Basic `while` Loop

```lucia
i: int = 0
while (i < 20):
    i += 1
end
```

### Controlled with Flags

```lucia
running: bool = true
while (running):
    i += 1
    if (i >= 40):
        running = false
    end
end
```

### Using `break`

```lucia
t: int = 0
while (true):
    t += 1
    if (t == 10):
        break
    end
end

assert_eq!(t, 10)
```

## Forget

The `forget` statement **removes a variable or slice from memory**, making it inaccessible. Attempting to use it afterward will throw an error.

### Forget a slice of a list

```lucia
a: list[int] = [0..5].collect()
assert_eq!(a, ([0, 1, 2, 3, 4, 5]))

forget a[2..4]  // removes indices 2 and 3
assert_eq!(a, ([0, 1, 4, 5]))
```

- `a[2..4]` removes the elements at indices 2 and 3 (`2` and `3`).
- The original list `a` is modified in-place.

### Forget the entire variable

```lucia
forget a

assert_err!(a)  // `a` no longer exists, accessing it throws a NameError
```

After `forget a`, the variable `a` is removed.

### Forgeting mutliple variables

```lucia
forget (a, b)

assert_err!(a + b)
```

After `forget (a, b)`, the variable `a` and the varbaile `b` are gone.

## Try and catch

The `try` statement **catches runtime errors**, allowing you to handle exceptions.

### Basic `try` / `catch`

```lucia
try:
    1 / 0
catch (e):
    println(e)  // prints: ("ZeroDivisionError", "Division by zero.")
end
```

Catch blocks receive the error as a tuple.

### Catching specific elements

```lucia
try:
    0 / 0
catch (err_type, err_msg):
    assert_eq!(err_type, "ZeroDivisionError")
    assert_eq!(err_msg, "Division by zero.")
end
```

- You can destructure errors into type, message, and help:
  `(err_type, err_msg, err_help)`

### Optional `try` without catch

```lucia
try:
    1 / 0
end
```

### Suppressing errors with `?`

```lucia
fun may_fail()? -> int:
    1/0
end

assert!(may_fail()? is void)       // no crash, returns null
assert_eq!(may_fail()?, null)      // ? operator returns null if error occurs
assert_eq!(may_fail()? or 42, 42)  // provide default value
```

- `?` **suppresses errors** and returns `null` instead.
- You can chain `?` operators, and provide default values with `or` or `??`.

## Throw

```lucia
throw "msg" from "err_type"
throw "msg"
```

Throws an error.
If `err_type` is ommited throws as `"LuciaError"`.

### Throwing a tuple

```lucia
throw ("CustomError", "Something went wrong", "Helpful text")
```

Error can include 1–3 elements: type, message, help.
Default type is `"LuciaError"` if omitted.

## Comments

Lucia supports single-line and multi-line comments:

### Single-line Comment

```lucia
// This is a single-line comment
```

### Multi-line Comment

```lucia
/*
This is a multi-line comment
spanning multiple lines.
*/
```

### In-line Comment

The in-line comment is used to add comments inside the code.
It's started by `<#` and closed by `#>`:

```lucia
println("Hello" <# This is an in-line comment #>, "world")
```

## Lambda Functions

Lambdas in Lucia are **inline anonymous functions** using the `=>` syntax. They are useful for short computations without creating a full `fun`.

```lucia
variable: function = (param1, param2, ...) => expression
```

- Single parameter: parentheses optional.
- Multiple parameters: parentheses required.
- Zero parameters: use `()`.

### Examples

```lucia
// Double a number
double: function = x => x * 2
println(double(5))  // Output: 10

// Sum two numbers
sum: function = (a, b) => a + b
println(sum(3, 4))  // Output: 7

// Return a constant
answer: function = () => 42
println(answer())   // Output: 42

// Empty lambda
noop: function = () => ()
noop()              // Does nothing
```

Sure! Here's a clean, concise doc for **`where` + `then` expressions** in Lucia with minimal examples instead of full tests:

## Where and then

Lucia supports **local bindings** and **chained transformations** using `where` and `then`.

### 1\. Where — Local Bindings

The `where` clause allows defining **local variables scoped to an expression**.

```lucia
import (sqrt) from math

a: int = 3
b: int = 4

hypotenuse := sqrt(a*a + b*b)
  where
      sqA = a * a
      sqB = b * b
  end

println(hypotenuse)  // Output: 5.0
```

- Variables defined in `where` **cannot be accessed outside** of the expression.
- Useful for **breaking complex calculations** into readable parts.

### 2\. `then` — Chained Transformations

The `then` keyword allows **post-processing the result of an expression**.

```lucia
result := 137.4567
  then round(2)
  then format(sep="'")

println(result)  // Output: "137.46"
```

This is equivalent to:

```lucia
result := 137.4567.round(2).format(sep="'")

println(result)  // Output: "137.46"
```

- Multiple `then` calls can be **chained in order**.
- Each `then` receives the **previous result** as its input.

> NOTE:  
> The difference between `then` and `|>` (pipe) is that `then` runs methods and operations, where `|>` just pipes into the next expression.

### 3\. Combining `where` and `then`

```lucia
c: int = 3
d: int = 4

result := sqrt(sqC + sqD)
  where
      sqC = c*c
      sqD = d*d
  then round(1)
  then * 10

println(result)  // Output: 50.0
```

- `where` defines temporary variables for the main expression.
- `then` transforms the final value step by step.

## Tuple Unpacking / Multiple Assignment

Lucia allows **assigning multiple variables at once** using tuples or sequences. This works with variables, literals, strings, lists, and even references.

### 1\. Basic Swapping / Multiple Assignment

```lucia
a: int = 10
b: int = 20
c: int = 30

(a, b, c) = (b, c, a)

println(a, b, c)  // Output: 20, 30, 10
```

- The right-hand side can be **any expression producing a tuple or sequence**.
- Variables are assigned **simultaneously**, so swapping is easy.

### 2\. Mixed Expressions

```lucia
(a, b, c) = (b, c, a + 1)
println(a, b, c)  // Output: 30, 10, 21
```

Expressions like `a + 1` can be used directly on the right-hand side.

### 3\. `final` and `mutable` Modifiers

```lucia
(final d: int, e: int, mutable f: int) = (a, b, c)

println(d, e, f)  // Output: 30, 10, 21

f = 40            // Allowed (mutable)
println(f)        // Output: 40

d = 50            // Error: d is final
```

- `final` = cannot be reassigned.
- `mutable` = can be changed after assignment.

### 4\. Sequence / String Unpacking

```lucia
(x, y) := "ab"
println(x, y)  // Output: "a", "b"

(m, n, o) := "xyz"
println(m, n, o)  // Output: "x", "y", "z"
```

- Works with **strings and lists**.
- Lengths must match, otherwise **runtime error**.

### 5\. Single-element Tuples

```lucia
(h) := "abc"
println(h)  // Output: "abc"
```

Parentheses are optional for single-element unpacking.

### 6\. List Unpacking

```lucia
l: list[int] = [1, 2, 3]
(x: int, y, z) := l
println(x, y, z)  // Output: 1, 2, 3
```

- Types can be annotated individually.
- Must match the sequence length exactly.

## Defer

The `defer` statement schedules a block of code to run **at the end of the current scope**.

> NOTE:  
> list.extend and other list methods currently create a new list instead of directly mutating the current one due to bad architecture
> I am working on fixing this

```lucia
output: list[str] = ["first"]

output = output.extend(
    scope defer_test:
        scope_output: list[str] = []

        defer (scope_output = scope_output.append("third"))
        defer:
            scope_output = scope_output.append("fourth")
        end

        scope_output = scope_output.append("second")
        return scope_output
    end
)

println(output)  // ["first", "second", "third", "fourth"]
```

- `defer (expr)` runs the expression at the end of the scope.
- `defer: ... end` runs a full block at the end of the scope.
- Useful for cleanup, logging, or adding elements in a defined order.

## Scopes

`scope` defines a **local scope** for variables and allows **temporary changes**. It can also manage **unsafe references**.

```lucia
#config allow_unsafe = true

a: &int = &10

scope (a):
    *a = 20
end

println(*a)  // 20
```

- Variables declared inside a `scope` are **local**.
- Pointers can be temporarily modified safely using `scope`.
- `scope (*)` is an alias for grouping multiple expressions (see below).

## Groups

Groups let you **combine multiple expressions** into a single unit. They are mainly used in macros to avoid parsing ambiguities.

```lucia
assert_eq!(\ 1 + 1 \, 2)  // evaluates multiple expressions

#macro group_test($a):
    b := $a
    b
#endmacro

println(group_test!(5 + 5))  // 10
```

- Without groups, macro expansion may misinterpret sequences as function calls.
- Groups preserve the **logical block** of expressions.
- `scope (*)` can also act as a group:

```lucia
scope (*):
    b := 5 + 5
    b
end

println(b)  // 10
```

Both `\ ... \` and `scope (*)` allow multiple expressions to behave as a **single value** in macros or assignments.

## Importing Modules

```lucia
import math

println(math.sqrt(16))  // 4.0
println(math.PI)        // 3.141592653589793

forget math  // removes the module
```

You can give a module a shorter name with `as`:

```lucia
import math as m

println(m.sqrt(25))  // 5.0
println(m.PI)        // 3.141592653589793

forget m
```

You can import only specific symbols, and rename them if needed:

```lucia
import (sin, cos as c) from math

println(sin(42))
println(c(42))  // cos function using alias

forget (sin, c)
```

> NOTE:  
> The parenthasies around imported elements are reqiured
> For example, you cannot do:
>
> ```lucia
> import sin from math
> ```
>
> This wont work because it thinks you want to import sin from the path of variable `math` which is not defined.

You can import other scripts:

```lucia
import "04_variables.lc" from "." as vars

println(vars.i)         // 100
println(vars.public_j)  // 200

// private variables are not accessible
// println(vars.j)
```

## Match

`match` lets you compare a value against literals, variables, or patterns and execute code depending on which branch matches.

### Matching Literals

```lucia
command: str = "start"

match (command):
    "start":
        println("Starting engine...")
    end

    "stop":
        println("Stopping engine...")
    end

    _ ->
        println("Unknown command")
    end
end
```

- `"literal":` matches **exactly that literal**.
- `_ ->` is a **wildcard** that catches anything else.

### Capturing Values

```lucia
user_input: str = "Alice"

match (user_input):
    name -> 
        println(f"Hello, {name}!")  // binds input to `name`
    end
end
```

- `a ->` captures the matched value into a variable (`name` here).
- Always succeeds if no previous branch matched.

### Matching Against a Variable

```lucia
expected: str = "secret"
attempt: str = "guess"

match (attempt):
    expected:
        println("Correct password!")
    end

    attempt -> 
        println(f"Wrong password: {attempt}")
    end
end
```

- `expected:` matches **the value of the variable `expected`**.
- `attempt ->` captures the value if it didn't match earlier.

> WARNING:  
> Never confuse `->` with `:` in the match arm.  
> For example if you wanted to match with the variable `a` and you wrote `a -> ... end` it will **ALWAYS** match.
> In this case you should write `a: ... end`

### Guards and Continue

```lucia
level: int = 2

match (level):
    n if n < 3 -> 
        println("Level is low")
        continue  // also check next branches
    end

    2:
        println("Level is exactly 2")
    end
end
```

- `(n if condition) ->` captures the value **only if the condition is true**.
- `continue` lets matching **move on to check later branches**, so multiple branches can run for the same value.

### Breaking Out Early

```lucia
status: str = "error"

match (status):
    "ok":
        println("All good")
    end

    "error":
        println("Error detected, stop matching")
        break  // stops evaluating further branches
    end

    "warning":
        println("Won't execute if break triggered")
    end
end
```

- `break` exits the match immediately.
- Useful to prevent later branches from running.

### Union Matches

```lucia
fruit: str = "apple"

match (fruit):
    "apple" | "banana" | "orange":
        println("We have a fruit!")
    end

    _ ->
        println("Unknown item")
    end
end
```

- `"a" | "b"` matches **any of the listed literals**, like Rust's `A | B => ...`.

### Combined Example

```lucia
input: str = "start"
level: int = 2

match (input):
    "start" | "restart":
        println("Starting system...")
    end

    "stop":
        println("Stopping system...")
    end

    other -> 
        println(f"Unknown command: {other}")
    end
end

match (level):
    n if n < 3 ->
        println("Level too low")
        continue  // allows next branch to also match
    end

    2:
        println("Level is exactly 2")
    end

    _ ->
        println("Other level")
    end
end
```

## Indexing and Slicing

Lucia lets you access and modify elements in **lists, strings, and nested containers** using square brackets `[]`.

### Basic Indexing

```lucia
nums: list[int] = [0..10].collect()
text: str = "i love lucia"

println(nums[0])  // 0
println(text[2])  // 'l'
```

- Positive indices start from 0.
- Negative indices count from the end: `-1` is the last element.

```lucia
println(nums[-1])  // 10
println(text[-1])  // 'a'
```

### Slicing

```lucia
nums[2..5]  // [2, 3, 4]
text[2..6]  // "love"
```

- `start..end` slices from `start` **inclusive** to `end` **exclusive**.

You can omit start or end:

```lucia
nums[5..]   // [5, 6, 7, 8, 9, 10]
text[7..]   // "lucia"

nums[..3]   // [0, 1, 2]
text[..4]   // "i lo"

nums[..]    // entire list
text[..]    // entire string
```

### Index Assignment

```lucia
nums[0] = 42
text[0] = 'I'

println(nums[0])  // 42
println(text[0])  // 'I'
```

- Works for lists and strings.
- Strings are mutable in Lucia.

### Nested Indexing

```lucia
nested: list[int, list[int;2], int] = [1, [2, 3], 4]

println(nested[1][0])  // 2

nested[1][1] = 43
println(nested)  // [1, [2, 43], 4]
```

```lucia
deep: list = [10, [20, 30, [40, 50]]]
deep[1][0] = 42
deep[1][2][1] = 40
println(deep)  // [10, [42, 30, [40, 40]]]
```

### Map Indexing

```lucia
data: map[str, map[str, int]] = {"outer": {"inner": 10}}
println(data["outer"]["inner"])  // 10

data["outer"]["inner"] = 42
println(data["outer"]["inner"])  // 42
```

## Enums

Enums are types that can have several **variants**. Variants can either be **plain** or **carry data**.

### Basic Enum

```lucia
typedef enum Color = {
    Red,
    Green,
    Blue,
}

c1: Color = Red
c2: Color = Color.Green
```

```lucia
typedef enum Color = {
    Red,
    Green,
    Blue,
}

typedef enum CarColor = {
    Red,
    White,
    Silver
}

c1: Color = Red // error, Red is ambiguous
c2: Color = Color.Green
```

### Enum with Data

```lucia
typedef enum Rgb {
    Red(int),
    Green(int),
    Blue(int),
}

c: Rgb = Red(255)
```

- Variants can store **values**, like `Red(255)`.
- To get the stored value:

```lucia
c_value := c.unwrap()
println(c_value)  // 255

match (c):
    Red(value) -> println(value)
    _ -> println("unexpected variant")
end
```

### Pattern Matching

```lucia
c3: Color = Color.Rgb(255, 0, 127)

match (c3):
    Rgb(r, g, b) ->
        println(r, g, b)  // 255, 0, 127
    end
end
```

- Pattern matching lets you extract **fields** from variants.
- You can match on **fully qualified** names (`Color.Rgb`) or unambiguous ones (`Rgb`).

### Discriminants

Variants can optionally have **explicit numeric values**:

```lucia
typedef enum TestEnum = {
    First = 1,
    SixtyNine = 69,
    AutoNext,  // automatically continues from previous, so 70
}

println(TestEnum.First)           // TestEnum[1]
println(TestEnum.SixtyNine)       // TestEnum[69]
println(AutoNext.discriminant())  // 70
```

## Structs

Structs in Lucia are typed containers for data. They can have methods and static functions.

### Defining and Instantiating

```lucia
typedef struct Point = {
    x: int,
    y: int,
}

let p1 = Point { x = 10, y = 20 }
let p2 = Point { x = 30, y = 40 }
```

Access fields with `.`:

```lucia
p1.x   // 10
p2.y   // 40
```

Structs support equality:

```lucia
p1 == p2    // false
p1 != p2    // true
```

### Methods and Static Functions

You can define methods and static functions in a `for StructName:` block.

```lucia
for Point:
    static fun new(x: int, y: int) -> Self:
        return Point { x, y }
    end

    static mutable fun origin() -> Self:
        return Point { x = 0, y = 0 }
    end

    fun display(self) -> str:
        return f"Point({self.x}, {self.y})"
    end
end
```

Static functions are called on the type:

```lucia
let p = Point.new(50, 60)
let o = Point.origin()
```

Methods are called on instances:

```lucia
p.display()   // "Point(50, 60)"
```

### Mutable Fields

Struct fields are mutable unless the struct instance is declared `final`.

```lucia
p.x = 70
p.display()  // updates to "Point(70, 60)"
```

### Instance Methods

You can define methods that modify the struct:

```lucia
for Point:
    fun move(self, dx: int, dy: int):
        self.x += dx
        self.y += dy
    end
end

let p = Point.origin()
p.move(10, 20)
p.display()  // "Point(10, 20)"
```

Methods can also return a new shifted struct:

```lucia
for Point:
    fun shifted(self, dx: int, dy: int) -> Self:
        return Point { x = self.x + dx, y = self.y + dy }
    end
end

let p2 = Point.origin().shifted(5, 5)
p2.display()  // "Point(5, 5)"
```

### Field Validation

Missing or extra fields at construction raise an error:

```lucia
Point { x = 1 }            // Error: missing y
Point { x = 1, y = 2, z = 3 } // Error: extra field
Point { x = "hi", y = 2 }  // Error: wrong type
```

### Equality

Structs compare field by field:

```lucia
let a = Point.new(5, 5)
let b = Point.new(5, 5)
a == b    // true
a.x = 99
a == b    // false
```

### Overriding Methods

- Methods are `final` by default and cannot be redefined.
- Mutable static functions can be overridden:

```lucia
for Point:
    static fun origin() -> Self:
        return Point { x = 0, y = 0 }
    end
end
```

### Casting

Structs can be cast if compatible:

```lucia
typedef struct H = { f: int }
typedef struct I = { g: int }

let h = H { f = 10 }
let i = h as I
i.g   // 10
```

Incompatible casts raise an error:

```lucia
let p = h as P   // Error: incompatible types
```

## Interfaces on structs

Lucia uses **`impl` types** to define interfaces (similar to Rust traits). Any struct that provides the required methods automatically satisfies the interface.

### Basic Interface

```lucia
typedef Displayable = impl display[] -> str
```

`Displayable` requires a `display` method that takes no arguments and returns a `str`.
Any struct implementing `display()` can be stored in a variable of type `Displayable`.

### Structs Implementing Interfaces

```lucia
typedef struct Point = { x: int, y: int }
typedef struct Circle = { center: Point, radius: int }

for Point:
    fun display(self) -> str:
        return f"Point({self.x}, {self.y})"
    end
end

for Circle:
    fun display(self) -> str:
        return f"Circle(center: {self.center.display()}, radius: {self.radius})"
    end
end
```

`Point` and `Circle` both implement `display()`.
You can now assign them to `Displayable` variables:

```lucia
a: Displayable = Point { x = 5, y = 10 }
b: Displayable = Circle { center = a, radius = 15 }

a.display()  // "Point(5, 10)"
b.display()  // "Circle(center: Point(5, 10), radius: 15)"
```

### Type Safety

- Only structs with the correct method signature can satisfy an interface:

```lucia
typedef struct Rectangle = { top_left: Point, bottom_right: Point }

c: Displayable = Rectangle.new(Point.new(0,0), Point.new(10,10))  // ERROR
```

- Errors occur if:

  - The struct doesn't implement the method.
  - The return type is wrong.
  - The method arguments don't match.

### Passing Interfaces to Functions

```lucia
fun display_shape(s: Displayable) -> str:
    return s.display()
end

display_shape(a)  // works
display_shape(b)  // works
```

- Functions can accept any struct that satisfies the interface.

## Operator Interfaces (`impl add`, `impl sub`, etc.)

Interfaces can also define operator-like methods:

```lucia
typedef Add = impl add[any] -> int
typedef Sub = impl sub[Self] -> int
typedef Counter = impl Add + Sub
```

- `Add` requires an `add` method.
- `Sub` requires a `sub` method.
- `Counter` combines both interfaces.

Example implementation:

```lucia
typedef struct H = { value: int }

for H:
    fun add(self, amount: int) -> int:
        self.value += amount
        return self.value
    end

    fun sub(self, amount: int) -> int:
        self.value -= amount
        return self.value
    end
end

c: Counter = H { value = 10 }

fun test_counter(mutable c: Counter) -> Counter:
    c.add(5)
    c.sub(3)
    return c
end

test_counter(c).value  // 12
```

Any struct implementing the required operator methods can satisfy `Counter`.

## Static Factory Interface (`impl static new[] -> any`)

Some interfaces require **static methods**, for example constructors:

```lucia
typedef Creatable = impl static new[] -> any

typedef struct A = { x: int }

for A:
    static fun new() -> Self:
        return A { x = 42 }
    end
end
```

`A` satisfies `Creatable` because it has a `static new()` method.

```lucia
a: Creatable = A

fun create(c: Creatable) -> any:
    return c.new()
end

create(a).x  // 42
```

## Generics

Lucia supports **parametric types**, allowing structs, enums, and interfaces to be generic.

### Generic Structs

```lucia
typedef struct Holder<T> {
    value: T
}

int_holder: Holder[int] = Holder { value = 10 }
str_holder: Holder[str] = Holder { value = "abc" }

int_holder.value  // 10
str_holder.value  // "abc"
```

`Holder[T]` can hold any type `T`.
You access the value via `.value`.

### Generic Enums

#### Single type parameter

```lucia
typedef enum Option<T> {
    Some(T),
    None,
}

opt1: Option[int] = Some(123)
opt2: Option[int] = None
opt3: Option[str] = Some("hello")
```

`Option[T]` is a generic container that either has a value (`Some`) or not (`None`).
You **can** pattern-match on generics:

```lucia
match (opt1):
    Some(v) -> v  // v = 123
    None -> 0
end
```

#### Two type parameters

```lucia
typedef enum Response<T, E> {
    Success(T),
    Failure(E)
}

resp1: Response[int, str] = Success(200)
resp2: Response[int, str] = Failure("Not found")
```

`Response[T, E]` can store either a success of type `T` or an error of type `E`.
Nested generics are allowed:

```lucia
nested: Option[Result[int, str]] = Some(Ok(42))
```

Pattern matching works with nested generics:

```lucia
match (nested):
    Some(Ok(x)) -> x  // 42
    Some(Err(e)) -> throw e
    None -> 0
end
```

### Generic Constraints

You can constrain generic parameters with **interfaces**:

```lucia
typedef Clone = impl fun[Self] -> Self

typedef struct Pair<T, U> {
    first: T,
    second: U
} where (T: Clone, U: Clone)
```

Both `T` and `U` must implement `Clone`.
`Pair[int, str]` works because integers and strings implement `Clone`.

## Preprocessor

The preprocessor runs **before parsing and type checking**.
It operates purely on tokens and text and is mainly used for configuration, conditional code, constants, and low-level tooling like FFI and macro support.

It is intentionally explicit.

### Including files

```lucia
#include "04_variables.lc"
#include <std/assert>
```

`#include` inserts the contents of another file directly into the current file.
After inclusion, all symbols from the included file behave as if they were written inline.

### Defines and aliases

```lucia
#define PI 3.14
#define HELLO "world"
```

`#define` creates a compile-time constant replacement.

```lucia
#alias add sum
#unalias add
```

`#alias` rewrites one token into another during preprocessing.
This works for identifiers, literals, and even operators.

Aliasing is purely textual and happens before parsing.

### Conditional code

```lucia
#ifdef PI
    pi: float = PI
#endif

#ifndef UNDEFINED
    hello: str = HELLO
#endif
```

These blocks are included or skipped based on whether a symbol exists at preprocess time.

### Precompile blocks

```lucia
i: int = #precompile: 1 + 2 #endprecompile
```

`#precompile` evaluates code **at preprocess time** and inserts the result as a literal token.

This is useful for:

- generating constants
- unrolling compile-time data
- avoiding runtime cost

### Token injection

```lucia
#push ( ("IDENTIFIER", "42") )
```

`#push` injects raw tokens into the token stream.

This happens:

- at preprocess time
- in its own isolated scope
- without access to runtime variables

It is intentionally unsafe and mainly intended for tooling, macros, and language extensions.

### Preprocessor errors

```lucia
#error "This should not be triggered"
```

Immediately aborts with a message.

### Link preprocessor (FFI)

```lucia
#link "header.h" "library.so"
```

Generates FFI bindings and wrappers automatically.

Currently limited to primitive types:
`int`, `float`, `str`, `bool`, `void`

## Macros

Macros are **preprocessor-time code generators** built on top of the preprocessor.
They expand into tokens, not values.

Macros are not functions:

- no runtime
- no types
- no variables (unless explicitly generated)

### Defining a macro

```lucia
#macro hello($name):
    println(f"Hello, $name$!")
#endmacro
```

Invocation:

```lucia
hello!("Lucia")
```

Macros expand inline before parsing.

### Hygiene and name mangling

By default, macros are **hygienic**.
Variables defined inside a macro do not leak or overwrite outer variables.

```lucia
#macro mangling_test($a):
    i: int = $a
#endmacro
```

The outer `i` is not affected.

You can disable this:

```lucia
#macro #no-mangle m!():
    h = 100
#endmacro
```

### Grouping behavior

Macros normally expand as groups to avoid parsing ambiguity.

You can disable grouping:

```lucia
#macro #no-group g!():
    f: function = () => (42)
    f
#endmacro
```

This matters when macro output would otherwise be parsed as a function call or expression chain.

### Bracket types

Macros can use different brackets:

```lucia
#macro vec![$els...]:
    [$els]
#endmacro
```

All of these are valid:
`()`, `[]`, `{}`, `<>`

Different brackets allow multiple macros with the same name.

### Variadic macros

```lucia
#macro vec![$els...]:
    [$els]
#endmacro
```

`$els...` captures a list of tokens.

### Token inspection

```lucia
#macro is_ident($token):
    tokens: list = [$?token]
    tokens[0][0] == "IDENTIFIER"
#endmacro
```

`$?` gives raw token data instead of parsed syntax.

The format of raw token data is

```template
("TYPE", "CONTENT"), ("TYPE", "CONTENT"), ...
```

It is on the caller to decide the type of the raw data.

Most of the type you will see:

```lucia
[$?token]
```

Which means the type is `list[(str, str)]`

### Deprecated macros

```lucia
#macro #deprecated("This macro is deprecated") deprecated_macro():
    println("This macro is deprecated")
#endmacro
```

Calling the macro emits a warning.

### Standard library and macros

The standard library of Lucia contains a `macros.lc` file which includes:

| Name                               | Description                                                    |
| ---------------------------------- | -------------------------------------------------------------- |
| unreachable!(msg)                  | Throws an UnreachableError                                     |
| todo!(msg)                         | Throws a NotImplemented error                                  |
| dbg!(value...)                     | debugs the formatted value with the code of the value          |
| dbglog!(value)                     | Prints the value in debug color from config                    |
| time_it!(expr)                     | Times the expression and returns (result, elapsed: ApolloTime) |
| stringify!(value)                  | Stringifies a value into a string                              |
| stringify_tokens!(tokens)          | Stringifies tokens into a string                               |
| matches!(value, pattern)           | Returns if a value matches the pattern                         |
| get_matched!(value, pattern, vars) | Returns variable it matched                                    |
| sort!(val, reverse, f)             | Sorts a value                                                  |
| mod!{code}                         | Creates a module out of code                                   |
| mod!(path, name)                   | Creates a module from a file                                   |

There is also `assert.lc` which is included in the `macros.lc`.

| Name                                  | Description                                                                      |
| ------------------------------------- | -------------------------------------------------------------------------------- |
| assert!(cond, msg)                    | Asserts that cond is true with the msg as error                                  |
| assert_eq!(a, b, msg)                 | Asserts that `a` and `b` are equal                                               |
| assert_approx_eq!(a, b, epsilon. msg) | Asserts that `a` and `b` are approximately equal by the epsilon, used for floats |
| assert_ne!(a, b, msg)                 | Asserts that `a` and `b` are not equal                                           |
| assert_err(expr, msg)                 | Asserts that `expr` fails at runtime                                             |
| assert_type(value, it, msg)           | Asserts that `value` is of type `it`                                             |
| assert_matches!(value, pattern, msg)  | Asserts that `value` matches the `pattern`                                       |
