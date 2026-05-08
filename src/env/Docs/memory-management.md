# Memory Management in Lucia

Lucia is written in rust, so most runtime memory follows rust ownership rules and is released automatically when values are dropped.

Lucia currently uses `mimalloc` as the global allocator, which improves allocation performance and reduces fragmentation in many real workloads.

---

## Main memory

### 1. Runtime Values

- Values (`int`, `float`, `list`, `map`, `struct`, etc.) are owned by the interpreter runtime.
- When a value is replaced (for example `a = [1, 2]` then `a = [3, 4]`), the old value is dropped when no longer referenced.
- Temporary values created during expression evaluation are also dropped after they are no longer needed.

### 2. Scope and Variables

- Variables live inside interpreter scopes.
- Leaving a scope releases variables from that scope (unless captured by something that still references them).
- `forget(name)` removes a variable binding explicitly.

### 3. Containers and Nested Data

- `list`, `tuple`, `map`, and `struct` own their contained values.
- Dropping a container recursively drops owned elements.

### 4. Pointers and Shared Values

- Pointer-like runtime values and shared objects are reference-counted internally.
- Memory is released when the last live reference is gone.

---

## Caching and Memory Growth

Lucia caches some computed results for speed.

### Operation Cache

- Arithmetic/unary operation results are cached.
- The operation cache now uses an LRU policy.
- Maximum size is configurable with `operation_cache_size` in `config.json`.

Example:

```json
{
	"operation_cache_size": 8192
}
```

If your scripts are long-running and highly dynamic, lowering this value can reduce memory usage at the cost of more recomputation.

---

## Notes

### 1. Intentional Static Leaks

The current code has a bunch of calls to `to_static` function which is flawed by design because it leaks the string into 'static.
This is VERY WRONG and im refactoring the codebase to remove it completely.

### 2. No GC

Lucia does not use GC. Rust semantics are better. (i might add lifetimes later)

simply i dont like GCs
