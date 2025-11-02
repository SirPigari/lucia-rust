# Benchmarks

You have to note that Lucia is *interpreted* and *slow as fuck*.

## Hello world

```lucia
print("hello world")
```

| Platform                 | Specs                                                                                    | Debug (µs) | Release (µs) |
|--------------------------|------------------------------------------------------------------------------------------|------------|--------------|
| x86_64-pc-windows-msvc   | Intel i3-14100F - 4C/8T @ 3.5 GHz - 32 GB RAM - Win11 Pro x64    - RTX 3060 Ti           | 2665µs     | 1404µs       |
| x86_64-unknown-linux-gnu | Intel i5-5200U  - 2C/4T @ 2.7 GHz - 8 GB RAM  - Arch Linux 6.17  - Intel HD 5500         | 2968µs     | 755µs        |

So about `712` *hello worlds per second* on release on first machine.

## Fibonacci 1 million

```lucia
(a, b) := (0, 1)
while a < 1_000_000:
    print(a)
    (a, b) = (b, a + b)
end
```

| Platform                 | Specs                                                                                    | Debug (µs) | Release (µs) |
|--------------------------|------------------------------------------------------------------------------------------|------------|--------------|
| x86_64-pc-windows-msvc   | Intel i3-14100F - 4C/8T @ 3.5 GHz - 32 GB RAM - Win11 Pro x64 - RTX 3060 Ti              | 40452µs    | 31142µs      |
| x86_64-unknown-linux-gnu | Intel i5-5200U  - 2C/4T @ 2.7 GHz - 8 GB RAM - Arch Linux 6.17 - Intel HD 5500           | 74047µs    | 22645µs      |

## Tests benchmarks

Run `make benchmark`
