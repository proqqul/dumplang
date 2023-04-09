## Session 1

- [x] Basic pipeline
  - [x] source code -> AST (hs)
  - [x] AST -> bytecode (hs)
  - [x] bytecode -> output (zig)

## Session 2

- [ ] make a slightly more than minimal language
  - [ ] strings
  - [x] bools
  - [ ] let blocks
  - [ ] procedures
  - [ ] syntax
  - [ ] control flow
    - [x] if
    - [ ] while

- [ ] non-trivial value
  - [ ] array
  - [ ] cons list
  - [ ] user-defined structs

- Representing values on stack:
  We use a tagged pointer, where a value is either:
  - Int (i32)
  - True
  - False
  - Pointer to a chunk of memory, the first part is a "type tag" and then the rest is stuff.

```
fun foo()
  x = 1
  y = 2
  (x * y + (y * 2))
end

fun foo()
  %1 = 1
  %2 = 2
  %3 = %2 * 2
  %4 = %1 * %2
  %5 = %3 + %4
  ret %5
end
```

## Session 3

- [x] let blocks
- [ ] procedures

## Session 4

- [ ] clean up let blocks
- [ ] procedures
