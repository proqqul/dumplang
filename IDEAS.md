## per-scope heap allocation
- bump / arena / something allocator per scope (function)

there are two reasons one might allocate something on the heap, rather than the stack:

1. its lifetime lasts longer than the function
2. it is of dynamic size

the idea here would be to eliminate concern (2). you can write
```
var a = 5;
var b = [5]{undefined};
var c = [runtime_val]{undefined};
```
in the background, the language will put `a` and `b` on the stack, but `c` in the function's heap allocator, which gets dumped at the end of scope, like the stack would.

This is slightly slower than stack values, but not much. Maybe the same area in memory would even be hot in the cache if the function gets called in a loop.

### caller allocations

maybe there can be a way to allocate something on the caller's heap allocator too.
```
fn foo() {
  local a = somethingOrOther(); // allocated on this function's scope allocator
  caller b = somethingElse(); // allocated on the caller's scope allocator
  // return a; // invalid. cannot return something declared with `local`
  return b; // this is fine
}
```

if we keep going with this idea, do we end up just reinventing the rust borrow checker?
