The following things are defined in the `builtin` package, which is implicitly
used in every file. Therefore, these things are effectively like "globals". Also,
you can add your own things to the `builtin` package to define your own "globals"
for your project.

- `str` -
This is the builtin string type, which is just a slice of bytes.

- `cstr` -
This is the builtin cstring type, used in every program as the type of the command line arguments given to main.

- `range` -
This is the type of a range literal, i.e. 1 .. 5. You can also use this type in a for-loop to iterate over the values. 

- `vararg` -
This is a deprecated type used when you have an untyped variadic procedure, i.e. x :: (va: ...). This is deprecated because of the 'any' type.

- `vararg_get` -
This is also deprecated but it allowed you get values out of a `vararg` type.

- `null_proc` -
This is a special procedure that always matches any procedure type, regardless of whether the types actually match. This allows you to use it as a placeholder or "null" value when a procedure is expected.

- `null` -
Much like every other systems-ish programming language, Onyx has a "null" type. In Onyx, it is nothing more than a 0 value casted to a `rawptr`.

- `null_str` -
A helpful constant that is a string with the data being null and the count being 0. Useful if you want to have a blank string value. It is different from "", because the latter will still be assigned a data location, even through there will not be any data there.

- `OnyxContext` -
The builtin type of the `context`, described below. In theory, this type could exist at file scope in builtin.onyx, but I decided to leave it exposed.

- `context` -
This is a thread-local variable that stores the active allocators, loggers, and assert_handler for the current thread.

- `assert` -
A function that asserts a certain condition is true; if not, it calls the assert handler on the `context`.

- `Logger` -
The type used for the logger on the `context`. 

- `log` -
Logs a message to the `context.logger`. Currently, this does not support formatted arguments, but that is planned in the near future.

- `AllocationAction` -
A simple enum that represent what action is to be taken when calling an allocator_proc.

- `Allocator` -
This type represents something that can allocate memory. It has two members, `data` and `func`. The `func` is an `allocator_proc`, which takes the data, the allocation action, size, alignment and old pointer. Not all of these values are used for every action, but the allocator procedure must consume all of them.

- `raw_alloc` `raw_resize` `raw_free` -
These functions are simply wrappers provided to invoke an `Allocator` with a specific action.

- `calloc` `cresize` `cfree` -
These functions simply call `raw_alloc`, `raw_resize` and `raw_free` using `context.allocator` as the allocator. These are effectively the equivalent of `malloc`, `realloc`, and `free` in C.

- `new` -
This function allocates enough space for the given type from the given allocator, which by default is the context's allocator. It also initializes the resulting memory, which uses the `__initialize` intrinsic that initializes all types, but most importantly it evalutes the default values for struct members.

- `make` -
This function is like new, except it does not initialize the memory.

- `Iterator` -
This type represents a custom iterator that can be iterated over using a `for` loop. It has three members. `data`, a simple pointer passed to the other functions. `next` which provides the next value for the iterator, and a continuation boolean. If the boolean is false, the iterator is done and the value given is garbage. And finally `close`, which is optional and allows you to run code when the iterator is done being used. When using a `for` loop, `close` is called automatically for you when the loop exits in any way.

- `CallSite` -
This type represents the value given by the `#callsite` expression. In practice, you will never use this type because you will just do something like: `f :: (site := #callsite)`

- `any` -
This type represents any value. It does this by using a data pointer and a type expression. Using the `runtime.info` package, you can introspect that type expression to retrieve data about the type, and from there reconstruct how to use the data pointer. See `conv.onyx` for how this works with `printf`.

- `Code` -
This is a dummy type that represents the type of a `#unquote {}` block. It is used when passing code around to macros or procedures, i.e. `f :: macro (body: Code)`