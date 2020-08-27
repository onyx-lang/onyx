# Onyx
A simple, yet powerful language for WebAssembly.

## Project Goals
This project was started for me to learn the ins and outs of compiler and programming language design.
I do not intend for this language to be used for anything "professional".
It is purely for me to learn and experiment with compiler architecture.
To that end, I believe I have made a considerable amount of progress towards a "real" and "professional" programming language.

## High-level language features
### Intuitive symbol resolution
Symbols in the global scope are always resolvable, even if the definition for a symbol comes after its use.
This means Onyx does not have header files like C or C++, since procedures and variables can be resolved out of order.
Take this example:
```
main :: proc (args: [] cstring) {
    val := fib(20);
    print(val);
}

fib :: proc (n: i32) -> i32 {
    switch n {
        case 0 do return 1;
        case 1 do return 1;
        case #default {
            return fib(n - 1) + fib(n - 2);
        }
    }
}
```
`main` can call `fib`, even though `fib` is declared lexographically after `main`.
In fact, the recursion of `fib` also works due to this feature.
Before this feature was implemented, recursion in any form did not work.

### Package System
Organizing code is a perpetual problem in all programming language.
_"Where should this function go?"_ is something I ask myself too frequently.
To aid in this problem, the following features / design decisions are available in Onyx:
  * `#include_file` adds a file to the queue of files to process. Note, this does not textually include the contents of the file; it just tells the compiler to add the file to a queue to process later. If a file has already been included, it is ignored.
  * `#include_folder` adds a folder to the search path of files. By default, the only folder in the search path is the current directory, though this is subject to change.
  * a package system described below
  
Every onyx source file is part of a *package*; if no package is explicity stated, the file is part of the `main` package.
All symbols declared in the file are added to the package's public scope, unless they explicitly marked with the `#private` directive.
It is worth mentioning that the directory structure does not correlate to the package structure at all.
Any file from anywhere can be part of any package.
Through my experience, I've found that langauges that try to enforce a directory structure become hard to maintain.
I believe it is the programmers job to organize their source files into something that makes sense to them.
For this reason, Onyx is folder structure agnostic.

To use symbols from another package, use the `use package package_name` statement at the top level.
There are several variations of `use package`:
  * `use package pac` will add all public symbols from `pac` to the package scope of the current package.
  * `use package pac as other` will add the symbol `other` into the package scope, which can be used to access all public symbols of the package `pac`.
  * `use package pac { foo, bar as other }` will add only the symbol `foo` from the package `pac`, as well as add a symbol `other` which aliases to the `bar` symbol in the package `pac`.

An important thing to note is that `use package` statements are not transitive. This means that if _A_ uses package _B_, and _B_ uses package _C_, _A_ does not have access to the symbols in _C_, unless _A_ also explicitly uses the package _C_.

Another important note is that currently, there is no way to introduce a symbol at the file scope. All symbols in a file will be added to the corresponding package. File scoped symbols may be added in the future.

### The |> operator
Though Onyx is not a functional language in almost any respect, it borrows the |> operator from langauges like Elixir and F#.
The semantics of the operator are as follows:
  * `left |> right(param1, param2)` is translated to `right(left, param1, param2)`. This makes it closer to the Elixir version of the operator. The left side of the operator becomes the _first_ parameter of the calling function.
  * The operator is left associative, which means it can be chained intutively. The following examples are equivalent:
  ```
  o |> foo(p1) |> bar(p2)
  
  (o |> foo(p1)) |> bar(p2)
  
  foo(o, p1) |> bar(p2)
  
  bar(foo(o, p1), p2)
  ```
Because of the existance of this operator, it is common to design an API where many of the functions look like:
```
api_func :: proc (val: T, ...) -> T {
    // ...
    return val;
}
```
This makes it easy to chain many of the api functions together. The `StringBuilder` is a good example of this.
  
  

## Compiler progress
There are a couple of missing features of the compiler that will need to be addressed fairly soon, such as being able to compile on Windows.
A very rough list of things being worked on can be found in the `docs/plan` file.


