Link Options for Onyx
===

## Preface

As Onyx compiles to WebAssembly, a sophisticated link-phase is not
necessary. That is why, up until a week ago, Onyx did not have a way
to specify any linking options, as Onyx could determine what it wants
to do with the linear memory space. However, as I am trying to use
Onyx with more things, I'm realizing that other WebAssembly runtimes
are a bit stricter, and expect things to be in a particular way.

Specifically, I was looking at the WASM4 "game engine" as something
interesting Onyx could target. As WASM4 is trying to be as restrictive
as possible to increase creativity, the memory layout for the program
is defined. This means that Onyx's default layout will not suffice.
Instead, you need to be able to control where the stack and data section
elements go in the program. I have recently renovated the code that
determines where a piece of data will be placed, as well as added a
"link-phase" to update all references in the program to the data section
element. Now I need to determine what options you will be allowed to
control and what the syntax / semantics / method of communication will
be. A good reference for which options should be supported is
[wasm-ld](https://lld.llvm.org/WebAssembly.html)

Because I am trying to stay away from a ton of command line options,
especially options that *required* for your program to compile and work
correctly, I would like specifying link options to be contained in the
syntax of the program. Command-line options should be reserved for
changing meta-level parameters about the program, such as the runtime
and whether or not to disable features based on where the program is
being compiled. *Fundamental* options, such as how to lay out the data
section, should be within the program.

## Proposal

That being said, I think the syntax should like so:

```onyx

#link_options .{
    .stack_size = 4096, // 1MiB
    .stack_alignment = 16, // Align the start of the stack to 16 bytes
    .stack_first = true, // Stack-before data section

    .null_reserve_size = 16, // Reserve 16 bytes for null

    .import_memory = true,
    .import_memory_module_name = "onyx",
    .import_memory_import_name = "memory",

    .memory_min_size = 16, // 16 * 65536 Bytes
    .memory_max_size = 24, // 24 * 65536 Bytes
}

```

There will only be one `#link_options` directive in the entire set
of included files. After it, it takes an expression that is of type
`runtime.Link_Options`, which is type inferred, as seen above. All
members of this structure will have default values given by the settings
that the program is compiling under. If no `#link_options` is provided,
these default values are used.

Alternatively, there could just be a optional variable in
`package runtime.vars` that would define the link options. And if
one is not specified than a definition in the standard library
would define it like so

```onyx
package runtime.vars

#if !#defined(link_options) {
    link_options :: runtime.Link_Options.{}
}

```

This would simplify a lot, as there would not have to be any other logic
to deduplicate multiple `#link_options`. The only inconvenience is that
it will have to be part of a separate package, which currently means
a separate file. That is a separate issue that will hopefully be tackled
later.
