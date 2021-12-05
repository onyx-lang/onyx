Pluggable Modules
-----------------


After using the 'onyx run' functionality for a while, I've come to the realization
that maybe pushing that feature farther may be the future of Onyx. Onyx can still
target WebAssembly and be used for performance based web-applications. But using
libwasmer and providing a way to do "pluggable modules" gives me the ability to use
Onyx more natively. I'm basing this functionality on Lua's approach to C interop,
which does seem a little cumbersome, but also is easy enough to understand and
program for.


The canonical name for this with be a "library", where "module" is reserved for a
collection of "packages". A library is included in Onyx using the following syntax:

    // Includes "some_useful_code.so / .dll" in this execution.
    // Currently, this will only be allowed with the "onyx" runtime.
    #library "some_useful_code"

    // #library_path can be used to add to the places that libraries are
    // searched for.
    #if runtime.OS == runtime.OS_Linux {
        #library_path "./libs/linux"
    } else {
        #library_path ".\libs\windows"
    }

Libraries will be shared objects files or dlls, depending on the OS. Hopefully, that
is the only change that has to be added to the Onyx language itself. Everything else
will be handled behind the scenes from the programmer.

The "behinds the scenes" stuff is as follows:
    - Locate and load the library using dlopen or LoadLibrary.
    - Lookup the predefined function that will return a pointer to the table describing
        the provided functions.
    - During the module linking phase (aka building the import table), these functions
        will be considered as options.




A typical Onyx file for a library will look like:

    package some_useful_code

    #library "some_useful_code"

    the_code :: (a, b: i32) -> i32 #foreign "some_useful_code" "the_code" ---


The corresponding C file that will be compiled to a so/dll looks like:

    #include "onyx_module.h"

    #define ONYX_LIBRARY_NAME some_useful_code

    ONYX_DEF(the_code, (I32, I32), (I32)) {
        i32 a = params->data[0].of.i32;
        i32 b = params->data[1].of.i32;
        results->data[0] = WASM_I32_VAL(a + b);
        return NULL;
    }

    ONYX_LIBRARY {
        ONYX_FUNC(the_code)
    }

Compiling the C file with:

    gcc -o some_useful_code.so -shared -fPIC some_useful_code.c -I ...


A couple of questions that need to be answered:
    - How is the WASM memory object going to be given to the shared object
        code? Can it just be a public symbol that gets linked against? or
        does it need to be passed to an initialization function?