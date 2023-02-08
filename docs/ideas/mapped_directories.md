Mapped Directories
==================

I am thinking about complicating how the file resolution
system works when loading new files. Currently, it is
intentionally very simple. There is a global "search path"
that will be sequentially appended to the file path being
loaded, and the first full path that is a file will be used.
This makes it easy to use, but also causes some confusion.

I think it would be worth investigating using a *relative only*
scheme for file resolution, UNLESS you specify the full path,
or a *mapped directory* to use as the base directory.

For example, instead of relying on "/usr/share/onyx" to be in
the search path to make "core/std" work, we would instead have
"core" be a mapped directory to "/usr/share/onyx/core". Then,
you would say, `#load "core:std"` or `#load core "std"` to
specify the base path.
