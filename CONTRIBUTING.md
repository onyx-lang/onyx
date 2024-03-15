# Contributing to Onyx

Thank you for wanting to contribute to Onyx!

Please read Onyx's [Code of Conduct][code-of-conduct] before contributing, as all contributing members are expected to adhere to it.

[code-of-conduct]: https://github.com/onyx-lang/onyx/blob/main/CODE_OF_CONDUCT.md

## Bug Reports

If you found a bug in Onyx, first ensure there is not already
a similar bug open on Onyx's [GitHub issue tracker][issues].
If there is not, feel free to open one.

[issues]: https://github.com/onyx-lang/onyx/issues

A bug could be anything from a compiler crash, to the wrong
output from a core library function. If you are unsure if
what you found is a bug, open an issue anyway and we'll
discuss from there.


## Code Changes

Contributing code changes to Onyx is welcome by following the process outlined below.

1. Find or open a GitHub issue relevant to the change you are wanting to make
   and comment saying you wish to work on this issue. This just helps me know
   what is getting done so I know I don't have to worry about something. If
   your change would include new functionality, this issue will serve as a place
   to discuss the details of what you are adding, to ensure everyone is in agreement
   on how the new functionality will work.
2. Create a fork and work on your code change.
3. When ready, open a GitHub pull request to merge your changes with the `master` branch.
   Once approved, your changes will be tested to ensure they pass CI/CD.
4. I will review the change and reserve the right to provide feedback and
   request modifications to the way something was implemented. There may be
   multiple rounds of feedback.
5. Once everything is approved, your code will be merged/rebased into the `master` branch.


## Development Environment

Since Onyx is developed in C, the barrier to entry is quite low.
You simply need a C compiler (gcc, clang and MSCV are known to work).

The only external dependency you may need is [Wasmer](https://wasmer.io)
installed and accessible by your user. Simply run `wasmer` to ensure it
is working. Note, this is not needed if you are on Windows, or compiling
using the `ovmwasm` runtime.

To compile on Linux and MacOS, you need to source the `settings.sh` file first.
This sets environment variables that are used by the build script.

```shell
source ./settings.sh
```

Now you should be ready to compile the code. Simply run the following on Linux/MacOS.

```shell
./build.sh compile install
```

Or the following on Windows.

```batch
build.bat
```

This will compile the code, place the new version in the `dist` folder,
and then copy the `dist` folder to your `ONYX_PATH`.

