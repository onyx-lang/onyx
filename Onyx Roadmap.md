# Onyx Roadmap

Short term (0.1.x):
- x Add 'poll' to 'Stream' vtable
    - Useful for reader and writer
    - poll(stream, .Read, timeout=-1) -> PollResponse
- x Move networking support to platform layer
- x Add WASIX support with flag
    - Will be meaningful when networking is in platform layer
- Switch interface checks to be separate from `where` statements

Medium term (0.2.x):
- x Change package file format from ".ini" to a better thing
    -  KDL
- x Add package index to Onyx website
- Create worksheets, like Ziglings
- Update and flush out Onyx Book
- Add OVM-Wasm support for Windows
- Better JS support (packages with JS components)
    - WebGL 2
    - WebGPU
    - etc

Long term (0.3.x - 1.0.0):
- Make standard library a separate package
- Consider if WIT or WITX is something Onyx should support
- Look into how native libraries should work on Windows

