Onyx includes these external libraries:
    - wasmer: This is used to run WebAssembly directly from Onyx using 'onyx run'. Instead of relying on the host system to have Wasmer installed, it is included in the project. This will need to be updated as Wasmer is updated.
    - dyncall: Allows for dynamically calling arbitrary C function at runtime.
