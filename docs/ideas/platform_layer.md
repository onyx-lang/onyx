Onyx Standard Library Platform Layer
====================================

Onyx has *soft* definition for what is expected from the supporting
platform within its standard library. This should be solidified into
a concrete set of types and functions that are expected to be implemented,
or _explicity not_ implemented for a given platform.

This is where not having header files and having a fluid definition of
functions does hurt a little bit, because it is not possible to say
"This set of files should define all of these functions". Nonetheless,
this document will serve as that "header file"

## Always Expected

### Types

### Procedures
- `__start() -> void`

### Values
- `Supports_Files :: bool`
- `Supports_Directories :: bool`
- `Supports_Os :: bool`
- `Supports_Processes :: bool`
- `Supports_Time :: bool`
- `Supports_Networking :: bool`
- `Supports_Type_Info :: bool`
- `Supports_Threads :: bool`


## Files

### Types
- `FileData`

### Procedures
- `__file_open(path: str, mode: core.os.OpenMode) -> (FileData, core.os.FileError)`
- `__file_close(fd: FileData) -> core.os.FileError`
- `__file_stat(path: str, stat: ^os.FileStat) -> bool`
- `__file_exists(path: str) -> bool`
- `__file_remove(path: str) -> bool`
- `__file_rename(old_path, new_path: str) -> bool`

### Values
- `__file_stream_vtable: io.Stream_Vtable`


## Directories

### Types
- `DirectoryData`

### Procedures
- `__dir_open(path: str, dir: ^DirectoryData) -> bool`
- `__dir_close(dir: DirectoryData) -> void`
- `__dir_read(dir: DirectoryData, out_entry: ^os.DirectoryEntry) -> bool`
- `__dir_create(path: str) -> bool`
- `__dir_remove(path: str) -> bool`

### Values



## Standard I/O

### Types

### Procedures
- `__output_string(s: str) -> u32`
- `__output_error(s: str) -> u32`
- `__read_from_input(buffer: [] u8) -> i32`
    - `> 0` on success. Returns number of bytes
    - `0` on no-input, but not error.
    - `< 0` on error.

### Values


## OS

### Types

### Procedures
- `__exit(code: i32) -> void`
- `__sleep(milliseconds: i32) -> void`

### Values


## Time

### Types

### Procedures
- `__time() -> u64`
- `__time_localtime(time: u64, tm: ^core.time.Timestamp) -> void`
- `__time_gmtime(time: u64, tm: ^core.time.Timestamp) -> void`
- `__time_gmtime(tm: ^core.time.Timestamp) -> i64`
- `__time_strftime(buf: [] u8, format: cstr, tm: ^core.time.Timestamp) -> u32`

### Values


## Threads

### Types

### Procedures
- `__spawn_thread(id: i32, tls_base, stack_base: rawptr, func: (rawptr) -> void, data: rawptr) -> bool`
- `__kill_thread(id: i32) -> i32`

### Values


## Processes

### Types
- `ProcessData`

### Procedures
- `__process_spawn(path: str, args: [] str, non_blocking_io: bool, starting_directory: str) -> ProcessData`
- `__process_read(handle: ProcessData, buffer: [] u8) -> i32`
- `__process_write(handle: ProcessData, buffer: [] u8) -> i32`
- `__process_kill(handle: ProcessData) -> bool`
- `__process_wait(handle: ProcessData) -> os.ProcessResult`
- `__process_destroy(handle: ProcessData) -> void`

### Values


## Environment Variables

### Procedures
- `__get_all_env() -> [] Pair(str, str)`
- `__get_env(key: str) -> str`



