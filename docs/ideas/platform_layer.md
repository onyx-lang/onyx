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
- `Supports_Env_Vars :: bool`
- `Supports_Futexes :: bool`
- `Supports_TTY :: bool`


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
- `__wait_for_input(timeout: i32) -> bool`
    - waits for standard input to be readable
    - return `true` as the default

### Values


## OS

### Types

### Procedures
- `__exit(code: i32) -> void`
- `__sleep(milliseconds: i32) -> void`
- `__args(allocator: Allocator) -> [] cstr`

### Values


## Time

### Types

### Procedures
- `__time() -> u64`

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


## Futexes

### Procedures
- `__futex_wait(addr: rawptr, expected: i32, timeout: i32) -> i32`
- `__futex_wake(addr: rawptr, maximum: i32) -> i32`


## TTY

### Procedures
- `__tty_get(state: &core.os.TTY_State) -> void`
- `__tty_set(state: &core.os.TTY_State) -> bool`



## Networking

### Types
- `SocketData`

### Procedures
- `__net_sock_create(af: SocketFamily, type: SocketType, proto: SocketProto) -> Result(SocketData, io.Error)`
- `__net_sock_opt_flag(SocketData, sockopt: SocketOption, flag: bool) -> bool`
- `__net_sock_opt_time(SocketData, sockopt: SocketOption, time: ? u64) -> bool`
- `__net_sock_opt_size(SocketData, sockopt: SocketOption, size: i64) -> bool`
- `__net_sock_bind(SocketData, addr: &SocketAddress) -> bool`
- `__net_sock_listen(SocketData, backlog: i32) -> bool`
- `__net_sock_accept(SocketData, addr: &SocketAddress) -> Result(SocketData, io.Error)`
- `__net_sock_connect(SocketData, addr: &SocketAddress) -> io.Error`
- `__net_sock_recv_from(SocketData, buf: [] u8, addr: &SocketAddress) -> Result(i32, io.Error)`
- `__net_sock_send_to(SocketData, buf: [] u8, addr: &SocketAddress) -> Result(i32, io.Error)`
- `__net_sock_recv(SocketData, buf: [] u8) -> Result(i32, io.Error)`
- `__net_sock_send(SocketData, buf: [] u8) -> Result(i32, io.Error)`
- `__net_sock_shutdown(SocketData, how: SocketShutdown) -> io.Error`
- `__net_sock_close(SocketData) -> void`
- `__net_resolve(host: str, port: u16, out_addrs: [] SocketAddress) -> i32`


### Values

