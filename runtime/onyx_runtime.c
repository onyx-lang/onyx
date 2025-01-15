
#define BH_DEFINE
#include "bh.h"

#define ONYX_LIBRARY_NAME onyx_runtime
#define ONYX_NO_SHORT_NAMES
#include "onyx_library.h"

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    #include <pthread.h>
    #include <signal.h>
    #include <sys/wait.h>
    #include <sys/types.h>
    #include <dlfcn.h>
    #include <dirent.h>
    #include <arpa/inet.h>
    #include <netdb.h>
    #include <netinet/in.h>
    #include <sys/socket.h>
    #include <sys/un.h>
    #include <sys/syscall.h>
    #include <sys/random.h>
    #include <poll.h>
    #include <termios.h>
    #include <sys/ioctl.h>
    #include <unistd.h>
#endif

#if defined(_BH_LINUX)
    #include <linux/futex.h>
#endif

#if defined(_BH_DARWIN)
    #include <Security/Security.h>
#endif

#include "types.h"  // For POINTER_SIZE

#include "src/ort_files.h"
#include "src/ort_directories.h"
#include "src/ort_threads.h"
#include "src/ort_processes.h"
#include "src/ort_os.h"
#include "src/ort_cptr.h"
#include "src/ort_tty.h"

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include "src/ort_net_linux.h"
#endif

#ifdef _BH_WINDOWS
#include "src/ort_net_windows.h"
#endif


ONYX_LIBRARY {
    ONYX_FUNC(__file_open_impl)
    ONYX_FUNC(__file_close)
    ONYX_FUNC(__file_exists)
    ONYX_FUNC(__file_stat)
    ONYX_FUNC(__file_remove)
    ONYX_FUNC(__file_seek)
    ONYX_FUNC(__file_tell)
    ONYX_FUNC(__file_read)
    ONYX_FUNC(__file_write)
    ONYX_FUNC(__file_flush)
    ONYX_FUNC(__file_size)
    ONYX_FUNC(__file_get_standard)
    ONYX_FUNC(__file_rename)
    ONYX_FUNC(__poll)

    ONYX_FUNC(__dir_open)
    ONYX_FUNC(__dir_read)
    ONYX_FUNC(__dir_close)
    ONYX_FUNC(__dir_create)
    ONYX_FUNC(__dir_remove)
    ONYX_FUNC(__dir_cwd)
    ONYX_FUNC(__dir_chdir)

    ONYX_FUNC(__spawn_thread)
    ONYX_FUNC(__kill_thread)

    ONYX_FUNC(__process_spawn)
    ONYX_FUNC(__process_read)
    ONYX_FUNC(__process_write)
    ONYX_FUNC(__process_kill)
    ONYX_FUNC(__process_wait)
    ONYX_FUNC(__process_destroy)

    ONYX_FUNC(__args_get)
    ONYX_FUNC(__args_sizes_get)

    ONYX_FUNC(__exit)
    ONYX_FUNC(__sleep)
    ONYX_FUNC(__time)
    ONYX_FUNC(__lookup_env)
    ONYX_FUNC(__random_get)
    ONYX_FUNC(__futex_wait)
    ONYX_FUNC(__futex_wake)
    ONYX_FUNC(__tty_get)
    ONYX_FUNC(__tty_set)
    ONYX_FUNC(__register_cleanup)

    ONYX_FUNC(__net_create_socket)
    ONYX_FUNC(__net_close_socket)
    ONYX_FUNC(__net_setting_flag)
    ONYX_FUNC(__net_bind_unix)
    ONYX_FUNC(__net_bind_ipv4)
    ONYX_FUNC(__net_bind_host)
    ONYX_FUNC(__net_listen)
    ONYX_FUNC(__net_accept)
    ONYX_FUNC(__net_connect_unix)
    ONYX_FUNC(__net_connect_ipv4)
    ONYX_FUNC(__net_connect_ipv6)
    ONYX_FUNC(__net_connect_host)
    ONYX_FUNC(__net_shutdown)
    ONYX_FUNC(__net_send)
    ONYX_FUNC(__net_sendto_unix)
    ONYX_FUNC(__net_sendto_ipv4)
    ONYX_FUNC(__net_sendto_ipv6)
    ONYX_FUNC(__net_sendto_host)
    ONYX_FUNC(__net_recv)
    ONYX_FUNC(__net_recvfrom)
    ONYX_FUNC(__net_resolve_start)
    ONYX_FUNC(__net_resolve_next)
    ONYX_FUNC(__net_resolve_end)

    ONYX_FUNC(__cptr_make)
    ONYX_FUNC(__cptr_read)
    ONYX_FUNC(__cptr_read_u8)
    ONYX_FUNC(__cptr_read_u16)
    ONYX_FUNC(__cptr_read_u32)
    ONYX_FUNC(__cptr_read_u64)
    ONYX_FUNC(__cptr_extract_str)

    NULL
};
