
#define BH_DEFINE
#include "bh.h"

#define ONYX_LIBRARY_NAME onyx_runtime
#define ONYX_NO_SHORT_NAMES
#include "onyx_library.h"

#ifdef _BH_LINUX
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
    #include <poll.h>
#endif

#include "types.h"  // For POINTER_SIZE

#include "src/ort_files.h"
#include "src/ort_directories.h"
#include "src/ort_threads.h"
#include "src/ort_processes.h"
#include "src/ort_os.h"
#include "src/ort_time.h"
#include "src/ort_cptr.h"
#include "src/ort_net.h"


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

    ONYX_FUNC(__dir_open)
    ONYX_FUNC(__dir_read)
    ONYX_FUNC(__dir_close)
    ONYX_FUNC(__dir_create)
    ONYX_FUNC(__dir_remove)

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

    ONYX_FUNC(__time_localtime)
    ONYX_FUNC(__time_gmtime)
    ONYX_FUNC(__time_strftime)
    ONYX_FUNC(__time_strptime)

    ONYX_FUNC(__net_create_socket)
    ONYX_FUNC(__net_close_socket)
    ONYX_FUNC(__net_setting)
    ONYX_FUNC(__net_bind)
    ONYX_FUNC(__net_listen)
    ONYX_FUNC(__net_accept)
    ONYX_FUNC(__net_connect_unix)
    ONYX_FUNC(__net_connect_ipv4)
    ONYX_FUNC(__net_send)
    ONYX_FUNC(__net_sendto)
    ONYX_FUNC(__net_recv)
    ONYX_FUNC(__net_recvfrom)
    ONYX_FUNC(__net_poll_recv)
    ONYX_FUNC(__net_host_to_net_s)
    ONYX_FUNC(__net_host_to_net_l)
    ONYX_FUNC(__net_net_to_host_s)
    ONYX_FUNC(__net_net_to_host_l)

    ONYX_FUNC(__cptr_make)
    ONYX_FUNC(__cptr_read)
    ONYX_FUNC(__cptr_read_u8)
    ONYX_FUNC(__cptr_read_u16)
    ONYX_FUNC(__cptr_read_u32)
    ONYX_FUNC(__cptr_read_u64)
    ONYX_FUNC(__cptr_extract_str)

    NULL
};
