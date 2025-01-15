
//
// Networking
//

#define SOCKET_ERROR_NONE 0
#define SOCKET_ERROR_BAD_SETTINGS 1
#define SOCKET_ERROR_CONNECT_FAILED 1

ONYX_DEF(__net_create_socket, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_close_socket, (WASM_I32), ()) {
    return NULL;
}

ONYX_DEF(__net_setting_flag, (WASM_I32, WASM_I32, WASM_I32), ()) {
    return NULL;
}

ONYX_DEF(__net_bind_unix, (WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_bind_ipv4, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}


ONYX_DEF(__net_bind_host, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_listen, (WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_accept, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_connect_unix, (WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_connect_ipv4, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_connect_ipv6, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_connect_host, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_shutdown, (WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_send, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_sendto_unix, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_sendto_ipv4, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_sendto_ipv6, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_sendto_host, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_recv, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_recvfrom, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    return NULL;
}

ONYX_DEF(__net_resolve_start, (WASM_I32, WASM_I32), (WASM_I64)) {
    return NULL;
}

ONYX_DEF(__net_resolve_next, (WASM_I64, WASM_I32, WASM_I32), (WASM_I64)) {
    return NULL;
}

ONYX_DEF(__net_resolve_end, (WASM_I64), ()) {
    return NULL;
}
