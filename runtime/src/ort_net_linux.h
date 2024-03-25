
//
// Networking
//

#define SOCKET_ERROR_NONE 0
#define SOCKET_ERROR_BAD_SETTINGS 1
#define SOCKET_ERROR_CONNECT_FAILED 1

struct onyx_socket_addr {
    unsigned short family;
    unsigned short port;
    unsigned int   addr;
};

static inline int onyx_socket_family(int i) {
    // :EnumDependent
    switch (i) {
        case 1: return AF_INET;
        case 2: return AF_INET6;
        case 3: return AF_UNIX;
        default: return -1;
    }
}

static inline int onyx_socket_socktype(int i) {
    // :EnumDependent
    switch (i) {
        case 0: return SOCK_STREAM;
        case 1: return SOCK_DGRAM;
        default: return -1;
    }
}

static inline int socket_family_to_onyx(int i) {
    // :EnumDependent
    switch (i) {
        case AF_INET:  return 1;
        case AF_INET6: return 2;
        case AF_UNIX:  return 3;
        default: return -1;
    }
}

static inline int socket_socktype_to_onyx(int i) {
    // :EnumDependent
    switch (i) {
        case SOCK_STREAM: return 0;
        case SOCK_DGRAM:  return 1;
        default: return -1;
    }
}

ONYX_DEF(__net_create_socket, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int family = onyx_socket_family(params->data[1].of.i32);
    if (family == -1) goto bad_settings;

    int type = onyx_socket_socktype(params->data[2].of.i32);
    if (type == -1) goto bad_settings;

    int proto = params->data[3].of.i32;

    int sock = socket(family, type, proto);
    if (sock >= 0)
    {
        *((int *) ONYX_PTR(params->data[0].of.i32)) = sock;

        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_NONE);
        return NULL;
    }

bad_settings:
    results->data[0] = WASM_I32_VAL(SOCKET_ERROR_BAD_SETTINGS);
    return NULL;
}

ONYX_DEF(__net_close_socket, (WASM_I32), ()) {
    shutdown(params->data[0].of.i32, SHUT_RDWR);
    close(params->data[0].of.i32);

    return NULL;
}

ONYX_DEF(__net_setting_flag, (WASM_I32, WASM_I32, WASM_I32), ()) {
    switch (params->data[1].of.i32) {
        case 1: { // :EnumDependent  Non-Blocking
            int s = params->data[0].of.i32;
            int flags = fcntl(s, F_GETFL, 0);
            if (params->data[2].of.i32 > 0) {
                flags |= O_NONBLOCK;
            } else {
                flags &= ~O_NONBLOCK;
            }

            fcntl(s, F_SETFL, flags);
            break;
        }

        case 2: { // :EnumDependent  Broadcast
            int s = params->data[0].of.i32;
            setsockopt(s, SOL_SOCKET, SO_BROADCAST, (void *) &params->data[2].of.i32, sizeof(int));
            break;
        }

        case 3: { // :EnumDependent  Reuse-Address
            int s = params->data[0].of.i32;
            setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (void *) &params->data[2].of.i32, sizeof(int));
            break;
        }
    }

    return NULL;
}

ONYX_DEF(__net_bind_unix, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *hostname = ONYX_PTR(params->data[1].of.i32);

    struct sockaddr_un bind_addr;
    memset(&bind_addr, 0, sizeof(bind_addr));

    bind_addr.sun_family = AF_UNIX;
    strncpy(&bind_addr.sun_path, hostname, 108);
    
    if (bind(params->data[0].of.i32, &bind_addr, sizeof(bind_addr)) >= 0) {
        results->data[0] = WASM_I32_VAL(1);
    } else {
        results->data[0] = WASM_I32_VAL(0);
    }

    return NULL;
}

ONYX_DEF(__net_bind_ipv4, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    struct sockaddr_in bind_addr;
    memset(&bind_addr, 0, sizeof(bind_addr));

    bind_addr.sin_family = AF_INET;
    bind_addr.sin_addr.s_addr = params->data[1].of.i32;
    bind_addr.sin_port = htons(params->data[2].of.i32);
    
    if (bind(params->data[0].of.i32, &bind_addr, sizeof(bind_addr)) >= 0) {
        results->data[0] = WASM_I32_VAL(1);
    } else {
        results->data[0] = WASM_I32_VAL(0);
    }

    return NULL;
}


ONYX_DEF(__net_bind_host, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int   hostlen  = params->data[2].of.i32;
    char *hostname = alloca(hostlen + 1);
    memcpy(hostname, ONYX_PTR(params->data[1].of.i32), hostlen);
    hostname[hostlen] = '\0';

    char portstr[8] = { 0 };
    bh_snprintf(portstr, 8, "%d", params->data[3].of.i32);

    struct addrinfo *result;
    if (getaddrinfo(hostname, portstr, NULL, &result) < 0)
    {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    if (bind(params->data[0].of.i32, result->ai_addr, result->ai_addrlen) >= 0) {
        results->data[0] = WASM_I32_VAL(1);
    } else {
        results->data[0] = WASM_I32_VAL(0);
    }

    return NULL;
}

ONYX_DEF(__net_listen, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(listen(params->data[0].of.i32, params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(__net_accept, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int client_socket = accept(
        params->data[0].of.i32,
        ONYX_PTR(params->data[1].of.i32),
        ONYX_PTR(params->data[2].of.i32)
    );

    if (client_socket < 0) {
        if (errno == EWOULDBLOCK || errno == EAGAIN) {
            client_socket = -2;
        }
    }

    results->data[0] = WASM_I32_VAL(client_socket);

    return NULL;
}

ONYX_DEF(__net_connect_unix, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *hostname = ONYX_PTR(params->data[1].of.i32);

    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));

    server_addr.sun_family = AF_UNIX; // See comment above
    memcpy((char *)& server_addr.sun_path, hostname, strnlen(hostname, 512)); // Arbitrary max path length

    int result = connect(params->data[0].of.i32, &server_addr, sizeof(server_addr));
    if (result == 0) {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_NONE);
    } else {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_CONNECT_FAILED);
    }
    
    return NULL;
}

ONYX_DEF(__net_connect_ipv4, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));

    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = params->data[1].of.i32;
    server_addr.sin_port = htons(params->data[2].of.i32);

    int result = connect(params->data[0].of.i32, &server_addr, sizeof(server_addr));
    if (result == 0) {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_NONE);
    } else {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_CONNECT_FAILED);
    }

    return NULL;
}

ONYX_DEF(__net_connect_ipv6, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    struct sockaddr_in6 server_addr;
    memset(&server_addr, 0, sizeof(server_addr));

    char *addr = ONYX_PTR(params->data[1].of.i32);

    server_addr.sin6_family = AF_INET6;
    memcpy(&server_addr.sin6_addr.s6_addr, addr, 16);
    server_addr.sin6_port = htons(params->data[2].of.i32);

    int result = connect(params->data[0].of.i32, &server_addr, sizeof(server_addr));
    if (result == 0) {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_NONE);
    } else {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_CONNECT_FAILED);
    }

    return NULL;
}

ONYX_DEF(__net_connect_host, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int   hostlen  = params->data[2].of.i32;
    char *hostname = alloca(hostlen + 1);
    memcpy(hostname, ONYX_PTR(params->data[1].of.i32), hostlen);
    hostname[hostlen] = '\0';

    char portstr[8] = { 0 };
    bh_snprintf(portstr, 8, "%d", params->data[3].of.i32);

    struct addrinfo *result;
    if (getaddrinfo(hostname, portstr, NULL, &result) < 0)
    {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_CONNECT_FAILED);
        return NULL;
    }

    int res = connect(params->data[0].of.i32, result->ai_addr, result->ai_addrlen);
    if (res == 0) {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_NONE);
    } else {
        results->data[0] = WASM_I32_VAL(SOCKET_ERROR_CONNECT_FAILED);
    }

    return NULL;
}

ONYX_DEF(__net_shutdown, (WASM_I32, WASM_I32), (WASM_I32)) {
    // This assumes that the `SocketShutdown` enum uses
    // the same values as what this function is expecting.
    //  Closing READ = 0
    //  Closing WRITE = 1
    //  Closing READ and WRITE = 2
    results->data[0] = WASM_I32_VAL(shutdown(params->data[0].of.i32, params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(__net_send, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int sent = send(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, MSG_NOSIGNAL);
    results->data[0] = WASM_I32_VAL(sent);

    // If there was an error sending, see if it was just a non-blocking issue.
    // In that case, return -2 to signal no error, but no data sent.
    if (sent < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }
    
    return NULL;
}

ONYX_DEF(__net_sendto_unix, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    struct sockaddr_un dest_addr;
    memset(&dest_addr, 0, sizeof(dest_addr));

    char *hostname = ONYX_PTR(params->data[3].of.i32);

    dest_addr.sun_family = AF_UNIX;
    memcpy((char *)& dest_addr.sun_path, hostname, strnlen(hostname, 512)); // Arbitrary max path length

    int sent = sendto(
        params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32,
        MSG_NOSIGNAL, &dest_addr, sizeof(dest_addr));
    results->data[0] = WASM_I32_VAL(sent);

    if (sent < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }
    return NULL;
}

ONYX_DEF(__net_sendto_ipv4, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    struct sockaddr_in dest_addr;
    memset(&dest_addr, 0, sizeof(dest_addr));

    char *addr = ONYX_PTR(params->data[3].of.i32);

    dest_addr.sin_family = AF_INET;
    dest_addr.sin_addr.s_addr = params->data[3].of.i32;
    dest_addr.sin_port = htons(params->data[4].of.i32);

    int sent = sendto(
        params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32,
        MSG_NOSIGNAL, &dest_addr, sizeof(dest_addr));
    results->data[0] = WASM_I32_VAL(sent);

    if (sent < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }
    return NULL;
}

ONYX_DEF(__net_sendto_ipv6, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    struct sockaddr_in6 dest_addr;
    memset(&dest_addr, 0, sizeof(dest_addr));

    char *addr = ONYX_PTR(params->data[3].of.i32);

    dest_addr.sin6_family = AF_INET6;
    memcpy(&dest_addr.sin6_addr.s6_addr, addr, 16);
    dest_addr.sin6_port = htons(params->data[4].of.i32);

    int sent = sendto(
        params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32,
        MSG_NOSIGNAL, &dest_addr, sizeof(dest_addr));
    results->data[0] = WASM_I32_VAL(sent);

    if (sent < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }
    return NULL;
}

ONYX_DEF(__net_sendto_host, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int   hostlen  = params->data[4].of.i32;
    char *hostname = alloca(hostlen + 1);
    memcpy(hostname, ONYX_PTR(params->data[3].of.i32), hostlen);
    hostname[hostlen] = '\0';

    char portstr[8] = { 0 };
    bh_snprintf(portstr, 8, "%d", params->data[5].of.i32);

    struct addrinfo *result;
    if (getaddrinfo(hostname, portstr, NULL, &result) < 0)
    {
        results->data[0] = WASM_I32_VAL(-1);
        return NULL;
    }

    int sent = sendto(
        params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32,
        MSG_NOSIGNAL, result->ai_addr, result->ai_addrlen);
    results->data[0] = WASM_I32_VAL(sent);

    if (sent < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }
    return NULL;
}

ONYX_DEF(__net_recv, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    // TODO: The flags at the end should be controllable.
    int received = recv(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, MSG_NOSIGNAL);
    results->data[0] = WASM_I32_VAL(received);

    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }

    return NULL;
}

ONYX_DEF(__net_recvfrom, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    int received = recvfrom(
        params->data[0].of.i32,
        ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32,
        MSG_NOSIGNAL,
        ONYX_PTR(params->data[3].of.i32), ONYX_PTR(params->data[4].of.i32));
    results->data[0] = WASM_I32_VAL(received);

    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            results->data[0] = WASM_I32_VAL(-2);
        }
    }

    return NULL;
}

ONYX_DEF(__net_resolve_start, (WASM_I32, WASM_I32), (WASM_I64)) {
    char *hostname = ONYX_PTR(params->data[0].of.i32);
    char portstr[8] = { 0 };
    bh_snprintf(portstr, 8, "%d", params->data[1].of.i32);

    struct addrinfo *result;
    int err = getaddrinfo(hostname, portstr, NULL, &result);
    if (err < 0) {
        results->data[0] = WASM_I64_VAL(0);
    } else {
        results->data[0] = WASM_I64_VAL((u64) result);
    }

    return NULL;
}

ONYX_DEF(__net_resolve_next, (WASM_I64, WASM_I32, WASM_I32), (WASM_I64)) {
    struct addrinfo *info = (struct addrinfo *) params->data[0].of.i64;
    char *buf = ONYX_PTR(params->data[1].of.i32);

    if (!info) {
        results->data[0] = WASM_I64_VAL(0);
        return NULL;
    }

    *(i32 *) &buf[0]  = socket_family_to_onyx(info->ai_family);
    *(i32 *) &buf[4]  = socket_socktype_to_onyx(info->ai_socktype);
    *(i32 *) &buf[8]  = info->ai_protocol;
    memcpy(&buf[12], info->ai_addr, bh_min(info->ai_addrlen, params->data[2].of.i32 - 12));

    results->data[0] = WASM_I64_VAL((u64) info->ai_next);
    return NULL;
}

ONYX_DEF(__net_resolve_end, (WASM_I64), ()) {
    struct addrinfo *info = (struct addrinfo *) params->data[0].of.i64;
    
    if (!info) return NULL;

    freeaddrinfo(info);

    return NULL;
}


