
//
// Networking
//

struct onyx_socket_addr {
    unsigned short family;
    unsigned short port;
    unsigned int   addr;
};

static inline int onyx_socket_domain(int i) {
    #if defined(_BH_LINUX)
    switch (i) {    // :EnumDependent
        case 0: return AF_UNIX;
        case 1: return AF_INET;
        case 2: return AF_INET6;
        default: return -1;
    }
    #elif defined(_BH_WINDOWS)
    return -1;
    #endif
}

static inline int onyx_socket_protocol(int i) {
    #if defined(_BH_LINUX)
    switch (i) {    // :EnumDependent
        case 0: return SOCK_STREAM;
        case 1: return SOCK_DGRAM;
        default: return -1;
    }
    #elif defined(_BH_WINDOWS)
    return -1;
    #endif
}

ONYX_DEF(__net_create_socket, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {

    #ifdef _BH_LINUX
    int domain = onyx_socket_domain(params->data[1].of.i32);
    if (domain == -1) goto bad_settings;

    int type = onyx_socket_protocol(params->data[2].of.i32);
    if (type == -1) goto bad_settings;

    *((int *) ONYX_PTR(params->data[0].of.i32)) = socket(domain, type, 0);

    results->data[0] = WASM_I32_VAL(0);
    return NULL;
    #endif 

    #ifdef _BH_WINDOWS
    #endif

bad_settings:
    results->data[0] = WASM_I32_VAL(1); // :EnumDependent
    return NULL;
}

ONYX_DEF(__net_close_socket, (WASM_I32), ()) {
    #ifdef _BH_LINUX
    shutdown(params->data[0].of.i32, SHUT_RDWR);
    close(params->data[0].of.i32);
    #endif

    #ifdef _BH_WINDOWS
    #endif

    return NULL;
}

ONYX_DEF(__net_setting, (WASM_I32, WASM_I32, WASM_I32), ()) {
    #ifdef _BH_LINUX
    switch (params->data[1].of.i32) {
        case 1: { // :EnumDependent  Non-Blocking
            int s = params->data[0].of.i32;
            int flags = fcntl(s, F_GETFL, 0);
            if (params->data[2].of.i32) {
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
    #endif

    return NULL;
}

ONYX_DEF(__net_bind, (WASM_I32, WASM_I32), (WASM_I32)) {

    #ifdef _BH_LINUX
    int res = -1;

    struct onyx_socket_addr *oaddr = (void *) ONYX_PTR(params->data[1].of.i32);
    int family = onyx_socket_domain(oaddr->family);
    int port   = oaddr->port;

    switch (family) {
        case AF_INET: {
            struct sockaddr_in bind_addr;
            memset(&bind_addr, 0, sizeof(bind_addr));

            bind_addr.sin_family = AF_INET;
            bind_addr.sin_addr.s_addr = htonl(oaddr->addr);
            bind_addr.sin_port = htons(port);

            res = bind(params->data[0].of.i32, &bind_addr, sizeof(bind_addr));
            break;
        }

        case AF_INET6: {
            struct sockaddr_in6 bind_addr;
            memset(&bind_addr, 0, sizeof(bind_addr));

            bind_addr.sin6_family = AF_INET6;
            memcpy(&bind_addr.sin6_addr.s6_addr, (void *) &oaddr->addr, 16);
            bind_addr.sin6_port = htons(port);
            
            res = bind(params->data[0].of.i32, &bind_addr, sizeof(bind_addr));
            break;
        }

        case AF_UNIX: {
            struct sockaddr_un bind_addr;
            memset(&bind_addr, 0, sizeof(bind_addr));

            bind_addr.sun_family = AF_UNIX;
            strncpy(&bind_addr.sun_path, (char *) &oaddr->addr, 108);
            
            res = bind(params->data[0].of.i32, &bind_addr, sizeof(bind_addr));
            break;
        }
    }

    results->data[0] = WASM_I32_VAL(res >= 0);
    #endif

    #ifdef _BH_WINDOWS
    #endif

    return NULL;
}

ONYX_DEF(__net_listen, (WASM_I32, WASM_I32), ()) {
    #ifdef _BH_LINUX
    listen(params->data[0].of.i32, params->data[1].of.i32);
    #endif

    #ifdef _BH_WINDOWS
    #endif

    return NULL;
}

ONYX_DEF(__net_accept, (WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    struct sockaddr_in client_addr;
    int client_len = sizeof(client_addr);
    memset(&client_addr, 0, client_len);

    int client_socket = accept(params->data[0].of.i32, &client_addr, &client_len);

    struct onyx_socket_addr* out_addr = (struct onyx_socket_addr *) ONYX_PTR(params->data[1].of.i32); 
    out_addr->family = client_addr.sin_family;
    out_addr->port   = ntohs(client_addr.sin_port);
    out_addr->addr   = ntohl(client_addr.sin_addr.s_addr);

    results->data[0] = WASM_I32_VAL(client_socket);
    #endif

    return NULL;
}

ONYX_DEF(__net_connect_unix, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    int   hostlen  = params->data[2].of.i32;
    char *hostname = alloca(hostlen + 1);
    memcpy(hostname, ONYX_PTR(params->data[1].of.i32), hostlen);
    hostname[hostlen] = '\0';

    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));

    server_addr.sun_family = AF_UNIX; // See comment above
    memcpy((char *)&server_addr.sun_path, hostname, hostlen);

    int result = connect(params->data[0].of.i32, &server_addr, sizeof(server_addr));
    if (result == 0) results->data[0] = WASM_I32_VAL(0);
    else             results->data[0] = WASM_I32_VAL(3); // :EnumDependent
    #endif
    
    return NULL;
}

ONYX_DEF(__net_connect_ipv4, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    int   hostlen  = params->data[2].of.i32;
    char *hostname = alloca(hostlen + 1);
    memcpy(hostname, ONYX_PTR(params->data[1].of.i32), hostlen);
    hostname[hostlen] = '\0';

    struct hostent *host;
    host = gethostbyname(hostname); // TODO: Replace this call, as it is obselete.
    if (host == NULL) {
        results->data[0] = WASM_I32_VAL(2);  // :EnumDependent
        return NULL;
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));

    server_addr.sin_family = AF_INET; // See comment above
    memcpy((char *)&server_addr.sin_addr.s_addr, (char *)host->h_addr, host->h_length);
    server_addr.sin_port = htons(params->data[3].of.i32);

    int result = connect(params->data[0].of.i32, &server_addr, sizeof(server_addr));
    if (result == 0) results->data[0] = WASM_I32_VAL(0);
    else             results->data[0] = WASM_I32_VAL(3); // :EnumDependent

    return NULL;
    #endif

    #ifdef _BH_WINDOWS
    #endif
}

ONYX_DEF(__net_shutdown, (WASM_I32, WASM_I32), ()) {
    #ifdef _BH_LINUX
    shutdown(params->data[0].of.i32, params->data[1].of.i32);
    return NULL;
    #endif

    #ifdef _BH_WINDOWS
    #endif
}

ONYX_DEF(__net_send, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    // TODO: The flags at the end should be controllable.
    int sent = send(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, MSG_NOSIGNAL);
    results->data[0] = WASM_I32_VAL(sent);
    #endif
    
    return NULL;
}

ONYX_DEF(__net_sendto, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    struct sockaddr_in dest_addr;
    int dest_addr_len = sizeof(dest_addr);
    memset(&dest_addr, 0, dest_addr_len);

    struct onyx_socket_addr *o_addr = (struct onyx_socket_addr *) ONYX_PTR(params->data[3].of.i32);
    dest_addr.sin_family = AF_INET; // TODO: See other comments related to AF_NET above.
    dest_addr.sin_port = htons(o_addr->port);
    dest_addr.sin_addr.s_addr = htonl(o_addr->addr);

    // TODO: The flags at the end should be controllable.
    int sent = sendto(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, MSG_NOSIGNAL, &dest_addr, dest_addr_len);
    results->data[0] = WASM_I32_VAL(sent);
    #endif
    
    return NULL;
}

ONYX_DEF(__net_recv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    *(i32 *) ONYX_PTR(params->data[3].of.i32) = 0;

    #ifdef _BH_LINUX
    // TODO: The flags at the end should be controllable.
    int received = recv(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, 0);
    results->data[0] = WASM_I32_VAL(received);

    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32) = 1;
        }
    }
    #endif

    return NULL;
}

ONYX_DEF(__net_recvfrom, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    *(i32 *) ONYX_PTR(params->data[4].of.i32) = 0;

    #ifdef _BH_LINUX
    struct onyx_socket_addr *out_addr = (struct onyx_socket_addr *) ONYX_PTR(params->data[3].of.i32);

    struct sockaddr_in client_addr;
    int socket_len = sizeof(client_addr);
    memset(&client_addr, 0, socket_len);

    int received = recvfrom(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, 0, &client_addr, &socket_len);
    out_addr->family = client_addr.sin_family;
    out_addr->port   = ntohs(client_addr.sin_port);
    out_addr->addr   = ntohl(client_addr.sin_addr.s_addr);

    results->data[0] = WASM_I32_VAL(received);

    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32) = 1;
        }
    }
    #endif

    return NULL;
}

ONYX_DEF(__net_poll_recv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    #ifdef _BH_LINUX
    int i, res, cursor;
    struct pollfd* fds;

    fds = alloca(params->data[1].of.i32 * sizeof(struct pollfd));

    for (i=0; i < params->data[1].of.i32; i++) {
        fds[i].fd = *(i32 *) ONYX_PTR(params->data[0].of.i32 + 4 * i);
        fds[i].events = POLLIN;
        fds[i].revents = 0;
    }

    res = poll(fds, params->data[1].of.i32, params->data[2].of.i32);

    for (i=0; i<params->data[1].of.i32; i++) {
        *(i32 *) ONYX_PTR(params->data[3].of.i32 + 4 * i) = 0; // NO_CHANGE

        if (fds[i].revents & POLLIN) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32 + 4 * i) = 1; // READABLE
        }

        if ((fds[i].revents & POLLHUP)
            || (fds[i].revents & POLLNVAL)
            || (fds[i].revents & POLLERR)) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32 + 4 * i) = 2; // CLOSED
        }
    }

    #endif

    return NULL;
}

ONYX_DEF(__net_host_to_net_s, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(htons(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__net_host_to_net_l, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(htonl(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__net_net_to_host_s, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(ntohs(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__net_net_to_host_l, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(ntohl(params->data[0].of.i32));
    return NULL;
}
