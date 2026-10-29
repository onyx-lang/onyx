
struct Onyx_TTY_State {
    int rows, columns;
    unsigned char stdin_is_tty, stdout_is_tty, stderr_is_tty;
    unsigned char echo, input_buffered, input_linefeeds;
};

ONYX_DEF(__tty_get, (WASM_I32), ()) {
    struct Onyx_TTY_State *state = ONYX_PTR(params->data[0].of.i32);

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct winsize sz;
    ioctl(0, TIOCGWINSZ, &sz);
    state->rows = sz.ws_row;
    state->columns = sz.ws_col;

    struct termios term;
    state->stdout_is_tty = tcgetattr(1, &term) == 0;
    state->stderr_is_tty = tcgetattr(2, &term) == 0;
    state->stdin_is_tty = tcgetattr(0, &term) == 0; // Do stdin last because we use it next.

    state->echo = (term.c_lflag & ECHO) != 0;
    state->input_buffered = (term.c_lflag & ICANON) != 0;
    state->input_linefeeds = (term.c_lflag & ONLCR) != 0;
#endif

#ifdef _BH_WINDOWS
    memset(state, 0, sizeof(*state));

    state->rows = 80;
    state->columns = 25;
    state->echo = 1;
    state->input_buffered = 1;
    state->input_linefeeds = 0;
#endif

    return NULL;
}

ONYX_DEF(__tty_set, (WASM_I32), (WASM_I32)) {
    struct Onyx_TTY_State *state = ONYX_PTR(params->data[0].of.i32);

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int success = 1;

    struct termios term;
    if (!tcgetattr(0, &term)) {
        if (state->echo) term.c_lflag |=  (ECHO | ECHOE | ECHOK | ECHOCTL | IEXTEN);
        else             term.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHOCTL | IEXTEN);

        if (state->input_buffered) term.c_lflag |=  ICANON;
        else                       term.c_lflag &= ~ICANON;

        if (state->input_linefeeds) term.c_lflag |=  ONLCR;
        else                        term.c_lflag &= ~ONLCR;

        success = tcsetattr(0, TCSANOW, &term) == 0;

    } else {
        success = 0;
    }

    results->data[0] = WASM_I32_VAL(success);
#endif

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(0);
#endif
    return NULL;
}

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static struct termios onyx_tty_saved;
static int onyx_tty_raw = 0;
static int onyx_tty_atexit_installed = 0;

static int onyx_tty_caught[] = {
    SIGINT, SIGTERM, SIGQUIT, SIGHUP, SIGABRT, SIGPIPE
};
#define ONYX_TTY_CAUGHT_COUNT ((int)(sizeof(onyx_tty_caught) / sizeof(onyx_tty_caught[0])))
static struct sigaction onyx_tty_old_actions[6];

static void onyx_tty_restore_termios(void) {
    if (!onyx_tty_raw) return;
    tcsetattr(STDIN_FILENO, TCSANOW, &onyx_tty_saved);
    onyx_tty_raw = 0;
}

static void onyx_tty_atexit_handler(void) {
    onyx_tty_restore_termios();
}

static void onyx_tty_on_signal(int signo) {
    struct sigaction old;
    int found = 0;
    int i;

    onyx_tty_restore_termios();
    for (i = 0; i < ONYX_TTY_CAUGHT_COUNT; i++) {
        if (onyx_tty_caught[i] == signo) {
            old = onyx_tty_old_actions[i];
            found = 1;
            break;
        }
    }
    if (found) sigaction(signo, &old, NULL);
    else signal(signo, SIG_DFL);
    raise(signo);
}

static void onyx_tty_make_raw(struct termios *t) {
    t->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    t->c_oflag &= ~OPOST;
    t->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    t->c_cflag &= ~(CSIZE | PARENB);
    t->c_cflag |= CS8;
    t->c_cc[VMIN] = 0;
    t->c_cc[VTIME] = 0;
}
#endif

ONYX_DEF(__tty_enter_raw, (), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct termios raw;
    int i;

    if (onyx_tty_raw) {
        results->data[0] = WASM_I32_VAL(1);
        return NULL;
    }

    if (tcgetattr(STDIN_FILENO, &onyx_tty_saved) != 0) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    raw = onyx_tty_saved;
    onyx_tty_make_raw(&raw);
    if (tcsetattr(STDIN_FILENO, TCSANOW, &raw) != 0) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    onyx_tty_raw = 1;
    if (!onyx_tty_atexit_installed) {
        atexit(onyx_tty_atexit_handler);
        onyx_tty_atexit_installed = 1;
    }

    for (i = 0; i < ONYX_TTY_CAUGHT_COUNT; i++) {
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_handler = onyx_tty_on_signal;
        sigemptyset(&sa.sa_mask);
        sigaction(onyx_tty_caught[i], &sa, &onyx_tty_old_actions[i]);
    }

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#else
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
#endif
}

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static int onyx_winch_fds[2] = {-1, -1};
static struct sigaction onyx_winch_old;
static int onyx_winch_installed = 0;

static void onyx_winch_on_signal(int signo) {
    int fd;
    char b;
    ssize_t n;

    (void)signo;
    fd = onyx_winch_fds[1];
    if (fd < 0) return;
    b = 1;
    n = write(fd, &b, 1);
    (void)n;
}
#endif

// Stops this process group with SIGTSTP. Returns after the process continues.
ONYX_DEF(__tty_raise_tstp, (), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct sigaction sa, old;
    sigset_t unblock, previous;

    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGTSTP, &sa, &old);

    sigemptyset(&unblock);
    sigaddset(&unblock, SIGTSTP);
    pthread_sigmask(SIG_UNBLOCK, &unblock, &previous);

    // Delivered before raise returns. The process is stopped until SIGCONT.
    int rc = raise(SIGTSTP);

    pthread_sigmask(SIG_SETMASK, &previous, NULL);
    sigaction(SIGTSTP, &old, NULL);
    results->data[0] = WASM_I32_VAL(rc == 0);
    return NULL;
#else
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
#endif
}

ONYX_DEF(__tty_winch_install, (), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int fds[2];
    struct sigaction sa;
    sigset_t unblock;

    if (onyx_winch_installed) {
        results->data[0] = WASM_I32_VAL(1);
        return NULL;
    }
    if (pipe(fds) != 0) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }
    fcntl(fds[0], F_SETFL, O_NONBLOCK);
    fcntl(fds[1], F_SETFL, O_NONBLOCK);
    fcntl(fds[0], F_SETFD, FD_CLOEXEC);
    fcntl(fds[1], F_SETFD, FD_CLOEXEC);
    onyx_winch_fds[0] = fds[0];
    onyx_winch_fds[1] = fds[1];

    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = onyx_winch_on_signal;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    sigaction(SIGWINCH, &sa, &onyx_winch_old);

    sigemptyset(&unblock);
    sigaddset(&unblock, SIGWINCH);
    pthread_sigmask(SIG_UNBLOCK, &unblock, NULL);

    onyx_winch_installed = 1;
    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#else
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
#endif
}

ONYX_DEF(__tty_winch_remove, (), ()) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int read_fd, write_fd;

    if (!onyx_winch_installed) return NULL;
    sigaction(SIGWINCH, &onyx_winch_old, NULL);
    onyx_winch_installed = 0;
    read_fd = onyx_winch_fds[0];
    write_fd = onyx_winch_fds[1];
    onyx_winch_fds[0] = -1;
    onyx_winch_fds[1] = -1;
    if (read_fd >= 0) close(read_fd);
    if (write_fd >= 0) close(write_fd);
#endif
    return NULL;
}

ONYX_DEF(__tty_wake, (), ()) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int fd;
    char b;
    ssize_t n;

    fd = onyx_winch_fds[1];
    if (fd < 0) return NULL;
    b = 1;
    n = write(fd, &b, 1);
    (void)n;
#endif
    return NULL;
}

// Bits: 1 stdin, 2 wake pipe. 0 is a timeout. -1 is an error.
// timeout_ms < 0 waits until stdin or the wake pipe is ready.
ONYX_DEF(__tty_wait, (WASM_I32), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct pollfd fds[2];
    int nfds;
    int timeout;
    int rc;
    int bits;
    char buf[64];

    timeout = params->data[0].of.i32;
    nfds = 1;
    fds[0].fd = STDIN_FILENO;
    fds[0].events = POLLIN;
    fds[0].revents = 0;
    if (onyx_winch_fds[0] >= 0) {
        fds[1].fd = onyx_winch_fds[0];
        fds[1].events = POLLIN;
        fds[1].revents = 0;
        nfds = 2;
    }

    do {
        rc = poll(fds, nfds, timeout);
    } while (rc < 0 && errno == EINTR);

    if (rc < 0) {
        results->data[0] = WASM_I32_VAL(-1);
        return NULL;
    }
    if (rc == 0) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    bits = 0;
    if (fds[0].revents & (POLLIN | POLLHUP | POLLERR | POLLNVAL)) bits |= 1;
    if (nfds == 2 && (fds[1].revents & (POLLIN | POLLHUP | POLLERR | POLLNVAL))) {
        bits |= 2;
        while (read(onyx_winch_fds[0], buf, sizeof(buf)) > 0) {}
    }
    results->data[0] = WASM_I32_VAL(bits);
    return NULL;
#else
    (void)params;
    results->data[0] = WASM_I32_VAL(-1);
    return NULL;
#endif
}

ONYX_DEF(__tty_leave_raw, (), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int i;

    if (onyx_tty_raw) {
        for (i = 0; i < ONYX_TTY_CAUGHT_COUNT; i++) {
            sigaction(onyx_tty_caught[i], &onyx_tty_old_actions[i], NULL);
        }
        onyx_tty_restore_termios();
    }

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#else
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
#endif
}


