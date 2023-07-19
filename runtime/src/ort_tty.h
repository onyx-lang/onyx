
struct Onyx_TTY_State {
    int rows, columns;
    unsigned char stdin_is_tty, stdout_is_tty, stderr_is_tty;
    unsigned char echo, input_buffered, input_linefeeds;
};

ONYX_DEF(__tty_get, (WASM_I32), ()) {
    struct Onyx_TTY_State *state = ONYX_PTR(params->data[0].of.i32);

#ifdef _BH_LINUX
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
    state->linefeeds_ignored = 0;
#endif

    return NULL;
}

ONYX_DEF(__tty_set, (WASM_I32), (WASM_I32)) {
    struct Onyx_TTY_State *state = ONYX_PTR(params->data[0].of.i32);

#ifdef _BH_LINUX
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


