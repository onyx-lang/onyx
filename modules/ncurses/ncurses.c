
#include "ncurses.h"

WINDOW *__get_stdscr() {
    return stdscr;
}

void __get_yx    (WINDOW *w, int *y, int *x) { getyx(w, *y, *x); }
void __get_par_yx(WINDOW *w, int *y, int *x) { getparyx(w, *y, *x); }
void __get_beg_yx(WINDOW *w, int *y, int *x) { getbegyx(w, *y, *x); }
void __get_max_yx(WINDOW *w, int *y, int *x) { getmaxyx(w, *y, *x); }

chtype __ACS_LOOKUP(int a) { return NCURSES_ACS(a); }



#define ONYX_LIBRARY_NAME onyx_ncurses
#include "onyx_library.h"

#define P(i, k) (params->data[i].of.k)

ONYX_DEF(initscr, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(initscr());
    return NULL;
}

ONYX_DEF(endwin, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(endwin());
    return NULL;
}

ONYX_DEF(isendwin, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(isendwin());
    return NULL;
}

ONYX_DEF(newterm, (WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(newterm(ONYX_PTR(P(0, i32)), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(set_term, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(set_term(P(0, i64)));
    return NULL;
}

ONYX_DEF(delscreen, (WASM_I64), ()) {
    delscreen(P(0, i64));
    return NULL;
}

ONYX_DEF(start_color, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(start_color());
    return NULL;
}

ONYX_DEF(has_colors, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(has_colors());
    return NULL;
}

ONYX_DEF(can_change_color, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(can_change_color());
    return NULL;
}

ONYX_DEF(init_pair, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(init_pair(P(0, i32), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(init_color, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(init_color(P(0, i32), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(color_content, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(color_content(P(0, i32), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(pair_content, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(pair_content(P(0, i32), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(reset_color_pairs, (), ()) {
    reset_color_pairs();
    return NULL;
}

ONYX_DEF(COLOR_PAIR, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(COLOR_PAIR(P(0, i32)));
    return NULL;
}

ONYX_DEF(PAIR_NUMBER, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(PAIR_NUMBER(P(0, i32)));
    return NULL;
}

ONYX_DEF(cbreak, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(cbreak());
    return NULL;
}

ONYX_DEF(nocbreak, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(nocbreak());
    return NULL;
}

ONYX_DEF(echo, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(echo());
    return NULL;
}

ONYX_DEF(noecho, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(noecho());
    return NULL;
}

ONYX_DEF(intrflush, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(intrflush((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(keypad, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(keypad((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(meta, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(meta((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(nodelay, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(nodelay((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(notimeout, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(notimeout((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(nl, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(nl());
    return NULL;
}

ONYX_DEF(nonl, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(nonl());
    return NULL;
}

ONYX_DEF(raw, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(raw());
    return NULL;
}

ONYX_DEF(noraw, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(noraw());
    return NULL;
}

ONYX_DEF(qiflush, (), ()) {
    qiflush();
    return NULL;
}

ONYX_DEF(noqiflush, (), ()) {
    noqiflush();
    return NULL;
}

ONYX_DEF(halfdelay, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(halfdelay(P(0, i32)));
    return NULL;
}

ONYX_DEF(timeout, (WASM_I32), ()) {
    timeout(P(0, i32));
    return NULL;
}

ONYX_DEF(wtimeout, (WASM_I64, WASM_I32), ()) {
    wtimeout((WINDOW *) P(0, i64), P(1, i32));
    return NULL;
}

ONYX_DEF(typeahead, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(typeahead(P(0, i32)));
    return NULL;
}

ONYX_DEF(clearkok, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(clearkok((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(idlok, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(idlok((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(idcok, (WASM_I64, WASM_I32), ()) {
    idcok((WINDOW *) P(0, i64), P(1, i32));
    return NULL;
}

ONYX_DEF(immedok, (WASM_I64, WASM_I32), ()) {
    immedok((WINDOW *) P(0, i64), P(1, i32));
    return NULL;
}

ONYX_DEF(leaveok, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(leaveok((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(scrollok, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(scrollok((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(setscrreg, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(setscrreg(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(wsetscrreg, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wsetscrreg((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(attr_get, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attr_get(ONYX_PTR(P(0, i32)), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(wattr_get, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattr_get((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(attr_set, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attr_set(ONYX_PTR(P(0, i32)), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(wattr_set, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattr_set((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(attr_off, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attr_off(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(wattr_off, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattr_off((WINDOW *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(attr_on, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attr_on(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(wattr_on, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattr_on((WINDOW *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(attroff, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attroff(P(0, i32)));
    return NULL;
}

ONYX_DEF(wattroff, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattroff((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(attron, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attron(P(0, i32)));
    return NULL;
}

ONYX_DEF(wattron, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattron((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(attrset, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(attrset(P(0, i32)));
    return NULL;
}

ONYX_DEF(wattrset, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wattrset((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(chgat, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(chgat(P(0, i32), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(wchgat, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wchgat((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), ONYX_PTR(P(4, i32))));
    return NULL;
}

ONYX_DEF(mvchgat, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvchgat(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), ONYX_PTR(P(5, i32))));
    return NULL;
}

ONYX_DEF(mvmchgat, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvmchgat((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), ONYX_PTR(P(6, i32))));
    return NULL;
}

ONYX_DEF(color_set, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(color_set(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(wcolor_set, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wcolor_set((WINDOW *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(standend, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(standend());
    return NULL;
}

ONYX_DEF(wstandend, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wstandend((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(standout, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(standout());
    return NULL;
}

ONYX_DEF(wstandout, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wstandout((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(unctrl, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(unctrl(P(0, i32)));
    return NULL;
}

ONYX_DEF(keyname, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(keyname(P(0, i32)));
    return NULL;
}

ONYX_DEF(key_name, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(key_name(P(0, i32)));
    return NULL;
}

ONYX_DEF(filter, (), ()) {
    filter();
    return NULL;
}

ONYX_DEF(nofilter, (), ()) {
    nofilter();
    return NULL;
}

ONYX_DEF(use_env, (WASM_I32), ()) {
    use_env(P(0, i32));
    return NULL;
}

ONYX_DEF(use_tioctl, (WASM_I32), ()) {
    use_tioctl(P(0, i32));
    return NULL;
}

ONYX_DEF(getwin, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(getwin(P(0, i32)));
    return NULL;
}

ONYX_DEF(delay_output, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(delay_output(P(0, i32)));
    return NULL;
}

ONYX_DEF(flushinp, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(flushinp());
    return NULL;
}

ONYX_DEF(def_prog_mode, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(def_prog_mode());
    return NULL;
}

ONYX_DEF(def_shell_mode, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(def_shell_mode());
    return NULL;
}

ONYX_DEF(reset_prog_mode, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(reset_prog_mode());
    return NULL;
}

ONYX_DEF(reset_shell_mode, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(reset_shell_mode());
    return NULL;
}

ONYX_DEF(resetty, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(resetty());
    return NULL;
}

ONYX_DEF(savetty, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(savetty());
    return NULL;
}

ONYX_DEF(getsyx, (WASM_I32, WASM_I32), ()) {
    getsyx(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(setsyx, (WASM_I32, WASM_I32), ()) {
    setsyx(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(curs_set, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(curs_set(P(0, i32)));
    return NULL;
}

ONYX_DEF(napms, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(napms(P(0, i32)));
    return NULL;
}

ONYX_DEF(refresh, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(refresh());
    return NULL;
}

ONYX_DEF(wrefresh, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wrefresh((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(wnoutrefresh, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wnoutrefresh((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(doupdate, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(doupdate());
    return NULL;
}

ONYX_DEF(redrawwin, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(redrawwin((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(wredrawln, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wredrawln((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(addch, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(addch(P(0, i32)));
    return NULL;
}

ONYX_DEF(waddch, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(waddch((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(mvaddch, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvaddch(P(0, i32), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(mvwaddch, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwaddch((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(echochar, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(echochar(P(0, i32)));
    return NULL;
}

ONYX_DEF(wechochar, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wechochar((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(addchstr, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(addchstr(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(addchnstr, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(addchnstr(ONYX_PTR(P(0, i32)), P(1, i32)));
    return NULL;
}

ONYX_DEF(waddchstr, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(waddchstr((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(waddchnstr, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(waddchnstr((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32)));
    return NULL;
}

ONYX_DEF(mvaddchstr, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvaddchstr(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(mvaddchnstr, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvaddchnstr(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32)));
    return NULL;
}

ONYX_DEF(mvwaddchstr, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwaddchstr((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(mvwaddchnstr, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwaddchnstr((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32)));
    return NULL;
}

ONYX_DEF(addstr, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(addstr(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(addnstr, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(addnstr(ONYX_PTR(P(0, i32)), P(1, i32)));
    return NULL;
}

ONYX_DEF(waddstr, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(waddstr((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(waddnstr, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(waddnstr((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32)));
    return NULL;
}

ONYX_DEF(mvaddstr, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvaddstr(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(mvaddnstr, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvaddnstr(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32)));
    return NULL;
}

ONYX_DEF(mvwaddstr, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwaddstr((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(mvwaddnstr, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwaddnstr((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32)));
    return NULL;
}

ONYX_DEF(erase, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(erase());
    return NULL;
}

ONYX_DEF(werase, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(werase((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(clear, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(clear());
    return NULL;
}

ONYX_DEF(wclear, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wclear((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(clrtobot, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(clrtobot());
    return NULL;
}

ONYX_DEF(wclrtobot, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wclrtobot((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(clrtoeol, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(clrtoeol());
    return NULL;
}

ONYX_DEF(wclrtoeol, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wclrtoeol((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_cleared, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_cleared((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_idcok, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_idcok((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_idlok, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_idlok((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_immedok, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_immedok((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_keypad, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_keypad((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_leaveok, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_leaveok((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_nodelay, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_nodelay((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_notimeout, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_notimeout((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_pad, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_pad((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_scrollok, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_scrollok((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_subwin, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_subwin((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_syncok, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_syncok((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(wgetparent, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(wgetparent((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(wgetdelay, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wgetdelay((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(newwin, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(newwin(P(0, i32), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(delwin, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(delwin((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(mvwin, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwin((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(subwin, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(subwin((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32)));
    return NULL;
}

ONYX_DEF(derwin, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(derwin((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32)));
    return NULL;
}

ONYX_DEF(mvderwin, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvderwin((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(dupwin, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(dupwin((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(wsyncup, (WASM_I64), ()) {
    wsyncup((WINDOW *) P(0, i64));
    return NULL;
}

ONYX_DEF(syncok, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(syncok((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(wcursyncup, (WASM_I64), ()) {
    wcursyncup((WINDOW *) P(0, i64));
    return NULL;
}

ONYX_DEF(wsyncdown, (WASM_I64), ()) {
    wsyncdown((WINDOW *) P(0, i64));
    return NULL;
}

ONYX_DEF(touchline, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(touchline((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(touchwin, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(touchwin((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(wtouchln, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wtouchln((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(untouchwin, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(untouchwin((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(is_linetouched, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_linetouched((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(is_wintouched, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(is_wintouched((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(bkgdset, (WASM_I32), ()) {
    bkgdset(P(0, i32));
    return NULL;
}

ONYX_DEF(wbkgdset, (WASM_I64, WASM_I32), ()) {
    wbkgdset((WINDOW *) P(0, i64), P(1, i32));
    return NULL;
}

ONYX_DEF(bkgd, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(bkgd(P(0, i32)));
    return NULL;
}

ONYX_DEF(wbkgd, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wbkgd((WINDOW *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(getbkgd, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(getbkgd((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(inch, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(inch());
    return NULL;
}

ONYX_DEF(winch, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(winch((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(mvinch, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvinch(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(mvwinch, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwinch((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(getch, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(getch());
    return NULL;
}

ONYX_DEF(wgetch, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wgetch((WINDOW *) P(0, i64)));
    return NULL;
}

ONYX_DEF(mvgetch, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvgetch(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(mvwgetch, (WASM_I32, WASM_I32, WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwgetch(P(0, i32), P(1, i32), (WINDOW *) P(2, i64)));
    return NULL;
}

ONYX_DEF(ungetch, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(ungetch(P(0, i32)));
    return NULL;
}

ONYX_DEF(has_key, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(has_key(P(0, i32)));
    return NULL;
}

ONYX_DEF(__get_yx, (WASM_I64, WASM_I32, WASM_I32), ()) {
    __get_yx((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(__get_par_yx, (WASM_I64, WASM_I32, WASM_I32), ()) {
    __get_par_yx((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(__get_beg_yx, (WASM_I64, WASM_I32, WASM_I32), ()) {
    __get_beg_yx((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(__get_max_yx, (WASM_I64, WASM_I32, WASM_I32), ()) {
    __get_max_yx((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(getstr, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(getstr(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(getnstr, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(getnstr(ONYX_PTR(P(0, i32)), P(1, i32)));
    return NULL;
}

ONYX_DEF(wgetstr, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wgetstr((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(wgetnstr, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wgetnstr((WINDOW *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32)));
    return NULL;
}

ONYX_DEF(mvgetstr, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvgetstr(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(mvgetnstr, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvgetnstr(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32)));
    return NULL;
}

ONYX_DEF(mvwgetstr, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwgetstr((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32))));
    return NULL;
}

ONYX_DEF(mvwgetnstr, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwgetnstr((WINDOW *) P(0, i64), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32)));
    return NULL;
}

ONYX_DEF(border, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(border(P(0, i32), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32)));
    return NULL;
}

ONYX_DEF(wborder, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wborder((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32), P(7, i32), P(8, i32)));
    return NULL;
}

ONYX_DEF(box, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(box((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(hline, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(hline(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(whline, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(whline((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(vline, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(vline(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(wvline, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(wvline((WINDOW *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(mvhline, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvhline(P(0, i32), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(mvwhline, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwhline((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32)));
    return NULL;
}

ONYX_DEF(mvvline, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvvline(P(0, i32), P(1, i32), P(2, i32), P(3, i32)));
    return NULL;
}

ONYX_DEF(mvwvline, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(mvwvline((WINDOW *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32)));
    return NULL;
}

ONYX_DEF(__get_stdscr, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(__get_stdscr());
    return NULL;
}

ONYX_DEF(NCURSES_ACS, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(NCURSES_ACS(P(0, i32)));
    return NULL;
}



ONYX_LIBRARY {
    ONYX_FUNC(initscr)
    ONYX_FUNC(endwin)
    ONYX_FUNC(isendwin)
    ONYX_FUNC(newterm)
    ONYX_FUNC(set_term)
    ONYX_FUNC(delscreen)
    ONYX_FUNC(start_color)
    ONYX_FUNC(has_colors)
    ONYX_FUNC(can_change_color)
    ONYX_FUNC(init_pair)
    ONYX_FUNC(init_color)
    ONYX_FUNC(color_content)
    ONYX_FUNC(pair_content)
    ONYX_FUNC(reset_color_pairs)
    ONYX_FUNC(COLOR_PAIR)
    ONYX_FUNC(PAIR_NUMBER)
    ONYX_FUNC(cbreak)
    ONYX_FUNC(nocbreak)
    ONYX_FUNC(echo)
    ONYX_FUNC(noecho)
    ONYX_FUNC(intrflush)
    ONYX_FUNC(keypad)
    ONYX_FUNC(meta)
    ONYX_FUNC(nodelay)
    ONYX_FUNC(notimeout)
    ONYX_FUNC(nl)
    ONYX_FUNC(nonl)
    ONYX_FUNC(raw)
    ONYX_FUNC(noraw)
    ONYX_FUNC(qiflush)
    ONYX_FUNC(noqiflush)
    ONYX_FUNC(halfdelay)
    ONYX_FUNC(timeout)
    ONYX_FUNC(wtimeout)
    ONYX_FUNC(typeahead)
    ONYX_FUNC(clearkok)
    ONYX_FUNC(idlok)
    ONYX_FUNC(idcok)
    ONYX_FUNC(immedok)
    ONYX_FUNC(leaveok)
    ONYX_FUNC(scrollok)
    ONYX_FUNC(setscrreg)
    ONYX_FUNC(wsetscrreg)
    ONYX_FUNC(attr_get)
    ONYX_FUNC(wattr_get)
    ONYX_FUNC(attr_set)
    ONYX_FUNC(wattr_set)
    ONYX_FUNC(attr_off)
    ONYX_FUNC(wattr_off)
    ONYX_FUNC(attr_on)
    ONYX_FUNC(wattr_on)
    ONYX_FUNC(attroff)
    ONYX_FUNC(wattroff)
    ONYX_FUNC(attron)
    ONYX_FUNC(wattron)
    ONYX_FUNC(attrset)
    ONYX_FUNC(wattrset)
    ONYX_FUNC(chgat)
    ONYX_FUNC(wchgat)
    ONYX_FUNC(mvchgat)
    ONYX_FUNC(mvmchgat)
    ONYX_FUNC(color_set)
    ONYX_FUNC(wcolor_set)
    ONYX_FUNC(standend)
    ONYX_FUNC(wstandend)
    ONYX_FUNC(standout)
    ONYX_FUNC(wstandout)
    ONYX_FUNC(unctrl)
    ONYX_FUNC(keyname)
    ONYX_FUNC(key_name)
    ONYX_FUNC(filter)
    ONYX_FUNC(nofilter)
    ONYX_FUNC(use_env)
    ONYX_FUNC(use_tioctl)
    ONYX_FUNC(getwin)
    ONYX_FUNC(delay_output)
    ONYX_FUNC(flushinp)
    ONYX_FUNC(def_prog_mode)
    ONYX_FUNC(def_shell_mode)
    ONYX_FUNC(reset_prog_mode)
    ONYX_FUNC(reset_shell_mode)
    ONYX_FUNC(resetty)
    ONYX_FUNC(savetty)
    ONYX_FUNC(getsyx)
    ONYX_FUNC(setsyx)
    ONYX_FUNC(curs_set)
    ONYX_FUNC(napms)
    ONYX_FUNC(refresh)
    ONYX_FUNC(wrefresh)
    ONYX_FUNC(wnoutrefresh)
    ONYX_FUNC(doupdate)
    ONYX_FUNC(redrawwin)
    ONYX_FUNC(wredrawln)
    ONYX_FUNC(addch)
    ONYX_FUNC(waddch)
    ONYX_FUNC(mvaddch)
    ONYX_FUNC(mvwaddch)
    ONYX_FUNC(echochar)
    ONYX_FUNC(wechochar)
    ONYX_FUNC(addchstr)
    ONYX_FUNC(addchnstr)
    ONYX_FUNC(waddchstr)
    ONYX_FUNC(waddchnstr)
    ONYX_FUNC(mvaddchstr)
    ONYX_FUNC(mvaddchnstr)
    ONYX_FUNC(mvwaddchstr)
    ONYX_FUNC(mvwaddchnstr)
    ONYX_FUNC(addstr)
    ONYX_FUNC(addnstr)
    ONYX_FUNC(waddstr)
    ONYX_FUNC(waddnstr)
    ONYX_FUNC(mvaddstr)
    ONYX_FUNC(mvaddnstr)
    ONYX_FUNC(mvwaddstr)
    ONYX_FUNC(mvwaddnstr)
    ONYX_FUNC(erase)
    ONYX_FUNC(werase)
    ONYX_FUNC(clear)
    ONYX_FUNC(wclear)
    ONYX_FUNC(clrtobot)
    ONYX_FUNC(wclrtobot)
    ONYX_FUNC(clrtoeol)
    ONYX_FUNC(wclrtoeol)
    ONYX_FUNC(is_cleared)
    ONYX_FUNC(is_idcok)
    ONYX_FUNC(is_idlok)
    ONYX_FUNC(is_immedok)
    ONYX_FUNC(is_keypad)
    ONYX_FUNC(is_leaveok)
    ONYX_FUNC(is_nodelay)
    ONYX_FUNC(is_notimeout)
    ONYX_FUNC(is_pad)
    ONYX_FUNC(is_scrollok)
    ONYX_FUNC(is_subwin)
    ONYX_FUNC(is_syncok)
    ONYX_FUNC(wgetparent)
    ONYX_FUNC(wgetdelay)
    ONYX_FUNC(newwin)
    ONYX_FUNC(delwin)
    ONYX_FUNC(mvwin)
    ONYX_FUNC(subwin)
    ONYX_FUNC(derwin)
    ONYX_FUNC(mvderwin)
    ONYX_FUNC(dupwin)
    ONYX_FUNC(wsyncup)
    ONYX_FUNC(syncok)
    ONYX_FUNC(wcursyncup)
    ONYX_FUNC(wsyncdown)
    ONYX_FUNC(touchline)
    ONYX_FUNC(touchwin)
    ONYX_FUNC(wtouchln)
    ONYX_FUNC(untouchwin)
    ONYX_FUNC(is_linetouched)
    ONYX_FUNC(is_wintouched)
    ONYX_FUNC(bkgdset)
    ONYX_FUNC(wbkgdset)
    ONYX_FUNC(bkgd)
    ONYX_FUNC(wbkgd)
    ONYX_FUNC(getbkgd)
    ONYX_FUNC(inch)
    ONYX_FUNC(winch)
    ONYX_FUNC(mvinch)
    ONYX_FUNC(mvwinch)
    ONYX_FUNC(getch)
    ONYX_FUNC(wgetch)
    ONYX_FUNC(mvgetch)
    ONYX_FUNC(mvwgetch)
    ONYX_FUNC(ungetch)
    ONYX_FUNC(has_key)
    ONYX_FUNC(__get_yx)
    ONYX_FUNC(__get_par_yx)
    ONYX_FUNC(__get_beg_yx)
    ONYX_FUNC(__get_max_yx)
    ONYX_FUNC(getstr)
    ONYX_FUNC(getnstr)
    ONYX_FUNC(wgetstr)
    ONYX_FUNC(wgetnstr)
    ONYX_FUNC(mvgetstr)
    ONYX_FUNC(mvgetnstr)
    ONYX_FUNC(mvwgetstr)
    ONYX_FUNC(mvwgetnstr)
    ONYX_FUNC(border)
    ONYX_FUNC(wborder)
    ONYX_FUNC(box)
    ONYX_FUNC(hline)
    ONYX_FUNC(whline)
    ONYX_FUNC(vline)
    ONYX_FUNC(wvline)
    ONYX_FUNC(mvhline)
    ONYX_FUNC(mvwhline)
    ONYX_FUNC(mvvline)
    ONYX_FUNC(mvwvline)
    ONYX_FUNC(__get_stdscr)
    ONYX_FUNC(NCURSES_ACS)
    NULL
};