
//
// Files
//

#define ONYX_FILE_ERROR_NONE 0
#define ONYX_FILE_ERROR_NOT_FOUND 1
#define ONYX_FILE_ERROR_EXISTS 2
#define ONYX_FILE_ERROR_PERMISSION 3
#define ONYX_FILE_ERROR_BAD_FILE 4
#define ONYX_FILE_ERROR_BAD_MODE 5

ONYX_DEF(__file_open_impl, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    int mode = params->data[2].of.i32;

    bh_file_mode bh_mode;
    switch (mode) {
        case 1: bh_mode = BH_FILE_MODE_READ; break;
        case 2: bh_mode = BH_FILE_MODE_WRITE; break;
        case 3: bh_mode = BH_FILE_MODE_APPEND; break;
    }

    bh_file file;
    bh_file_error error = bh_file_open_mode(&file, bh_mode, path);
    if (error == BH_FILE_ERROR_INVALID) {
        results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NOT_FOUND);
        return NULL;
    }

    *(u64 *) ONYX_PTR(params->data[3].of.i32) = (u64) file.fd;
    results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NONE);
    return NULL;
}

ONYX_DEF(__file_close, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;

    bh_file file = { (bh_file_descriptor) fd };
    bh_file_error error = bh_file_close(&file);
    if (error == BH_FILE_ERROR_INVALID) {
        results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NOT_FOUND);
        return NULL;
    }

    results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NONE);
    return NULL;
}

ONYX_DEF(__file_exists, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    results->data[0] = WASM_I32_VAL(bh_file_exists(path));
    return NULL;
}

ONYX_DEF(__file_stat, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    results->data[0] = WASM_I32_VAL(bh_file_stat(path, ONYX_PTR(params->data[2].of.i32)));
    return NULL;
}

ONYX_DEF(__file_remove, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    results->data[0] = WASM_I32_VAL(bh_file_remove(path));
    return NULL;
}

ONYX_DEF(__file_seek, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    i32 offset = params->data[1].of.i32;
    i32 whence = params->data[2].of.i32;

    bh_file file = { (bh_file_descriptor) fd };
    bh_file_whence bh_whence;
    switch (whence) {
        case 0: bh_whence = BH_FILE_WHENCE_BEGIN; break;
        case 1: bh_whence = BH_FILE_WHENCE_CURRENT; break;
        case 2: bh_whence = BH_FILE_WHENCE_END; break;
    }

    i64 new_offset = bh_file_seek(&file, offset, whence);
    results->data[0] = WASM_I32_VAL((i32) new_offset);
    return NULL;
}

ONYX_DEF(__file_tell, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    results->data[0] = WASM_I32_VAL(bh_file_tell(&file));
    return NULL;
}

ONYX_DEF(__file_read, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };

    i32 curr_pos = bh_file_tell(&file);
    b32 success = bh_file_read_at(&file,
            bh_file_tell(&file),
            ONYX_PTR(params->data[1].of.i32),
            params->data[2].of.i32,
            (i64 *) ONYX_PTR(params->data[3].of.i32));

    bh_file_seek_to(&file, curr_pos + *(i32 *) ONYX_PTR(params->data[3].of.i32));

    results->data[0] = WASM_I32_VAL(0);
    if (!success) results->data[0] = WASM_I32_VAL(2);
    return NULL;
}

ONYX_DEF(__file_write, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };

    i32 curr_pos = bh_file_tell(&file);
    b32 success = bh_file_write_at(&file,
            bh_file_tell(&file),
            ONYX_PTR(params->data[1].of.i32),
            params->data[2].of.i32,
            (i64 *) ONYX_PTR(params->data[3].of.i32));

    bh_file_seek_to(&file, curr_pos + *(i32 *) ONYX_PTR(params->data[3].of.i32));

    results->data[0] = WASM_I32_VAL(0);
    if (!success) results->data[0] = WASM_I32_VAL(2);
    return NULL;
}

ONYX_DEF(__file_flush, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    bh_file_flush(&file);
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
}

ONYX_DEF(__file_size, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    results->data[0] = WASM_I32_VAL(bh_file_size(&file));
    return NULL;
}

ONYX_DEF(__file_get_standard, (WASM_I32, WASM_I32), (WASM_I32)) {
    bh_file_standard standard = (bh_file_standard) params->data[0].of.i32;

    bh_file file;
    bh_file_error error = bh_file_get_standard(&file, standard);
    if (error == BH_FILE_ERROR_NONE) {
        *(u64 *) ONYX_PTR(params->data[1].of.i32) = (u64) file.fd;
    }

    results->data[0] = WASM_I32_VAL(error == BH_FILE_ERROR_NONE);
    return NULL;
}

ONYX_DEF(__file_rename, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *old_path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   old_path_len = params->data[1].of.i32;

    char old_path[512] = {0};
    old_path_len = bh_min(old_path_len, 511);
    strncpy(old_path, old_path_ptr, old_path_len);
    old_path[old_path_len] = 0;

    char *new_path_ptr = ONYX_PTR(params->data[2].of.i32);
    int   new_path_len = params->data[3].of.i32;

    char new_path[512] = {0};
    new_path_len = bh_min(new_path_len, 511);
    strncpy(new_path, new_path_ptr, new_path_len);
    new_path[new_path_len] = 0;

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(MoveFileA(old_path, new_path));
    return NULL;
#endif

#ifdef _BH_LINUX
    results->data[0] = WASM_I32_VAL(rename(old_path, new_path) == 0);
    return NULL;
#endif
}

#ifdef _BH_LINUX
ONYX_DEF(__enable_non_blocking_stdin, (), ()) {
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    flags |= O_NONBLOCK;
    fcntl(STDIN_FILENO, F_SETFL, flags);

    return NULL;
}
#endif
