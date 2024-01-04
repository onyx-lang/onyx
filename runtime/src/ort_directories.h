//
// Directories
//

ONYX_DEF(__dir_open, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

#ifdef _BH_WINDOWS
    for (int i=0; i<path_len; i++) if (path[i] == '/') path[i] = '\\';
    strncat(path, "\\*.*", 511);

    Windows_Directory_Opened* dir = malloc(sizeof(Windows_Directory_Opened));
    dir->hndl = FindFirstFileA(path, &dir->found_file);
    if (dir->hndl == INVALID_HANDLE_VALUE) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    *(u64 *) ONYX_PTR(params->data[2].of.i32) = (u64) dir;

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    DIR* dir = opendir(path);
    *(u64 *) ONYX_PTR(params->data[2].of.i32) = (u64) dir;
    results->data[0] = WASM_I32_VAL(dir != NULL);
    return NULL;
#endif
}

// (DIR*, PTR<DIRENT>) -> BOOL
ONYX_DEF(__dir_read, (WASM_I64, WASM_I32), (WASM_I32)) {
#ifdef _BH_WINDOWS
    Windows_Directory_Opened* dir = (Windows_Directory_Opened *) params->data[0].of.i64;
    if (dir == NULL) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    do {
        BOOL success = FindNextFileA(dir->hndl, &dir->found_file);
        if (!success) {
            results->data[0] = WASM_I32_VAL(0);
            return NULL;
        }
    } while (!strcmp(dir->found_file.cFileName, ".") || !strcmp(dir->found_file.cFileName, ".."));

    u32 out = params->data[1].of.i32;
    assert(out != 0);

    *(u32 *) ONYX_PTR(out + 0) = (dir->found_file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? 3 : 4;
    *(u32 *) ONYX_PTR(out + 4) = 0;
    *(u32 *) ONYX_PTR(out + 8) = strlen(dir->found_file.cFileName);
    strncpy(ONYX_PTR(out + 12), dir->found_file.cFileName, 256);

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    DIR* dir = (DIR *) params->data[0].of.i64;
    if (dir == NULL) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    struct dirent *ent;
    while (1) {
        ent = readdir(dir);
        if (ent == NULL) {
            results->data[0] = WASM_I32_VAL(0);
            return NULL;
        }

        // Skip the current directory and parent directory
        if (strcmp(ent->d_name, ".") && strcmp(ent->d_name, "..")) break;
    }

    u32 type = 0;
    switch (ent->d_type) {
        case DT_UNKNOWN: break;
        case DT_BLK: type = 1; break;
        case DT_CHR: type = 2; break;
        case DT_DIR: type = 3; break;
        case DT_LNK: type = 5; break;
        case DT_REG: type = 4; break;
        default: type = 6; break;
    }

    u32 out = params->data[1].of.i32;
    assert(out != 0);

    *(u32 *) ONYX_PTR(out + 0) = type;
    *(u32 *) ONYX_PTR(out + 4) = (u32) ent->d_ino;
    *(u32 *) ONYX_PTR(out + 8) = strlen(ent->d_name);
    strncpy(ONYX_PTR(out + 12), ent->d_name, 256);

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif
}

ONYX_DEF(__dir_close, (WASM_I64), ()) {
#ifdef _BH_WINDOWS
    Windows_Directory_Opened* dir = (Windows_Directory_Opened *) params->data[0].of.i64;
    if (dir == NULL) return NULL;

    FindClose(dir->hndl);
    free(dir);

    return NULL;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    DIR* dir = (DIR *) params->data[0].of.i64;
    if (dir == NULL) return NULL;

    closedir(dir);
#endif
    return NULL;
}

ONYX_DEF(__dir_create, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(CreateDirectoryA(path, NULL));
    return NULL;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    results->data[0] = WASM_I32_VAL(mkdir(path, 0777) == 0);
    return NULL;
#endif
}

ONYX_DEF(__dir_remove, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(RemoveDirectoryA(path));
    return NULL;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    results->data[0] = WASM_I32_VAL(rmdir(path) == 0);
    return NULL;
#endif
}

ONYX_DEF(__dir_cwd, (WASM_I32, WASM_I32), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    char *dir = getcwd(ONYX_PTR(params->data[0].of.i32), params->data[1].of.i32);
    if (!dir) {
        results->data[0] = WASM_I32_VAL(-1);
        return NULL;
    }

    results->data[0] = WASM_I32_VAL( strlen(dir) );
    return NULL;
#endif

#if defined(_BH_WINDOWS)
    int length = GetCurrentDirectoryA(params->data[1].of.i32, ONYX_PTR(params->data[0].of.i32));
    if (length == 0 || length > params->data[1].of.i32) {
        results->data[0] = WASM_I32_VAL(-1);
        return NULL;
    }

    results->data[0] = WASM_I32_VAL(length);
    return NULL;
#endif
}

ONYX_DEF(__dir_chdir, (WASM_I32), (WASM_I32)) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int result = chdir(ONYX_PTR(params->data[0].of.i32));
    results->data[0] = WASM_I32_VAL(result ? 0 : 1);
    return NULL;
#endif

#if defined(_BH_WINDOWS)
    int result = SetCurrentDirectoryA(ONYX_PTR(params->data[0].of.i32));
    results->data[0] = WASM_I32_VAL(result ? 1 : 0);
    return NULL;
#endif
}

