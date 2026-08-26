
//
// Files
//

#define ONYX_FILE_ERROR_NONE 0
#define ONYX_FILE_ERROR_NOT_FOUND 1
#define ONYX_FILE_ERROR_EXISTS 2
#define ONYX_FILE_ERROR_PERMISSION 3
#define ONYX_FILE_ERROR_BAD_FILE 4
#define ONYX_FILE_ERROR_BAD_MODE 5

#define ONYX_OPEN_EXCLUSIVE 0x01
#define ONYX_OPEN_CREATE    0x02

static void onyx_copy_path512(char *dst, int ptr, int len) {
    char *src = ONYX_PTR(ptr);
    if (len > 511) len = 511;
    memcpy(dst, src, len);
    dst[len] = 0;
}

static i32 onyx_map_file_error(void) {
#ifdef _BH_WINDOWS
    switch (GetLastError()) {
        case ERROR_FILE_NOT_FOUND:
        case ERROR_PATH_NOT_FOUND:
        case ERROR_INVALID_NAME:
            return ONYX_FILE_ERROR_NOT_FOUND;
        case ERROR_ALREADY_EXISTS:
        case ERROR_FILE_EXISTS:
            return ONYX_FILE_ERROR_EXISTS;
        case ERROR_ACCESS_DENIED:
        case ERROR_SHARING_VIOLATION:
            return ONYX_FILE_ERROR_PERMISSION;
        default:
            return ONYX_FILE_ERROR_BAD_FILE;
    }
#else
    switch (errno) {
        case ENOENT:
        case ENOTDIR:
            return ONYX_FILE_ERROR_NOT_FOUND;
        case EEXIST:
            return ONYX_FILE_ERROR_EXISTS;
        case EACCES:
        case EPERM:
            return ONYX_FILE_ERROR_PERMISSION;
        case EISDIR:
            return ONYX_FILE_ERROR_BAD_FILE;
        case EINVAL:
            return ONYX_FILE_ERROR_BAD_MODE;
        default:
            return ONYX_FILE_ERROR_BAD_FILE;
    }
#endif
}

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static void onyx_fill_stat(struct stat *s, bh_file_stats *out) {
    out->size = s->st_size;
#if defined(_BH_DARWIN)
    out->modified_time = (u64) s->st_mtimespec.tv_sec * 1000 + s->st_mtimespec.tv_nsec / 1000000;
    out->accessed_time = (u64) s->st_atimespec.tv_sec * 1000 + s->st_atimespec.tv_nsec / 1000000;
    out->change_time   = (u64) s->st_ctimespec.tv_sec * 1000 + s->st_ctimespec.tv_nsec / 1000000;
#else
    out->modified_time = (u64) s->st_mtim.tv_sec * 1000 + s->st_mtim.tv_nsec / 1000000;
    out->accessed_time = (u64) s->st_atim.tv_sec * 1000 + s->st_atim.tv_nsec / 1000000;
    out->change_time   = (u64) s->st_ctim.tv_sec * 1000 + s->st_ctim.tv_nsec / 1000000;
#endif
    out->file_type = BH_FILE_TYPE_FILE;
    if ((s->st_mode & S_IFMT) == S_IFDIR) out->file_type = BH_FILE_TYPE_DIRECTORY;
    if ((s->st_mode & S_IFMT) == S_IFLNK) out->file_type = BH_FILE_TYPE_LINK;
}
#endif

ONYX_DEF(__file_open_impl, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char path[512] = {0};
    onyx_copy_path512(path, params->data[0].of.i32, params->data[1].of.i32);

    int mode  = params->data[2].of.i32;
    int flags = params->data[3].of.i32;

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    int oflags = 0;
    switch (mode) {
        case 1: oflags = O_RDONLY; break;
        case 2: oflags = O_WRONLY | O_CREAT | O_TRUNC; break;
        case 3: oflags = O_WRONLY | O_APPEND | O_CREAT; break;
        case 4: oflags = O_RDWR; break;
        default:
            results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_BAD_MODE);
            return NULL;
    }

    if (flags & ONYX_OPEN_CREATE) oflags |= O_CREAT;
    if ((flags & ONYX_OPEN_EXCLUSIVE) && (oflags & O_CREAT)) oflags |= O_EXCL;

    int fd = open(path, oflags, 0666);
    if (fd < 0) {
        results->data[0] = WASM_I32_VAL(onyx_map_file_error());
        return NULL;
    }

    *(u64 *) ONYX_PTR(params->data[4].of.i32) = (u64) fd;
    results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NONE);
    return NULL;
#endif

#ifdef _BH_WINDOWS
    DWORD desired_access = 0;
    DWORD creation = OPEN_EXISTING;
    switch (mode) {
        case 1:
            desired_access = GENERIC_READ;
            creation = OPEN_EXISTING;
            break;
        case 2:
            desired_access = GENERIC_WRITE;
            creation = (flags & ONYX_OPEN_EXCLUSIVE) ? CREATE_NEW : CREATE_ALWAYS;
            break;
        case 3:
            desired_access = FILE_APPEND_DATA | SYNCHRONIZE;
            creation = OPEN_ALWAYS;
            break;
        case 4:
            desired_access = GENERIC_READ | GENERIC_WRITE;
            if (flags & ONYX_OPEN_EXCLUSIVE) creation = CREATE_NEW;
            else if (flags & ONYX_OPEN_CREATE) creation = OPEN_ALWAYS;
            else creation = OPEN_EXISTING;
            break;
        default:
            results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_BAD_MODE);
            return NULL;
    }

    HANDLE h = CreateFileA(path, desired_access, FILE_SHARE_READ, NULL,
                           creation, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) {
        results->data[0] = WASM_I32_VAL(onyx_map_file_error());
        return NULL;
    }

    *(u64 *) ONYX_PTR(params->data[4].of.i32) = (u64) h;
    results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NONE);
    return NULL;
#endif
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

    #if _BH_WINDOWS
        DWORD attrs = GetFileAttributesA(path);
        attrs &= ~FILE_ATTRIBUTE_READONLY;
        SetFileAttributesA(path, attrs);
    #endif

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

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    results->data[0] = WASM_I32_VAL(rename(old_path, new_path) == 0);
    return NULL;
#endif
}

ONYX_DEF(__file_lstat, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char path[512] = {0};
    onyx_copy_path512(path, params->data[0].of.i32, params->data[1].of.i32);

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct stat s;
    if (lstat(path, &s) == -1) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }
    onyx_fill_stat(&s, ONYX_PTR(params->data[2].of.i32));
    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(bh_file_stat(path, ONYX_PTR(params->data[2].of.i32)));
    return NULL;
#endif
}

ONYX_DEF(__file_symlink, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char target[512] = {0};
    char linkpath[512] = {0};
    onyx_copy_path512(target, params->data[0].of.i32, params->data[1].of.i32);
    onyx_copy_path512(linkpath, params->data[2].of.i32, params->data[3].of.i32);

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    results->data[0] = WASM_I32_VAL(symlink(target, linkpath) == 0);
    return NULL;
#endif

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(CreateSymbolicLinkA(linkpath, target, 0) != 0);
    return NULL;
#endif
}

ONYX_DEF(__file_link, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char target[512] = {0};
    char linkpath[512] = {0};
    onyx_copy_path512(target, params->data[0].of.i32, params->data[1].of.i32);
    onyx_copy_path512(linkpath, params->data[2].of.i32, params->data[3].of.i32);

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    results->data[0] = WASM_I32_VAL(link(target, linkpath) == 0);
    return NULL;
#endif

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(CreateHardLinkA(linkpath, target, NULL) != 0);
    return NULL;
#endif
}

ONYX_DEF(__file_readlink, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char path[512] = {0};
    onyx_copy_path512(path, params->data[0].of.i32, params->data[1].of.i32);

    char *out = ONYX_PTR(params->data[2].of.i32);
    int out_len = params->data[3].of.i32;

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    isize n = readlink(path, out, out_len);
    results->data[0] = WASM_I32_VAL(n < 0 ? -1 : (i32) n);
    return NULL;
#endif

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(-1);
    return NULL;
#endif
}

ONYX_DEF(__file_truncate, (WASM_I64, WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    i64 size = params->data[1].of.i64;

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    results->data[0] = WASM_I32_VAL(ftruncate((int) fd, (off_t) size) == 0);
    return NULL;
#endif

#ifdef _BH_WINDOWS
    HANDLE h = (HANDLE) fd;
    LARGE_INTEGER dist;
    dist.QuadPart = size;
    if (!SetFilePointerEx(h, dist, NULL, FILE_BEGIN) || !SetEndOfFile(h)) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }
    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif
}

