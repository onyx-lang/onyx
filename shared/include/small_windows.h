// Bill's Mini Windows.h from https://github.com/odin-lang/Odin/blob/master/src/gb/gb.h

////////////////////////////////////////////////////////////////
//
// Bill's Mini Windows.h
//
//

#define GB_EXTERN extern
#define GB_DLL_EXPORT GB_EXTERN __declspec(dllexport)
#define GB_DLL_IMPORT GB_EXTERN __declspec(dllimport)

#define WINAPI   __stdcall
#define WINAPIV  __cdecl
#define CALLBACK __stdcall
#define MAX_PATH 260
#define CCHDEVICENAME 32
#define CCHFORMNAME   32

typedef unsigned long DWORD;
typedef int WINBOOL;
#ifndef XFree86Server
    #ifndef __OBJC__
    typedef WINBOOL BOOL;
    #else
    #define BOOL WINBOOL
    #endif
typedef unsigned char BYTE;
#endif
typedef unsigned short WORD;
typedef float FLOAT;
typedef int INT;
typedef unsigned int UINT;
typedef short SHORT;
typedef long LONG;
typedef long long LONGLONG;
typedef unsigned short USHORT;
typedef unsigned long ULONG;
typedef unsigned long long ULONGLONG;

typedef UINT WPARAM;
typedef LONG LPARAM;
typedef LONG LRESULT;
#ifndef _HRESULT_DEFINED
typedef LONG HRESULT;
#define _HRESULT_DEFINED
#endif
#ifndef XFree86Server
typedef WORD ATOM;
#endif /* XFree86Server */
typedef void *HANDLE;
typedef HANDLE HGLOBAL;
typedef HANDLE HLOCAL;
typedef HANDLE GLOBALHANDLE;
typedef HANDLE LOCALHANDLE;
typedef HANDLE BCRYPT_ALG_HANDLE;
typedef void *HGDIOBJ;

#define DECLARE_HANDLE(name) typedef HANDLE name
DECLARE_HANDLE(HACCEL);
DECLARE_HANDLE(HBITMAP);
DECLARE_HANDLE(HBRUSH);
DECLARE_HANDLE(HCOLORSPACE);
DECLARE_HANDLE(HDC);
DECLARE_HANDLE(HGLRC);
DECLARE_HANDLE(HDESK);
DECLARE_HANDLE(HENHMETAFILE);
DECLARE_HANDLE(HFONT);
DECLARE_HANDLE(HICON);
DECLARE_HANDLE(HKEY);
typedef HKEY *PHKEY;
DECLARE_HANDLE(HMENU);
DECLARE_HANDLE(HMETAFILE);
DECLARE_HANDLE(HINSTANCE);
typedef HINSTANCE HMODULE;
DECLARE_HANDLE(HPALETTE);
DECLARE_HANDLE(HPEN);
DECLARE_HANDLE(HRGN);
DECLARE_HANDLE(HRSRC);
DECLARE_HANDLE(HSTR);
DECLARE_HANDLE(HTASK);
DECLARE_HANDLE(HWND);
DECLARE_HANDLE(HWINSTA);
DECLARE_HANDLE(HKL);
DECLARE_HANDLE(HRAWINPUT);
DECLARE_HANDLE(HMONITOR);
#undef DECLARE_HANDLE

typedef int HFILE;
typedef HICON HCURSOR;
typedef DWORD COLORREF;
typedef int (WINAPI *FARPROC)();
typedef int (WINAPI *NEARPROC)();
typedef int (WINAPI *PROC)();
typedef LRESULT (CALLBACK *WNDPROC)(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);

#if defined(_WIN64)
typedef unsigned __int64 ULONG_PTR;
typedef signed __int64 LONG_PTR;
#else
typedef unsigned long ULONG_PTR;
typedef signed long LONG_PTR;
#endif
typedef ULONG_PTR DWORD_PTR;

typedef struct tagRECT {
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT;
typedef struct tagRECTL {
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECTL;
typedef struct tagPOINT {
    LONG x;
    LONG y;
} POINT;
typedef struct tagSIZE {
    LONG cx;
    LONG cy;
} SIZE;
typedef struct tagPOINTS {
    SHORT x;
    SHORT y;
} POINTS;
typedef struct _SECURITY_ATTRIBUTES {
    DWORD  nLength;
    HANDLE lpSecurityDescriptor;
    BOOL   bInheritHandle;
} SECURITY_ATTRIBUTES;
typedef enum _LOGICAL_PROCESSOR_RELATIONSHIP {
    RelationProcessorCore,
    RelationNumaNode,
    RelationCache,
    RelationProcessorPackage,
    RelationGroup,
    RelationAll               = 0xffff
} LOGICAL_PROCESSOR_RELATIONSHIP;
typedef enum _PROCESSOR_CACHE_TYPE {
    CacheUnified,
    CacheInstruction,
    CacheData,
    CacheTrace
} PROCESSOR_CACHE_TYPE;
typedef struct _CACHE_DESCRIPTOR {
    BYTE                 Level;
    BYTE                 Associativity;
    WORD                 LineSize;
    DWORD                Size;
    PROCESSOR_CACHE_TYPE Type;
} CACHE_DESCRIPTOR;
typedef struct _SYSTEM_LOGICAL_PROCESSOR_INFORMATION {
    ULONG_PTR                       ProcessorMask;
    LOGICAL_PROCESSOR_RELATIONSHIP Relationship;
    union {
        struct {
            BYTE Flags;
        } ProcessorCore;
        struct {
            DWORD NodeNumber;
        } NumaNode;
        CACHE_DESCRIPTOR Cache;
        ULONGLONG        Reserved[2];
    };
} SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
typedef struct _MEMORY_BASIC_INFORMATION {
    void *BaseAddress;
    void *AllocationBase;
    DWORD AllocationProtect;
    size_t RegionSize;
    DWORD State;
    DWORD Protect;
    DWORD Type;
} MEMORY_BASIC_INFORMATION;
typedef struct _SYSTEM_INFO {
    union {
        DWORD   dwOemId;
        struct {
            WORD wProcessorArchitecture;
            WORD wReserved;
        };
    };
    DWORD     dwPageSize;
    void *    lpMinimumApplicationAddress;
    void *    lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD     dwNumberOfProcessors;
    DWORD     dwProcessorType;
    DWORD     dwAllocationGranularity;
    WORD      wProcessorLevel;
    WORD      wProcessorRevision;
} SYSTEM_INFO;
typedef union _LARGE_INTEGER {
    struct {
        DWORD LowPart;
        LONG  HighPart;
    };
    struct {
        DWORD LowPart;
        LONG  HighPart;
    } u;
    LONGLONG QuadPart;
} LARGE_INTEGER;
typedef union _ULARGE_INTEGER {
    struct {
        DWORD LowPart;
        DWORD HighPart;
    };
    struct {
        DWORD LowPart;
        DWORD HighPart;
    } u;
    ULONGLONG QuadPart;
} ULARGE_INTEGER;

typedef struct _OVERLAPPED {
    ULONG_PTR Internal;
    ULONG_PTR InternalHigh;
    union {
        struct {
            DWORD Offset;
            DWORD OffsetHigh;
        };
        void *Pointer;
    };
    HANDLE hEvent;
} OVERLAPPED;
typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME;
typedef struct _WIN32_FIND_DATAW {
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
    DWORD    dwReserved0;
    DWORD    dwReserved1;
    wchar_t  cFileName[MAX_PATH];
    wchar_t  cAlternateFileName[14];
} WIN32_FIND_DATAW;
typedef struct _WIN32_FIND_DATAA {
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
    DWORD    dwReserved0;
    DWORD    dwReserved1;
    char  cFileName[MAX_PATH];
    char  cAlternateFileName[14];
} WIN32_FIND_DATAA;
typedef struct _WIN32_FILE_ATTRIBUTE_DATA {
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
} WIN32_FILE_ATTRIBUTE_DATA;
typedef enum _GET_FILEEX_INFO_LEVELS {
    GetFileExInfoStandard,
    GetFileExMaxInfoLevel
} GET_FILEEX_INFO_LEVELS;
typedef struct _STARTUPINFOA {
  DWORD  cb;
  char * lpReserved;
  char * lpDesktop;
  char * lpTitle;
  DWORD  dwX;
  DWORD  dwY;
  DWORD  dwXSize;
  DWORD  dwYSize;
  DWORD  dwXCountChars;
  DWORD  dwYCountChars;
  DWORD  dwFillAttribute;
  DWORD  dwFlags;
  WORD   wShowWindow;
  WORD   cbReserved2;
  char * lpReserved2;
  HANDLE hStdInput;
  HANDLE hStdOutput;
  HANDLE hStdError;
} STARTUPINFOA, *LPSTARTUPINFOA;
typedef struct _PROCESS_INFORMATION {
  HANDLE hProcess;
  HANDLE hThread;
  DWORD  dwProcessId;
  DWORD  dwThreadId;
} PROCESS_INFORMATION, *PPROCESS_INFORMATION, *LPPROCESS_INFORMATION;

#define INFINITE 0xffffffffl
#define INVALID_HANDLE_VALUE ((void *)(intptr_t)(-1))
#define STARTF_USESTDHANDLES 0x00000100

typedef DWORD WINAPI THREAD_START_ROUTINE(void *parameter);

GB_DLL_IMPORT DWORD   WINAPI GetLastError       (void);
GB_DLL_IMPORT BOOL    WINAPI CloseHandle        (HANDLE object);
GB_DLL_IMPORT HANDLE  WINAPI CreateSemaphoreA   (SECURITY_ATTRIBUTES *semaphore_attributes, LONG initial_count,
                                                 LONG maximum_count, char const *name);
GB_DLL_IMPORT BOOL    WINAPI ReleaseSemaphore   (HANDLE semaphore, LONG release_count, LONG *previous_count);
GB_DLL_IMPORT DWORD   WINAPI WaitForSingleObject(HANDLE handle, DWORD milliseconds);
GB_DLL_IMPORT HANDLE  WINAPI CreateThread       (SECURITY_ATTRIBUTES *semaphore_attributes, size_t stack_size,
                                                 THREAD_START_ROUTINE *start_address, void *parameter,
                                                 DWORD creation_flags, DWORD *thread_id);
GB_DLL_IMPORT DWORD   WINAPI GetThreadId        (HANDLE handle);
GB_DLL_IMPORT void    WINAPI RaiseException     (DWORD, DWORD, DWORD, ULONG_PTR const *);
GB_DLL_IMPORT BOOL    WINAPI TerminateThread    (HANDLE hThread, DWORD dwExitCode);
GB_DLL_IMPORT BOOL    WINAPI CreateProcessA     (char const * lpApplicationName, char * lpCommandLine,
                                                 SECURITY_ATTRIBUTES* lpProcessAttrs, SECURITY_ATTRIBUTES* lpThreadAttributes,
                                                 BOOL bInheritHandles, DWORD dwCreationFlags, void* lpEnvironment,
                                                 char const * lpCurrentDirectory, LPSTARTUPINFOA lpStartupInfo,
                                                 LPPROCESS_INFORMATION lpProcessInformation);
GB_DLL_IMPORT BOOL    WINAPI GetExitCodeProcess (HANDLE hProcess, DWORD *lpExitCode);
GB_DLL_IMPORT BOOL    WINAPI CreatePipe         (HANDLE *hReadPipe, HANDLE *hWritePipe, SECURITY_ATTRIBUTES* lpPipeAttributes,
                                                 DWORD nSize);
GB_DLL_IMPORT BOOL    WINAPI TerminateProcess   (HANDLE hProcess, UINT uExitCode);
GB_DLL_IMPORT BOOL    WINAPI SetHandleInformation(HANDLE hObject, DWORD dwMask, DWORD dwFlags);

uintptr_t _beginthreadex( // NATIVE CODE
   void *security,
   unsigned stack_size,
   unsigned ( __stdcall *start_address )( void * ),
   void *arglist,
   unsigned initflag,
   unsigned *thrdaddr
);


GB_DLL_IMPORT BOOL      WINAPI GetLogicalProcessorInformation(SYSTEM_LOGICAL_PROCESSOR_INFORMATION *buffer, DWORD *return_length);
GB_DLL_IMPORT DWORD_PTR WINAPI SetThreadAffinityMask(HANDLE thread, DWORD_PTR check_mask);
GB_DLL_IMPORT HANDLE    WINAPI GetCurrentThread(void);

#define PAGE_NOACCESS          0x01
#define PAGE_READONLY          0x02
#define PAGE_READWRITE         0x04
#define PAGE_WRITECOPY         0x08
#define PAGE_EXECUTE           0x10
#define PAGE_EXECUTE_READ      0x20
#define PAGE_EXECUTE_READWRITE 0x40
#define PAGE_EXECUTE_WRITECOPY 0x80
#define PAGE_GUARD            0x100
#define PAGE_NOCACHE          0x200
#define PAGE_WRITECOMBINE     0x400

#define MEM_COMMIT           0x1000
#define MEM_RESERVE          0x2000
#define MEM_DECOMMIT         0x4000
#define MEM_RELEASE          0x8000
#define MEM_FREE            0x10000
#define MEM_PRIVATE         0x20000
#define MEM_MAPPED          0x40000
#define MEM_RESET           0x80000
#define MEM_TOP_DOWN       0x100000
#define MEM_LARGE_PAGES  0x20000000
#define MEM_4MB_PAGES    0x80000000




GB_DLL_IMPORT void * WINAPI VirtualAlloc (void *addr, size_t size, DWORD allocation_type, DWORD protect);
GB_DLL_IMPORT size_t  WINAPI VirtualQuery (void const *address, MEMORY_BASIC_INFORMATION *buffer, size_t length);
GB_DLL_IMPORT BOOL   WINAPI VirtualFree  (void *address, size_t size, DWORD free_type);
GB_DLL_IMPORT void   WINAPI GetSystemInfo(SYSTEM_INFO *system_info);


#define GENERIC_READ             0x80000000
#define GENERIC_WRITE            0x40000000
#define GENERIC_EXECUTE          0x20000000
#define GENERIC_ALL              0x10000000
#define FILE_SHARE_READ          0x00000001
#define FILE_SHARE_WRITE         0x00000002
#define FILE_SHARE_DELETE        0x00000004
#define CREATE_NEW               1
#define CREATE_ALWAYS            2
#define OPEN_EXISTING            3
#define OPEN_ALWAYS              4
#define TRUNCATE_EXISTING        5
#define FILE_ATTRIBUTE_READONLY  0x00000001
#define FILE_ATTRIBUTE_NORMAL    0x00000080
#define FILE_ATTRIBUTE_TEMPORARY 0x00000100
#define FILE_ATTRIBUTE_DIRECTORY 0x00000010
#define ERROR_FILE_NOT_FOUND     2l
#define ERROR_ACCESS_DENIED      5L
#define ERROR_NO_MORE_FILES      18l
#define ERROR_FILE_EXISTS        80l
#define ERROR_ALREADY_EXISTS     183l
#define STD_INPUT_HANDLE         ((DWORD)-10)
#define STD_OUTPUT_HANDLE        ((DWORD)-11)
#define STD_ERROR_HANDLE         ((DWORD)-12)

GB_DLL_IMPORT int           MultiByteToWideChar(UINT code_page, DWORD flags, char const *   multi_byte_str, int multi_byte_len, wchar_t const *wide_char_str,  int wide_char_len);
GB_DLL_IMPORT int           WideCharToMultiByte(UINT code_page, DWORD flags, wchar_t const *wide_char_str,  int wide_char_len, char const *    multi_byte_str, int multi_byte_len);
GB_DLL_IMPORT BOOL   WINAPI SetFilePointerEx(HANDLE file, LARGE_INTEGER distance_to_move,
                                             LARGE_INTEGER *new_file_pointer, DWORD move_method);
GB_DLL_IMPORT BOOL   WINAPI ReadFile        (HANDLE file, void *buffer, DWORD bytes_to_read, DWORD *bytes_read, OVERLAPPED *overlapped);
GB_DLL_IMPORT BOOL   WINAPI WriteFile       (HANDLE file, void const *buffer, DWORD bytes_to_write, DWORD *bytes_written, OVERLAPPED *overlapped);
GB_DLL_IMPORT HANDLE WINAPI CreateFileW     (wchar_t const *path, DWORD desired_access, DWORD share_mode,
                                             SECURITY_ATTRIBUTES *, DWORD creation_disposition,
                                             DWORD flags_and_attributes, HANDLE template_file);
GB_DLL_IMPORT HANDLE WINAPI CreateFileA     (char const *path, DWORD desired_access, DWORD share_mode,
                                             SECURITY_ATTRIBUTES *, DWORD creation_disposition,
                                             DWORD flags_and_attributes, HANDLE template_file);
GB_DLL_IMPORT HANDLE WINAPI GetStdHandle    (DWORD std_handle);
GB_DLL_IMPORT BOOL   WINAPI GetFileSizeEx   (HANDLE file, LARGE_INTEGER *size);
GB_DLL_IMPORT BOOL   WINAPI SetEndOfFile    (HANDLE file);
GB_DLL_IMPORT HANDLE WINAPI FindFirstFileW  (wchar_t const *path, WIN32_FIND_DATAW *data);
GB_DLL_IMPORT HANDLE WINAPI FindFirstFileA  (char const *path, WIN32_FIND_DATAA *data);
GB_DLL_IMPORT BOOL   WINAPI FindNextFileA   (HANDLE find_find, WIN32_FIND_DATAA *data);
GB_DLL_IMPORT BOOL   WINAPI FindClose       (HANDLE find_file);
GB_DLL_IMPORT BOOL   WINAPI GetFileAttributesExW(wchar_t const *path, GET_FILEEX_INFO_LEVELS info_level_id, WIN32_FILE_ATTRIBUTE_DATA *data);
GB_DLL_IMPORT BOOL   WINAPI CopyFileW(wchar_t const *old_f, wchar_t const *new_f, BOOL fail_if_exists);
GB_DLL_IMPORT BOOL   WINAPI MoveFileW(wchar_t const *old_f, wchar_t const *new_f);
GB_DLL_IMPORT BOOL   WINAPI DeleteFileA     (char const *path);
GB_DLL_IMPORT BOOL   WINAPI CreateDirectoryA(char const *path, SECURITY_ATTRIBUTES *lpSecurityAttributes);
GB_DLL_IMPORT BOOL   WINAPI RemoveDirectoryA(char const *path);
GB_DLL_IMPORT BOOL   WINAPI MoveFileA       (char const *old_path, char const *new_path);

GB_DLL_IMPORT DWORD  WINAPI GetFullPathNameA(char const *lpFileName, DWORD nBufferLength, char *lpBuffer, char **lpFilePart);

GB_DLL_IMPORT HMODULE WINAPI LoadLibraryA  (char const *filename);
GB_DLL_IMPORT BOOL    WINAPI FreeLibrary   (HMODULE module);
GB_DLL_IMPORT FARPROC WINAPI GetProcAddress(HMODULE module, char const *name);

GB_DLL_IMPORT BOOL WINAPI QueryPerformanceFrequency(LARGE_INTEGER *frequency);
GB_DLL_IMPORT BOOL WINAPI QueryPerformanceCounter  (LARGE_INTEGER *counter);
GB_DLL_IMPORT void WINAPI GetSystemTimeAsFileTime  (FILETIME *system_time_as_file_time);
GB_DLL_IMPORT void WINAPI Sleep(DWORD milliseconds);
GB_DLL_IMPORT void WINAPI ExitProcess(UINT exit_code);

GB_DLL_IMPORT BOOL  WINAPI SetEnvironmentVariableA(char const *name, char const *value);
GB_DLL_IMPORT DWORD WINAPI GetEnvironmentVariableA(char const * lpName, char * lpBuffer, DWORD nSize);

GB_DLL_IMPORT short WINAPI htons(short hostshort);
GB_DLL_IMPORT int   WINAPI htonl(int   hostint);
GB_DLL_IMPORT short WINAPI ntohs(short netshort);
GB_DLL_IMPORT int   WINAPI ntohl(int   netint);


GB_DLL_IMPORT int BCryptGenRandom(
    BCRYPT_ALG_HANDLE hAlgorithm,
    unsigned char *   pbBuffer,
    unsigned long     cbBuffer,
    unsigned long     wFlags
);

GB_DLL_IMPORT int BCryptOpenAlgorithmProvider(
    BCRYPT_ALG_HANDLE *phAlgorithm,
    const wchar_t *    pszAlgId,
    const wchar_t *    pszImplementation,
    unsigned long      dwFlags
);

GB_DLL_IMPORT int BCryptCloseAlgorithmProvider(
    BCRYPT_ALG_HANDLE hAlgorithm,
    unsigned long     dwFlags
);

GB_DLL_IMPORT void WINAPI WakeByAddressSingle(void * Address);
GB_DLL_IMPORT BOOL WINAPI WaitOnAddress(volatile void * Address, void * compareAddress, size_t addressSize, DWORD milliseconds);

GB_DLL_IMPORT DWORD GetCurrentDirectoryA(DWORD nBufferLength, char *lpBuffer);
GB_DLL_IMPORT BOOL  SetCurrentDirectoryA(char *lpPathName);

GB_DLL_IMPORT DWORD GetFileAttributesA(char *lpPathName);
GB_DLL_IMPORT BOOL  SetFileAttributesA(char *lpPathName, DWORD attrs);
