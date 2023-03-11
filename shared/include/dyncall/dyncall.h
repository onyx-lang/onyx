/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall.h
 Description: public header for library dyncall
 License:

   Copyright (c) 2007-2022 Daniel Adler <dadler@uni-goettingen.de>,
                           Tassilo Philipp <tphilipp@potion-studios.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/


#ifndef DYNCALL_H
#define DYNCALL_H

#include "dyncall_types.h"
#include "dyncall_signature.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct DCCallVM_    DCCallVM;
typedef struct DCaggr_      DCaggr;

/* Supported Calling Convention Modes */

/* default */
#define DC_CALL_C_DEFAULT               0   /* C default (platform native) */
#define DC_CALL_C_DEFAULT_THIS         99   /* for C++ calls where first param is hidden this ptr (platform native) */
#define DC_CALL_C_ELLIPSIS            100   /* to be set for vararg calls' non-hidden (e.g. C++ this ptr), named arguments */
#define DC_CALL_C_ELLIPSIS_VARARGS    101   /* to be set for vararg calls' non-hidden (e.g. C++ this ptr), variable arguments (in ... part) */
/* platform specific */
#define DC_CALL_C_X86_CDECL             1
#define DC_CALL_C_X86_WIN32_STD         2
#define DC_CALL_C_X86_WIN32_FAST_MS     3
#define DC_CALL_C_X86_WIN32_FAST_GNU    4
#define DC_CALL_C_X86_WIN32_THIS_MS     5
#define DC_CALL_C_X86_WIN32_THIS_GNU    DC_CALL_C_X86_CDECL /* alias - identical to cdecl (w/ this-ptr as 1st arg) */
#define DC_CALL_C_X64_WIN64             7
#define DC_CALL_C_X64_WIN64_THIS       70   /* only needed when using aggregate by value as return type */
#define DC_CALL_C_X64_SYSV              8
#define DC_CALL_C_X64_SYSV_THIS         DC_CALL_C_X64_SYSV  /* alias */
#define DC_CALL_C_PPC32_DARWIN          9
#define DC_CALL_C_PPC32_OSX            DC_CALL_C_PPC32_DARWIN /* alias */
#define DC_CALL_C_ARM_ARM_EABI         10
#define DC_CALL_C_ARM_THUMB_EABI       11
#define DC_CALL_C_ARM_ARMHF            30
#define DC_CALL_C_MIPS32_EABI          12
#define DC_CALL_C_MIPS32_PSPSDK        DC_CALL_C_MIPS32_EABI /* alias - deprecated. */
#define DC_CALL_C_PPC32_SYSV           13
#define DC_CALL_C_PPC32_LINUX          DC_CALL_C_PPC32_SYSV /* alias */
#define DC_CALL_C_ARM_ARM              14
#define DC_CALL_C_ARM_THUMB            15
#define DC_CALL_C_MIPS32_O32           16
#define DC_CALL_C_MIPS64_N32           17
#define DC_CALL_C_MIPS64_N64           18
#define DC_CALL_C_X86_PLAN9            19
#define DC_CALL_C_SPARC32              20
#define DC_CALL_C_SPARC64              21
#define DC_CALL_C_ARM64                22
#define DC_CALL_C_PPC64                23
#define DC_CALL_C_PPC64_LINUX          DC_CALL_C_PPC64 /* alias */
/* syscalls, default */
#define DC_CALL_SYS_DEFAULT           200
/* syscalls, platform specific */
#define DC_CALL_SYS_X86_INT80H_LINUX  201
#define DC_CALL_SYS_X86_INT80H_BSD    202
#define DC_CALL_SYS_X64_SYSCALL_SYSV  204
#define DC_CALL_SYS_PPC32             210
#define DC_CALL_SYS_PPC64             211

/* Error codes. */

#define DC_ERROR_NONE                0
#define DC_ERROR_UNSUPPORTED_MODE   -1

DC_API DCCallVM*  dcNewCallVM     (DCsize size);
DC_API void       dcFree          (DCCallVM* vm);
DC_API void       dcReset         (DCCallVM* vm);

DC_API void       dcMode          (DCCallVM* vm, DCint mode);

DC_API void       dcBeginCallAggr (DCCallVM* vm, const DCaggr* ag);

DC_API void       dcArgBool       (DCCallVM* vm, DCbool     value);
DC_API void       dcArgChar       (DCCallVM* vm, DCchar     value);
DC_API void       dcArgShort      (DCCallVM* vm, DCshort    value);
DC_API void       dcArgInt        (DCCallVM* vm, DCint      value);
DC_API void       dcArgLong       (DCCallVM* vm, DClong     value);
DC_API void       dcArgLongLong   (DCCallVM* vm, DClonglong value);
DC_API void       dcArgFloat      (DCCallVM* vm, DCfloat    value);
DC_API void       dcArgDouble     (DCCallVM* vm, DCdouble   value);
DC_API void       dcArgPointer    (DCCallVM* vm, DCpointer  value);
DC_API void       dcArgAggr       (DCCallVM* vm, const DCaggr* ag, const void* value);

DC_API void       dcCallVoid      (DCCallVM* vm, DCpointer funcptr);
DC_API DCbool     dcCallBool      (DCCallVM* vm, DCpointer funcptr);
DC_API DCchar     dcCallChar      (DCCallVM* vm, DCpointer funcptr);
DC_API DCshort    dcCallShort     (DCCallVM* vm, DCpointer funcptr);
DC_API DCint      dcCallInt       (DCCallVM* vm, DCpointer funcptr);
DC_API DClong     dcCallLong      (DCCallVM* vm, DCpointer funcptr);
DC_API DClonglong dcCallLongLong  (DCCallVM* vm, DCpointer funcptr);
DC_API DCfloat    dcCallFloat     (DCCallVM* vm, DCpointer funcptr);
DC_API DCdouble   dcCallDouble    (DCCallVM* vm, DCpointer funcptr);
DC_API DCpointer  dcCallPointer   (DCCallVM* vm, DCpointer funcptr);
DC_API DCpointer  dcCallAggr      (DCCallVM* vm, DCpointer funcptr, const DCaggr* ag, DCpointer ret); /* retval is written to *ret, returns ret */

DC_API DCint      dcGetError      (DCCallVM* vm);

DC_API DCaggr*    dcNewAggr       (DCsize maxFieldCount, DCsize size);
DC_API void       dcFreeAggr      (DCaggr* ag);
/* if type == DC_SIGCHAR_AGGREGATE, pass DCaggr* of nested struct/union in ...  */
DC_API void       dcAggrField     (DCaggr* ag, DCsigchar type, DCint offset, DCsize array_len, ...);
DC_API void       dcCloseAggr     (DCaggr* ag);   /* to indicate end of struct definition, required */


/* helpers */

/* returns respective mode for callconv sig char (w/o checking if mode exists */
/* on current platform), or DC_ERROR_UNSUPPORTED_MODE if char isn't a sigchar */
DC_API DCint      dcGetModeFromCCSigChar(DCsigchar sig_char);

#ifdef __cplusplus
}
#endif

#endif /* DYNCALL_H */

