/*

 Package: dyncall
 Library: dyncallback
 File: dyncallback/dyncall_args.h
 Description: Callback's Arguments VM - Interface
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


#ifndef DYNCALL_ARGS_H
#define DYNCALL_ARGS_H

/*
 * dyncall args C API
 *
 * dyncall args provides serialized access to arguments of a function call.
 * related concepts: callback
 *
 */

#include "dyncall.h"

#include "dyncall_value.h"


#ifdef __cplusplus
extern "C" {
#endif

typedef struct DCArgs DCArgs;

/* functions to retrieve callback's params in callback handler */
DC_API DCbool      dcbArgBool     (DCArgs* p);
DC_API DCchar      dcbArgChar     (DCArgs* p);
DC_API DCshort     dcbArgShort    (DCArgs* p);
DC_API DCint       dcbArgInt      (DCArgs* p);
DC_API DClong      dcbArgLong     (DCArgs* p);
DC_API DClonglong  dcbArgLongLong (DCArgs* p);
DC_API DCuchar     dcbArgUChar    (DCArgs* p);
DC_API DCushort    dcbArgUShort   (DCArgs* p);
DC_API DCuint      dcbArgUInt     (DCArgs* p);
DC_API DCulong     dcbArgULong    (DCArgs* p);
DC_API DCulonglong dcbArgULongLong(DCArgs* p);
DC_API DCfloat     dcbArgFloat    (DCArgs* p);
DC_API DCdouble    dcbArgDouble   (DCArgs* p);
DC_API DCpointer   dcbArgPointer  (DCArgs* p);
/* for trivial aggrs: 'target' points to space to copy aggr to, returns 'target'
   for C++ non-trivial aggrs: target is ignored, returns ptr to aggr arg */
DC_API DCpointer   dcbArgAggr     (DCArgs* p, DCpointer target);

/* helper func to put a to be returned struct-by-value into the 'result'
   param of the callback handler; for C++ non-trivial aggrs, pass NULL in
   'ret', then copy aggr into result->p */
DC_API void dcbReturnAggr(DCArgs *args, DCValue *result, DCpointer ret);

#ifdef __cplusplus
}
#endif

#endif /* DYNCALL_ARGS_H */

