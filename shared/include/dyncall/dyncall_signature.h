/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_signature.h
 Description: Type and calling-convention signature character defines
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


#ifndef DYNCALL_SIGNATURE_H
#define DYNCALL_SIGNATURE_H

typedef char DCsigchar;

#define DC_SIGCHAR_VOID         'v'
#define DC_SIGCHAR_BOOL         'B'
#define DC_SIGCHAR_CHAR         'c'
#define DC_SIGCHAR_UCHAR        'C'
#define DC_SIGCHAR_SHORT        's'
#define DC_SIGCHAR_USHORT       'S'
#define DC_SIGCHAR_INT          'i'
#define DC_SIGCHAR_UINT         'I'
#define DC_SIGCHAR_LONG         'j'
#define DC_SIGCHAR_ULONG        'J'
#define DC_SIGCHAR_LONGLONG     'l'
#define DC_SIGCHAR_ULONGLONG    'L'
#define DC_SIGCHAR_FLOAT        'f'
#define DC_SIGCHAR_DOUBLE       'd'
#define DC_SIGCHAR_POINTER      'p' /* also used for arrays, as such args decay to ptrs */
#define DC_SIGCHAR_STRING       'Z' /* in theory same as 'p', but convenient to disambiguate */
#define DC_SIGCHAR_AGGREGATE    'A' /* aggregate (struct/union described out-of-band via DCaggr) */
#define DC_SIGCHAR_ENDARG       ')'

/* calling convention / mode signatures */

#define DC_SIGCHAR_CC_PREFIX           '_' /* announces next char to be one of the below calling convention mode chars */
#define DC_SIGCHAR_CC_DEFAULT          ':' /* default calling conv (platform native) */
#define DC_SIGCHAR_CC_THISCALL         '*' /* C++ this calls (platform native) */
#define DC_SIGCHAR_CC_ELLIPSIS         'e'
#define DC_SIGCHAR_CC_ELLIPSIS_VARARGS '.'
#define DC_SIGCHAR_CC_CDECL            'c' /* x86 specific */
#define DC_SIGCHAR_CC_STDCALL          's' /* x86 specific */
#define DC_SIGCHAR_CC_FASTCALL_MS      'F' /* x86 specific */
#define DC_SIGCHAR_CC_FASTCALL_GNU     'f' /* x86 specific */
#define DC_SIGCHAR_CC_THISCALL_MS      '+' /* x86 specific, MS C++ this calls */
#define DC_SIGCHAR_CC_THISCALL_GNU     '#' /* x86 specific, GNU C++ this calls are cdecl, but keep specific sig char for clarity */
#define DC_SIGCHAR_CC_ARM_ARM          'A'
#define DC_SIGCHAR_CC_ARM_THUMB        'a'
#define DC_SIGCHAR_CC_SYSCALL          '$'

#endif /* DYNCALL_SIGNATURE_H */

