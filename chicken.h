/* chicken.h - General headerfile for compiler generated executables
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany
*/


/* Configuration: */

#ifndef ___CHICKEN
#define ___CHICKEN

#if defined(HAVE_CONFIG_H) || defined(HAVE_CHICKEN_CONFIG_H)
# include "chicken-config.h"
# include "chicken-defaults.h"
#endif

#if !defined(__GNUC__) && !defined(__WATCOMC__)
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#   pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#elif (defined(__sun__) && defined(__svr4__)) || defined(__sgi__)
# if HAVE_ALLOCA_H
#  include <alloca.h>
# endif
#endif

#ifdef __cplusplus
# define C_extern                  extern "C"
# define C_BEGIN_C_DECLS           extern "C" {
# define C_END_C_DECLS             }
#else
# define C_extern                  extern
# define C_BEGIN_C_DECLS
# define C_END_C_DECLS
#endif
 
#define C_varextern                C_extern
#define C_fctimport
#define C_fctexport
#define C_externimport             C_extern
#define C_externexport             C_extern
#if !(defined(C_NO_PIC_NO_DLL) && !defined(PIC))
# if defined(__CYGWIN__) || defined(__MINGW32__)
#  ifndef C_BUILDING_LIBCHICKEN
#   undef  C_varextern
#   define C_varextern             C_extern __declspec(dllimport)
#  endif
# elif defined(_MSC_VER)
#  undef  C_fctimport
#  define C_fctimport              __declspec(dllexport)
#  undef  C_externimport
#  undef  C_externexport
#  define C_externimport           C_extern __declspec(dllimport)
#  define C_externexport           C_extern __declspec(dllexport)
#  undef  C_varextern
#  undef  C_fctexport
#  ifdef C_BUILDING_LIBCHICKEN
#   define C_varextern             C_extern __declspec(dllexport)
#   define C_fctexport             __declspec(dllexport)
#  else
#   define C_varextern             C_extern __declspec(dllimport)
#   define C_fctexport             __declspec(dllimport)
#  endif
# elif defined(__WATCOMC__)
#  undef  C_fctimport
#  define C_fctimport              __declspec(dllexport)
#  undef  C_externimport
#  undef  C_externexport
#  define C_externexport           C_extern __declspec(dllexport)
#  undef  C_varextern
#  undef  C_fctexport
#  ifdef C_BUILDING_LIBCHICKEN
#   define C_varextern             C_extern __declspec(dllexport)
#   define C_fctexport             __declspec(dllexport)
#   define C_externimport          C_extern __declspec(dllexport)
#  else
#   define C_varextern             C_extern __declspec(dllimport)
#   define C_fctexport             __declspec(dllimport)
#   define C_externimport          C_extern __declspec(dllimport)
#  endif
# endif
#endif

#ifdef C_ENABLE_TLS
# if defined(__GNUC__)
#  define C_TLS                    __thread
# elif defined(_MSC_VER)
#  define C_TLS                    __declspec(thread)
# endif
#endif

#ifndef C_TLS
# define C_TLS
#endif

#ifndef C_STACK_GROWS_DOWNWARD
# define C_STACK_GROWS_DOWNWARD    -1
#endif

#if C_STACK_GROWS_DOWNWARD == -1
# ifdef __hppa__
#  undef C_STACK_GROWS_DOWNWARD
#  define C_STACK_GROWS_DOWNWARD 0
# else
#  undef C_STACK_GROWS_DOWNWARD
#  define C_STACK_GROWS_DOWNWARD 1
# endif
#endif

#if defined(C_WINDOWS_GUI)
# define C_MICROSOFT_WINDOWS
#else
# define C_GENERIC_CONSOLE
#endif

#define C_TIMER_INTERRUPTS

#ifdef C_DEFAULT_TARGET_STACK_SIZE
# define C_resize_stack(n)           C_do_resize_stack(C_DEFAULT_TARGET_STACK_SIZE)
#else
# define C_resize_stack(n)           C_do_resize_stack(n)
#endif

#if defined (__alpha__) || defined (__sparc_v9__) || defined (__sparcv9) || defined(__ia64__) || defined(__x86_64__) || defined(__LP64__)
# define C_SIXTY_FOUR
#endif

#if defined(__mips64) && (!defined(__GNUC__) || _MIPS_SZPTR == 64)
# define C_SIXTY_FOUR
#endif

#if defined(__APPLE__) && defined(__MACH__)
# define C_MACOSX
/*
 * Darwin provides gcvt/ecvt/fcvt for compatibility with legacy code.
 * (They don't even have a header definition!)
 * Use snprintf instead.
 */
#endif

#if defined(_MSC_VER) || defined(__MWERKS__) || defined(__DJGPP__) || defined(__MINGW32__) || defined(__WATCOMC__)
# define C_NONUNIX
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>
#include <limits.h>
#include <time.h>

#ifdef C_SIXTY_FOUR
# if defined(HAVE_STDINT_H)
#  include <stdint.h>
# else
#  include <sys/types.h>
# endif
#endif

#ifndef C_NONUNIX
# include <unistd.h>
#endif

#if defined(__MWERKS__) && !defined(__INTEL__)
/* This is a rather crude way of assuming this is MacOS 9 or lower! */
# define C_MACOS
# include <alloca.h>
int strncasecmp(const char *one, const char *two, size_t n);
#endif

#ifdef __WATCOMC__
# include <malloc.h>
#endif

#ifdef _MSC_VER
# include <malloc.h>
# include <io.h>
# define alloca            _alloca
# define strncasecmp       strnicmp
# define isatty            _isatty
# pragma warning(disable: 4101)
#endif

#ifdef __MINGW32__
# include <malloc.h>
#endif

#if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
# define C_GNU_ENV
#endif

/* For the easy FFI: */

#define ___fixnum           int
#define ___number           double
#define ___bool             int
#define ___byte             char
#define ___scheme_value     C_word
#define ___scheme_pointer   void *
#define ___byte_vector      unsigned char *
#define ___symbol           char *
#define ___callback
#define ___safe
#define ___declare(x, y)
#define ___specialize
#define ___abstract
#define ___discard
#define ___in
#define ___out
#define ___inout
#define ___mutable
#define ___length(var)
#define ___pointer
#define ___u32              C_u32
#define ___s32              C_s32
#define ___u64              C_u64
#define ___s64              C_s64


/* Constants: */

#define C_STACK_RESERVE                   4096
#define C_DEFAULT_MAX_PENDING_FINALIZERS  2048

#define C_IMMEDIATE_MARK_BITS     0x00000003
#define C_IMMEDIATE_TYPE_BITS     0x0000000f

#define C_BOOLEAN_BITS            0x00000006
#define C_CHARACTER_BITS          0x0000000a
#define C_SPECIAL_BITS            0x0000000e

#define C_SCHEME_FALSE            ((C_word)(C_BOOLEAN_BITS | 0x00000000))
#define C_SCHEME_TRUE             ((C_word)(C_BOOLEAN_BITS | 0x00000010))

#define C_SCHEME_END_OF_LIST      ((C_word)(C_SPECIAL_BITS | 0x00000000))
#define C_SCHEME_UNDEFINED        ((C_word)(C_SPECIAL_BITS | 0x00000010))
#define C_SCHEME_UNBOUND          ((C_word)(C_SPECIAL_BITS | 0x00000020))
#define C_SCHEME_END_OF_FILE      ((C_word)(C_SPECIAL_BITS | 0x00000030))

#define C_FIXNUM_BIT              0x00000001
#define C_FIXNUM_SHIFT            1

#define C_CHAR_BIT_MASK           0x1fffff

#ifdef C_SIXTY_FOUR
# define C_MOST_POSITIVE_FIXNUM   0x3fffffffffffffffL
# define C_WORD_SIZE              64
#else
# define C_MOST_POSITIVE_FIXNUM   0x3fffffff
# define C_WORD_SIZE              32
#endif

#define C_MOST_NEGATIVE_FIXNUM    (-C_MOST_POSITIVE_FIXNUM - 1)

#ifdef C_SIXTY_FOUR
# define C_INT_SIGN_BIT           0x8000000000000000L
# define C_INT_TOP_BIT            0x4000000000000000L
# define C_HEADER_BITS_MASK       0xff00000000000000L
# define C_HEADER_TYPE_BITS       0x0f00000000000000L
# define C_HEADER_SIZE_MASK       0x00ffffffffffffffL
# define C_GC_FORWARDING_BIT      0x8000000000000000L   /* header contains forwarding pointer */
# define C_BYTEBLOCK_BIT          0x4000000000000000L   /* block contains bytes instead of slots */
# define C_SPECIALBLOCK_BIT       0x2000000000000000L   /* 1st item is a non-value */
# define C_8ALIGN_BIT             0x1000000000000000L   /* data is aligned to 8-byte boundary */

# define C_SYMBOL_TYPE            (0x0100000000000000L)
# define C_STRING_TYPE            (0x0200000000000000L | C_BYTEBLOCK_BIT)
# define C_PAIR_TYPE              (0x0300000000000000L)
# define C_CLOSURE_TYPE           (0x0400000000000000L | C_SPECIALBLOCK_BIT)
# define C_FLONUM_TYPE            (0x0500000000000000L | C_BYTEBLOCK_BIT | C_8ALIGN_BIT)
# define C_UNUSED_TYPE            (0x0600000000000000L)
# define C_PORT_TYPE              (0x0700000000000000L | C_SPECIALBLOCK_BIT)
# define C_STRUCTURE_TYPE         (0x0800000000000000L)
# define C_POINTER_TYPE           (0x0900000000000000L | C_SPECIALBLOCK_BIT)
# define C_BUCKET_TYPE            (0x0f00000000000000L)
# define C_LOCATIVE_TYPE          (0x0a00000000000000L | C_SPECIALBLOCK_BIT)
# define C_TAGGED_POINTER_TYPE    (0x0b00000000000000L | C_SPECIALBLOCK_BIT)
# define C_SWIG_POINTER_TYPE      (0x0c00000000000000L | C_BYTEBLOCK_BIT)
# define C_LAMBDA_INFO_TYPE       (0x0d00000000000000L | C_BYTEBLOCK_BIT)
#else
# define C_INT_SIGN_BIT           0x80000000
# define C_INT_TOP_BIT            0x40000000
# define C_HEADER_BITS_MASK       0xff000000
# define C_HEADER_TYPE_BITS       0x0f000000
# define C_HEADER_SIZE_MASK       0x00ffffff
# define C_GC_FORWARDING_BIT      0x80000000
# define C_BYTEBLOCK_BIT          0x40000000
# define C_SPECIALBLOCK_BIT       0x20000000
# define C_8ALIGN_BIT             0x10000000

# define C_SYMBOL_TYPE            (0x01000000)
# define C_STRING_TYPE            (0x02000000 | C_BYTEBLOCK_BIT)
# define C_PAIR_TYPE              (0x03000000)
# define C_CLOSURE_TYPE           (0x04000000 | C_SPECIALBLOCK_BIT)
# ifdef C_DOUBLE_IS_32_BITS
#  define C_FLONUM_TYPE            (0x05000000 | C_BYTEBLOCK_BIT)
# else
#  define C_FLONUM_TYPE            (0x05000000 | C_BYTEBLOCK_BIT | C_8ALIGN_BIT)
# endif
# define C_UNUSED_TYPE            (0x06000000)
# define C_PORT_TYPE              (0x07000000 | C_SPECIALBLOCK_BIT)
# define C_STRUCTURE_TYPE         (0x08000000)
# define C_POINTER_TYPE           (0x09000000 | C_SPECIALBLOCK_BIT)
# define C_BUCKET_TYPE            (0x0f000000)
# define C_LOCATIVE_TYPE          (0x0a000000 | C_SPECIALBLOCK_BIT)
# define C_TAGGED_POINTER_TYPE    (0x0b000000 | C_SPECIALBLOCK_BIT)
# define C_SWIG_POINTER_TYPE      (0x0c000000 | C_BYTEBLOCK_BIT)
# define C_LAMBDA_INFO_TYPE       (0x0d000000 | C_BYTEBLOCK_BIT)
#endif

#define C_SLOT_LOCATIVE           0
#define C_CHAR_LOCATIVE           1
#define C_U8_LOCATIVE             2
#define C_S8_LOCATIVE             3
#define C_U16_LOCATIVE            4
#define C_S16_LOCATIVE            5
#define C_U32_LOCATIVE            6
#define C_S32_LOCATIVE            7
#define C_F32_LOCATIVE            8
#define C_F64_LOCATIVE            9

#define C_VECTOR_TYPE             0x00000000
#define C_BYTEVECTOR_TYPE         (C_VECTOR_TYPE | C_BYTEBLOCK_BIT | C_8ALIGN_BIT)

#define C_SIZEOF_LIST(n)          ((n) * 3 + 1)
#define C_SIZEOF_PAIR             3
#define C_SIZEOF_STRING(n)        (C_bytestowords(n) + 2)
#ifdef C_EXTRA_SYMBOL_SLOT
# define C_SIZEOF_SYMBOL          4
#else
# define C_SIZEOF_SYMBOL          3
#endif
#define C_SIZEOF_INTERNED_SYMBOL(n) (C_SIZEOF_SYMBOL + C_SIZEOF_BUCKET + C_SIZEOF_STRING(n))
#ifdef C_DOUBLE_IS_32_BITS
# define C_SIZEOF_FLONUM           2
#else
# define C_SIZEOF_FLONUM           4
#endif
#define C_SIZEOF_POINTER          2
#define C_SIZEOF_TAGGED_POINTER   3
#define C_SIZEOF_SWIG_POINTER     3
#define C_SIZEOF_VECTOR(n)        ((n) + 1)
#define C_SIZEOF_BUCKET           3
#define C_SIZEOF_LOCATIVE         5
#define C_SIZEOF_PORT             16

#define C_PAIR_TAG                (C_PAIR_TYPE | (C_SIZEOF_PAIR - 1))
#define C_POINTER_TAG             (C_POINTER_TYPE | (C_SIZEOF_POINTER - 1))
#define C_LOCATIVE_TAG            (C_LOCATIVE_TYPE | (C_SIZEOF_LOCATIVE - 1))
#define C_TAGGED_POINTER_TAG      (C_TAGGED_POINTER_TYPE | (C_SIZEOF_TAGGED_POINTER - 1))
#define C_SWIG_POINTER_TAG        (C_SWIG_POINTER_TYPE | (C_wordstobytes(C_SIZEOF_SWIG_POINTER - 1)))
#define C_SYMBOL_TAG              (C_SYMBOL_TYPE | (C_SIZEOF_SYMBOL - 1))
#define C_FLONUM_TAG             (C_FLONUM_TYPE | sizeof(double))

#ifdef C_SIXTY_FOUR
# define C_word                   long
# define C_u32                    uint32_t
# define C_s32                    int32_t
#else
# define C_word                   int
# define C_u32                    unsigned int
# define C_s32                    int
#endif

#if defined(_MSC_VER) || defined (__MINGW32__)
# define C_s64                    __int64
#else
# define C_s64                    int64_t
#endif

#define C_char                    char
#define C_uchar                   unsigned C_char
#define C_byte                    char
#define C_uword                   unsigned C_word
#define C_header                  C_uword
#define C_text(x)                 x

#define C_TIMER_INTERRUPT_NUMBER  255

#define C_BAD_ARGUMENT_COUNT_ERROR                    1
#define C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR            2
#define C_BAD_ARGUMENT_TYPE_ERROR                     3
#define C_UNBOUND_VARIABLE_ERROR                      4
#define C_TOO_MANY_PARAMETERS_ERROR                   5
#define C_OUT_OF_MEMORY_ERROR                         6
#define C_DIVISION_BY_ZERO_ERROR                      7
#define C_OUT_OF_RANGE_ERROR                          8
#define C_NOT_A_CLOSURE_ERROR                         9
#define C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR      10
#define C_TOO_DEEP_RECURSION_ERROR                    12
#define C_CANT_REPRESENT_INEXACT_ERROR                13
#define C_NOT_A_PROPER_LIST_ERROR                     14
#define C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR           15
#define C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR           16
#define C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR           17
#define C_BAD_ARGUMENT_TYPE_NO_PAIR_ERROR             18
#define C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR             19
#define C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR             20
#define C_BAD_ARGUMENT_TYPE_NO_VECTOR_ERROR           21
#define C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR           22
#define C_STACK_OVERFLOW_ERROR                        23
#define C_BAD_ARGUMENT_TYPE_BAD_STRUCT_ERROR          24
#define C_BAD_ARGUMENT_TYPE_NO_BYTEVECTOR_ERROR       25
#define C_LOST_LOCATIVE_ERROR                         26
#define C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR            27
#define C_BAD_ARGUMENT_TYPE_NO_NUMBER_VECTOR_ERROR    28
#define C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR          29
#define C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR         30
#define C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR          31
#define C_BAD_ARGUMENT_TYPE_NO_TAGGED_POINTER_ERROR   32
#define C_RUNTIME_UNSAFE_DLOAD_SAFE_ERROR             33
#define C_RUNTIME_SAFE_DLOAD_UNSAFE_ERROR             34
#define C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR           35


#define CHICKEN_gc_root_ref(root)      (((C_GC_ROOT *)(root))->value)
#define CHICKEN_gc_root_set(root, x)   C_mutate(&((C_GC_ROOT *)(root))->value, (x))

#define CHICKEN_global_ref(root)       C_u_i_car(((C_GC_ROOT *)(root))->value)
#define CHICKEN_global_set(root, x)    C_mutate(&C_u_i_car(((C_GC_ROOT *)(root))->value), (x))

#define CHICKEN_default_toplevel       ((void *)C_default_stub_toplevel)


/* Language specifics: */
#if defined(__GNUC__) || defined(__INTEL_COMPILER)
# ifndef __cplusplus
#  define C_cblock                ({
#  define C_cblockend             })
#  define C_noret                 __attribute__ ((noreturn))
#  define C_noret_decl(name)
# endif
# ifdef __i386__
#  define C_regparm               __attribute__ ((regparm(3)))
# endif
#elif defined(_MSC_VER)
# define C_fcall                  __fastcall
#elif defined(__WATCOMC__)
# define C_ccall                  __cdecl
#endif

#ifndef C_cblock
# define C_cblock                 do{
# define C_cblockend              }while(0)
# define C_noret
# define C_noret_decl(name)
#endif

#ifndef C_regparm
# define C_regparm
#endif

#ifndef C_fcall
# define C_fcall
#endif

#ifndef C_ccall
# define C_ccall
#endif

#define C_c_regparm

/* Types: */

typedef struct C_block_struct
{
  C_header header;
  C_word data[ 1 ];
} C_SCHEME_BLOCK;

typedef struct C_symbol_table_struct
{
  char *name;
  unsigned int size;
  C_word *table;
  struct C_symbol_table_struct *next;
} C_SYMBOL_TABLE;

typedef struct C_gc_root_struct
{
  C_word value;
  struct C_gc_root_struct *next, *prev;
} C_GC_ROOT;

typedef struct C_ptable_entry_struct
{
  C_char *id;
  void *ptr;
} C_PTABLE_ENTRY;

#ifdef __x86_64__
# define C_AMD64_ABI_WEIRDNESS      , ...
#else
# define C_AMD64_ABI_WEIRDNESS      
#endif

/* C_WORD_p<P>_<B>: List of ((2 ** P) * B) 'C_word' parameters */
#define C_WORD_p0_0
#define C_WORD_p1_0
#define C_WORD_p2_0
#define C_WORD_p3_0
#define C_WORD_p4_0
#define C_WORD_p5_0
#define C_WORD_p6_0
#define C_WORD_p7_0
#define C_WORD_p0_1     C_word,
#define C_WORD_p1_1     C_word, C_word,
#define C_WORD_p2_1     C_WORD_p1_1 C_WORD_p1_1
#define C_WORD_p3_1     C_WORD_p2_1 C_WORD_p2_1
#define C_WORD_p4_1     C_WORD_p3_1 C_WORD_p3_1
#define C_WORD_p5_1     C_WORD_p4_1 C_WORD_p4_1
#define C_WORD_p6_1     C_WORD_p5_1 C_WORD_p5_1
#define C_WORD_p7_1     C_WORD_p6_1 C_WORD_p6_1

/* DECL_C_PROC_p0 (n0,  p7,p6,p5,p4,p3,p2,p1,p0):
 *  declare function C_proc<n0>, which have <n0> 'C_word' parameters
 *  (not counting last 'C_word C_AMD64_ABI_WEIRDNESS' one).
 *  We must have:   n0 = SUM (i = 7 to 0, p<i> * (1 << i)).
 * DECL_C_PROC_p<N+1> (...):
 *  declare 2 as much functions as DECL_C_PROC_p<N>...
 */
#define DECL_C_PROC_p0( n0,  p7,p6,p5,p4,p3,p2,p1,p0) \
    typedef void (C_ccall *C_proc##n0) (C_WORD_p7_##p7 C_WORD_p6_##p6 \
                                        C_WORD_p5_##p5 C_WORD_p4_##p4 \
                                        C_WORD_p3_##p3 C_WORD_p2_##p2 \
                                        C_WORD_p1_##p1 C_WORD_p0_##p0 \
                                        C_word C_AMD64_ABI_WEIRDNESS) C_noret;
#define DECL_C_PROC_p1( n0,n1,  p7,p6,p5,p4,p3,p2,p1) \
        DECL_C_PROC_p0 (n0,  p7,p6,p5,p4,p3,p2,p1,0) \
        DECL_C_PROC_p0 (n1,  p7,p6,p5,p4,p3,p2,p1,1)
#define DECL_C_PROC_p2( n0,n1,n2,n3,  p7,p6,p5,p4,p3,p2) \
        DECL_C_PROC_p1 (n0,n1,  p7,p6,p5,p4,p3,p2,0) \
        DECL_C_PROC_p1 (n2,n3,  p7,p6,p5,p4,p3,p2,1)
#define DECL_C_PROC_p3( n0,n1,n2,n3,n4,n5,n6,n7,  p7,p6,p5,p4,p3) \
        DECL_C_PROC_p2 (n0,n1,n2,n3,  p7,p6,p5,p4,p3,0) \
        DECL_C_PROC_p2 (n4,n5,n6,n7,  p7,p6,p5,p4,p3,1)

DECL_C_PROC_p1 (2,3,  0,0,0,0,0,0,1)
DECL_C_PROC_p2 (4,5,6,7,  0,0,0,0,0,1)
DECL_C_PROC_p3 (8,9,10,11,12,13,14,15,    0,0,0,0,1)
DECL_C_PROC_p3 (16,17,18,19,20,21,22,23,  0,0,0,1,0)
DECL_C_PROC_p3 (24,25,26,27,28,29,30,31,  0,0,0,1,1)
DECL_C_PROC_p3 (32,33,34,35,36,37,38,39,  0,0,1,0,0)
DECL_C_PROC_p3 (40,41,42,43,44,45,46,47,  0,0,1,0,1)
DECL_C_PROC_p3 (48,49,50,51,52,53,54,55,  0,0,1,1,0)
DECL_C_PROC_p3 (56,57,58,59,60,61,62,63,  0,0,1,1,1)
DECL_C_PROC_p1 (64,65,  0,1,0,0,0,0,0)
DECL_C_PROC_p0 (66,  0,1,0,0,0,0,1,0)
DECL_C_PROC_p0 (67,  0,1,0,0,0,0,1,1)
DECL_C_PROC_p2 (68,69,70,71,  0,1,0,0,0,1)
DECL_C_PROC_p3 (72,73,74,75,76,77,78,79,  0,1,0,0,1)
DECL_C_PROC_p3 (80,81,82,83,84,85,86,87,  0,1,0,1,0)
DECL_C_PROC_p3 (88,89,90,91,92,93,94,95,  0,1,0,1,1)
DECL_C_PROC_p3 (96,97,98,99,100,101,102,103,  0,1,1,0,0)
DECL_C_PROC_p3 (104,105,106,107,108,109,110,111,  0,1,1,0,1)
DECL_C_PROC_p3 (112,113,114,115,116,117,118,119,  0,1,1,1,0)
DECL_C_PROC_p3 (120,121,122,123,124,125,126,127,  0,1,1,1,1)
DECL_C_PROC_p0 (128,  1,0,0,0,0,0,0,0)


/* Macros: */

/* This is word-size dependent: */
#ifdef C_SIXTY_FOUR
# define C_align(n)                (((n) + 7) & ~7)
# define C_wordstobytes(n)         ((n) << 3)
# define C_bytestowords(n)         (((n) + 7) >> 3)
# define C_wordsperdouble(n)       (n)
# define C_WORD_MIN                LONG_MIN
# define C_WORD_MAX                LONG_MAX
# define C_UWORD_MAX               ULONG_MAX
#else
# define C_align(n)                (((n) + 3) & ~3)
# define C_wordstobytes(n)         ((n) << 2)
# define C_bytestowords(n)         (((n) + 3) >> 2)
# define C_wordsperdouble(n)       ((n) << 1)
# define C_WORD_MIN                INT_MIN
# define C_WORD_MAX                INT_MAX
# define C_UWORD_MAX               UINT_MAX
#endif

#ifndef C_PROVIDE_LIBC_STUBS
# define C_FILEPTR                  FILE *

# define C_stdin                    stdin
# define C_stdout                   stdout
# define C_stderr                   stderr

# define C_memcpy                   memcpy
# define C_memcmp                   memcmp
# define C_strcpy                   strcpy
# define C_strncpy                  strncpy
# define C_strcmp                   strcmp
# define C_strncmp                  strncmp
# define C_strlen                   strlen
# define C_strcat                   strcat
# define C_memset                   memset
# define C_memmove                  memmove
# define C_strncasecmp              strncasecmp
# define C_malloc                   malloc
# define C_calloc                   calloc
# define C_free                     free
# define C_strchr                   strchr
# define C_realloc                  realloc
# define C_strdup                   strdup
# define C_strtol                   strtol
# define C_strtod                   strtod
# define C_strtoul                  strtoul
# define C_fopen                    fopen
# define C_fclose                   fclose
# define C_strpbrk                  strpbrk
# define C_gcvt                     gcvt
# define C_sprintf                  sprintf
# define C_snprintf                 snprintf
# define C_printf                   printf
# define C_fprintf                  fprintf
# define C_fflush                   fflush
# define C_getchar                  getchar
# define C_exit                     exit
# define C_dlopen                   dlopen
# define C_dlclose                  dlclose
# define C_dlsym                    dlsym
# define C_fwrite                   fwrite
# define C_fread                    fread
# define C_fputs                    fputs
# define C_fputc                    fputc
# define C_putchar                  putchar
# define C_fgetc                    fgetc
# define C_fgets                    fgets
# define C_ungetc                   ungetc
# define C_system                   system
# define C_isatty                   isatty
# define C_fileno                   fileno
# define C_select                   select
# define C_signal                   signal
# define C_getrusage                getrusage
# define C_tolower                  tolower
# define C_toupper                  toupper
# define C_gettimeofday             gettimeofday
# define C_gmtime                   gmtime
# define C_localtime                localtime
# define C_setjmp                   setjmp
# define C_longjmp                  longjmp
# define C_alloca                   alloca
# define C_strerror                 strerror
# define C_isalpha                  isalpha
# define C_isdigit                  isdigit
# define C_isspace                  isspace
# define C_islower                  islower
# define C_isupper                  isupper
#else
# include "chicken-libc-stubs.h"
#endif

#define C_return(x)                return(x)

#define C_memcpy_slots(t, f, n)    C_memcpy((t), (f), (n) * sizeof(C_word))
#define C_block_header(x)          (((C_SCHEME_BLOCK *)(x))->header)
#define C_header_bits(x)           (C_block_header(x) & C_HEADER_BITS_MASK)
#define C_header_size(x)           (C_block_header(x) & C_HEADER_SIZE_MASK)
#define C_make_header(type, size)  ((C_header)(((type) & C_HEADER_BITS_MASK) | ((size) & C_HEADER_SIZE_MASK)))
#define C_symbol_value(x)          (C_block_item(x, 0))
#define C_block_item(x, i)         (((C_SCHEME_BLOCK *)(x))->data[ i ])
#define C_set_block_item(x, i, y)  (C_block_item(x, i) = (y))
#define C_save(x)	           (*(--C_temporary_stack) = (C_word)(x))
#define C_adjust_stack(n)          (C_temporary_stack -= (n))
#define C_rescue(x, i)             (C_temporary_stack[ i ] = (x))
#define C_save_rest(s, c, n)  	   for(va_start(v, s); c-- > (n); C_save(va_arg(v, C_word)))
#define C_rest_count(c)            ((C_temporary_stack_bottom - C_temporary_stack) - (c))
#define C_restore                  (*(C_temporary_stack++))
#define C_heaptop                  ((C_word **)(&C_fromspace_top))
#define C_pick(n)                  (C_temporary_stack[ n ])
#define C_drop(n)                  (C_temporary_stack += (n))
#define C_alloc(n)                 ((C_word *)C_alloca((n) * sizeof(C_word)))
#define C_stack_pointer            ((C_word *)C_alloca(0))
#define C_stack_pointer_test       ((C_word *)C_alloca(1))
#define C_demand_2(n)              (((C_word *)C_fromspace_top + (n)) < (C_word *)C_fromspace_limit)
#define C_fix(n)                   (((C_word)(n) << C_FIXNUM_SHIFT) | C_FIXNUM_BIT)
#define C_unfix(x)                 ((x) >> C_FIXNUM_SHIFT)
#define C_make_character(c)        ((((c) & C_CHAR_BIT_MASK) << 8) | C_CHARACTER_BITS)
#define C_character_code(x)        (((x) >> 8) & C_CHAR_BIT_MASK)
#define C_flonum_magnitude(x)      (*((double *)(((C_SCHEME_BLOCK *)(x))->data)))
#define C_c_string(x)              ((C_char *)(((C_SCHEME_BLOCK *)(x))->data))
#define C_c_pointer(x)             ((void *)(x))
#define C_c_pointer_nn(x)          ((void *)C_block_item(x, 0))
#define C_truep(x)                 ((x) != C_SCHEME_FALSE)
#define C_immediatep(x)            ((x) & C_IMMEDIATE_MARK_BITS)
#define C_mk_bool(x)               ((x) ? C_SCHEME_TRUE : C_SCHEME_FALSE)
#define C_mk_nbool(x)              ((x) ? C_SCHEME_FALSE : C_SCHEME_TRUE)
#define C_port_file(p)             ((C_FILEPTR)C_u_i_car(p))
#define C_data_pointer(x)          ((void *)((C_SCHEME_BLOCK *)(x))->data)
#define C_invert_flag(f)           (!(f))
#define C_fitsinfixnump(n)         (((n) & C_INT_SIGN_BIT) == (((n) & C_INT_TOP_BIT) << 1))
#define C_ufitsinfixnump(n)        (((n) & (C_INT_SIGN_BIT | (C_INT_SIGN_BIT >> 1))) == 0)
#define C_quickflonumtruncate(n)   (C_fix((C_word)C_flonum_magnitude(n)))
#define C_and(x, y)                (C_truep(x) ? (y) : C_SCHEME_FALSE)
#define C_c_bytevector(x)          ((unsigned char *)C_data_pointer(x))
#define C_c_bytevector_or_null(x)  ((unsigned char *)C_data_pointer_or_null(x))
#define C_c_u8vector(x)            ((unsigned char *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_u8vector_or_null(x)    ((unsigned char *)C_srfi_4_vector_or_null(x))
#define C_c_s8vector(x)            ((char *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_s8vector_or_null(x)    ((char *)C_srfi_4_vector_or_null(x))
#define C_c_u16vector(x)           ((unsigned short *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_u16vector_or_null(x)   ((unsigned short *)C_srfi_4_vector_or_null(x))
#define C_c_s16vector(x)           ((short *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_s16vector_or_null(x)   ((short *)C_srfi_4_vector_or_null(x))
#define C_c_u32vector(x)           ((C_u32 *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_u32vector_or_null(x)   ((C_u32 *)C_srfi_4_vector_or_null(x))
#define C_c_s32vector(x)           ((C_s32 *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_s32vector_or_null(x)   ((C_s32 *)C_srfi_4_vector_or_null(x))
#define C_c_f32vector(x)           ((float *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_f32vector_or_null(x)   ((float *)C_srfi_4_vector_or_null(x))
#define C_c_f64vector(x)           ((double *)C_data_pointer(C_u_i_cdr(x)))
#define C_c_f64vector_or_null(x)   ((double *)C_srfi_4_vector_or_null(x))

#ifdef C_STRESS_TEST
# define C_STRESS_FAILURE          3
# define C_stress                  (rand() % C_STRESS_FAILURE)
#else
# define C_stress                  1
#endif

#if C_STACK_GROWS_DOWNWARD
# define C_demand(n)              (C_stress && ((C_word)(C_stack_pointer - C_stack_limit) > (n)))
# define C_stack_probe(p)         (C_stress && ((C_word *)(p) >= C_stack_limit))
# define C_stack_check            if(!C_disable_overflow_check && (C_byte*)(C_stack_pointer) + C_STACK_RESERVE < (C_byte *)C_stack_limit) C_stack_overflow()
#else
# define C_demand(n)              (C_stress && ((C_word)(C_stack_limit - C_stack_pointer) > (n)))
# define C_stack_probe(p)         (C_stress && ((C_word *)(p) < C_stack_limit))
# define C_stack_check            if(!C_disable_overflow_check && (C_byte*)(C_stack_pointer) - C_STACK_RESERVE > (C_byte *)C_stack_limit) C_stack_overflow()
#endif

#define C_zero_length_p(x)        C_mk_bool(C_header_size(x) == 0)
#define C_boundp(x)               C_mk_bool(((C_SCHEME_BLOCK *)(x))->data[ 0 ] != C_SCHEME_UNBOUND)
#define C_blockp(x)               C_mk_bool(!C_immediatep(x))
#define C_immp(x)                 C_mk_bool(C_immediatep(x))
#define C_flonump(x)              C_mk_bool(C_block_header(x) == C_FLONUM_TAG)
#define C_stringp(x)              C_mk_bool(C_header_bits(x) == C_STRING_TYPE)
#define C_symbolp(x)              C_mk_bool(C_block_header(x) == C_SYMBOL_TAG)
#define C_pairp(x)                C_mk_bool(C_block_header(x) == C_PAIR_TAG)
#define C_closurep(x)             C_mk_bool(C_header_bits(x) == C_CLOSURE_TYPE)
#define C_vectorp(x)              C_mk_bool(C_header_bits(x) == C_VECTOR_TYPE)
#define C_bytevectorp(x)          C_mk_bool(C_header_bits(x) == C_BYTEVECTOR_TYPE)
#define C_portp(x)                C_mk_bool(C_header_bits(x) == C_PORT_TYPE)
#define C_structurep(x)           C_mk_bool(C_header_bits(x) == C_STRUCTURE_TYPE)
#define C_locativep(x)            C_mk_bool(C_block_header(x) == C_LOCATIVE_TAG)
#define C_charp(x)                C_mk_bool(((x) & C_IMMEDIATE_TYPE_BITS) == C_CHARACTER_BITS)
#define C_booleanp(x)             C_mk_bool(((x) & C_IMMEDIATE_TYPE_BITS) == C_BOOLEAN_BITS)
#define C_eofp(x)                 C_mk_bool((x) == C_SCHEME_END_OF_FILE)
#define C_undefinedp(x)           C_mk_bool((x) == C_SCHEME_UNDEFINED)
#define C_fixnump(x)              C_mk_bool((x) & C_FIXNUM_BIT)
#define C_nfixnump(x)             C_mk_nbool((x) & C_FIXNUM_BIT)
#define C_pointerp(x)             C_mk_bool(C_block_header(x) == C_POINTER_TAG)
#define C_taggedpointerp(x)       C_mk_bool(C_block_header(x) == C_TAGGED_POINTER_TAG)
#define C_swigpointerp(x)         C_mk_bool(C_block_header(x) == C_SWIG_POINTER_TAG)
#define C_lambdainfop(x)          C_mk_bool(C_header_bits(x) == C_LAMBDA_INFO_TYPE)
#define C_anypointerp(x)          C_mk_bool(C_block_header(x) == C_POINTER_TAG || C_block_header(x) == C_TAGGED_POINTER_TAG || C_block_header(x) == C_SWIG_POINTER_TAG)
#define C_specialp(x)             C_mk_bool(C_header_bits(x) & C_SPECIALBLOCK_BIT)
#define C_byteblockp(x)           C_mk_bool(C_header_bits(x) & C_BYTEBLOCK_BIT)
#define C_anyp(x)                 C_SCHEME_TRUE
#define C_eqp(x, y)               C_mk_bool((x) == (y))
#define C_vemptyp(x)              C_mk_bool(C_header_size(x) == 0)
#define C_notvemptyp(x)           C_mk_bool(C_header_size(x) > 0)
#define C_slot(x, i)              (((C_SCHEME_BLOCK *)(x))->data[ C_unfix(i) ])
#define C_slot0(x)                (((C_SCHEME_BLOCK *)(x))->data[ 0 ])
#define C_subbyte(x, i)           C_fix(((C_byte *)((C_SCHEME_BLOCK *)(x))->data)[ C_unfix(i) ] & 0xff)
#define C_subchar(x, i)           C_make_character(((C_uchar *)((C_SCHEME_BLOCK *)(x))->data)[ C_unfix(i) ])
#define C_setbyte(x, i, n)        (((C_byte *)((C_SCHEME_BLOCK *)(x))->data)[ C_unfix(i) ] = C_unfix(n), C_SCHEME_UNDEFINED)
#define C_setsubchar(x, i, n)     (((C_char *)((C_SCHEME_BLOCK *)(x))->data)[ C_unfix(i) ] = C_character_code(n), C_SCHEME_UNDEFINED)
#define C_setsubbyte(x, i, n)     (((C_char *)((C_SCHEME_BLOCK *)(x))->data)[ C_unfix(i) ] = C_unfix(n), C_SCHEME_UNDEFINED)
#define C_fixnum_times(n1, n2)          (C_fix(C_unfix(n1) * C_unfix(n2)))
#define C_u_fixnum_plus(n1, n2)         (((n1) - C_FIXNUM_BIT) + (n2))
#define C_fixnum_plus(n1, n2)           (C_u_fixnum_plus(n1, n2) | C_FIXNUM_BIT)
#define C_u_fixnum_difference(n1, n2)   ((n1) - (n2) + C_FIXNUM_BIT)
#define C_fixnum_difference(n1, n2)     (C_u_fixnum_difference(n1, n2) | C_FIXNUM_BIT)
#define C_fixnum_divide(n1, n2)         (C_fix(C_unfix(n1) / C_unfix(n2)))
#define C_fixnum_modulo(n1, n2)         (C_fix(C_unfix(n1) % C_unfix(n2)))
#define C_u_fixnum_and(n1, n2)          ((n1) & (n2))
#define C_fixnum_and(n1, n2)            (C_u_fixnum_and(n1, n2) | C_FIXNUM_BIT)
#define C_u_fixnum_or(n1, n2)           ((n1) | (n2))
#define C_fixnum_or(n1, n2)             (C_u_fixnum_or(n1, n2) | C_FIXNUM_BIT)
#define C_fixnum_xor(n1, n2)            (((n1) ^ (n2)) | C_FIXNUM_BIT)
#define C_fixnum_not(n)                 ((~(n)) | C_FIXNUM_BIT)
#define C_fixnum_shift_left(n1, n2)     (C_fix(C_unfix(n1) << C_unfix(n2)))
#define C_fixnum_shift_right(n1, n2)    (((n1) >> C_unfix(n2)) | C_FIXNUM_BIT)
#define C_u_fixnum_negate(n)            (-(n) + 2 * C_FIXNUM_BIT)
#define C_fixnum_negate(n)              (C_u_fixnum_negate(n) | C_FIXNUM_BIT)
#define C_fixnum_greaterp(n1, n2)       (C_mk_bool((C_word)(n1) > (C_word)(n2)))
#define C_fixnum_lessp(n1, n2)          (C_mk_bool((C_word)(n1) < (C_word)(n2)))
#define C_fixnum_greater_or_equal_p(n1, n2) (C_mk_bool((C_word)(n1) >= (C_word)(n2)))
#define C_fixnum_less_or_equal_p(n1, n2)(C_mk_bool((C_word)(n1) <= (C_word)(n2)))
#define C_u_fixnum_increase(n)          ((n) + (1 << C_FIXNUM_SHIFT))
#define C_fixnum_increase(n)            (C_u_fixnum_increase(n) | C_FIXNUM_BIT)
#define C_u_fixnum_decrease(n)          ((n) - (1 << C_FIXNUM_SHIFT))
#define C_fixnum_decrease(n)            (C_u_fixnum_decrease(n) | C_FIXNUM_BIT)
#define C_fixnum_abs(n)                 C_fix(abs(C_unfix(n)))

#define C_flonum_equalp(n1, n2)         C_mk_bool(C_flonum_magnitude(n1) == C_flonum_magnitude(n2))
#define C_flonum_greaterp(n1, n2)       C_mk_bool(C_flonum_magnitude(n1) > C_flonum_magnitude(n2))
#define C_flonum_lessp(n1, n2)          C_mk_bool(C_flonum_magnitude(n1) < C_flonum_magnitude(n2))
#define C_flonum_greater_or_equal_p(n1, n2) C_mk_bool(C_flonum_magnitude(n1) >= C_flonum_magnitude(n2))
#define C_flonum_less_or_equal_p(n1, n2) C_mk_bool(C_flonum_magnitude(n1) <= C_flonum_magnitude(n2))

#define C_display_fixnum(p, n)          (C_fprintf(C_port_file(p), C_text("%d"), C_unfix(n)), C_SCHEME_UNDEFINED)
#define C_display_char(p, c)            (C_fputc(C_character_code(c), C_port_file(p)), C_SCHEME_UNDEFINED)
#define C_display_string(p, s)          (C_fwrite(((C_SCHEME_BLOCK *)(s))->data, sizeof(C_char), C_header_size(s), \
                                         C_port_file(p)), C_SCHEME_UNDEFINED)
#define C_fix_to_char(x)                (C_make_character(C_unfix(x)))
#define C_char_to_fix(x)                (C_fix(C_character_code(x)))
#define C_math_result(x)                (C_temporary_flonum = (x), C_SCHEME_UNDEFINED)
#define C_substring_copy(s1, s2, start1, end1, start2) \
                                        (C_memcpy((C_char *)C_data_pointer(s2) + C_unfix(start2), \
                                                  (C_char *)C_data_pointer(s1) + C_unfix(start1), \
                                                  C_unfix(end1) - C_unfix(start1) ), C_SCHEME_UNDEFINED)
#define C_substring_compare(s1, s2, start1, start2, len) \
                                        C_mk_bool(C_memcmp((C_char *)C_data_pointer(s1) + C_unfix(start1), \
                                                           (C_char *)C_data_pointer(s2) + C_unfix(start2), \
                                                           C_unfix(len) ) == 0)
#define C_substring_compare_case_insensitive(s1, s2, start1, start2, len) \
                                        C_mk_bool(C_strncasecmp((C_char *)C_data_pointer(s1) + C_unfix(start1), \
                                                                (C_char *)C_data_pointer(s2) + C_unfix(start2), \
                                                                C_unfix(len) ) == 0)
#define C_subvector_copy(v1, v2, start1, end1, start2) \
                                        (C_memcpy_slots((C_char *)C_data_pointer(v2) + C_unfix(start2), \
                                                  (C_char *)C_data_pointer(v1) + C_unfix(start1), \
						  C_unfix(end1) - C_unfix(start1) ), C_SCHEME_UNDEFINED)
#define C_words(n)                      C_fix(C_bytestowords(C_unfix(n)))
#define C_bytes(n)                      C_fix(C_wordstobytes(C_unfix(n)))
#define C_random_fixnum(n)              C_fix(rand() % C_unfix(n))
#define C_randomize(n)                  (srand(C_unfix(n)), C_SCHEME_UNDEFINED)
#define C_block_size(x)                 C_fix(C_header_size(x))
#define C_pointer_address(x)            ((C_byte *)C_u_i_car(x))
#define C_block_address(ptr, n, x)      C_a_unsigned_int_to_num(ptr, n, x)
#define C_offset_pointer(x, y)          (C_pointer_address(x) + (y))
#define C_kontinue(k, r)                ((C_proc2)(void *)C_u_i_car(k))(2, (k), (r))
#define C_fetch_byte(x, p)              (((unsigned C_byte *)((C_SCHEME_BLOCK *)(x))->data)[ p ])
#define C_poke_integer(x, i, n)         (C_set_block_item(x, C_unfix(i), C_num_to_int(n)), C_SCHEME_UNDEFINED)
#define C_pointer_to_block(p, x)        (C_set_block_item(p, 0, (C_word)C_data_pointer(x)), C_SCHEME_UNDEFINED)
#define C_null_pointerp(x)              C_mk_bool((void *)C_u_i_car(x) == NULL)
#define C_update_pointer(p, ptr)        (C_set_block_item(ptr, 0, C_num_to_unsigned_int(p)), C_SCHEME_UNDEFINED)
#define C_copy_pointer(from, to)        (C_set_block_item(to, 0, C_u_i_car(from)), C_SCHEME_UNDEFINED)

#define C_direct_return(dk, x)          (C_kontinue(dk, x), C_SCHEME_UNDEFINED)

#ifdef C_SIXTY_FOUR
# define C_poke_integer_32(x, i, n)     (((C_s32 *)C_data_pointer(x))[ C_unfix(i) ] = C_unfix(n), C_SCHEME_UNDEFINED)
#else
# define C_poke_integer_32              C_poke_integer
#endif

#define C_copy_memory(to, from, n)      (C_memcpy(C_data_pointer(to), C_data_pointer(from), C_unfix(n)), C_SCHEME_UNDEFINED)
#define C_set_memory(to, c, n)          (C_memset(C_data_pointer(to), C_character_code(c), C_unfix(n)), C_SCHEME_UNDEFINED)
#define C_string_compare(to, from, n)   C_fix(C_strncmp(C_c_string(to), C_c_string(from), C_unfix(n)))
#define C_string_compare_case_insensitive(from, to, n) \
                                        C_fix(C_strncasecmp(C_c_string(from), C_c_string(to), C_unfix(n)))
#define C_rename_file(old, new)         C_fix(rename(C_c_string(old), C_c_string(new)))
#define C_delete_file(fname)            C_fix(remove(C_c_string(fname)))
#define C_poke_double(b, i, n)          (((double *)C_data_pointer(b))[ C_unfix(i) ] = C_c_double(n), C_SCHEME_UNDEFINED)
#define C_poke_c_string(b, i, from)     (C_strcpy((char *)C_block_item(b, C_unfix(i)), C_data_pointer(from)), C_SCHEME_UNDEFINED)
#define C_peek_fixnum(b, i)             C_fix(C_block_item(b, C_unfix(i)))
#define C_peek_byte(ptr, i)             C_fix(((unsigned char *)C_u_i_car(ptr))[ C_unfix(i) ])
#define C_dupstr(s)                     C_strdup(C_data_pointer(s))
#define C_poke_pointer(b, i, x)         (C_set_block_item(b, C_unfix(i), (C_word)C_data_pointer(x)), C_SCHEME_UNDEFINED)
#define C_poke_pointer_or_null(b, i, x) (C_set_block_item(b, C_unfix(i), (C_word)C_data_pointer_or_null(x)), C_SCHEME_UNDEFINED)
#define C_qfree(ptr)                    (C_free(C_c_pointer_nn(ptr)), C_SCHEME_UNDEFINED)

#if defined(__MWERKS__) && !defined(__INTEL__)
# define C_tty_portp(p)                 C_SCHEME_FALSE
#else
# define C_tty_portp(p)                 C_mk_bool(isatty(fileno(C_port_file(p))))
#endif

#define C_emit_eval_trace_info(x, y, z) C_emit_trace_info2("<eval>", x, y, z)
#define C_emit_syntax_trace_info(x, y, z) C_emit_trace_info2("<syntax>", x, y, z)

/* These expect C_VECTOR_TYPE to be 0: */
#define C_vector_to_structure(v)        (((C_SCHEME_BLOCK *)(v))->header |= C_STRUCTURE_TYPE, C_SCHEME_UNDEFINED)
#define C_vector_to_closure(v)          (((C_SCHEME_BLOCK *)(v))->header |= C_CLOSURE_TYPE, C_SCHEME_UNDEFINED)
#define C_string_to_bytevector(s)       (((C_SCHEME_BLOCK *)(s))->header = C_header_size(s) | C_BYTEVECTOR_TYPE, C_SCHEME_UNDEFINED)
#define C_string_to_lambdainfo(s)       (((C_SCHEME_BLOCK *)(s))->header = C_header_size(s) | C_LAMBDA_INFO_TYPE, C_SCHEME_UNDEFINED)

#ifdef C_TIMER_INTERRUPTS
# ifdef PARANOIA
#  define C_check_for_interrupt         C_paranoid_check_for_interrupt()
# else
#  define C_check_for_interrupt         if(--C_timer_interrupt_counter <= 0) C_raise_interrupt(C_TIMER_INTERRUPT_NUMBER)
# endif
#else
# define C_check_for_interrupt
#endif

#if defined(__GNUC__) || defined(__INTEL_COMPILER)
# define C_a_i(a, n)                    ({C_word *tmp = *a; *a += (n); tmp;})
# define C_a_i_cons(a, n, car, cdr)     ({C_word tmp = (C_word)(*a); (*a)[0] = C_PAIR_TYPE | 2; *a += 3; \
                                          C_set_block_item(tmp, 0, car); C_set_block_item(tmp, 1, cdr); tmp;})
#else
# define C_a_i_cons(a, n, car, cdr)     C_pair(a, car, cdr)
#endif /* __GNUC__ */

#define C_a_i_data_mpointer(ptr, n, x)  C_mpointer(ptr, C_data_pointer(x))
#define C_a_int_to_num(ptr, n, i)       C_int_to_num(ptr, i)
#define C_a_unsigned_int_to_num(ptr, n, i)  C_unsigned_int_to_num(ptr, i)
#define C_a_i_vector                    C_vector
#define C_list                          C_a_i_list
#define C_i_setslot(x, i, y)            (C_mutate(&C_block_item(x, C_unfix(i)), y), C_SCHEME_UNDEFINED)
#define C_i_set_i_slot(x, i, y)         (C_set_block_item(x, C_unfix(i), y), C_SCHEME_UNDEFINED)
#define C_u_i_set_car(p, x)             (C_mutate(&C_u_i_car(p), x), C_SCHEME_UNDEFINED)
#define C_u_i_set_cdr(p, x)             (C_mutate(&C_u_i_cdr(p), x), C_SCHEME_UNDEFINED)

#define C_i_not(x)                      (C_truep(x) ? C_SCHEME_FALSE : C_SCHEME_TRUE)
#define C_i_equalp(x, y)                C_mk_bool(C_equalp((x), (y)))
#define C_i_fixnumevenp(x)              C_mk_nbool((x) & 0x00000002)
#define C_i_fixnumoddp(x)               C_mk_bool((x) & 0x00000002)
#define C_i_nullp(x)                    C_mk_bool((x) == C_SCHEME_END_OF_LIST)
#define C_i_structurep(x, s)            C_mk_bool(!C_immediatep(x) && C_header_bits(x) == C_STRUCTURE_TYPE && C_block_item(x, 0) == (s))

#define C_u_i_char_alphabeticp(x)       C_mk_bool(C_isalpha(C_character_code(x)))
#define C_u_i_char_numericp(x)          C_mk_bool(C_isdigit(C_character_code(x)))
#define C_u_i_char_whitespacep(x)       C_mk_bool(C_isspace(C_character_code(x)))
#define C_u_i_char_upper_casep(x)       C_mk_bool(C_isupper(C_character_code(x)))
#define C_u_i_char_lower_casep(x)       C_mk_bool(C_islower(C_character_code(x)))

#define C_u_i_char_upcase(x)            C_make_character(C_toupper(C_character_code(x)))
#define C_u_i_char_downcase(x)          C_make_character(C_tolower(C_character_code(x)))

#define C_i_list_ref(lst, i)            C_i_car(C_i_list_tail(lst, i))
#define C_u_i_list_ref(lst, i)          C_u_i_car(C_i_list_tail(lst, i))

#define C_u_i_car(x)                    C_block_item(x, 0)
#define C_u_i_cdr(x)                    C_block_item(x, 1)
#define C_u_i_caar(x)                   C_u_i_car( C_u_i_car( x ) )
#define C_u_i_cadr(x)                   C_u_i_car( C_u_i_cdr( x ) )
#define C_u_i_cdar(x)                   C_u_i_cdr( C_u_i_car( x ) )
#define C_u_i_cddr(x)                   C_u_i_cdr( C_u_i_cdr( x ) )
#define C_u_i_caaar(x)                  C_u_i_car( C_u_i_caar( x ) )
#define C_u_i_caadr(x)                  C_u_i_car( C_u_i_cadr( x ) )
#define C_u_i_cadar(x)                  C_u_i_car( C_u_i_cdar( x ) )
#define C_u_i_caddr(x)                  C_u_i_car( C_u_i_cddr( x ) )
#define C_u_i_cdaar(x)                  C_u_i_cdr( C_u_i_caar( x ) )
#define C_u_i_cdadr(x)                  C_u_i_cdr( C_u_i_cadr( x ) )
#define C_u_i_cddar(x)                  C_u_i_cdr( C_u_i_cdar( x ) )
#define C_u_i_cdddr(x)                  C_u_i_cdr( C_u_i_cddr( x ) )
#define C_u_i_caaaar(x)                 C_u_i_car( C_u_i_caaar( x ) )
#define C_u_i_caaadr(x)                 C_u_i_car( C_u_i_caadr( x ) )
#define C_u_i_caadar(x)                 C_u_i_car( C_u_i_cadar( x ) )
#define C_u_i_caaddr(x)                 C_u_i_car( C_u_i_caddr( x ) )
#define C_u_i_cadaar(x)                 C_u_i_car( C_u_i_cdaar( x ) )
#define C_u_i_cadadr(x)                 C_u_i_car( C_u_i_cdadr( x ) )
#define C_u_i_caddar(x)                 C_u_i_car( C_u_i_cddar( x ) )
#define C_u_i_cadddr(x)                 C_u_i_car( C_u_i_cdddr( x ) )
#define C_u_i_cdaaar(x)                 C_u_i_cdr( C_u_i_caaar( x ) )
#define C_u_i_cdaadr(x)                 C_u_i_cdr( C_u_i_caadr( x ) )
#define C_u_i_cdadar(x)                 C_u_i_cdr( C_u_i_cadar( x ) )
#define C_u_i_cdaddr(x)                 C_u_i_cdr( C_u_i_caddr( x ) )
#define C_u_i_cddaar(x)                 C_u_i_cdr( C_u_i_cdaar( x ) )
#define C_u_i_cddadr(x)                 C_u_i_cdr( C_u_i_cdadr( x ) )
#define C_u_i_cdddar(x)                 C_u_i_cdr( C_u_i_cddar( x ) )
#define C_u_i_cddddr(x)                 C_u_i_cdr( C_u_i_cdddr( x ) )

#define C_a_i_times( ptr, n, x, y)      C_2_times( ptr, x, y)
#define C_a_i_plus(  ptr, n, x, y)      C_2_plus(  ptr, x, y)
#define C_a_i_minus( ptr, n, x, y)      C_2_minus( ptr, x, y)
#define C_a_i_divide(ptr, n, x, y)      C_2_divide(ptr, x, y)

#if defined(__GNUC__) || defined(__INTEL_COMPILER)
# define C_i_not_pair_p(x)              ({C_word tmp = (x); C_mk_bool(C_immediatep(tmp) || C_block_header(tmp) != C_PAIR_TAG);})
#else
# define C_i_not_pair_p                 C_i_not_pair_p_2
#endif

#define C_i_check_exact(x)              C_i_check_exact_2(x, C_SCHEME_FALSE)
#define C_i_check_number(x)             C_i_check_number_2(x, C_SCHEME_FALSE)
#define C_i_check_string(x)             C_i_check_string_2(x, C_SCHEME_FALSE)
#define C_i_check_bytevector(x)         C_i_check_bytevector_2(x, C_SCHEME_FALSE)
#define C_i_check_symbol(x)             C_i_check_symbol_2(x, C_SCHEME_FALSE)
#define C_i_check_list(x)               C_i_check_list_2(x, C_SCHEME_FALSE)
#define C_i_check_pair(x)               C_i_check_pair_2(x, C_SCHEME_FALSE)
#define C_i_check_vector(x)             C_i_check_vector_2(x, C_SCHEME_FALSE)
#define C_i_check_structure(x, st)      C_i_check_structure_2(x, (st), C_SCHEME_FALSE)
#define C_i_check_char(x)               C_i_check_char_2(x, C_SCHEME_FALSE)

#define C_u_i_8vector_length(x)         C_fix(C_header_size(C_block_item(x, 1)))
#define C_u_i_16vector_length(x)        C_fix(C_header_size(C_block_item(x, 1)) >> 1)
#define C_u_i_32vector_length(x)        C_fix(C_header_size(C_block_item(x, 1)) >> 2)
#define C_u_i_64vector_length(x)        C_fix(C_header_size(C_block_item(x, 1)) >> 3)

#define C_u_i_u8vector_ref(x, i)        C_fix(((unsigned char *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_u_i_s8vector_ref(x, i)        C_fix(((char *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_u_i_u16vector_ref(x, i)       C_fix(((unsigned short *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_u_i_s16vector_ref(x, i)       C_fix(((short *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_u_i_u32vector_ref(x, i)       C_fix(((C_u32 *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_u_i_s32vector_ref(x, i)       C_fix(((C_u32 *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_a_i_u32vector_ref(ptr, c, x, i)  C_unsigned_int_to_num(ptr, ((C_u32 *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])
#define C_a_i_s32vector_ref(ptr, c, x, i)  C_int_to_num(ptr, ((C_s32 *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ])

#define C_u_i_u8vector_set(x, i, v)     ((((unsigned char *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ] = C_unfix(v)), C_SCHEME_UNDEFINED)
#define C_u_i_s8vector_set(x, i, v)     ((((char *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ] = C_unfix(v)), C_SCHEME_UNDEFINED)
#define C_u_i_u16vector_set(x, i, v)    ((((unsigned short *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ] = C_unfix(v)), C_SCHEME_UNDEFINED)
#define C_u_i_s16vector_set(x, i, v)    ((((short *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ] = C_unfix(v)), C_SCHEME_UNDEFINED)
#define C_u_i_u32vector_set(x, i, v)    ((((C_u32 *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ] = C_num_to_unsigned_int(v)), C_SCHEME_UNDEFINED)
#define C_u_i_s32vector_set(x, i, v)    ((((C_s32 *)C_data_pointer(C_block_item((x), 1)))[ C_unfix(i) ] = C_num_to_int(v)), C_SCHEME_UNDEFINED)

#define C_u_i_bit_setp(x, i)            C_mk_bool((C_unfix(x) & (1 << C_unfix(i))) != 0)

#define C_end_of_main

#if !defined(C_EMBEDDED) && !defined(C_SHARED)
# ifndef C_WINDOWS_GUI
#  define C_main_entry_point            int main(int argc, char *argv[]) { return CHICKEN_main(argc, argv, (void*)C_toplevel); } C_end_of_main
# else
#  define C_main_entry_point            \
  int WINAPI WinMain(HINSTANCE me, HINSTANCE you, LPSTR cmdline, int show) \
  { return CHICKEN_main(0, NULL, (void *)C_toplevel); } C_end_of_main
# endif
#else
# define C_main_entry_point
#endif


/* Variables: */

C_varextern C_TLS time_t C_startup_time_seconds;
C_varextern C_TLS C_word 
  *C_temporary_stack,
  *C_temporary_stack_bottom,
  *C_stack_limit;
C_varextern C_TLS long
  C_timer_interrupt_counter,
  C_initial_timer_interrupt_period;
C_varextern C_TLS C_byte
  *C_fromspace_top,
  *C_fromspace_limit;
C_varextern C_TLS double C_temporary_flonum;
C_varextern C_TLS jmp_buf C_restart;
C_varextern C_TLS void *C_restart_address;
C_varextern C_TLS int C_entry_point_status;

C_varextern C_TLS void (C_fcall *C_restart_trampoline)(void *proc) C_regparm C_noret;
C_varextern C_TLS void (*C_post_gc_hook)(int mode);

C_varextern C_TLS int
  C_abort_on_thread_exceptions,
  C_interrupts_enabled,
  C_disable_overflow_check,
  C_enable_gcweak,
  C_heap_size_is_fixed,
  C_max_pending_finalizers,
  C_trace_buffer_size,
  C_main_argc;
C_varextern C_TLS C_uword 
  C_heap_growth,
  C_heap_shrinkage;
C_varextern C_TLS char 
  **C_main_argv,
  *C_dlerror;
C_varextern C_TLS C_uword C_maximal_heap_size;
C_varextern C_TLS int (*C_gc_mutation_hook)(C_word *slot, C_word val);
C_varextern C_TLS void (*C_gc_trace_hook)(C_word *var, int mode);
C_varextern C_TLS C_word (*C_get_unbound_variable_value_hook)(C_word sym);


/* Prototypes: */

C_BEGIN_C_DECLS

C_fctexport int CHICKEN_main(int argc, char *argv[], void *toplevel);
C_fctexport int CHICKEN_initialize(int heap, int stack, int symbols, void *toplevel);
C_fctexport C_word CHICKEN_run(void *toplevel);
C_fctexport C_word CHICKEN_continue(C_word k);
C_fctexport void *CHICKEN_new_gc_root();
C_fctexport void CHICKEN_delete_gc_root(void *root);
C_fctexport void *CHICKEN_global_lookup(char *name);
C_fctexport int CHICKEN_is_running();
C_fctexport void CHICKEN_interrupt();

C_fctexport void C_check_nursery_minimum(C_word size);
C_fctexport int C_fcall C_save_callback_continuation(C_word **ptr, C_word k);
C_fctexport C_word C_fcall C_restore_callback_continuation(void);
C_fctexport C_word C_fcall C_callback(C_word closure, int argc);
C_fctexport C_word C_fcall C_callback_wrapper(void *proc, int argc);
C_fctexport void C_fcall C_callback_adjust_stack_limits(C_word *base);
C_fctexport void CHICKEN_parse_command_line(int argc, char *argv[], C_word *heap, C_word *stack, C_word *symbols);
C_fctexport void C_fcall C_toplevel_entry(C_char *name) C_regparm;
C_fctexport C_word C_fcall C_enable_interrupts(void) C_regparm;
C_fctexport C_word C_fcall C_disable_interrupts(void) C_regparm;
C_fctexport void C_fcall C_paranoid_check_for_interrupt(void) C_regparm;
C_fctexport double C_fcall C_c_double(C_word x) C_regparm;
C_fctexport C_word C_fcall C_num_to_int(C_word x) C_regparm;
C_fctexport C_s64 C_fcall C_num_to_int64(C_word x) C_regparm;
C_fctexport C_uword C_fcall C_num_to_unsigned_int(C_word x) C_regparm;
C_fctexport C_word C_fcall C_int_to_num(C_word **ptr, C_word n) C_regparm;
C_fctexport C_word C_fcall C_unsigned_int_to_num(C_word **ptr, C_uword n) C_regparm;
C_fctexport C_word C_fcall C_long_to_num(C_word **ptr, long n) C_regparm;
C_fctexport C_word C_fcall C_unsigned_long_to_num(C_word **ptr, unsigned long n) C_regparm;
C_fctexport long C_fcall C_num_to_long(C_word x) C_regparm;
C_fctexport unsigned long C_fcall C_num_to_unsigned_long(C_word x) C_regparm;
C_fctexport C_word C_fcall C_flonum_in_int_range_p(C_word n) C_regparm;
C_fctexport C_word C_fcall C_flonum_in_uint_range_p(C_word n) C_regparm;
C_fctexport C_word C_fcall C_double_to_number(C_word n) C_regparm;
C_fctexport char *C_fcall C_string_or_null(C_word x) C_regparm;
C_fctexport void *C_fcall C_data_pointer_or_null(C_word x) C_regparm;
C_fctexport void *C_fcall C_srfi_4_vector_or_null(C_word x) C_regparm;
C_fctexport void *C_fcall C_c_pointer_or_null(C_word x) C_regparm;
C_fctexport void *C_fcall C_scheme_or_c_pointer(C_word x) C_regparm;
C_fctexport C_word C_fcall C_flonum_in_fixnum_range_p(C_word n) C_regparm;
C_fctexport void C_zap_strings(C_word str);
C_fctexport void C_set_or_change_heap_size(C_word heap, int reintern);
C_fctexport void C_do_resize_stack(C_word stack);
C_fctexport C_word C_resize_pending_finalizers(C_word size);
C_fctexport void C_initialize_lf(C_word *lf, int count);
C_fctexport void *C_register_lf(C_word *lf, int count);
C_fctexport void *C_register_lf2(C_word *lf, int count, C_PTABLE_ENTRY *ptable);
C_fctexport void C_unregister_lf(void *handle);
C_fctexport C_char *C_dump_trace(int start);
C_fctexport void C_fcall C_clear_trace_buffer(void) C_regparm;
C_fctexport C_word C_fetch_trace(C_word start, C_word buffer);
C_fctexport C_word C_fcall C_string(C_word **ptr, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_static_string(C_word **ptr, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_static_lambda_info(C_word **ptr, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_bytevector(C_word **ptr, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_pbytevector(int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_string_aligned8(C_word **ptr, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_string2(C_word **ptr, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_string2_safe(C_word **ptr, int max, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_intern(C_word **ptr, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_intern_in(C_word **ptr, int len, C_char *str, C_SYMBOL_TABLE *stable) C_regparm;
C_fctexport C_word C_fcall C_h_intern(C_word *slot, int len, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_h_intern_in(C_word *slot, int len, C_char *str, C_SYMBOL_TABLE *stable) C_regparm;
C_fctexport C_word C_fcall C_intern2(C_word **ptr, C_char *str) C_regparm;
C_fctexport C_word C_fcall C_intern3(C_word **ptr, C_char *str, C_word value) C_regparm;
C_fctexport C_word C_fcall C_restore_rest(C_word *ptr, int num) C_regparm;
C_fctexport C_word C_fcall C_restore_rest_vector(C_word *ptr, int num) C_regparm;
C_fctexport void C_bad_memory(void) C_noret;
C_fctexport void C_bad_memory_2(void) C_noret;
C_fctexport void C_bad_argc(int c, int n) C_noret;
C_fctexport void C_bad_min_argc(int c, int n) C_noret;
C_fctexport void C_bad_argc_2(int c, int n, C_word closure) C_noret;
C_fctexport void C_bad_min_argc_2(int c, int n, C_word closure) C_noret;
C_fctexport void C_stack_overflow(void) C_noret;
C_fctexport void C_unbound_error(C_word sym) C_noret;
C_fctexport void C_no_closure_error(C_word x) C_noret;
C_fctexport C_word C_closure(C_word **ptr, int cells, C_word proc, ...);
C_fctexport C_word C_fcall C_pair(C_word **ptr, C_word car, C_word cdr) C_regparm;
C_fctexport C_word C_fcall C_h_pair(C_word car, C_word cdr) C_regparm;
C_fctexport C_word C_fcall C_flonum(C_word **ptr, double n) C_regparm;
C_fctexport C_word C_fcall C_number(C_word **ptr, double n) C_regparm;
C_fctexport C_word C_fcall C_mpointer(C_word **ptr, void *mp) C_regparm;
C_fctexport C_word C_fcall C_mpointer_or_false(C_word **ptr, void *mp) C_regparm;
C_fctexport C_word C_fcall C_mpointer(C_word **ptr, void *mp) C_regparm;
C_fctexport C_word C_fcall C_mpointer_or_false(C_word **ptr, void *mp) C_regparm;
C_fctexport C_word C_fcall C_taggedmpointer(C_word **ptr, C_word tag, void *mp) C_regparm;
C_fctexport C_word C_fcall C_taggedmpointer_or_false(C_word **ptr, C_word tag, void *mp) C_regparm;
C_fctexport C_word C_fcall C_swigmpointer(C_word **ptr, void *mp, void *sdata) C_regparm;
C_fctexport C_word C_vector(C_word **ptr, int n, ...);
C_fctexport C_word C_h_vector(int n, ...);
C_fctexport C_word C_structure(C_word **ptr, int n, ...);
C_fctexport C_word C_h_structure(int n, ...);
C_fctexport C_word C_fcall C_mutate(C_word *slot, C_word val) C_regparm;
C_fctexport void C_fcall C_reclaim(void *trampoline, void *proc) C_regparm C_noret;
C_fctexport void C_save_and_reclaim(void *trampoline, void *proc, int n, ...) C_noret;
C_fctexport void C_fcall C_rereclaim(long size) C_regparm; /* deprecated */
C_fctexport void C_fcall C_rereclaim2(C_uword size, int double_plus) C_regparm;
C_fctexport C_word C_fcall C_retrieve(C_word sym) C_regparm;
C_fctexport C_word C_fcall C_retrieve2(C_word val, char *name) C_regparm;
C_fctexport void *C_fcall C_retrieve_proc(C_word closure) C_regparm;
C_fctexport C_word C_fcall C_permanentp(C_word x) C_regparm;
C_fctexport int C_in_stackp(C_word x) C_regparm;
C_fctexport int C_fcall C_in_heapp(C_word x) C_regparm;
C_fctexport int C_fcall C_in_fromspacep(C_word x) C_regparm;
C_fctexport void C_fcall C_trace(C_char *name) C_regparm;
C_fctexport C_word C_fcall C_emit_trace_info(C_word x, C_word y, C_word t) C_regparm;
C_fctexport C_word C_fcall C_emit_trace_info2(char *raw, C_word x, C_word y, C_word t) C_regparm;
C_fctexport C_word C_fcall C_hash_string(C_word str) C_regparm;
C_fctexport C_word C_fcall C_hash_string_ci(C_word str) C_regparm;
C_fctexport C_word C_halt(C_word msg);
C_fctexport C_word C_message(C_word msg);
C_fctexport C_word C_fcall C_equalp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_set_gc_report(C_word flag) C_regparm;
C_fctexport C_word C_fcall C_start_timer(void) C_regparm;
C_fctexport C_word C_exit_runtime(C_word code);
C_fctexport C_word C_fcall C_display_flonum(C_word port, C_word n) C_regparm;
C_fctexport C_word C_fcall C_read_char(C_word port) C_regparm;
C_fctexport C_word C_fcall C_peek_char(C_word port) C_regparm;
C_fctexport C_word C_fcall C_execute_shell_command(C_word string) C_regparm;
C_fctexport C_word C_fcall C_char_ready_p(C_word port) C_regparm;
C_fctexport C_word C_fcall C_flush_output(C_word port) C_regparm;
C_fctexport C_word C_fcall C_fudge(C_word fudge_factor) C_regparm;
C_fctexport void C_fcall C_raise_interrupt(int reason) C_regparm;
C_fctexport C_word C_fcall C_set_initial_timer_interrupt_period(C_word n) C_regparm;
C_fctexport C_word C_fcall C_establish_signal_handler(C_word signum, C_word reason) C_regparm;
C_fctexport C_word C_fcall C_fits_in_int_p(C_word x) C_regparm;
C_fctexport C_word C_fcall C_fits_in_unsigned_int_p(C_word x) C_regparm;
C_fctexport C_word C_fcall C_copy_block(C_word from, C_word to) C_regparm;
C_fctexport C_word C_fcall C_evict_block(C_word from, C_word ptr) C_regparm;
C_fctexport void C_fcall C_gc_protect(C_word **addr, int n) C_regparm;
C_fctexport void C_fcall C_gc_unprotect(int n) C_regparm;
C_fctexport C_SYMBOL_TABLE *C_new_symbol_table(char *name, unsigned int size) C_regparm;
C_fctexport void C_delete_symbol_table(C_SYMBOL_TABLE *st) C_regparm;
C_fctexport void C_set_symbol_table(C_SYMBOL_TABLE *st) C_regparm;
C_fctexport C_SYMBOL_TABLE *C_find_symbol_table(char *name) C_regparm;
C_fctexport C_word C_find_symbol(C_word str, C_SYMBOL_TABLE *stable) C_regparm;
C_fctexport C_word C_fcall C_lookup_symbol(C_word sym) C_regparm;
C_fctexport C_word C_enumerate_symbols(C_SYMBOL_TABLE *stable, C_word pos) C_regparm;
C_fctexport void C_do_register_finalizer(C_word x, C_word proc);
C_fctexport int C_do_unregister_finalizer(C_word x);
C_fctexport C_word C_dbg_hook(C_word x);

C_fctimport void C_ccall C_toplevel(C_word c, C_word self, C_word k) C_noret;
C_fctexport void C_ccall C_stop_timer(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_apply(C_word c, C_word closure, C_word k, C_word fn, ...) C_noret;
C_fctexport void C_ccall C_do_apply(C_word n, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_call_cc(C_word c, C_word closure, C_word k, C_word cont) C_noret;
C_fctexport void C_ccall C_continuation_graft(C_word c, C_word closure, C_word k, C_word kk, C_word proc) C_noret;
C_fctexport void C_ccall C_values(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_apply_values(C_word c, C_word closure, C_word k, C_word lst) C_noret;
C_fctexport void C_ccall C_call_with_values(C_word c, C_word closure, C_word k, C_word thunk, C_word kont) C_noret;
C_fctexport void C_ccall C_u_call_with_values(C_word c, C_word closure, C_word k, C_word thunk, C_word kont) C_noret;
C_fctexport void C_ccall C_times(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_plus(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_minus(C_word c, C_word closure, C_word k, C_word n1, ...) C_noret;
C_fctexport void C_ccall C_divide(C_word c, C_word closure, C_word k, C_word n1, ...) C_noret;
C_fctexport void C_ccall C_nequalp(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_greaterp(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_lessp(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_greater_or_equal_p(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_less_or_equal_p(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_expt(C_word c, C_word closure, C_word k, C_word n1, C_word n2) C_noret;
C_fctexport void C_ccall C_gc(C_word c, C_word closure, C_word k, ...) C_noret;
C_fctexport void C_ccall C_open_file_port(C_word c, C_word closure, C_word k, C_word port, C_word channel, C_word mode) C_noret;
C_fctexport void C_ccall C_allocate_vector(C_word c, C_word closure, C_word k, C_word size, C_word type, C_word init, C_word align8) C_noret;
C_fctexport void C_ccall C_string_to_symbol(C_word c, C_word closure, C_word k, C_word string) C_noret;
C_fctexport void C_ccall C_build_symbol(C_word c, C_word closure, C_word k, C_word string) C_noret;
C_fctexport void C_ccall C_cons_flonum(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_flonum_fraction(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_exact_to_inexact(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_flonum_floor(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_flonum_ceiling(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_flonum_truncate(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_flonum_round(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_quotient(C_word c, C_word closure, C_word k, C_word n1, C_word n2) C_noret;
C_fctexport void C_ccall C_string_to_number(C_word c, C_word closure, C_word k, C_word str, ...) C_noret;
C_fctexport void C_ccall C_number_to_string(C_word c, C_word closure, C_word k, C_word num, ...) C_noret;
C_fctexport void C_ccall C_get_argv(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_make_structure(C_word c, C_word closure, C_word k, C_word type, ...) C_noret;
C_fctexport void C_ccall C_make_symbol(C_word c, C_word closure, C_word k, C_word name) C_noret;
C_fctexport void C_ccall C_make_pointer(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_make_tagged_pointer(C_word c, C_word closure, C_word k, C_word tag) C_noret;
C_fctexport void C_ccall C_ensure_heap_reserve(C_word c, C_word closure, C_word k, C_word n) C_noret;
C_fctexport void C_ccall C_return_to_host(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_file_info(C_word c, C_word closure, C_word k, C_word port) C_noret;
C_fctexport void C_ccall C_get_environment_variable(C_word c, C_word closure, C_word k, C_word name) C_noret;
C_fctexport void C_ccall C_get_symbol_table_info(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_get_memory_info(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_context_switch(C_word c, C_word closure, C_word k, C_word state) C_noret;
C_fctexport void C_ccall C_peek_signed_integer(C_word c, C_word closure, C_word k, C_word v, C_word index) C_noret;
C_fctexport void C_ccall C_peek_unsigned_integer(C_word c, C_word closure, C_word k, C_word v, C_word index) C_noret;
C_fctexport void C_ccall C_decode_seconds(C_word c, C_word closure, C_word k, C_word secs, C_word mode) C_noret;
C_fctexport void C_ccall C_software_type(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_machine_type(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_software_version(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_build_platform(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_c_runtime(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_register_finalizer(C_word c, C_word closure, C_word k, C_word x, C_word proc) C_noret;
C_fctexport void C_ccall C_set_dlopen_flags(C_word c, C_word closure, C_word k, C_word now, C_word global) C_noret;
C_fctexport void C_ccall C_dload(C_word c, C_word closure, C_word k, C_word name, C_word entry, C_word reloadable) C_noret;
C_fctexport void C_ccall C_become(C_word c, C_word closure, C_word k, C_word table) C_noret;
C_fctexport void C_ccall C_cpu_time(C_word c, C_word closure, C_word k) C_noret;
C_fctexport void C_ccall C_locative_ref(C_word c, C_word closure, C_word k, C_word loc) C_noret;
C_fctexport void C_ccall C_call_with_cthulhu(C_word c, C_word self, C_word k, C_word proc) C_noret;
C_fctexport void C_ccall C_copy_closure(C_word c, C_word closure, C_word k, C_word proc) C_noret;

#if !defined(__GNUC__) && !defined(__INTEL_COMPILER)
C_fctexport C_word *C_a_i(C_word **a, int n);
#endif

C_fctexport time_t C_fcall C_seconds(long *ms) C_regparm;
C_fctexport C_word C_a_i_list(C_word **a, int c, ...);
C_fctexport C_word C_h_list(int c, ...);
C_fctexport C_word C_a_i_string(C_word **a, int c, ...);
C_fctexport C_word C_a_i_record(C_word **a, int c, ...);
C_fctexport C_word C_a_i_port(C_word **a, int c);
C_fctexport C_word C_fcall C_a_i_bytevector(C_word **a, int c, C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_eqvp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_symbolp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_pairp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_vectorp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_closurep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_portp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_stringp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_numberp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_integerp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_flonump(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_finitep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_locativep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_fixnum_min(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_fixnum_max(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_flonum_min(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_flonum_max(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_a_i_abs(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_i_listp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_string_equal_p(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_string_ci_equal_p(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_u_i_string_equal_p(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_set_car(C_word p, C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_set_cdr(C_word p, C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_vector_set(C_word v, C_word i, C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_exactp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_exactp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_inexactp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_inexactp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_zerop(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_zerop(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_positivep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_positivep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_negativep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_negativep(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_car(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_cdr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_cadr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_cddr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_caddr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_cdddr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_cadddr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_cddddr(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_list_tail(C_word lst, C_word i) C_regparm;
C_fctexport C_word C_fcall C_i_evenp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_evenp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_oddp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_u_i_oddp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_vector_ref(C_word v, C_word i) C_regparm;
C_fctexport C_word C_fcall C_i_block_ref(C_word x, C_word i) C_regparm;
C_fctexport C_word C_fcall C_i_string_set(C_word s, C_word i, C_word c) C_regparm;
C_fctexport C_word C_fcall C_i_string_ref(C_word s, C_word i) C_regparm;
C_fctexport C_word C_fcall C_i_vector_length(C_word v) C_regparm;
C_fctexport C_word C_fcall C_i_string_length(C_word s) C_regparm;
C_fctexport C_word C_fcall C_i_assq(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_u_i_assq(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_assv(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_assoc(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_memq(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_u_i_memq(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_memv(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_member(C_word x, C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_length(C_word lst) C_regparm;
C_fctexport C_word C_fcall C_i_inexact_to_exact(C_word n) C_regparm;
C_fctexport C_word C_fcall C_i_check_exact_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_number_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_string_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_bytevector_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_symbol_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_list_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_pair_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_vector_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_structure_2(C_word x, C_word st, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_i_check_char_2(C_word x, C_word loc) C_regparm;
C_fctexport C_word C_fcall C_2_times(C_word **ptr, C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_2_plus(C_word **ptr, C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_2_minus(C_word **ptr, C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_2_divide(C_word **ptr, C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_nequalp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_greaterp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_lessp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_greater_or_equalp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_less_or_equalp(C_word x, C_word y) C_regparm;
C_fctexport C_word C_fcall C_i_not_pair_p_2(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_null_list_p(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_string_null_p(C_word x) C_regparm;
C_fctexport C_word C_fcall C_string_to_pbytevector(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_null_pointerp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_fixnum_arithmetic_shift(C_word n, C_word c) C_regparm; 
C_fctexport C_word C_fcall C_i_locative_set(C_word loc, C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_locative_to_object(C_word loc) C_regparm;
C_fctexport C_word C_fcall C_a_i_make_locative(C_word **a, int c, C_word type, C_word object, C_word index, C_word weak) C_regparm;
C_fctexport C_word C_fcall C_a_i_flonum_plus(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_flonum_difference(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_flonum_times(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_flonum_quotient(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_flonum_negate(C_word **a, int c, C_word n1) C_regparm;
C_fctexport C_word C_fcall C_a_i_bitwise_and(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_bitwise_ior(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_bitwise_not(C_word **a, int c, C_word n1) C_regparm;
C_fctexport C_word C_fcall C_i_bit_setp(C_word n, C_word i) C_regparm;
C_fctexport C_word C_fcall C_a_i_bitwise_xor(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_arithmetic_shift(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_exp(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_log(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_sin(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_cos(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_tan(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_asin(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_acos(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_atan(C_word **a, int c, C_word n) C_regparm;
C_fctexport C_word C_fcall C_a_i_atan2(C_word **a, int c, C_word n1, C_word n2) C_regparm;
C_fctexport C_word C_fcall C_a_i_sqrt(C_word **a, int c, C_word n) C_regparm;

C_fctexport C_word C_fcall C_i_foreign_char_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_fixnum_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_flonum_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_block_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_number_vector_argumentp(C_word t, C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_string_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_symbol_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_tagged_pointer_argumentp(C_word x, C_word t) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_pointer_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_scheme_or_c_pointer_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_integer_argumentp(C_word x) C_regparm;
C_fctexport C_word C_fcall C_i_foreign_unsigned_integer_argumentp(C_word x) C_regparm;

C_fctexport C_char *C_lookup_procedure_id(void *ptr);
C_fctexport void *C_lookup_procedure_ptr(C_char *id);
C_fctexport C_word C_dunload(C_word name);

#ifdef C_SIXTY_FOUR
C_fctexport void C_ccall C_peek_signed_integer_32(C_word c, C_word closure, C_word k, C_word v, C_word index) C_noret;
C_fctexport void C_ccall C_peek_unsigned_integer_32(C_word c, C_word closure, C_word k, C_word v, C_word index) C_noret;
#else
# define C_peek_signed_integer_32    C_peek_signed_integer
# define C_peek_unsigned_integer_32  C_peek_unsigned_integer
#endif

/* defined in eval.scm: */
C_fctexport  void  CHICKEN_get_error_message(char *buf,int bufsize);
C_fctexport  int  CHICKEN_load(char * filename);
C_fctexport  int  CHICKEN_read(char * str,C_word *result);
C_fctexport  int  CHICKEN_apply_to_string(C_word func,C_word args,char *buf,int bufsize);
C_fctexport  int  CHICKEN_apply(C_word func,C_word args,C_word *result);
C_fctexport  int  CHICKEN_eval_string_to_string(char *str,char *buf,int bufsize);
C_fctexport  int  CHICKEN_eval_to_string(C_word exp,char *buf,int bufsize);
C_fctexport  int  CHICKEN_eval_string(char * str,C_word *result);
C_fctexport  int  CHICKEN_eval(C_word exp,C_word *result);
C_fctexport  int  CHICKEN_yield();

C_fctexport void C_default_stub_toplevel(C_word c,C_word d,C_word k) C_noret;

C_END_C_DECLS

#endif /* ___CHICKEN */
