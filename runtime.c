/* runtime.c - Runtime code for compiler generated executables
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


#include "chicken.h"
#include <errno.h>
#include <signal.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

#ifdef HAVE_SYSEXITS_H
# include <sysexits.h>
#endif

#if defined(C_NO_PIC_NO_DLL) && !defined(PIC)
# define NO_DLOAD2
#endif

#ifndef NO_DLOAD2
# ifdef HAVE_DLFCN_H
#  include <dlfcn.h>
# endif

# ifdef HAVE_DL_H
#  include <dl.h>
# endif
#endif

#ifndef EX_SOFTWARE
# define EX_SOFTWARE  70
#endif

#ifdef __WATCOMC__
# define NSIG (_SIGMAX - _SIGMIN + 1)
#endif

#if defined(__MWERKS__) && !defined(__INTEL__)

# include <stat.h>
# ifndef MACINTOSH_GUI
#  include <console.h>
#  include <SIOUX.h>
# endif

# define NSIG                          32
# define EINVAL                        ERANGE
static C_TLS int timezone;

#elif !defined(C_NONUNIX)

# include <sys/types.h>
# include <sys/stat.h>
# include <sys/time.h>
# include <sys/resource.h>
# include <sys/wait.h>

#else

# include <sys/types.h>
# include <sys/stat.h>

#ifdef ECOS
#include <cyg/kernel/kapi.h>
static C_TLS int timezone;
#define NSIG                          32
#endif

#endif

#if defined(HAVE_FFI_H)
# include <ffi.h>
#endif

#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL                   0
#endif

#ifndef RTLD_NOW
# define RTLD_NOW                      0
#endif

#ifndef RTLD_LOCAL
# define RTLD_LOCAL                    0
#endif

#ifndef RTLD_LAZY
# define RTLD_LAZY                     0
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

#ifdef HAVE_CONFIG_H
# ifdef PACKAGE
#  undef PACKAGE
# endif
# ifdef VERSION
#  undef VERSION
# endif
# include <chicken-config.h>

# ifndef HAVE_ALLOCA
#  error this package requires "alloca()"
# endif
#endif

#ifdef _MSC_VER
# define S_IFMT             _S_IFMT
# define S_IFDIR            _S_IFDIR
# define timezone           _timezone

# ifdef _M_IX86
#  define HACKED_APPLY
# endif

#endif


/* Parameters: */

#define RELAX_MULTIVAL_CHECK

#define DEFAULT_STACK_SIZE             64000
#define DEFAULT_SYMBOL_TABLE_SIZE      2999
#define DEFAULT_HEAP_SIZE              500000
#define MINIMAL_HEAP_SIZE              500000
#define DEFAULT_MAXIMAL_HEAP_SIZE      0x7ffffff0
#define DEFAULT_HEAP_GROWTH            200
#define DEFAULT_HEAP_SHRINKAGE         50
#define DEFAULT_HEAP_SHRINKAGE_USED    25
#define DEFAULT_FORWARDING_TABLE_SIZE  32
#define DEFAULT_LOCATIVE_TABLE_SIZE    32
#define DEFAULT_COLLECTIBLES_SIZE      1024
#define DEFAULT_TRACE_BUFFER_SIZE      16

#define MAX_HASH_PREFIX                64

#define WEAK_TABLE_SIZE                997
#define WEAK_HASH_ITERATIONS           4
#define WEAK_HASH_DISPLACEMENT         7
#define WEAK_COUNTER_MASK              3
#define WEAK_COUNTER_MAX               2

#define TEMPORARY_STACK_SIZE	       2048
#define STRING_BUFFER_SIZE             4096
#define DEFAULT_MUTATION_STACK_SIZE    1024
#define MUTATION_STACK_GROWTH          1024

#define FILE_INFO_SIZE                 7

#ifdef C_DOUBLE_IS_32_BITS
# define FLONUM_PRINT_PRECISION         7
#else
# define FLONUM_PRINT_PRECISION         15
#endif

#define WORDS_PER_FLONUM              C_SIZEOF_FLONUM

#define MAXIMAL_NUMBER_OF_COMMAND_LINE_ARGUMENTS 32

#define INITIAL_TIMER_INTERRUPT_PERIOD 10000


/* Constants: */

#ifdef C_SIXTY_FOUR
# define ALIGNMENT_HOLE_MARKER         ((C_word)0xfffffffffffffffeL)
# define FORWARDING_BIT_SHIFT          63
# define UWORD_FORMAT_STRING           "0x%lx"
#else
# define ALIGNMENT_HOLE_MARKER         ((C_word)0xfffffffe)
# define FORWARDING_BIT_SHIFT          31
# define UWORD_FORMAT_STRING           "0x%x"
#endif

#define GC_MINOR           0
#define GC_MAJOR           1
#define GC_REALLOC         2


/* Macros: */

#ifdef PARANOIA
# define check_alignment(p)           assert(((C_word)(p) & 3) == 0)
#else
# ifndef NDEBUG
#  define NDEBUG
# endif
# define check_alignment(p)
#endif

#define aligned8(n)                  ((((C_word)(n)) & 7) == 0)
#define nmax(x, y)                   ((x) > (y) ? (x) : (y))
#define nmin(x, y)                   ((x) < (y) ? (x) : (y))
#define percentage(n, p)             ((long)(((double)(n) * (double)p) / 100))

#define is_fptr(x)                   (((x) & C_GC_FORWARDING_BIT) != 0)
#define ptr_to_fptr(x)               ((((x) >> FORWARDING_BIT_SHIFT) & 1) | C_GC_FORWARDING_BIT | ((x) & ~1))
#define fptr_to_ptr(x)               (((x) << FORWARDING_BIT_SHIFT) | ((x) & ~(C_GC_FORWARDING_BIT | 1)))

#ifdef C_UNSAFE_RUNTIME
# define C_check_flonum(x, w)
# define C_check_real(x, w, v)       if(((x) & C_FIXNUM_BIT) != 0) v = C_unfix(x); \
                                     else v = C_flonum_magnitude(x);
# define resolve_procedure(x, w)     (x)
#else
# define C_check_flonum(x, w)        if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR, w, x);
# define C_check_real(x, w, v)       if(((x) & C_FIXNUM_BIT) != 0) v = C_unfix(x); \
                                     else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                     else v = C_flonum_magnitude(x);
#endif

#define C_isnan(f)                   (!((f) == (f)))
#define C_isinf(f)                   ((f) == (f) + (f) && (f) != 0.0)


/* these could be shorter in unsafe mode: */
#define C_check_int(x, f, n, w)     if(((x) & C_FIXNUM_BIT) != 0) n = C_unfix(x); \
                                     else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                     else { double _m; \
                                       f = C_flonum_magnitude(x); \
                                       if(modf(f, &_m) != 0.0 || f < C_WORD_MIN || f > C_WORD_MAX) \
                                         barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, w, x); \
                                       else n = (C_word)f; \
                                     }

#define C_check_uint(x, f, n, w)    if(((x) & C_FIXNUM_BIT) != 0) n = C_unfix(x); \
                                     else if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG) \
                                       barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, w, x); \
                                     else { double _m; \
                                       f = C_flonum_magnitude(x); \
                                       if(modf(f, &_m) != 0.0 || f < 0 || f > C_UWORD_MAX) \
                                         barf(C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR, w, x); \
                                       else n = (C_uword)f; \
                                     }

#ifdef C_SIXTY_FOUR
#define C_limit_fixnum(n)            ((n) & C_MOST_POSITIVE_FIXNUM)
#else
#define C_limit_fixnum(n)            (n)
#endif

#define C_pte(name)                  pt[ i ].id = #name; pt[ i++ ].ptr = (void *)name;


/* Type definitions: */

typedef void (*TOPLEVEL)(C_word c, C_word self, C_word k) C_noret;
typedef void (C_fcall *TRAMPOLINE)(void *proc) C_regparm C_noret;

typedef struct lf_list_struct
{
  C_word *lf;
  int count;
  struct lf_list_struct *next, *prev;
  C_PTABLE_ENTRY *ptable;
  void *module_handle;
  char *module_name;
} LF_LIST;

typedef struct weak_table_entry_struct
{
  C_word item,
         container;
} WEAK_TABLE_ENTRY;

typedef struct finalizer_node_struct
{
  struct finalizer_node_struct
    *next,
    *previous;
  C_word
    item,
    finalizer;
} FINALIZER_NODE;

typedef struct trace_info_struct
{
  C_char *raw;
  C_word cooked1, cooked2, thread;
} TRACE_INFO;


/* Variables: */

C_TLS C_word
  *C_temporary_stack,
  *C_temporary_stack_bottom,
  *C_temporary_stack_limit,
  *C_stack_limit;
C_TLS long
  C_timer_interrupt_counter,
  C_initial_timer_interrupt_period;
C_TLS C_byte 
  *C_fromspace_top,
  *C_fromspace_limit;
C_TLS double C_temporary_flonum;
C_TLS jmp_buf C_restart;
C_TLS void *C_restart_address;
C_TLS int C_entry_point_status;
C_TLS int (*C_gc_mutation_hook)(C_word *slot, C_word val);
C_TLS void (*C_gc_trace_hook)(C_word *var, int mode);
C_TLS C_word(*C_get_unbound_variable_value_hook)(C_word sym);

C_TLS void (C_fcall *C_restart_trampoline)(void *proc) C_regparm C_noret;
C_TLS void (*C_post_gc_hook)(int mode);

C_TLS int
  C_abort_on_thread_exceptions,
  C_enable_repl,
  C_interrupts_enabled,
  C_disable_overflow_check,
#ifdef C_COLLECT_ALL_SYMBOLS
  C_enable_gcweak = 1,
#else
  C_enable_gcweak = 0,
#endif
  C_heap_size_is_fixed,
  C_trace_buffer_size = DEFAULT_TRACE_BUFFER_SIZE,
  C_max_pending_finalizers = C_DEFAULT_MAX_PENDING_FINALIZERS,
  C_main_argc;
C_TLS C_uword 
  C_heap_growth,
  C_heap_shrinkage;
C_TLS C_uword C_maximal_heap_size;
C_TLS time_t C_startup_time_seconds;

C_TLS char 
  **C_main_argv,
  *C_dlerror;

static C_TLS TRACE_INFO
  *trace_buffer,
  *trace_buffer_limit,
  *trace_buffer_top;

static C_TLS C_byte 
  *fromspace_start,
  *tospace_start,
  *tospace_top,
  *tospace_limit,
  *new_tospace_start,
  *new_tospace_top,
  *new_tospace_limit,
  *heap_scan_top,
  *timer_start_fromspace_top;
static C_TLS C_char 
  buffer[ STRING_BUFFER_SIZE ],
  *current_module_name,
  *save_string;
static C_TLS C_SYMBOL_TABLE
  *symbol_table,
  *symbol_table_list;
static C_TLS C_word 
  **collectibles,
  **collectibles_top,
  **collectibles_limit,
  *saved_stack_limit,
  **mutation_stack_bottom,
  **mutation_stack_limit,
  **mutation_stack_top,
  *stack_bottom,
  *locative_table,
  error_location,
  interrupt_hook_symbol,
  current_thread_symbol,
  error_hook_symbol,
  invalid_procedure_call_hook_symbol,
  unbound_variable_value_hook_symbol,
  last_invalid_procedure_symbol,
  identity_unbound_value_symbol,
  apply_hook_symbol,
  last_applied_procedure_symbol,
  pending_finalizers_symbol,
  callback_continuation_stack_symbol,
  *forwarding_table;
static C_TLS int 
  trace_buffer_full,
  forwarding_table_size,
  return_to_host,
  show_trace,
  fake_tty_flag,
  debug_mode,
  gc_bell,
  gc_report_flag,
  gc_mode,
  gc_count_1,
  gc_count_2,
  timer_start_gc_count_1,
  timer_start_gc_count_2,
  interrupt_reason,
  stack_size_changed,
  dlopen_flags,
  heap_size_changed,
  chicken_is_running,
  chicken_ran_once;
static C_TLS unsigned int
  mutation_count,
  stack_size,
  heap_size,
  timer_start_mutation_count;
static C_TLS int chicken_is_initialized;
static C_TLS jmp_buf gc_restart;
static C_TLS long
  timer_start_ms,
  timer_start_gc_ms,
  timer_accumulated_gc_ms,
  interrupt_time,
  last_interrupt_latency;
static C_TLS LF_LIST 
  *lf_list,
  *reload_lf;
static C_TLS int signal_mapping_table[ NSIG ];
static C_TLS int
  locative_table_size,
  locative_table_count,
  live_finalizer_count,
  allocated_finalizer_count,
  pending_finalizer_count,
  callback_returned_flag;
static C_TLS WEAK_TABLE_ENTRY *weak_item_table;
static C_TLS C_GC_ROOT *gc_root_list;
static C_TLS FINALIZER_NODE 
  *finalizer_list,
  *finalizer_free_list,
  **pending_finalizer_indices;
static C_TLS void *current_module_handle;


/* Prototypes: */

static void parse_argv(C_char *cmds);
static void initialize_symbol_table(void);
static void global_signal_handler(int signum);
static C_word arg_val(C_char *arg);
static void barf(int code, char *loc, ...) C_noret;
static void panic(C_char *msg) C_noret;
static void horror(C_char *msg) C_noret;
static void C_fcall initial_trampoline(void *proc) C_regparm C_noret;
static C_ccall void termination_continuation(C_word c, C_word self, C_word result) C_noret;
static void C_fcall mark_system_globals(void) C_regparm;
static void C_fcall mark(C_word *x) C_regparm;
static WEAK_TABLE_ENTRY *C_fcall lookup_weak_table_entry(C_word item, C_word container) C_regparm;
static C_ccall void values_continuation(C_word c, C_word closure, C_word dummy, ...) C_noret;
static C_word add_symbol(C_word **ptr, C_word key, C_word string, C_SYMBOL_TABLE *stable);
static int C_fcall hash_string(int len, C_char *str, unsigned int m) C_regparm;
static C_word C_fcall lookup(C_word key, int len, C_char *str, C_SYMBOL_TABLE *stable) C_regparm;
static double compute_symbol_table_load(double *avg_bucket_len, int *total);
static C_word C_fcall convert_string_to_number(C_char *str, int radix, C_word *fix, double *flo) C_regparm;
static long C_fcall milliseconds(void);
static long C_fcall cpu_milliseconds(void);
static void C_fcall remark_system_globals(void) C_regparm;
static void C_fcall remark(C_word *x) C_regparm;
static C_word C_fcall intern0(C_char *name) C_regparm;
static void C_fcall update_locative_table(int mode) C_regparm;
static C_word get_unbound_variable_value(C_word sym);
static LF_LIST *find_module_handle(C_char *name);

static C_ccall void call_cc_wrapper(C_word c, C_word closure, C_word k, C_word result) C_noret;
static C_ccall void call_cc_values_wrapper(C_word c, C_word closure, C_word k, ...) C_noret;
static void cons_flonum_trampoline(void *dummy) C_noret;
static void gc_2(void *dummy) C_noret;
static void allocate_vector_2(void *dummy) C_noret;
static void cons_string_trampoline(void *dummy) C_noret;
static void get_argv_2(void *dummy) C_noret;
static void make_structure_2(void *dummy) C_noret;
static void generic_trampoline(void *dummy) C_noret;
static void file_info_2(void *dummy) C_noret;
static void get_environment_variable_2(void *dummy) C_noret;
static void handle_interrupt(void *trampoline, void *proc) C_noret;
static void callback_trampoline(void *dummy) C_noret;
static C_ccall void callback_return_continuation(C_word c, C_word self, C_word r) C_noret;
static void become_2(void *dummy) C_noret;
static void copy_closure_2(void *dummy) C_noret;

static C_PTABLE_ENTRY *create_initial_ptable();

#if !defined(NO_DLOAD2) &&(defined(HAVE_DLFCN_H) || defined(HAVE_DL_H) || (defined(HAVE_LOADLIBRARY) && defined(HAVE_GETPROCADDRESS)))
static void dload_2(void *dummy) C_noret;
#endif


/* Startup code: */

int CHICKEN_main(int argc, char *argv[], void *toplevel) 
{
  C_word h, s, n;

#if defined(__MWERKS__) && !defined(__INTEL__) && !defined(MACINTOSH_GUI)
  argc = ccommand(&argv);
  SIOUXSettings.asktosaveonclose = 0;
  SIOUXSettings.autocloseonquit = 1;
#elif defined(C_WINDOWS_GUI)
  parse_argv(GetCommandLine());
  argc = C_main_argc;
  argv = C_main_argv;
#endif

  CHICKEN_parse_command_line(argc, argv, &h, &s, &n);
  
  if(!CHICKEN_initialize(h, s, n, toplevel))
    panic(C_text("can not initialize - out of memory"));

  CHICKEN_run(NULL);
  return 0;
}


/* Custom argv parser for Windoze: */

#ifdef C_WINDOWS_GUI
void parse_argv(C_char *cmds)
{
  C_char *ptr = cmds,
         *bptr0, *bptr, *aptr;
  int n = 0;

  C_main_argv = (C_char **)malloc(MAXIMAL_NUMBER_OF_COMMAND_LINE_ARGUMENTS * sizeof(C_char *));

  if(C_main_argv == NULL)
    panic(C_text("can not allocate argument-list buffer"));

  C_main_argc = 0;

  for(;;) {
    while(isspace(*ptr)) ++ptr;

    if(*ptr == '\0') break;

    for(bptr0 = bptr = buffer; !isspace(*ptr) && *ptr != '\0'; *(bptr++) = *(ptr++))
      ++n;
    
    *bptr = '\0';
    aptr = (C_char *)malloc(sizeof(C_char) * (n + 1));
    
    if(aptr == NULL)
      panic(C_text("can not allocate argument buffer"));

    C_strcpy(aptr, bptr0);
    C_main_argv[ C_main_argc++ ] = aptr;
  }
}
#endif


#if defined(__MWERKS__) && !defined(__INTEL__)
int strncasecmp(const C_char *one, const C_char *two, size_t n)
{
  int c;

  while(n--) {
    if((c = C_tolower(*(one++)) - C_tolower(*(two++))) < 0) return -1;
    else if(c > 0) return 1;
  }

  return 0;
}
#endif


/* Initialize runtime system: */

int CHICKEN_initialize(int heap, int stack, int symbols, void *toplevel)
{
  int i;

#ifndef C_NONUNIX
  struct timeval tv;
  C_gettimeofday(&tv, NULL);
  C_startup_time_seconds = tv.tv_sec;
#else
  C_startup_time_seconds = (time_t)0;
#endif

  if(chicken_is_initialized) return 1;
  else chicken_is_initialized = 1;

  if(debug_mode) C_printf(C_text("[debug] application startup...\n"));

  symbol_table_list = NULL;

  if((symbol_table = C_new_symbol_table(".", symbols ? symbols : DEFAULT_SYMBOL_TABLE_SIZE)) == NULL)
    return 0;

  stack_size = stack ? stack : DEFAULT_STACK_SIZE;
  C_set_or_change_heap_size(heap ? heap : DEFAULT_HEAP_SIZE, 0);

  /* Allocate temporary stack: */
  if((C_temporary_stack_limit = (C_word *)C_malloc(TEMPORARY_STACK_SIZE * sizeof(C_word))) == NULL)
    return 0;
  
  C_temporary_stack_bottom = C_temporary_stack_limit + TEMPORARY_STACK_SIZE;
  C_temporary_stack = C_temporary_stack_bottom;
  
  /* Allocate mutation stack: */
  mutation_stack_bottom = (C_word **)C_malloc(DEFAULT_MUTATION_STACK_SIZE * sizeof(C_word *));

  if(mutation_stack_bottom == NULL) return 0;

  mutation_stack_top = mutation_stack_bottom;
  mutation_stack_limit = mutation_stack_bottom + DEFAULT_MUTATION_STACK_SIZE;
  C_gc_mutation_hook = NULL;
  C_gc_trace_hook = NULL;
  C_get_unbound_variable_value_hook = get_unbound_variable_value;

  /* Allocate weak item table: */
  if(C_enable_gcweak) {
    if((weak_item_table = (WEAK_TABLE_ENTRY *)C_calloc(WEAK_TABLE_SIZE, sizeof(WEAK_TABLE_ENTRY))) == NULL)
      return 0;
  }

  /* Initialize finalizer lists: */
  finalizer_list = NULL;
  finalizer_free_list = NULL;
  pending_finalizer_indices = (FINALIZER_NODE **)C_malloc(C_max_pending_finalizers * sizeof(FINALIZER_NODE *));

  if(pending_finalizer_indices == NULL) return 0;

  /* Initialize forwarding table: */
  forwarding_table = (C_word *)C_malloc((DEFAULT_FORWARDING_TABLE_SIZE + 1) * 2 * sizeof(C_word));

  if(forwarding_table == NULL) return 0;
  
  *forwarding_table = 0;
  forwarding_table_size = DEFAULT_FORWARDING_TABLE_SIZE;

  /* Initialize locative table: */
  locative_table = (C_word *)C_malloc(DEFAULT_LOCATIVE_TABLE_SIZE * sizeof(C_word));
   
  if(locative_table == NULL) return 0;
 
  locative_table_size = DEFAULT_LOCATIVE_TABLE_SIZE;
  locative_table_count = 0;

  /* Setup collectibles: */
  collectibles = (C_word **)C_malloc(sizeof(C_word *) * DEFAULT_COLLECTIBLES_SIZE);

  if(collectibles == NULL) return 0;

  collectibles_top = collectibles;
  collectibles_limit = collectibles + DEFAULT_COLLECTIBLES_SIZE;
  gc_root_list = NULL;
 
  /* Initialize global variables: */
  if(C_heap_growth == 0) C_heap_growth = DEFAULT_HEAP_GROWTH;

  if(C_heap_shrinkage == 0) C_heap_shrinkage = DEFAULT_HEAP_SHRINKAGE;

  if(C_maximal_heap_size == 0) C_maximal_heap_size = DEFAULT_MAXIMAL_HEAP_SIZE;

#if !defined(NO_DLOAD2) && defined(HAVE_DLFCN_H)
  dlopen_flags = RTLD_LAZY | RTLD_GLOBAL;
#else
  dlopen_flags = 0;
#endif

  gc_report_flag = 0;
  mutation_count = gc_count_1 = gc_count_2 = 0;
  lf_list = NULL;
  C_register_lf2(NULL, 0, create_initial_ptable());
  C_restart_address = toplevel;
  C_restart_trampoline = initial_trampoline;
  trace_buffer = NULL;
  C_clear_trace_buffer();
  chicken_is_running = chicken_ran_once = 0;
  interrupt_reason = 0;
  last_interrupt_latency = 0;
  C_interrupts_enabled = 1;
  C_initial_timer_interrupt_period = INITIAL_TIMER_INTERRUPT_PERIOD;
  C_timer_interrupt_counter = INITIAL_TIMER_INTERRUPT_PERIOD;
  memset(signal_mapping_table, 0, sizeof(int) * NSIG);
  initialize_symbol_table();
  C_dlerror = "can not load compiled code dynamically - this is a statically linked executable";
  error_location = C_SCHEME_FALSE;
  C_post_gc_hook = NULL;
  live_finalizer_count = 0;
  allocated_finalizer_count = 0;
  current_module_name = NULL;
  current_module_handle = NULL;
  reload_lf = NULL;
  C_randomize(time(NULL));
  return 1;
}


static C_PTABLE_ENTRY *create_initial_ptable()
{
  C_PTABLE_ENTRY *pt = (C_PTABLE_ENTRY *)C_malloc(sizeof(C_PTABLE_ENTRY) * 65);
  int i = 0;

  if(pt == NULL)
    panic(C_text("out of memory - can not create initial ptable"));

  C_pte(termination_continuation);
  C_pte(callback_return_continuation);
  C_pte(values_continuation);
  C_pte(call_cc_values_wrapper);
  C_pte(call_cc_wrapper);
  C_pte(C_gc);
  C_pte(C_allocate_vector);
  C_pte(C_get_argv);
  C_pte(C_make_structure);
  C_pte(C_ensure_heap_reserve);
  C_pte(C_return_to_host);
  C_pte(C_file_info);
  C_pte(C_get_symbol_table_info);
  C_pte(C_get_memory_info);
  C_pte(C_cpu_time);
  C_pte(C_decode_seconds);
  C_pte(C_get_environment_variable);
  C_pte(C_stop_timer);
  C_pte(C_dload);
  C_pte(C_set_dlopen_flags);
  C_pte(C_become);
  C_pte(C_apply_values);
  C_pte(C_times);
  C_pte(C_minus);
  C_pte(C_plus);
  C_pte(C_divide);
  C_pte(C_nequalp);
  C_pte(C_greaterp);
  C_pte(C_lessp);
  C_pte(C_greater_or_equal_p);
  C_pte(C_less_or_equal_p);
  C_pte(C_flonum_floor);
  C_pte(C_flonum_ceiling);
  C_pte(C_flonum_truncate);
  C_pte(C_flonum_round);
  C_pte(C_quotient);
  C_pte(C_cons_flonum);
  C_pte(C_flonum_fraction);
  C_pte(C_expt);
  C_pte(C_exact_to_inexact);
  C_pte(C_string_to_number);
  C_pte(C_number_to_string);
  C_pte(C_make_symbol);
  C_pte(C_string_to_symbol);
  C_pte(C_apply);
  C_pte(C_call_cc);
  C_pte(C_values);
  C_pte(C_call_with_values);
  C_pte(C_continuation_graft);
  C_pte(C_open_file_port);
  C_pte(C_software_type);
  C_pte(C_machine_type);
  C_pte(C_software_version);
  C_pte(C_build_platform);
  C_pte(C_c_runtime);
  C_pte(C_make_pointer);
  C_pte(C_make_tagged_pointer);
  C_pte(C_peek_signed_integer);
  C_pte(C_peek_unsigned_integer);
  C_pte(C_context_switch);
  C_pte(C_register_finalizer);
  C_pte(C_locative_ref);
  C_pte(C_call_with_cthulhu);
  C_pte(C_dunload);
  pt[ i ].id = NULL;
  return pt;
}


void *CHICKEN_new_gc_root()
{
  C_GC_ROOT *r = (C_GC_ROOT *)C_malloc(sizeof(C_GC_ROOT));

  if(r == NULL)
    panic(C_text("out of memory - can not allocate GC root"));

  r->value = C_SCHEME_UNDEFINED;
  r->next = gc_root_list;
  r->prev = NULL;

  if(gc_root_list != NULL) gc_root_list->prev = r;

  gc_root_list = r;
  return (void *)r;
}


void CHICKEN_delete_gc_root(void *root)
{
  C_GC_ROOT *r = (C_GC_ROOT *)root;

  if(r->prev == NULL) gc_root_list = r->next;
  else r->prev->next = r->next;

  if(r->next != NULL) r->next->prev = r->prev;

  C_free(root);
}


void *CHICKEN_global_lookup(char *name)
{
  int 
    len = C_strlen(name),
    key = hash_string(len, name, symbol_table->size);
  C_word s;
  void *root = CHICKEN_new_gc_root();

  if(C_truep(s = lookup(key, len, name, symbol_table))) {
    if(C_u_i_car(s) != C_SCHEME_UNBOUND) {
      CHICKEN_gc_root_set(root, s);
      return root;
    }
  }

  return NULL;
}


int CHICKEN_is_running()
{
  return chicken_is_running;
}


void CHICKEN_interrupt()
{
  C_timer_interrupt_counter = 0;
}


C_regparm C_SYMBOL_TABLE *C_new_symbol_table(char *name, unsigned int size)
{
  C_SYMBOL_TABLE *stp;
  int i;

  if((stp = C_find_symbol_table(name)) != NULL) return stp;

  if((stp = (C_SYMBOL_TABLE *)C_malloc(sizeof(C_SYMBOL_TABLE))) == NULL)
    return NULL;

  stp->name = name;
  stp->size = size;
  stp->next = symbol_table_list;

  if((stp->table = (C_word *)C_malloc(size * sizeof(C_word))) == NULL)
    return NULL;

  for(i = 0; i < stp->size; stp->table[ i++ ] = C_SCHEME_END_OF_LIST);

  symbol_table_list = stp;
  return stp;
}  


C_regparm void C_delete_symbol_table(C_SYMBOL_TABLE *st)
{
  C_SYMBOL_TABLE *stp, *prev = NULL;

  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    if(stp == st) {
      if(prev != NULL) prev->next = stp->next;
      else symbol_table_list = stp->next;

      return;
    }
}


C_regparm void C_set_symbol_table(C_SYMBOL_TABLE *st)
{
  symbol_table = st;
}


C_regparm C_SYMBOL_TABLE *C_find_symbol_table(char *name)
{
  C_SYMBOL_TABLE *stp;

  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    if(!C_strcmp(name, stp->name)) return stp;

  return NULL;
}


C_regparm C_word C_find_symbol(C_word str, C_SYMBOL_TABLE *stable)
{
  char *sptr = C_c_string(str);
  int 
    len = C_header_size(str),
    key = hash_string(len, sptr, stable->size);
  C_word s;

  if(C_truep(s = lookup(key, len, sptr, stable))) return s;
  else return C_SCHEME_FALSE;
}


C_regparm C_word C_enumerate_symbols(C_SYMBOL_TABLE *stable, C_word pos)
{
  int i;
  C_word 
    sym,
    bucket = C_u_i_car(pos);

  if(!C_truep(bucket)) return C_SCHEME_FALSE; /* end already reached */
  else i = C_unfix(bucket);

  bucket = C_u_i_cdr(pos); 

  while(bucket == C_SCHEME_END_OF_LIST) {
    if(++i >= stable->size) {
      C_set_block_item(pos, 0, C_SCHEME_FALSE);	/* no more buckets */
      return C_SCHEME_FALSE;
    }
    else bucket = stable->table[ i ];
  }

  sym = C_u_i_car(bucket);
  C_set_block_item(pos, 0, C_fix(i));
  C_mutate(&C_u_i_cdr(pos), C_u_i_cdr(bucket));
  return sym;
}


/* Setup symbol-table with internally used symbols; */

void initialize_symbol_table(void)
{
  int i;

  for(i = 0; i < symbol_table->size; symbol_table->table[ i++ ] = C_SCHEME_END_OF_LIST);

  /* Obtain reference to hooks for later: */
  interrupt_hook_symbol = C_intern2(C_heaptop, C_text("\003sysinterrupt-hook"));
  error_hook_symbol = C_intern2(C_heaptop, C_text("\003syserror-hook"));
  callback_continuation_stack_symbol = C_intern3(C_heaptop, C_text("\003syscallback-continuation-stack"), C_SCHEME_END_OF_LIST);
  pending_finalizers_symbol = C_intern2(C_heaptop, C_text("\003syspending-finalizers"));
  invalid_procedure_call_hook_symbol = C_intern3(C_heaptop, C_text("\003sysinvalid-procedure-call-hook"), C_SCHEME_FALSE);
  unbound_variable_value_hook_symbol = C_intern3(C_heaptop, C_text("\003sysunbound-variable-value-hook"), C_SCHEME_FALSE);
  last_invalid_procedure_symbol = C_intern3(C_heaptop, C_text("\003syslast-invalid-procedure"), C_SCHEME_FALSE);
  identity_unbound_value_symbol = C_intern3(C_heaptop, C_text("\003sysidentity-unbound-value"), C_SCHEME_FALSE);
  current_thread_symbol = C_intern3(C_heaptop, C_text("\003syscurrent-thread"), C_SCHEME_FALSE);
  apply_hook_symbol = C_intern3(C_heaptop, C_text("\003sysapply-hook"), C_SCHEME_FALSE);
  last_applied_procedure_symbol = C_intern2(C_heaptop, C_text("\003syslast-applied-procedure"));
}


/* This is called from POSIX signals: */

void global_signal_handler(int signum)
{
  C_raise_interrupt(signal_mapping_table[ signum ]);
  signal(signum, global_signal_handler);
}


/* Modify heap size at runtime: */

void C_set_or_change_heap_size(C_word heap, int reintern)
{
  C_byte *ptr1, *ptr2;
  C_word size = heap / 2;

  if(heap_size_changed && fromspace_start) return;

  if(fromspace_start && heap_size >= heap) return;

  if(debug_mode) C_printf(C_text("[debug] heap resized to %d bytes\n"), (int)heap);

  heap_size = heap;

  if((ptr1 = (C_byte *)realloc(fromspace_start, size)) == NULL ||
     (ptr2 = (C_byte *)realloc(tospace_start, size)) == NULL)
    panic(C_text("out of memory - can not allocate heap"));

  fromspace_start = (C_byte *)C_align((C_uword)ptr1);
  C_fromspace_top = fromspace_start;
  C_fromspace_limit = fromspace_start + size;
  tospace_start = (C_byte *)C_align((C_uword)ptr2);
  tospace_top = tospace_start;
  tospace_limit = tospace_start + size;
  mutation_stack_top = mutation_stack_bottom;

  if(reintern) initialize_symbol_table();
}
 

/* Modify stack-size at runtime: */

void C_do_resize_stack(C_word stack)
{
  C_uword old = stack_size,
          diff = stack - old;

  if(diff != 0 && !stack_size_changed) {
    if(debug_mode) C_printf(C_text("[debug] stack resized to %d bytes\n"), (int)stack);

    stack_size = stack;

#if C_STACK_GROWS_DOWNWARD
    C_stack_limit = (C_word *)((C_byte *)C_stack_limit - diff);
#else
    C_stack_limit = (C_word *)((C_byte *)C_stack_limit + diff);
#endif
  }
}


/* Check whether nursery is sufficiently big: */

void C_check_nursery_minimum(C_word words)
{
  if(words >= C_bytestowords(stack_size))
    panic(C_text("nursery is too small - try higher setting using the `-:s' option"));
}

C_word C_resize_pending_finalizers(C_word size) {
  int sz = C_num_to_int(size);

  FINALIZER_NODE **newmem = 
    (FINALIZER_NODE **)C_realloc(pending_finalizer_indices, sz * sizeof(FINALIZER_NODE *));
  
  if (newmem == NULL)
    return C_SCHEME_FALSE;

  pending_finalizer_indices = newmem;
  C_max_pending_finalizers = sz;
  return C_SCHEME_TRUE;
}


/* Parse runtime options from command-line: */

void CHICKEN_parse_command_line(int argc, char *argv[], C_word *heap, C_word *stack, C_word *symbols)
{
  int i;
  char *ptr;
  C_word x;

  C_main_argc = argc;
  C_main_argv = argv;
  *heap = DEFAULT_HEAP_SIZE;
  *stack = DEFAULT_STACK_SIZE;
  *symbols = DEFAULT_SYMBOL_TABLE_SIZE;

  for(i = 1; i < C_main_argc; ++i)
    if(!strncmp(C_main_argv[ i ], C_text("-:"), 2)) {
      for(ptr = &C_main_argv[ i ][ 2 ]; *ptr != '\0';) {
	switch(*(ptr++)) {
	case '?':
	  C_printf("\nRuntime options:\n\n"
		 " -:?              display this text\n"
		 " -:c              always treat stdin as console\n"
		 " -:d              enable debug output\n"
		 " -:D              enable more debug output\n"
		 " -:o              disable stack overflow checks\n"
		 " -:hiSIZE         set initial heap size\n"
		 " -:hmSIZE         set maximal heap size\n"
		 " -:hgPERCENTAGE   set heap growth percentage\n"
		 " -:hsPERCENTAGE   set heap shrink percentage\n"
		 " -:hSIZE          set fixed heap size\n"
		 " -:r              write trace output to stderr\n"
		 " -:sSIZE          set nursery (stack) size\n"
		 " -:tSIZE          set symbol-table size\n"
                 " -:fSIZE          set maximal number of pending finalizers\n"
		 " -:w              enable garbage collection of unused symbols\n"
		 " -:x              deliver uncaught exceptions of other threads to primordial one\n"
		 " -:b              enter REPL on error\n"
		 " -:B              sound bell on major GC\n"
		 " -:aSIZE          set trace-buffer/call-chain size\n"
		 "\n  SIZE may have a `k' (`K'), `m' (`M') or `g' (`G') suffix, meaning size\n"
		 "  times 1024, 1048576, and 1073741824, respectively.\n\n");
	  exit(0);

	case 'h':
	  switch(*ptr) {
	  case 'i':
	    *heap = arg_val(ptr + 1); 
	    heap_size_changed = 1;
	    goto next;
	  case 'g':
	    C_heap_growth = arg_val(ptr + 1);
	    goto next;
	  case 'm':
	    C_maximal_heap_size = arg_val(ptr + 1);
	    goto next;
	  case 's':
	    C_heap_shrinkage = arg_val(ptr + 1);
	    goto next;
	  default:
	    *heap = arg_val(ptr); 
	    heap_size_changed = 1;
	    C_heap_size_is_fixed = 1;
	    goto next;
	  }

	case 'o':
	  C_disable_overflow_check = 1;
	  break;

	case 'B':
	  gc_bell = 1;
	  break;

	case 's':
	  *stack = arg_val(ptr);
	  stack_size_changed = 1;
	  goto next;

	case 'f':
	  C_max_pending_finalizers = arg_val(ptr);
	  goto next;

	case 'a':
	  C_trace_buffer_size = arg_val(ptr);
	  goto next;

	case 't':
	  *symbols = arg_val(ptr);
	  goto next;

	case 'c':
	  fake_tty_flag = 1;
	  break;

	case 'd':
	  debug_mode = 1;
	  break;

	case 'D':
	  debug_mode = 2;
	  break;

	case 'w':
	  C_enable_gcweak = 1;
	  break;

	case 'r':
	  show_trace = 1;
	  break;

	case 'x':
	  C_abort_on_thread_exceptions = 1;
	  break;

	case 'b':
	  C_enable_repl = 1;
	  break;

	default: panic(C_text("illegal runtime option"));
	}
      }

    next:;
    }
}


C_word arg_val(C_char *arg)
{
  int len = C_strlen(arg);

  if(len < 1) panic(C_text("illegal runtime-option argument"));

  switch(arg[ len - 1 ]) {
  case 'k':
  case 'K':
    return atol(arg) * 1024;

  case 'm':
  case 'M':
    return atol(arg) * 1024 * 1024;

  case 'g':
  case 'G':
    return atol(arg) * 1024 * 1024 * 1024;

  default:
    return atol(arg);
  }
}


/* Run embedded code with arguments: */

C_word CHICKEN_run(void *toplevel)
{
  if(!chicken_is_initialized && !CHICKEN_initialize(0, 0, 0, toplevel))
    panic(C_text("could not initialize"));

  if(chicken_is_running)
    panic(C_text("re-invocation of Scheme world while process is already running"));

  chicken_is_running = chicken_ran_once = 1;
  return_to_host = 0;

#if C_STACK_GROWS_DOWNWARD
  C_stack_limit = (C_word *)((C_byte *)C_stack_pointer - stack_size);
#else
  C_stack_limit = (C_word *)((C_byte *)C_stack_pointer + stack_size);
#endif

  stack_bottom = C_stack_pointer;

  if(debug_mode)
    C_printf(C_text("[debug] stack bottom is 0x%lx.\n"), (long)stack_bottom);

  /* The point of (usually) no return... */
  C_setjmp(C_restart);

  if(!return_to_host)
    (C_restart_trampoline)(C_restart_address);

  chicken_is_running = 0;
  return C_restore;
}


C_word CHICKEN_continue(C_word k)
{
  if(C_temporary_stack_bottom != C_temporary_stack)
    panic(C_text("invalid level of temporary stack"));

  if(!chicken_is_initialized)
    panic(C_text("runtime system has not been initialized - `CHICKEN_run' has probably not been called"));

  C_save(k);
  return CHICKEN_run(NULL);
}


/* Trampoline called at system startup: */

C_regparm void C_fcall initial_trampoline(void *proc)
{
  TOPLEVEL top = (TOPLEVEL)proc;
  C_word closure = (C_word)C_alloc(2);

  ((C_SCHEME_BLOCK *)closure)->header = C_CLOSURE_TYPE | 1;
  C_set_block_item(closure, 0, (C_word)termination_continuation);
  (top)(2, C_SCHEME_UNDEFINED, closure);
}


/* The final continuation: */

void C_ccall termination_continuation(C_word c, C_word self, C_word result)
{
  if(debug_mode) C_printf(C_text("[debug] application terminated normally.\n"));

  exit(0);
}


/* Signal unrecoverable runtime error: */

void panic(C_char *msg)
{
  C_char *dmp = C_dump_trace(0);

  C_dbg_hook(C_SCHEME_UNDEFINED);

#ifdef C_MICROSOFT_WINDOWS
  C_sprintf(buffer, C_text("%s\n\n%s"), msg, dmp);

  MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK);
  ExitProcess(1);
#else
  C_fprintf(C_stderr, C_text("\n%s - execution terminated\n\n%s"), msg, dmp);
  
# if defined(__MWERKS__) && !defined(__INTEL__) && !defined(MACINTOSH_GUI)
  C_printf("\n[press RETURN to exit...]");
  C_fflush(C_stdout);
  C_getchar();
# endif

  C_exit(1);
#endif
}


void horror(C_char *msg)
{
  C_dbg_hook(C_SCHEME_UNDEFINED);

#ifdef C_MICROSOFT_WINDOWS
  C_sprintf(buffer, C_text("%s"), msg);

  MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK);
  ExitProcess(1);
#else
  C_fprintf(C_stderr, C_text("\n%s - execution terminated"), msg);
  
# if defined(__MWERKS__) && !defined(__INTEL__) && !defined(MACINTOSH_GUI)
  C_printf("\n[press RETURN to exit...]");
  C_fflush(C_stdout);
  C_getchar();
# endif

  C_exit(1);
#endif
}


/* Error-hook, called from C-level runtime routines: */

void barf(int code, char *loc, ...)
{
  C_char *msg;
  C_word err = error_hook_symbol;
  int c, i;
  va_list v;

  C_dbg_hook(C_SCHEME_UNDEFINED);

  C_temporary_stack = C_temporary_stack_bottom;
  err = C_u_i_car(err);

  if(C_immediatep(err))
    panic(C_text("`##sys#error-hook' is not defined - the `library' unit was probably not linked with this executable"));

  switch(code) {
  case C_BAD_ARGUMENT_COUNT_ERROR:
    msg = C_text("wrong number of arguments in function call");
    c = 3;
    break;

  case C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR:
    msg = C_text("too few arguments in function call");
    c = 3;
    break;
  
  case C_BAD_ARGUMENT_TYPE_ERROR:
    msg = C_text("bad argument type");
    c = 1;
    break;

  case C_UNBOUND_VARIABLE_ERROR:
    msg = C_text("unbound variable");
    c = 1;
    break;

  case C_TOO_MANY_PARAMETERS_ERROR:
    msg = C_text("parameter limit exceeded");
    c = 0;
    break;

  case C_OUT_OF_MEMORY_ERROR:
    msg = C_text("not enough memory");
    c = 0;
    break;

  case C_DIVISION_BY_ZERO_ERROR:
    msg = C_text("division by zero");
    c = 0;
    break;

  case C_OUT_OF_RANGE_ERROR:
    msg = C_text("out of range");
    c = 2;
    break;

  case C_NOT_A_CLOSURE_ERROR:
    msg = C_text("call of non-procedure");
    c = 1;
    break;

  case C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR:
    msg = C_text("continuation can not receive multiple values");
    c = 1;
    break;

  case C_TOO_DEEP_RECURSION_ERROR:
    msg = C_text("recursion too deep");
    c = 0;
    break;

  case C_CANT_REPRESENT_INEXACT_ERROR:
    msg = C_text("inexact number can not be represented as an exact number");
    c = 1;
    break;

  case C_NOT_A_PROPER_LIST_ERROR:
    msg = C_text("argument is not a proper list");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR:
    msg = C_text("bad argument type - not a fixnum");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR:
    msg = C_text("bad argument type - not a string");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_PAIR_ERROR:
    msg = C_text("bad argument type - not a pair");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR:
    msg = C_text("bad argument type - not a list");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR:
    msg = C_text("bad argument type - not a number");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR:
    msg = C_text("bad argument type - not a symbol");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_VECTOR_ERROR:
    msg = C_text("bad argument type - not a vector");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR:
    msg = C_text("bad argument type - not a character");
    c = 1;
    break;

  case C_STACK_OVERFLOW_ERROR:
    msg = C_text("stack overflow");
    c = 0;
    break;

  case C_BAD_ARGUMENT_TYPE_BAD_STRUCT_ERROR:
    msg = C_text("bad argument type - not a structure of the required type");
    c = 2;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_BYTEVECTOR_ERROR:
    msg = C_text("bad argument type - not a bytevector");
    c = 1;
    break;

  case C_LOST_LOCATIVE_ERROR:
    msg = C_text("locative refers to reclaimed object");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR:
    msg = C_text("bad argument type - immediate value expected");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_NUMBER_VECTOR_ERROR:
    msg = C_text("bad argument type - number vector expected");
    c = 2;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR:
    msg = C_text("bad argument type - integer expected");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR:
    msg = C_text("bad argument type - unsigned integer expected");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR:
    msg = C_text("bad argument type - pointer expected");
    c = 1;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_TAGGED_POINTER_ERROR:
    msg = C_text("bad argument type - tagged pointer expected");
    c = 2;
    break;

  case C_RUNTIME_UNSAFE_DLOAD_SAFE_ERROR:
    msg = C_text("code to load dynamically was linked with safe runtime libraries, but executing runtime was not");
    c = 0;
    break;

  case C_RUNTIME_SAFE_DLOAD_UNSAFE_ERROR:
    msg = C_text("code to load dynamically was linked with unsafe runtime libraries, but executing runtime was not");
    c = 0;
    break;

  case C_BAD_ARGUMENT_TYPE_NO_FLONUM_ERROR:
    msg = C_text("bad argument type - floating-point number expected");
    c = 1;
    break;

  default: panic(C_text("illegal internal error code"));
  }
  
  if(!C_immediatep(err)) {
    C_save(C_fix(code));
    
    if(loc != NULL)
      C_save(intern0(loc));
    else {
      C_save(error_location);
      error_location = C_SCHEME_FALSE;
    }
    
    va_start(v, loc);
    i = c;

    while(i--)
      C_save(va_arg(v, C_word));

    va_end(v);
    /* No continuation is passed: '##sys#error-hook' may not return: */
    C_do_apply(c + 2, err, C_SCHEME_UNDEFINED); 
  }
  else panic(msg);
}


/* Hook for setting breakpoints */

C_word C_dbg_hook(C_word dummy)
{
  return dummy;
}


/* Timing routines: */

long C_fcall milliseconds(void)
{
#ifdef C_NONUNIX
    if(CLOCKS_PER_SEC == 1000) return clock();
    else return ((double)clock() / (double)CLOCKS_PER_SEC) * 1000;
#else
    struct timeval tv;

    if(C_gettimeofday(&tv, NULL) == -1) return 0;
    else return (tv.tv_sec - C_startup_time_seconds) * 1000 + tv.tv_usec / 1000;
#endif
}


C_regparm time_t C_fcall C_seconds(long *ms)
{
#ifdef C_NONUNIX
  if(ms != NULL) *ms = 0;

  return (time_t)(clock() / CLOCKS_PER_SEC);
#else
  struct timeval tv;

  if(C_gettimeofday(&tv, NULL) == -1) {
    if(ms != NULL) *ms = 0;

    return (time_t)0;
  }
  else {
    if(ms != NULL) *ms = tv.tv_usec / 1000;

    return tv.tv_sec;
  }
#endif
}


long C_fcall cpu_milliseconds(void)
{
#if defined(C_NONUNIX) || defined(__CYGWIN__)
    if(CLOCKS_PER_SEC == 1000) return clock();
    else return ((double)clock() / (double)CLOCKS_PER_SEC) * 1000;
#else
    struct rusage ru;

    if(C_getrusage(RUSAGE_SELF, &ru) == -1) return 0;
    else return (ru.ru_utime.tv_sec + ru.ru_stime.tv_sec) * 1000 + (ru.ru_utime.tv_usec + ru.ru_stime.tv_usec) / 1000;
#endif
}


/* Support code for callbacks: */

int C_fcall C_save_callback_continuation(C_word **ptr, C_word k)
{
  C_word p = C_pair(ptr, k, C_block_item(callback_continuation_stack_symbol, 0));
  
  C_mutate(&C_block_item(callback_continuation_stack_symbol, 0), p);
  return 0;
}


C_word C_fcall C_restore_callback_continuation(void) 
{
  C_word p = C_block_item(callback_continuation_stack_symbol, 0),
         k;

  assert(!C_immediatep(p) && C_block_header(p) == C_PAIR_TAG);
  k = C_u_i_car(p);

  C_mutate(&C_block_item(callback_continuation_stack_symbol, 0), C_u_i_cdr(p));
  return k;
}


C_word C_fcall C_callback(C_word closure, int argc)
{
  jmp_buf prev;
  C_word 
    *a = C_alloc(2),
    k = C_closure(&a, 1, (C_word)callback_return_continuation);

  C_memcpy(&prev, &C_restart, sizeof(jmp_buf));
  callback_returned_flag = 0;       
  chicken_is_running = 1;

  if(!C_setjmp(C_restart)) C_do_apply(argc, closure, k);

  chicken_is_running = 0;

  if(!callback_returned_flag) (C_restart_trampoline)(C_restart_address);
  else {
    C_memcpy(&C_restart, &prev, sizeof(jmp_buf));
    callback_returned_flag = 0;
  }
 
  return C_restore;
}


void C_fcall C_callback_adjust_stack_limits(C_word *a)
{
  if(!chicken_is_running && !C_in_stackp((C_word)a)) {
    if(debug_mode)
      C_printf(C_text("[debug] callback invoked in lower stack region - adjusting limits:\n"
		      "[debug]   current:  \t%p\n"
		      "[debug]   previous: \t%p (bottom) - %p (limit)\n"),
	       a, stack_bottom, C_stack_limit);

#if C_STACK_GROWS_DOWNWARD
    C_stack_limit = (C_word *)((C_byte *)a - stack_size);
#else
    C_stack_limit = (C_word *)((C_byte *)a + stack_size);
#endif
    stack_bottom = a;

    if(debug_mode)
      C_printf(C_text("[debug]   new:      \t%p (bottom) - %p (limit)\n"),
	       stack_bottom, C_stack_limit);
  }
}


C_word C_fcall C_callback_wrapper(void *proc, int argc)
{
  C_word
    *a = C_alloc(2),
    closure = C_closure(&a, 1, (C_word)proc),
    result;
  
  result = C_callback(closure, argc);
  assert(C_temporary_stack == C_temporary_stack_bottom);
  return result;
}


void C_ccall callback_return_continuation(C_word c, C_word self, C_word r)
{
  assert(callback_returned_flag == 0);
  callback_returned_flag = 1;
  C_save(r);
  C_reclaim(NULL, NULL);
}


/* Zap symbol names: */

void C_zap_strings(C_word str)
{
  C_word bucket, sym;
  int i;
  
  for(i = 0; i < symbol_table->size; ++i)
    for(bucket = symbol_table->table[ i ]; bucket != C_SCHEME_END_OF_LIST; bucket = C_u_i_cdr(bucket)) {
      sym = C_u_i_car(bucket);
      C_set_block_item(sym, 1, str);
    }  
}


/* Register/unregister literal frame: */

void C_initialize_lf(C_word *lf, int count)
{
  while(count-- > 0)
    *(lf++) = C_SCHEME_UNBOUND;
}


void *C_register_lf(C_word *lf, int count)
{
  return C_register_lf2(lf, count, NULL);
}


void *C_register_lf2(C_word *lf, int count, C_PTABLE_ENTRY *ptable)
{
  LF_LIST *node = (LF_LIST *)C_malloc(sizeof(LF_LIST));
  LF_LIST *np;
  int status = 0;

  node->lf = lf;
  node->count = count;
  node->ptable = ptable;
  node->module_name = NULL;
  node->module_handle = NULL;
  
  if(reload_lf != NULL) {
    if(debug_mode)
      C_printf(C_text("[debug] replacing previous LF-entry for `%s'\n"), current_module_name);
    
    C_free(reload_lf->module_name);
    reload_lf->lf = lf;
    reload_lf->count = count;
    reload_lf->ptable = ptable;
    C_free(node);
    node = reload_lf;
  }

  node->module_name = current_module_name;
  node->module_handle = current_module_handle;
  current_module_handle = NULL;

  if(reload_lf != node) {
    if(lf_list) lf_list->prev = node;

    node->next = lf_list;
    node->prev = NULL;
    lf_list = node;
  }
  else reload_lf = NULL;

  return (void *)node;
}


LF_LIST *find_module_handle(char *name)
{
  LF_LIST *np;

  for(np = lf_list; np != NULL; np = np->next) {
    if(np->module_name != NULL && !C_strcmp(np->module_name, name)) 
      return np;
  }

  return NULL;
}


void C_unregister_lf(void *handle)
{
  LF_LIST *node = (LF_LIST *) handle;

  if (node->next) node->next->prev = node->prev;

  if (node->prev) node->prev->next = node->next;

  if (lf_list == node) lf_list = node->next;

  C_free(node->module_name);
  C_free(node);
}


/* Intern symbol into symbol-table: */

C_regparm C_word C_fcall C_intern(C_word **ptr, int len, C_char *str)
{
  return C_intern_in(ptr, len, str, symbol_table);
}


C_regparm C_word C_fcall C_h_intern(C_word *slot, int len, C_char *str)
{
  return C_h_intern_in(slot, len, str, symbol_table);
}


C_regparm C_word C_fcall C_intern_in(C_word **ptr, int len, C_char *str, C_SYMBOL_TABLE *stable)
{
  int key;
  C_word s;

  if(stable == NULL) stable = symbol_table;

  key = hash_string(len, str, stable->size);

  if(C_truep(s = lookup(key, len, str, stable))) return s;

  s = C_string(ptr, len, str);
  return add_symbol(ptr, key, s, stable);
}


C_regparm C_word C_fcall C_h_intern_in(C_word *slot, int len, C_char *str, C_SYMBOL_TABLE *stable)
{
  /* Intern as usual, but remember slot, if looked up symbol is in nursery.
     also: allocatein static memory. */
  int key;
  C_word s;

  if(stable == NULL) stable = symbol_table;

  key = hash_string(len, str, stable->size);

  if(C_truep(s = lookup(key, len, str, stable))) {
    if(C_in_stackp(s)) C_mutate(slot, s);
    
    return s;
  }

  s = C_static_string(C_heaptop, len, str);
  return add_symbol(C_heaptop, key, s, stable);
}


C_regparm C_word C_fcall intern0(C_char *str)
{
  int len = C_strlen(str);
  int key = hash_string(len, str, symbol_table->size);
  C_word s;

  if(C_truep(s = lookup(key, len, str, symbol_table))) return s;
  else return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_lookup_symbol(C_word sym)
{
  int key;
  C_word str = C_block_item(sym, 1);
  int len = C_header_size(str);

  key = hash_string(len, C_c_string(str), symbol_table->size);

  return lookup(key, len, C_c_string(str), symbol_table);
}


C_regparm C_word C_fcall C_intern2(C_word **ptr, C_char *str)
{
  return C_intern_in(ptr, C_strlen(str), str, symbol_table);
}


C_regparm C_word C_fcall C_intern3(C_word **ptr, C_char *str, C_word value)
{
  C_word s = C_intern_in(ptr, C_strlen(str), str, symbol_table);
  
  C_mutate(&C_u_i_car(s), value);
  return s;
}


C_regparm int C_fcall hash_string(int len, C_char *str, unsigned int m)
{
  unsigned int key = 0;

  while(len--) key = (key << 4) + *(str++);

  return (int)(key % m);
}


C_regparm C_word C_fcall lookup(C_word key, int len, C_char *str, C_SYMBOL_TABLE *stable)
{
  C_word bucket, sym, s;

  for(bucket = stable->table[ key ]; bucket != C_SCHEME_END_OF_LIST; bucket = C_u_i_cdr(bucket)) {
    sym = C_u_i_car(bucket);
    s = C_u_i_cdr(sym);

    if(C_header_size(s) == (C_word)len && !C_memcmp(str, (C_char *)((C_SCHEME_BLOCK *)s)->data, len))
      return sym;
  }

  return C_SCHEME_FALSE;
}


double compute_symbol_table_load(double *avg_bucket_len, int *total_n)
{
  C_word bucket;
  int i, j, alen = 0, bcount = 0, total = 0;

  for(i = 0; i < symbol_table->size; ++i) {
    bucket = symbol_table->table[ i ];

    for(j = 0; bucket != C_SCHEME_END_OF_LIST; ++j)
      bucket = C_u_i_cdr(bucket);

    if(j > 0) {
      alen += j;
      ++bcount;
    }

    total += j;
  }

  if(avg_bucket_len != NULL)
    *avg_bucket_len = (double)alen / (double)bcount;

  *total_n = total;

  /* return load: */
  return (double)total / (double)symbol_table->size;
}


C_word add_symbol(C_word **ptr, C_word key, C_word string, C_SYMBOL_TABLE *stable)
{
  C_word bucket, sym, b2, *p;
  int keyw = C_header_size(string) > 0 && *((char *)C_data_pointer(string)) == 0;

  p = *ptr;
  sym = (C_word)p;
  p += C_SIZEOF_SYMBOL;
  ((C_SCHEME_BLOCK *)sym)->header = C_SYMBOL_TYPE | (C_SIZEOF_SYMBOL - 1);
  C_set_block_item(sym, 0, keyw ? sym : C_SCHEME_UNBOUND); /* keyword? */
  C_set_block_item(sym, 1, string);
#ifdef C_EXTRA_SYMBOL_SLOT
  C_set_block_item(sym, 2, C_SCHEME_UNDEFINED);
#endif
  *ptr = p;
  b2 = stable->table[ key ];	/* previous bucket */
  bucket = C_pair(ptr, sym, b2); /* create new bucket */
  ((C_SCHEME_BLOCK *)bucket)->header = 
    (((C_SCHEME_BLOCK *)bucket)->header & ~C_HEADER_TYPE_BITS) | C_BUCKET_TYPE;

  if(ptr != C_heaptop) C_mutate(&stable->table[ key ], bucket);
  else {
    /* If a stack-allocated bucket was here, and we allocate from 
       heap-top (say, in a toplevel literal frame allocation) then we have
       to inform the memory manager that a 2nd gen. block points to a 
       1st gen. block, hence the mutation: */
    C_mutate(&C_u_i_cdr(bucket), b2);
    stable->table[ key ] = bucket;
  }

  return sym;
}


/* Check block allocation: */

C_regparm C_word C_fcall C_permanentp(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && !C_in_stackp(x) && !C_in_heapp(x));
}


C_regparm int C_in_stackp(C_word x)
{
  C_word *ptr = (C_word *)(C_uword)x;

#if C_STACK_GROWS_DOWNWARD
  return ptr >= C_stack_pointer_test && ptr <= stack_bottom;
#else
  return ptr < C_stack_pointer_test && ptr >= stack_bottom;
#endif
}


C_regparm int C_fcall C_in_heapp(C_word x)
{
  C_byte *ptr = (C_byte *)(C_uword)x;
  return (ptr >= fromspace_start && ptr < C_fromspace_limit) ||
         (ptr >= tospace_start && ptr < tospace_limit);
}


C_regparm int C_fcall C_in_fromspacep(C_word x)
{
  C_byte *ptr = (C_byte *)(C_uword)x;
  return (ptr >= fromspace_start && ptr < C_fromspace_limit);
}


/* Cons the rest-aguments together: */

C_regparm C_word C_fcall C_restore_rest(C_word *ptr, int num)
{
  C_word x = C_SCHEME_END_OF_LIST;
  C_SCHEME_BLOCK *node;

  while(num--) {
    node = (C_SCHEME_BLOCK *)ptr;
    ptr += 3;
    node->header = C_PAIR_TYPE | (C_SIZEOF_PAIR - 1);
    node->data[ 0 ] = C_restore;
    node->data[ 1 ] = x;
    x = (C_word)node;
  }

  return x;
}


C_regparm C_word C_fcall C_restore_rest_vector(C_word *ptr, int num)
{
  C_word *p0 = ptr;

  *(ptr++) = C_VECTOR_TYPE | num;
  ptr += num;

  while(num--) *(--ptr) = C_restore;

  return (C_word)p0;
}


/* Print error messages and exit: */

void C_bad_memory(void)
{
  panic(C_text("there is not enough stack-space to run this executable"));
}


void C_bad_memory_2(void)
{
  panic(C_text("there is not enough heap-space to run this executable - try using the '-:h...' option"));
}


/* The following two can be thrown out in the next release... */

void C_bad_argc(int c, int n)
{
  C_bad_argc_2(c, n, C_SCHEME_FALSE);
}


void C_bad_min_argc(int c, int n)
{
  C_bad_min_argc_2(c, n, C_SCHEME_FALSE);
}


void C_bad_argc_2(int c, int n, C_word closure)
{
  barf(C_BAD_ARGUMENT_COUNT_ERROR, NULL, C_fix(n - 2), C_fix(c - 2), closure);
}


void C_bad_min_argc_2(int c, int n, C_word closure)
{
  barf(C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR, NULL, C_fix(n - 2), C_fix(c - 2), closure);
}


void C_stack_overflow(void)
{
  barf(C_STACK_OVERFLOW_ERROR, NULL);
}


void C_unbound_error(C_word sym)
{
  barf(C_UNBOUND_VARIABLE_ERROR, NULL, sym);
}


void C_no_closure_error(C_word x)
{
  barf(C_NOT_A_CLOSURE_ERROR, NULL, x);
}


/* Allocate and initialize record: */

C_regparm C_word C_fcall C_string(C_word **ptr, int len, C_char *str)
{
  C_word strblock = (C_word)(*ptr);

  *ptr = (C_word *)((C_word)(*ptr) + sizeof(C_header) + C_align(len));
  ((C_SCHEME_BLOCK *)strblock)->header = C_STRING_TYPE | len;
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_static_string(C_word **ptr, int len, C_char *str)
{
  C_word *dptr = (C_word *)C_malloc(sizeof(C_header) + C_align(len));
  C_word strblock;

  if(dptr == NULL)
    panic(C_text("out of memory - can not allocate static string"));
    
  strblock = (C_word)dptr;
  ((C_SCHEME_BLOCK *)strblock)->header = C_STRING_TYPE | len;
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_static_lambda_info(C_word **ptr, int len, C_char *str)
{
  int dlen = sizeof(C_header) + C_align(len);
  void *dptr = C_malloc(dlen);
  C_word strblock;

  if(dptr == NULL)
    panic(C_text("out of memory - can not allocate static lambda info"));

  strblock = (C_word)dptr;
  ((C_SCHEME_BLOCK *)strblock)->header = C_LAMBDA_INFO_TYPE | len;
  C_memcpy(C_data_pointer(strblock), str, len);
  return strblock;
}


C_regparm C_word C_fcall C_bytevector(C_word **ptr, int len, C_char *str)
{
  C_word strblock = C_string(ptr, len, str);

  C_string_to_bytevector(strblock);
  return strblock;
}


C_regparm C_word C_fcall C_pbytevector(int len, C_char *str)
{
  C_SCHEME_BLOCK *pbv = C_malloc(len + sizeof(C_header));

  if(pbv == NULL) panic(C_text("out of memory - can not allocate permanent bytevector"));

  pbv->header = C_BYTEVECTOR_TYPE | len;
  C_memcpy(pbv->data, str, len);
  return (C_word)pbv;
}


C_regparm C_word C_fcall C_string_aligned8(C_word **ptr, int len, C_char *str)
{
  C_word *p = *ptr,
         *p0;

#ifndef C_SIXTY_FOUR
  /* Align on 8-byte boundary: */
  if(aligned8(p)) ++p;
#endif

  p0 = p;
  *ptr = p + 1 + C_bytestowords(len);
  *(p++) = C_STRING_TYPE | C_8ALIGN_BIT | len;
  C_memcpy(p, str, len);
  return (C_word)p0;
}


C_regparm C_word C_fcall C_string2(C_word **ptr, C_char *str)
{
  C_word strblock = (C_word)(*ptr);
  int len;

  if(str == NULL) return C_SCHEME_FALSE;

  len = C_strlen(str);
  *ptr = (C_word *)((C_word)(*ptr) + sizeof(C_header) + C_align(len));
  ((C_SCHEME_BLOCK *)strblock)->header = C_STRING_TYPE | len;
  C_memcpy(((C_SCHEME_BLOCK *)strblock)->data, str, len);
  return strblock;
}


C_regparm C_word C_fcall C_string2_safe(C_word **ptr, int max, C_char *str)
{
  C_word strblock = (C_word)(*ptr);
  int len;

  if(str == NULL) return C_SCHEME_FALSE;

  len = C_strlen(str);

  if(len >= max) {
    C_sprintf(buffer, C_text("foreign string result exceeded maximum of %d bytes"), max);
    panic(buffer);
  }

  *ptr = (C_word *)((C_word)(*ptr) + sizeof(C_header) + C_align(len));
  ((C_SCHEME_BLOCK *)strblock)->header = C_STRING_TYPE | len;
  C_memcpy(((C_SCHEME_BLOCK *)strblock)->data, str, len);
  return strblock;
}


C_word C_fcall C_closure(C_word **ptr, int cells, C_word proc, ...)
{
  va_list va;
  C_word *p = *ptr,
         *p0 = p;

  *p = C_CLOSURE_TYPE | cells;
  *(++p) = proc;

  for(va_start(va, proc); --cells; *(++p) = va_arg(va, C_word));

  va_end(va);
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_pair(C_word **ptr, C_word car, C_word cdr)
{
  C_word *p = *ptr,
         *p0 = p;
 
  *(p++) = C_PAIR_TYPE | (C_SIZEOF_PAIR - 1);
  *(p++) = car;
  *(p++) = cdr;
  *ptr = p;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_h_pair(C_word car, C_word cdr)
{
  /* Allocate on heap and check for non-heap slots: */
  C_word *p = (C_word *)C_fromspace_top,
         *p0 = p;
 
  *(p++) = C_PAIR_TYPE | (C_SIZEOF_PAIR - 1);

  if(C_in_stackp(car)) C_mutate(p++, car);
  else *(p++) = car;

  if(C_in_stackp(cdr)) C_mutate(p++, cdr);
  else *(p++) = cdr;

  C_fromspace_top = (C_byte *)p;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_flonum(C_word **ptr, double n)
{
  C_word 
    *p = *ptr,
    *p0;

#ifndef C_SIXTY_FOUR
#ifndef C_DOUBLE_IS_32_BITS
  /* Align double on 8-byte boundary: */
  if(aligned8(p)) ++p;
#endif
#endif

  p0 = p;
  *(p++) = C_FLONUM_TAG;
  *((double *)p) = n;
  *ptr = p + sizeof(double) / sizeof(C_word);
  return (C_word)p0;
}


C_regparm C_word C_fcall C_number(C_word **ptr, double n)
{
  C_word 
    *p = *ptr,
    *p0;
  double m;

  if(n <= (double)C_MOST_POSITIVE_FIXNUM && n >= (double)C_MOST_NEGATIVE_FIXNUM && modf(n, &m) == 0.0) 
    return C_fix(n);

#ifndef C_SIXTY_FOUR
#ifndef C_DOUBLE_IS_32_BITS
  /* Align double on 8-byte boundary: */
  if(aligned8(p)) ++p;
#endif
#endif

  p0 = p;
  *(p++) = C_FLONUM_TAG;
  *((double *)p) = n;
  *ptr = p + sizeof(double) / sizeof(C_word);
  return (C_word)p0;
}


C_regparm C_word C_fcall C_mpointer(C_word **ptr, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  *(p++) = C_POINTER_TYPE | 1;
  *((void **)p) = mp;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_mpointer_or_false(C_word **ptr, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  if(mp == NULL) return C_SCHEME_FALSE;

  *(p++) = C_POINTER_TYPE | 1;
  *((void **)p) = mp;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_taggedmpointer(C_word **ptr, C_word tag, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  *(p++) = C_TAGGED_POINTER_TAG;
  *((void **)p) = mp;
  *(++p) = tag;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_taggedmpointer_or_false(C_word **ptr, C_word tag, void *mp)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  if(mp == NULL) return C_SCHEME_FALSE;
 
  *(p++) = C_TAGGED_POINTER_TAG;
  *((void **)p) = mp;
  *(++p) = tag;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_swigmpointer(C_word **ptr, void *mp, void *sdata)
{
  C_word 
    *p = *ptr,
    *p0 = p;

  *(p++) = C_SWIG_POINTER_TAG;
  *((void **)p) = mp;
  *((void **)p + 1) = sdata;
  *ptr = p + 2;
  return (C_word)p0;
}


C_word C_vector(C_word **ptr, int n, ...)
{
  va_list v;
  C_word 
    *p = *ptr,
    *p0 = p; 

  *(p++) = C_VECTOR_TYPE | n;
  va_start(v, n);

  while(n--)
    *(p++) = va_arg(v, C_word);

  *ptr = p;
  va_end(v);
  return (C_word)p0;
}


C_word C_structure(C_word **ptr, int n, ...)
{
  va_list v;
  C_word *p = *ptr,
         *p0 = p; 

  *(p++) = C_STRUCTURE_TYPE | n;
  va_start(v, n);

  while(n--)
    *(p++) = va_arg(v, C_word);

  *ptr = p;
  va_end(v);
  return (C_word)p0;
}


C_word C_h_vector(int n, ...)
{
  /* As C_vector(), but remember slots containing nursery pointers: */
  va_list v;
  C_word *p = (C_word *)C_fromspace_top,
         *p0 = p,
         x; 

  *(p++) = C_VECTOR_TYPE | n;
  va_start(v, n);

  while(n--) {
    x = va_arg(v, C_word);

    if(C_in_stackp(x)) C_mutate(p++, x);
    else *(p++) = x;
  }

  C_fromspace_top = (C_byte *)p;
  va_end(v);
  return (C_word)p0;
}


C_word C_h_structure(int n, ...)
{
  /* As C_structure(), but remember slots containing nursery pointers: */
  va_list v;
  C_word *p = (C_word *)C_fromspace_top,
         *p0 = p,
         x; 

  *(p++) = C_STRUCTURE_TYPE | n;
  va_start(v, n);

  while(n--) {
    x = va_arg(v, C_word);

    if(C_in_stackp(x)) C_mutate(p++, x);
    else *(p++) = x;
  }

  C_fromspace_top = (C_byte *)p;
  va_end(v);
  return (C_word)p0;
}


C_regparm C_word C_fcall C_mutate(C_word *slot, C_word val)
{
  int mssize;

  if(!C_immediatep(val)) {
#ifdef C_GC_HOOKS
    if(C_gc_mutation_hook != NULL && C_gc_mutation_hook(slot, val)) return val;
#endif

    if(mutation_stack_top >= mutation_stack_limit) {
      assert(mutation_stack_top == mutation_stack_limit);
      mssize = mutation_stack_top - mutation_stack_bottom;
      mutation_stack_bottom = (C_word **)realloc(mutation_stack_bottom, (mssize + MUTATION_STACK_GROWTH) * sizeof(C_word *));
      
      if(mutation_stack_bottom == NULL)
	panic(C_text("out of memory - can not re-allocate mutation stack"));

      mutation_stack_limit = mutation_stack_bottom + mssize + MUTATION_STACK_GROWTH;
      mutation_stack_top = mutation_stack_bottom + mssize;
    }

    *(mutation_stack_top++) = slot;
    ++mutation_count;
  }

  return *slot = val;
}


/* Initiate garbage collection: */


void C_save_and_reclaim(void *trampoline, void *proc, int n, ...)
{
  va_list v;
  
  va_start(v, n);

  while(n--) C_save(va_arg(v, C_word));

  va_end(v);
  C_reclaim(trampoline, proc);
}


C_regparm void C_fcall C_reclaim(void *trampoline, void *proc)
{
  int i, j, n, fcount, weakn;
  C_uword count, bytes;
  C_word *p, **msp, bucket, last, item, container;
  C_header h;
  C_byte *tmp, *start;
  LF_LIST *lfn;
  C_SCHEME_BLOCK *bp;
  C_GC_ROOT *gcrp;
  WEAK_TABLE_ENTRY *wep;
  long tgc;
  C_SYMBOL_TABLE *stp;
  volatile int finalizers_checked;
  FINALIZER_NODE *flist;
  TRACE_INFO *tinfo;

  /* assert(C_timer_interrupt_counter >= 0); */

  if(interrupt_reason && C_interrupts_enabled)
    handle_interrupt(trampoline, proc);

  finalizers_checked = 0;
  C_restart_trampoline = (TRAMPOLINE)trampoline;
  C_restart_address = proc;
  heap_scan_top = (C_byte *)C_align((C_uword)C_fromspace_top);
  gc_mode = GC_MINOR;

  /* Entry point for second-level GC (on explicit request or because of full fromspace): */
  if(C_setjmp(gc_restart) || (start = C_fromspace_top) >= C_fromspace_limit) {
    if(gc_bell) C_putchar(7);

    tgc = cpu_milliseconds();

    if(gc_mode == GC_REALLOC) {
      C_rereclaim2(percentage(heap_size, C_heap_growth), 0);
      gc_mode = GC_MAJOR;
      goto never_mind_edsgar;
    }

    heap_scan_top = (C_byte *)C_align((C_uword)tospace_top);    
    gc_mode = GC_MAJOR;

    /* Mark items in forwarding table: */
    for(p = forwarding_table; *p != 0; p += 2) {
      last = p[ 1 ];
      mark(&p[ 1 ]);
      C_block_header(p[ 0 ]) = C_block_header(last);
    }

    /* Mark literal frames: */
    for(lfn = lf_list; lfn != NULL; lfn = lfn->next)
      for(i = 0; i < lfn->count; mark(&lfn->lf[ i++ ]));

    /* Mark symbol tables: */
    for(stp = symbol_table_list; stp != NULL; stp = stp->next)
      for(i = 0; i < stp->size; mark(&stp->table[ i++ ]));

    /* Mark collectibles: */
    for(msp = collectibles; msp < collectibles_top; ++msp)
      if(*msp != NULL) mark(*msp);
    
    for(gcrp = gc_root_list; gcrp != NULL; gcrp = gcrp->next)
      mark(&gcrp->value);

    mark_system_globals();
  }
  else {
    /* Mark mutated slots: */
    for(msp = mutation_stack_bottom; msp < mutation_stack_top; mark(*(msp++)));
  }

  /* Clear the mutated slot stack: */
  mutation_stack_top = mutation_stack_bottom;

  /* Mark live values: */
  for(p = C_temporary_stack; p < C_temporary_stack_bottom; mark(p++));

  /* Mark trace-buffer: */
  for(tinfo = trace_buffer; tinfo < trace_buffer_limit; ++tinfo) {
    mark(&tinfo->cooked1);
    mark(&tinfo->cooked2);
    mark(&tinfo->thread);
  }

 rescan:
  /* Mark nested values in already moved (marked) blocks in breadth-first manner: */
  while(heap_scan_top < (gc_mode == GC_MINOR ? C_fromspace_top : tospace_top)) {
    bp = (C_SCHEME_BLOCK *)heap_scan_top;

    if(*((C_word *)bp) == ALIGNMENT_HOLE_MARKER) 
      bp = (C_SCHEME_BLOCK *)((C_word *)bp + 1);

    n = C_header_size(bp);
    h = bp->header;
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);
    p = bp->data;

    if(n > 0 && (h & C_BYTEBLOCK_BIT) == 0) {
      if(h & C_SPECIALBLOCK_BIT) {
	--n;
	++p;
      }

      while(n--) mark(p++);
    }

    heap_scan_top = (C_byte *)bp + C_align(bytes) + sizeof(C_word);
  }

  if(gc_mode == GC_MINOR) {
    count = (C_uword)C_fromspace_top - (C_uword)start;
    ++gc_count_1;
    update_locative_table(GC_MINOR);
  }
  else {
    if(!finalizers_checked) {
      /* Mark finalizer list and remember pointers to non-forwarded items: */
      last = C_block_item(pending_finalizers_symbol, 0);

      if(!C_immediatep(last) && (j = C_unfix(C_block_item(last, 0))) != 0) { 
	/* still finalizers pending: just mark table items... */
	if(gc_report_flag) 
	  C_printf(C_text("[GC] %d finalized item(s) still pending\n"), j);

	j = fcount = 0;

	for(flist = finalizer_list; flist != NULL; flist = flist->next) {
	  mark(&flist->item);
	  mark(&flist->finalizer);
	  ++fcount;
	}

	if(gc_report_flag && fcount > 0)
	  C_printf(C_text("[GC] %d finalizer value(s) marked\n"), fcount);
      }
      else {
	j = fcount = 0;

	for(flist = finalizer_list; flist != NULL; flist = flist->next) {
	  if(j < C_max_pending_finalizers) {
	    if(!is_fptr(C_block_header(flist->item))) 
	      pending_finalizer_indices[ j++ ] = flist;
	  }

	  mark(&flist->item);
	  mark(&flist->finalizer);
	}
      }

      pending_finalizer_count = j;
      finalizers_checked = 1;

      if(pending_finalizer_count > 0 && gc_report_flag)
	C_printf(C_text("[GC] finalizers pending for rescan:\t %d (%d live)\n"), 
		 pending_finalizer_count, live_finalizer_count);

      goto rescan;
    }
    else {
      /* Copy finalized items with remembered indices into `##sys#pending-finalizers' 
	 (and release finalizer node): */
      if(pending_finalizer_count > 0) {
	if(gc_report_flag) C_printf(C_text("[GC] queueing %d finalizers\n"), pending_finalizer_count);

	last = C_block_item(pending_finalizers_symbol, 0);
	assert(C_u_i_car(last) == C_fix(0));
	C_set_block_item(last, 0, C_fix(pending_finalizer_count));

	for(i = 0; i < pending_finalizer_count; ++i) {
	  flist = pending_finalizer_indices[ i ];
	  C_set_block_item(last, 1 + i * 2, flist->item);
	  C_set_block_item(last, 2 + i * 2, flist->finalizer);
	  
	  if(flist->previous != NULL) flist->previous->next = flist->next;
	  else finalizer_list = flist->next;

	  if(flist->next != NULL) flist->next->previous = flist->previous;

	  flist->next = finalizer_free_list;
	  flist->previous = NULL;
	  finalizer_free_list = flist;
	  --live_finalizer_count;
	}
      }
    }

    update_locative_table(gc_mode);
    count = (C_uword)tospace_top - (C_uword)tospace_start;

    /* isn't gc_mode always GC_MAJOR here? */
    if(gc_mode == GC_MAJOR && count < percentage(percentage(heap_size, C_heap_shrinkage), DEFAULT_HEAP_SHRINKAGE_USED) &&
       heap_size > MINIMAL_HEAP_SIZE && !C_heap_size_is_fixed)
      C_rereclaim2(percentage(heap_size, C_heap_shrinkage), 0);
    else {
      C_fromspace_top = tospace_top;
      tmp = fromspace_start;
      fromspace_start = tospace_start;
      tospace_start = tospace_top = tmp;
      tmp = C_fromspace_limit;
      C_fromspace_limit = tospace_limit;
      tospace_limit = tmp;
    }

  never_mind_edsgar:
    ++gc_count_2;

    if(C_enable_gcweak) {
      /* Check entries in weak item table and recover items ref'd only once and which are unbound symbols: */
      weakn = 0;
      wep = weak_item_table;

      for(i = 0; i < WEAK_TABLE_SIZE; ++i, ++wep)
	if(wep->item != 0) { 
	  if((wep->container & WEAK_COUNTER_MAX) == 0 && is_fptr((item = C_block_header(wep->item)))) {
	    item = fptr_to_ptr(item);
	    container = wep->container & ~WEAK_COUNTER_MASK;

	    if(C_header_bits(item) == C_SYMBOL_TYPE && C_u_i_car(item) == C_SCHEME_UNBOUND) {
	      ++weakn;
#ifdef PARANOIA
	      item = C_u_i_cdr(item);
	      C_fprintf(C_stderr, C_text("[recovered: %.*s]\n"), (int)C_header_size(item), (char *)C_data_pointer(item));
#endif
	      C_set_block_item(container, 0, C_SCHEME_UNDEFINED);
	    }
	  }

	  wep->item = wep->container = 0;
	}

      /* Remove empty buckets in symbol table: */
      for(stp = symbol_table_list; stp != NULL; stp = stp->next) {
	for(i = 0; i < stp->size; ++i) {
	  last = 0;
	  
	  for(bucket = stp->table[ i ]; bucket != C_SCHEME_END_OF_LIST; bucket = C_u_i_cdr(bucket))
	    if(C_u_i_car(bucket) == C_SCHEME_UNDEFINED) {
	      if(last) C_set_block_item(last, 1, C_u_i_cdr(bucket));
	      else stp->table[ i ] = C_u_i_cdr(bucket);
	    }
	    else last = bucket;
	}
      }
    }
  }

  if(gc_mode == GC_MAJOR) {
    tgc = cpu_milliseconds() - tgc;
    timer_start_gc_ms += tgc;
    timer_accumulated_gc_ms += tgc;
  }

  /* Display GC report: 
     Note: stubbornly writes to stdout - there is no provision for other output-ports */
  if(gc_report_flag == 1 || (gc_report_flag && gc_mode == GC_MAJOR)) {
    C_printf(C_text("[GC] level  %d\tgcs(minor)  %d\tgcs(major)  %d\n"),
	     gc_mode, gc_count_1, gc_count_2);
    i = (C_uword)C_stack_pointer;

#if C_STACK_GROWS_DOWNWARD
    C_printf(C_text("[GC] stack\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING), 
	   (C_uword)C_stack_limit, (C_uword)i, (C_uword)C_stack_limit + stack_size);
#else
    C_printf(C_text("[GC] stack\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING), 
	   (C_uword)C_stack_limit - stack_size, (C_uword)i, (C_uword)C_stack_limit);
#endif

    if(gc_mode == GC_MINOR) printf(C_text("\t" UWORD_FORMAT_STRING), count);

    C_printf(C_text("\n[GC]  from\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING),
	   (C_uword)fromspace_start, (C_uword)C_fromspace_top, (C_uword)C_fromspace_limit);

    if(gc_mode == GC_MAJOR) printf(C_text("\t" UWORD_FORMAT_STRING), count);

    C_printf(C_text("\n[GC]    to\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING "\t" UWORD_FORMAT_STRING" \n"), 
	   (C_uword)tospace_start, (C_uword)tospace_top, 
	   (C_uword)tospace_limit);

    if(gc_mode == GC_MAJOR && C_enable_gcweak && weakn)
      C_printf(C_text("[GC] %d recoverable weakly held items found\n"), weakn);

    C_printf(C_text("[GC] %d locatives (from %d)\n"), locative_table_count, locative_table_size);
  }

  if(gc_mode == GC_MAJOR) gc_count_1 = 0;

  if(C_post_gc_hook != NULL) C_post_gc_hook(gc_mode);

  /* Jump from the Empire State Building... */
  C_longjmp(C_restart, 1);
}


C_regparm void C_fcall mark_system_globals(void)
{
  mark(&interrupt_hook_symbol);
  mark(&error_hook_symbol);
  mark(&callback_continuation_stack_symbol);
  mark(&pending_finalizers_symbol);
  mark(&invalid_procedure_call_hook_symbol);
  mark(&unbound_variable_value_hook_symbol);
  mark(&last_invalid_procedure_symbol);
  mark(&identity_unbound_value_symbol);
  mark(&current_thread_symbol);
  mark(&apply_hook_symbol);
  mark(&last_applied_procedure_symbol);
}


C_regparm void C_fcall mark(C_word *x)
{
  C_word val, item;
  C_uword n, bytes;
  C_header h;
  C_SCHEME_BLOCK *p, *p2;
  WEAK_TABLE_ENTRY *wep;

  val = *x;

  if(C_immediatep(val)) return;

  p = (C_SCHEME_BLOCK *)val;
  
  /* not in stack and not in heap? */
  if (
#if C_STACK_GROWS_DOWNWARD
       p < (C_SCHEME_BLOCK *)C_stack_pointer || p >= (C_SCHEME_BLOCK *)stack_bottom
#else
       p >= (C_SCHEME_BLOCK *)C_stack_pointer || p < (C_SCHEME_BLOCK *)stack_bottom
#endif
     )
    if((p < (C_SCHEME_BLOCK *)fromspace_start || p >= (C_SCHEME_BLOCK *)C_fromspace_limit) &&
       (p < (C_SCHEME_BLOCK *)tospace_start || p >= (C_SCHEME_BLOCK *)tospace_limit) ) {
#ifdef C_GC_HOOKS
      if(C_gc_trace_hook != NULL) 
	C_gc_trace_hook(x, gc_mode);
#endif

      return;
    }

  h = p->header;

  if(gc_mode == GC_MINOR) {
    if(is_fptr(h)) {
      *x = val = fptr_to_ptr(h);
      return;
    }

    if((C_uword)val >= (C_uword)fromspace_start && (C_uword)val < (C_uword)C_fromspace_top) return;

    p2 = (C_SCHEME_BLOCK *)C_align((C_uword)C_fromspace_top);

#ifndef C_SIXTY_FOUR
    if((h & C_8ALIGN_BIT) && aligned8(p2) && (C_byte *)p2 < C_fromspace_limit) {
      *((C_word *)p2) = ALIGNMENT_HOLE_MARKER;
      p2 = (C_SCHEME_BLOCK *)((C_word *)p2 + 1);
    }
#endif

    n = C_header_size(p);
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);

    if(((C_byte *)p2 + bytes + sizeof(C_word)) > C_fromspace_limit)
      C_longjmp(gc_restart, 1);

    C_fromspace_top = (C_byte *)p2 + C_align(bytes) + sizeof(C_word);

  scavenge:
    *x = (C_word)p2;
    p2->header = h;
    p->header = ptr_to_fptr((C_uword)p2);
    C_memcpy(p2->data, p->data, bytes);
  }
  else {
    /* Increase counter if weakly held item: */
    if(C_enable_gcweak && (wep = lookup_weak_table_entry(val, 0)) != NULL) {
      if((wep->container & WEAK_COUNTER_MAX) == 0) ++wep->container;
    }

    if(is_fptr(h)) {
      val = fptr_to_ptr(h);

      if((C_uword)val >= (C_uword)tospace_start && (C_uword)val < (C_uword)tospace_top) {
	*x = val;
	return;
      }

      /* Link points into fromspace: fetch new pointer + header and copy... */
      p = (C_SCHEME_BLOCK *)val;
      h = p->header;

      if(is_fptr(h)) {
	/* Link points into fromspace and into a link which points into from- or tospace: */
	val = fptr_to_ptr(h);
	
	if((C_uword)val >= (C_uword)tospace_start && (C_uword)val < (C_uword)tospace_top) {
	  *x = val;
	  return;
	}

	p = (C_SCHEME_BLOCK *)val;
	h = p->header;
      }
    }

    p2 = (C_SCHEME_BLOCK *)C_align((C_uword)tospace_top);

#ifndef C_SIXTY_FOUR
    if((h & C_8ALIGN_BIT) && aligned8(p2) && (C_byte *)p2 < tospace_limit) {
      *((C_word *)p2) = ALIGNMENT_HOLE_MARKER;
      p2 = (C_SCHEME_BLOCK *)((C_word *)p2 + 1);
    }
#endif

    if(C_enable_gcweak && (h & C_HEADER_TYPE_BITS) == C_BUCKET_TYPE) {
      item = C_u_i_car(val);

      /* Lookup item in weak item table or add entry: */
      if((wep = lookup_weak_table_entry(item, (C_word)p2)) != NULL) {
	/* If item is already forwarded, then set count to 2: */
	if(is_fptr(C_block_header(item))) wep->container |= 2;
      }
    }

    n = C_header_size(p);
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);

    if(((C_byte *)p2 + bytes + sizeof(C_word)) > tospace_limit) {
      if(C_heap_size_is_fixed)
	panic(C_text("out of memory - heap full"));
      
      gc_mode = GC_REALLOC;
      C_longjmp(gc_restart, 1);
    }

    tospace_top = (C_byte *)p2 + C_align(bytes) + sizeof(C_word);
    goto scavenge;
  }
}


/* Do a major GC into a freshly allocated heap: */

C_regparm void C_fcall C_rereclaim(long size) 
{
  C_rereclaim2(size < 0 ? -size : size, size < 0);
}


C_regparm void C_fcall C_rereclaim2(C_uword size, int double_plus)
{
  int i, j;
  C_uword count, n, bytes;
  C_word *p, **msp, item, last;
  C_header h;
  C_byte *tmp, *start;
  LF_LIST *lfn;
  C_SCHEME_BLOCK *bp;
  WEAK_TABLE_ENTRY *wep;
  C_GC_ROOT *gcrp;
  C_SYMBOL_TABLE *stp;
  FINALIZER_NODE *flist;
  TRACE_INFO *tinfo;

  if(double_plus) size = heap_size * 2 + size;

  if(size < MINIMAL_HEAP_SIZE) size = MINIMAL_HEAP_SIZE;

  if(size > C_maximal_heap_size) size = C_maximal_heap_size;

  if(size == heap_size) return;

  if(debug_mode) 
    C_printf(C_text("[debug] resizing heap dynamically from " UWORD_FORMAT_STRING " to " UWORD_FORMAT_STRING " bytes...\n"), 
	   (C_uword)heap_size, size);

  if(gc_report_flag) {
    C_printf(C_text("(old) fromspace: \tstart=%08lx, \tlimit=%08lx\n"), (long)fromspace_start, (long)C_fromspace_limit);
    C_printf(C_text("(old) tospace:   \tstart=%08lx, \tlimit=%08lx\n"), (long)tospace_start, (long)tospace_limit);
  }

  heap_size = size;
  size /= 2;

  if((new_tospace_start = (C_byte *)C_malloc(size)) == NULL)
    panic(C_text("out of memory - can not allocate heap segment"));

  new_tospace_start = (C_byte *)C_align((long)new_tospace_start);
  new_tospace_top = new_tospace_start;
  new_tospace_limit = new_tospace_start + size;
  heap_scan_top = new_tospace_top;

  /* Mark items in forwarding table: */
  for(p = forwarding_table; *p != 0; p += 2) {
    last = p[ 1 ];
    remark(&p[ 1 ]);
    C_block_header(p[ 0 ]) = C_block_header(last);
  }

  /* Mark literal frames: */
  for(lfn = lf_list; lfn != NULL; lfn = lfn->next)
    for(i = 0; i < lfn->count; remark(&lfn->lf[ i++ ]));

  /* Mark symbol table: */
  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    for(i = 0; i < stp->size; remark(&stp->table[ i++ ]));

  /* Mark collectibles: */
  for(msp = collectibles; msp < collectibles_top; ++msp)
    if(*msp != NULL) remark(*msp);

  for(gcrp = gc_root_list; gcrp != NULL; gcrp = gcrp->next)
    remark(&gcrp->value);

  remark_system_globals();

  /* Clear the mutated slot stack: */
  mutation_stack_top = mutation_stack_bottom;

  /* Mark live values: */
  for(p = C_temporary_stack; p < C_temporary_stack_bottom; remark(p++));

  /* Mark locative table: */
  for(i = 0; i < locative_table_count; ++i)
    remark(&locative_table[ i ]);

  /* Mark finalizer table: */
  for(flist = finalizer_list; flist != NULL; flist = flist->next) {
    remark(&flist->item);
    remark(&flist->finalizer);
  }

  /* Mark weakly held items: */
  if(C_enable_gcweak) {
    wep = weak_item_table; 

    for(i = 0; i < WEAK_TABLE_SIZE; ++i, ++wep)
      if(wep->item != 0) remark(&wep->item);
  }

  /* Mark trace-buffer: */
  for(tinfo = trace_buffer; tinfo < trace_buffer_limit; ++tinfo) {
    remark(&tinfo->cooked1);
    remark(&tinfo->cooked2);
    remark(&tinfo->thread);
  }

  update_locative_table(GC_REALLOC);

  /* Mark nested values in already moved (marked) blocks in breadth-first manner: */
  while(heap_scan_top < new_tospace_top) {
    bp = (C_SCHEME_BLOCK *)heap_scan_top;

    if(*((C_word *)bp) == ALIGNMENT_HOLE_MARKER) 
      bp = (C_SCHEME_BLOCK *)((C_word *)bp + 1);

    n = C_header_size(bp);
    h = bp->header;
    assert(!is_fptr(h));
    bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);
    p = bp->data;

    if(n > 0 && (h & C_BYTEBLOCK_BIT) == 0) {
      if(h & C_SPECIALBLOCK_BIT) {
	--n;
	++p;
      }

      while(n--) remark(p++);
    }

    heap_scan_top = (C_byte *)bp + C_align(bytes) + sizeof(C_word);
  }

  C_free(tospace_start);
  C_free(fromspace_start);
  
  if((tospace_start = (C_byte *)C_malloc(size)) == NULL)
    panic(C_text("out ot memory - can not allocate heap segment"));

  tospace_start = (C_byte *)C_align((long)tospace_start);
  tospace_limit = tospace_start + size;
  tospace_top = tospace_start;
  fromspace_start = new_tospace_start;
  C_fromspace_top = new_tospace_top;
  C_fromspace_limit = new_tospace_limit;
  
  if(gc_report_flag) {
    C_printf(C_text("[GC] resized heap to %d bytes\n"), heap_size);
    C_printf(C_text("(new) fromspace: \tstart=%08lx, \tlimit=%08lx\n"), (long)fromspace_start, (long)C_fromspace_limit);
    C_printf(C_text("(new) tospace:   \tstart=%08lx, \tlimit=%08lx\n"), (long)tospace_start, (long)tospace_limit);
  }
}


C_regparm void C_fcall remark_system_globals(void)
{
  remark(&interrupt_hook_symbol);
  remark(&error_hook_symbol);
  remark(&callback_continuation_stack_symbol);
  remark(&pending_finalizers_symbol);
  remark(&invalid_procedure_call_hook_symbol);
  remark(&unbound_variable_value_hook_symbol);
  remark(&last_invalid_procedure_symbol);
  remark(&identity_unbound_value_symbol);
  remark(&current_thread_symbol);
  remark(&apply_hook_symbol);
  remark(&last_applied_procedure_symbol);
}


C_regparm void C_fcall remark(C_word *x)
{
  C_word val, item;
  C_uword n, bytes;
  C_header h;
  C_SCHEME_BLOCK *p, *p2;
  WEAK_TABLE_ENTRY *wep;

  val = *x;

  if(C_immediatep(val)) return;

  p = (C_SCHEME_BLOCK *)val;
  
  /* not in stack and not in heap? */
  if(
#if C_STACK_GROWS_DOWNWARD
       p < (C_SCHEME_BLOCK *)C_stack_pointer || p >= (C_SCHEME_BLOCK *)stack_bottom
#else
       p >= (C_SCHEME_BLOCK *)C_stack_pointer || p < (C_SCHEME_BLOCK *)stack_bottom
#endif
    )
    if((p < (C_SCHEME_BLOCK *)fromspace_start || p >= (C_SCHEME_BLOCK *)C_fromspace_limit) &&
       (p < (C_SCHEME_BLOCK *)tospace_start || p >= (C_SCHEME_BLOCK *)tospace_limit) &&
       (p < (C_SCHEME_BLOCK *)new_tospace_start || p >= (C_SCHEME_BLOCK *)new_tospace_limit) ) {
#ifdef C_GC_HOOKS
      if(C_gc_trace_hook != NULL) 
	C_gc_trace_hook(x, gc_mode);
#endif

      return;
    }

  h = p->header;

  if(is_fptr(h)) {
    val = fptr_to_ptr(h);

    if((C_uword)val >= (C_uword)new_tospace_start && (C_uword)val < (C_uword)new_tospace_top) {
      *x = val;
      return;
    }

    /* Link points into nursery, fromspace or the old tospace: fetch new pointer + header and copy... */
    p = (C_SCHEME_BLOCK *)val;
    h = p->header;
    n = 1;

    while(is_fptr(h)) {
      /* Link points into fromspace or old tospace and into a link which points into tospace or new-tospace: */
      val = fptr_to_ptr(h);
	
      if((C_uword)val >= (C_uword)new_tospace_start && (C_uword)val < (C_uword)new_tospace_top) {
	*x = val;
	return;
      }

      p = (C_SCHEME_BLOCK *)val;
      h = p->header;

      if(++n > 3)
	panic(C_text("forwarding chain during re-reclamation is longer than 3. somethings fishy."));
    }
  }

  p2 = (C_SCHEME_BLOCK *)C_align((C_uword)new_tospace_top);

#ifndef C_SIXTY_FOUR
  if((h & C_8ALIGN_BIT) && aligned8(p2) && (C_byte *)p2 < new_tospace_limit) {
    *((C_word *)p2) = ALIGNMENT_HOLE_MARKER;
    p2 = (C_SCHEME_BLOCK *)((C_word *)p2 + 1);
  }
#endif

  n = C_header_size(p);
  bytes = (h & C_BYTEBLOCK_BIT) ? n : n * sizeof(C_word);

  if(((C_byte *)p2 + bytes + sizeof(C_word)) > new_tospace_limit) {
    panic(C_text("out of memory - heap full while resizing"));
  }

  new_tospace_top = (C_byte *)p2 + C_align(bytes) + sizeof(C_word);
  *x = (C_word)p2;
  p2->header = h;
  assert(!is_fptr(h));
  p->header = ptr_to_fptr((C_word)p2);
  C_memcpy(p2->data, p->data, bytes);
}


C_regparm void C_fcall update_locative_table(int mode)
{
  int i, hi = 0;
  C_header h;
  C_word loc, obj, obj2, offset, loc2, ptr;
  C_uword ptr2;

  /*  C_printf("major: %d, %d locs in %d\n", major, locative_table_count, locative_table_size); */

  for(i = 0; i < locative_table_count; ++i) {
    loc = locative_table[ i ];
    /*    C_printf("%d: %08lx %d/%d\n", i, loc, C_in_stackp(loc), C_in_heapp(loc)); */

    if(loc != C_SCHEME_UNDEFINED) {
      h = C_block_header(loc);

      switch(mode) {
      case GC_MINOR:
        if(is_fptr(h))		/* forwarded? update l-table entry */
          loc = locative_table[ i ] = fptr_to_ptr(h);
        else if(C_in_stackp(loc)) { /* otherwise it must have been GC'd (since this is a minor one) */
          locative_table[ i ] = C_SCHEME_UNDEFINED;
          C_set_block_item(loc, 0, 0);
          break;
        }

        ptr = C_block_item(loc, 0); /* forwarded. fix up ptr and check pointed-at object for being forwarded... */
        offset = C_unfix(C_block_item(loc, 1));
        obj = ptr - offset;
        h = C_block_header(obj);

        if(is_fptr(h)) {	/* pointed-at object forwarded? update */
          C_set_block_item(loc, 0, (C_uword)fptr_to_ptr(h) + offset);
	  hi = i + 1;
	}
        else if(C_in_stackp(obj)) { /* pointed-at object GC'd, locative is invalid */
          locative_table[ i ] = C_SCHEME_UNDEFINED;
          C_set_block_item(loc, 0, 0);
        }
	else hi = i + 1;
        
        break;

      case GC_MAJOR:
        if(is_fptr(h))		/* forwarded? update l-table entry */
          loc = locative_table[ i ] = fptr_to_ptr(h);
        else {			/* otherwise, throw away */
          locative_table[ i ] = C_SCHEME_UNDEFINED;
          C_set_block_item(loc, 0, 0);
          break;
        }

        h = C_block_header(loc);
        
        if(is_fptr(h))		/* new instance is forwarded itself? update again */
          loc = locative_table[ i ] = fptr_to_ptr(h);

        ptr = C_block_item(loc, 0); /* fix up ptr */
        offset = C_unfix(C_block_item(loc, 1));
        obj = ptr - offset;
        h = C_block_header(obj);

        if(is_fptr(h)) {	/* pointed-at object has been forwarded? */
	  ptr2 = (C_uword)fptr_to_ptr(h);
	  h = C_block_header(ptr2);

	  if(is_fptr(h)) {	/* secondary forwarding check for pointed-at object */
	    ptr2 = (C_uword)fptr_to_ptr(h) + offset;
	    C_set_block_item(loc, 0, ptr2);
	  }
	  else C_set_block_item(loc, 0, ptr2 + offset); /* everything's fine, fixup pointer */

	  hi = i + 1;
        }
        else {
          locative_table[ i ] = C_SCHEME_UNDEFINED; /* pointed-at object is dead */
          C_set_block_item(loc, 0, 0);
        }
        
        break;

      case GC_REALLOC:
        ptr = C_block_item(loc, 0); /* just update ptr's pointed-at objects */
        offset = C_unfix(C_block_item(loc, 1));
        obj = ptr - offset;
        remark(&obj);
        C_set_block_item(loc, 0, obj + offset);        
        break;
      }
    }
  }

  if(mode != GC_REALLOC) locative_table_count = hi;
}


C_regparm WEAK_TABLE_ENTRY *C_fcall lookup_weak_table_entry(C_word item, C_word container)
{
  int key = (C_uword)item >> 2,
      disp = 0,
      n;
  WEAK_TABLE_ENTRY *wep;

  for(n = 0; n < WEAK_HASH_ITERATIONS; ++n) {
    key = (key + disp) % WEAK_TABLE_SIZE;
    wep = &weak_item_table[ key ];

    if(wep->item == 0) {
      if(container != 0) {
	/* Add fresh entry: */
	wep->item = item;
	wep->container = container;
	return wep;
      }

      return NULL;
    }
    else if(wep->item == item) return wep;
    else disp += WEAK_HASH_DISPLACEMENT;
  }

  return NULL;
}


void handle_interrupt(void *trampoline, void *proc)
{
  C_word *p, x, n;
  int i;
  long c;

  /* Build vector with context information: */
  n = C_temporary_stack_bottom - C_temporary_stack;
  p = C_alloc(19 + n); /* 19 <=> 2 headers + trampoline + proc + 1 extra slot + 9 for interning + 5 for string */
  x = (C_word)p;
  *(p++) = C_VECTOR_TYPE | C_BYTEBLOCK_BIT | (2 * sizeof(C_word));
  *(p++) = (C_word)trampoline;
  *(p++) = (C_word)proc;
  C_save(x);
  x = (C_word)p;
  *(p++) = C_VECTOR_TYPE | (n + 1);
  *(p++) = C_restore;
  C_memcpy(p, C_temporary_stack, n * sizeof(C_word));

  /* Restore state to the one at the time of the interrupt: */
  C_temporary_stack = C_temporary_stack_bottom;
  i = interrupt_reason;
  interrupt_reason = 0;
  C_stack_limit = saved_stack_limit;

  /* Invoke high-level interrupt handler: */
  C_save(C_fix(i));
  C_save(x);
  x = C_block_item(interrupt_hook_symbol, 0);

  if(C_immediatep(x))
    panic(C_text("`##sys#interrupt-hook' is not defined"));

  c = cpu_milliseconds() - interrupt_time;
  last_interrupt_latency = c;
  C_timer_interrupt_counter = C_initial_timer_interrupt_period;	/* just in case */
  C_do_apply(2, x, C_SCHEME_UNDEFINED);  /* <- no continuation is passed: "##sys#interrupt-hook" may not return! */
}


C_regparm C_word C_fcall C_retrieve(C_word sym)
{
  C_word val = C_u_i_car(sym);

  if(val == C_SCHEME_UNBOUND)
    return C_get_unbound_variable_value_hook(sym);

  return val;
}


C_word get_unbound_variable_value(C_word sym)
{
  C_word x = C_block_item(unbound_variable_value_hook_symbol, 0);

  if(x == identity_unbound_value_symbol) return sym;
  else if(x == C_SCHEME_FALSE)
    barf(C_UNBOUND_VARIABLE_ERROR, NULL, sym);

  return C_block_item(x, 0);
}


C_regparm C_word C_fcall C_retrieve2(C_word val, char *name)
{
  C_word *p;
  int len;

  if(val == C_SCHEME_UNBOUND) {
    len = C_strlen(name);
    p = C_alloc(C_SIZEOF_STRING(len));	/* this is ok: we won't return from `C_retrieve2' (or the value isn't needed). */
    return get_unbound_variable_value(C_string2(&p, name));
  }

  return val;
}


#ifndef C_UNSAFE_RUNTIME
static C_word resolve_procedure(C_word closure, C_char *where)
{
  C_word s;

  if(C_immediatep(closure) || C_header_bits(closure) != C_CLOSURE_TYPE) {
    s = C_block_item(invalid_procedure_call_hook_symbol, 0);

    if(s == C_SCHEME_FALSE)
      barf(C_NOT_A_CLOSURE_ERROR, where, closure);

    C_mutate(&C_block_item(last_invalid_procedure_symbol, 0), closure);
    closure = s;
  }

  return closure;
}
#endif


C_regparm void *C_fcall C_retrieve_proc(C_word closure)
{
  closure = resolve_procedure(closure, NULL);

#ifndef C_NO_APPLY_HOOK
  if(C_block_item(apply_hook_symbol, 0) != C_SCHEME_FALSE) {
    C_mutate(&C_block_item(last_applied_procedure_symbol, 0), closure);
    return (void *)C_block_item(C_block_item(apply_hook_symbol, 0), 0);
  }
#endif

  return (void *)C_block_item(closure, 0);
}


C_regparm void C_fcall C_trace(C_char *name)
{
  if(show_trace) {
    C_fputs(name, C_stderr);
    C_fputc('\n', C_stderr);
  }

  if(trace_buffer_top >= trace_buffer_limit) {
    trace_buffer_top = trace_buffer;
    trace_buffer_full = 1;
  }

  trace_buffer_top->raw = name;
  trace_buffer_top->cooked1 = C_SCHEME_FALSE;
  trace_buffer_top->cooked2 = C_SCHEME_FALSE;
  trace_buffer_top->thread = C_block_item(current_thread_symbol, 0);
  ++trace_buffer_top;
}


/* DEPRECATED: throw out at some stage: */
C_regparm C_word C_fcall C_emit_trace_info(C_word x, C_word y, C_word t)
{
  if(trace_buffer_top >= trace_buffer_limit) {
    trace_buffer_top = trace_buffer;
    trace_buffer_full = 1;
  }

  trace_buffer_top->raw = "<eval>";
  trace_buffer_top->cooked1 = x;
  trace_buffer_top->cooked2 = y;
  trace_buffer_top->thread = t;
  ++trace_buffer_top;
  return x;
}


C_regparm C_word C_fcall C_emit_trace_info2(char *raw, C_word x, C_word y, C_word t)
{
  if(trace_buffer_top >= trace_buffer_limit) {
    trace_buffer_top = trace_buffer;
    trace_buffer_full = 1;
  }

  trace_buffer_top->raw = raw;
  trace_buffer_top->cooked1 = x;
  trace_buffer_top->cooked2 = y;
  trace_buffer_top->thread = t;
  ++trace_buffer_top;
  return x;
}


C_char *C_dump_trace(int start)
{
  TRACE_INFO *ptr;
  C_char *result;
  int i;

  if((result = (char *)C_malloc(STRING_BUFFER_SIZE)) == NULL)
    horror(C_text("out of memory - can not allocate trace-dump buffer"));

  *result = '\0';

  if(trace_buffer_top > trace_buffer || trace_buffer_full) {
    if(trace_buffer_full) {
      i = C_trace_buffer_size;
      C_strcat(result, C_text("...more...\n"));
    }
    else i = trace_buffer_top - trace_buffer;

    ptr = trace_buffer_full ? trace_buffer_top : trace_buffer;
    ptr += start;
    i -= start;

    for(;i--; ++ptr) {
      if(ptr >= trace_buffer_limit) ptr = trace_buffer;

      if(C_strlen(result) > STRING_BUFFER_SIZE - 32) {
	if((result = C_realloc(result, C_strlen(result) * 2)) == NULL)
	  horror(C_text("out of memory - can not reallocate trace-dump buffer"));
      }

      C_strcat(result, ptr->raw);

      if(i > 0) C_strcat(result, "\n");
      else C_strcat(result, " \t<--\n");
    }
  }

  return result;
}


C_regparm void C_fcall C_clear_trace_buffer(void)
{
  int i;

  if(trace_buffer == NULL) {
    trace_buffer = (TRACE_INFO *)C_malloc(sizeof(TRACE_INFO) * C_trace_buffer_size);

    if(trace_buffer == NULL)
      panic(C_text("out of memory - can not allocate trace-buffer"));
  }

  trace_buffer_top = trace_buffer;
  trace_buffer_limit = trace_buffer + C_trace_buffer_size;
  trace_buffer_full = 0;

  for(i = 0; i < C_trace_buffer_size; ++i) {
    trace_buffer[ i ].cooked1 = C_SCHEME_FALSE;
    trace_buffer[ i ].cooked2 = C_SCHEME_FALSE;
    trace_buffer[ i ].thread = C_SCHEME_FALSE;
  }
}


C_word C_fetch_trace(C_word starti, C_word buffer)
{
  TRACE_INFO *ptr;
  int i, p = 0, start = C_unfix(starti);

  if(trace_buffer_top > trace_buffer || trace_buffer_full) {
    if(trace_buffer_full) i = C_trace_buffer_size;
    else i = trace_buffer_top - trace_buffer;

    ptr = trace_buffer_full ? trace_buffer_top : trace_buffer;
    ptr += start;
    i -= start;

    if(C_header_size(buffer) < i * 4)
      panic(C_text("destination buffer too small for call-chain"));

    for(;i--; ++ptr) {
      if(ptr >= trace_buffer_limit) ptr = trace_buffer;

      C_mutate(&C_block_item(buffer, p++), (C_word)ptr->raw); /* outside-pointer, will be ignored by GC */
      C_mutate(&C_block_item(buffer, p++), ptr->cooked1);
      C_mutate(&C_block_item(buffer, p++), ptr->cooked2);
      C_mutate(&C_block_item(buffer, p++), ptr->thread);
    }
  }

  return C_fix(p);
}


C_regparm C_word C_fcall C_hash_string(C_word str)
{
  unsigned C_word key = 0;
  int len = C_header_size(str);
  C_byte *ptr = C_data_pointer(str);

  while(len--) key = (key << 4) + *(ptr++);

  return C_fix(key & C_MOST_POSITIVE_FIXNUM);
}


C_regparm C_word C_fcall C_hash_string_ci(C_word str)
{
  unsigned C_word key = 0;
  int len = C_header_size(str);
  C_byte *ptr = C_data_pointer(str);

  while(len--) key = (key << 4) + C_tolower(*(ptr++));

  return C_fix(key & C_MOST_POSITIVE_FIXNUM);
}


C_regparm void C_fcall C_toplevel_entry(C_char *name)
{
  if(debug_mode) {
    C_printf(C_text("[debug] entering toplevel %s...\n"), name);
    C_fflush(stdout);
  }
}


C_word C_halt(C_word msg)
{
  C_char *dmp = msg != C_SCHEME_FALSE ? C_dump_trace(0) : NULL;

#ifdef C_MICROSOFT_WINDOWS
  if(msg != C_SCHEME_FALSE) {
    int n = C_header_size(msg);

    C_strncpy(buffer, (C_char *)C_data_pointer(msg), n);
    buffer[ n ] = '\0';
  }
  else C_strcpy(buffer, C_text("(aborted)"));

  C_strcat(buffer, C_text("\n\n"));

  if(dmp != NULL) C_strcat(buffer, dmp);

  MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK);
#else
  if(msg != C_SCHEME_FALSE) {
    C_fwrite(C_data_pointer(msg), C_header_size(msg), sizeof(C_char), C_stderr);
    C_fputc('\n', C_stderr);
  }

  if(dmp != NULL) C_fprintf(stderr, C_text("\n%s"), dmp);
#endif
  
  C_exit(EX_SOFTWARE);
  return 0;
}


C_word C_message(C_word msg)
{
#ifdef C_MICROSOFT_WINDOWS
  int n = C_header_size(msg);

  C_strncpy(buffer, (C_char *)((C_SCHEME_BLOCK *)msg)->data, n);
  buffer[ n ] = '\0';
  MessageBox(NULL, buffer, C_text("CHICKEN runtime"), MB_OK);
#else
  C_fwrite(((C_SCHEME_BLOCK *)msg)->data, C_header_size(msg), sizeof(C_char), stdout);
  C_putchar('\n');
#endif
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_equalp(C_word x, C_word y)
{
  C_header header;
  C_word bits, n, i;

  C_stack_check;

 loop:
  if(x == y) return 1;

  if(C_immediatep(x) || C_immediatep(y)) return 0;

  if((header = C_block_header(x)) != C_block_header(y)) return 0;
  else if((bits = header & C_HEADER_BITS_MASK) & C_BYTEBLOCK_BIT) {
    if(header == C_FLONUM_TAG && C_block_header(y) == C_FLONUM_TAG)
      return C_flonum_magnitude(x) == C_flonum_magnitude(y);
    else return !C_memcmp(C_data_pointer(x), C_data_pointer(y), header & C_HEADER_SIZE_MASK);
  }
  else if(header == C_SYMBOL_TAG) return 0;
  else {
    i = 0;
    n = header & C_HEADER_SIZE_MASK;

    if(bits & C_SPECIALBLOCK_BIT) {
      if(C_u_i_car(x) != C_u_i_car(y)) return 0;
      else ++i;

      if(n == 1) return 1;
    }

    if(--n < 0) return 1;

    while(i < n)
      if(!C_equalp(C_block_item(x, i), C_block_item(y, i))) return 0;
      else ++i;

    x = C_block_item(x, i);
    y = C_block_item(y, i);
    goto loop;
  }
    
  return 1;
}


C_regparm C_word C_fcall C_set_gc_report(C_word flag)
{
  if(flag == C_SCHEME_FALSE) gc_report_flag = 0;
  else if(flag == C_SCHEME_TRUE) gc_report_flag = 2;
  else gc_report_flag = 1;

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_start_timer(void)
{
  timer_start_mutation_count = mutation_count;
  timer_start_gc_count_1 = gc_count_1;
  timer_start_gc_count_2 = gc_count_2;
  timer_start_fromspace_top = C_fromspace_top;
  timer_start_ms = cpu_milliseconds();
  timer_start_gc_ms = 0;
  return C_SCHEME_UNDEFINED;
}


void C_ccall C_stop_timer(C_word c, C_word closure, C_word k)
{
  long t0 = cpu_milliseconds() - timer_start_ms;
  int gc2 = gc_count_2 - timer_start_gc_count_2,
      gc1 = gc2 ? gc_count_1 : (gc_count_1 - timer_start_gc_count_1),
      mutations = mutation_count - timer_start_mutation_count,
      from = gc2 ? ((C_uword)C_fromspace_top - (C_uword)fromspace_start)
                 : ((C_uword)C_fromspace_top - (C_uword)timer_start_fromspace_top);
  C_word 
    ab[ WORDS_PER_FLONUM * 2 + 7 ], /* 2 flonums, 1 vector of 6 elements */
    *a = ab,
    elapsed = C_flonum(&a, (double)t0 / 1000.0),
    gc_time = C_flonum(&a, (double)timer_start_gc_ms / 1000.0),
    info;

  info = C_vector(&a, 6, elapsed, gc_time, C_fix(mutations), C_fix(gc1), C_fix(gc2), C_fix(from));
  C_kontinue(k, info);
}


C_word C_exit_runtime(C_word code)
{
  exit(C_unfix(code));
  return 0;			/* to please the compiler... */
}


C_regparm C_word C_fcall C_display_flonum(C_word port, C_word n)
{
  C_FILEPTR fp = C_port_file(port);

#ifdef HAVE_GCVT
  C_fprintf(fp, C_text("%s"), C_gcvt(C_flonum_magnitude(n), FLONUM_PRINT_PRECISION, buffer));
#else
  C_fprintf(fp, C_text("%.*g"), FLONUM_PRINT_PRECISION, C_flonum_magnitude(n));
#endif
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_read_char(C_word port)
{
  int c = C_fgetc(C_port_file(port));

  return c == EOF ? C_SCHEME_END_OF_FILE : C_make_character(c);
}


C_regparm C_word C_fcall C_peek_char(C_word port)
{
  C_FILEPTR fp = C_port_file(port);
  int c = C_fgetc(fp);

  C_ungetc(c, fp);
  return c == EOF ? C_SCHEME_END_OF_FILE : C_make_character(c);
}


C_regparm C_word C_fcall C_execute_shell_command(C_word string)
{
  int n = C_header_size(string);
  char *buf = buffer;

  if(n >= STRING_BUFFER_SIZE) {
    if((buf = (char *)C_malloc(n + 1)) == NULL)
      barf(C_OUT_OF_MEMORY_ERROR, "system");
  }

  C_memcpy(buf, ((C_SCHEME_BLOCK *)string)->data, n);
  buf[ n ] = '\0';

  n = C_system(buf);

  if(buf != buffer) C_free(buf);

  if(n == -1)
    return C_fix(errno);

#ifdef C_NONUNIX
  return C_fix(n);
#else
  return C_fix(WIFEXITED(n) ? WEXITSTATUS(n) : (WIFSIGNALED(n) ? WTERMSIG(n) : WSTOPSIG(n)));
#endif
}


C_regparm C_word C_fcall C_string_to_pbytevector(C_word s)
{
  return C_pbytevector(C_header_size(s), C_data_pointer(s));
}


C_regparm C_word C_fcall C_char_ready_p(C_word port)
{
#if !defined(C_NONUNIX)
  fd_set fs;
  struct timeval to;
  int fd = C_fileno(C_port_file(port));

  FD_ZERO(&fs);
  FD_SET(fd, &fs);
  to.tv_sec = to.tv_usec = 0;
  return C_mk_bool(C_select(fd + 1, &fs, NULL, NULL, &to) == 1);
#else
  return C_SCHEME_TRUE;
#endif
}


C_regparm C_word C_fcall C_flush_output(C_word port)
{
  C_fflush(C_port_file(port));
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_fudge(C_word fudge_factor)
{
  int i, j;
  long tgc;

  switch(fudge_factor) {
  case C_fix(1): return C_SCHEME_END_OF_FILE;
  case C_fix(2): 
    /* can be considered broken (overflows into negatives), but is useful for randomize */
    return C_fix(C_MOST_POSITIVE_FIXNUM & time(NULL));

  case C_fix(3):
#ifdef C_SIXTY_FOUR
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(4):
#ifdef C_GENERIC_CONSOLE
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(5):
#ifdef C_GENERIC_CONSOLE
    return C_fix(0);
#elif defined(C_WINDOWS_GUI)
    return C_fix(1);
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(6): 
    return C_fix(C_MOST_POSITIVE_FIXNUM & cpu_milliseconds());

  case C_fix(7):
    return C_fix(sizeof(C_word));

  case C_fix(8):
    return C_fix(C_wordsperdouble(1));

  case C_fix(9):
    return C_fix(last_interrupt_latency);

  case C_fix(10):
    return C_fix(CLOCKS_PER_SEC);

  case C_fix(11):
#ifdef MACINTOSH_GUI
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(12):
    return C_mk_bool(fake_tty_flag);

  case C_fix(13):
    return C_mk_bool(debug_mode);

  case C_fix(14):
    return C_mk_bool(C_interrupts_enabled);

  case C_fix(15):
    return C_mk_bool(C_enable_gcweak);

  case C_fix(16):
    return C_fix(C_MOST_POSITIVE_FIXNUM & milliseconds());

  case C_fix(17):
    return(C_mk_bool(C_heap_size_is_fixed));

  case C_fix(18):
    return(C_fix(C_STACK_GROWS_DOWNWARD));

  case C_fix(19):
    for(i = j = 0; i < locative_table_count; ++i)
      if(locative_table[ i ] != C_SCHEME_UNDEFINED) ++j;
 
    return C_fix(j);

  case C_fix(20):
#ifdef C_UNSAFE_RUNTIME
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(21):
    return C_fix(C_MOST_POSITIVE_FIXNUM);

  case C_fix(22):
#if defined(HAVE_FFI_H)
    return C_SCHEME_TRUE;
#else 
    return C_SCHEME_FALSE;
#endif

  case C_fix(23):
    return C_fix(C_startup_time_seconds);

  case C_fix(24):
#ifdef NO_DLOAD2
    return C_SCHEME_FALSE;
#else
    return C_SCHEME_TRUE;
#endif

  case C_fix(25):
    return C_mk_bool(C_enable_repl);

  case C_fix(26):
    return C_fix(live_finalizer_count);

  case C_fix(27):
    return C_fix(allocated_finalizer_count);

  case C_fix(28):
#ifdef C_ENABLE_PTABLES
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(29):
    return C_fix(C_trace_buffer_size);

  case C_fix(30):
#ifdef _MSC_VER
    return C_fix(_MSC_VER);
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(31):
    tgc = timer_accumulated_gc_ms;
    timer_accumulated_gc_ms = 0;
    return C_fix(tgc);

  case C_fix(32):
#ifdef C_GC_HOOKS
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(33):
#ifdef C_EXTRA_SYMBOL_SLOT
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(34):
#ifdef HAVE_FFI_H
    return C_fix(1000);
#else
    return C_fix(126);
#endif

  case C_fix(35):
#ifndef C_NO_APPLY_HOOK
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(38):
#ifdef CMAKE_BUILD
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  case C_fix(39):
#if defined(C_CROSS_CHICKEN) && C_CROSS_CHICKEN
    return C_SCHEME_TRUE;
#else
    return C_SCHEME_FALSE;
#endif

  default: return C_SCHEME_UNDEFINED;
  }
}


C_regparm void C_fcall C_paranoid_check_for_interrupt(void)
{
  if(--C_timer_interrupt_counter <= 0)
    C_raise_interrupt(C_TIMER_INTERRUPT_NUMBER);
}


C_regparm void C_fcall C_raise_interrupt(int reason)
{
  if(C_interrupts_enabled) {
    saved_stack_limit = C_stack_limit;

#if C_STACK_GROWS_DOWNWARD
    C_stack_limit = C_stack_pointer + 1000;
#else
    C_stack_limit = C_stack_pointer - 1000;
#endif

    interrupt_reason = reason;
    interrupt_time = cpu_milliseconds();
  }
}


C_regparm C_word C_fcall C_set_initial_timer_interrupt_period(C_word n)
{
  C_initial_timer_interrupt_period = C_unfix(n);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_enable_interrupts(void)
{
  C_timer_interrupt_counter = C_initial_timer_interrupt_period;
  /* assert(C_timer_interrupt_counter > 0); */
  C_interrupts_enabled = 1;
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_disable_interrupts(void)
{
  C_interrupts_enabled = 0;
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_establish_signal_handler(C_word signum, C_word reason)
{
  int sig = C_unfix(signum);

  if(reason == C_SCHEME_FALSE) C_signal(sig, SIG_IGN);
  else {
    signal_mapping_table[ sig ] = C_unfix(reason);
    C_signal(sig, global_signal_handler);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_flonum_in_fixnum_range_p(C_word n)
{
  double f = C_flonum_magnitude(n);

  return C_mk_bool(f <= (double)C_MOST_POSITIVE_FIXNUM && f >= (double)C_MOST_NEGATIVE_FIXNUM);
}


C_regparm C_word C_fcall C_double_to_number(C_word n)
{
  double m, f = C_flonum_magnitude(n);

  if(f <= (double)C_MOST_POSITIVE_FIXNUM && f >= (double)C_MOST_NEGATIVE_FIXNUM && modf(f, &m) == 0.0) 
    return C_fix(f);
  else return f;
}


C_regparm C_word C_fcall C_fits_in_int_p(C_word x)
{
  double n, m;

  if(x & C_FIXNUM_BIT) return C_SCHEME_TRUE;

  n = C_flonum_magnitude(x);
  return C_mk_bool(modf(n, &m) == 0.0 && n >= C_WORD_MIN && n <= C_WORD_MAX);
}


C_regparm C_word C_fcall C_fits_in_unsigned_int_p(C_word x)
{
  double n, m;

  if(x & C_FIXNUM_BIT) return C_SCHEME_TRUE;

  n = C_flonum_magnitude(x);
  return C_mk_bool(modf(n, &m) == 0.0 && n >= 0 && n <= C_UWORD_MAX);
}


/* Copy blocks into collected or static memory: */

C_regparm C_word C_fcall C_copy_block(C_word from, C_word to)
{
  int n = C_header_size(from);
  long bytes;

  if(C_header_bits(from) & C_BYTEBLOCK_BIT) {
    bytes = n;
    C_memcpy((C_SCHEME_BLOCK *)to, (C_SCHEME_BLOCK *)from, bytes + sizeof(C_header));
  }
  else {
    bytes = C_wordstobytes(n);
    C_memcpy((C_SCHEME_BLOCK *)to, (C_SCHEME_BLOCK *)from, bytes + sizeof(C_header));
  }

  return to;
}


C_regparm C_word C_fcall C_evict_block(C_word from, C_word ptr)
{
  int n = C_header_size(from);
  long bytes;
  C_word *p = (C_word *)C_pointer_address(ptr);

  if(C_header_bits(from) & C_BYTEBLOCK_BIT) bytes = n;
  else bytes = C_wordstobytes(n);

  C_memcpy(p, (C_SCHEME_BLOCK *)from, bytes + sizeof(C_header));
  return (C_word)p;
}


/* Conversion routines: */

C_regparm double C_fcall C_c_double(C_word x)
{
  if(x & C_FIXNUM_BIT) return (double)C_unfix(x);
  else return C_flonum_magnitude(x);
}


C_regparm C_word C_fcall C_num_to_int(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_unfix(x);
  else return (int)C_flonum_magnitude(x);
}


C_regparm C_s64 C_fcall C_num_to_int64(C_word x)
{
  if(x & C_FIXNUM_BIT) return (C_s64)C_unfix(x);
  else return (C_s64)C_flonum_magnitude(x);
}


C_regparm C_uword C_fcall C_num_to_unsigned_int(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_unfix(x);
  else return (unsigned int)C_flonum_magnitude(x);
}


C_regparm C_word C_fcall C_int_to_num(C_word **ptr, C_word n)
{
  if(C_fitsinfixnump(n)) return C_fix(n);
  else return C_flonum(ptr, (double)n);
}


C_regparm C_word C_fcall C_unsigned_int_to_num(C_word **ptr, C_uword n)
{
  if(C_ufitsinfixnump(n)) return C_fix(n);
  else return C_flonum(ptr, (double)n);
}


C_regparm C_word C_fcall C_long_to_num(C_word **ptr, long n)
{
  if(C_fitsinfixnump(n)) return C_fix(n);
  else return C_flonum(ptr, (double)n);
}


C_regparm C_word C_fcall C_unsigned_long_to_num(C_word **ptr, unsigned long n)
{
  if(C_ufitsinfixnump(n)) return C_fix(n);
  else return C_flonum(ptr, (double)n);
}


C_regparm C_word C_fcall C_flonum_in_int_range_p(C_word n)
{
  double m = C_flonum_magnitude(n);

  return C_mk_bool(m >= C_WORD_MIN && m <= C_WORD_MAX);
}


C_regparm C_word C_fcall C_flonum_in_uint_range_p(C_word n)
{
  double m = C_flonum_magnitude(n);

  return C_mk_bool(m >= 0 && m <= C_UWORD_MAX);
}


C_regparm char *C_fcall C_string_or_null(C_word x)
{
  return C_truep(x) ? C_c_string(x) : NULL;
}


C_regparm void *C_fcall C_data_pointer_or_null(C_word x) 
{
  return C_truep(x) ? C_data_pointer(x) : NULL;
}


C_regparm void *C_fcall C_srfi_4_vector_or_null(C_word x) 
{
  return C_truep(x) ? C_data_pointer(C_block_item(x, 1)) : NULL;
}


C_regparm void *C_fcall C_c_pointer_or_null(C_word x) 
{
  return C_truep(x) ? (void *)C_block_item(x, 0) : NULL;
}


C_regparm void *C_fcall C_scheme_or_c_pointer(C_word x) 
{
  return C_anypointerp(x) ? (void *)C_block_item(x, 0) : C_data_pointer(x);
}


C_regparm long C_fcall C_num_to_long(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_unfix(x);
  else return (long)C_flonum_magnitude(x);
}


C_regparm unsigned long C_fcall C_num_to_unsigned_long(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_unfix(x);
  else return (unsigned long)C_flonum_magnitude(x);
}


/* Inline versions of some standard procedures: */

C_regparm C_word C_fcall C_i_listp(C_word x)
{
  C_word fast = x, slow = x;

  while(fast != C_SCHEME_END_OF_LIST)
    if(!C_immediatep(fast) && C_block_header(fast) == C_PAIR_TAG) {
      fast = C_u_i_cdr(fast);
      
      if(fast == C_SCHEME_END_OF_LIST) return C_SCHEME_TRUE;
      else if(!C_immediatep(fast) && C_block_header(fast) == C_PAIR_TAG) {
	fast = C_u_i_cdr(fast);
	slow = C_u_i_cdr(slow);

	if(fast == slow) return C_SCHEME_FALSE;
      }
      else return C_SCHEME_FALSE;
    }
    else return C_SCHEME_FALSE;

  return C_SCHEME_TRUE;
}


C_regparm C_word C_fcall C_i_string_equal_p(C_word x, C_word y)
{
  C_word n;

  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string=?", x);

  if(C_immediatep(y) || C_header_bits(y) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string=?", y);

  n = C_header_size(x);

  return C_mk_bool(n == C_header_size(y) && !C_memcmp((char *)C_data_pointer(x), (char *)C_data_pointer(y), n));
}


C_regparm C_word C_fcall C_u_i_string_equal_p(C_word x, C_word y)
{
  C_word n;

  n = C_header_size(x);
  return C_mk_bool(n == C_header_size(y) && !C_memcmp((char *)C_data_pointer(x), (char *)C_data_pointer(y), n));
}


C_regparm C_word C_fcall C_i_string_ci_equal_p(C_word x, C_word y)
{
  C_word n;
  char *p1, *p2;

  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ci=?", x);

  if(C_immediatep(y) || C_header_bits(y) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ci=?", y);

  n = C_header_size(x);

  if(n != C_header_size(y)) return C_SCHEME_FALSE;

  p1 = (char *)C_data_pointer(x);
  p2 = (char *)C_data_pointer(y);

  while(n--) 
    if(C_tolower(*(p1++)) != C_tolower(*(p2++))) return C_SCHEME_FALSE;

  return C_SCHEME_TRUE;
}


C_regparm C_word C_fcall C_i_eqvp(C_word x, C_word y)
{
  return
    C_mk_bool(x == y ||
	      (!C_immediatep(x) && !C_immediatep(y) &&
	       C_block_header(x) == C_FLONUM_TAG && C_block_header(y) == C_FLONUM_TAG &&
	       C_flonum_magnitude(x) == C_flonum_magnitude(y) ) );
}


C_regparm C_word C_fcall C_i_symbolp(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_block_header(x) == C_SYMBOL_TAG);
}


C_regparm C_word C_fcall C_i_pairp(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_block_header(x) == C_PAIR_TAG);
}


C_regparm C_word C_fcall C_i_stringp(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_header_bits(x) == C_STRING_TYPE);
}


C_regparm C_word C_fcall C_i_locativep(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_block_header(x) == C_LOCATIVE_TAG);
}


C_regparm C_word C_fcall C_i_vectorp(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_header_bits(x) == C_VECTOR_TYPE);
}


C_regparm C_word C_fcall C_i_portp(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_header_bits(x) == C_PORT_TYPE);
}


C_regparm C_word C_fcall C_i_closurep(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_header_bits(x) == C_CLOSURE_TYPE);
}


C_regparm C_word C_fcall C_i_numberp(C_word x)
{
  return C_mk_bool((x & C_FIXNUM_BIT) || (!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG));
}


C_regparm C_word C_fcall C_i_integerp(C_word x)
{
  double dummy;

  return C_mk_bool((x & C_FIXNUM_BIT) || 
		   ((!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) &&
		    modf(C_flonum_magnitude(x), &dummy) == 0.0 ) );
}


C_regparm C_word C_fcall C_i_flonump(C_word x)
{
  return C_mk_bool(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG);
}


C_regparm C_word C_fcall C_i_finitep(C_word x)
{
  if((x & C_FIXNUM_BIT) != 0) return C_SCHEME_TRUE;
  else return C_mk_bool(!C_isinf(C_flonum_magnitude(x)));
}


C_regparm C_word C_fcall C_i_fixnum_min(C_word x, C_word y)
{
  return ((C_word)x < (C_word)y) ? x : y;
}


C_regparm C_word C_fcall C_i_fixnum_max(C_word x, C_word y)
{
  return ((C_word)x > (C_word)y) ? x : y;
}


C_regparm C_word C_fcall C_i_flonum_min(C_word x, C_word y)
{
  double 
    xf = C_flonum_magnitude(x),
    yf = C_flonum_magnitude(y);

  return xf < yf ? x : y;
}


C_regparm C_word C_fcall C_i_flonum_max(C_word x, C_word y)
{
  double 
    xf = C_flonum_magnitude(x),
    yf = C_flonum_magnitude(y);

  return xf > yf ? x : y;
}


#if !defined(__GNUC__) && !defined(__INTEL_COMPILER)

C_word *C_a_i(C_word **a, int n)
{
  C_word *p = *a;
  
  *a += n;
  return p;
}

#endif


C_word C_a_i_list(C_word **a, int c, ...)
{
  va_list v;
  C_word x, last, current,
         first = C_SCHEME_END_OF_LIST;

  va_start(v, c);

  for(last = C_SCHEME_UNDEFINED; c--; last = current) {
    x = va_arg(v, C_word);
    current = C_pair(a, x, C_SCHEME_END_OF_LIST);

    if(last != C_SCHEME_UNDEFINED)
      C_set_block_item(last, 1, current);
    else first = current;
  }

  va_end(v);
  return first;
}


C_word C_h_list(int c, ...)
{
  /* Similar to C_a_i_list(), but put slots with nursery data into mutation stack: */
  va_list v;
  C_word x, last, current,
         first = C_SCHEME_END_OF_LIST;

  va_start(v, c);

  for(last = C_SCHEME_UNDEFINED; c--; last = current) {
    x = va_arg(v, C_word);
    current = C_pair(C_heaptop, x, C_SCHEME_END_OF_LIST);

    if(C_in_stackp(x)) 
      C_mutate(&C_u_i_car(current), x);

    if(last != C_SCHEME_UNDEFINED)
      C_set_block_item(last, 1, current);
    else first = current;
  }

  va_end(v);
  return first;
}


C_word C_a_i_string(C_word **a, int c, ...)
{
  va_list v;
  C_word x, s = (C_word)(*a);
  char *p;

  *a = (C_word *)((C_word)(*a) + sizeof(C_header) + C_align(c));
  ((C_SCHEME_BLOCK *)s)->header = C_STRING_TYPE | c;
  p = (char *)C_data_pointer(s);
  va_start(v, c);

  while(c--) {
    x = va_arg(v, C_word);

    if((x & C_IMMEDIATE_TYPE_BITS) == C_CHARACTER_BITS)
      *(p++) = C_character_code(x);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "string", x);
  }

  return s;
}


C_word C_a_i_record(C_word **ptr, int n, ...)
{
  va_list v;
  C_word *p = *ptr,
         *p0 = p; 

  *(p++) = C_STRUCTURE_TYPE | n;
  va_start(v, n);

  while(n--)
    *(p++) = va_arg(v, C_word);

  *ptr = p;
  va_end(v);
  return (C_word)p0;
}


C_word C_a_i_port(C_word **ptr, int n)
{
  C_word 
    *p = *ptr,
    *p0 = p; 
  int i;

  *(p++) = C_PORT_TYPE | (C_SIZEOF_PORT - 1);
  *(p++) = (C_word)NULL;
  
  for(i = 0; i < C_SIZEOF_PORT - 2; ++i)
    *(p++) = C_SCHEME_FALSE;

  *ptr = p;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_a_i_bytevector(C_word **ptr, int c, C_word num)
{
  C_word *p = *ptr,
         *p0;
  int n = C_unfix(num);

#ifndef C_SIXTY_FOUR
  /* Align on 8-byte boundary: */
  if(aligned8(p)) ++p;
#endif

  p0 = p;
  *(p++) = C_BYTEVECTOR_TYPE | C_wordstobytes(n);
  *ptr = p + n;
  return (C_word)p0;
}


C_word C_fcall C_a_i_smart_mpointer(C_word **ptr, int c, C_word x)
{
  C_word 
    *p = *ptr,
    *p0 = p;
  void *mp;

  if(C_immediatep(x)) mp = NULL;
  else if((C_header_bits(x) && C_SPECIALBLOCK_BIT) != 0) mp = C_pointer_address(x);
  else mp = C_data_pointer(x);

  *(p++) = C_POINTER_TYPE | 1;
  *((void **)p) = mp;
  *ptr = p + 1;
  return (C_word)p0;
}


C_regparm C_word C_fcall C_i_exactp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_SCHEME_TRUE;

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "exact?", x);

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_u_i_exactp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_SCHEME_TRUE;

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_inexactp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_SCHEME_FALSE;

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "inexact?", x);

  return C_SCHEME_TRUE;
}


C_regparm C_word C_fcall C_u_i_inexactp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_SCHEME_FALSE;

  return C_SCHEME_TRUE;
}


C_regparm C_word C_fcall C_i_zerop(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x == C_fix(0));

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "zero?", x);

  return C_mk_bool(C_flonum_magnitude(x) == 0.0);
}


C_regparm C_word C_fcall C_u_i_zerop(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x == C_fix(0));

  return C_mk_bool(C_flonum_magnitude(x) == 0.0);
}


C_regparm C_word C_fcall C_i_positivep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) > 0);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "positive?", x);

  return C_mk_bool(C_flonum_magnitude(x) > 0.0);
}


C_regparm C_word C_fcall C_u_i_positivep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) > 0);

  return C_mk_bool(C_flonum_magnitude(x) > 0.0);
}


C_regparm C_word C_fcall C_i_negativep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) < 0);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "negative?", x);

  return C_mk_bool(C_flonum_magnitude(x) < 0.0);
}


C_regparm C_word C_fcall C_u_i_negativep(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(C_unfix(x) < 0);

  return C_mk_bool(C_flonum_magnitude(x) < 0.0);
}


C_regparm C_word C_fcall C_i_evenp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_nbool(x & 0x02);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "even?", x);

  return C_mk_bool(fmod(C_flonum_magnitude(x), 2.0) == 0.0);
}


C_regparm C_word C_fcall C_u_i_evenp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_nbool(x & 0x02);

  return C_mk_bool(fmod(C_flonum_magnitude(x), 2.0) == 0.0);
}


C_regparm C_word C_fcall C_i_oddp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x & 0x02);

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "odd?", x);

  return C_mk_bool(fmod(C_flonum_magnitude(x), 2.0) != 0.0);
}


C_regparm C_word C_fcall C_u_i_oddp(C_word x)
{
  if(x & C_FIXNUM_BIT) return C_mk_bool(x & 0x02);

  return C_mk_bool(fmod(C_flonum_magnitude(x), 2.0) != 0.0);
}


C_regparm C_word C_fcall C_i_car(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "car", x);

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cdr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cdr", x);

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_cadr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cadr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_caddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "caddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cdddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cdddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_cadddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cadddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_car(x);
}


C_regparm C_word C_fcall C_i_cddddr(C_word x)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
  bad:
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "cddddr", x);
  }

  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;
  x = C_u_i_cdr(x);
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) goto bad;

  return C_u_i_cdr(x);
}


C_regparm C_word C_fcall C_i_list_tail(C_word lst, C_word i)
{
  C_word lst0 = lst;
  int n;

  if(i & C_FIXNUM_BIT) n = C_unfix(i);
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "list-tail", i);

  while(n--) {
    if(C_immediatep(lst) || C_block_header(lst) != C_PAIR_TAG)
      barf(C_OUT_OF_RANGE_ERROR, "list-tail", lst0, i);
    
    lst = C_u_i_cdr(lst);
  }

  return lst;
}


C_regparm C_word C_fcall C_i_vector_ref(C_word v, C_word i)
{
  int j;

  if(C_immediatep(v) || C_header_bits(v) != C_VECTOR_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-ref", v);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(v)) barf(C_OUT_OF_RANGE_ERROR, "vector-ref", v, i);

    return C_block_item(v, j);
  }
  
  barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-ref", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_block_ref(C_word x, C_word i)
{
  int j;

  if(C_immediatep(x) || (C_header_bits(x) & C_BYTEBLOCK_BIT) != 0)
    barf(C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR, "##sys#block-ref", x);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(x)) barf(C_OUT_OF_RANGE_ERROR, "##sys#block-ref", x, i);

    return C_block_item(x, j);
  }
  
  barf(C_BAD_ARGUMENT_TYPE_ERROR, "##sys#block-ref", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_string_set(C_word s, C_word i, C_word c)
{
  int j;

  if(C_immediatep(s) || C_header_bits(s) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-set!", s);

  if(!C_immediatep(c) || (c & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-set!", c);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(s)) barf(C_OUT_OF_RANGE_ERROR, "string-set!", s, i);

    return C_setsubchar(s, i, c);
  }

  barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-set!", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_string_ref(C_word s, C_word i)
{
  int j;

  if(C_immediatep(s) || C_header_bits(s) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ref", s);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(s)) barf(C_OUT_OF_RANGE_ERROR, "string-ref", s, i);

    return C_subchar(s, i);
  }
  
  barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-ref", i);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_vector_length(C_word v)
{
  if(C_immediatep(v) || C_header_bits(v) != C_VECTOR_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-length", v);

  return C_fix(C_header_size(v));
}


C_regparm C_word C_fcall C_i_string_length(C_word s)
{
  if(C_immediatep(s) || C_header_bits(s) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string-length", s);

  return C_fix(C_header_size(s));
}


C_regparm C_word C_fcall C_i_length(C_word lst)
{
  int n = 0;

  if(lst != C_SCHEME_END_OF_LIST) {
    if(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
      do {
	lst = C_u_i_cdr(lst);
	++n;
      } while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG);
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "length", lst);
  }

  return C_fix(n);
}


C_regparm C_word C_fcall C_i_inexact_to_exact(C_word n)
{
  double m;
  C_word r;

  if(n & C_FIXNUM_BIT) return n;
  else if(C_immediatep(n) || C_block_header(n) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "inexact->exact", n);

  if(modf(C_flonum_magnitude(n), &m) == 0.0) {
    r = (C_word)m;
    
    if(r == m && C_fitsinfixnump(r))
      return C_fix(r);
  }

  barf(C_CANT_REPRESENT_INEXACT_ERROR, "inexact->exact", n);
  return 0;
}


C_regparm C_word C_fcall C_i_set_car(C_word x, C_word val)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "set-car!", x);

  C_mutate(&C_u_i_car(x), val);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_set_cdr(C_word x, C_word val)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "set-cdr!", x);

  C_mutate(&C_u_i_cdr(x), val);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_vector_set(C_word v, C_word i, C_word x)
{
  int j;

  if(C_immediatep(v) || C_header_bits(v) != C_VECTOR_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-set!", v);

  if(i & C_FIXNUM_BIT) {
    j = C_unfix(i);

    if(j < 0 || j >= C_header_size(v)) barf(C_OUT_OF_RANGE_ERROR, "vector-set!", v, i);

    C_mutate(&C_block_item(v, j), x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "vector-set!", i);

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_a_i_abs(C_word **a, int c, C_word x)
{
  if(x & C_FIXNUM_BIT) return C_fix(abs(C_unfix(x)));

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "abs", x);

  return C_flonum(a, fabs(C_flonum_magnitude(x)));
}


C_regparm C_word C_fcall C_a_i_flonum_plus(C_word **a, int c, C_word n1, C_word n2)
{
  return C_flonum(a, C_flonum_magnitude(n1) + C_flonum_magnitude(n2));
}


C_regparm C_word C_fcall C_a_i_flonum_difference(C_word **a, int c, C_word n1, C_word n2)
{
  return C_flonum(a, C_flonum_magnitude(n1) - C_flonum_magnitude(n2));
}


C_regparm C_word C_fcall C_a_i_flonum_times(C_word **a, int c, C_word n1, C_word n2)
{
  return C_flonum(a, C_flonum_magnitude(n1) * C_flonum_magnitude(n2));
}


C_regparm C_word C_fcall C_a_i_flonum_quotient(C_word **a, int c, C_word n1, C_word n2)
{
  return C_flonum(a, C_flonum_magnitude(n1) / C_flonum_magnitude(n2));
}


C_regparm C_word C_fcall C_a_i_flonum_negate(C_word **a, int c, C_word n)
{
  return C_flonum(a, -C_flonum_magnitude(n));
}


C_regparm C_word C_fcall C_a_i_bitwise_and(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;
  C_uword nn1, nn2;

  C_check_uint(n1, f1, nn1, "bitwise-and");
  C_check_uint(n2, f2, nn2, "bitwise-and");
  nn1 = C_limit_fixnum(nn1 & nn2);

  if(C_ufitsinfixnump(nn1)) return C_fix(nn1);
  else return C_flonum(a, nn1);
}


C_regparm C_word C_fcall C_a_i_bitwise_ior(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;
  C_uword nn1, nn2;

  C_check_uint(n1, f1, nn1, "bitwise-ior");
  C_check_uint(n2, f2, nn2, "bitwise-ior");
  nn1 = C_limit_fixnum(nn1 | nn2);

  if(C_ufitsinfixnump(nn1)) return C_fix(nn1);
  else return C_flonum(a, nn1);
}


C_regparm C_word C_fcall C_a_i_bitwise_xor(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;
  C_uword nn1, nn2;

  C_check_uint(n1, f1, nn1, "bitwise-xor");
  C_check_uint(n2, f2, nn2, "bitwise-xor");
  nn1 = C_limit_fixnum(nn1 ^ nn2);

  if(C_ufitsinfixnump(nn1)) return C_fix(nn1);
  else return C_flonum(a, nn1);
}


C_regparm C_word C_fcall C_i_bit_setp(C_word n, C_word i)
{
  double f1;
  C_uword nn1;
  int index;

  if((i & C_FIXNUM_BIT) == 0) 
    barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, "bit-set?", i);

  index = C_unfix(i);

  if(index < 0 || index >= C_WORD_SIZE)
    barf(C_OUT_OF_RANGE_ERROR, "bit-set?", n, i);

  C_check_uint(n, f1, nn1, "bit-set?");
  return C_mk_bool((nn1 & (1 << index)) != 0);
}


C_regparm C_word C_fcall C_a_i_bitwise_not(C_word **a, int c, C_word n)
{
  double f;
  C_uword nn;

  C_check_uint(n, f, nn, "bitwise-not");
  nn = C_limit_fixnum(~nn);

  if(C_ufitsinfixnump(nn)) return C_fix(nn);
  else return C_flonum(a, nn);
}


C_regparm C_word C_fcall C_a_i_arithmetic_shift(C_word **a, int c, C_word n1, C_word n2)
{
  C_word nn;
  C_uword unn;
  C_word s;
  int sgn = 1;

  if((n1 & C_FIXNUM_BIT) != 0) {
    nn = C_unfix(n1);

    if((sgn = nn < 0 ? -1 : 1) >= 0) unn = nn;
  }
  else if(C_immediatep(n1) || C_block_header(n1) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, "arithmetic-shift", n1);
  else { 
    double m, f;

    f = C_flonum_magnitude(n1);
    
    if(modf(f, &m) != 0.0)
      barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "arithmetic-shift", n1);

    if(f < C_WORD_MIN || f > C_UWORD_MAX)
      barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "arithmetic-shift", n1);
    else if(f < 0) {
      if(f > C_WORD_MAX)
	barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, "arithmetic-shift", n1);
      else {
	sgn = -1;
	nn = (C_word)f;
      }
    }
    else if(f > C_WORD_MAX) unn = (C_uword)f;
    else {
      nn = (C_word)f;
      sgn = -1;
    }
  }

  if((n2 & C_FIXNUM_BIT) != 0) s = C_unfix(n2);
  else barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, "arithmetic-shift", n2);

  if(sgn < 0) {
    if(s < 0) nn >>= -s;
    else nn <<= s;

    if(C_fitsinfixnump(nn)) return C_fix(nn);
    else return C_flonum(a, nn);
  } 
  else {
    if(s < 0) unn >>= -s;
    else unn <<= s;
  
    if(C_ufitsinfixnump(unn)) return C_fix(unn);
    else return C_flonum(a, unn);
  }
}


C_regparm C_word C_fcall C_a_i_exp(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "exp", f);
  return C_flonum(a, exp(f));
}


C_regparm C_word C_fcall C_a_i_log(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "log", f);
  return C_flonum(a, log(f));
}


C_regparm C_word C_fcall C_a_i_sin(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "sin", f);
  return C_flonum(a, sin(f));
}


C_regparm C_word C_fcall C_a_i_cos(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "cos", f);
  return C_flonum(a, cos(f));
}


C_regparm C_word C_fcall C_a_i_tan(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "tan", f);
  return C_flonum(a, tan(f));
}


C_regparm C_word C_fcall C_a_i_asin(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "asin", f);
  return C_flonum(a, asin(f));
}


C_regparm C_word C_fcall C_a_i_acos(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "acos", f);
  return C_flonum(a, acos(f));
}


C_regparm C_word C_fcall C_a_i_atan(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "atan", f);
  return C_flonum(a, atan(f));
}


C_regparm C_word C_fcall C_a_i_atan2(C_word **a, int c, C_word n1, C_word n2)
{
  double f1, f2;

  C_check_real(n1, "atan", f1);
  C_check_real(n2, "atan", f2);
  return C_flonum(a, atan2(f1, f2));
}


C_regparm C_word C_fcall C_a_i_sqrt(C_word **a, int c, C_word n)
{
  double f;

  C_check_real(n, "sqrt", f);
  return C_flonum(a, sqrt(f));
}


C_regparm C_word C_fcall C_i_fixnum_arithmetic_shift(C_word n, C_word c)
{
  if(C_unfix(c) < 0) return C_fixnum_shift_right(n, C_u_fixnum_negate(c));
  else return C_fixnum_shift_left(n, c);
}


C_regparm C_word C_fcall C_i_assq(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    a = C_u_i_car(lst);

    if(!C_immediatep(a) && C_block_header(a) == C_PAIR_TAG) {
      if(C_u_i_car(a) == x) return a;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "assq", a);
  
    lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_u_i_assq(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst)) {
    a = C_u_i_car(lst);

    if(C_u_i_car(a) == x) return a;
    else lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_assv(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    a = C_u_i_car(lst);

    if(!C_immediatep(a) && C_block_header(a) == C_PAIR_TAG) {
      if(C_truep(C_i_eqvp(C_u_i_car(a), x))) return a;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "assv", a);
  
    lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_assoc(C_word x, C_word lst)
{
  C_word a;

  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    a = C_u_i_car(lst);

    if(!C_immediatep(a) && C_block_header(a) == C_PAIR_TAG) {
      if(C_equalp(C_u_i_car(a), x)) return a;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "assoc", a);
  
    lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_memq(C_word x, C_word lst)
{
  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    if(C_u_i_car(lst) == x) return lst;
    else lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_u_i_memq(C_word x, C_word lst)
{
  while(!C_immediatep(lst)) {
    if(C_u_i_car(lst) == x) return lst;
    else lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_memv(C_word x, C_word lst)
{
  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    if(C_truep(C_i_eqvp(C_u_i_car(lst), x))) return lst;
    else lst = C_u_i_cdr(lst);
  }

  return C_SCHEME_FALSE;
}


C_regparm C_word C_fcall C_i_member(C_word x, C_word lst)
{
  while(!C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG) {
    if(C_equalp(C_u_i_car(lst), x)) return lst;
    else lst = C_u_i_cdr(lst);
  }
  
  return C_SCHEME_FALSE;
}


/* Inline routines for extended bindings: */

C_regparm C_word C_fcall C_i_check_exact_2(C_word x, C_word loc)
{
  if((x & C_FIXNUM_BIT) == 0) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_char_2(C_word x, C_word loc)
{
  if((x & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_number_2(C_word x, C_word loc)
{
  if((x & C_FIXNUM_BIT) == 0 && (C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_string_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_bytevector_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_BYTEVECTOR_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_BYTEVECTOR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_vector_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_VECTOR_TYPE) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_VECTOR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_structure_2(C_word x, C_word st, C_word loc)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRUCTURE_TYPE || C_u_i_car(x) != st) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_BAD_STRUCT_ERROR, NULL, x, st);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_pair_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_PAIR_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_symbol_2(C_word x, C_word loc)
{
  if(C_immediatep(x) || C_block_header(x) != C_SYMBOL_TAG) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_check_list_2(C_word x, C_word loc)
{
  if(x != C_SCHEME_END_OF_LIST && (C_immediatep(x) || C_block_header(x) != C_PAIR_TAG)) {
    error_location = loc;
    barf(C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR, NULL, x);
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_foreign_char_argumentp(C_word x)
{
  if((x & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS)
    barf(C_BAD_ARGUMENT_TYPE_NO_CHAR_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_fixnum_argumentp(C_word x)
{
  if((x & C_FIXNUM_BIT) == 0)
    barf(C_BAD_ARGUMENT_TYPE_NO_FIXNUM_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_flonum_argumentp(C_word x)
{
  if((x & C_FIXNUM_BIT) != 0) return x;

  if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_block_argumentp(C_word x)
{
  if(C_immediatep(x))
    barf(C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_number_vector_argumentp(C_word t, C_word x)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRUCTURE_TYPE || C_block_item(x, 0) != t)
    barf(C_BAD_ARGUMENT_TYPE_NO_NUMBER_VECTOR_ERROR, NULL, x, t);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_string_argumentp(C_word x)
{
  if(C_immediatep(x) || C_header_bits(x) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_symbol_argumentp(C_word x)
{
  if(C_immediatep(x) || C_header_bits(x) != C_SYMBOL_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_NO_SYMBOL_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_pointer_argumentp(C_word x)
{
  if(C_immediatep(x) || 
     (C_header_bits(x) != C_SWIG_POINTER_TYPE &&
      (C_header_bits(x) & C_SPECIALBLOCK_BIT) == 0) )
    barf(C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_scheme_or_c_pointer_argumentp(C_word x)
{
  if(C_immediatep(x) || 
     (C_header_bits(x) != C_SWIG_POINTER_TYPE &&
      (C_header_bits(x) & C_SPECIALBLOCK_BIT) == 0) )
    barf(C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR, NULL, x);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_tagged_pointer_argumentp(C_word x, C_word t)
{
  if(C_immediatep(x) || (C_header_bits(x) & C_SPECIALBLOCK_BIT) == 0 || (t != C_SCHEME_FALSE && !C_equalp(C_block_item(x, 1), t)))
    barf(C_BAD_ARGUMENT_TYPE_NO_TAGGED_POINTER_ERROR, NULL, x, t);

  return x;
}


C_regparm C_word C_fcall C_i_foreign_integer_argumentp(C_word x)
{
  double m;

  if((x & C_FIXNUM_BIT) != 0) return x;

  if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    m = C_flonum_magnitude(x);

    if(m >= C_WORD_MIN && m <= C_WORD_MAX) return x;
  }

  barf(C_BAD_ARGUMENT_TYPE_NO_INTEGER_ERROR, NULL, x);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_foreign_unsigned_integer_argumentp(C_word x)
{
  double m;

  if((x & C_FIXNUM_BIT) != 0) return x;

  if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    m = C_flonum_magnitude(x);

    if(m >= 0 && m <= C_UWORD_MAX) return x;
  }

  barf(C_BAD_ARGUMENT_TYPE_NO_UINTEGER_ERROR, NULL, x);
  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_not_pair_p_2(C_word x)
{
  return C_mk_bool(C_immediatep(x) || C_block_header(x) != C_PAIR_TAG);
}


C_regparm C_word C_fcall C_i_null_list_p(C_word x)
{
  if(x == C_SCHEME_END_OF_LIST) return C_SCHEME_TRUE;
  else if(!C_immediatep(x) && C_block_header(x) == C_PAIR_TAG) return C_SCHEME_FALSE;
  else {
    barf(C_BAD_ARGUMENT_TYPE_NO_LIST_ERROR, "null-list?", x);
    return C_SCHEME_FALSE;
  }
}


C_regparm C_word C_fcall C_i_string_null_p(C_word x)
{
  if(!C_immediatep(x) && C_header_bits(x) == C_STRING_TYPE)
    return C_zero_length_p(x);
  else {
    barf(C_BAD_ARGUMENT_TYPE_NO_STRING_ERROR, "string-null?", x);
    return C_SCHEME_FALSE;
  }
}


C_regparm C_word C_fcall C_i_null_pointerp(C_word x)
{
  if(!C_immediatep(x) && (C_header_bits(x) & C_SPECIALBLOCK_BIT) != 0)
    return C_null_pointerp(x);

  barf(C_BAD_ARGUMENT_TYPE_ERROR, "null-pointer?", x);
  return C_SCHEME_FALSE;
}


/* Primitives: */

void C_ccall C_apply(C_word c, C_word closure, C_word k, C_word fn, ...)
{
  va_list v;
  int i, n = c - 3;
  C_word x, skip;
#if defined(HAVE_FFI_H)
  ffi_cif cif;
  ffi_type **argtypes;
  void **argvalues;
  ffi_status status;
#endif
#ifdef HACKED_APPLY
  C_word *buf = C_temporary_stack_limit;
  void *proc;
#endif

#ifndef C_UNSAFE_RUNTIME
  if(c < 4) C_bad_min_argc(c, 4);

  fn = resolve_procedure(fn, "apply");
#endif

  va_start(v, fn);

  for(i = n; i > 1; --i) {
    x = va_arg(v, C_word);
#ifdef HACKED_APPLY
    *(buf++) = x;
#else
    C_save(x);
#endif
  }

  x = va_arg(v, C_word);

#ifndef C_UNSAFE_RUNTIME
  if(x != C_SCHEME_END_OF_LIST && (C_immediatep(x) || C_block_header(x) != C_PAIR_TAG))
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "apply", x);
#endif

  for(skip = x; !C_immediatep(skip) && C_block_header(skip) == C_PAIR_TAG; skip = C_u_i_cdr(skip)) {
    x = C_u_i_car(skip);

#ifdef HACKED_APPLY
# ifndef C_UNSAFE_RUNTIME
    if(buf >= C_temporary_stack_bottom) barf(C_TOO_MANY_PARAMETERS_ERROR, "apply");
# endif

    *(buf++) = x;
#else
    C_save(x);

# ifndef C_UNSAFE_RUNTIME
    if(C_temporary_stack < C_temporary_stack_limit)
      barf(C_TOO_MANY_PARAMETERS_ERROR, "apply");
# endif

#endif
    ++n;
  }

  va_end(v);
  --n;

#ifdef HAVE_FFI_H
  if(n >= 120) {
# ifndef C_UNSAFE_RUNTIME
    if(debug_mode == 2 && n > 126)
      fputs(C_text("[debug] `apply' argument list length exceeds default maximum (using libffi)\n"), stderr);

    if(n > 1000) barf(C_TOO_MANY_PARAMETERS_ERROR, "apply"); /* arbitrary */
# endif
  
    argtypes = (ffi_type **)C_alloca(sizeof(ffi_type *) * (n + 3));
    argvalues = (void **)C_alloca(sizeof(void *) * (n + 3));
    argtypes[ 0 ] = &ffi_type_pointer;
    argtypes[ 1 ] = &ffi_type_pointer;
    argtypes[ 2 ] = &ffi_type_pointer;
    c = n + 2;
    argvalues[ 0 ] = &c;
    argvalues[ 1 ] = &fn;
    argvalues[ 2 ] = &k;

    for(i = 0; i < n; ++i) {
      argtypes[ i + 3 ] = &ffi_type_pointer;
      argvalues[ i + 3 ] = C_temporary_stack_bottom - (i + 1);
    }

    C_temporary_stack = C_temporary_stack_bottom;
    status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, n + 3, &ffi_type_void, argtypes);
    assert(status == FFI_OK);
    ffi_call(&cif, (void *)C_block_item(fn, 0), NULL, argvalues);
  }

#elif defined(HACKED_APPLY)
  buf = alloca((n + 3) * sizeof(C_word));
  buf[ 0 ] = n + 2;
  buf[ 1 ] = fn;
  buf[ 2 ] = k;
  C_memcpy(&buf[ 3 ], C_temporary_stack_limit, n * sizeof(C_word));
  proc = (void *)C_block_item(fn, 0);
  __asm { 
    mov eax, proc
    mov esp, buf
    call eax
  }
#endif

  C_do_apply(n, fn, k);
}


void C_ccall C_do_apply(C_word n, C_word fn, C_word k)
{
  void *pr = (void *)C_block_item(fn, 0);
  C_word *ptr = C_temporary_stack = C_temporary_stack_bottom;

/* PTR_O_p<P>_<B>(o): list of COUNT = ((2 ** P) * B) '*(ptr-I)' arguments,
 * with offset I in range [o, o+COUNT-1].
 */
#define PTR_O_p0_0(o)
#define PTR_O_p1_0(o)
#define PTR_O_p2_0(o)
#define PTR_O_p3_0(o)
#define PTR_O_p4_0(o)
#define PTR_O_p5_0(o)
#define PTR_O_p6_0(o)
#define PTR_O_p7_0(o)
#define PTR_O_p0_1(o)   , *(ptr-(o))
#define PTR_O_p1_1(o)   , *(ptr-(o)), *(ptr-(o+1))
#define PTR_O_p2_1(o)   PTR_O_p1_1(o) PTR_O_p1_1(o+2)
#define PTR_O_p3_1(o)   PTR_O_p2_1(o) PTR_O_p2_1(o+4)
#define PTR_O_p4_1(o)   PTR_O_p3_1(o) PTR_O_p3_1(o+8)
#define PTR_O_p5_1(o)   PTR_O_p4_1(o) PTR_O_p4_1(o+16)
#define PTR_O_p6_1(o)   PTR_O_p5_1(o) PTR_O_p5_1(o+32)
#define PTR_O_p7_1(o)   PTR_O_p6_1(o) PTR_O_p6_1(o+64)

/* CASE_C_PROC_p0 (n0,  p6,p5,p4,p3,p2,p1,p0):
 *  let's note <N> = <n0> - 2; the macro inserts:
 *      case <N>: ((C_cproc<n0>)pr) (<n0>, fn, k, <rest>);
 *  where <rest> is:    *(ptr-1), ..., *(ptr-<N>)
 *  (<rest> is empty for <n0> == 2).
 *  We must have:   n0 = SUM (i = 7 to 0, p<i> * (1 << i)).
 * CASE_C_PROC_p<N+1> (...):
 *  like CASE_C_PROC_p<N>, but with doubled output...
 */
#define CASE_C_PROC_p0(n0,  p6,p5,p4,p3,p2,p1,p0) \
    case (n0-2): ((C_proc##n0)pr)(n0, fn, k \
PTR_O_p6_##p6(((n0-2)&0x80)+1)\
PTR_O_p5_##p5(((n0-2)&0xC0)+1)\
PTR_O_p4_##p4(((n0-2)&0xE0)+1)\
PTR_O_p3_##p3(((n0-2)&0xF0)+1)\
PTR_O_p2_##p2(((n0-2)&0xF8)+1)\
PTR_O_p1_##p1(((n0-2)&0xFC)+1)\
PTR_O_p0_##p0(((n0-2)&0xFE)+1));
#define CASE_C_PROC_p1( n0,n1,  p6,p5,p4,p3,p2,p1) \
        CASE_C_PROC_p0 (n0,  p6,p5,p4,p3,p2,p1,0) \
        CASE_C_PROC_p0 (n1,  p6,p5,p4,p3,p2,p1,1)
#define CASE_C_PROC_p2( n0,n1,n2,n3,  p6,p5,p4,p3,p2) \
        CASE_C_PROC_p1 (n0,n1,  p6,p5,p4,p3,p2,0) \
        CASE_C_PROC_p1 (n2,n3,  p6,p5,p4,p3,p2,1)
#define CASE_C_PROC_p3( n0,n1,n2,n3,n4,n5,n6,n7,  p6,p5,p4,p3) \
        CASE_C_PROC_p2 (n0,n1,n2,n3,  p6,p5,p4,p3,0) \
        CASE_C_PROC_p2 (n4,n5,n6,n7,  p6,p5,p4,p3,1)

  switch(n) {
    CASE_C_PROC_p3 (2,3,4,5,6,7,8,9,  0,0,0,0)
    CASE_C_PROC_p3 (10,11,12,13,14,15,16,17,  0,0,0,1)
    CASE_C_PROC_p3 (18,19,20,21,22,23,24,25,  0,0,1,0)
    CASE_C_PROC_p3 (26,27,28,29,30,31,32,33,  0,0,1,1)
    CASE_C_PROC_p3 (34,35,36,37,38,39,40,41,  0,1,0,0)
    CASE_C_PROC_p3 (42,43,44,45,46,47,48,49,  0,1,0,1)
    CASE_C_PROC_p3 (50,51,52,53,54,55,56,57,  0,1,1,0)
    CASE_C_PROC_p3 (58,59,60,61,62,63,64,65,  0,1,1,1)
    CASE_C_PROC_p0 (66,  1,0,0,0,0,0,0)
    CASE_C_PROC_p0 (67,  1,0,0,0,0,0,1)
    CASE_C_PROC_p1 (68,69,  1,0,0,0,0,1)
    CASE_C_PROC_p2 (70,71,72,73,  1,0,0,0,1)
    CASE_C_PROC_p3 (74,75,76,77,78,79,80,81,  1,0,0,1)
    CASE_C_PROC_p3 (82,83,84,85,86,87,88,89,  1,0,1,0)
    CASE_C_PROC_p3 (90,91,92,93,94,95,96,97,  1,0,1,1)
    CASE_C_PROC_p3 (98,99,100,101,102,103,104,105,  1,1,0,0)
    CASE_C_PROC_p3 (106,107,108,109,110,111,112,113,  1,1,0,1)
    CASE_C_PROC_p3 (114,115,116,117,118,119,120,121,  1,1,1,0)
    CASE_C_PROC_p2 (122,123,124,125,  1,1,1,1,0)
    CASE_C_PROC_p1 (126,127,  1,1,1,1,1,0)
    CASE_C_PROC_p0 (128,  1,1,1,1,1,1,0)
  default: barf(C_TOO_MANY_PARAMETERS_ERROR, "apply");
  }
}


void C_ccall C_call_cc(C_word c, C_word cl, C_word k, C_word cont)
{
  C_word *a = C_alloc(3),
         wrapper;
  void *pr = (void *)C_u_i_car(cont);

  if(C_immediatep(cont) || C_header_bits(cont) != C_CLOSURE_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "call-with-current-continuation", cont);

  /* Check for values-continuation: */
  if(C_u_i_car(k) == (C_word)values_continuation)
    wrapper = C_closure(&a, 2, (C_word)call_cc_values_wrapper, k);
  else wrapper = C_closure(&a, 2, (C_word)call_cc_wrapper, k);

  ((C_proc3)pr)(3, cont, k, wrapper);
}


void C_ccall call_cc_wrapper(C_word c, C_word closure, C_word k, C_word result)
{
  C_word cont = C_u_i_cdr(closure);

  if(c != 3) C_bad_argc(c, 3);

  C_kontinue(cont, result);
}


void C_ccall call_cc_values_wrapper(C_word c, C_word closure, C_word k, ...)
{
  va_list v;
  C_word cont = C_u_i_cdr(closure),
         x1;
  int n = c;

  va_start(v, k);

  if(c > 2) {
    x1 = va_arg(v, C_word);
    --n;
    
    while(--c > 2) C_save(va_arg(v, C_word));
  }
  else x1 = C_SCHEME_UNBOUND;

  va_end(v);
  C_do_apply(n - 2, cont, x1);
}


void C_ccall C_continuation_graft(C_word c, C_word self, C_word k, C_word kk, C_word proc)
{
  ((C_proc2)C_retrieve_proc(proc))(2, proc, C_block_item(kk, 1));
}


void C_ccall C_values(C_word c, C_word closure, C_word k, ...)
{
  va_list v;
  C_word n = c;

  if(c < 2) C_bad_min_argc(c, 2);

  va_start(v, k);

  /* Check continuation wether it receives multiple values: */
  if(C_block_item(k, 0) == (C_word)values_continuation) {
    while(c-- > 2)
      C_save(va_arg(v, C_word));

    va_end(v);
    C_do_apply(n - 2, k, C_SCHEME_UNBOUND); /* unbound value marks direct invocation */
  }
  
  if(c != 3) {
#ifdef RELAX_MULTIVAL_CHECK
    if(c == 2) n = C_SCHEME_UNDEFINED;
    else n = va_arg(v, C_word);
#else
    barf(C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR, "values", k);
#endif
  }
  else n = va_arg(v, C_word);

  va_end(v);
  C_kontinue(k, n);
}


void C_ccall C_apply_values(C_word c, C_word closure, C_word k, C_word lst)
{
  C_word n;

#ifndef C_UNSAFE_RUNTIME
  if(c != 3) C_bad_argc(c, 3);
#endif

  /* Check continuation wether it receives multiple values: */
  if(C_block_item(k, 0) == (C_word)values_continuation) {
    for(n = 0; !C_immediatep(lst) && C_block_header(lst) == C_PAIR_TAG; ++n) {
      C_save(C_u_i_car(lst));
      lst = C_u_i_cdr(lst);
    }

    C_do_apply(n, k, C_SCHEME_UNBOUND); /* unbound value marks direct invocation */
  }
  
  if(C_immediatep(lst) || (C_block_header(lst) == C_PAIR_TAG && C_u_i_cdr(lst) == C_SCHEME_END_OF_LIST)) {
#ifdef RELAX_MULTIVAL_CHECK
    if(C_immediatep(lst)) n = C_SCHEME_UNDEFINED;
    else n = C_u_i_car(lst);
#else
    barf(C_CONTINUATION_CANT_RECEIVE_VALUES_ERROR, "values", k);
#endif
  }
  else n = C_u_i_car(lst);

  C_kontinue(k, n);
}


void C_ccall C_call_with_values(C_word c, C_word cl, C_word k, C_word thunk, C_word kont)
{
  C_word *a = C_alloc(4),
         kk;

#ifndef C_UNSAFE_RUNTIME
  if(c != 4) C_bad_argc(c, 4);

  if(C_immediatep(thunk) || C_header_bits(thunk) != C_CLOSURE_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "call-with-values", thunk);

  if(C_immediatep(kont) || C_header_bits(kont) != C_CLOSURE_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "call-with-values", kont);
#endif

  kk = C_closure(&a, 3, (C_word)values_continuation, kont, k);
  C_do_apply(0, thunk, kk);
}


void C_ccall C_u_call_with_values(C_word c, C_word cl, C_word k, C_word thunk, C_word kont)
{
  C_word *a = C_alloc(4),
         kk;

  kk = C_closure(&a, 3, (C_word)values_continuation, kont, k);
  C_do_apply(0, thunk, kk);
}


void C_ccall values_continuation(C_word c, C_word closure, C_word arg0, ...)
{
  C_word kont = ((C_SCHEME_BLOCK *)closure)->data[ 1 ],
         k = ((C_SCHEME_BLOCK *)closure)->data[ 2 ],
         n = c,
         *ptr;
  va_list v;

  if(arg0 == C_SCHEME_UNBOUND) { /* This continuation was called by 'values'... */
    va_start(v, arg0);

    for(; c-- > 2; C_save(va_arg(v, C_word)));

    va_end(v);
  }
  else {			/* This continuation was captured and called explicity... */
    ++n;
    c -= 1;

    /* move temporary-stack contents upwards one slot: */
    for(ptr = C_temporary_stack - c; --c; ++ptr) *ptr = ptr[ 1 ];

    C_save(arg0);
  }

  C_do_apply(n - 2, kont, k);
}


void C_ccall C_times(C_word c, C_word closure, C_word k, ...)
{
  va_list v;
  C_word x;
  C_word iresult = 1;
  int fflag = 0;
  double fresult = 1;

  va_start(v, k);
  c -= 2;

  while(c--) {
    x = va_arg(v, C_word);
    
    if(x & C_FIXNUM_BIT) {
	fresult *= C_unfix(x);
	
	if(!fflag) iresult *= C_unfix(x);
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
	fresult *= C_flonum_magnitude(x);

	if(!fflag) fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", x);
  }

  va_end(v);
  x = C_fix(iresult);
  
  if(fflag || (double)C_unfix(x) != fresult) {
      C_temporary_flonum = fresult;
      C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
  }

  C_kontinue(k, x);
}


C_regparm C_word C_fcall C_2_times(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;
  double fresult;
  int fflag = 0;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      iresult = C_unfix(x) * C_unfix(y);
      fresult = (double)C_unfix(x) * (double)C_unfix(y);
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      fresult = C_unfix(x) * C_flonum_magnitude(y);
      fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;

    if(y & C_FIXNUM_BIT) fresult = C_flonum_magnitude(x) * C_unfix(y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      fresult = C_flonum_magnitude(x) * C_flonum_magnitude(y);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "*", x);

  iresult = C_fix(iresult);

  if(fflag || (double)C_unfix(iresult) != fresult) return C_flonum(ptr, fresult);
  
  return iresult;
}


void C_ccall C_plus(C_word c, C_word closure, C_word k, ...)
{
  va_list v;
  C_word x;
  C_word iresult = 0;
  int fflag = 0;
  double fresult = 0;

  va_start(v, k);
  c -= 2;

  while(c--) {
    x = va_arg(v, C_word);
    
    if(x & C_FIXNUM_BIT) {
	fresult += C_unfix(x);

	if(!fflag) iresult += C_unfix(x);
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      fresult += C_flonum_magnitude(x);

      if(!fflag) fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", x);
  }

  va_end(v);
  x = C_fix(iresult);

  if(fflag || (double)C_unfix(x) != fresult) {
    C_temporary_flonum = fresult;
    C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
  }

  C_kontinue(k, x);
}


C_regparm C_word C_fcall C_2_plus(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;
  double fresult;
  int fflag = 0;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      iresult = C_unfix(x) + C_unfix(y);
      fresult = (double)C_unfix(x) + (double)C_unfix(y);
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      fresult = C_unfix(x) + C_flonum_magnitude(y);
      fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;

    if(y & C_FIXNUM_BIT) fresult = C_flonum_magnitude(x) + C_unfix(y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      fresult = C_flonum_magnitude(x) + C_flonum_magnitude(y);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "+", x);
  
  iresult = C_fix(iresult);

  if(fflag || (double)C_unfix(iresult) != fresult) return C_flonum(ptr, fresult);
  
  return iresult;
}


void cons_flonum_trampoline(void *dummy)
{
  C_word k = C_restore,
         *a = C_alloc(WORDS_PER_FLONUM);

  C_kontinue(k, C_flonum(&a, C_temporary_flonum));
}


void C_ccall C_minus(C_word c, C_word closure, C_word k, C_word n1, ...)
{
  va_list v;
  C_word iresult;
  int fflag;
  double fresult;

  if(c < 3) C_bad_min_argc(c, 3);

  if(n1 & C_FIXNUM_BIT) {
    fresult = iresult = C_unfix(n1);
    fflag = 0;
  }
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
    fresult = C_flonum_magnitude(n1);
    fflag = 1;
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", n1);

  if(c == 3) {
    if(fflag) fresult = -fresult;
    else fresult = iresult = -iresult;

    goto cont;
  }

  va_start(v, n1);
  c -= 3;

  while(c--) {
    n1 = va_arg(v, C_word);
    
    if(n1 & C_FIXNUM_BIT) {
      fresult -= C_unfix(n1);

      if(!fflag) iresult -= C_unfix(n1);
    }
    else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
      fresult -= C_flonum_magnitude(n1);

      if(!fflag) fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", n1);
  }

  va_end(v);
  
 cont:
  n1 = C_fix(iresult);

  if(fflag || (double)C_unfix(n1) != fresult) {
    C_temporary_flonum = fresult;
    C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
  }

  C_kontinue(k, n1);
}


C_regparm C_word C_fcall C_2_minus(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;
  double fresult;
  int fflag = 0;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      iresult = C_unfix(x) - C_unfix(y);
      fresult = (double)C_unfix(x) - (double)C_unfix(y);
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      fresult = C_unfix(x) - C_flonum_magnitude(y);
      fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;

    if(y & C_FIXNUM_BIT) fresult = C_flonum_magnitude(x) - C_unfix(y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      fresult = C_flonum_magnitude(x) - C_flonum_magnitude(y);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "-", x);
  
  iresult = C_fix(iresult);

  if(fflag || (double)C_unfix(iresult) != fresult) return C_flonum(ptr, fresult);
  
  return iresult;
}


void C_ccall C_divide(C_word c, C_word closure, C_word k, C_word n1, ...)
{
  va_list v;
  C_word n2;
  C_word iresult;
  int fflag;
  double fresult, f2;

  if(c < 3) C_bad_min_argc(c, 3);

  if(n1 & C_FIXNUM_BIT) {
    iresult = C_unfix(n1);
    fflag = 0;
  }
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
    fresult = C_flonum_magnitude(n1);
    fflag = 1;
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", n1);

  if(c == 3) {
    if(fflag) {
      if(fresult == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = 1.0 / fresult;
    }
    else {
      if(iresult == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = 1.0 / (double)iresult;
      fflag = 1;
    }

    goto cont;
  }

  va_start(v, n1);
  c -= 3;

  while(c--) {
    n1 = va_arg(v, C_word);
    
    if(n1 & C_FIXNUM_BIT) {
      if(fflag) {
	if((n1 = C_unfix(n1)) == 0) 
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	fresult /= n1;
      }
      else {
	if((n2 = C_unfix(n1)) == 0)
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	if((fresult = (double)iresult / (double)n2) != (iresult /= n2))
	  fflag = 1;
      }
    }
    else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
      if(fflag) {
	if((f2 = C_flonum_magnitude(n1)) == 0)
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	fresult /= f2;
      }
      else {
	fflag = 1;

	if((f2 = C_flonum_magnitude(n1)) == 0)
	  barf(C_DIVISION_BY_ZERO_ERROR, "/");

	fresult = (double)iresult / f2;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", n1);
  }

  va_end(v);
  
 cont:
  if(fflag) {
    C_temporary_flonum = fresult;
    C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
  }
  else n1 = C_fix(iresult);

  C_kontinue(k, n1);
}


C_regparm C_word C_fcall C_2_divide(C_word **ptr, C_word x, C_word y)
{
  C_word iresult;
  double fresult;
  int fflag = 0;

  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) {
      if((iresult = C_unfix(y)) == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = (double)C_unfix(x) / (double)iresult;
      iresult = C_unfix(x) / iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      if((fresult = C_flonum_magnitude(y)) == 0.0)
	barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = (double)C_unfix(x) / fresult;
      fflag = 1;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;

    if(y & C_FIXNUM_BIT) {
      fresult = C_flonum_magnitude(x);

      if((iresult = C_unfix(y)) == 0) barf(C_DIVISION_BY_ZERO_ERROR, "/");

      fresult = fresult / (double)iresult;
    }
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG) {
      if((fresult = C_flonum_magnitude(y)) == 0.0) barf(C_DIVISION_BY_ZERO_ERROR, "/");
      
      fresult = C_flonum_magnitude(x) / fresult;
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", y);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "/", x);

  iresult = C_fix(iresult);

  if(fflag || (double)C_unfix(iresult) != fresult) return C_flonum(ptr, fresult);
  
  return iresult;
}


void C_ccall C_nequalp(C_word c, C_word closure, C_word k, ...)
{
  C_word x, i2, f, fflag, ilast;
  double flast, f2;
  va_list v;

  c -= 2; 
  f = 1;
  va_start(v, k);

  if(c == 0) goto cont;
  
  x = va_arg(v, C_word);

  if(x & C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "=", x);

  while(--c) {
    x = va_arg(v, C_word);
    
    if(x & C_FIXNUM_BIT) {
      if(fflag) {
	f = flast == (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast == (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast == (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast == (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "=", x);

    if(!f) break;
  }

 cont:
  va_end(v);
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_nequalp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x == y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) == C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_ERROR, "=", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) == (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) == C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "=", y);
  }

  return C_SCHEME_FALSE;
}


void C_ccall C_greaterp(C_word c, C_word closure, C_word k, ...)
{
  C_word x, i2, f, fflag, ilast;
  double flast, f2;
  va_list v;

  c -= 2; 
  f = 1;
  va_start(v, k);

  if(c == 0) goto cont;
  
  x = va_arg(v, C_word);

  if(x & C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">", x);

  while(--c) {
    x = va_arg(v, C_word);
    
    if(x & C_FIXNUM_BIT) {
      if(fflag) {
	f = flast > (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast > (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast > (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast > (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">", x);

    if(!f) break;
  }

 cont:
  va_end(v);
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_greaterp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x > y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) > C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_ERROR, ">", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) > (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) > C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_ERROR, ">", y);
  }

  return C_SCHEME_FALSE;
}


void C_ccall C_lessp(C_word c, C_word closure, C_word k, ...)
{
  C_word x, i2, f, fflag, ilast;
  double flast, f2;
  va_list v;

  c -= 2; 
  f = 1;
  va_start(v, k);

  if(c == 0) goto cont;
  
  x = va_arg(v, C_word);

  if(x &C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<", x);

  while(--c) {
    x = va_arg(v, C_word);
    
    if(x &C_FIXNUM_BIT) {
      if(fflag) {
	f = flast < (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast < (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast < (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast < (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<", x);

    if(!f) break;
  }

 cont:
  va_end(v);
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_lessp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x < y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) < C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_ERROR, "<", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) < (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) < C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "<", y);
  }

  return C_SCHEME_FALSE;
}


void C_ccall C_greater_or_equal_p(C_word c, C_word closure, C_word k, ...)
{
  C_word x, i2, f, fflag, ilast;
  double flast, f2;
  va_list v;

  c -= 2; 
  f = 1;
  va_start(v, k);

  if(c == 0) goto cont;
  
  x = va_arg(v, C_word);

  if(x &C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">=", x);

  while(--c) {
    x = va_arg(v, C_word);
    
    if(x &C_FIXNUM_BIT) {
      if(fflag) {
	f = flast >= (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast >= (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast >= (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast >= (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, ">=", x);

    if(!f) break;
  }

 cont:
  va_end(v);
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_greater_or_equalp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x >= y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) >= C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_ERROR, ">=", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) >= (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) >= C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_ERROR, ">=", y);
  }

  return C_SCHEME_FALSE;
}


void C_ccall C_less_or_equal_p(C_word c, C_word closure, C_word k, ...)
{
  C_word x, i2, f, fflag, ilast;
  double flast, f2;
  va_list v;

  c -= 2; 
  f = 1;
  va_start(v, k);

  if(c == 0) goto cont;
  
  x = va_arg(v, C_word);

  if(x &C_FIXNUM_BIT) {
    fflag = 0;
    ilast = C_unfix(x);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    fflag = 1;
    flast = C_flonum_magnitude(x);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<=", x);

  while(--c) {
    x = va_arg(v, C_word);
    
    if(x &C_FIXNUM_BIT) {
      if(fflag) {
	f = flast <= (f2 = (double)C_unfix(x));
        flast = f2;
      }
      else {
	f = ilast <= (i2 = C_unfix(x));
	ilast = i2;
      }
    }
    else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
      if(fflag) {
	f = flast <= (f2 = C_flonum_magnitude(x));
	flast = f2;
      }
      else {
	f = (double)ilast <= (f2 = C_flonum_magnitude(x));
	flast = f2;
	fflag = 1;
      }
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "<=", x);

    if(!f) break;
  }

 cont:
  va_end(v);
  C_kontinue(k, C_mk_bool(f));
}


C_regparm C_word C_fcall C_i_less_or_equalp(C_word x, C_word y)
{
  if(x & C_FIXNUM_BIT) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(x <= y);
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool((double)C_unfix(x) <= C_flonum_magnitude(y));

    barf(C_BAD_ARGUMENT_TYPE_ERROR, "<=", y);
  }
  else if(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG) {
    if(y & C_FIXNUM_BIT) return C_mk_bool(C_flonum_magnitude(x) <= (double)C_unfix(y));
    else if(!C_immediatep(y) && C_block_header(y) == C_FLONUM_TAG)
      return C_mk_bool(C_flonum_magnitude(x) <= C_flonum_magnitude(y));
    
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "<=", y);
  }

  return C_SCHEME_FALSE;
}


void C_ccall C_expt(C_word c, C_word closure, C_word k, C_word n1, C_word n2)
{
  double m1, m2;
  C_word r;

  if(c != 4) C_bad_argc(c, 4);

  if(n1 & C_FIXNUM_BIT) m1 = C_unfix(n1);
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG)
    m1 = C_flonum_magnitude(n1);
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "expt", n1);

  if(n2 & C_FIXNUM_BIT) m2 = C_unfix(n2);
  else if(!C_immediatep(n2) && C_block_header(n2) == C_FLONUM_TAG)
    m2 = C_flonum_magnitude(n2);
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "expt", n2);

  m1 = pow(m1, m2);
  r = (C_word)m1;

  if(r == m1 && (n1 & C_FIXNUM_BIT) && (n2 & C_FIXNUM_BIT) && modf(m1, &m2) == 0.0 && C_fitsinfixnump(r))
    C_kontinue(k, C_fix(r));

  C_temporary_flonum = m1;
  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_gc(C_word c, C_word closure, C_word k, ...)
{
  int f;
  C_word arg;
  long size = 0;
  va_list v;

  va_start(v, k);

  if(c == 3) {
    arg = va_arg(v, C_word);
    f = C_truep(arg);
  }
  else if(c != 2) C_bad_min_argc(c, 2);
  else f = 1;

  C_save(k);
  va_end(v);

  if(c == 3) {
    if((arg & C_FIXNUM_BIT) != 0) size = C_unfix(arg);
    else if(arg == C_SCHEME_END_OF_LIST) size = percentage(heap_size, C_heap_growth);
  }

  if(size && !C_heap_size_is_fixed) {
    C_rereclaim2(size, 0);
    gc_2(NULL);
  }
  else if(f) C_fromspace_top = C_fromspace_limit;

  C_reclaim(gc_2, NULL);
}


void gc_2(void *dummy)
{
  C_word k = C_restore;
  
  C_kontinue(k, C_fix((C_uword)C_fromspace_limit - (C_uword)C_fromspace_top));
}


void C_ccall C_open_file_port(C_word c, C_word closure, C_word k, C_word port, C_word channel, C_word mode)
{
  C_FILEPTR fp = (C_FILEPTR)NULL;
  C_char fmode[ 4 ];
  C_word n;
  char *buf;

  switch(channel) {
  case C_fix(0): fp = C_stdin; break;
  case C_fix(1): fp = C_stdout; break;
  case C_fix(2): fp = C_stderr; break;
  default:
    n = C_header_size(channel);
    buf = buffer;

    if(n >= STRING_BUFFER_SIZE) {
      if((buf = (char *)C_malloc(n + 1)) == NULL)
	barf(C_OUT_OF_MEMORY_ERROR, "system");
    }

    C_strncpy(buf, C_c_string(channel), n);
    buf[ n ] = '\0';
    n = C_header_size(mode);
    C_strncpy(fmode, C_c_string(mode), n);
    fmode[ n ] = '\0';
    fp = C_fopen(buf, fmode);

    if(buf != buffer) C_free(buf);
  }
  
  C_set_block_item(port, 0, (C_word)fp);
  C_kontinue(k, C_mk_bool(fp != NULL));
}


void C_ccall C_allocate_vector(C_word c, C_word closure, C_word k, C_word size, C_word bvecf, C_word init, C_word align8)
{
  C_uword bytes, n = C_unfix(size);

  if(c != 6) C_bad_argc(c, 6);

  if(n > C_HEADER_SIZE_MASK)
    barf(C_OUT_OF_RANGE_ERROR, NULL, size, C_fix(C_HEADER_SIZE_MASK));

  if(!C_truep(bvecf)) bytes = C_wordstobytes(n) + sizeof(C_word);
  else bytes = n + sizeof(C_word);

  if(C_truep(align8)) bytes += sizeof(C_word);

  C_save(k);
  C_save(size);
  C_save(init);
  C_save(bvecf);
  C_save(align8);
  C_save(C_fix(bytes));

  if(!C_demand(C_bytestowords(bytes))) {
    /* Allocate on heap: */
    if((C_uword)(C_fromspace_limit - C_fromspace_top) < (bytes + stack_size * 2))
      C_fromspace_top = C_fromspace_limit; /* trigger major GC */
  
    C_save(C_SCHEME_TRUE);
    C_reclaim(allocate_vector_2, NULL);
  }

  C_save(C_SCHEME_FALSE);
  allocate_vector_2(NULL);
}


void allocate_vector_2(void *dummy)
{
  C_word mode = C_restore;
  int bytes = C_unfix(C_restore);
  C_word align8 = C_restore,
         bvecf = C_restore,
         init = C_restore;
  C_word size = C_unfix(C_restore);
  C_word k = C_restore,
         *v0, v;

  if(C_truep(mode)) {
    while((C_uword)(C_fromspace_limit - C_fromspace_top) < (bytes + stack_size)) {
      if(C_heap_size_is_fixed)
	panic(C_text("out of memory - can not allocate vector (heap resizing disabled)"));

      C_save(init);
      C_save(k);
      C_rereclaim2(percentage(heap_size, C_heap_growth) + (C_uword)bytes, 0);
      k = C_restore;
      init = C_restore;
    }

    v0 = (C_word *)C_align((C_word)C_fromspace_top);
    C_fromspace_top += C_align(bytes);
  }
  else v0 = C_alloc(C_bytestowords(bytes));

#ifndef C_SIXTY_FOUR
  if(C_truep(align8) && aligned8(v0)) ++v0;
#endif

  v = (C_word)v0;

  if(!C_truep(bvecf)) {
    *(v0++) = C_VECTOR_TYPE | size | (C_truep(align8) ? C_8ALIGN_BIT : 0);
  
    while(size--) *(v0++) = init;
  }
  else {
    *(v0++) = C_STRING_TYPE | size;

    if(C_truep(init))
      C_memset(v0, C_character_code(init), size);
  }

  C_kontinue(k, v);
}


void C_ccall C_string_to_symbol(C_word c, C_word closure, C_word k, C_word string)
{
  int len, key;
  C_word s, *a = C_alloc(6);	/* 6 <=> 1 bucket (pair) + 1 symbol */
  C_char *name;

  if(c != 3) C_bad_argc(c, 3);

  if(C_immediatep(string) || C_header_bits(string) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string->symbol", string);
    
  len = C_header_size(string);
  name = (C_char *)C_data_pointer(string);
  key = hash_string(len, name, symbol_table->size);

  if(!C_truep(s = lookup(key, len, name, symbol_table))) 
    s = add_symbol(&a, key, string, symbol_table);

  C_kontinue(k, s);
}


void C_ccall C_flonum_fraction(C_word c, C_word closure, C_word k, C_word n)
{
  double i, fn = C_flonum_magnitude(n);

  C_temporary_flonum = modf(fn, &i);
  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_exact_to_inexact(C_word c, C_word closure, C_word k, C_word n)
{
  if(c != 3) C_bad_argc(c, 3);

  if(n & C_FIXNUM_BIT) {
    C_temporary_flonum = (double)C_unfix(n);
    C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
  }
  else if(C_immediatep(n) || C_block_header(n) != C_FLONUM_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "exact->inexact", n);
 
  C_kontinue(k, n);
}


void C_ccall C_flonum_floor(C_word c, C_word closure, C_word k, C_word n)
{
  C_temporary_flonum = floor(C_flonum_magnitude(n));
  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_flonum_ceiling(C_word c, C_word closure, C_word k, C_word n)
{
  C_temporary_flonum = ceil(C_flonum_magnitude(n));
  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_flonum_truncate(C_word c, C_word closure, C_word k, C_word n)
{
  modf(C_flonum_magnitude(n), &C_temporary_flonum);
  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_flonum_round(C_word c, C_word closure, C_word k, C_word n)
{
  double fn, i, f, i2;

  fn = C_flonum_magnitude(n);
  if(fn < 0.0) {
    f = modf(-fn, &i);
    if(f < 0.5 || (f == 0.5 && modf(i * 0.5, &i2) == 0.0))
      C_temporary_flonum = -i;
    else
      C_temporary_flonum = -(i + 1.0);
  }
  else if(fn == 0.0/* || fn == -0.0*/)
    C_temporary_flonum = fn;
  else {
    f = modf(fn, &i);
    if(f < 0.5 || (f == 0.5 && modf(i * 0.5, &i2) == 0.0))
      C_temporary_flonum = i;
    else
      C_temporary_flonum = i + 1.0;
  }

  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_quotient(C_word c, C_word closure, C_word k, C_word n1, C_word n2)
{
  double f1, f2;
  C_word result;

  if(c != 4) C_bad_argc(c, 4);

  if(n1 &C_FIXNUM_BIT) {
    if(n2 &C_FIXNUM_BIT) {
      if((n2 = C_unfix(n2)) == 0)
	barf(C_DIVISION_BY_ZERO_ERROR, "quotient");
      
      result = C_fix(C_unfix(n1) / n2);
      C_kontinue(k, result);
    }
    else if(!C_immediatep(n2) && C_block_header(n2) == C_FLONUM_TAG) {
      f1 = (double)C_unfix(n1);
      f2 = C_flonum_magnitude(n2);
    }
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "quotient", n2);
  }
  else if(!C_immediatep(n1) && C_block_header(n1) == C_FLONUM_TAG) {
    f1 = C_flonum_magnitude(n1);

    if(n2 &C_FIXNUM_BIT)
      f2 = (double)C_unfix(n2);
    else if(!C_immediatep(n2) && C_block_header(n2) == C_FLONUM_TAG)
      f2 = C_flonum_magnitude(n2);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "quotient", n2);
  }
  else barf(C_BAD_ARGUMENT_TYPE_ERROR, "quotient", n1);

  if(f2 == 0)
    barf(C_DIVISION_BY_ZERO_ERROR, "quotient");

  modf(f1 / f2, &C_temporary_flonum);
  C_cons_flonum(2, C_SCHEME_UNDEFINED, k);
}


void C_ccall C_cons_flonum(C_word c, C_word closure, C_word k)
{
  C_word *a = C_alloc(WORDS_PER_FLONUM);

  C_kontinue(k, C_flonum(&a, C_temporary_flonum));
}


void C_ccall C_string_to_number(C_word c, C_word closure, C_word k, C_word str, ...)
{
  int radix, radixpf = 0, sharpf = 0, ratp = 0, exactf, exactpf = 0;
  C_word n1, n, *a = C_alloc(WORDS_PER_FLONUM);
  C_char *sptr, *eptr;
  double fn1, fn;
  va_list v;

  if(c == 3) radix = 10;	/* default radix is 10 */
  else if(c == 4) {
    va_start(v, str);
    radix = va_arg(v, C_word);
    va_end(v);
    
    if(radix & C_FIXNUM_BIT) radix = C_unfix(radix);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "string->number", radix);
  }
  else C_bad_argc(c, 3);

  if(C_immediatep(str) || C_header_bits(str) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "string->number", str);

  if((n = C_header_size(str)) == 0) {
  fail:
    n = C_SCHEME_FALSE;
    goto fini;
  }

  if(n >= STRING_BUFFER_SIZE - 1) {
    n = C_SCHEME_FALSE;
    goto fini;
  }

  C_memcpy(sptr = buffer, C_c_string(str), n > (STRING_BUFFER_SIZE - 1) ? STRING_BUFFER_SIZE : n);
  buffer[ n ] = '\0';
  
  while(*sptr == '#') {
    switch(*(++sptr)) {
    case 'b': if(radixpf) goto fail; else { radix = 2; radixpf = 1; } break;
    case 'o': if(radixpf) goto fail; else { radix = 8; radixpf = 1; } break;
    case 'd': if(radixpf) goto fail; else { radix = 10; radixpf = 1; } break;
    case 'x': if(radixpf) goto fail; else { radix = 16; radixpf = 1; } break;
    case 'e': if(exactpf) goto fail; else { exactf = 1; exactpf = 1; } break;
    case 'i': if(exactpf) goto fail; else { exactf = 0; exactpf = 1; } break;
    default: --sptr;
    }

    ++sptr;
  }

  /* check for embedded '#'s: */
  while((eptr = C_strchr(sptr, '#')) != NULL) {
    if(eptr[ 1 ] == '\0' || C_strchr("#.0123456789", eptr[ 1 ]) != NULL) {
      sharpf = 1;
      *eptr = '0';
    }
    else {
      n = C_SCHEME_FALSE;
      goto fini;
    }
  }

  /* check for rational representation: */
  if((eptr = C_strchr(sptr, '/')) != NULL) {
    *eptr = '\0';
    ratp = 1;

    switch(convert_string_to_number(sptr, radix, &n1, &fn1)) {
    case 0:
      n = C_SCHEME_FALSE;
      goto fini;

    case 1:
      fn1 = (double)n1;
      break;

      /* case 2: nop */
    }

    sptr = eptr + 1;
  }    
  
  /* convert number and return result: */
  switch(convert_string_to_number(sptr, radix, &n, &fn)) {
  case 0: 
    n = C_SCHEME_FALSE;
    break;

  case 1:
    if(sharpf || ratp || (exactpf && !exactf)) {
      n = C_flonum(&a, ratp ? fn1 / (double)n : (double)n);

      if(exactpf && exactf) n = C_i_inexact_to_exact(n);
    }
    else n = C_fix(n);

    break;

  case 2:
    n = C_flonum(&a, ratp ? fn1 / fn : fn);

    if(exactpf && exactf) n = C_i_inexact_to_exact(n);

    break;
  }

 fini:
  C_kontinue(k, n);
}


C_regparm C_word C_fcall convert_string_to_number(C_char *str, int radix, C_word *fix, double *flo)
{
  unsigned long ln;
  C_word n;
  C_char *eptr, *eptr2;
  double fn;
#if defined(__CYGWIN__) || defined(__MINGW32__)
  int len = C_strlen(str);

  if(len >= 4) {
    if(!C_strncmp(str, "+nan.0", len)) {
      *flo = 0.0/0.0;
      return 2;
    }
    else if(!C_strncmp(str, "+inf.0", len)) {
      *flo = 1.0/0.0;
      return 2;
    }
    else if(!C_strncmp(str, "-inf.0", len)) {
      *flo = -1.0/0.0;
      return 2;
    }
  }
#endif

  if(C_strpbrk(str, "xX\0") != NULL) return 0;

  errno = 0;
  n = C_strtol(str, &eptr, radix);
  
  if(((n == LONG_MAX || n == LONG_MIN) && errno == ERANGE) || *eptr != '\0') {
    if(radix != 10) {
      errno = 0;
      ln = C_strtoul(str, &eptr, radix);
      
      if((ln == 0 && errno == EINVAL) || (ln == ULONG_MAX && errno == ERANGE)) return 0;

      *flo = (double)ln;
      return 2;
    }

    errno = 0;
    fn = C_strtod(str, &eptr2);

    if(fn == HUGE_VAL && errno == ERANGE) return 0;
    else if(eptr2 == str) return 0;
    else if(*eptr2 == '\0' || (eptr != eptr2 && !C_strncmp(eptr2, ".0", C_strlen(eptr2)))) {
      *flo = fn;
      return 2;
    }

    return 0;
  }
  else if((n & C_INT_SIGN_BIT) != ((n << 1) & C_INT_SIGN_BIT)) { /* doesn't fit into fixnum? */
    if(*eptr == '\0' || !C_strncmp(eptr, ".0", C_strlen(eptr))) {
      *flo = (double)n;
      return 2;
    }
    else return 0;
  }
  else {
    *fix = n;
    return 1;
  }
}


void C_ccall C_number_to_string(C_word c, C_word closure, C_word k, C_word num, ...)
{
  C_word radix, *a;
  C_char *p;
  double f;
  va_list v;

  if(c == 3) radix = 10;
  else if(c == 4) {
    va_start(v, num);
    radix = va_arg(v, C_word);
    va_end(v);
    
    if(radix & C_FIXNUM_BIT) radix = C_unfix(radix);
    else barf(C_BAD_ARGUMENT_TYPE_ERROR, "number->string", radix);
  }
  else C_bad_argc(c, 3);

  if(num & C_FIXNUM_BIT) {
    num = C_unfix(num);

    switch(radix) {
    case 2:
      buffer[ 65 ] = '\0';
      p = buffer + 65;

      do {
	*(--p) = (num & 1) ? '1' : '0';
	num /= 2;
      } while(num);

      break;
     
#ifdef C_SIXTY_FOUR
    case 8: C_sprintf(p = buffer, C_text("%lo"), num); break;
    case 10: C_sprintf(p = buffer, C_text("%ld"), num); break;
    case 16: C_sprintf(p = buffer, C_text("%lx"), num); break;
#else
    case 8: C_sprintf(p = buffer, C_text("%o"), num); break;
    case 10: C_sprintf(p = buffer, C_text("%d"), num); break;
    case 16: C_sprintf(p = buffer, C_text("%x"), num); break;
#endif
    default: barf(C_BAD_ARGUMENT_TYPE_ERROR, "number->string", C_fix(radix));
    }

  fini:
    radix = C_strlen(p);
    
    if(!C_demand(C_bytestowords(radix) + 1)) {
      C_save(k);
      cons_string_trampoline(NULL);
    }

    a = C_alloc((C_bytestowords(radix) + 1));
    radix = C_string(&a, radix, p);
    C_kontinue(k, radix);
  }

  if(!C_immediatep(num) && C_block_header(num) == C_FLONUM_TAG) {
    f = C_flonum_magnitude(num);

    if(C_fits_in_unsigned_int_p(num) == C_SCHEME_TRUE) {
      switch(radix) {
	case 8:
	  C_sprintf(p = buffer, "%o", (unsigned int)f);
	  goto fini;

	case 16:
	  C_sprintf(p = buffer, "%x", (unsigned int)f);
	  goto fini;
      }
    } 

#if defined(__CYGWIN__) || defined(__MINGW32__)
    if(C_isnan(f)) {
      C_strcpy(p = buffer, "+nan.0");
      goto fini;
    }
    else if(C_isinf(f)) {
      C_sprintf(p = buffer, "%cinf.0", f > 0 ? '+' : '-');
      goto fini;
    }
#endif

#ifdef HAVE_GCVT
    C_gcvt(f, FLONUM_PRINT_PRECISION, buffer);
#else
    C_sprintf(buffer, C_text("%.*g"), FLONUM_PRINT_PRECISION, f);
#endif

    if((p = C_strpbrk(buffer, C_text(".eE"))) == NULL) {
      if(*buffer == 'i' || *buffer == 'n') { /* inf or nan */
	C_memmove(buffer + 1, buffer, C_strlen(buffer) + 1);
	*buffer = '+';
      }
      else if(buffer[ 1 ] != 'i') C_strcat(buffer, C_text(".0")); /* negative infinity? */
    }

    p = buffer;
    goto fini;
  }

  barf(C_BAD_ARGUMENT_TYPE_ERROR, "number->string", num);
}


void cons_string_trampoline(void *dummy)
{
  int len = C_strlen(buffer);
  C_word k = C_restore,
      *a = C_alloc(C_bytestowords(len) + 1);

  C_kontinue(k, C_string(&a, len, buffer));
}


void C_ccall C_get_argv(C_word c, C_word closure, C_word k)
{
  int i, cells;

  if(c != 2) C_bad_argc(c, 2);

  i = C_main_argc;
  cells = 0;

  while(i--)
    cells += 7 + C_align(C_strlen(C_main_argv[ i ]));

  C_save(k);
  C_save(C_fix(cells));

  if(!C_demand(cells)) C_reclaim(get_argv_2, NULL);

  get_argv_2(NULL);
}


void get_argv_2(void *dummy)
{
  int cells = C_unfix(C_restore),
      i = C_main_argc;
  C_word k = C_restore, 
         *a = C_alloc(cells),
         list, str;
  
  for(list = C_SCHEME_END_OF_LIST; i--; list = C_pair(&a, str, list))
    str = C_string2(&a, C_main_argv[ i ]);

  C_kontinue(k, list);
}


void C_ccall C_make_structure(C_word c, C_word closure, C_word k, C_word type, ...)
{
  va_list v;
  int i;

  va_start(v, type);

  for(i = c - 3; i--; C_save(va_arg(v, C_word)));

  va_end(v);
  C_save(type);
  C_save(k);

  if(!C_demand(c - 1)) 
    C_reclaim(make_structure_2, NULL);

  make_structure_2(NULL);
}


void make_structure_2(void *dummy)
{
  C_word k = C_restore,
      type = C_restore,
      size = C_rest_count(0),
      *a = C_alloc(size + 2),
      *s = a,
      s0 = (C_word)s;

  *(s++) = C_STRUCTURE_TYPE | (size + 1);
  *(s++) = type;
  s += size;

  while(size--)
    *(--s) = C_restore;

  C_kontinue(k, s0);
}


void C_ccall C_make_symbol(C_word c, C_word closure, C_word k, C_word name)
{
  C_word ab[ C_SIZEOF_SYMBOL ], *a = ab,
         s0 = (C_word)a;

  *(a++) = C_SYMBOL_TYPE | (C_SIZEOF_SYMBOL - 1);
  *(a++) = C_SCHEME_UNBOUND;
  *(a++) = name;
#ifdef C_EXTRA_SYMBOL_SLOT
  *a = C_SCHEME_UNDEFINED;
#endif
  C_kontinue(k, s0);
}


void C_ccall C_make_pointer(C_word c, C_word closure, C_word k)
{
  C_word ab[ 2 ], *a = ab,
         p;

  p = C_mpointer(&a, NULL);
  C_kontinue(k, p);
}


void C_ccall C_make_tagged_pointer(C_word c, C_word closure, C_word k, C_word tag)
{
  C_word ab[ 3 ], *a = ab,
         p;

  p = C_taggedmpointer(&a, tag, NULL);
  C_kontinue(k, p);
}


void C_ccall C_ensure_heap_reserve(C_word c, C_word closure, C_word k, C_word n)
{
  C_save(k);

  if(!C_demand(C_bytestowords(C_unfix(n))))
    C_reclaim(generic_trampoline, NULL);

  generic_trampoline(NULL);
}


void generic_trampoline(void *dummy)
{
  C_word k = C_restore;

  C_kontinue(k, C_SCHEME_UNDEFINED);
}


void C_ccall C_return_to_host(C_word c, C_word closure, C_word k)
{
  return_to_host = 1;
  C_save(k);
  C_reclaim(generic_trampoline, NULL);
}


void C_ccall C_file_info(C_word c, C_word closure, C_word k, C_word name)
{
  C_save(k);
  C_save(name);
  
  if(!C_demand(FILE_INFO_SIZE + 1 + C_SIZEOF_FLONUM * 3)) C_reclaim(file_info_2, NULL);

  file_info_2(NULL);
}


void file_info_2(void *dummy)
{
  C_word name = C_restore,
      k = C_restore,
      *a = C_alloc(FILE_INFO_SIZE + 1 + C_SIZEOF_FLONUM * 3),
      v = C_SCHEME_FALSE,
      t, f1, f2, f3;
  int len = C_header_size(name);

#ifdef _MSC_VER
  struct _stat buf;
#else
  struct stat buf;
#endif
  C_strncpy(buffer, C_c_string(name), len);
  buffer[ len ] = '\0';

#ifdef _MSC_VER
  if(_stat(buffer, &buf) != 0) v = C_SCHEME_FALSE;
#else
  if(stat(buffer, &buf) != 0) v = C_SCHEME_FALSE;
#endif
  else {
    switch(buf.st_mode & S_IFMT) {
    case S_IFDIR: t = 1; break;
#if !defined(_MSC_VER)
    case S_IFIFO: t = 3; break;
# if !defined(__MINGW32__)
    case S_IFSOCK: t = 4; break;
# endif
#endif
    default: t = 0;
    }

    f1 = C_flonum(&a, buf.st_atime);
    f2 = C_flonum(&a, buf.st_ctime);
    f3 = C_flonum(&a, buf.st_mtime);
    v = C_vector(&a, FILE_INFO_SIZE, f1, f2, f3,
		 C_fix(buf.st_size), C_fix(t), C_fix(buf.st_mode), C_fix(buf.st_uid) ); 
  }

  C_kontinue(k, v);
}


/* The following code was contributed by Sergey Khorev: */
#if defined(_MSC_VER) && !defined(_DLL)
/* we're using static C runtime
 * each module has its own environment block
 * use WinAPI to have consistent look to environment */

# define ENV_SIZE 32767
static char *envbuf;
static char *C_getenv(const char *var)
{
  envbuf = (char *)malloc(ENV_SIZE);
  if(!envbuf)
    return NULL;
  if(!GetEnvironmentVariable(var, envbuf, ENV_SIZE))
  {
    free(envbuf);
    return NULL;
  }
  else
    return envbuf;
}


static void C_free_envbuf()
{
  free(envbuf);
}
#else
# define C_getenv(v) getenv(v)
# define C_free_envbuf() {}
#endif


void C_ccall C_get_environment_variable(C_word c, C_word closure, C_word k, C_word name)
{
  int len;

  if(c != 3) C_bad_argc(c, 3);

  if(C_immediatep(name) || C_header_bits(name) != C_STRING_TYPE)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "getenv", name);
  
  if((len = C_header_size(name)) >= STRING_BUFFER_SIZE)
    C_kontinue(k, C_SCHEME_FALSE);

  strncpy(buffer, C_c_string(name), len);
  buffer[ len ] = '\0';

  if((save_string = C_getenv(buffer)) == NULL)
    C_kontinue(k, C_SCHEME_FALSE);

  C_save(k);
  
  len = C_strlen(save_string);
  if(!C_demand(1 + C_bytestowords(len + 1)))
    C_reclaim(get_environment_variable_2, NULL);

  get_environment_variable_2(NULL);
}


void get_environment_variable_2(void *dummy)
{
  int len = C_strlen(save_string);
  C_word k = C_restore,
         *a = C_alloc(1 + C_bytestowords(len + 1)),
         str = C_string(&a, len, save_string);
  
  C_free_envbuf();
  C_kontinue(k, str);
}


void C_ccall C_get_symbol_table_info(C_word c, C_word closure, C_word k)
{
  double d1, d2;
  int n = 0, total;
  C_SYMBOL_TABLE *stp;
  C_word x, y, ab[ WORDS_PER_FLONUM * 2 + 5 ], *a = ab; /* 2 flonums + 1 vector of 4 elements */

  for(stp = symbol_table_list; stp != NULL; stp = stp->next)
    ++n;
  
  d1 = compute_symbol_table_load(&d2, &total);
  x = C_flonum(&a, d1);
  y = C_flonum(&a, d2);
  C_kontinue(k, C_vector(&a, 4, x, y, C_fix(total), C_fix(n)));
}


void C_ccall C_get_memory_info(C_word c, C_word closure, C_word k)
{
  C_word ab[ 3 ], *a = ab;

  C_kontinue(k, C_vector(&a, 2, C_fix(heap_size), C_fix(stack_size)));
}


void C_ccall C_context_switch(C_word c, C_word closure, C_word k, C_word state)
{
  C_word n = C_header_size(state) - 1,
    adrs = C_block_item(state, 0);
  TRAMPOLINE trampoline;

  C_temporary_stack = C_temporary_stack_bottom - n;
  C_memcpy(C_temporary_stack, (C_word *)state + 2, n * sizeof(C_word));
  trampoline = (TRAMPOLINE)C_u_i_car(adrs);
  trampoline((void *)C_u_i_cdr(adrs));
}


void C_ccall C_peek_signed_integer(C_word c, C_word closure, C_word k, C_word v, C_word index)
{
  C_word x = C_block_item(v, C_unfix(index));

  if((x & C_INT_SIGN_BIT) != ((x << 1) & C_INT_SIGN_BIT)) {
    C_save(k);
    C_temporary_flonum = (double)x;
    cons_flonum_trampoline(NULL);
  }

  C_kontinue(k, C_fix(x));
}


void C_ccall C_peek_unsigned_integer(C_word c, C_word closure, C_word k, C_word v, C_word index)
{
  C_word x = C_block_item(v, C_unfix(index));

  if((x & C_INT_SIGN_BIT) || ((x << 1) & C_INT_SIGN_BIT)) {
    C_save(k);
    C_temporary_flonum = (double)(C_uword)x;
    cons_flonum_trampoline(NULL);
  }

  C_kontinue(k, C_fix(x));
}


void C_ccall C_decode_seconds(C_word c, C_word closure, C_word k, C_word secs, C_word mode)
{
  time_t tsecs;
  struct tm *tmt;
  C_word ab[ 11 ], *a = ab,
         info;

  tsecs = (time_t)((secs & C_FIXNUM_BIT) != 0 ? C_unfix(secs) : C_flonum_magnitude(secs));
  
  if(mode == C_SCHEME_FALSE) tmt = C_localtime(&tsecs);
  else tmt = C_gmtime(&tsecs);

  if(tmt  == NULL)
    C_kontinue(k, C_SCHEME_FALSE);
  
  info = C_vector(&a, 10, C_fix(tmt->tm_sec), C_fix(tmt->tm_min), C_fix(tmt->tm_hour),
		  C_fix(tmt->tm_mday), C_fix(tmt->tm_mon), C_fix(tmt->tm_year),
		  C_fix(tmt->tm_wday), C_fix(tmt->tm_yday),
		  tmt->tm_isdst > 0 ? C_SCHEME_TRUE : C_SCHEME_FALSE,
#ifdef C_MACOSX
		  C_fix(tmt->tm_gmtoff)
#else
		  C_fix(timezone)
#endif
		  );
  C_kontinue(k, info);
}


void C_ccall C_machine_type(C_word c, C_word closure, C_word k)
{
  C_word *a, s;

  if(c != 2) C_bad_argc(c, 2);

#if defined(__alpha__)
  a = C_alloc(2 + C_bytestowords(5));
  s = C_string2(&a, "alpha");
#elif defined(__mips__)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "mips");
#elif defined(__hppa__)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "hppa");
#elif defined(__sparc_v9__) || defined(__sparcv9)
  a = C_alloc(2 + C_bytestowords(10));
  s = C_string2(&a, "ultrasparc");
#elif defined(__sparc__)
  a = C_alloc(2 + C_bytestowords(5));
  s = C_string2(&a, "sparc");
#elif defined(__ppc__)
  a = C_alloc(2 + C_bytestowords(3));
  s = C_string2(&a, "ppc");
#elif defined(_M_IX86) || defined(__i386__)
  a = C_alloc(2 + C_bytestowords(3));
  s = C_string2(&a, "x86");
#elif defined(__ia64__)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "ia64");
#elif defined(__x86_64__)
  a = C_alloc(2 + C_bytestowords(6));
  s = C_string2(&a, "x86-64");
#elif defined(__arm__)
  a = C_alloc(2 + C_bytestowords(3));
  s = C_string2(&a, "arm");
#else
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "unknown");
#endif
  
  C_kontinue(k, s);
}


void C_ccall C_software_type(C_word c, C_word closure, C_word k)
{
  C_word *a, s;

  if(c != 2) C_bad_argc(c, 2);

#if defined(__CYGWIN__) || defined(__MINGW32__) || defined(_WIN32) || defined(__WINNT__)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "windows");
#elif defined(__unix__) || defined(C_MACOSX)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "unix");
#elif defined(C_MACOS)
  a = C_alloc(2 + C_bytestowords(5));
  s = C_string2(&a, "macos");
#elif defined(ECOS)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "ecos");
#else
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "unknown");
#endif

 C_kontinue(k, s);
}


void C_ccall C_build_platform(C_word c, C_word closure, C_word k)
{
  C_word *a, s;

  if(c != 2) C_bad_argc(c, 2);

#if defined(__CYGWIN__)
  a = C_alloc(2 + C_bytestowords(6));
  s = C_string2(&a, "cygwin");
#elif defined(_MSC_VER)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "msvc");
#elif defined(__SUNPRO_C)
  a = C_alloc(2 + C_bytestowords(3));
  s = C_string2(&a, "sun");
#elif defined(__MINGW32__)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "mingw32");
#elif defined(__GNUC__)
  a = C_alloc(2 + C_bytestowords(3));
  s = C_string2(&a, "gnu");
#elif defined(__MWERKS__)
  a = C_alloc(2 + C_bytestowords(10));
  s = C_string2(&a, "metrowerks");
#elif defined(__INTEL_COMPILER)
  a = C_alloc(2 + C_bytestowords(5));
  s = C_string2(&a, "intel");
#elif defined(__WATCOMC__)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "watcom");
#else
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "unknown");
#endif

 C_kontinue(k, s);
}


/* By Sergey Khorev: */
void C_ccall C_c_runtime(C_word c, C_word closure, C_word k)
{
  C_word *a, s;

  if(c != 2) C_bad_argc(c, 2);

#if defined(_MSC_VER)
# if defined(_DLL)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "dynamic");
# else
  a = C_alloc(2 + C_bytestowords(6));
  s = C_string2(&a, "static");
# endif
#else
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "unknown");
#endif

 C_kontinue(k, s);
}


void C_ccall C_software_version(C_word c, C_word closure, C_word k)
{
  C_word *a, s;

  if(c != 2) C_bad_argc(c, 2);

#if defined(__linux__)
  a = C_alloc(2 + C_bytestowords(5));
  s = C_string2(&a, "linux");
#elif defined(__FreeBSD__)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "freebsd");
#elif defined(__NetBSD__)
  a = C_alloc(2 + C_bytestowords(6));
  s = C_string2(&a, "netbsd");
#elif defined(__OpenBSD__)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "openbsd");
#elif defined(C_MACOSX)
  a = C_alloc(2 + C_bytestowords(6));
  s = C_string2(&a, "macosx");
#elif defined(__hpux__)
  a = C_alloc(2 + C_bytestowords(4));
  s = C_string2(&a, "hpux");
#elif defined(__DragonFly__)
  a = C_alloc(2 + C_bytestowords(9));
  s = C_string2(&a, "dragonfly");
#elif defined(__sun__)
 #if defined(__svr4__)
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "solaris");
 #else
  a = C_alloc(2 + C_bytestowords(5));
  s = C_string2(&a, "sunos");
 #endif
#else
  a = C_alloc(2 + C_bytestowords(7));
  s = C_string2(&a, "unknown");
#endif

 C_kontinue(k, s);
}


/* Register finalizer: */

void C_ccall C_register_finalizer(C_word c, C_word closure, C_word k, C_word x, C_word proc)
{
  if(C_immediatep(x)) C_kontinue(k, x );

  C_do_register_finalizer(x, proc);
  C_kontinue(k, x);
}


void C_ccall C_do_register_finalizer(C_word x, C_word proc)
{
  C_word *ptr;
  int n, i;
  FINALIZER_NODE *flist;

  if(finalizer_free_list == NULL) {
    if((flist = (FINALIZER_NODE *)C_malloc(sizeof(FINALIZER_NODE))) == NULL)
      panic(C_text("out of memory - can not allocate finalizer node"));

    ++allocated_finalizer_count;
  }
  else {
    flist = finalizer_free_list;
    finalizer_free_list = flist->next;
  }

  if(finalizer_list != NULL) finalizer_list->previous = flist;

  flist->previous = NULL;
  flist->next = finalizer_list;
  finalizer_list = flist;

  if(C_in_stackp(x)) C_mutate(&flist->item, x);
  else flist->item = x;

  if(C_in_stackp(proc)) C_mutate(&flist->finalizer, proc);
  else flist->finalizer = proc;

  ++live_finalizer_count;
}


int C_do_unregister_finalizer(C_word x)
{
  int n;
  FINALIZER_NODE *flist;

  for(flist = finalizer_list; flist != NULL; flist = flist->next) {
    if(flist->item == x) {
      if(flist->previous == NULL) finalizer_list = flist->next;
      else flist->previous->next = flist->next;

      return 1;
    }
  }

  return 0;
}


/* Dynamic loading of shared objects: */

void C_ccall C_set_dlopen_flags(C_word c, C_word closure, C_word k, C_word now, C_word global)
{
#if !defined(NO_DLOAD2) && defined(HAVE_DLFCN_H)
  dlopen_flags = (C_truep(now) ? RTLD_NOW : RTLD_LAZY) | (C_truep(global) ? RTLD_GLOBAL : RTLD_LOCAL);
#endif
  C_kontinue(k, C_SCHEME_UNDEFINED);
}


void C_ccall C_dload(C_word c, C_word closure, C_word k, C_word name, C_word entry, C_word reloadable)
{
#if !defined(NO_DLOAD2) && (defined(HAVE_DLFCN_H) || defined(HAVE_DL_H) || (defined(HAVE_LOADLIBRARY) && defined(HAVE_GETPROCADDRESS)))
  /* Force minor GC: otherwise the lf may contain pointers to stack-data
     (stack allocated interned symbols, for example) */
  C_save_and_reclaim(dload_2, NULL, 4, k, name, entry, reloadable);
#endif

  C_kontinue(k, C_SCHEME_FALSE);
}


#ifdef DLOAD_2_DEFINED
# undef DLOAD_2_DEFINED
#endif

#if !defined(NO_DLOAD2) && defined(HAVE_DL_H) && !defined(DLOAD_2_DEFINED)
# ifdef __hpux__
#  define DLOAD_2_DEFINED
void dload_2(void *dummy)
{
  void *handle, *p;
  C_word reloadable = C_restore,
         entry = C_restore,
         name = C_restore,
         k = C_restore;
  C_char *mname = (C_char *)C_data_pointer(name);

  /*
   * C_fprintf(C_stderr,
   *   "shl_loading %s : %s\n",
   *   (char *) C_data_pointer(name),
   *   (char *) C_data_pointer(entry));
   */

  if(C_truep(reloadable) && (reload_lf = find_module_handle(mname)) != NULL) {
    shl_unload((shl_t)reload_lf->module_handle); /*** failure currently not handled - what does shl_unload() return ? */
  }
  else reload_lf = NULL;

  if ((handle = (void *) shl_load(mname,
				  BIND_IMMEDIATE | DYNAMIC_PATH,
				  0L)) != NULL) {
    shl_t shl_handle = (shl_t) handle;

    /*** This version does not check for C_dynamic_and_unsafe. Fix it. */
    if (shl_findsym(&shl_handle, (char *) C_data_pointer(entry), TYPE_PROCEDURE, &p) == 0) {
      current_module_name = C_strdup(mname);
      current_module_handle = handle;

      if(debug_mode) {
	if(reload_lf != NULL)
	  C_printf(C_text("[debug] reloading compiled module `%s' (previous handle was " UWORD_FORMAT_STRING ", new is "
			  UWORD_FORMAT_STRING ")\n"), current_module_name, (C_uword)reload_lf->module_handle, 
		   (C_uword)current_module_handle);
	else 
	  C_printf(C_text("[debug] loading compiled module `%s' (handle is " UWORD_FORMAT_STRING ")\n"),
		   current_module_name, (C_uword)current_module_handle);
      }

      ((C_proc2)p)(2, C_SCHEME_UNDEFINED, k);
    } else {
      C_dlerror = (char *) C_strerror(errno);
      shl_unload(shl_handle);
    }
  } else {
    C_dlerror = (char *) C_strerror(errno);
  }

  C_kontinue(k, C_SCHEME_FALSE);
}
# endif
#endif


#if !defined(NO_DLOAD2) && defined(HAVE_DLFCN_H) && !defined(DLOAD_2_DEFINED)
# ifndef __hpux__
#  define DLOAD_2_DEFINED
void dload_2(void *dummy)
{
  void *handle, *p, *p2;
  C_word 
    reloadable = C_restore,
    entry = C_restore,
    name = C_restore,
    k = C_restore;
  C_char *topname = (C_char *)C_data_pointer(entry);
  C_char *mname = (C_char *)C_data_pointer(name);
  C_char *tmp;
  int ok;

  if(C_truep(reloadable) && (reload_lf = find_module_handle(mname)) != NULL) {
    if(C_dlclose(reload_lf->module_handle) != 0)
      panic(C_text("Unable to unload previously loaded compiled code"));
  }
  else reload_lf = NULL;

  if((handle = C_dlopen(mname, dlopen_flags)) != NULL) {
    if((p = C_dlsym(handle, topname)) == NULL) {
      tmp = (C_char *)C_malloc(C_strlen(topname) + 2);
      
      if(tmp == NULL)
	panic(C_text("out of memory - can not allocate toplevel name string"));
      
      C_strcpy(tmp, C_text("_"));
      C_strcat(tmp, topname);
      p = C_dlsym(handle, tmp);
      C_free(tmp);
    }

    if(p != NULL) {
      /* check whether dloaded code is not a library unit and matches current safety setting: */
      if((p2 = C_dlsym(handle, C_text("C_dynamic_and_unsafe"))) == NULL)
	p2 = C_dlsym(handle, C_text("_C_dynamic_and_unsafe"));

#ifdef C_UNSAFE_RUNTIME
      ok = p2 != NULL;		/* unsafe runtime, unsafe code */
#else
      ok = p2 == NULL;		/* safe runtime, safe code */
#endif
      
      if(!ok && !C_strcmp(topname, "C_toplevel")) /* unsafe marker not found and this is not a library unit? */
#ifdef C_UNSAFE_RUNTIME
	barf(C_RUNTIME_UNSAFE_DLOAD_SAFE_ERROR, NULL);
#else
	barf(C_RUNTIME_SAFE_DLOAD_UNSAFE_ERROR, NULL);
#endif

      current_module_name = C_strdup(mname);
      current_module_handle = handle;

      if(debug_mode) {
	if(reload_lf != NULL)
	  C_printf(C_text("[debug] reloading compiled module `%s' (previous handle was " UWORD_FORMAT_STRING ", new is "
			  UWORD_FORMAT_STRING ")\n"), current_module_name, (C_uword)reload_lf->module_handle, 
		   (C_uword)current_module_handle);
	else 
	  C_printf(C_text("[debug] loading compiled module `%s' (handle is " UWORD_FORMAT_STRING ")\n"),
		   current_module_name, (C_uword)current_module_handle);
      }

      ((C_proc2)p)(2, C_SCHEME_UNDEFINED, k); /* doesn't return */
    }

    C_dlclose(handle);
  }
  
  C_dlerror = (char *)dlerror();
  C_kontinue(k, C_SCHEME_FALSE);
}
# endif
#endif


#if !defined(NO_DLOAD2) && (defined(HAVE_LOADLIBRARY) && defined(HAVE_GETPROCADDRESS)) && !defined(DLOAD_2_DEFINED)
# define DLOAD_2_DEFINED
void dload_2(void *dummy)
{
  HINSTANCE handle;
  int ok;
  FARPROC p = NULL, p2;
  C_word
    reloadable = C_restore,
    entry = C_restore,
    name = C_restore,
    k = C_restore;
  C_char *topname = (C_char *)C_data_pointer(entry);
  C_char *mname = (C_char *)C_data_pointer(name);

  /* cannot use LoadLibrary on non-DLLs, so we use extension checking */
  if (C_header_size(name) >= 5) {
    char *n = (char*) C_data_pointer(name);
    int l = C_header_size(name);
    if (C_strncasecmp(".dll", n+l-5, 4) && 
	C_strncasecmp(".so", n+l-4, 3))
      C_kontinue(k, C_SCHEME_FALSE);
  }

  if(C_truep(reloadable) && (reload_lf = find_module_handle((char *)C_data_pointer(name))) != NULL) {
    FreeLibrary((HINSTANCE)reload_lf->module_handle); /*** failure currently not handled - what does FreeLibrary() return ? */
  }
  else reload_lf = NULL;

  if((handle = LoadLibrary(mname)) != NULL) {
    if ((p = GetProcAddress(handle, topname)) != NULL) {
      /* check whether dloaded code is not a library unit and matches current safety setting: */
      p2 = GetProcAddress(handle, C_text("C_dynamic_and_unsafe"));

#ifdef C_UNSAFE_RUNTIME
      ok = p2 != NULL;		/* unsafe runtime, unsafe code */
#else
      ok = p2 == NULL;		/* safe runtime, safe code */
#endif
      
      if(!ok && !C_strcmp(topname, "C_toplevel")) /* unsafe marker not found and this is not a library unit? */
#ifdef C_UNSAFE_RUNTIME
	barf(C_RUNTIME_UNSAFE_DLOAD_SAFE_ERROR, NULL);
#else
        barf(C_RUNTIME_SAFE_DLOAD_UNSAFE_ERROR, NULL);
#endif

      current_module_name = C_strdup(mname);
      current_module_handle = handle;

      if(debug_mode) {
	if(reload_lf != NULL)
	  C_printf(C_text("[debug] reloading compiled module `%s' (previous handle was " UWORD_FORMAT_STRING ", new is "
			  UWORD_FORMAT_STRING ")\n"), current_module_name, (C_uword)reload_lf->module_handle, 
		   (C_uword)current_module_handle);
	else 
	  C_printf(C_text("[debug] loading compiled module `%s' (handle is " UWORD_FORMAT_STRING ")\n"),
		   current_module_name, (C_uword)current_module_handle);
      }

      ((C_proc2)p)(2, C_SCHEME_UNDEFINED, k);
    }
    else FreeLibrary(handle);
  }

  C_dlerror = (char *) C_strerror(errno);
  C_kontinue(k, C_SCHEME_FALSE);
}
#endif


C_word C_ccall C_dunload(C_word name)
{
  LF_LIST *m = find_module_handle(C_c_string(name));

  if(m == NULL) return C_SCHEME_FALSE;

#ifndef NO_DLOAD2
# if defined(__hpux__) && defined(HAVE_DL_H)
  shl_unload((shl_t)m->module_handle);
# elif defined(HAVE_DLFCN_H)
  if(dlclose(m->module_handle) != 0) return C_SCHEME_FALSE;
# elif defined(HAVE_LOADLIBRARY)
  FreeLibrary(m->module_handle);
# else
  return C_SCHEME_FALSE;
# endif
# else
  return C_SCHEME_FALSE;
#endif

  C_unregister_lf(m);
  return C_SCHEME_TRUE;
}


void C_ccall C_become(C_word c, C_word closure, C_word k, C_word table)
{
  C_word tp, x, old, new, i, *p;

  i = forwarding_table_size;
  p = forwarding_table;

  for(tp = table; tp != C_SCHEME_END_OF_LIST; tp = C_u_i_cdr(tp)) {
    x = C_u_i_car(tp);
    old = C_u_i_car(x);
    new = C_u_i_cdr(x);

    if(i == 0) {
      if((forwarding_table = (C_word *)realloc(forwarding_table, (forwarding_table_size + 1) * 4 * sizeof(C_word))) == NULL)
	panic(C_text("out of memory - can not re-allocate forwarding table"));
	
      i = forwarding_table_size;
      p = forwarding_table + forwarding_table_size * 2;
      forwarding_table_size *= 2;
    }

    *(p++) = old;
    *(p++) = new;
    --i;
  }

  *p = 0;
  C_fromspace_top = C_fromspace_limit;
  C_save_and_reclaim(become_2, NULL, 1, k);
}


void become_2(void *dummy)
{
  C_word k = C_restore;
  *forwarding_table = 0;
  C_kontinue(k, C_SCHEME_UNDEFINED);
}


void C_ccall C_cpu_time(C_word c, C_word closure, C_word k)
{
  C_word u, s = 0;

#if defined(C_NONUNIX) || defined(__CYGWIN__)
  if(CLOCKS_PER_SEC == 1000) u = clock();
  else u = ((double)clock() / (double)CLOCKS_PER_SEC) * 1000;
#else
  struct rusage ru;

  if(C_getrusage(RUSAGE_SELF, &ru) == -1) u = 0;
  else {
    u = ru.ru_utime.tv_sec * 1000 + ru.ru_utime.tv_usec / 1000;
    s = ru.ru_stime.tv_sec * 1000 + ru.ru_stime.tv_usec / 1000;
  }
#endif
  
  C_values(4, C_SCHEME_UNDEFINED, k, C_fix(u & C_MOST_POSITIVE_FIXNUM), C_fix(s & C_MOST_POSITIVE_FIXNUM));
}


C_regparm C_word C_fcall C_a_i_make_locative(C_word **a, int c, C_word type, C_word object, C_word index, C_word weak)
{
  C_word *loc = *a;
  int offset, i, in = C_unfix(index);
  *a = loc + C_SIZEOF_LOCATIVE;

  loc[ 0 ] = C_LOCATIVE_TAG;

  switch(C_unfix(type)) {
  case C_SLOT_LOCATIVE: in *= sizeof(C_word); break;
  case C_U16_LOCATIVE:
  case C_S16_LOCATIVE: in *= 2; break;
  case C_U32_LOCATIVE:
  case C_F32_LOCATIVE:
  case C_S32_LOCATIVE: in *= 4; break;
  case C_F64_LOCATIVE: in *= 8; break;
  }

  offset = in + sizeof(C_header);
  loc[ 1 ] = object + offset;
  loc[ 2 ] = C_fix(offset);
  loc[ 3 ] = type;
  loc[ 4 ] = C_truep(weak) ? C_SCHEME_FALSE : object;

  for(i = 0; i < locative_table_count; ++i)
    if(locative_table[ i ] == C_SCHEME_UNDEFINED) {
      locative_table[ i ] = (C_word)loc;
      return (C_word)loc;
    }

  if(locative_table_count >= locative_table_size) {
    if(debug_mode == 2)
      C_printf(C_text("[debug] resizing locative table from %d to %d (count is %d)\n"), 
		      locative_table_size, locative_table_size * 2, locative_table_count);

    locative_table = (C_word *)C_realloc(locative_table, locative_table_size * 2 * sizeof(C_word));

    if(locative_table == NULL) 
      panic(C_text("out of memory - can not resize locative table"));

    locative_table_size *= 2;
  }

  locative_table[ locative_table_count++ ] = (C_word)loc;

  return (C_word)loc;
}


void C_ccall C_locative_ref(C_word c, C_word closure, C_word k, C_word loc)
{
  C_word *ptr, val;

  if(c != 3) C_bad_argc(c, 3);

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", loc);

  ptr = (C_word *)C_block_item(loc, 0);

  if(ptr == NULL) barf(C_LOST_LOCATIVE_ERROR, "locative-ref", loc);

  switch(C_unfix(C_block_item(loc, 2))) {
  case C_SLOT_LOCATIVE: C_kontinue(k, *ptr);
  case C_CHAR_LOCATIVE: C_kontinue(k, C_make_character(*((char *)ptr)));
  case C_U8_LOCATIVE: C_kontinue(k, C_fix(*((unsigned char *)ptr)));
  case C_S8_LOCATIVE: C_kontinue(k, C_fix(*((char *)ptr)));
  case C_U16_LOCATIVE: C_kontinue(k, C_fix(*((unsigned short *)ptr)));
  case C_S16_LOCATIVE: C_kontinue(k, C_fix(*((short *)ptr)));
  case C_U32_LOCATIVE: C_peek_unsigned_integer(0, 0, k, (C_word)(ptr - 1), 0);
  case C_S32_LOCATIVE: C_peek_signed_integer(0, 0, k, (C_word)(ptr - 1), 0);
  case C_F32_LOCATIVE: C_temporary_flonum = *((float *)ptr); C_cons_flonum(0, 0, k);
  case C_F64_LOCATIVE: C_temporary_flonum = *((double *)ptr); C_cons_flonum(0, 0, k);
  default: panic(C_text("bad locative type"));
  }
}


C_regparm C_word C_fcall C_i_locative_set(C_word loc, C_word x)
{
  C_word *ptr, val;

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", loc);

  ptr = (C_word *)C_block_item(loc, 0);

  if(ptr == NULL)
    barf(C_LOST_LOCATIVE_ERROR, "locative-set!", loc);

  switch(C_unfix(C_block_item(loc, 2))) {
  case C_SLOT_LOCATIVE: C_mutate(ptr, x); break;

  case C_CHAR_LOCATIVE:
    if((x & C_IMMEDIATE_TYPE_BITS) != C_CHARACTER_BITS)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
      
    *((char *)ptr) = C_character_code(x); 
    break;

  case C_U8_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((unsigned char *)ptr) = C_unfix(x); 
    break;

  case C_S8_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((char *)ptr) = C_unfix(x); 
    break;

  case C_U16_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((unsigned short *)ptr) = C_unfix(x); 
    break;

  case C_S16_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((short *)ptr) = C_unfix(x); 
    break;

  case C_U32_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0 && (C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG))
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);

    *((C_u32 *)ptr) = C_num_to_unsigned_int(x); 
    break;

  case C_S32_LOCATIVE: 
    if((x & C_FIXNUM_BIT) == 0 && (C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG))
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((C_s32 *)ptr) = C_num_to_int(x); 
    break;

  case C_F32_LOCATIVE: 
    if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((float *)ptr) = C_flonum_magnitude(x); 
    break;

  case C_F64_LOCATIVE: 
    if(C_immediatep(x) || C_block_header(x) != C_FLONUM_TAG)
      barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative-set!", x);
    
    *((double *)ptr) = C_flonum_magnitude(x); 
    break;

  default: panic(C_text("bad locative type"));
  }

  return C_SCHEME_UNDEFINED;
}


C_regparm C_word C_fcall C_i_locative_to_object(C_word loc)
{
  C_word *ptr;

  if(C_immediatep(loc) || C_block_header(loc) != C_LOCATIVE_TAG)
    barf(C_BAD_ARGUMENT_TYPE_ERROR, "locative->object", loc);

  ptr = (C_word *)C_u_i_car(loc);

  if(ptr == NULL) return C_SCHEME_FALSE;
  else return (C_word)ptr - C_unfix(C_u_i_cdr(loc));
}


/* GC protection of user-variables: */

C_regparm void C_fcall C_gc_protect(C_word **addr, int n)
{
  int k;

  if(collectibles_top + n >= collectibles_limit) {
    k = collectibles_limit - collectibles;
    collectibles = (C_word **)C_realloc(collectibles, sizeof(C_word *) * k * 2);

    if(collectibles == NULL)
      panic(C_text("out of memory - can not allocate GC protection vector"));
    
    collectibles_top = collectibles + k;
    collectibles_limit = collectibles + k * 2;
  }

  C_memcpy(collectibles_top, addr, n * sizeof(C_word *));
  collectibles_top += n;
}


C_regparm void C_fcall C_gc_unprotect(int n)
{
  collectibles_top -= n;
}


/* Map procedure-ptr to id or id to ptr: */

C_char *C_lookup_procedure_id(void *ptr)
{
  LF_LIST *lfl;
  C_PTABLE_ENTRY *pt;

  for(lfl = lf_list; lfl != NULL; lfl = lfl->next) {
    pt = lfl->ptable;

    if(pt != NULL) {
      while(pt->id != NULL) {
	if(pt->ptr == ptr) return pt->id;
	else ++pt;
      }
    }
  }

  return NULL;
}


void *C_lookup_procedure_ptr(C_char *id)
{
  LF_LIST *lfl;
  C_PTABLE_ENTRY *pt;

  for(lfl = lf_list; lfl != NULL; lfl = lfl->next) {
    pt = lfl->ptable;

    if(pt != NULL) {
      while(pt->id != NULL) {
	if(!C_strcmp(id, pt->id)) return pt->ptr;
	else ++pt;
      }
    }
  }

  return NULL;
}


void C_ccall C_copy_closure(C_word c, C_word closure, C_word k, C_word proc)
{
  int n = C_header_size(proc);

  if(!C_demand(n + 1)) C_save_and_reclaim(copy_closure_2, NULL, 2, proc, k);
  else {
    C_save(proc);
    C_save(k);
    copy_closure_2(NULL);
  }
}


static void copy_closure_2(void *dummy)
{
  C_word 
    k = C_restore,
    proc = C_restore;
  int cells = C_header_size(proc);
  C_word
    *ptr = C_alloc(cells + 1),
    *p = ptr;

  *(p++) = C_CLOSURE_TYPE | cells;
  C_memcpy_slots(p, C_data_pointer(proc), cells);
  C_kontinue(k, (C_word)ptr);
}


/* Creating black holes: */

void C_call_with_cthulhu(C_word c, C_word self, C_word k, C_word proc)
{
  C_word *a = C_alloc(3);
  
  k = C_closure(&a, 1, (C_word)termination_continuation);
  C_apply(4, C_SCHEME_UNDEFINED, k, proc, C_SCHEME_END_OF_LIST);
}
