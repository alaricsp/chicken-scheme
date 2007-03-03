;;;; posixunix.scm - Miscellaneous file- and process-handling routines
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


(declare
  (unit posix)
  (uses scheduler regex extras utils)
  (disable-interrupts)
  (usual-integrations)
  (hide ##sys#stat group-member _get-groups _ensure-groups posix-error)
  (foreign-declare #<<EOF
#include <signal.h>
#include <errno.h>

static int C_not_implemented(void);
int C_not_implemented() { return -1; }

static C_TLS int C_wait_status;

#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <pwd.h>

#ifdef ECOS
#include <cyg/posix/signal.h>
#endif

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#include <sys/mman.h>
#include <time.h>

#ifndef O_FSYNC
# define O_FSYNC O_SYNC
#endif

#ifndef PIPE_BUF
# ifdef __CYGWIN__
#  define PIPE_BUF       _POSIX_PIPE_BUF
# else
#  define PIPE_BUF 1024
# endif
#endif

#ifndef O_BINARY
# define O_BINARY        0
#endif
#ifndef O_TEXT
# define O_TEXT          0
#endif

#ifndef ARG_MAX
# define ARG_MAX 256
#endif

#ifndef MAP_FILE
# define MAP_FILE    0
#endif

#ifndef MAP_ANON
# define MAP_ANON    0
#endif

#if defined(HAVE_CRT_EXTERNS_H)
# include <crt_externs.h>
# define C_getenventry(i)       ((*_NSGetEnviron())[ i ])
#elif defined(C_MACOSX)
# define C_getenventry(i)       NULL
#else
extern char **environ;
# define C_getenventry(i)       (environ[ i ])
#endif

#ifndef ENV_MAX
# define ENV_MAX        1024
#endif

static C_TLS char *C_exec_args[ ARG_MAX ];
static C_TLS char *C_exec_env[ ENV_MAX ];
static C_TLS struct utsname C_utsname;
static C_TLS struct flock C_flock;
static C_TLS DIR *temphandle;
#ifndef ECOS
static C_TLS struct passwd *C_user;
#ifdef HAVE_GRP_H
static C_TLS struct group *C_group;
#else
struct C_fake_group {
  int gr_gid;
  int gr_mem[ 1 ];
  char *gr_name;
  char *gr_passwd;
};
static C_TLS struct C_fake_group *C_group;
#endif
static C_TLS int C_pipefds[ 2 ];
#endif
static C_TLS time_t C_secs;
static C_TLS struct tm C_tm;
static C_TLS fd_set C_fd_sets[ 2 ];
static C_TLS struct timeval C_timeval;
static C_TLS char C_hostbuf[ 256 ];
static C_TLS struct stat C_statbuf;

#define C_mkdir(str)        C_fix(mkdir(C_c_string(str), S_IRWXU | S_IRWXG | S_IRWXO))
#define C_chdir(str)        C_fix(chdir(C_c_string(str)))
#define C_rmdir(str)        C_fix(rmdir(C_c_string(str)))

#define C_opendir(x,h)          C_set_block_item(h, 0, (C_word) opendir(C_c_string(x)))
#define C_closedir(h)           (closedir((DIR *)C_block_item(h, 0)), C_SCHEME_UNDEFINED)
#define C_readdir(h,e)          C_set_block_item(e, 0, (C_word) readdir((DIR *)C_block_item(h, 0)))
#define C_foundfile(e,b)        (strcpy(C_c_string(b), ((struct dirent *) C_block_item(e, 0))->d_name), C_fix(strlen(((struct dirent *) C_block_item(e, 0))->d_name)))

#define C_curdir(buf)       (getcwd(C_c_string(buf), 256) ? C_fix(strlen(C_c_string(buf))) : C_SCHEME_FALSE)

#define open_binary_input_pipe(a, n, name)   C_mpointer(a, popen(C_c_string(name), "r"))
#define open_text_input_pipe(a, n, name)     open_binary_input_pipe(a, n, name)
#define open_binary_output_pipe(a, n, name)  C_mpointer(a, popen(C_c_string(name), "w"))
#define open_text_output_pipe(a, n, name)    open_binary_output_pipe(a, n, name)
#define close_pipe(p)                        C_fix(pclose(C_port_file(p)))

#define C_set_file_ptr(port, ptr)  (C_set_block_item(port, 0, (C_block_item(ptr, 0))), C_SCHEME_UNDEFINED)

#ifndef ECOS
#define C_fork              fork
#define C_waitpid(id, o)    C_fix(waitpid(C_unfix(id), &C_wait_status, C_unfix(o)))
#define C_getpid            getpid
#define C_getppid           getppid
#define C_kill(id, s)       C_fix(kill(C_unfix(id), C_unfix(s)))
#define C_getuid            getuid
#define C_getgid            getgid
#define C_geteuid           geteuid
#define C_getegid           getegid
#define C_chown(fn, u, g)   C_fix(chown(C_data_pointer(fn), C_unfix(u), C_unfix(g)))
#define C_chmod(fn, m)      C_fix(chmod(C_data_pointer(fn), C_unfix(m)))
#define C_setuid(id)        C_fix(setuid(C_unfix(id)))
#define C_setgid(id)        C_fix(setgid(C_unfix(id)))
#define C_setsid(dummy)     C_fix(setsid())
#define C_setpgid(x, y)     C_fix(setpgid(C_unfix(x), C_unfix(y)))
#define C_getpgid(x)        C_fix(getpgid(C_unfix(x)))
#define C_symlink(o, n)     C_fix(symlink(C_data_pointer(o), C_data_pointer(n)))
#define C_readlink(f, b)    C_fix(readlink(C_data_pointer(f), C_data_pointer(b), FILENAME_MAX))
#define C_getpwnam(n)       C_mk_bool((C_user = getpwnam((char *)C_data_pointer(n))) != NULL)
#define C_getpwuid(u)       C_mk_bool((C_user = getpwuid(C_unfix(u))) != NULL)
#ifdef HAVE_GRP_H
#define C_getgrnam(n)       C_mk_bool((C_group = getgrnam((char *)C_data_pointer(n))) != NULL)
#define C_getgrgid(u)       C_mk_bool((C_group = getgrgid(C_unfix(u))) != NULL)
#else
#define C_getgrnam(n)       C_SCHEME_FALSE
#define C_getgrgid(n)       C_SCHEME_FALSE
#endif
#define C_pipe(d)           C_fix(pipe(C_pipefds))
#define C_truncate(f, n)    C_fix(truncate((char *)C_data_pointer(f), C_num_to_int(n)))
#define C_ftruncate(f, n)   C_fix(ftruncate(C_unfix(f), C_num_to_int(n)))
#endif
#define C_uname             C_fix(uname(&C_utsname))
#define C_fdopen(a, n, fd, m) C_mpointer(a, fdopen(C_unfix(fd), C_c_string(m)))
#define C_C_fileno(p)       C_fix(fileno(C_port_file(p)))
#define C_dup(x)            C_fix(dup(C_unfix(x)))
#define C_dup2(x, y)        C_fix(dup2(C_unfix(x), C_unfix(y)))
#define C_alarm             alarm
#define C_setvbuf(p, m, s)  C_fix(setvbuf(C_port_file(p), NULL, C_unfix(m), C_unfix(s)))
#define C_access(fn, m)     C_fix(access((char *)C_data_pointer(fn), C_unfix(m)))
#define C_close(fd)         C_fix(close(C_unfix(fd)))
#define C_sleep             sleep

#define C_putenv(s)         C_fix(putenv((char *)C_data_pointer(s)))
#define C_stat(fn)          C_fix(stat((char *)C_data_pointer(fn), &C_statbuf))
#define C_lstat(fn)         C_fix(lstat((char *)C_data_pointer(fn), &C_statbuf))
#define C_fstat(f)          C_fix(fstat(C_unfix(f), &C_statbuf))

#define C_islink            ((C_statbuf.st_mode & S_IFMT) == S_IFLNK)
#define C_isreg             ((C_statbuf.st_mode & S_IFMT) == S_IFREG)

#ifdef C_GNU_ENV
# define C_setenv(x, y)     C_fix(setenv((char *)C_data_pointer(x), (char *)C_data_pointer(y), 1))
#else
static C_word C_fcall C_setenv(C_word x, C_word y);
C_word C_fcall C_setenv(C_word x, C_word y) {
  char *sx = C_data_pointer(x),
       *sy = C_data_pointer(y);
  int n1 = C_strlen(sx), n2 = C_strlen(sy);
  char *buf = (char *)C_malloc(n1 + n2 + 2);
  if(buf == NULL) return(C_fix(0));
  else {
    C_strcpy(buf, sx);
    buf[ n1 ] = '=';
    C_strcpy(buf + n1 + 1, sy);
    return(C_fix(putenv(buf)));
  }
}
#endif

static void C_fcall C_set_arg_string(char **where, int i, char *a, int len) {
  char *ptr;
  if(a != NULL) {
    ptr = (char *)C_malloc(len + 1);
    C_memcpy(ptr, a, len);
    ptr[ len ] = '\0';
  }
  else ptr = NULL;
  where[ i ] = ptr;
}

static void C_fcall C_free_exec_args() {
  char **a = C_exec_args;
  while((*a) != NULL) C_free(*(a++));
}

static void C_fcall C_free_exec_env() {
  char **a = C_exec_env;
  while((*a) != NULL) C_free(*(a++));
}

#define C_set_exec_arg(i, a, len)      C_set_arg_string(C_exec_args, i, a, len)
#define C_set_exec_env(i, a, len)      C_set_arg_string(C_exec_env, i, a, len)

#define C_execvp(f)         C_fix(execvp(C_data_pointer(f), C_exec_args))
#define C_execve(f)         C_fix(execve(C_data_pointer(f), C_exec_args, C_exec_env))

#if defined(__FreeBSD__) || defined(C_MACOSX) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__sgi__) || defined(sgi) || defined(__DragonFly__) || defined(__SUNPRO_C)
static C_TLS int C_uw;
# define C_WIFEXITED(n)      (C_uw = C_unfix(n), C_mk_bool(WIFEXITED(C_uw)))
# define C_WIFSIGNALED(n)    (C_uw = C_unfix(n), C_mk_bool(WIFSIGNALED(C_uw)))
# define C_WIFSTOPPED(n)     (C_uw = C_unfix(n), C_mk_bool(WIFSTOPPED(C_uw)))
# define C_WEXITSTATUS(n)    (C_uw = C_unfix(n), C_fix(WEXITSTATUS(C_uw)))
# define C_WTERMSIG(n)       (C_uw = C_unfix(n), C_fix(WTERMSIG(C_uw)))
# define C_WSTOPSIG(n)       (C_uw = C_unfix(n), C_fix(WSTOPSIG(C_uw)))
#else
# define C_WIFEXITED(n)      C_mk_bool(WIFEXITED(C_unfix(n)))
# define C_WIFSIGNALED(n)    C_mk_bool(WIFSIGNALED(C_unfix(n)))
# define C_WIFSTOPPED(n)     C_mk_bool(WIFSTOPPED(C_unfix(n)))
# define C_WEXITSTATUS(n)    C_fix(WEXITSTATUS(C_unfix(n)))
# define C_WTERMSIG(n)       C_fix(WTERMSIG(C_unfix(n)))
# define C_WSTOPSIG(n)       C_fix(WSTOPSIG(C_unfix(n)))
#endif

#ifdef __CYGWIN__
# define C_mkfifo(fn, m)    C_fix(-1);
#else
# define C_mkfifo(fn, m)    C_fix(mkfifo((char *)C_data_pointer(fn), C_unfix(m)))
#endif

#define C_flock_setup(t, s, n) (C_flock.l_type = C_unfix(t), C_flock.l_start = C_num_to_int(s), C_flock.l_whence = SEEK_SET, C_flock.l_len = C_num_to_int(n), C_SCHEME_UNDEFINED)
#define C_flock_test(p)     (fcntl(fileno(C_port_file(p)), F_GETLK, &C_flock) >= 0 ? (C_flock.l_type == F_UNLCK ? C_fix(0) : C_fix(C_flock.l_pid)) : C_SCHEME_FALSE)
#define C_flock_lock(p)     C_fix(fcntl(fileno(C_port_file(p)), F_SETLK, &C_flock))
#define C_flock_lockw(p)    C_fix(fcntl(fileno(C_port_file(p)), F_SETLKW, &C_flock))

#ifndef FILENAME_MAX
# define FILENAME_MAX          1024
#endif

static C_TLS sigset_t C_sigset;
#define C_sigemptyset(d)    (sigemptyset(&C_sigset), C_SCHEME_UNDEFINED)
#define C_sigaddset(s)      (sigaddset(&C_sigset, C_unfix(s)), C_SCHEME_UNDEFINED)
#define C_sigdelset(s)      (sigdelset(&C_sigset, C_unfix(s)), C_SCHEME_UNDEFINED)
#define C_sigismember(s)    C_mk_bool(sigismember(&C_sigset, C_unfix(s)))
#define C_sigprocmask_set(d)        C_fix(sigprocmask(SIG_SETMASK, &C_sigset, NULL))
#define C_sigprocmask_block(d)      C_fix(sigprocmask(SIG_BLOCK, &C_sigset, NULL))
#define C_sigprocmask_unblock(d)    C_fix(sigprocmask(SIG_UNBLOCK, &C_sigset, NULL))

#define C_open(fn, fl, m)   C_fix(open(C_c_string(fn), C_unfix(fl), C_unfix(m)))
#define C_read(fd, b, n)    C_fix(read(C_unfix(fd), C_data_pointer(b), C_unfix(n)))
#define C_write(fd, b, n)   C_fix(write(C_unfix(fd), C_data_pointer(b), C_unfix(n)))
#define C_mkstemp(t)        C_fix(mkstemp(C_c_string(t)))

#define C_ftell(p)            C_fix(ftell(C_port_file(p)))
#define C_fseek(p, n, w)      C_mk_nbool(fseek(C_port_file(p), C_unfix(n), C_unfix(w)))
#define C_lseek(fd, o, w)     C_fix(lseek(C_unfix(fd), C_unfix(o), C_unfix(w)))

#define C_zero_fd_set(i)      FD_ZERO(&C_fd_sets[ i ])
#define C_set_fd_set(i, fd)   FD_SET(fd, &C_fd_sets[ i ])
#define C_test_fd_set(i, fd)  FD_ISSET(fd, &C_fd_sets[ i ])
#define C_C_select(m)         C_fix(select(C_unfix(m), &C_fd_sets[ 0 ], &C_fd_sets[ 1 ], NULL, NULL))
#define C_C_select_t(m, t)    (C_timeval.tv_sec = C_unfix(t), C_timeval.tv_usec = 0, C_fix(select(C_unfix(m), &C_fd_sets[ 0 ], &C_fd_sets[ 1 ], NULL, &C_timeval)))

#define C_ctime(n)          (C_secs = (n), ctime(&C_secs))

#if defined(__CYGWIN__) || defined(__SVR4)
/* Seen here: http://lists.samba.org/archive/samba-technical/2002-November/025571.html */

time_t timegm(struct tm *t)
{
  time_t tl, tb;
  struct tm *tg;

  tl = mktime (t);
  if (tl == -1)
    {
      t->tm_hour--;
      tl = mktime (t);
      if (tl == -1)
        return -1; /* can't deal with output from strptime */
      tl += 3600;
    }
  tg = gmtime (&tl);
  tg->tm_isdst = 0;
  tb = mktime (tg);
  if (tb == -1)
    {
      tg->tm_hour--;
      tb = mktime (tg);
      if (tb == -1)
        return -1; /* can't deal with output from gmtime */
      tb += 3600;
    }
  return (tl - (tb - tl));
}
#endif

#if !defined(C_GNU_ENV) || defined(__CYGWIN__) || defined(__uClinux__)
# define C_asctime(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), asctime(&C_tm) )
# define C_mktime(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), (C_temporary_flonum = mktime(&C_tm)) != -1)
# define C_timegm(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), (C_temporary_flonum = timegm(&C_tm)) != -1)
#else
# define C_asctime(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), C_tm.tm_gmtoff = C_unfix(C_block_item(v, 9)), asctime(&C_tm) )
# define C_mktime(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), C_tm.tm_gmtoff = C_unfix(C_block_item(v, 9)), (C_temporary_flonum = mktime(&C_tm)) != -1)
# define C_timegm(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), C_tm.tm_gmtoff = C_unfix(C_block_item(v, 9)), (C_temporary_flonum = timegm(&C_tm)) != -1)
#endif

#ifndef ECOS
static gid_t *C_groups = NULL;

#define C_get_gid(n)      C_fix(C_groups[ C_unfix(n) ])
#define C_set_gid(n, id)  (C_groups[ C_unfix(n) ] = C_unfix(id), C_SCHEME_UNDEFINED)
#define C_set_groups(n)   C_fix(setgroups(C_unfix(n), C_groups))
#endif
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#thread-yield! ##sys#make-string
     ##sys#make-port ##sys#file-info ##sys#update-errno ##sys#fudge ##sys#make-c-string ##sys#check-port
     ##sys#error ##sys#signal-hook ##sys#peek-unsigned-integer make-pathname glob directory?
     pathname-file string-match process-fork file-close duplicate-fileno process-execute getenv
     make-string make-input-port make-output-port ##sys#thread-block-for-i/o create-pipe
     process-wait pathname-strip-directory ##sys#expand-home-path glob->regexp directory
     decompose-pathname ##sys#cons-flonum ##sys#decode-seconds ##sys#null-pointer ##sys#pointer->address
     ##sys#substring ##sys#context-switch close-input-pipe close-output-pipe change-directory
     current-directory ##sys#make-pointer port? ##sys#schedule ##sys#process
     ##sys#peek-fixnum ##sys#make-structure ##sys#check-structure ##sys#enable-interrupts) ) ] )

(cond-expand
 [unsafe
  (eval-when (compile)
    (define-macro (##sys#check-structure . _) '(##core#undefined))
    (define-macro (##sys#check-range . _) '(##core#undefined))
    (define-macro (##sys#check-pair . _) '(##core#undefined))
    (define-macro (##sys#check-list . _) '(##core#undefined))
    (define-macro (##sys#check-symbol . _) '(##core#undefined))
    (define-macro (##sys#check-string . _) '(##core#undefined))
    (define-macro (##sys#check-char . _) '(##core#undefined))
    (define-macro (##sys#check-exact . _) '(##core#undefined))
    (define-macro (##sys#check-port . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "posix.exports")) ] )

(register-feature! 'posix)

(define posix-error
  (let ([strerror (foreign-lambda c-string "strerror" int)]
        [string-append string-append] )
    (lambda (type loc msg . args)
      (let ([rn (##sys#update-errno)])
        (apply ##sys#signal-hook type loc (string-append msg " - " (strerror rn)) args) ) ) ) )

;; Faster versions of common operations

(define ##sys#posix-error posix-error)

(define ##sys#file-nonblocking!
  (foreign-lambda* bool ([int fd])
    "int val = fcntl(fd, F_GETFL, 0);"
    "if(val == -1) return(0);"
    "return(fcntl(fd, F_SETFL, val | O_NONBLOCK) != -1);" ) )

(define ##sys#file-select-one
  (foreign-lambda* int ([int fd])
    "fd_set in;"
    "struct timeval tm;"
    "FD_ZERO(&in);"
    "FD_SET(fd, &in);"
    "tm.tv_sec = tm.tv_usec = 0;"
    "if(select(fd + 1, &in, NULL, NULL, &tm) == -1) return(-1);"
    "else return(FD_ISSET(fd, &in) ? 1 : 0);" ) )


;;; Lo-level I/O:

(define-foreign-variable _pipe_buf int "PIPE_BUF")

(define pipe/buf _pipe_buf)

(define-foreign-variable _f_dupfd int "F_DUPFD")
(define-foreign-variable _f_getfd int "F_GETFD")
(define-foreign-variable _f_setfd int "F_SETFD")
(define-foreign-variable _f_getfl int "F_GETFL")
(define-foreign-variable _f_setfl int "F_SETFL")

(define fcntl/dupfd _f_dupfd)
(define fcntl/getfd _f_getfd)
(define fcntl/setfd _f_setfd)
(define fcntl/getfl _f_getfl)
(define fcntl/setfl _f_setfl)

(define-foreign-variable _o_rdonly int "O_RDONLY")
(define-foreign-variable _o_wronly int "O_WRONLY")
(define-foreign-variable _o_rdwr int "O_RDWR")
(define-foreign-variable _o_creat int "O_CREAT")
(define-foreign-variable _o_append int "O_APPEND")
(define-foreign-variable _o_excl int "O_EXCL")
(define-foreign-variable _o_noctty int "O_NOCTTY")
(define-foreign-variable _o_nonblock int "O_NONBLOCK")
(define-foreign-variable _o_trunc int "O_TRUNC")
(define-foreign-variable _o_fsync int "O_FSYNC")
(define-foreign-variable _o_binary int "O_BINARY")
(define-foreign-variable _o_text int "O_TEXT")

(define open/rdonly _o_rdonly)
(define open/wronly _o_wronly)
(define open/rdwr _o_rdwr)
(define open/read _o_rdonly)
(define open/write _o_wronly)
(define open/creat _o_creat)
(define open/append _o_append)
(define open/excl _o_excl)
(define open/noctty _o_noctty)
(define open/nonblock _o_nonblock)
(define open/trunc _o_trunc)
(define open/sync _o_fsync)
(define open/fsync _o_fsync)
(define open/binary _o_binary)
(define open/text _o_text)

(define-foreign-variable _s_irusr int "S_IRUSR")
(define-foreign-variable _s_iwusr int "S_IWUSR")
(define-foreign-variable _s_ixusr int "S_IXUSR")
(define-foreign-variable _s_irgrp int "S_IRGRP")
(define-foreign-variable _s_iwgrp int "S_IWGRP")
(define-foreign-variable _s_ixgrp int "S_IXGRP")
(define-foreign-variable _s_iroth int "S_IROTH")
(define-foreign-variable _s_iwoth int "S_IWOTH")
(define-foreign-variable _s_ixoth int "S_IXOTH")
(define-foreign-variable _s_irwxu int "S_IRWXU")
(define-foreign-variable _s_irwxg int "S_IRWXG")
(define-foreign-variable _s_irwxo int "S_IRWXO")
(define-foreign-variable _s_isuid int "S_ISUID")
(define-foreign-variable _s_isgid int "S_ISGID")
(cond-expand
 [ecos]
 [else
  (define-foreign-variable _s_isvtx int "S_ISVTX")])

(define perm/irusr _s_irusr)
(define perm/iwusr _s_iwusr)
(define perm/ixusr _s_ixusr)
(define perm/irgrp _s_irgrp)
(define perm/iwgrp _s_iwgrp)
(define perm/ixgrp _s_ixgrp)
(define perm/iroth _s_iroth)
(define perm/iwoth _s_iwoth)
(define perm/ixoth _s_ixoth)
(define perm/irwxu _s_irwxu)
(define perm/irwxg _s_irwxg)
(define perm/irwxo _s_irwxo)
(define perm/isvtx _s_isvtx)
(define perm/isuid _s_isuid)
(define perm/isgid _s_isgid)

(define file-control
  (let ([fcntl (foreign-lambda int fcntl int int long)])
    (lambda (fd cmd #!optional (arg 0))
      (##sys#check-exact fd 'file-control)
      (##sys#check-exact cmd 'file-control)
      (let ([res (fcntl fd cmd arg)])
        (if (fx= res -1)
          (posix-error #:file-error 'file-control "cannot control file" fd cmd)
          res ) ) ) ) )

(define file-open
  (let ([defmode (bitwise-ior _s_irwxu (bitwise-ior _s_irgrp _s_iroth))] )
    (lambda (filename flags . mode)
      (let ([mode (if (pair? mode) (car mode) defmode)])
        (##sys#check-string filename 'file-open)
        (##sys#check-exact flags 'file-open)
        (##sys#check-exact mode 'file-open)
        (let ([fd (##core#inline "C_open" (##sys#make-c-string (##sys#expand-home-path filename)) flags mode)])
          (when (eq? -1 fd)
            (posix-error #:file-error 'file-open "cannot open file" filename flags mode) )
          fd) ) ) ) )

(define file-close
  (lambda (fd)
    (##sys#check-exact fd 'file-close)
    (when (fx< (##core#inline "C_close" fd) 0)
      (posix-error #:file-error 'file-close "cannot close file" fd) ) ) )

(define file-read
  (let ([make-string make-string] )
    (lambda (fd size . buffer)
      (##sys#check-exact fd 'file-read)
      (##sys#check-exact size 'file-read)
      (let ([buf (if (pair? buffer) (car buffer) (make-string size))])
        (unless (and (##core#inline "C_blockp" buf) (##core#inline "C_byteblockp" buf))
          (##sys#signal-hook #:type-error 'file-read "bad argument type - not a string or byte-vector" buf) )
        (let ([n (##core#inline "C_read" fd buf size)])
          (when (eq? -1 n)
            (posix-error #:file-error 'file-read "cannot read from file" fd size) )
          (list buf n) ) ) ) ) )

(define file-write
  (lambda (fd buffer . size)
    (##sys#check-exact fd 'file-write)
    (unless (and (##core#inline "C_blockp" buffer) (##core#inline "C_byteblockp" buffer))
      (##sys#signal-hook #:type-error 'file-write "bad argument type - not a string or byte-vector" buffer) )
    (let ([size (if (pair? size) (car size) (##sys#size buffer))])
      (##sys#check-exact size 'file-write)
      (let ([n (##core#inline "C_write" fd buffer size)])
        (when (eq? -1 n)
          (posix-error #:file-error 'file-write "cannot write to file" fd size) )
        n) ) ) )

(cond-expand
 [ecos]
 [else
  (define file-mkstemp
    (lambda (template)
      (##sys#check-string template 'file-mkstemp)
      (let* ([buf (##sys#make-c-string template)]
             [fd (##core#inline "C_mkstemp" buf)]
             [path-length (##sys#size buf)])
        (when (eq? -1 fd)
          (posix-error #:file-error 'file-mkstemp "cannot create temporary file" template) )
        (values fd (##sys#substring buf 0 (fx- path-length 1) ) ) ) ) ) ] )


;;; I/O multiplexing:

(define file-select
  (let ([fd_zero (foreign-lambda void "C_zero_fd_set" int)]
        [fd_set (foreign-lambda void "C_set_fd_set" int int)]
        [fd_test (foreign-lambda bool "C_test_fd_set" int int)] )
    (lambda (fdsr fdsw . timeout)
      (let ([fdmax 0]
            [tm (if (pair? timeout) (car timeout) #f)] )
        (fd_zero 0)
        (fd_zero 1)
        (cond [(not fdsr)]
              [(fixnum? fdsr)
               (set! fdmax fdsr)
               (fd_set 0 fdsr) ]
              [else
               (##sys#check-list fdsr 'file-select)
               (for-each
                (lambda (fd)
                  (##sys#check-exact fd 'file-select)
                  (set! fdmax (##core#inline "C_i_fixnum_max" fdmax fd))
                  (fd_set 0 fd) )
                fdsr) ] )
        (cond [(not fdsw)]
              [(fixnum? fdsw)
               (set! fdmax fdsw)
               (fd_set 1 fdsw) ]
              [else
               (##sys#check-list fdsw 'file-select)
               (for-each
                (lambda (fd)
                  (##sys#check-exact fd 'file-select)
                  (set! fdmax (##core#inline "C_i_fixnum_max" fdmax fd))
                  (fd_set 1 fd) )
                fdsw) ] )
        (let ([n (cond [tm
                        (##sys#check-exact tm 'file-select)
                        (##core#inline "C_C_select_t" (fx+ fdmax 1) tm) ]
                       [else (##core#inline "C_C_select" (fx+ fdmax 1))] ) ] )
          (cond [(fx< n 0)
                 (posix-error #:file-error 'file-select "failed" fdsr fdsw) ]
                [(fx= n 0) (values (if (pair? fdsr) '() #f) (if (pair? fdsw) '() #f))]
                [else
                 (values
                  (and fdsr
                       (if (fixnum? fdsr)
                           (fd_test 0 fdsr)
                           (let ([lstr '()])
                             (for-each (lambda (fd) (when (fd_test 0 fd) (set! lstr (cons fd lstr)))) fdsr)
                             lstr) ) )
                  (and fdsw
                       (if (fixnum? fdsw)
                           (fd_test 1 fdsw)
                           (let ([lstw '()])
                             (for-each (lambda (fd) (when (fd_test 1 fd) (set! lstw (cons fd lstw)))) fdsw)
                             lstw) ) ) ) ] ) ) ) ) ) )


;;; File attribute access:

(define-foreign-variable _seek_set int "SEEK_SET")
(define-foreign-variable _seek_cur int "SEEK_CUR")
(define-foreign-variable _seek_end int "SEEK_END")

(define seek/set _seek_set)
(define seek/end _seek_end)
(define seek/cur _seek_cur)

(define-foreign-variable _stat_st_ino unsigned-int "C_statbuf.st_ino")
(define-foreign-variable _stat_st_nlink unsigned-int "C_statbuf.st_nlink")
(define-foreign-variable _stat_st_gid unsigned-int "C_statbuf.st_gid")
(define-foreign-variable _stat_st_size unsigned-int "C_statbuf.st_size")
(define-foreign-variable _stat_st_mtime double "C_statbuf.st_mtime")
(define-foreign-variable _stat_st_atime double "C_statbuf.st_atime")
(define-foreign-variable _stat_st_ctime double "C_statbuf.st_ctime")
(define-foreign-variable _stat_st_uid unsigned-int "C_statbuf.st_uid")
(define-foreign-variable _stat_st_mode unsigned-int "C_statbuf.st_mode")
(define-foreign-variable _stat_st_dev unsigned-int "C_statbuf.st_dev")
(define-foreign-variable _stat_st_rdev unsigned-int "C_statbuf.st_rdev")
(define-foreign-variable _stat_st_blksize unsigned-int "C_statbuf.st_blksize")
(define-foreign-variable _stat_st_blocks unsigned-int "C_statbuf.st_rdev")

(define (##sys#stat file link loc)
  (let ([r (cond [(fixnum? file) (##core#inline "C_fstat" file)]
                 [(string? file)
                  (let ([path (##sys#make-c-string (##sys#expand-home-path file))])
                    (cond-expand
                     [ecos
                      (##core#inline "C_stat" path)]
                     [else
                      (if link
                          (##core#inline "C_lstat" path)
                          (##core#inline "C_stat" path) ) ] )  ) ]
                 [else (##sys#signal-hook #:type-error "bad argument type - not a fixnum or string" file)] ) ] )
    (when (fx< r 0)
      (posix-error #:file-error loc "cannot access file" file) ) ) )

(define (file-stat f . link)
  (##sys#stat f (:optional link #f) 'file-stat)
  (vector _stat_st_ino _stat_st_mode _stat_st_nlink
          _stat_st_uid _stat_st_gid _stat_st_size
          _stat_st_atime _stat_st_ctime _stat_st_mtime
          _stat_st_dev _stat_st_rdev
          _stat_st_blksize _stat_st_blocks) )

(define (file-size f) (##sys#stat f #f 'file-size) _stat_st_size)
(define (file-modification-time f) (##sys#stat f #f 'file-modification-time) _stat_st_mtime)
(define (file-access-time f) (##sys#stat f #f 'file-access-time) _stat_st_atime)
(define (file-change-time f) (##sys#stat f #f 'file-change-time) _stat_st_ctime)
(define (file-owner f) (##sys#stat f #f 'file-owner) _stat_st_uid)
(define (file-permissions f) (##sys#stat f #f 'file-permissions) _stat_st_mode)

(define (regular-file? fname)
  (##sys#check-string fname 'regular-file?)
  (##sys#stat fname #t 'regular-file?)
  (foreign-value "C_isreg" bool) )

(define (symbolic-link? fname)
  (##sys#check-string fname 'symbolic-link?)
  (##sys#stat fname #t 'symbolic-link?)
  (foreign-value "C_islink" bool) )

(define set-file-position!              ; DEPRECATED
  (lambda (port pos . whence)
    (let ([whence (if (pair? whence) (car whence) _seek_set)])
      (##sys#check-exact pos 'set-file-position!)
      (##sys#check-exact whence 'set-file-position!)
      (when (fx< pos 0) (##sys#signal-hook #:bounds-error 'set-file-position! "invalid negative port position" pos port))
      (unless (cond [(port? port)
                     (and (eq? (##sys#slot port 7) 'stream)
                          (##core#inline "C_fseek" port pos whence) ) ]
                    [(fixnum? port) (##core#inline "C_lseek" port pos whence)]
                    [else (##sys#signal-hook #:type-error 'set-file-position! "invalid file" port)] )
        (posix-error #:file-error 'set-file-position! "cannot set file position" port pos) ) ) ) )

(define file-position
  (getter-with-setter
   (lambda (port)
    (let ([pos (cond [(port? port)
                      (if (eq? (##sys#slot port 7) 'stream)
                          (##core#inline "C_ftell" port)
                          -1) ]
                     [(fixnum? port) (##core#inline "C_lseek" port 0 _seek_cur)]
                     [else (##sys#signal-hook #:type-error 'file-position "invalid file" port)] ) ] )
      (when (fx< pos 0)
        (posix-error #:file-error 'file-position "cannot retrieve file position of port" port) )
      pos) )
   set-file-position!) )


;;; Directory stuff:

(define create-directory
  (lambda (name)
    (##sys#check-string name 'create-directory)
    (unless (zero? (##core#inline "C_mkdir" (##sys#make-c-string (##sys#expand-home-path name))))
      (posix-error #:file-error 'create-directory "cannot create directory" name) ) ) )

(define change-directory
  (lambda (name)
    (##sys#check-string name 'change-directory)
    (unless (zero? (##core#inline "C_chdir" (##sys#make-c-string (##sys#expand-home-path name))))
      (posix-error #:file-error 'change-directory "cannot change current directory" name) ) ) )

(define delete-directory
  (lambda (name)
    (##sys#check-string name 'delete-directory)
    (unless (zero? (##core#inline "C_rmdir" (##sys#make-c-string (##sys#expand-home-path name))))
      (posix-error #:file-error 'delete-directory "cannot delete directory" name) ) ) )

(define directory
  (let ([string-append string-append]
        [make-string make-string]
        [string string] )
    (lambda (#!optional (spec (current-directory)) show-dotfiles?)
      (##sys#check-string spec 'directory)
      (let ([buffer (make-string 256)]
            [handle (##sys#make-pointer)]
            [entry (##sys#make-pointer)] )
        (##core#inline "C_opendir" (##sys#make-c-string (##sys#expand-home-path spec)) handle)
        (if (##sys#null-pointer? handle)
            (posix-error #:file-error 'directory "cannot open directory" spec)
            (let loop ()
              (##core#inline "C_readdir" handle entry)
              (if (##sys#null-pointer? entry)
                  (begin
                    (##core#inline "C_closedir" handle)
                    '() )
                  (let* ([flen (##core#inline "C_foundfile" entry buffer)]
                         [file (##sys#substring buffer 0 flen)]
                         [char1 (string-ref file 0)]
                         [char2 (and (fx> flen 1) (string-ref file 1))] )
                    (if (and (eq? char1 #\.)
                             (or (not char2)
                                 (and (eq? char2 #\.) (eq? flen 2))
                                 (not show-dotfiles?) ) )
                        (loop)
                        (cons file (loop)) ) ) ) ) ) ) ) ) )

(define (directory? fname)
  (##sys#check-string fname 'directory?)
  (let ((info (##sys#file-info (##sys#expand-home-path fname))))
    (and info (fx= 1 (##sys#slot info 4))) ) )

(define current-directory
  (let ([make-string make-string])
    (lambda (#!optional dir)
      (if dir
          (change-directory dir)
          (let* ([buffer (make-string 256)]
                 [len (##core#inline "C_curdir" buffer)] )
            (if len
                (##sys#substring buffer 0 len)
                (posix-error #:file-error 'current-directory "cannot retrieve current directory") ) ) ) ) ) )


;;; Pipes:

(cond-expand
 [ecos]
 [else

  (let ()
    (define (mode arg) (if (pair? arg) (##sys#slot arg 0) '###text))
    (define (badmode m) (##sys#error "illegal input/output mode specifier" m))
    (define (check loc cmd inp r)
      (if (##sys#null-pointer? r)
          (posix-error #:file-error loc "cannot open pipe" cmd)
          (let ([port (##sys#make-port inp ##sys#stream-port-class "(pipe)" 'stream)])
            (##core#inline "C_set_file_ptr" port r)
            port) ) )
    (set! open-input-pipe
      (lambda (cmd . m)
        (##sys#check-string cmd 'open-input-pipe)
        (let ([m (mode m)])
          (check
           'open-input-pipe
           cmd #t
           (case m
             ((###text) (##core#inline_allocate ("open_text_input_pipe" 2) (##sys#make-c-string cmd)))
             ((###binary) (##core#inline_allocate ("open_binary_input_pipe" 2) (##sys#make-c-string cmd)))
             (else (badmode m)) ) ) ) ) )
    (set! open-output-pipe
      (lambda (cmd . m)
        (##sys#check-string cmd 'open-output-pipe)
        (let ((m (mode m)))
          (check
           'open-output-pipe
           cmd #f
           (case m
             ((###text) (##core#inline_allocate ("open_text_output_pipe" 2) (##sys#make-c-string cmd)))
             ((###binary) (##core#inline_allocate ("open_binary_output_pipe" 2) (##sys#make-c-string cmd)))
             (else (badmode m)) ) ) ) ) )
    (set! close-input-pipe
      (lambda (port)
        (##sys#check-port port 'close-input-pipe)
        (let ((r (##core#inline "close_pipe" port)))
          (when (eq? -1 r) (posix-error #:file-error 'close-input/output-pipe "error while closing pipe" port))
          r) ) )
    (set! close-output-pipe close-input-pipe) )

  (let ([open-input-pipe open-input-pipe]
        [open-output-pipe open-output-pipe]
        [close-input-pipe close-input-pipe]
        [close-output-pipe close-output-pipe] )
    (set! call-with-input-pipe
      (lambda (cmd proc . mode)
        (let ([p (apply open-input-pipe cmd mode)])
          (##sys#call-with-values
           (lambda () (proc p))
           (lambda results
             (close-input-pipe p)
             (apply values results) ) ) ) ) )
    (set! call-with-output-pipe
      (lambda (cmd proc . mode)
        (let ([p (apply open-output-pipe cmd mode)])
          (##sys#call-with-values
           (lambda () (proc p))
           (lambda results
             (close-output-pipe p)
             (apply values results) ) ) ) ) )
    (set! with-input-from-pipe
      (lambda (cmd thunk . mode)
        (let ([old ##sys#standard-input]
              [p (apply open-input-pipe cmd mode)] )
          (set! ##sys#standard-input p)
          (##sys#call-with-values thunk
                                  (lambda results
                                    (close-input-pipe p)
                                    (set! ##sys#standard-input old)
                                    (apply values results) ) ) ) ) )
    (set! with-output-to-pipe
      (lambda (cmd thunk . mode)
        (let ([old ##sys#standard-output]
              [p (apply open-output-pipe cmd mode)] )
          (set! ##sys#standard-output p)
          (##sys#call-with-values thunk
                                  (lambda results
                                    (close-output-pipe p)
                                    (set! ##sys#standard-output old)
                                    (apply values results) ) ) ) ) ) )

  (define-foreign-variable _pipefd0 int "C_pipefds[ 0 ]")
  (define-foreign-variable _pipefd1 int "C_pipefds[ 1 ]")

  (define create-pipe
    (lambda ()
      (when (fx< (##core#inline "C_pipe" #f) 0)
        (posix-error #:file-error 'create-pipe "cannot create pipe") )
      (values _pipefd0 _pipefd1) ) ) ] )


;;; Signal processing:

(define-foreign-variable _nsig int "NSIG")
(define-foreign-variable _sigterm int "SIGTERM")
(define-foreign-variable _sigkill int "SIGKILL")
(define-foreign-variable _sigint int "SIGINT")
(define-foreign-variable _sighup int "SIGHUP")
(define-foreign-variable _sigfpe int "SIGFPE")
(define-foreign-variable _sigill int "SIGILL")
(define-foreign-variable _sigsegv int "SIGSEGV")
(define-foreign-variable _sigabrt int "SIGABRT")
(define-foreign-variable _sigtrap int "SIGTRAP")
(define-foreign-variable _sigquit int "SIGQUIT")
(define-foreign-variable _sigalrm int "SIGALRM")
(define-foreign-variable _sigpipe int "SIGPIPE")
(define-foreign-variable _sigusr1 int "SIGUSR1")
(define-foreign-variable _sigusr2 int "SIGUSR2")
(cond-expand
 [ecos]
 [else
  (define-foreign-variable _sigvtalrm int "SIGVTALRM")
  (define-foreign-variable _sigprof int "SIGPROF")
  (define-foreign-variable _sigio int "SIGIO")
  (define-foreign-variable _sigurg int "SIGURG")
  (define-foreign-variable _sigchld int "SIGCHLD")
  (define-foreign-variable _sigcont int "SIGCONT")
  (define-foreign-variable _sigstop int "SIGSTOP")
  (define-foreign-variable _sigtstp int "SIGTSTP")
  (define-foreign-variable _sigxcpu int "SIGXCPU")
  (define-foreign-variable _sigxfsz int "SIGXFSZ")
  (define-foreign-variable _sigwinch int "SIGWINCH") ] )

(define signal/term _sigterm)
(define signal/kill _sigkill)
(define signal/int _sigint)
(define signal/hup _sighup)
(define signal/fpe _sigfpe)
(define signal/ill _sigill)
(define signal/segv _sigsegv)
(define signal/abrt _sigabrt)
(define signal/trap _sigtrap)
(define signal/quit _sigquit)
(define signal/alrm _sigalrm)
(define signal/vtalrm _sigvtalrm)
(define signal/prof _sigprof)
(define signal/io _sigio)
(define signal/urg _sigurg)
(define signal/chld _sigchld)
(define signal/cont _sigcont)
(define signal/stop _sigstop)
(define signal/tstp _sigtstp)
(define signal/pipe _sigpipe)
(define signal/xcpu _sigxcpu)
(define signal/xfsz _sigxfsz)
(define signal/usr1 _sigusr1)
(define signal/usr2 _sigusr2)
(define signal/winch _sigwinch)

(define signals-list
  (list
    signal/term signal/kill signal/int signal/hup signal/fpe signal/ill
    signal/segv signal/abrt signal/trap signal/quit signal/alrm signal/vtalrm
    signal/prof signal/io signal/urg signal/chld signal/cont signal/stop
    signal/tstp signal/pipe signal/xcpu signal/xfsz signal/usr1 signal/usr2
    signal/winch))

(let ([oldhook ##sys#interrupt-hook]
      [sigvector (make-vector 256 #f)] )
  (set! signal-handler
    (lambda (sig)
      (##sys#check-exact sig 'signal-handler)
      (##sys#slot sigvector sig) ) )
  (set! set-signal-handler!
    (lambda (sig proc)
      (##sys#check-exact sig 'set-signal-handler!)
      (##core#inline "C_establish_signal_handler" sig (and proc sig))
      (vector-set! sigvector sig proc) ) )
  (set! ##sys#interrupt-hook
    (lambda (reason state)
      (let ([h (##sys#slot sigvector reason)])
        (if h
            (begin
              (h reason)
              (##sys#context-switch state) )
            (oldhook reason state) ) ) ) ) )

(define set-signal-mask!
  (lambda (sigs)
    (##sys#check-list sigs 'set-signal-mask!)
    (##core#inline "C_sigemptyset" 0)
    (for-each
      (lambda (s)
        (##sys#check-exact s 'set-signal-mask!)
        (##core#inline "C_sigaddset" s) )
      sigs)
    (when (fx< (##core#inline "C_sigprocmask_set" 0) 0)
      (posix-error #:process-error 'set-signal-mask! "cannot set signal mask") ) ) )

(define (signal-mask)
  (let loop ([sigs signals-list] [mask '()])
    (if (null? sigs)
        mask
        (let ([sig (car sigs)])
          (loop (cdr sigs) (if (##core#inline "C_sigismember" sig) (cons sig mask) mask)) ) ) ) )

(define (signal-masked? sig)
  (##sys#check-exact sig 'signal-masked?)
  (##core#inline "C_sigismember" sig) )

(define (signal-mask! sig)
  (##sys#check-exact sig 'signal-mask!)
  (##core#inline "C_sigaddset" sig)
  (when (fx< (##core#inline "C_sigprocmask_block" 0) 0)
      (posix-error #:process-error 'signal-mask! "cannot block signal") )  )

(define (signal-unmask! sig)
  (##sys#check-exact sig 'signal-unmask!)
  (##core#inline "C_sigdelset" sig)
  (when (fx< (##core#inline "C_sigprocmask_unblock" 0) 0)
      (posix-error #:process-error 'signal-unmask! "cannot unblock signal") )  )

;;; Set SIGINT handler:

(set-signal-handler!
 signal/int
 (lambda (n) (##sys#user-interrupt-hook)) )


;;; Getting system-, group- and user-information:

(cond-expand
 [ecos]
 [else

  (define-foreign-variable _uname int "C_uname")
  (define-foreign-variable _uname-sysname nonnull-c-string "C_utsname.sysname")
  (define-foreign-variable _uname-nodename nonnull-c-string "C_utsname.nodename")
  (define-foreign-variable _uname-release nonnull-c-string "C_utsname.release")
  (define-foreign-variable _uname-version nonnull-c-string "C_utsname.version")
  (define-foreign-variable _uname-machine nonnull-c-string "C_utsname.machine")

  (define system-information
    (lambda ()
      (when (fx< _uname 0)
        (##sys#update-errno)
        (##sys#error 'system-information "cannot retrieve system information") )
      (list _uname-sysname
            _uname-nodename
            _uname-release
            _uname-version
            _uname-machine) ) )

  (define-foreign-variable _user-name nonnull-c-string "C_user->pw_name")
  (define-foreign-variable _user-passwd nonnull-c-string "C_user->pw_passwd")
  (define-foreign-variable _user-uid int "C_user->pw_uid")
  (define-foreign-variable _user-gid int "C_user->pw_gid")
  (define-foreign-variable _user-gecos nonnull-c-string "C_user->pw_gecos")
  (define-foreign-variable _user-dir c-string "C_user->pw_dir")
  (define-foreign-variable _user-shell c-string "C_user->pw_shell")

  (define (user-information user)
    (let ([r (if (fixnum? user)
               (##core#inline "C_getpwuid" user)
               (begin
                 (##sys#check-string user 'user-information)
                 (##core#inline "C_getpwnam" (##sys#make-c-string user)) ) ) ] )
      (and r
         (list _user-name
               _user-passwd
               _user-uid
               _user-gid
               _user-gecos
               _user-dir
               _user-shell) ) ) )

  (define-foreign-variable _group-name nonnull-c-string "C_group->gr_name")
  (define-foreign-variable _group-passwd nonnull-c-string "C_group->gr_passwd")
  (define-foreign-variable _group-gid int "C_group->gr_gid")

  (define group-member
    (foreign-lambda* c-string ([int i])
      "return(C_group->gr_mem[ i ]);") )

  (define (group-information group)
    (let ([r (if (fixnum? group)
               (##core#inline "C_getgrgid" group)
               (begin
                 (##sys#check-string group 'group-information)
                 (##core#inline "C_getgrnam" (##sys#make-c-string group)) ) ) ] )
      (and r
         (list _group-name
                 _group-passwd
                 _group-gid
                 (let rec ([i 0])
                   (let ([n (group-member i)])
                     (if n
                         (cons n (rec (fx+ i 1)))
                         '() ) ) ) ) ) ) )

  (define _get-groups
    (foreign-lambda* int ([int n])
      "return(getgroups(n, C_groups));") )

  (define _ensure-groups
    (foreign-lambda* bool ([int n])
      "if(C_groups != NULL) C_free(C_groups);"
      "C_groups = (gid_t *)C_malloc(sizeof(gid_t) * n);"
      "if(C_groups == NULL) return(0);"
      "else return(1);") )

  (define (get-groups)
    (let ([n (foreign-value "getgroups(0, C_groups)" int)])
      (when (fx< n 0)
        (##sys#update-errno)
        (##sys#error 'get-groups "cannot retrieve supplementary group ids") )
      (unless (_ensure-groups n)
        (##sys#error 'get-groups "out of memory") )
      (when (fx< (_get-groups n) 0)
        (##sys#update-errno)
        (##sys#error 'get-groups "cannot retrieve supplementary group ids") )
      (let loop ([i 0])
        (if (fx>= i n)
          '()
          (cons (##core#inline "C_get_gid" i) (loop (fx+ i 1))) ) ) ) )

  (define (set-groups! lst0)
    (unless (_ensure-groups (length lst0))
      (##sys#error 'set-groups! "out of memory") )
    (do ([lst lst0 (##sys#slot lst 1)]
         [i 0 (fx+ i 1)] )
        ((null? lst)
         (when (fx< (##core#inline "C_set_groups" i) 0)
         (##sys#update-errno)
         (##sys#error 'set-groups! "cannot set supplementary group ids" lst0) ) )
      (let ([n (##sys#slot lst 0)])
        (##sys#check-exact n 'set-groups!)
        (##core#inline "C_set_gid" i n) ) ) )

  (define initialize-groups
    (let ([init (foreign-lambda int "initgroups" c-string int)])
      (lambda (user id)
        (##sys#check-string user 'initialize-groups)
        (##sys#check-exact id 'initialize-groups)
        (when (fx< (init user id) 0)
        (##sys#update-errno)
        (##sys#error 'initialize-groups "cannot initialize supplementary group ids" user id) ) ) ) ) ] )


;;; More errno codes:

(define-foreign-variable _errno int "errno")

(define-foreign-variable _eperm int "EPERM")
(define-foreign-variable _enoent int "ENOENT")
(define-foreign-variable _esrch int "ESRCH")
(define-foreign-variable _eintr int "EINTR")
(define-foreign-variable _eio int "EIO")
(cond-expand
 [ecos]
 [else
  (define-foreign-variable _efault int "EFAULT")
  (define-foreign-variable _echild int "ECHILD")
  (define-foreign-variable _enoexec int "ENOEXEC")])
(define-foreign-variable _ebadf int "EBADF")
(define-foreign-variable _enomem int "ENOMEM")
(define-foreign-variable _eacces int "EACCES")
(define-foreign-variable _ebusy int "EBUSY")
(define-foreign-variable _eexist int "EEXIST")
(define-foreign-variable _enotdir int "ENOTDIR")
(define-foreign-variable _eisdir int "EISDIR")
(define-foreign-variable _einval int "EINVAL")
(define-foreign-variable _emfile int "EMFILE")
(define-foreign-variable _enospc int "ENOSPC")
(define-foreign-variable _espipe int "ESPIPE")
(define-foreign-variable _epipe int "EPIPE")
(define-foreign-variable _eagain int "EAGAIN")
(define-foreign-variable _erofs int "EROFS")
(define-foreign-variable _ewouldblock int "EWOULDBLOCK")

(define errno/perm _eperm)
(define errno/noent _enoent)
(define errno/srch _esrch)
(define errno/intr _eintr)
(define errno/io _eio)
(define errno/noexec _enoexec)
(define errno/badf _ebadf)
(define errno/child _echild)
(define errno/nomem _enomem)
(define errno/acces _eacces)
(define errno/fault _efault)
(define errno/busy _ebusy)
(define errno/notdir _enotdir)
(define errno/isdir _eisdir)
(define errno/inval _einval)
(define errno/mfile _emfile)
(define errno/nospc _enospc)
(define errno/spipe _espipe)
(define errno/pipe _epipe)
(define errno/again _eagain)
(define errno/rofs _erofs)
(define errno/exist _eexist)
(define errno/wouldblock _ewouldblock)

(define errno/2big 0)
(define errno/deadlk 0)
(define errno/dom 0)
(define errno/fbig 0)
(define errno/ilseq 0)
(define errno/mlink 0)
(define errno/nametoolong 0)
(define errno/nfile 0)
(define errno/nodev 0)
(define errno/nolck 0)
(define errno/nosys 0)
(define errno/notempty 0)
(define errno/notty 0)
(define errno/nxio 0)
(define errno/range 0)
(define errno/xdev 0)

(cond-expand
 [ecos]
 [else

  ;;; Permissions and owners:

  (define change-file-mode
    (lambda (fname m)
      (##sys#check-string fname 'change-file-mode)
      (##sys#check-exact m 'change-file-mode)
      (when (fx< (##core#inline "C_chmod" (##sys#make-c-string (##sys#expand-home-path fname)) m) 0)
        (posix-error #:file-error 'change-file-mode "cannot change file mode" fname m) ) ) )

  (define change-file-owner
    (lambda (fn uid gid)
      (##sys#check-string fn 'change-file-owner)
      (##sys#check-exact uid 'change-file-owner)
      (##sys#check-exact gid 'change-file-owner)
      (when (fx< (##core#inline "C_chown" (##sys#make-c-string (##sys#expand-home-path fn)) uid gid) 0)
        (posix-error #:file-error 'change-file-owner "cannot change file owner" fn uid gid) ) ) )

  (define current-effective-user-id (foreign-lambda int "C_geteuid"))
  (define current-effective-group-id (foreign-lambda int "C_getegid"))

  (define set-user-id!                  ; DEPRECATED
    (lambda (id)
      (when (fx< (##core#inline "C_setuid" id) 0)
        (##sys#update-errno)
        (##sys#error 'set-user-id! "cannot set user ID" id) ) ) )

  (define current-user-id
    (getter-with-setter
     (foreign-lambda int "C_getuid")
     set-user-id!) )

  (define set-group-id!                 ; DEPRECATED
    (lambda (id)
      (when (fx< (##core#inline "C_setgid" id) 0)
        (##sys#update-errno)
        (##sys#error 'set-user-id! "cannot set group ID" id) ) ) )

  (define current-group-id
    (getter-with-setter
     (foreign-lambda int "C_getgid")
     set-group-id!) )

  (define-foreign-variable _r_ok int "R_OK")
  (define-foreign-variable _w_ok int "W_OK")
  (define-foreign-variable _x_ok int "X_OK")

  (let ()
    (define (check filename acc loc)
      (##sys#check-string filename loc)
      (let ([r (fx= 0 (##core#inline "C_access" (##sys#make-c-string (##sys#expand-home-path filename)) acc))])
        (unless r (##sys#update-errno))
        r) )
    (set! file-read-access? (lambda (filename) (check filename _r_ok 'file-read-access?)))
    (set! file-write-access? (lambda (filename) (check filename _w_ok 'file-write-access?)))
    (set! file-execute-access? (lambda (filename) (check filename _x_ok 'file-execute-access?))) )

  (define (create-session)
    (let ([a (##core#inline "C_setsid" #f)])
      (when (fx< a 0)
        (##sys#update-errno)
        (##sys#error 'create-session "cannot create session") )
      a) )

  (define (set-process-group-id! pid pgid) ; DEPRECATED
    (##sys#check-exact pid 'set-process-group-id!)
    (##sys#check-exact pgid 'set-process-group-id!)
    (when (fx< (##core#inline "C_setpgid" pid pgid) 0)
      (##sys#update-errno)
      (##sys#error 'set-process-group-id! "cannot set process group ID" pid pgid) ) )

  (define process-group-id
    (getter-with-setter
     (lambda (pid)
       (##sys#check-exact pid 'process-group-id)
       (let ([a (##core#inline "C_getpgid" pid)])
         (when (fx< a 0)
           (##sys#update-errno)
           (##sys#error 'process-group-id "cannot retrieve process group ID" pid) )
      a) )
     set-process-group-id!) )

  ;;; Hard and symbolic links:

  (define create-symbolic-link
    (lambda (old new)
      (##sys#check-string old 'create-symbolic-link)
      (##sys#check-string new 'create-symbolic-link)
      (when (fx< (##core#inline
                "C_symlink"
                (##sys#make-c-string (##sys#expand-home-path old))
                (##sys#make-c-string (##sys#expand-home-path new)) )
               0)
        (posix-error #:file-error 'create-symbol-link "cannot create symbolic link" old new) ) ) )

  (define-foreign-variable _filename_max int "FILENAME_MAX")

  (define read-symbolic-link
    (let ([substring substring]
        [buf (make-string (fx+ _filename_max 1))] )
      (lambda (fname)
        (##sys#check-string fname 'read-symbolic-link)
        (let ([len (##core#inline "C_readlink" (##sys#make-c-string (##sys#expand-home-path fname)) buf)])
        (when (fx< len 0)
          (posix-error #:file-error 'read-symbolic-link "cannot read symbolic link" fname) )
        (substring buf 0 len) ) ) ) )

  (define file-link
    (let ([link (foreign-lambda int "link" c-string c-string)])
      (lambda (old new)
        (##sys#check-string old 'file-link)
        (##sys#check-string new 'file-link)
        (when (fx< (link old new) 0)
        (posix-error #:file-error 'hard-link "could not create hard link" old new) ) ) ) ) ] )


;;; Using file-descriptors:

(define-foreign-variable _stdin_fileno int "STDIN_FILENO")
(define-foreign-variable _stdout_fileno int "STDOUT_FILENO")
(define-foreign-variable _stderr_fileno int "STDERR_FILENO")

(define fileno/stdin _stdin_fileno)
(define fileno/stdout _stdout_fileno)
(define fileno/stderr _stderr_fileno)

(let ()
  (define (mode inp m)
    (##sys#make-c-string
     (cond [(pair? m)
            (let ([m (car m)])
              (case m
                [(###append) (if (not inp) "a" (##sys#error "invalid mode for input file" m))]
                [else (##sys#error "invalid mode argument" m)] ) ) ]
           [inp "r"]
           [else "w"] ) ) )
  (define (check loc fd inp r)
    (if (##sys#null-pointer? r)
        (posix-error #:file-error loc "cannot open file" fd)
        (let ([port (##sys#make-port inp ##sys#stream-port-class "(fdport)" 'stream)])
          (##core#inline "C_set_file_ptr" port r)
          port) ) )
  (set! open-input-file*
    (lambda (fd . m)
      (##sys#check-exact fd 'open-input-file*)
      (check 'open-input-file* fd #t (##core#inline_allocate ("C_fdopen" 2) fd (mode #t m))) ) )
  (set! open-output-file*
    (lambda (fd . m)
      (##sys#check-exact fd 'open-output-file*)
      (check 'open-output-file* fd #f (##core#inline_allocate ("C_fdopen" 2) fd (mode #f m)) ) ) ) )

(define port->fileno
  (lambda (port)
    (##sys#check-port port 'port->fileno)
    (cond [(eq? 'socket (##sys#slot port 7)) (##sys#tcp-port->fileno port)]
          [(not (zero? (##sys#peek-unsigned-integer port 0)))
           (let ([fd (##core#inline "C_C_fileno" port)])
             (when (fx< fd 0)
               (posix-error #:file-error 'port->fileno "cannot access file-descriptor of port" port) )
             fd) ]
          [else (posix-error #:type-error 'port->fileno "port has no attached file" port)] ) ) )

(define duplicate-fileno
  (lambda (old . new)
    (##sys#check-exact old duplicate-fileno)
    (let ([fd (if (null? new)
                  (##core#inline "C_dup" old)
                  (let ([n (car new)])
                    (##sys#check-exact n 'duplicate-fileno)
                    (##core#inline "C_dup2" old n) ) ) ] )
      (when (fx< fd 0)
        (posix-error #:file-error 'duplicate-fileno "cannot duplicate file-descriptor" old) )
      fd) ) )

;FIXME - UTF8?

(define ##sys#custom-input-port
  (let ([make-input-port make-input-port]
        [set-port-name! set-port-name!] )
    (lambda (loc nam fd
             #!optional (bufi 0) (on-close (lambda () (void))) (more? #f))
      (let ([bufsiz (if (fixnum? bufi) (fxmax bufi 1) (##sys#size bufi))]
            [buf (if (fixnum? bufi) (##sys#make-string (fxmax bufi 1)) bufi)]
            [len 0]
            [pos 0] )
        (let (
            [ready?
              (lambda ()
                (when (fx= -1 (##sys#file-select-one fd))
                  (posix-error #:file-error loc "cannot select" fd nam) ) )]
            [peek
              (lambda ()
                (if (fx>= pos len)
                  #!eof
                  (##core#inline "C_subchar" buf pos)) )]
            [fetch
              (lambda ()
                (when (fx>= pos len)
                  (let loop ()
                    (let ([n (##core#inline "C_read" fd buf bufsiz)])
                      (cond
                        [(fx= n -1)
                          (if (fx= _errno _ewouldblock)
                            (begin
                              (##sys#thread-block-for-i/o! ##sys#current-thread fd #t)
                              (##sys#thread-yield!)
                              (loop) )
                            (posix-error #:file-error loc "cannot read" fd nam) )]
                        [(and more? (fx= n 0))
                          ; When "more" keep trying, otherwise read once more
                          ; to guard against race conditions
                          (if (more?)
                            (begin
                              (##sys#thread-yield!)
                              (loop) )
                            (let ([n (##core#inline "C_read" fd buf bufsiz)])
                              (when (fx= n -1)
                                (if (fx= _errno _ewouldblock)
                                  (set! n 0)
                                  (posix-error #:file-error loc "cannot read" fd nam) ) )
                              (set! len n)
                              (set! pos 0) ) )]
                        [else
                          (set! len n)
                          (set! pos 0)]) ) ) ) )] )
          (letrec (
              [port
                (make-input-port
                  (lambda () ; Read
                    (fetch)
                    (let ([ch (peek)])
                      #;(unless (eof-object? ch) (set! pos (fx+ pos 1)))
                      (set! pos (fx+ pos 1))
                      ch ) )
                  (lambda () ; Ready?
                    (or (fx< pos len) (ready?)) )
                  (lambda () ; Close
                    (unless (##sys#slot port 8) ;closed?
                      (when (fx< (##core#inline "C_close" fd) 0)
                        (posix-error #:file-error loc "cannot close" fd nam) )
                      (on-close) ) )
                  (lambda () ; Peek
                    (fetch)
                    (peek) )
                  #;(lambda (port n dest start) ; Read-String
                    )
                  #;(lambda (port limit) ; Read-Line
                    ) )] )
            (set-port-name! port nam)
            port ) ) ) ) ) )

(define ##sys#custom-output-port
  (let ([make-output-port make-output-port]
        [set-port-name! set-port-name!] )
    (lambda (loc nam fd
             #!optional (bufi 0) (on-close (lambda () (void))))
      (letrec (
          [poke
            (lambda (s len)
              (let ([n (##core#inline "C_write" fd s len)])
                (cond
                  [(fx= -1 n)
                    (if (fx= _errno _ewouldblock)
                      (begin
                        (##sys#thread-yield!)
                        (poke s len) )
                      (posix-error loc #:file-error "cannot write" fd nam) ) ]
                  [(fx< n len)
                    (poke (##sys#substring s n len) (fx- len n)) ] ) ) )]
          [store
            (let ([bufsiz (if (fixnum? bufi) bufi (##sys#size bufi))])
              (if (fx= 0 bufsiz)
                (lambda (s)
                  (when s
                    (poke s (##sys#size s)) ) )
                (let ([buf (if (fixnum? bufi) (##sys#make-string bufi) bufi)]
                      [pos 0])
                  (lambda (s)
                    (if s
                      (let loop ([rem (fx- bufsiz pos)] [start 0] [len (##sys#size s)])
                        (cond
                          [(fx= 0 rem)
                            (poke buf bufsiz)
                            (set! pos 0)
                            (loop bufsiz 0 len)]
                          [(fx< rem len)
                            (##core#inline "C_substring_copy" s buf start rem pos)
                            (loop 0 rem (fx- len rem))]
                          [else
                            (##core#inline "C_substring_copy" s buf start len pos)
                            (set! pos (fx+ pos len))] ) )
                      (when (fx< 0 pos)
                        (poke buf pos) ) ) ) ) ) )])
        (letrec (
            [port
              (make-output-port
                (lambda (s) ; Write
                  (store s) )
                (lambda () ; Close
                  (unless (##sys#slot port 8) ;closed?
                    (when (fx< (##core#inline "C_close" fd) 0)
                      (posix-error #:file-error loc "cannot close" fd nam) )
                    (on-close) ) )
                (lambda () ; Flush
                  (store #f) ) )] )
          (set-port-name! port nam)
          port ) ) ) ) )


;;; Other file operations:

(cond-expand
 [ecos]
 [else
  (define file-truncate
    (lambda (fname off)
      (##sys#check-number off 'file-truncate)
      (when (fx< (cond [(string? fname) (##core#inline "C_truncate" (##sys#make-c-string (##sys#expand-home-path fname)) off)]
                       [(fixnum? fname) (##core#inline "C_ftruncate" fname off)]
                       [else (##sys#error 'file-truncate "invalid file" fname)] )
                 0)
            (posix-error #:file-error 'file-truncate "cannot truncate file" fname off) ) ) ) ] )


;;; Record locking:

(define-foreign-variable _f_wrlck int "F_WRLCK")
(define-foreign-variable _f_rdlck int "F_RDLCK")
(define-foreign-variable _f_unlck int "F_UNLCK")

(let ()
  (define (setup port args loc)
    (let-optionals* args ([start 0]
                          [len #t] )
      (##sys#check-port port loc)
      (##sys#check-number start loc)
      (if (eq? #t len)
          (set! len 0)
          (##sys#check-number len loc) )
      (##core#inline "C_flock_setup" (if (##sys#slot port 1) _f_rdlck _f_wrlck) start len)
      (##sys#make-structure 'lock port start len) ) )
  (define (err msg lock loc)
    (posix-error #:file-error loc msg (##sys#slot lock 1) (##sys#slot lock 2) (##sys#slot lock 3)) )
  (set! file-lock
    (lambda (port . args)
      (let ([lock (setup port args 'file-lock)])
        (if (fx< (##core#inline "C_flock_lock" port) 0)
            (err "cannot lock file" lock 'file-lock)
            lock) ) ) )
  (set! file-lock/blocking
    (lambda (port . args)
      (let ([lock (setup port args 'file-lock/blocking)])
        (if (fx< (##core#inline "C_flock_lockw" port) 0)
            (err "cannot lock file" lock 'file-lock/blocking)
            lock) ) ) )
  (set! file-test-lock
    (lambda (port . args)
      (let ([lock (setup port args 'file-test-lock)])
        (cond [(##core#inline "C_flock_test" port) => (lambda (c) (and (not (fx= c 0)) c))]
              [else (err "cannot unlock file" lock 'file-test-lock)] ) ) ) ) )

(define file-unlock
  (lambda (lock)
    (##sys#check-structure lock 'lock 'file-unlock)
    (##core#inline "C_flock_setup" _f_unlck (##sys#slot lock 2) (##sys#slot lock 3))
    (when (fx< (##core#inline "C_flock_lock" (##sys#slot lock 1)) 0)
      (posix-error #:file-error 'file-unlock "cannot unlock file" lock) ) ) )


(cond-expand
 [ecos]
 [else

;;; FIFOs:

  (define create-fifo
    (lambda (fname . mode)
      (##sys#check-string fname 'create-fifo)
      (let ([mode (if (pair? mode) (car mode) (fxior _s_irwxu (fxior _s_irwxg _s_irwxo)))])
        (##sys#check-exact mode 'create-fifo)
        (when (fx< (##core#inline "C_mkfifo" (##sys#make-c-string (##sys#expand-home-path fname)) mode) 0)
        (posix-error #:file-error 'create-fifo "cannot create FIFO" fname mode) ) ) ) )

  (define fifo?
    (lambda (filename)
      (##sys#check-string filename 'fifo?)
      (let ([v (##sys#file-info (##sys#expand-home-path filename))])
        (if v
          (fx= 3 (##sys#slot v 4))
          (posix-error #:file-error 'fifo? "file does not exist" filename) ) ) ) )

;;; Environment access:

  (define setenv
    (lambda (var val)
      (##sys#check-string var 'setenv)
      (##sys#check-string val 'setenv)
      (##core#inline "C_setenv" (##sys#make-c-string var) (##sys#make-c-string val))
      (##core#undefined) ) )

  (define (unsetenv var)
    (##sys#check-string var 'unsetenv)
    (##core#inline "C_putenv" (##sys#make-c-string var))
    (##core#undefined) )

  (define current-environment
    (let ([get (foreign-lambda c-string "C_getenventry" int)])
      (lambda ()
        (let loop ([i 0])
          (let ([entry (get i)])
            (if entry
                (let scan ([j 0])
                  (if (char=? #\= (##core#inline "C_subchar" entry j))
                      (cons (cons (##sys#substring entry 0 j)
                                  (##sys#substring entry (fx+ j 1) (##sys#size entry)))
                            (loop (fx+ i 1)))
                      (scan (fx+ j 1)) ) )
                '() ) ) ) ) ) ) ] )

;;; Memory mapped I/O:
(cond-expand
 [ecos]
 [else

  (define-foreign-variable _prot_read int "PROT_READ")
  (define-foreign-variable _prot_write int "PROT_WRITE")
  (define-foreign-variable _prot_exec int "PROT_EXEC")
  (define-foreign-variable _prot_none int "PROT_NONE")

  (define prot/read _prot_read)
  (define prot/write _prot_write)
  (define prot/exec _prot_exec)
  (define prot/none _prot_none)

  (define-foreign-variable _map_fixed int "MAP_FIXED")
  (define-foreign-variable _map_shared int "MAP_SHARED")
  (define-foreign-variable _map_private int "MAP_PRIVATE")
  (define-foreign-variable _map_anonymous int "MAP_ANON")
  (define-foreign-variable _map_file int "MAP_FILE")

  (define map/fixed _map_fixed)
  (define map/shared _map_shared)
  (define map/private _map_private)
  (define map/anonymous _map_anonymous)
  (define map/file _map_file)

  (define map-file-to-memory
    (let ([mmap (foreign-lambda c-pointer "mmap" c-pointer integer int int int integer)] )
      (lambda (addr len prot flag fd . off)
        (let ([addr (if (not addr) (##sys#null-pointer) addr)]
              [off (if (pair? off) (car off) 0)] )
          (unless (and (##core#inline "C_blockp" addr) (##core#inline "C_specialp" addr))
                  (##sys#signal-hook #:type-error 'map-file-to-memory "bad argument type - not a foreign pointer" addr) )
          (let ([addr2 (mmap addr len prot flag fd off)])
            (when (eq? -1 (##sys#pointer->address addr2))
                  (posix-error #:file-error 'map-file-to-memory "cannot map file to memory" addr len prot flag fd off) )
            (##sys#make-structure 'mmap addr2 len) ) ) ) ) )

  (define unmap-file-from-memory
    (let ([munmap (foreign-lambda int "munmap" c-pointer integer)] )
      (lambda (mmap . len)
        (##sys#check-structure mmap 'mmap 'unmap-file-from-memory)
        (let ([len (if (pair? len) (car len) (##sys#slot mmap 2))])
          (unless (eq? 0 (munmap (##sys#slot mmap 1) len))
                  (posix-error #:file-error 'unmap-file-from-memory "cannot unmap file from memory" mmap len) ) ) ) ) )

  (define (memory-mapped-file-pointer mmap)
    (##sys#check-structure mmap 'mmap 'memory-mapped-file-pointer)
    (##sys#slot mmap 1) )

  (define (memory-mapped-file? x)
    (##sys#structure? x 'mmap) )])

;;; Time related things:

(define (seconds->local-time secs)
  (##sys#check-number secs 'seconds->local-time)
  (##sys#decode-seconds secs #f) )

(define (seconds->utc-time secs)
  (##sys#check-number secs 'seconds->utc-time)
  (##sys#decode-seconds secs #t) )

(define seconds->string
  (let ([ctime (foreign-lambda c-string "C_ctime" integer)])
    (lambda (secs)
      (let ([str (ctime secs)])
        (unless str (##sys#error 'seconds->string "cannot convert seconds to string" secs))
        str) ) ) )

(define time->string
  (let ([asctime (foreign-lambda c-string "C_asctime" scheme-object)])
    (lambda (tm)
      (##sys#check-vector tm 'time->string)
      (when (fx< (##sys#size tm) 10) (##sys#error 'time->string "time vector too short" tm))
      (let ([str (asctime tm)])
        (unless str (##sys#error 'time->string "cannot convert time vector to string" tm))
        str) ) ) )

(define (local-time->seconds tm)
  (##sys#check-vector tm 'local-time->seconds)
  (when (fx< (##sys#size tm) 10) (##sys#error 'local-time->seconds "time vector too short" tm))
  (if (##core#inline "C_mktime" tm)
      (##sys#cons-flonum)
      (##sys#error 'local-time->seconds "cannot convert time vector to seconds" tm) ) )

(define (utc-time->seconds tm)
  (##sys#check-vector tm 'utc-time->seconds)
  (when (fx< (##sys#size tm) 10) (##sys#error 'utc-time->seconds "time vector too short" tm))
  (if (##core#inline "C_timegm" tm)
      (##sys#cons-flonum)
      (##sys#error 'utc-time->seconds "cannot convert time vector to seconds" tm) ) )

(define local-timezone-abbreviation
  (foreign-lambda* c-string ()
   "\n#if !defined(__CYGWIN__) && !defined(__SVR4) && !defined(__uClinux__) && !defined(__hpux__)\n"
   "time_t clock = (time_t)0;"
   "struct tm *ltm = C_localtime(&clock);"
   "char *z = ltm ? (char *)ltm->tm_zone : 0;"
   "\n#else\n"
   "char *z = (daylight ? tzname[1] : tzname[0]);"
   "\n#endif\n"
   "return(z);") )


;;; Other things:

(define _exit
  (let ([ex0 (foreign-lambda void "_exit" int)])
    (lambda code
      (##sys#cleanup-before-exit)
      (ex0 (if (pair? code) (car code) 0)) ) ) )

(define set-alarm! (foreign-lambda int "C_alarm" int))

(define-foreign-variable _iofbf int "_IOFBF")
(define-foreign-variable _iolbf int "_IOLBF")
(define-foreign-variable _ionbf int "_IONBF")
(define-foreign-variable _bufsiz int "BUFSIZ")

(define set-buffering-mode!
    (lambda (port mode . size)
      (##sys#check-port port 'set-buffering-mode!)
      (let ([size (if (pair? size) (car size) _bufsiz)]
            [mode (case mode
                    [(###full) _iofbf]
                    [(###line) _iolbf]
                    [(###none) _ionbf]
                    [else (##sys#error 'set-buffering-mode! "invalid buffering-mode" mode port)] ) ] )
        (##sys#check-exact size 'set-buffering-mode!)
        (when (fx< (if (eq? 'stream (##sys#slot port 7))
                       (##core#inline "C_setvbuf" port mode size)
                       -1)
                   0)
          (##sys#error 'set-buffering-mode! "cannot set buffering mode" port mode size) ) ) ) )

(cond-expand
 [ecos]
 [else
  (define (terminal-port? port)
    (##sys#check-port port 'terminal-port?)
    (let ([fp (##sys#peek-unsigned-integer port 0)])
      (and (not (eq? fp 0)) (##core#inline "C_tty_portp" port) ) ) )

  (define terminal-name
    (let ([ttyname (foreign-lambda nonnull-c-string "ttyname" int)] )
      (lambda (port)
        (##sys#check-port port 'terminal-name)
        (unless (and (eq? 'stream (##sys#slot port 7))
                   (##core#inline "C_tty_portp" port) )
        (##sys#error 'terminal-name "port is not connected to a terminal" port) )
        (ttyname (##core#inline "C_C_fileno" port) ) ) ) )

  (define get-host-name
    (let ([getit
         (foreign-lambda* c-string ()
           "if(gethostname(C_hostbuf, 256) == -1) return(NULL);
              else return(C_hostbuf);") ] )
      (lambda ()
        (let ([host (getit)])
        (unless host
          (posix-error #:error 'get-host-name "cannot retrieve host-name") )
        host) ) ) ) ] )


;;; Filename globbing:

(define glob
  (let ([glob->regexp glob->regexp]
        [directory directory]
        [make-pathname make-pathname]
        [decompose-pathname decompose-pathname] )
    (lambda paths
      (let conc ([paths paths])
        (if (null? paths)
            '()
            (let ([path (car paths)])
              (let-values ([(dir file ext) (decompose-pathname path)])
                (let ([rx (glob->regexp (make-pathname #f (or file "*") ext))])
                  (let loop ([f (directory (or dir ".") #t)])
                    (cond [(null? f) (conc (cdr paths))]
                          [(string-match rx (car f))
                           => (lambda (m) (cons (make-pathname dir (car m)) (loop (cdr f)))) ]
                          [else (loop (cdr f))] ) ) ) ) ) ) ) ) ) )


;;; Process handling:

(cond-expand [ecos]
             [else
  (define process-fork
    (let ([fork (foreign-lambda int "C_fork")])
      (lambda thunk
        (let ([pid (fork)])
        (cond [(fx= -1 pid) (posix-error #:process-error 'process-fork "cannot create child process")]
              [(and (pair? thunk) (fx= pid 0))
               ((car thunk))
               ((foreign-lambda void "_exit" int) 0) ]
              [else pid] ) ) ) ) )

  (define process-execute
    (let ([setarg (foreign-lambda void "C_set_exec_arg" int scheme-pointer int)]
          [freeargs (foreign-lambda void "C_free_exec_args")]
          [setenv (foreign-lambda void "C_set_exec_env" int scheme-pointer int)]
          [freeenv (foreign-lambda void "C_free_exec_env")]
          [pathname-strip-directory pathname-strip-directory] )
      (lambda (filename #!optional (arglist '()) envlist)
        (##sys#check-string filename 'process-execute)
        (##sys#check-list arglist 'process-execute)
        (let ([s (pathname-strip-directory filename)])
        (setarg 0 s (##sys#size s)) )
        (do ([al arglist (cdr al)]
             [i 1 (fx+ i 1)] )
            ((null? al)
             (setarg i #f 0)
             (when envlist
               (do ([el envlist (cdr el)]
                    [i 0 (fx+ i 1)] )
                   ((null? el) (setenv i #f 0))
                 (let ([s (car el)])
                   (##sys#check-string s 'process-execute)
                   (setenv i s (##sys#size s)) ) ) )
             (let* ([prg (##sys#make-c-string (##sys#expand-home-path filename))]
                    [r (if envlist
                         (##core#inline "C_execve" prg)
                         (##core#inline "C_execvp" prg) )] )
               (when (fx= r -1)
                 (freeargs)
                 (freeenv)
                 (posix-error #:process-error 'process-execute "cannot execute process" filename) ) ) )
          (let ([s (car al)])
            (##sys#check-string s 'process-execute)
            (setarg i s (##sys#size s)) ) ) ) ) )

  (define-foreign-variable _wnohang int "WNOHANG")
  (define-foreign-variable _wait-status int "C_wait_status")

  (define (##sys#process-wait pid nohang)
    (let* ([res (##core#inline "C_waitpid" pid (if nohang _wnohang 0))]
           [norm (##core#inline "C_WIFEXITED" _wait-status)] )
      (values
        res
        norm
        (cond [norm (##core#inline "C_WEXITSTATUS" _wait-status)]
              [(##core#inline "C_WIFSIGNALED" _wait-status)
                (##core#inline "C_WTERMSIG" _wait-status)]
              [else (##core#inline "C_WSTOPSIG" _wait-status)] ) ) ) )

  (define process-wait
    (lambda args
      (let-optionals* args ([pid #f] [nohang #f])
        (let ([pid (or pid -1)])
          (##sys#check-exact pid 'process-wait)
          (receive [epid enorm ecode] (##sys#process-wait pid nohang)
            (if (fx= epid -1)
                (posix-error #:process-error 'process-wait "waiting for child process failed" pid)
                (values epid enorm ecode) ) ) ) ) ) )

  (define current-process-id (foreign-lambda int "C_getpid"))
  (define parent-process-id (foreign-lambda int "C_getppid"))

  (define sleep (foreign-lambda int "C_sleep" int))

  (define process-signal
    (lambda (id . sig)
      (let ([sig (if (pair? sig) (car sig) _sigterm)])
        (##sys#check-exact id 'process-signal)
        (##sys#check-exact sig 'process-signal)
        (let ([r (##core#inline "C_kill" id sig)])
        (when (fx= r -1) (posix-error #:process-error 'process-signal "could not send signal to process" id sig) ) ) ) ) )

  ;FIXME - shouldn't be private

  (define (##sys#shell-command)
    (or (getenv "SHELL") "/bin/sh") )

  (define (##sys#shell-command-arguments cmdlin)
    (list "-c" cmdlin) )

  (define process-run
    (let ([process-fork process-fork]
          [process-execute process-execute]
          [getenv getenv] )
      (lambda (f . args)
        (let ([args (if (pair? args) (car args) #f)]
              [pid (process-fork)] )
        (cond [(not (eq? pid 0)) pid]
              [args (process-execute f args)]
              [else
               (process-execute (##sys#shell-command) (##sys#shell-command-arguments f)) ] ) ) ) ) )

  ;;; Run subprocess connected with pipes:

  ;; ##sys#process
  ; loc            caller procedure symbol
  ; cmd            pathname or commandline
  ; args           string-list or '()
  ; env            string-list or #f
  ; stdoutf        #f then share, or #t then create
  ; stdinf         #f then share, or #t then create
  ; stderrf        #f then share, or #t then create
  ;
  ; (values stdin-input-port? stdout-output-port? pid stderr-input-port?)
  ; where stdin-input-port?, etc. is a port or #f, indicating no port created.

  (define-constant DEFAULT-INPUT-BUFFER-SIZE 256)
  (define-constant DEFAULT-OUTPUT-BUFFER-SIZE 0)

  ;FIXME process-execute, process-fork don't show parent caller

  (define ##sys#process
    (let (
        [create-pipe create-pipe]
        [process-wait process-wait]
        [process-fork process-fork]
        [process-execute process-execute]
        [duplicate-fileno duplicate-fileno]
        [file-close file-close]
        [replace-fd
          (lambda (loc fd stdfd)
            (unless (fx= stdfd fd)
              (duplicate-fileno fd stdfd)
              (file-close fd) ) )] )
      (let (
          [make-on-close
            (lambda (loc pid clsvec idx idxa idxb)
              (lambda ()
                (vector-set! clsvec idx #t)
                (when (and (vector-ref clsvec idxa) (vector-ref clsvec idxb))
                  (receive [_ flg cod] (process-wait pid)
                    (unless flg
                      (##sys#signal-hook #:process-error loc
                        "abnormal process exit" pid cod)) ) ) ) )]
          [needed-pipe
            (lambda (loc port)
              (and port
                   (receive [i o] (create-pipe) (cons i o))) )]
          [connect-parent
            (lambda (loc pipe port fd)
              (and port
                   (let ([usefd (car pipe)] [clsfd (cdr pipe)])
                     (file-close clsfd)
                     (##sys#file-nonblocking! usefd)
                     usefd) ) )]
          [connect-child
            (lambda (loc pipe port stdfd)
              (when port
                (let ([usefd (car pipe)] [clsfd (cdr pipe)])
                  (file-close clsfd)
                  (replace-fd loc usefd stdfd)) ) )] )
        (let (
            [child
              (lambda (loc cmd args env stdoutf stdinf stderrf)
                (let ([ipipe (needed-pipe loc stdinf)]
                      [opipe (needed-pipe loc stdoutf)]
                      [epipe (needed-pipe loc stderrf)]
                      [swap-ends
                        (lambda (pipe)
                          (and pipe (cons (cdr pipe) (car pipe)) ) )])
                  (values
                    ipipe (swap-ends opipe) epipe
                    (process-fork
                      (lambda ()
                        (connect-child loc opipe stdinf fileno/stdin)
                        (connect-child loc (swap-ends ipipe) stdoutf fileno/stdout)
                        (connect-child loc (swap-ends epipe) stderrf fileno/stderr)
                        (process-execute cmd args env)))) ) )]
            [input-port
              (lambda (loc pid cmd pipe stdf stdfd on-close)
                (and-let* ([fd (connect-parent loc pipe stdf stdfd)])
                  (##sys#custom-input-port loc cmd fd DEFAULT-INPUT-BUFFER-SIZE on-close) ) )]
            [output-port
              (lambda (loc pid cmd pipe stdf stdfd on-close)
                (and-let* ([fd (connect-parent loc pipe stdf stdfd)])
                  (##sys#custom-output-port loc cmd fd DEFAULT-OUTPUT-BUFFER-SIZE on-close) ) )] )
          (lambda (loc cmd args env stdoutf stdinf stderrf)
            (receive [inpipe outpipe errpipe pid]
                       (child loc cmd args env stdoutf stdinf stderrf)
              ;When shared assume already "closed", since only created ports
              ;should be explicitly closed, and when one is closed we want
              ;to wait.
              (let ([clsvec (vector (not stdinf) (not stdoutf) (not stderrf))])
                (values
                  (input-port loc pid cmd inpipe stdinf fileno/stdin
                    (make-on-close loc pid clsvec 0 1 2))
                  (output-port loc pid cmd outpipe stdoutf fileno/stdout
                    (make-on-close loc pid clsvec 1 0 2))
                  pid
                  (input-port loc pid cmd errpipe stderrf fileno/stderr
                    (make-on-close loc pid clsvec 2 0 1)) ) ) ) ) ) ) ) )

  ;;; Run subprocess connected with pipes:

  #;(define process (void))
  #;(define process* (void))
  (let ([%process
          (lambda (loc err? cmd args env)
            (##sys#check-string cmd loc)
            (if args
              (begin
                (##sys#check-list args loc)
                (for-each (cut ##sys#check-string <> loc) args) )
              (begin
                (set! args (##sys#shell-command-arguments cmd))
                (set! cmd (##sys#shell-command)) ) )
            (when env
              (##sys#check-list env loc)
              (for-each (cut ##sys#check-string <> loc) env) )
            (receive [in out pid err]
                       (##sys#process loc cmd args env #t #t err?)
              (if err?
                (values in out pid err)
                (values in out pid) ) ) )] )
    (set! process
      (lambda (cmd #!optional args env)
        (%process 'process #f cmd args env) ))
    (set! process*
      (lambda (cmd #!optional args env)
        (%process 'process* #t cmd args env) )) ) ] )

;;; Find matching files:

(define find-files
  (let ([glob glob]
        [string-match string-match]
        [make-pathname make-pathname]
        [directory? directory?] )
    (lambda (dir pred . action-id-limit)
      (let-optionals action-id-limit
          ([action (lambda (x y) (cons x y))] ; we want cons inlined
           [id '()]
           [limit #f] )
        (##sys#check-string dir 'find-files)
        (let* ([depth 0]
               [lproc
                (cond [(not limit) (lambda _ #t)]
                      [(fixnum? limit) (lambda _ (fx< depth limit))]
                      [else limit] ) ]
               [pproc
                (if (string? pred)
                    (lambda (x) (string-match pred x))
                    pred) ] )
          (let loop ([fs (glob (make-pathname dir "*"))]
                     [r id] )
            (if (null? fs)
                r
                (let ([f (##sys#slot fs 0)]
                      [rest (##sys#slot fs 1)] )
                  (cond [(directory? f)
                         (cond [(member (pathname-file f) '("." "..")) (loop rest r)]
                               [(lproc f)
                                (loop rest
                                      (fluid-let ([depth (fx+ depth 1)])
                                        (loop (glob (make-pathname f "*")) r) ) ) ]
                               [else (loop rest r)] ) ]
                        [(pproc f) (loop rest (action f r))]
                        [else (loop rest r)] ) ) ) ) ) ) ) ) )


(cond-expand
 [ecos]
 [else

  ;;; chroot:

  (define set-root-directory!
    (let ([chroot (foreign-lambda int "chroot" c-string)])
      (lambda (dir)
        (##sys#check-string dir 'set-root-directory!)
        (when (fx< (chroot dir) 0)
        (posix-error #:file-error 'set-root-directory! "unable to change root directory" dir) ) ) ) ) ] )
