;;;; posixwin.scm - Miscellaneous file- and process-handling routines, available on Windows
;
; By Sergey Khorev
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


; Not implemented:
;
; open/noctty  open/nonblock  open/fsync  open/sync
; perm/isvtx  perm/isuid  perm/isgid
; file-select
; symbolic-link?
; set-signal-mask!  signal-mask  signal-masked?  signal-mask!  signal-unmask!
; user-information  group-information  get-groups  set-groups!  initialize-groups
; errno/wouldblock
; change-file-owner
; current-user-id  current-group-id  current-effective-user-id  current-effective-groupd-id
; set-user-id!  set-group-id!
; create-session
; process-group-id  set-process-group-id!
; create-symbolic-link  read-symbolic-link
; file-truncate
; file-lock  file-lock/blocking  file-unlock  file-test-lock
; create-fifo  fifo?
; prot/...
; map/...
; map-file-to-memory  unmap-file-from-memory  memory-mapped-file-pointer  memory-mapped-file?
; set-alarm!
; terminal-port?  terminal-name
; process-fork  process-wait
; parent-process-id
; process-signal


(declare
  (unit posix)
  (uses scheduler regex extras utils)
  (disable-interrupts)
  (usual-integrations)
  (hide ##sys#stat close-handle posix-error)
  (foreign-declare #<<EOF
#ifndef WIN32_LEAN_AND_MEAN
# define WIN32_LEAN_AND_MEAN
#endif

/*
MinGW should have winsock2.h and ws2tcpip.h as well.
The CMake build will set HAVE_WINSOCK2_H and HAVE_WS2TCPIP_H.
However, the _MSC_VER test is still needed for vcbuild.bat.
./configure doesn't test for these.  It should, for MinGW.
*/
#if (_MSC_VER > 1300) || (defined(HAVE_WINSOCK2_H) && defined(HAVE_WS2TCPIP_H))
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <winsock.h>
#endif

#include <signal.h>
#include <errno.h>
#include <io.h>
#include <stdio.h>
#include <process.h>

static int C_not_implemented(void);
int C_not_implemented() { return -1; }

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <direct.h>

#include <time.h>

#define ARG_MAX 256
#define PIPE_BUF 512

static C_TLS char *C_exec_args[ ARG_MAX ];
static C_TLS struct group *C_group;
static C_TLS int C_pipefds[ 2 ];
static C_TLS time_t C_secs;
static C_TLS struct tm C_tm;
static C_TLS struct stat C_statbuf;

/* pipe handles */
static C_TLS HANDLE C_rd0, C_wr0, C_wr0_, C_rd1, C_wr1, C_rd1_;
static C_TLS HANDLE C_save0, C_save1; /* saved I/O handles */
static C_TLS char C_rdbuf; /* one-char buffer for read */
static C_TLS int C_exstatus;

/* platform information */
static C_TLS char C_hostname[256];
static C_TLS char C_osver[16];
static C_TLS char C_osrel[16];
static C_TLS char C_processor[16];

#define C_mkdir(str)        C_fix(mkdir(C_c_string(str)))
#define C_chdir(str)        C_fix(chdir(C_c_string(str)))
#define C_rmdir(str)        C_fix(rmdir(C_c_string(str)))

#ifndef __WATCOMC__
/* DIRENT stuff */
struct dirent
{
    char *		d_name;
};

typedef struct
{
    struct _finddata_t	fdata;
    int			handle;
    struct dirent	current;
} DIR;

static DIR *opendir(const char *name);
static int closedir(DIR *dir);
static struct dirent *readdir(DIR *dir);

static DIR *opendir(const char *name)
{
    int name_len = strlen(name);
    DIR *dir = (DIR *)malloc(sizeof(DIR));
    char *what;
    if (!dir)
    {
	errno = ENOMEM;
	return NULL;
    }
    what = (char *)malloc(name_len + 3);
    if (!what)
    {
	free(dir);
	errno = ENOMEM;
	return NULL;
    }
    strcpy(what, name);
    if (strchr("\\/", name[name_len - 1]))
	strcat(what, "*");
    else
	strcat(what, "\\*");

    dir->handle = _findfirst(what, &dir->fdata);
    if (dir->handle == -1)
    {
	free(what);
	free(dir);
	return NULL;
    }
    dir->current.d_name = NULL; /* as the first-time indicator */
    free(what);
    return dir;
}

static int closedir(DIR * dir)
{
    if (dir)
    {
	int res = _findclose(dir->handle);
	free(dir);
	return res;
    }
    return -1;
}

static struct dirent *readdir(DIR * dir)
{
    if (dir)
    {
	if (!dir->current.d_name /* first time after opendir */
	     || _findnext(dir->handle, &dir->fdata) != -1)
	{
	    dir->current.d_name = dir->fdata.name;
	    return &dir->current;
	}
    }
    return NULL;
}
#endif /* ifndef __WATCOMC__ */

#ifdef __WATCOMC__
# define mktemp _mktemp
/* there is no P_DETACH in Watcom CRTL */
# define P_DETACH P_NOWAIT
#endif

#define C_opendir(x,h)		C_set_block_item(h, 0, (C_word) opendir(C_c_string(x)))
#define C_closedir(h)   	(closedir((DIR *)C_block_item(h, 0)), C_SCHEME_UNDEFINED)
#define C_readdir(h,e)		C_set_block_item(e, 0, (C_word) readdir((DIR *)C_block_item(h, 0)))
#define C_foundfile(e,b)	(strcpy(C_c_string(b), ((struct dirent *) C_block_item(e, 0))->d_name),	C_fix(strlen(((struct dirent *) C_block_item(e, 0))->d_name)))

#define C_curdir(buf)       (getcwd(C_c_string(buf), 256) ? C_fix(strlen(C_c_string(buf))) : C_SCHEME_FALSE)

#define open_binary_input_pipe(a, n, name)   C_mpointer(a, _popen(C_c_string(name), "r"))
#define open_text_input_pipe(a, n, name)     open_binary_input_pipe(a, n, name)
#define open_binary_output_pipe(a, n, name)  C_mpointer(a, _popen(C_c_string(name), "w"))
#define open_text_output_pipe(a, n, name)    open_binary_output_pipe(a, n, name)
#define close_pipe(p)                        C_fix(_pclose(C_port_file(p)))

#define C_set_file_ptr(port, ptr)  (C_set_block_item(port, 0, (C_block_item(ptr, 0))), C_SCHEME_UNDEFINED)

#define C_getpid            getpid
#define C_chmod(fn, m)      C_fix(chmod(C_data_pointer(fn), C_unfix(m)))
#define C_fdopen(a, n, fd, m) C_mpointer(a, fdopen(C_unfix(fd), C_c_string(m)))
#define C_C_fileno(p)       C_fix(fileno(C_port_file(p)))
#define C_dup(x)            C_fix(dup(C_unfix(x)))
#define C_dup2(x, y)        C_fix(dup2(C_unfix(x), C_unfix(y)))
#define C_setvbuf(p, m, s)  C_fix(setvbuf(C_port_file(p), NULL, C_unfix(m), C_unfix(s)))
#define C_access(fn, m)     C_fix(access((char *)C_data_pointer(fn), C_unfix(m)))
#define C_pipe(d)           C_fix(_pipe(C_pipefds, PIPE_BUF, O_BINARY))
#define C_close(fd)         C_fix(close(C_unfix(fd)))

#define C_getenventry(i)   environ[ i ]

#define C_putenv(s)         C_fix(putenv((char *)C_data_pointer(s)))
#define C_stat(fn)          C_fix(stat((char *)C_data_pointer(fn), &C_statbuf))
#define C_fstat(f)          C_fix(fstat(C_unfix(f), &C_statbuf))

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

static void C_fcall C_set_exec_arg(int i, char *a, int len);
void C_fcall C_set_exec_arg(int i, char *a, int len) {
  char *ptr;
  if(a != NULL) {
    ptr = (char *)C_malloc(len + 1);
    C_memcpy(ptr, a, len);
    ptr[ len ] = '\0';
  }
  else ptr = NULL;
  C_exec_args[ i ] = ptr;
}

static void C_fcall C_free_exec_args();
void C_fcall C_free_exec_args() {
  char **a = C_exec_args;
  while((*a) != NULL) C_free(*(a++));
}

#define C_execvp(f)         C_fix(execvp(C_data_pointer(f), (const char *const *)C_exec_args))

/* MS replacement for the fork-exec pair */
#define C_spawnvp(m, f)	    C_fix(spawnvp(C_unfix(m), C_data_pointer(f), (const char *const *)C_exec_args))

#define C_open(fn, fl, m)   C_fix(open(C_c_string(fn), C_unfix(fl), C_unfix(m)))
#define C_read(fd, b, n)    C_fix(read(C_unfix(fd), C_data_pointer(b), C_unfix(n)))
#define C_write(fd, b, n)   C_fix(write(C_unfix(fd), C_data_pointer(b), C_unfix(n)))
#define C_mkstemp(t)        C_fix(mktemp(C_c_string(t)))

#define C_ftell(p)            C_fix(ftell(C_port_file(p)))
#define C_fseek(p, n, w)      C_mk_nbool(fseek(C_port_file(p), C_unfix(n), C_unfix(w)))
#define C_lseek(fd, o, w)     C_fix(lseek(C_unfix(fd), C_unfix(o), C_unfix(w)))

#define C_ctime(n)          (C_secs = (n), ctime(&C_secs))

#define C_asctime(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), asctime(&C_tm) )
#define C_mktime(v)        (memset(&C_tm, 0, sizeof(struct tm)), C_tm.tm_sec = C_unfix(C_block_item(v, 0)), C_tm.tm_min = C_unfix(C_block_item(v, 1)), C_tm.tm_hour = C_unfix(C_block_item(v, 2)), C_tm.tm_mday = C_unfix(C_block_item(v, 3)), C_tm.tm_mon = C_unfix(C_block_item(v, 4)), C_tm.tm_year = C_unfix(C_block_item(v, 5)), C_tm.tm_wday = C_unfix(C_block_item(v, 6)), C_tm.tm_yday = C_unfix(C_block_item(v, 7)), C_tm.tm_isdst = (C_block_item(v, 8) != C_SCHEME_FALSE), (C_temporary_flonum = mktime(&C_tm)) != -1)

/*
  mapping from Win32 error codes to errno
*/

typedef struct
{
    DWORD   win32;
    int	    libc;
} errmap_t;

static errmap_t errmap[] =
{
    {ERROR_INVALID_FUNCTION,      EINVAL},
    {ERROR_FILE_NOT_FOUND,        ENOENT},
    {ERROR_PATH_NOT_FOUND,        ENOENT},
    {ERROR_TOO_MANY_OPEN_FILES,   EMFILE},
    {ERROR_ACCESS_DENIED,         EACCES},
    {ERROR_INVALID_HANDLE,        EBADF},
    {ERROR_ARENA_TRASHED,         ENOMEM},
    {ERROR_NOT_ENOUGH_MEMORY,     ENOMEM},
    {ERROR_INVALID_BLOCK,         ENOMEM},
    {ERROR_BAD_ENVIRONMENT,       E2BIG},
    {ERROR_BAD_FORMAT,            ENOEXEC},
    {ERROR_INVALID_ACCESS,        EINVAL},
    {ERROR_INVALID_DATA,          EINVAL},
    {ERROR_INVALID_DRIVE,         ENOENT},
    {ERROR_CURRENT_DIRECTORY,     EACCES},
    {ERROR_NOT_SAME_DEVICE,       EXDEV},
    {ERROR_NO_MORE_FILES,         ENOENT},
    {ERROR_LOCK_VIOLATION,        EACCES},
    {ERROR_BAD_NETPATH,           ENOENT},
    {ERROR_NETWORK_ACCESS_DENIED, EACCES},
    {ERROR_BAD_NET_NAME,          ENOENT},
    {ERROR_FILE_EXISTS,           EEXIST},
    {ERROR_CANNOT_MAKE,           EACCES},
    {ERROR_FAIL_I24,              EACCES},
    {ERROR_INVALID_PARAMETER,     EINVAL},
    {ERROR_NO_PROC_SLOTS,         EAGAIN},
    {ERROR_DRIVE_LOCKED,          EACCES},
    {ERROR_BROKEN_PIPE,           EPIPE},
    {ERROR_DISK_FULL,             ENOSPC},
    {ERROR_INVALID_TARGET_HANDLE, EBADF},
    {ERROR_INVALID_HANDLE,        EINVAL},
    {ERROR_WAIT_NO_CHILDREN,      ECHILD},
    {ERROR_CHILD_NOT_COMPLETE,    ECHILD},
    {ERROR_DIRECT_ACCESS_HANDLE,  EBADF},
    {ERROR_NEGATIVE_SEEK,         EINVAL},
    {ERROR_SEEK_ON_DEVICE,        EACCES},
    {ERROR_DIR_NOT_EMPTY,         ENOTEMPTY},
    {ERROR_NOT_LOCKED,            EACCES},
    {ERROR_BAD_PATHNAME,          ENOENT},
    {ERROR_MAX_THRDS_REACHED,     EAGAIN},
    {ERROR_LOCK_FAILED,           EACCES},
    {ERROR_ALREADY_EXISTS,        EEXIST},
    {ERROR_FILENAME_EXCED_RANGE,  ENOENT},
    {ERROR_NESTING_NOT_ALLOWED,   EAGAIN},
    {ERROR_NOT_ENOUGH_QUOTA,      ENOMEM},
    {0, 0}
};

static void set_errno(DWORD w32err)
{
    errmap_t *map = errmap;
    for (; errmap->win32; ++map)
    {
	if (errmap->win32 == w32err)
	{
	    errno = errmap->libc;
	    return;
	}
    }
}

/* functions for creating process with redirected I/O */
static int zero_handles()
{
    C_rd0 = C_wr0 = C_wr0_ = INVALID_HANDLE_VALUE;
    C_rd1 = C_wr1 = C_rd1_ = INVALID_HANDLE_VALUE;
    C_save0 = C_save1 = INVALID_HANDLE_VALUE;
    return 1;
}

static int close_handles()
{
    if (C_rd0 != INVALID_HANDLE_VALUE)
	CloseHandle(C_rd0);
    if (C_rd1 != INVALID_HANDLE_VALUE)
	CloseHandle(C_rd1);
    if (C_wr0 != INVALID_HANDLE_VALUE)
	CloseHandle(C_wr0);
    if (C_wr1 != INVALID_HANDLE_VALUE)
	CloseHandle(C_wr1);
    if (C_rd1_ != INVALID_HANDLE_VALUE)
	CloseHandle(C_rd1_);
    if (C_wr0_ != INVALID_HANDLE_VALUE)
	CloseHandle(C_wr0_);
    if (C_save0 != INVALID_HANDLE_VALUE)
    {
	SetStdHandle(STD_INPUT_HANDLE, C_save0);
	CloseHandle(C_save0);
    }
    if (C_save1 != INVALID_HANDLE_VALUE)
    {
	SetStdHandle(STD_OUTPUT_HANDLE, C_save1);
	CloseHandle(C_save1);
    }
    return zero_handles();
}

static int redir_io()
{
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;

    zero_handles();

    C_save0 = GetStdHandle(STD_INPUT_HANDLE);
    C_save1 = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!CreatePipe(&C_rd0, &C_wr0, &sa, 0)
	    || !SetStdHandle(STD_INPUT_HANDLE, C_rd0)
	    || !DuplicateHandle(GetCurrentProcess(), C_wr0, GetCurrentProcess(),
		&C_wr0_, 0, FALSE, DUPLICATE_SAME_ACCESS)
	    || !CreatePipe(&C_rd1, &C_wr1, &sa, 0)
	    || !SetStdHandle(STD_OUTPUT_HANDLE, C_wr1)
	    || !DuplicateHandle(GetCurrentProcess(), C_rd1, GetCurrentProcess(),
		&C_rd1_, 0, FALSE, DUPLICATE_SAME_ACCESS))
    {
	set_errno(GetLastError());
	close_handles();
	return 0;
    }

    CloseHandle(C_wr0);
    C_wr0 = INVALID_HANDLE_VALUE;
    CloseHandle(C_rd1);
    C_rd1 = INVALID_HANDLE_VALUE;
    return 1;
}

static int run_process(char *cmdline)
{
    PROCESS_INFORMATION pi;
    STARTUPINFO si;

    ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
    ZeroMemory(&si, sizeof(STARTUPINFO));
    si.cb = sizeof(STARTUPINFO);

    C_wr0_ = C_rd1_ = INVALID_HANDLE_VALUE; /* these handles are saved */

    if (CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0, NULL,
		NULL, &si, &pi))
    {
	CloseHandle(pi.hThread);

	SetStdHandle(STD_INPUT_HANDLE, C_save0);
	SetStdHandle(STD_OUTPUT_HANDLE, C_save1);
	C_save0 = C_save1 = INVALID_HANDLE_VALUE;

	CloseHandle(C_rd0);
	CloseHandle(C_wr1);
	C_rd0 = C_wr1 = INVALID_HANDLE_VALUE;
	return (int)pi.hProcess;
    }
    else
    {
	set_errno(GetLastError());
	return 0;
    }
}

static int pipe_write(int hpipe, void* buf, int count)
{
    DWORD done = 0;
    if (WriteFile((HANDLE)hpipe, buf, count, &done, NULL))
	return 1;
    else
    {
	set_errno(GetLastError());
	return 0;
    }
}

static int pipe_read(int hpipe)
{
    DWORD done = 0;
    /* TODO:
    if (!pipe_ready(hpipe))
	go_to_sleep;
    */
    if (ReadFile((HANDLE)hpipe, &C_rdbuf, 1, &done, NULL))
    {
	if (done > 0) /* not EOF yet */
	    return 1;
	else
	    return -1;
    }
    set_errno(GetLastError());
    return 0;
}

static int pipe_ready(int hpipe)
{
    DWORD avail = 0;
    if (PeekNamedPipe((HANDLE)hpipe, NULL, 0, NULL, &avail, NULL) && avail)
	return 1;
    else
    {
	Sleep(0); /* give pipe a chance */
	if (PeekNamedPipe((HANDLE)hpipe, NULL, 0, NULL, &avail, NULL))
	    return (avail > 0);
	else
	    return 0;
    }
}

#define C_zero_handles() C_fix(zero_handles())
#define C_close_handles() C_fix(close_handles())
#define C_redir_io() (redir_io() ? C_SCHEME_TRUE : C_SCHEME_FALSE)
#define C_run_process(cmdline) C_fix(run_process(C_c_string(cmdline)))
#define C_pipe_write(h, b, n) (pipe_write(C_unfix(h), C_c_string(b), C_unfix(n)) ? C_SCHEME_TRUE : C_SCHEME_FALSE)
#define C_pipe_read(h) C_fix(pipe_read(C_unfix(h)))
#define C_pipe_ready(h) (pipe_ready(C_unfix(h)) ? C_SCHEME_TRUE : C_SCHEME_FALSE)
#define close_handle(h) CloseHandle((HANDLE)h)

int process_wait(int h, int t)
{
    if (WaitForSingleObject((HANDLE)h, (t ? 0 : INFINITE)) == WAIT_OBJECT_0)
    {
	DWORD ret;
	if (GetExitCodeProcess((HANDLE)h, &ret))
	{
	    CloseHandle((HANDLE)h);
	    C_exstatus = ret;
	    return 1;
	}
    }
    set_errno(GetLastError());
    return 0;
}

#define C_process_wait(p, t) (process_wait(C_unfix(p), C_truep(t)) ? C_SCHEME_TRUE : C_SCHEME_FALSE)
#define C_sleep(t) (Sleep(C_unfix(t) * 1000), C_SCHEME_UNDEFINED)

int get_hostname()
{
    WSADATA wsa;
    if (WSAStartup(MAKEWORD(1, 1), &wsa) == 0)
    {
	int nok = gethostname(C_hostname, 256);
	WSACleanup();
	return !nok;
    }
    return 0;
}

int sysinfo()
{
    OSVERSIONINFO ovf;
    ZeroMemory(&ovf, sizeof(ovf));
    ovf.dwOSVersionInfoSize = sizeof(ovf);
    if (get_hostname() && GetVersionEx(&ovf))
    {
	SYSTEM_INFO si;
	_snprintf(C_osver, sizeof(C_osver) - 1, "%d.%d.%d",
			   ovf.dwMajorVersion, ovf.dwMinorVersion, ovf.dwBuildNumber);
	switch (ovf.dwPlatformId)
	{
	case VER_PLATFORM_WIN32s:
	    strncpy(C_osrel, "Win32s", sizeof(C_osrel) - 1);
	    break;
	case VER_PLATFORM_WIN32_WINDOWS:
	    strncpy(C_osrel, "Win9x", sizeof(C_osrel) - 1);
	    break;
	case VER_PLATFORM_WIN32_NT:
	default:
	    strncpy(C_osrel, "WinNT", sizeof(C_osrel) - 1);
	    break;
	}
	GetSystemInfo(&si);
	switch (si.wProcessorArchitecture)
	{
    	case PROCESSOR_ARCHITECTURE_INTEL:
	    strncpy(C_processor, "x86", sizeof(C_processor) - 1);
	    break;
#       ifdef PROCESSOR_ARCHITECTURE_IA64
    	case PROCESSOR_ARCHITECTURE_IA64:
	    strncpy(C_processor, "IA64", sizeof(C_processor) - 1);
	    break;
#       endif
#       ifdef PROCESSOR_ARCHITECTURE_AMD64
    	case PROCESSOR_ARCHITECTURE_AMD64:
	    strncpy(C_processor, "x64", sizeof(C_processor) - 1);
	    break;
#       endif
#       ifdef PROCESSOR_ARCHITECTURE_IA32_ON_WIN64
    	case PROCESSOR_ARCHITECTURE_IA32_ON_WIN64:
	    strncpy(C_processor, "WOW64", sizeof(C_processor) - 1);
	    break;
#       endif
    	case PROCESSOR_ARCHITECTURE_UNKNOWN:
	default:
	    strncpy(C_processor, "Unknown", sizeof(C_processor) - 1);
	    break;
	}
	return 1;
    }
    set_errno(GetLastError());
    return 0;
}

#define C_get_hostname() (get_hostname() ? C_SCHEME_TRUE : C_SCHEME_FALSE)
#define C_sysinfo() (sysinfo() ? C_SCHEME_TRUE : C_SCHEME_FALSE)

/*
    Spawn a process, either through shell or directly.
    Params:
    cmd         Command to execute.
    env         Environment for the new process (may be NULL).
    handle, stdin, stdout, stderr
                Spawned process info are returned in integers.
                When spawned process shares standard io stream with the parent
                process the respective value in handle, stdin, stdout, stderr
                is -1.
    params      A bitmask controling operation.
                Bit 1: Child & parent share standard input if this bit is set.
                Bit 2: Share standard output if bit is set.
                Bit 3: Share standard error if bit is set.
                (Bit 4: Execute command in shell if bit is set.)

    Returns: nonzero return value indicates failure.
*/
static
int C_process(const char * cmd, const char ** env,
              int * phandle, int * pstdin_fd, int * pstdout_fd, int * pstderr_fd,
              int params)
{
    int exit_code = 0, i = 0;
    const int
        f_share_io[3] = { params & 1, params & 2, params & 4};
#if 0
    const int
        f_use_shell = params & 8;
#endif

    char * buf = NULL;
    const char * invoke_cmd = NULL;
    int io_fds[3]={-1,-1,-1};
    HANDLE
        child_io_handles[3]={NULL,NULL,NULL},
        standard_io_handles[3]={
            GetStdHandle(STD_INPUT_HANDLE),
            GetStdHandle(STD_OUTPUT_HANDLE),
            GetStdHandle(STD_ERROR_HANDLE)};

    const char modes[3]="rww";
    HANDLE cur_process = GetCurrentProcess(), child_process = NULL;

    /****** create io handles & fds ***/

    for (i=0; i<3 && exit_code == 0; ++i)
    {
        if (f_share_io[i])
        {
            exit_code = !DuplicateHandle(
                cur_process, standard_io_handles[i],
                cur_process, &child_io_handles[i],
                0, FALSE, DUPLICATE_SAME_ACCESS);
        }
        else
        {
            HANDLE a, b, parent_end;
            exit_code = !CreatePipe(&a,&b,NULL,0);
            if(0==exit_code)
            {
                if (modes[i]=='r') { child_io_handles[i]=a; parent_end=b; }
                else { parent_end=a; child_io_handles[i]=b; }
            }
            exit_code=(io_fds[i]=_open_osfhandle((long)parent_end,0))<0;
        }
    }

    /****** make handles inheritable */

    for (i=0; i<3 && exit_code == 0; ++i)
        exit_code = !SetHandleInformation(child_io_handles[i], HANDLE_FLAG_INHERIT, -1);

    /****** create command line ******/

#if 0
    if (f_use_shell && exit_code == 0)
    {
        const char * shell = NULL;
        static const char * const fmt = "%s /c %s";

        shell=getenv("COMSPEC");
        if (NULL==shell)
        {
            OSVERSIONINFO ovf;
            ovf.dwOSVersionInfoSize = sizeof(ovf);
            if (GetVersionEx(&ovf) && (ovf.dwPlatformId == VER_PLATFORM_WIN32_NT))
                shell="cmd.exe";
            else
                shell="command.com";
        }

        buf = (char*) malloc(strlen(fmt)+strlen(shell)+strlen(cmd));
        exit_code=(NULL==buf);
        if (0==exit_code) { sprintf(buf,fmt,shell,cmd); invoke_cmd = buf; }
    }
    else
#endif
        invoke_cmd = cmd;

    /****** finally spawn process ****/

    if (0==exit_code)
    {
        PROCESS_INFORMATION pi;
        STARTUPINFO si;

        ZeroMemory(&pi,sizeof pi);
        ZeroMemory(&si,sizeof si);
        si.cb = sizeof si;
        si.dwFlags = STARTF_USESTDHANDLES;
        si.hStdInput = child_io_handles[0];
        si.hStdOutput = child_io_handles[1];
        si.hStdError = child_io_handles[2];

        exit_code = !CreateProcess(
            NULL,(char*)invoke_cmd,NULL,NULL,TRUE,0,(char**)env,NULL,&si,&pi);

        if (0==exit_code)
        {
            child_process=pi.hProcess;
            CloseHandle(pi.hThread);
        }
    }

    /****** cleanup & return *********/

    free(buf);
    for (i=0; i<3; ++i) CloseHandle(child_io_handles[i]);
    if (exit_code != 0)
    {
        for (i=0; i<3; ++i) _close(io_fds[i]);
        set_errno(GetLastError());
    }
    else
    {
        *phandle = (int)child_process;
        *pstdin_fd = io_fds[0];
        *pstdout_fd = io_fds[1];
        *pstderr_fd = io_fds[2];
    }

    return exit_code;
}
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#make-port ##sys#file-info ##sys#update-errno ##sys#fudge ##sys#make-c-string ##sys#check-port
     ##sys#error ##sys#signal-hook ##sys#peek-unsigned-integer ##sys#process
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
  (declare (emit-exports "posix.exports"))] )

(register-feature! 'posix)

(define posix-error
  (let ([strerror (foreign-lambda c-string "strerror" int)]
        [string-append string-append] )
    (lambda (type loc msg . args)
      (let ([rn (##sys#update-errno)])
        (apply ##sys#signal-hook type loc (string-append msg " - " (strerror rn)) args) ) ) ) )

(define ##sys#posix-error posix-error)


;;; Lo-level I/O:

(define-foreign-variable _pipe_buf int "PIPE_BUF")

(define pipe/buf _pipe_buf)

(define-foreign-variable _o_rdonly int "O_RDONLY")
(define-foreign-variable _o_wronly int "O_WRONLY")
(define-foreign-variable _o_rdwr int "O_RDWR")
(define-foreign-variable _o_creat int "O_CREAT")
(define-foreign-variable _o_append int "O_APPEND")
(define-foreign-variable _o_excl int "O_EXCL")
(define-foreign-variable _o_trunc int "O_TRUNC")
(define-foreign-variable _o_binary int "O_BINARY")
(define-foreign-variable _o_text int "O_TEXT")

(define open/rdonly _o_rdonly)
(define open/wronly _o_wronly)
(define open/rdwr _o_rdwr)
(define open/read _o_rdwr)
(define open/write _o_wronly)
(define open/creat _o_creat)
(define open/append _o_append)
(define open/excl _o_excl)
(define open/trunc _o_trunc)
(define open/binary _o_binary)
(define open/text _o_text)

(define-foreign-variable _s_irusr int "S_IREAD")
(define-foreign-variable _s_iwusr int "S_IWRITE")
(define-foreign-variable _s_ixusr int "S_IEXEC")
(define-foreign-variable _s_irgrp int "S_IREAD")
(define-foreign-variable _s_iwgrp int "S_IWRITE")
(define-foreign-variable _s_ixgrp int "S_IEXEC")
(define-foreign-variable _s_iroth int "S_IREAD")
(define-foreign-variable _s_iwoth int "S_IWRITE")
(define-foreign-variable _s_ixoth int "S_IEXEC")
(define-foreign-variable _s_irwxu int "S_IREAD | S_IWRITE | S_IEXEC")
(define-foreign-variable _s_irwxg int "S_IREAD | S_IWRITE | S_IEXEC")
(define-foreign-variable _s_irwxo int "S_IREAD | S_IWRITE | S_IEXEC")

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

(define file-open
  (let ([defmode (bitwise-ior _s_irwxu (fxior _s_irgrp _s_iroth))] )
    (lambda (filename flags . mode)
      (let ([mode (if (pair? mode) (car mode) defmode)])
	(##sys#check-string filename 'file-open)
	(##sys#check-exact flags 'file-open)
	(##sys#check-exact mode 'file-open)
	(let ([fd (##core#inline "C_open" (##sys#make-c-string (##sys#expand-home-path filename)) flags mode)])
	  (when (eq? -1 fd)
	    (##sys#update-errno)
	    (##sys#signal-hook #:file-error 'file-open "cannot open file" filename flags mode) )
	  fd) ) ) ) )

(define file-close
  (lambda (fd)
    (##sys#check-exact fd 'file-close)
    (when (fx< (##core#inline "C_close" fd) 0)
      (##sys#update-errno)
      (##sys#signal-hook #:file-error 'file-close "cannot close file" fd) ) ) )

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
	    (##sys#update-errno)
	    (##sys#signal-hook #:file-error 'file-read "cannot read from file" fd size) )
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
	  (##sys#update-errno)
	  (##sys#signal-hook #:file-error 'file-write "cannot write to file" fd size) )
	n) ) ) )

(define file-mkstemp
  (let ([string-length string-length])
    (lambda (template)
      (##sys#check-string template 'file-mkstemp)
      (let* ([buf (##sys#make-c-string template)]
             [fd (##core#inline "C_mkstemp" buf)]
             [path-length (string-length buf)])
        (when (eq? -1 fd)
          (##sys#update-errno)
          (##sys#signal-hook #:file-error 'file-mkstemp "cannot create temporary file" template) )
        (values fd (##sys#substring buf 0 (fx- path-length 1) ) ) ) ) ) )


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

(define (##sys#stat file)
  (let ([r (cond [(fixnum? file) (##core#inline "C_fstat" file)]
		 [(string? file) (##core#inline "C_stat" (##sys#make-c-string (##sys#expand-home-path file)))]
		 [else (##sys#signal-hook #:type-error "bad argument type - not a fixnum or string" file)] ) ] )
    (when (fx< r 0)
      (##sys#update-errno)
      (##sys#signal-hook #:file-error "cannot access file" file) ) ) )

(define (file-stat f #!optional link)
  (##sys#stat f)
  (vector _stat_st_ino _stat_st_mode _stat_st_nlink
	  _stat_st_uid _stat_st_gid _stat_st_size
	  _stat_st_atime _stat_st_ctime _stat_st_mtime
	  0 0 0 0) )

(define (file-size f) (##sys#stat f) _stat_st_size)
(define (file-modification-time f) (##sys#stat f) _stat_st_mtime)
(define (file-access-time f) (##sys#stat f) _stat_st_atime)
(define (file-change-time f) (##sys#stat f) _stat_st_ctime)
(define (file-owner f) (##sys#stat f) _stat_st_uid)
(define (file-permissions f) (##sys#stat f) _stat_st_mode)

(define (regular-file? fname)
  (##sys#check-string fname 'regular-file?)
  (let ((info (##sys#file-info (##sys#expand-home-path fname))))
    (and info (fx= 0 (##sys#slot info 4))) ) )

(define (symbolic-link? fname)
  (##sys#check-string fname 'symbolic-link?)
  #f)

(define file-position
  (lambda (port)
    (let ([pos (cond [(port? port)
		      (if (eq? (##sys#slot port 7) 'stream)
			  (##core#inline "C_ftell" port)
			  -1) ]
		     [(fixnum? port) (##core#inline "C_lseek" port 0 _seek_cur)]
		     [else (##sys#signal-hook #:type-error 'file-position "invalid file" port)] ) ] )
      (when (fx< pos 0)
	(##sys#update-errno)
	(##sys#signal-hook #:file-error 'file-position "cannot retrieve file position of port" port) )
      pos) ) )

(define set-file-position!
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
	(##sys#update-errno)
	(##sys#signal-hook #:file-error 'set-file-position! "cannot set file position" port pos) ) ) ) )


;;; Directory stuff:

(define create-directory
  (lambda (name)
    (##sys#check-string name 'create-directory)
    (unless (zero? (##core#inline "C_mkdir" (##sys#make-c-string (##sys#expand-home-path name))))
      (##sys#update-errno)
      (##sys#signal-hook #:file-error 'create-directory "cannot create directory" name) ) ) )

(define change-directory
  (lambda (name)
    (##sys#check-string name 'change-directory)
    (unless (zero? (##core#inline "C_chdir" (##sys#make-c-string (##sys#expand-home-path name))))
      (##sys#update-errno)
      (##sys#signal-hook #:file-error 'change-directory "cannot change current directory" name) ) ) )

(define delete-directory
  (lambda (name)
    (##sys#check-string name 'delete-directory)
    (unless (zero? (##core#inline "C_rmdir" (##sys#make-c-string (##sys#expand-home-path name))))
      (##sys#update-errno)
      (##sys#signal-hook #:file-error 'delete-directory "cannot delete directory" name) ) ) )

(define directory
  (let ([string-append string-append]
	[make-string make-string]
	[string string])
    (lambda (#!optional (spec (current-directory)) show-dotfiles?)
      (##sys#check-string spec 'directory)
      (let ([buffer (make-string 256)]
	    [handle (##sys#make-pointer)]
	    [entry (##sys#make-pointer)] )
	(##core#inline "C_opendir" (##sys#make-c-string (##sys#expand-home-path spec)) handle)
	(if (##sys#null-pointer? handle)
	    (begin
	      (##sys#update-errno)
	      (##sys#signal-hook #:file-error 'directory "cannot open directory" spec) )
	    (let loop ()
	      (##core#inline "C_readdir" handle entry)
	      (if (##sys#null-pointer? entry)
		  (begin
		    (##core#inline "C_closedir" handle)
		    '() )
		  (let* ([flen (##core#inline "C_foundfile" entry buffer)]
			 [file (##sys#substring buffer 0 flen)]
			 [char1 (string-ref file 0)]
			 [char2 (and (> flen 1) (string-ref file 1))] )
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
	    (##sys#update-errno)
	    (if len
		(##sys#substring buffer 0 len)
		(##sys#signal-hook #:file-error 'current-directory "cannot retrieve current directory") ) ) ) ) ) )


;;; Pipes:

(let ()
  (define (mode arg) (if (pair? arg) (##sys#slot arg 0) '###text))
  (define (badmode m) (##sys#error "illegal input/output mode specifier" m))
  (define (check cmd inp r)
    (##sys#update-errno)
    (if (##sys#null-pointer? r)
	(##sys#signal-hook #:file-error "cannot open pipe" cmd)
	(let ([port (##sys#make-port inp ##sys#stream-port-class "(pipe)" 'stream)])
	  (##core#inline "C_set_file_ptr" port r)
	  port) ) )
  (set! open-input-pipe
    (lambda (cmd . m)
      (##sys#check-string cmd 'open-input-pipe)
      (let ([m (mode m)])
	(check
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
	 cmd #f
	 (case m
	   ((###text) (##core#inline_allocate ("open_text_output_pipe" 2) (##sys#make-c-string cmd)))
	   ((###binary) (##core#inline_allocate ("open_binary_output_pipe" 2) (##sys#make-c-string cmd)))
	   (else (badmode m)) ) ) ) ) )
  (set! close-input-pipe
    (lambda (port)
      (##sys#check-port port 'close-input-pipe)
      (let ((r (##core#inline "close_pipe" port)))
	(##sys#update-errno)
	(when (eq? -1 r) (##sys#signal-hook #:file-error 'close-input-pipe "error while closing pipe" port)) ) ) )
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


;;; Pipe primitive:

(define-foreign-variable _pipefd0 int "C_pipefds[ 0 ]")
(define-foreign-variable _pipefd1 int "C_pipefds[ 1 ]")

(define create-pipe
    (lambda ()
      (when (fx< (##core#inline "C_pipe" #f) 0)
	(##sys#update-errno)
	(##sys#signal-hook #:file-error 'create-pipe "cannot create pipe") )
      (values _pipefd0 _pipefd1) ) )

;;; Signal processing:

(define-foreign-variable _nsig int "NSIG")
(define-foreign-variable _sigterm int "SIGTERM")
(define-foreign-variable _sigint int "SIGINT")
(define-foreign-variable _sigfpe int "SIGFPE")
(define-foreign-variable _sigill int "SIGILL")
(define-foreign-variable _sigsegv int "SIGSEGV")
(define-foreign-variable _sigabrt int "SIGABRT")
(define-foreign-variable _sigbreak int "SIGBREAK")

(define signal/term _sigterm)
(define signal/int _sigint)
(define signal/fpe _sigfpe)
(define signal/ill _sigill)
(define signal/segv _sigsegv)
(define signal/abrt _sigabrt)
(define signal/break _sigbreak)
(define signal/alrm 0)
(define signal/chld 0)
(define signal/cont 0)
(define signal/hup 0)
(define signal/io 0)
(define signal/kill 0)
(define signal/pipe 0)
(define signal/prof 0)
(define signal/quit 0)
(define signal/stop 0)
(define signal/trap 0)
(define signal/tstp 0)
(define signal/urg 0)
(define signal/usr1 0)
(define signal/usr2 0)
(define signal/vtalrm 0)
(define signal/winch 0)
(define signal/xcpu 0)
(define signal/xfsz 0)

(define signals-list
  (list
    signal/term signal/int signal/fpe signal/ill
    signal/segv signal/abrt signal/break))

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

;;; More errno codes:

(define-foreign-variable _errno int "errno")

(define-foreign-variable _eperm int "EPERM")
(define-foreign-variable _enoent int "ENOENT")
(define-foreign-variable _esrch int "ESRCH")
(define-foreign-variable _eintr int "EINTR")
(define-foreign-variable _eio int "EIO")
(define-foreign-variable _enoexec int "ENOEXEC")
(define-foreign-variable _ebadf int "EBADF")
(define-foreign-variable _echild int "ECHILD")
(define-foreign-variable _enomem int "ENOMEM")
(define-foreign-variable _eacces int "EACCES")
(define-foreign-variable _efault int "EFAULT")
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
(define-foreign-variable _enxio int "ENXIO")
(define-foreign-variable _e2big int "E2BIG")
(define-foreign-variable _exdev int "EXDEV")
(define-foreign-variable _enodev int "ENODEV")
(define-foreign-variable _enfile int "ENFILE")
(define-foreign-variable _enotty int "ENOTTY")
(define-foreign-variable _efbig int "EFBIG")
(define-foreign-variable _emlink int "EMLINK")
(define-foreign-variable _edom int "EDOM")
(define-foreign-variable _erange int "ERANGE")
(define-foreign-variable _edeadlk int "EDEADLK")
(define-foreign-variable _enametoolong int "ENAMETOOLONG")
(define-foreign-variable _enolck int "ENOLCK")
(define-foreign-variable _enosys int "ENOSYS")
(define-foreign-variable _enotempty int "ENOTEMPTY")
(define-foreign-variable _eilseq int "EILSEQ")

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
(define errno/exist _eexist)
(define errno/notdir _enotdir)
(define errno/isdir _eisdir)
(define errno/inval _einval)
(define errno/mfile _emfile)
(define errno/nospc _enospc)
(define errno/spipe _espipe)
(define errno/pipe _epipe)
(define errno/again _eagain)
(define errno/rofs _erofs)
(define errno/nxio _enxio)
(define errno/2big _e2big)
(define errno/xdev _exdev)
(define errno/nodev _enodev)
(define errno/nfile _enfile)
(define errno/notty _enotty)
(define errno/fbig _efbig)
(define errno/mlink _emlink)
(define errno/dom _edom)
(define errno/range _erange)
(define errno/deadlk _edeadlk)
(define errno/nametoolong _enametoolong)
(define errno/nolck _enolck)
(define errno/nosys _enosys)
(define errno/notempty _enotempty)
(define errno/ilseq _eilseq)

;;; Permissions and owners:

(define change-file-mode
  (lambda (fname m)
    (##sys#check-string fname 'change-file-mode)
    (##sys#check-exact m 'change-file-mode)
    (when (fx< (##core#inline "C_chmod" (##sys#make-c-string (##sys#expand-home-path fname)) m) 0)
      (##sys#update-errno)
      (##sys#signal-hook #:file-error 'change-file-mode "cannot change file mode" fname m) ) ) )

(define-foreign-variable _r_ok int "2")
(define-foreign-variable _w_ok int "4")
(define-foreign-variable _x_ok int "2")

(let ()
  (define (check filename acc loc)
    (##sys#check-string filename loc)
    (let ([r (fx= 0 (##core#inline "C_access" (##sys#make-c-string (##sys#expand-home-path filename)) acc))])
      (unless r (##sys#update-errno))
      r) )
  (set! file-read-access? (lambda (filename) (check filename _r_ok 'file-read-access?)))
  (set! file-write-access? (lambda (filename) (check filename _w_ok 'file-write-access?)))
  (set! file-execute-access? (lambda (filename) (check filename _x_ok 'file-execute-access?))) )

(define-foreign-variable _filename_max int "FILENAME_MAX")

;;; Using file-descriptors:

(define-foreign-variable _stdin_fileno int "0")
(define-foreign-variable _stdout_fileno int "1")
(define-foreign-variable _stderr_fileno int "2")

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
  (define (check fd inp r)
    (##sys#update-errno)
    (if (##sys#null-pointer? r)
	(##sys#signal-hook #:file-error "cannot open file" fd)
	(let ([port (##sys#make-port inp ##sys#stream-port-class "(fdport)" 'stream)])
	  (##core#inline "C_set_file_ptr" port r)
	  port) ) )
  (set! open-input-file*
    (lambda (fd . m)
      (##sys#check-exact fd 'open-input-file*)
      (check fd #t (##core#inline_allocate ("C_fdopen" 2) fd (mode #t m))) ) )
  (set! open-output-file*
    (lambda (fd . m)
      (##sys#check-exact fd 'open-output-file*)
      (check fd #f (##core#inline_allocate ("C_fdopen" 2) fd (mode #f m)) ) ) ) )

(define port->fileno
  (lambda (port)
    (##sys#check-port port 'port->fileno)
    (if (not (zero? (##sys#peek-unsigned-integer port 0)))
	(let ([fd (##core#inline "C_C_fileno" port)])
	  (when (fx< fd 0)
	    (##sys#update-errno)
	    (##sys#signal-hook #:file-error 'port->fileno "cannot access file-descriptor of port" port) )
	  fd)
	(##sys#signal-hook #:type-error 'port->fileno "port has no attached file" port) ) ) )

(define duplicate-fileno
  (lambda (old . new)
    (##sys#check-exact old duplicate-fileno)
    (let ([fd (if (null? new)
		  (##core#inline "C_dup" old)
		  (let ([n (car new)])
		    (##sys#check-exact n 'duplicate-fileno)
		    (##core#inline "C_dup2" old n) ) ) ] )
      (when (fx< fd 0)
	(##sys#update-errno)
	(##sys#signal-hook #:file-error 'duplicate-fileno "cannot duplicate file descriptor" old) )
      fd) ) )

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
  (let ([get (foreign-lambda c-string "C_getenventry" int)]
	[substring substring] )
    (lambda ()
      (let loop ([i 0])
	(let ([entry (get i)])
	  (if entry
	      (let scan ([j 0])
		(if (char=? #\= (##core#inline "C_subchar" entry j))
		    (cons (cons (substring entry 0 j) (substring entry (fx+ j 1) (##sys#size entry))) (loop (fx+ i 1)))
		    (scan (fx+ j 1)) ) )
	      '() ) ) ) ) ) )

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
	(unless str (##sys#error 'time->string "cannot time vector to string" tm))
	str) ) ) )

(define (local-time->seconds tm)
  (##sys#check-vector tm 'local-time->seconds)
  (when (fx< (##sys#size tm) 10) (##sys#error 'local-time->seconds "time vector too short" tm))
  (if (##core#inline "C_mktime" tm)
      (##sys#cons-flonum)
      (##sys#error 'local-time->seconds "cannot convert time vector to seconds" tm) ) )



;;; Other things:

(define _exit
  (let ([ex0 (foreign-lambda void "_exit" int)])
    (lambda code
      (##sys#cleanup-before-exit)
      (ex0 (if (pair? code) (car code) 0)) ) ) )

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
		  (let loop ([f (directory (or dir "."))])
		    (cond [(null? f) (conc (cdr paths))]
			  [(string-match rx (car f))
			   => (lambda (m) (cons (make-pathname dir (car m)) (loop (cdr f)))) ]
			  [else (loop (cdr f))] ) ) ) ) ) ) ) ) ) )


;;; Process handling:

(define-foreign-variable _p_overlay int "P_OVERLAY")
(define-foreign-variable _p_wait int "P_WAIT")
(define-foreign-variable _p_nowait int "P_NOWAIT")
(define-foreign-variable _p_nowaito int "P_NOWAITO")
(define-foreign-variable _p_detach int "P_DETACH")

(define spawn/overlay _p_overlay)
(define spawn/wait _p_wait)
(define spawn/nowait _p_nowait)
(define spawn/nowaito _p_nowaito)
(define spawn/detach _p_detach)

(define process-execute
  (let ([setarg (foreign-lambda void "C_set_exec_arg" int scheme-pointer int)]
	[freeargs (foreign-lambda void "C_free_exec_args")]
	[pathname-strip-directory pathname-strip-directory] )
    (lambda (filename #!optional (arglist '()) envlist)
      (##sys#check-string filename 'process-execute)
      (let ([arglist (if (pair? arglist) (car arglist) '())])
	(##sys#check-list arglist 'process-execute)
	(let ([s (pathname-strip-directory filename)])
	  (setarg 0 s (##sys#size s)) )
	(do ([al arglist (cdr al)]
	     [i 1 (fx+ i 1)] )
	    ((null? al)
	     (setarg i #f 0)
	     (let ([r (##core#inline "C_execvp" (##sys#make-c-string (##sys#expand-home-path filename)))])
	       (##sys#update-errno)
	       (when (fx= r -1)
		 (freeargs)
		 (##sys#error 'process-execute "cannot execute process" filename) ) ) )
	  (let ([s (car al)])
	    (##sys#check-string s 'process-execute)
	    (setarg i s (##sys#size s)) ) ) ) ) ) )

(define process-spawn
  (let ([setarg (foreign-lambda void "C_set_exec_arg" int scheme-pointer int)]
	[freeargs (foreign-lambda void "C_free_exec_args")]
	[pathname-strip-directory pathname-strip-directory] )
    (lambda (mode filename . arglist)
      (##sys#check-exact mode 'process-spawn)
      (##sys#check-string filename 'process-spawn)
      (let ([arglist (if (pair? arglist) (car arglist) '())])
	(##sys#check-list arglist 'process-spawn)
	(let ([s (pathname-strip-directory filename)])
	  (setarg 0 s (##sys#size s)) )
	(do ([al arglist (cdr al)]
	     [i 1 (fx+ i 1)] )
	    ((null? al)
	     (setarg i #f 0)
	     (let ([r (##core#inline "C_spawnvp" mode
		       (##sys#make-c-string (##sys#expand-home-path filename)))])
	       (##sys#update-errno)
	       (when (fx= r -1)
		 (freeargs)
		 (##sys#error 'process-spawn "cannot execute process" filename) )
	       r) )
	  (let ([s (car al)])
	    (##sys#check-string s 'process-spawn)
	    (setarg i s (##sys#size s)) ) ) ) ) ) )

(define current-process-id (foreign-lambda int "C_getpid"))

(define ##sys#shell-command
  (foreign-lambda* c-string () #<<EOF
    char *ret = getenv("COMSPEC");
    if (ret)
	return (ret);
    else
    {
	OSVERSIONINFO ovf;
	ovf.dwOSVersionInfoSize = sizeof(ovf);
	if (GetVersionEx(&ovf) && (ovf.dwPlatformId == VER_PLATFORM_WIN32_NT))
	    return ("cmd.exe");
	else
	    return ("command.com");
    }
EOF
    ) )

(define (##sys#shell-command-arguments cmdlin)
  (list "/c" cmdlin) )

(define process-run
  (let ([process-spawn process-spawn]
	[getenv getenv] )
    (lambda (f . args)
      (let ([args (if (pair? args) (car args) #f)])
	(if args
	    (process-spawn spawn/nowait f args)
	    (process-spawn spawn/nowait (##sys#shell-command) (##sys#shell-command-arguments f)) ) ) ) ) )

;;; Run subprocess connected with pipes:
(define-foreign-variable _rdbuf char "C_rdbuf")
(define-foreign-variable _wr0 int "C_wr0_")
(define-foreign-variable _rd1 int "C_rd1_")

(define close-handle
  (foreign-lambda int "close_handle" bool))

; from original by Mejedi
(define ##sys#process
  (let (
      [c-process
        (foreign-lambda int "C_process" c-string c-pointer
          (pointer int) (pointer int) (pointer int) (pointer int)
          int)])
    (lambda (loc cmd args env stdoutf stdinf stderrf)
      (let (
          [commandline
            (if args
              (let loop ([args args] [cmdlin cmd])
                (if (null? args)
                  cmdlin
                  (loop (cdr args) (conc cmdlin " " (car args)))))
              cmd)])
        (let-location ([handle int -1] [stdin_fd int -1] [stdout_fd int -1] [stderr_fd int -1])
          (let (
              [code
                (c-process commandline env
                  (location handle) (location stdin_fd) (location stdout_fd) (location stderr_fd)
                  (+ (if stdinf 0 1) (if stdoutf 0 2) (if stderrf 0 4)))])
            (if (fx= 0 code)
              (values
                (and stdoutf (open-input-file* stdout_fd)) ;Parent stdin
                (and stdinf (open-output-file* stdin_fd))  ;Parent stdout
                handle
                (and stderrf (open-input-file* stderr_fd)))
              (begin
                (##sys#update-errno)
                (##sys#signal-hook #:process-error loc "cannot execute process" commandline))) ) ) ) ) ) )

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
      (%process 'process* #t cmd args env) )) )

(define-foreign-variable _exstatus int "C_exstatus")

(define (##sys#process-wait pid nohang)
  (if (##core#inline "C_process_wait" pid nohang)
    (values pid #t _exstatus)
    (values -1 #f #f) ) )

(define process-wait
  (lambda (pid . args)
    (let-optionals* args ([nohang #f])
      (##sys#check-exact pid 'process-wait)
      (receive [epid enorm ecode] (##sys#process-wait pid nohang)
        (if (fx= epid -1)
          (begin
	    (##sys#update-errno)
	    (##sys#signal-hook #:process-error 'process-wait "waiting for child process failed" pid) )
          (values epid enorm ecode) ) ) ) ) )

(define sleep
  (lambda (t)
    (##core#inline "C_sleep" t)
    0) )

(define-foreign-variable _hostname c-string "C_hostname")
(define-foreign-variable _osver c-string "C_osver")
(define-foreign-variable _osrel c-string "C_osrel")
(define-foreign-variable _processor c-string "C_processor")

(define get-host-name
  (lambda ()
    (if (##core#inline "C_get_hostname")
      _hostname
      (##sys#error 'get-host-name "cannot retrieve host-name") ) ) )

(define system-information
  (lambda ()
    (if (##core#inline "C_sysinfo")
      (list "windows" _hostname _osrel _osver _processor)
      (begin
	(##sys#update-errno)
	(##sys#error 'system-information "cannot retrieve system-information") ) ) ) )

;;; Find matching files:

(define find-files
  (let ([glob glob]
	[string-match string-match]
	[make-pathname make-pathname]
	[directory? directory?] )
    (lambda (dir pred . action-id-limit)
      (let-optionals action-id-limit
	  ([action (lambda (x y) (cons x y))] ; no eta reduction here - we want cons inlined.
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

;;; unimplemented stuff:

(define-macro (define-unimplemented name)
  `(define (,name . _)
     (error ',name (##core#immutable '"this function is not available on this platform")) ) )

(define-unimplemented change-file-owner)
(define-unimplemented create-fifo)
(define-unimplemented create-session)
(define-unimplemented create-symbolic-link)
(define-unimplemented current-effective-group-id)
(define-unimplemented current-effective-user-id)
(define-unimplemented current-group-id)
(define-unimplemented current-user-id)
(define-unimplemented map-file-to-memory)
(define-unimplemented file-link)
(define-unimplemented file-lock)
(define-unimplemented file-lock/blocking)
(define-unimplemented file-select)
(define-unimplemented file-test-lock)
(define-unimplemented file-truncate)
(define-unimplemented file-unlock)
(define-unimplemented get-groups)
(define-unimplemented group-information)
(define-unimplemented initialize-groups)
(define-unimplemented local-timezone-abbreviation)
(define-unimplemented memory-mapped-file-pointer)
(define-unimplemented parent-process-id)
(define-unimplemented process-fork)
(define-unimplemented process-group-id)
(define-unimplemented process-signal)
(define-unimplemented read-symbolic-link)
(define-unimplemented set-alarm!)
(define-unimplemented set-group-id!)
(define-unimplemented set-groups!)
(define-unimplemented set-process-group-id!)
(define-unimplemented set-root-directory!)
(define-unimplemented set-signal-mask!)
(define-unimplemented set-user-id!)
(define-unimplemented signal-mask)
(define-unimplemented signal-mask!)
(define-unimplemented signal-masked?)
(define-unimplemented signal-unmask!)
(define-unimplemented terminal-name)
(define-unimplemented terminal-port?)
(define-unimplemented unmap-file-from-memory)
(define-unimplemented user-information)
(define-unimplemented utc-time->seconds)

(define errno/wouldblock 0)

(define (fifo? _) #f)
(define (memory-mapped-file? _) #f)

(define map/anonymous 0)
(define map/file 0)
(define map/fixed 0)
(define map/private 0)
(define map/shared 0)
(define open/fsync 0)
(define open/noctty 0)
(define open/nonblock 0)
(define open/sync 0)
(define perm/isgid 0)
(define perm/isuid 0)
(define perm/isvtx 0)
(define prot/exec 0)
(define prot/none 0)
(define prot/read 0)
(define prot/write 0)
