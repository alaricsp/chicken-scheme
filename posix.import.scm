;;;; posix.import.scm - import library for "posix" module
;
; Copyright (c) 2008-2009, The Chicken Team
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


(##sys#register-primitive-module
 'posix
 '(_exit
   call-with-input-pipe
   call-with-output-pipe
   canonical-path			; DEPRECATED
   change-directory
   change-file-mode
   change-file-owner
   close-input-pipe
   close-output-pipe
   create-directory
   create-fifo
   create-pipe
   create-session
   create-symbolic-link
   current-directory
   current-effective-group-id
   current-effective-user-id
   current-effective-user-name
   current-environment			; DEPRECATED
   get-environment-variables
   current-group-id
   current-process-id
   current-user-id
   current-user-name
   delete-directory
   directory
   directory?
   duplicate-fileno
   errno/2big
   errno/acces
   errno/again
   errno/badf
   errno/busy
   errno/child
   errno/deadlk
   errno/dom
   errno/exist
   errno/fault
   errno/fbig
   errno/ilseq
   errno/intr
   errno/inval
   errno/io
   errno/isdir
   errno/mfile
   errno/mlink
   errno/nametoolong
   errno/nfile
   errno/nodev
   errno/noent
   errno/noexec
   errno/nolck
   errno/nomem
   errno/nospc
   errno/nosys
   errno/notdir
   errno/notempty
   errno/notty
   errno/nxio
   errno/perm
   errno/pipe
   errno/range
   errno/rofs
   errno/spipe
   errno/srch
   errno/wouldblock
   errno/xdev
   fcntl/dupfd
   fcntl/getfd
   fcntl/getfl
   fcntl/setfd
   fcntl/setfl
   fifo?
   file-access-time
   file-change-time
   file-close
   file-control
   file-execute-access?
   file-link
   file-lock
   file-lock/blocking
   file-mkstemp
   file-modification-time
   file-open
   file-owner
   file-permissions
   file-position
   set-file-position!
   file-read
   file-read-access?
   file-select
   file-size
   file-stat
   file-test-lock
   file-truncate
   file-unlock
   file-write
   file-write-access?
   fileno/stderr
   fileno/stdin
   fileno/stdout
   find-files
   get-groups
   get-host-name
   glob
   group-information
   initialize-groups
   local-time->seconds
   local-timezone-abbreviation
   map-file-to-memory
   map/anonymous
   map/file
   map/fixed
   map/private
   map/shared
   memory-mapped-file-pointer
   memory-mapped-file?
   open-input-file*
   open-input-pipe
   open-output-file*
   open-output-pipe
   open/append
   open/binary
   open/creat
   open/excl
   open/fsync
   open/noctty
   open/nonblock
   open/rdonly
   open/rdwr
   open/read
   open/sync
   open/text
   open/trunc
   open/write
   open/wronly
   parent-process-id
   perm/irgrp
   perm/iroth
   perm/irusr
   perm/irwxg
   perm/irwxo
   perm/irwxu
   perm/isgid
   perm/isuid
   perm/isvtx
   perm/iwgrp
   perm/iwoth
   perm/iwusr
   perm/ixgrp
   perm/ixoth
   perm/ixusr
   pipe/buf
   port->fileno
   process
   process*
   process-execute
   process-fork
   process-group-id
   process-run
   process-signal
   process-wait
   prot/exec
   prot/none
   prot/read
   prot/write
   read-symbolic-link
   regular-file?
   seconds->local-time
   seconds->string
   seconds->utc-time
   seek/cur
   seek/end
   seek/set
   set-alarm!
   set-buffering-mode!
   set-groups!
   set-root-directory!
   set-signal-handler!
   set-signal-mask!
   setenv
   signal-handler
   signal-mask
   signal-mask!
   signal-masked?
   signal-unmask!
   signal/abrt
   signal/alrm
   signal/chld
   signal/cont
   signal/fpe
   signal/hup
   signal/ill
   signal/int
   signal/io
   signal/kill
   signal/pipe
   signal/prof
   signal/quit
   signal/segv
   signal/stop
   signal/term
   signal/trap
   signal/tstp
   signal/urg
   signal/usr1
   signal/usr2
   signal/vtalrm
   signal/winch
   signal/xcpu
   signal/xfsz
   signals-list
   sleep
   stat-block-device?			; DEPRECATED
   block-device?
   character-device?
   stat-char-device?			; DEPRECATED
   stat-directory?			; DEPRECATED
   stat-fifo?				; DEPRECATED
   fifo?
   stat-regular?			; DEPRECATED
   stat-socket?				; DEPRECATED
   socket?
   stat-symlink?			; DEPRECATED
   string->time
   symbolic-link?
   system-information
   terminal-name
   terminal-port?
   terminal-size
   time->string
   unmap-file-from-memory
   unsetenv
   user-information
   utc-time->seconds
   with-input-from-pipe
   with-output-to-pipe))