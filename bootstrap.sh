# /bin/sh
### bootstrap.sh - Create minimal build tool from scratch

CSI=$1

if [ "$CSI" = "" ]; then 
    CSI=csi
fi

if [ -d boot/cfiles ]; then
    echo "copying .c sources from boot/cfiles ..."
    for cf in `ls boot/cfiles`; do
	if [ ! -e $cf ] || [ boot/cfiles/$cf -nt $cf ]; then
	    cp boot/cfiles/$cf $cf
	fi
    done
fi

if $CSI -v >/dev/null 2>&1; then
    RUN="$CSI -s chicken-build.scm"
else
    rm -f chicken-config.h chicken-defaults.h
    if [ ! -x chicken-build ]; then
	echo "building bootstrapping tool - this takes a while ..."
	cc -o chicken-build \
	    -DHAVE_MEMMOVE=1 -DHAVE_STRERROR=1 -I. -Ipcre \
	    chicken-build.c \
	    runtime.c \
	    library.c \
	    eval.c \
	    extras.c \
	    scheduler.c \
	    srfi-1.c \
	    posixunix.c \
	    regex.c \
	    utils.c \
	    pcre/chartables.c \
	    pcre/pcre_compile.c \
	    pcre/pcre_config.c \
	    pcre/pcre_dfa_exec.c \
	    pcre/pcre_exec.c \
	    pcre/pcre_fullinfo.c \
	    pcre/pcre_get.c \
	    pcre/pcre_globals.c \
	    pcre/pcre_info.c \
	    pcre/pcre_maketables.c \
	    pcre/pcre_ord2utf8.c \
	    pcre/pcre_printint.c \
	    pcre/pcre_refcount.c \
	    pcre/pcre_study.c \
	    pcre/pcre_tables.c \
	    pcre/pcre_try_flipped.c \
	    pcre/pcre_ucp_findchar.c \
	    pcre/pcre_valid_utf8.c \
	    pcre/pcre_version.c \
	    pcre/pcre_xclass.c \
	    -lm || exit 1
	RUN="./chicken-build"
    fi
fi

echo "generating dummy makefile ..."
cat >Makefile <<EOF
.PHONY: all help clean confclean install uninstall spotless dist wikisync testdist
.SILENT:

all:
	$RUN \$(VERBOSE) all
help:
	$RUN \$(VERBOSE) help
clean:
	$RUN \$(VERBOSE) clean
spotless:
	$RUN \$(VERBOSE) spotless
confclean:
	$RUN \$(VERBOSE) confclean
install:
	$RUN \$(VERBOSE) install
uninstall:
	$RUN \$(VERBOSE) uninstall
dist:
	$RUN \$(VERBOSE) dist
testdist:
	$RUN \$(VERBOSE) testdist
wikisync:
	$RUN \$(VERBOSE) wikisync
EOF
