.PHONY: all clean

CC=gcc
LD=gcc
CFLAGS=-Os -fomit-frame-pointer -fno-strict-aliasing
LDFLAGS=-s
LIBS=-lm

all: mini

mini: lib.o runtime.o
	$(LD) $(LDFLAGS) $^ -o $@ $(LIBS)

runtime.o: ../../runtime.c ../../chicken.h
	$(CC) -c $< -o $@ $(CFLAGS)

lib.o: lib.scm ../../chicken.h
	csc -cx -I../.. $< -o $@ -O2 -d0 -kv -raw -C "$(CFLAGS)"

clean:
	rm -f *.o mini
