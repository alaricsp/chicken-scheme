.PHONY: all clean

CC=gcc
LD=gcc
CFLAGS=-g
LDFLAGS=
LIBS=-lm

all: mini

mini: lib.o runtime.o
	$(LD) $(LDFLLAGS) $^ -o $@ $(LIBS)

runtime.o: ../../runtime.c ../../chicken.h
	$(CC) -c $< -o $@ $(CFLAGS)

lib.o: lib.scm ../../chicken.h
	csc -cx -I../.. $< -o $@ -O2 -d0 -kv -raw -C "$(CFLAGS)"

clean:
	rm -f *.o mini
