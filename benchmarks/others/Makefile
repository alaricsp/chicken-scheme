.PHONY: all clean

all: exception except setlongjmp except-fast except2

clean: 
	rm -f *.o except exception except-fast except2 setlongjmp

exception: exception.cpp
	g++ $< -o $@ -O2

except: except.scm
	csc $< -o $@ -O2 -d0

except-fast: except.scm
	csc $< -o $@ -Ob

except2: except2.scm
	csc $< -o $@ -Ob

setlongjmp: setlongjmp.c
	gcc $< -o $@ -O2
