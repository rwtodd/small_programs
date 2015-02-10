.PHONY: all doc
all: iching.exe

iching.exe: iching.o
	gcc -O2 -o $@ $< -lncurses
	strip $@

doc: iching.w
	cweave $< 

.w.c:
	ctangle $<

.c.o:
	gcc -std=c99 -O2 -c $<
