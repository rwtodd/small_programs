.PHONY: all doc
all: u5sav.exe

u5sav.exe: u5sav.o
	gcc -O2 -o $@ $<
	strip $@

doc: u5sav.tex

.w.tex:
	cweave $<

.w.c:
	ctangle $<

.c.o:
	gcc -std=c11 -O2 -c $<
